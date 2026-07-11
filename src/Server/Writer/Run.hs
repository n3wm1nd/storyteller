{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

-- | Polysemy interpreter stacks for the Writer server's connection levels.
--
-- 'SessionEffects' (the effect-membership vocabulary handlers are written
-- against) lives in 'Server.Core.Run' — a library declaration, not wiring.
-- Everything here is the actual assembly that satisfies it for this one
-- app: git/storage ref-move and tick-remap notification wiring, LLM
-- routing, and the logging/streaming-preview interpreters wired to a
-- specific WebSocket connection. A second server (Roleplay, Lector) would
-- likely need its own version of this module, not a shared one — see
-- STRUCTURE.md.
--
-- Handlers are written against 'SessionEffects' and never see IO, HTTP, or
-- websocket types.
module Server.Writer.Run
  ( runAction
  , actionStack
  , loggingWS
  , wsAction
  , notifyRemaps
  ) where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import qualified Data.Text as T
import Data.Aeson (encode, object, (.=), Value)
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Runix.Logging (Logging(..), Level(..))
import Runix.StreamChunk (StreamChunk(..), ignoreChunks)
import Runix.Config (runConfig, Config)
import Runix.LLM (LLM)
import Runix.LLM.Streaming (StreamEvent(..), StreamingEnabled(..))
import Runix.Runner (loggingIO, failLog)
import Runix.Git (Git)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Random (Random)
import Runix.Time (Sleep, Time)

import Server.Core.Run (SessionEffects)
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.GitWorker (runGitViaWorker)
import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.LLM.Registry (SomeLLMRunner(..))
import Storyteller.Core.LLM.Role (ProseModel, AgentModel, reinterpretProse, reinterpretAgent)
import Storyteller.Core.Runtime (runInfrastructureWith)
import Storyteller.Core.Prompt (interpretPromptStorageFS, PromptStorage)
import Storyteller.Core.Git (runStoryStorageGitNotify)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (TickId, unTickId)
import Storyteller.Core.Undo (Undo)

-- | Notify whenever a non-empty batch of tick ids gets renamed — the
-- point at which any client tracking one of those ids (a rebase marker, a
-- context selection) needs to move it. Passed to the root 'StoryStorage'
-- interpreter as its flush callback (see 'runStoryStorageGitNotify'), the
-- one place the *complete* applied mapping exists: the transaction's own
-- recorded renames plus everything the boundary cascade discovered while
-- rewriting other branches — an interceptor above the interpreter could
-- only ever see the former. Fires once per boundary, so one transaction
-- is one notification however many renames it made.
notifyRemaps
  :: Member (Embed IO) r
  => TChan BranchNotification
  -> [(TickId, TickId)]
  -> Sem r ()
notifyRemaps chan mapping = if null mapping then return () else
  embed $ atomically $ writeTChan chan $
    TicksRemapped (map (\(o, n) -> (unTickId o, unTickId n)) mapping)

-- | Logging interpreter that calls an IO callback for each log entry.
--   Use this to forward logs to a WebSocket connection or any other IO sink.
agentLogEvent :: T.Text -> T.Text -> Value
agentLogEvent level msg = object ["type" .= ("agent.log" :: T.Text), "level" .= level, "message" .= msg]

levelText :: Level -> T.Text
levelText Info    = "info"
levelText Warning = "warning"
levelText Error   = "error"

loggingWS :: Member (Embed IO) r => WS.Connection -> Sem (Logging : r) a -> Sem r a
loggingWS conn = interpret $ \(Log level _ msg) ->
  embed $ WS.sendTextData conn $ encode $ agentLogEvent (levelText level) msg

-- | Map a stream event to its wire event, or 'Nothing' to drop it (tool-call
--   events aren't produced by any agent flow today). This is a *preview*
--   only — an ephemeral, best-effort draft of what the LLM is generating.
--   There is no guarantee it matches, or is followed by, any persisted tick:
--   the client must treat it as void the moment the real 'update'/'error'
--   for the in-flight command arrives, and must also clear it on
--   "chat.preview.end" regardless, since a call can finish with nothing
--   persisted at all (e.g. tool-call-only output).
previewEvent :: StreamEvent -> Maybe Value
previewEvent StreamStarted                = Just $ object ["type" .= ("chat.preview.start"    :: T.Text)]
previewEvent (StreamText t)               = Just $ object ["type" .= ("chat.preview"          :: T.Text), "text" .= t]
previewEvent (StreamThinking t)           = Just $ object ["type" .= ("chat.preview.thinking" :: T.Text), "text" .= t]
previewEvent StreamDone                   = Just $ object ["type" .= ("chat.preview.end"      :: T.Text)]
previewEvent (StreamError _)              = Just $ object ["type" .= ("chat.preview.end"      :: T.Text)]
previewEvent (StreamToolCallStarted  _ _) = Nothing
previewEvent (StreamToolCallArgument _ _) = Nothing
previewEvent (StreamToolCallComplete _)   = Nothing

-- | Push each streamed LLM chunk to the client as it arrives. Styled after
--   'loggingWS': installed once around a connection's whole command loop so
--   it sees chunks emitted from anywhere in the stack, including deep
--   inside 'llmStreamingRestAPI'.
streamChunksWS :: Member (Embed IO) r => WS.Connection -> Sem (StreamChunk StreamEvent : r) a -> Sem r a
streamChunksWS conn = interpret $ \(EmitChunk event) ->
  maybe (return ()) (embed . WS.sendTextData conn . encode) (previewEvent event)

-- | Interprets both role proxy effects: 'reinterpretProse'\/'reinterpretAgent'
--   re-tag each role's requests onto its runtime-chosen model (see
--   'Storyteller.Core.LLM.Role'), and the model's own already-built
--   interpreter (resolved once at startup by 'Server.Writer.Env.loadServerEnv'
--   — includes streaming preview wiring, retry, and auth; see
--   'Storyteller.Core.LLM.Registry.resolveRoleRunner') takes it from there.
--   'raiseUnder' inserts the chosen model's own 'LLM' effect directly under
--   the role effect being eliminated, which is what lets the converted
--   request reach it.
actionStack
  :: (Member (Embed IO) r, Member (StreamChunk StreamEvent) r)
  => ServerEnv
  -> Sem ( LLM ProseModel : LLM AgentModel
         : Config StreamingEnabled : PromptStorage : StoryStorage : Undo
         : Random : HTTP : HTTPStreaming : Sleep : Time : Git
         : Fail : Logging : Error String : r) a
  -> Sem r (Either String a)
actionStack env action =
  case (envProseRunner env, envAgentRunner env) of
    ( SomeLLMRunner (proseRunner :: forall r' a'. Members
        '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled] r'
        => Sem (LLM proseChosen : r') a' -> Sem r' a')
      , SomeLLMRunner (agentRunner :: forall r' a'. Members
        '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled] r'
        => Sem (LLM agentChosen : r') a' -> Sem r' a')
      ) ->
        runError @String
      . loggingIO
      . failLog
      . runInfrastructureWith (runGitViaWorker (envGitWorker env))
      . runStoryStorageGitNotify (notifyRemaps (envNotifyChan env))
      . interpretPromptStorageFS
      . runConfig (StreamingEnabled True)
      . agentRunner
      . reinterpretAgent @agentChosen
      . raiseUnder @(LLM agentChosen)
      . proseRunner
      . reinterpretProse @proseChosen
      . raiseUnder @(LLM proseChosen)
      $ action

-- | No WS connection to push a streaming preview to (CLI/session-only use);
--   drop chunks instead, same as 'wsAction' consuming them into pushes.
runAction
  :: ServerEnv
  -> (forall r. SessionEffects r => Sem r a)
  -> IO (Either String a)
runAction env action = runM (ignoreChunks @StreamEvent (actionStack env action))

-- | Shared composition for WS connections: layers the connection's
--   log-forwarding interpreter *underneath* 'actionStack' (same as before —
--   real 'Log' calls from domain code are diverted to the socket instead of
--   reaching 'runInfrastructure's stdout logger), and its LLM-streaming
--   preview push *around the outside*. Unlike 'Logging', 'StreamChunk
--   StreamEvent' isn't eliminated anywhere inside 'actionStack' — nothing
--   there interprets it, so 'llmStreamingRestAPI's emitted chunks surface on
--   'actionStack's own return row instead of needing to be threaded through
--   'action'; 'streamChunksWS' has to consume it out here.
--
--   Both 'Server.Writer.File.Connection' and 'Server.Writer.Branch.Connection'
--   had been assembling this composition independently; this is the one
--   place it's built. Callers still own 'runM' at their own call site.
wsAction
  :: ServerEnv -> WS.Connection
  -> (forall r. (SessionEffects r, Member (Embed IO) r, Member (Error String) r) => Sem r a)
  -> Sem '[Embed IO] (Either String a)
wsAction env conn action = streamChunksWS conn (actionStack env (loggingWS conn action))
