{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
  ) where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import qualified Data.Text as T
import Data.Aeson (encode, object, (.=), Value)
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Error (Error, runError)
import Runix.Logging (Logging(..), Level(..))
import Runix.StreamChunk (StreamChunk(..), ignoreChunks)
import Runix.Config (runConfig)
import Runix.LLM.Streaming (llmStreamingRestAPI, StreamEvent(..), StreamingEnabled(..))
import Runix.Runner (loggingIO, failLog)

import Server.Core.Run (SessionEffects)
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.GitWorker (runGitViaWorker)
import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Runtime (runInfrastructureWith, StoryModel, storyModel)
import Storyteller.Core.Prompt (interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage(..))
import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Core.Types (unTickId)

import Runix.LLM.Interpreter (interpretLLM, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..), RestAPI, restapiHTTP, llmRetry)
import qualified UniversalLLM

-- | Intercept 'StoryStorage' and notify whenever 'UpdateReferences' rewrites
-- a non-empty batch of tick ids — the point at which any client tracking one
-- of those ids (a rebase marker, a context selection) needs to move it.
-- Doesn't consume the effect ('intercept', not 'interpret'): 'runStoryStorageGit'
-- still does the real work: this only observes the one call site that
-- already carries the mapping every rebase/replace/move computes internally.
storageNotify
  :: Members '[StoryStorage, Embed IO] r
  => TChan BranchNotification
  -> Sem r a
  -> Sem r a
storageNotify chan = intercept $ \case
  CreateBranch name -> send (CreateBranch name)
  DeleteBranch name -> send (DeleteBranch name)
  ListBranches      -> send ListBranches
  SetRef name mtid  -> send (SetRef name mtid)
  UpdateReferences mapping -> do
    result <- send (UpdateReferences mapping)
    if null mapping then return () else
      embed $ atomically $ writeTChan chan $
        TicksRemapped (map (\(o, n) -> (unTickId o, unTickId n)) mapping)
    return result

newtype ServerAuth = ServerAuth LlamaCppAuth

instance RestEndpoint ServerAuth where
  apiroot    (ServerAuth a) = apiroot a
  authheaders _             = []
  useragent  _              = "storyteller-server/0.1"

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

actionStack env action =
  let auth = ServerAuth (LlamaCppAuth (envLLMEndpoint env))
  in runError @String
   . loggingIO
   . failLog
   . runInfrastructureWith (runGitViaWorker (envGitWorker env))
   . runStoryStorageGit
   . storageNotify (envNotifyChan env)
   . interpretPromptStorageFS
   . runConfig (StreamingEnabled True)
   . restapiHTTP auth
   . llmStreamingRestAPI @StoryModel auth
   . llmRetry @ServerAuth
   . interpretLLM @ServerAuth (UniversalLLM.route @StoryModel) storyModel modelConfigs
   . raiseUnder @(RestAPI ServerAuth)
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
