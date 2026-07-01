{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Polysemy interpreter stacks for each connection level.
--
-- SessionEffects: storage-level effects, no branch open.
-- BranchEffects:  extends SessionEffects with an open StoryBranch + FS.
--
-- Handlers are written against these constraints and never see IO, HTTP,
-- or websocket types.
module Server.Run
  ( runAction
  , actionStack
  , loggingWS
  , SessionEffects
  ) where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import qualified Data.Text as T
import Data.Aeson (encode, object, (.=), Value)
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Runix.Git (Git(..))
import Runix.Logging (Logging(..), Level(..))
import Runix.LLM (LLM)
import Runix.Random (Random)
import Runix.Time (Time, Sleep)

import Server.Env (ServerEnv(..))
import Server.Notification (BranchNotification(..))
import Storyteller.CLI.Env (modelConfigs)
import Storyteller.Runtime (runInfrastructure, StoryModel, storyModel)
import Storyteller.Storage (StoryStorage)
import Storyteller.Git (runStoryStorageGit, refBranchName)
import Storyteller.Types (unBranchName)

import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import qualified UniversalLLM

-- | Intercept 'Git', pass every operation through, and notify whenever a
--   story branch ref moves. A ref update is the point at which a branch's
--   tick chain has actually changed (as opposed to 'WriteCommit'/'WriteObject',
--   which stage objects that may or may not end up referenced) — connections
--   for that branch then refetch and re-push their full state.
gitNotify
  :: Members '[Git, Embed IO] r
  => TChan BranchNotification
  -> Sem (Git : r) a
  -> Sem r a
gitNotify chan = interpret $ \case
  ResolveRef  ref         -> send (ResolveRef  ref)
  DeleteRef   ref         -> send (DeleteRef   ref)
  ListRefs    prefix      -> send (ListRefs    prefix)
  ReadCommit  hash        -> send (ReadCommit  hash)
  ReadObject  hash        -> send (ReadObject  hash)
  WriteObject obj         -> send (WriteObject obj)
  LookupPath  tree path   -> send (LookupPath  tree path)
  WriteCommit cd          -> send (WriteCommit cd)
  CreateRef   ref hash    -> send (CreateRef ref hash) <* notifyRef ref
  UpdateRef   ref hash    -> send (UpdateRef ref hash) <* notifyRef ref
  where
    notifyRef ref = case refBranchName ref of
      Nothing     -> return ()
      Just branch -> embed $ atomically $ writeTChan chan
        BranchNotification { bnBranch = unBranchName branch }

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

-- | Effects available at the session level (no branch open).
type SessionEffects r =
  Members '[Random, Sleep, Time, Git, Fail, Logging, Error String, StoryStorage, LLM StoryModel] r

actionStack env action =
  let auth = ServerAuth (LlamaCppAuth (envLLMEndpoint env))
  in runError @String
   . runInfrastructure (envRepoPath env) (envLLMEndpoint env)
   . gitNotify (envNotifyChan env)
   . runStoryStorageGit
   . interpretLLMWith auth (UniversalLLM.route @StoryModel) storyModel modelConfigs
   $ action

runAction
  :: ServerEnv
  -> (forall r. SessionEffects r => Sem r a)
  -> IO (Either String a)
runAction env action = runM (actionStack env action)
