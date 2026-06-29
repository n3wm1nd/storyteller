{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , SessionEffects
  ) where

import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (Logging)
import Runix.LLM (LLM)
import Runix.Random (Random)
import Runix.Time (Time, Sleep)

import Server.Env (ServerEnv(..))
import Storyteller.CLI.Env (modelConfigs)
import Storyteller.Runtime (runInfrastructure, StoryModel, storyModel)
import Storyteller.Storage (StoryStorage)
import Storyteller.Git (runStoryStorageGit)

import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import qualified UniversalLLM

newtype ServerAuth = ServerAuth LlamaCppAuth

instance RestEndpoint ServerAuth where
  apiroot    (ServerAuth a) = apiroot a
  authheaders _             = []
  useragent  _              = "storyteller-server/0.1"

-- | Effects available at the session level (no branch open).
type SessionEffects r =
  Members '[Random, Sleep, Time, Git, Fail, Logging, Error String, StoryStorage, LLM StoryModel] r

runAction
  :: ServerEnv
  -> (forall r. SessionEffects r => Sem r a)
  -> IO (Either String a)
runAction env action = do
  let auth = ServerAuth (LlamaCppAuth (envLLMEndpoint env))
  runM
    . runError @String
    . runInfrastructure (envRepoPath env) (envLLMEndpoint env)
    . runStoryStorageGit
    . interpretLLMWith auth (UniversalLLM.route @StoryModel) storyModel modelConfigs
    $ action
