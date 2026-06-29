{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Polysemy-to-Handler bridge.
--
-- Handlers are written as rank-2 polymorphic actions:
--
--   forall r. HandlerEffects r => Sem r a
--
-- The concrete row is never named in handler code. 'runRequest' supplies it.
-- Absent from 'HandlerEffects': 'Embed IO', 'HTTP', 'Cmds', 'Cmd "git"'.
module Server.Run
  ( runRequest
  , HandlerEffects
  , ServerAuth(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (Logging)
import Runix.LLM (LLM)
import Runix.Random (Random)
import Runix.Time (Time, Sleep)
import Servant (Handler, ServerError(..), err500, throwError)

import Server.Env (ServerEnv(..))
import Storyteller.CLI.Env (modelConfigs)
import Storyteller.Runtime (runInfrastructure, StoryModel, storyModel)
import Storyteller.Storage (StoryStorage)
import Storyteller.Git (runStoryStorageGit)

import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import qualified UniversalLLM

-- | Auth token for the LLM endpoint, used by 'runRequest'.
newtype ServerAuth = ServerAuth LlamaCppAuth

instance RestEndpoint ServerAuth where
  apiroot    (ServerAuth a) = apiroot a
  authheaders _             = []
  useragent  _              = "storyteller-server/0.1"

-- | Effects available to handler actions.
-- Absent: 'Embed IO', 'HTTP', 'Cmds', 'Cmd "git"'.
-- 'LLM StoryModel' is provided by 'runRequest' with default configs;
-- individual calls may pass override configs to 'continuationAgent' etc.
type HandlerEffects r =
  Members '[Random, Sleep, Time, Git, Fail, Logging, Error String, StoryStorage, LLM StoryModel] r

runRequest
  :: ServerEnv
  -> (forall r. HandlerEffects r => Sem r a)
  -> Handler a
runRequest env action = do
  let auth = ServerAuth (LlamaCppAuth (envLLMEndpoint env))
  result <- liftIO . runM
    . runError @String
    . runInfrastructure (envRepoPath env) (envLLMEndpoint env)
    . runStoryStorageGit
    . interpretLLMWith auth (UniversalLLM.route @StoryModel) storyModel modelConfigs
    $ action
  case result of
    Left err -> throwError err500 { errBody = LBS.pack err }
    Right a  -> return a
