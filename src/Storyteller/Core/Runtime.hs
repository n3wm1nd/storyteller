{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared runtime: model, interpreters, and IO effect stacks.
module Storyteller.Core.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Branch phantoms
  , Main
  , Prompts

    -- * Runners
  , runInfrastructure
  , runStoryGit

    -- * Re-exported for custom stacks
  , module Storyteller.Core.Git
  , runStoryStorageGit
  , Git
  ) where

import Control.Monad (void)
import Polysemy
import Polysemy.Fail
import Polysemy.Error (Error, runError)
import Runix.Cmd (Cmds, Cmd, cmdsIO, interpretCmd)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.Random (Random, randomIO)
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.HTTP (HTTP, HTTPStreaming, httpIOStreaming)
import Runix.Time (Time, Sleep, timeIO, sleepIO)
import Runix.Logging (Logging)

import Runix.Git (Git, runGitIO, withGitCache)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch, getBranch)

import UniversalLLM (Model(..), ModelConfig, Routing(..))
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_40B(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type StoryModel = Model Qwen35_40B LlamaCpp

storyModel :: StoryModel
storyModel = Model Qwen35_40B LlamaCpp

-- ---------------------------------------------------------------------------
-- Phantom tag
-- ---------------------------------------------------------------------------

data Main

-- | Phantom for the dedicated, project-scoped branch backing
--   'Storyteller.Core.Prompt.PromptStorage'. Not a content branch — it holds
--   only prompt/template overrides, keyed by path, independent of any story
--   or character branch.
data Prompts

-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

newtype StoryLlamaCppAuth = StoryLlamaCppAuth LlamaCppAuth

instance RestEndpoint StoryLlamaCppAuth where
  apiroot    (StoryLlamaCppAuth a) = apiroot a
  authheaders _                    = []
  useragent  _                     = "storyteller/0.1"

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Shared infrastructure interpreters: git, http, time, logging, error.
--   Every executable uses this as its base; branch/storage/LLM go on top.
runInfrastructure
  :: Members '[Error String, Embed IO] r
  => FilePath
  -> String
  -> Sem (Random : HTTP : HTTPStreaming : Sleep : Time : Git : Cmd "git" : Cmds : Fail : Logging : r) a
  -> Sem r a
runInfrastructure repoPath _endpoint =
    loggingIO
  . failLog
  . cmdsIO
  . interpretCmd @"git"
  . runGitIO repoPath
  . withGitCache
  . timeIO
  . sleepIO
  . httpIOStreaming (withRequestTimeout 600)
  . httpIO (withRequestTimeout 600)
  . randomIO

-- | One branch, storage, LLM. Creates the branch if it doesn't exist.
runStoryGit
  :: FilePath
  -> String
  -> BranchName
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[ LLM StoryModel
                           , FileSystem      (BranchTag Main)
                           , FileSystemRead  (BranchTag Main)
                           , FileSystemWrite (BranchTag Main)
                           , StoryBranch Main
                           , StoryStorage
                           , Git
                           , Logging, Fail ] r
       => Sem r a )
  -> IO (Either String a)
runStoryGit repoPath endpoint branch configs action =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runStoryStorageGit
  . runBranchAndFS @Main branch
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action
