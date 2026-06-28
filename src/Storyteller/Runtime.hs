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
module Storyteller.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Branch phantom
  , Main

    -- * Runners
  , runInfrastructure
  , runBranchIO
  , runStoryGitIO

    -- * Re-exported for custom stacks
  , module Storyteller.Git
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
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.HTTP (HTTP)
import Runix.Time (Time, Sleep, timeIO, sleepIO)
import Runix.Logging (Logging)

import Runix.Git (Git, runGitIO)
import Storyteller.Types (BranchName(..))
import Storyteller.Git
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, getBranch)

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
  -> Sem (HTTP : Sleep : Time : Git : Cmd "git" : Cmds : Fail : Logging : r) a
  -> Sem r a
runInfrastructure repoPath _endpoint =
    loggingIO
  . failLog
  . cmdsIO
  . interpretCmd @"git"
  . runGitIO repoPath
  . timeIO
  . sleepIO
  . httpIO (withRequestTimeout 600)

-- | One branch, storage, no LLM. Creates the branch if it doesn't exist.
runBranchIO
  :: forall branch a.
     FilePath
  -> String
  -> BranchName
  -> ( forall r. Members '[ FileSystem      (BranchTag branch)
                           , FileSystemRead  (BranchTag branch)
                           , FileSystemWrite (BranchTag branch)
                           , StoryBranch branch
                           , StoryStorage
                           , Git, Logging, Fail ] r
       => Sem r a )
  -> IO (Either String a)
runBranchIO repoPath endpoint branch action =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runBranchAndFS @branch branch
  . runStoryStorageGit
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action

-- | One branch, storage, LLM. Creates the branch if it doesn't exist.
runStoryGitIO
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
                           , Git, Logging, Fail ] r
       => Sem r a )
  -> IO (Either String a)
runStoryGitIO repoPath endpoint branch configs action =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runBranchAndFS @Main branch
  . runStoryStorageGit
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action
