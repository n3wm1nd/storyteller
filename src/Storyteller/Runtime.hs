{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared runtime: model, interpreter, and IO effect stack for all executables.
module Storyteller.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Filesystem tags
  , StoryFS(..)
  , Main

    -- * IO runners
  , runStoryIO
  , runStoryGitIO
  ) where

import Control.Monad (void)
import Polysemy
import Polysemy.Fail
import Polysemy.Error (runError)
import Polysemy.State (evalState)
import Runix.Cmd (cmdsIO, interpretCmd)
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , HasProjectPath(..), fileSystemLocal )
import Runix.FileSystem.System (filesystemReadIO, filesystemWriteIO)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.Time (timeIO, sleepIO)
import Runix.Logging (Logging)

import Runix.Git (runGitIO)
import Storyteller.Types (BranchName(..))
import Storyteller.Git ( BranchTag(..), WorkingTree, emptyWorkingTree
                       , runStoryStorageGit, runStoryBranchGit, runStoryFSGit )
import Storyteller.Storage (StoryBranch, createBranch, getBranch)

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
-- Filesystem tags
-- ---------------------------------------------------------------------------

-- | Tag for local-filesystem-backed story access (chrooted to a directory).
newtype StoryFS = StoryFS FilePath

instance HasProjectPath StoryFS where
  getProjectPath (StoryFS path) = path

-- | Phantom tag for the git-backed main story branch.
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
-- Local filesystem runner
-- ---------------------------------------------------------------------------

-- | Run a story action against a local directory (no git).
--   Provides: LLM StoryModel, FileSystem StoryFS, Logging, Fail.
runStoryIO
  :: String                    -- ^ llama-cpp base URL
  -> FilePath                  -- ^ root directory (chroot)
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[LLM StoryModel, FileSystem StoryFS, FileSystemRead StoryFS, FileSystemWrite StoryFS, Logging, Fail] r
       => Sem r a )
  -> IO (Either String a)
runStoryIO endpoint rootDir configs action =
  runM
  . runError
  . loggingIO
  . failLog
  . filesystemReadIO
  . filesystemWriteIO
  . fileSystemLocal (StoryFS rootDir)
  . timeIO
  . sleepIO
  . httpIO (withRequestTimeout 600)
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  $ action

-- ---------------------------------------------------------------------------
-- Git-backed runner
-- ---------------------------------------------------------------------------

-- | Run a story action against a git repository.
--   Opens (or creates) @branchName@ under @refs/heads/story/<name>@.
--   Provides: LLM StoryModel, FileSystem (BranchTag Main), Logging, Fail.
runStoryGitIO
  :: String                    -- ^ llama-cpp base URL
  -> FilePath                  -- ^ path to the git repository
  -> BranchName                -- ^ story branch name
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[ LLM StoryModel
                           , FileSystem      (BranchTag Main)
                           , FileSystemRead  (BranchTag Main)
                           , FileSystemWrite (BranchTag Main)
                           , StoryBranch Main
                           , Logging, Fail] r
       => Sem r a )
  -> IO (Either String a)
runStoryGitIO endpoint repoPath branch configs action =
  runM
  . runError
  . loggingIO
  . failLog
  . cmdsIO
  . interpretCmd @"git"
  . evalState (emptyWorkingTree :: WorkingTree)
  . runGitIO repoPath
  . runStoryFSGit @Main branch
  . runStoryBranchGit @Main branch
  . runStoryStorageGit
  . timeIO
  . sleepIO
  . httpIO (withRequestTimeout 600)
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action
