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
--
-- Design: effects are layered bottom-up. The infrastructure layer (git, HTTP,
-- logging) is common. Branch interpreters sit above that. LLM sits above the
-- branch interpreters (so it shares the HTTP stack).
--
-- 'runBranchIO' provides one branch with no LLM — for tools like story-rebase
-- and story-track that don't call an LLM.
-- 'runStoryGitIO' adds the LLM layer on top — for tools like story-write.
-- Multi-branch tools (story-track) assemble their own stack from primitives
-- re-exported here.
module Storyteller.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Branch phantom
  , Main

    -- * Runners
  , runBranchIO
  , runStoryGitIO

    -- * Re-exported primitives for multi-branch runners
  , module Storyteller.Git
  , emptyWorkingTree
  , Git
  , runGitIO
  , loggingIO
  , failLog
  , httpIO
  , withRequestTimeout
  , timeIO
  , sleepIO
  , cmdsIO
  , interpretCmd
  , runError
  , evalState
  , State
  ) where

import Control.Monad (void)
import Polysemy
import Polysemy.Fail
import Polysemy.Error (runError)
import Polysemy.State (State, evalState)
import Runix.Cmd (cmdsIO, interpretCmd)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.Time (timeIO, sleepIO)
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

-- | Phantom tag for the primary story branch in single-branch tools.
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
-- Base runner: one branch, no LLM
-- ---------------------------------------------------------------------------

-- | Run an action against a single git branch.
--
-- Provides:
--   FileSystem / FileSystemRead / FileSystemWrite for (BranchTag branch)
--   StoryBranch branch, StoryStorage, Logging, Fail
--
-- The branch is created if it doesn't exist. No LLM is wired.
runBranchIO
  :: forall branch a.
     String      -- ^ LLAMACPP_ENDPOINT (feeds the HTTP stack even if LLM unused)
  -> FilePath    -- ^ path to git repository
  -> BranchName  -- ^ branch name
  -> ( forall r. Members '[ FileSystem      (BranchTag branch)
                           , FileSystemRead  (BranchTag branch)
                           , FileSystemWrite (BranchTag branch)
                           , StoryBranch branch
                           , StoryStorage
                           , Logging, Fail ] r
       => Sem r a )
  -> IO (Either String a)
runBranchIO endpoint repoPath branch action =
  runM
  . runError
  . loggingIO
  . failLog
  . cmdsIO
  . interpretCmd @"git"
  . evalState (emptyWorkingTree :: WorkingTree)
  . runGitIO repoPath
  . runStoryFSGit @branch branch
  . runStoryBranchGit @branch branch
  . runStoryStorageGit
  . timeIO
  . sleepIO
  . httpIO (withRequestTimeout 600)
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action

-- ---------------------------------------------------------------------------
-- Story runner: one branch + LLM
-- ---------------------------------------------------------------------------

-- | Run an action against a single git branch with full LLM access.
--
-- Exposes 'Git' and 'State WorkingTree' so that actions can temporarily
-- install additional branch interpreters (e.g. for loading character context).
runStoryGitIO
  :: String
  -> FilePath
  -> BranchName
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[ LLM StoryModel
                           , FileSystem      (BranchTag Main)
                           , FileSystemRead  (BranchTag Main)
                           , FileSystemWrite (BranchTag Main)
                           , StoryBranch Main
                           , StoryStorage
                           , Git
                           , State WorkingTree
                           , Logging, Fail ] r
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
