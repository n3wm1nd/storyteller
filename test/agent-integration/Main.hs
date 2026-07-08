{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.State (evalState)
import Runix.FileSystem (fileSystemLocal)
import Runix.FileSystem.System (filesystemIO)
import Runix.HTTP (httpIO)
import Runix.LLM.Cache (cacheLLM, fileSystemLookup, fileSystemStore)
import Runix.Logging (loggingIO)
import Runix.Runner (withRequestTimeout)
import Runix.Time (timeIO, sleepIO)
import Test.Hspec

import Git.Mock (emptyGitState, runGitMock)
import Storyteller.Common.Splitter (splitMarkdownAware)
import Storyteller.Core.Git (runBranchAndFS, runStoryStorageGit)
import Storyteller.Core.Prompt (interpretPromptStorageMap)
import Storyteller.Core.Storage (createBranch)

import Agent.Integration.Harness
  ( CacheProject(..), LLMRunner(..), Main
  , mainBranch, resolveFixture, resolveKnownModel, withKnownModel
  )
import qualified Agent.Integration.CharContextWriteSpec
import qualified Agent.Integration.ReworkAtomSpec
import qualified Agent.Integration.JourneySpec

-- | Resolve both roles' models (@STORY_MODEL@\/@JUDGE_MODEL@, independent
--   env vars -- see 'Agent.Integration.Harness.knownModels') and build the
--   shared 'Agent.Integration.Harness.Runner' exactly once for the whole
--   suite. Neither role defaults to matching the other; each falls back
--   to its own default only when unset. The two nested 'withKnownModel'
--   calls are what pin @storyModel@\/@judgeModel@ to concrete types for
--   the rest of this scope, including the 'hspec' call below -- every
--   spec runs against whichever models were actually resolved this run.
--
--   Git storage is an in-memory 'Git.Mock' per scenario -- this suite
--   evaluates whether agents get the LLM to do the right thing, not
--   whether git plumbing works (that's what @storyteller-test@'s own
--   suite, against real interpreters, is for); a fresh 'emptyGitState' is
--   seeded on every 'runner' call, so scenarios stay hermetic from each
--   other. 'Agent.Integration.Harness.mainBranch' is created up front so a
--   scenario can start working against it immediately.
main :: IO ()
main = do
  storyKnown <- resolveKnownModel "STORY_MODEL" "qwen35-40b"
  judgeKnown <- resolveKnownModel "JUDGE_MODEL" "deepseek-v4-flash"
  agentCacheDir <- resolveFixture "test/fixtures/llm-agent-cache"

  withKnownModel storyKnown $ \runStory ->
    withKnownModel judgeKnown $ \runJudge -> do
      let runner action =
            runM
            . runFail
            . loggingIO
            . splitMarkdownAware
            . filesystemIO
            . fileSystemLocal (CacheProject agentCacheDir)
            . timeIO
            . sleepIO
            . httpIO (withRequestTimeout 600)
            . interpretPromptStorageMap mempty
            . runLLMRunner runStory
            -- '.' -- 'fileSystemLocal' above already chroots to its own
            -- cache dir, so the cache's own lookup/store path is just the
            -- chroot root, per 'Runix.LLM.Cache.fileSystemLookup''s
            -- Haddock.
            . cacheLLM (fileSystemLookup @CacheProject ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runStory)
            . runLLMRunner runJudge
            . cacheLLM (fileSystemLookup @CacheProject ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runJudge)
            . evalState emptyGitState
            . runGitMock
            . runStoryStorageGit
            $ do
                _ <- createBranch mainBranch
                runBranchAndFS @Main mainBranch action

      hspec $ do
        describe "Agent.Integration.CharContextWriteSpec" (Agent.Integration.CharContextWriteSpec.spec runner)
        describe "Agent.Integration.ReworkAtomSpec"        (Agent.Integration.ReworkAtomSpec.spec runner)
        describe "Agent.Integration.JourneySpec"           (Agent.Integration.JourneySpec.spec runner)
