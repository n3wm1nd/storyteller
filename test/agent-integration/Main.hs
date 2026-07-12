{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment (lookupEnv)

import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.State (evalState)
import Runix.FileSystem (fileSystemLocal)
import Runix.FileSystem.System (filesystemIO)
import Runix.HTTP (httpIO)
import Runix.LLM.Cache (cacheLLM, fileSystemLookup, fileSystemStore)
import Runix.Logging (Logging, loggingNull)
import Runix.Runner (withRequestTimeout)
import Runix.Time (timeIO, sleepIO)
import Test.Hspec

import Git.Mock (emptyGitState, runGitMock)
import Storyteller.Common.Splitter (splitMarkdownAware)
import Storyteller.Core.Git (runBranchAndFS, runStoryStorageGit)
import Storyteller.Core.LLM.Role (reinterpretProse, reinterpretAgent)
import Storyteller.Core.Prompt (interpretPromptStorageMap)
import Storyteller.Core.Storage (createBranch)

import Agent.Integration.Harness
  ( CacheProject(..), KnownModel(..), LLMRunner(..), Main, Runner
  , loggingPretty, mainBranch, modelInterpreter, resolveFixture, resolveKnownModel
  )
import qualified Agent.Integration.CharContextWriteSpec
import qualified Agent.Integration.ReworkAtomSpec
import qualified Agent.Integration.JourneySpec
import qualified Agent.Integration.OutlineSplitQualitySpec
import qualified Agent.Integration.OutlineSplitDefaultsSpec
import qualified Agent.Integration.OutlineSplitFreeformSpec
import qualified Agent.Integration.OutlineSplitBulkSpec
import qualified Agent.Integration.OutlineSplitEscapingSpec
import qualified Agent.Integration.CharacterPresenceSpec
import qualified Agent.Integration.WorldLoreSpec
import qualified Agent.Integration.JournalInstructionSpec
import qualified Agent.Integration.JournalIronySpec

-- | Resolve both roles' models (@STORY_MODEL@\/@JUDGE_MODEL@, independent
--   env vars -- see 'Agent.Integration.Harness.knownModels') and build the
--   shared 'Agent.Integration.Harness.Runner' exactly once for the whole
--   suite. Neither role defaults to matching the other; each falls back
--   to its own default only when unset. The two nested 'withKnownModel'
--   calls are what pin @storyModel@\/@judgeModel@ to concrete types for
--   the rest of this scope, including the 'hspec' call below -- every
--   spec runs against whichever models were actually resolved this run.
--   @storyModel@ backs *both* 'Storyteller.Core.LLM.Role.ProseModel' and
--   'Storyteller.Core.LLM.Role.AgentModel' -- production agents now
--   hardcode their role rather than staying generic (see
--   'Storyteller.Core.LLM.Role.LLMs'), so this suite routes both role
--   proxies to @STORY_MODEL@ via 'reinterpretProse'\/'reinterpretAgent', same mechanism
--   'Storyteller.Core.Runtime.runStoryGit' uses for the CLI -- rather than
--   picking two independent models for the two roles the way production
--   does, since @STORY_MODEL@\/@JUDGE_MODEL@ is this suite's own axis of
--   variation (agent-under-test vs. judge), not per-role model choice.
--   @storyKnown@ is unpacked once, via a direct 'KnownModel' pattern match,
--   giving one genuinely fresh existential model type scoped over the whole
--   @do@ block -- then 'modelInterpreter' is called on it twice (once per
--   role) to build two independent 'LLMRunner's. That's the fix for a
--   structural trap the equivalent nested-'withKnownModel'-CPS version fell
--   into: an 'LLMRunner'\'s own effect row is fixed the moment it's built,
--   so one value can't satisfy two different residual rows the way a fresh
--   'Runix.LLM.Interpreter.interpretLLMWith' call (as
--   'Storyteller.Core.Runtime.runStoryGit' uses) can when called twice --
--   and CPS-bound skolems from two separate 'withKnownModel' calls turned
--   out not to stay usefully distinct across this many nested nested
--   lambdas either. A plain pattern match sidesteps both: one scope, one
--   named existential, called from twice.
--
--   Git storage is an in-memory 'Git.Mock' per scenario -- this suite
--   evaluates whether agents get the LLM to do the right thing, not
--   whether git plumbing works (that's what @storyteller-test@'s own
--   suite, against real interpreters, is for); a fresh 'emptyGitState' is
--   seeded on every 'runner' call, so scenarios stay hermetic from each
--   other. 'Agent.Integration.Harness.mainBranch' is created up front so a
--   scenario can start working against it immediately.
-- | Whether to run in "verbose mode" -- @VERBOSE@ set to anything but
--   @0@\/@false@\/empty -- which keeps 'loggingPretty' (the agents' own
--   step-by-step 'Runix.Logging.info' noise, see @../PLAN.md@'s "keep the
--   caller informed": for a normal debugging run against a live model, that
--   noise is the point, since these are long-running calls with otherwise
--   no visible progress). Compact -- 'Runix.Logging.loggingNull', stdout is
--   just hspec's own pass\/fail output -- is the default: skimming a whole
--   suite's pass\/fail shape at a glance, e.g. after the first live run
--   already cached every response and a rerun is just confirming replay
--   still passes, is the common case; opt into the noise with @VERBOSE=1@
--   when actually debugging one scenario.
isVerbose :: IO Bool
isVerbose = maybe False (`notElem` ["", "0", "false"]) <$> lookupEnv "VERBOSE"

main :: IO ()
main = do
  storyKnown <- resolveKnownModel "STORY_MODEL" "qwen35-40b"
  judgeKnown <- resolveKnownModel "JUDGE_MODEL" "deepseek-v4-flash"
  agentCacheDir <- resolveFixture "test/fixtures/llm-agent-cache"
  verbose <- isVerbose

  case (storyKnown, judgeKnown) of
    (KnownModel storyID (story :: storyTy) storyConfigs, KnownModel judgeID (judgeVal :: judgeTy) judgeConfigs) -> do
      runStoryProse <- modelInterpreter storyID story storyConfigs
      runStoryAgent <- modelInterpreter storyID story storyConfigs
      runJudge      <- modelInterpreter judgeID judgeVal judgeConfigs
      let logInterpreter :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
          logInterpreter = if verbose then loggingPretty else loggingNull
          runner :: Runner judgeTy
          runner action =
              runM
              . runFail
              . logInterpreter
              . splitMarkdownAware
              . filesystemIO
              . fileSystemLocal (CacheProject agentCacheDir)
              . timeIO
              . sleepIO
              . httpIO (withRequestTimeout 600)
              . interpretPromptStorageMap mempty
              . runLLMRunner runStoryAgent
              -- '.' -- 'fileSystemLocal' above already chroots to its own
              -- cache dir, so the cache's own lookup/store path is just the
              -- chroot root, per 'Runix.LLM.Cache.fileSystemLookup''s
              -- Haddock.
              . cacheLLM (fileSystemLookup @CacheProject ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runStoryAgent)
              . reinterpretAgent @storyTy
              . raiseUnder
              . runLLMRunner runStoryProse
              . cacheLLM (fileSystemLookup @CacheProject ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runStoryProse)
              . reinterpretProse @storyTy
              . raiseUnder
              . runLLMRunner runJudge
              . cacheLLM (fileSystemLookup @CacheProject ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runJudge)
              . evalState emptyGitState
              . runGitMock
              . runStoryStorageGit
              $ do
                  _ <- createBranch mainBranch
                  runBranchAndFS @Main mainBranch action

      hspec $ do
        describe "Agent.Integration.CharContextWriteSpec" (Agent.Integration.CharContextWriteSpec.spec @judgeTy runner)
        describe "Agent.Integration.ReworkAtomSpec"        (Agent.Integration.ReworkAtomSpec.spec @judgeTy runner)
        describe "Agent.Integration.JourneySpec"           (Agent.Integration.JourneySpec.spec @judgeTy runner)
        describe "Agent.Integration.OutlineSplitQualitySpec" (Agent.Integration.OutlineSplitQualitySpec.spec @judgeTy runner)
        describe "Agent.Integration.OutlineSplitDefaultsSpec" (Agent.Integration.OutlineSplitDefaultsSpec.spec @judgeTy runner)
        describe "Agent.Integration.OutlineSplitFreeformSpec" (Agent.Integration.OutlineSplitFreeformSpec.spec @judgeTy runner)
        describe "Agent.Integration.OutlineSplitBulkSpec" (Agent.Integration.OutlineSplitBulkSpec.spec @judgeTy runner)
        describe "Agent.Integration.OutlineSplitEscapingSpec" (Agent.Integration.OutlineSplitEscapingSpec.spec @judgeTy runner)
        describe "Agent.Integration.CharacterPresenceSpec"    (Agent.Integration.CharacterPresenceSpec.spec @judgeTy runner)
        describe "Agent.Integration.WorldLoreSpec"            (Agent.Integration.WorldLoreSpec.spec @judgeTy runner)
        describe "Agent.Integration.JournalInstructionSpec"   (Agent.Integration.JournalInstructionSpec.spec @judgeTy runner)
        describe "Agent.Integration.JournalIronySpec"         (Agent.Integration.JournalIronySpec.spec @judgeTy runner)
