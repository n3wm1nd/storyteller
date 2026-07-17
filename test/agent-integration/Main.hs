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
import Runix.LLM.Cache (cacheLLM, fileSystemStore, regeneratingLookup)
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
  ( CacheProject(..), KnownAgentModel(..), LLMRunner(..), Main, Runner
  , loggingPretty, mainBranch, modelInterpreter, resolveFixture, resolveKnownAgentModel
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
import qualified Agent.Integration.RoleplaySpec
import qualified Agent.Integration.RoleplayMidStorySpec
import qualified Agent.Integration.WorldLoreSpec
import qualified Agent.Integration.JournalInstructionSpec
import qualified Agent.Integration.JournalIronySpec
import qualified Agent.Integration.WriterStyleGuideSpec
import qualified Agent.Integration.WriterPinnedContextSpec
import qualified Agent.Integration.WriterEarlierChaptersSpec
import qualified Agent.Integration.WriterConversationHistorySpec
import qualified Agent.Integration.TasksSteeringSpec
import qualified Agent.Integration.TasksSuggestionQualitySpec
import qualified Agent.Integration.TasksReconcileSpec

-- | Resolve both roles' models (@STORY_MODEL@\/@JUDGE_MODEL@, independent
--   env vars -- see 'Agent.Integration.Harness.knownModels' for what's
--   available) and build the shared 'Agent.Integration.Harness.Runner'
--   exactly once for the whole suite. Neither role defaults to matching the
--   other; each falls back to its own default only when unset. The two
--   nested 'withKnownModel' calls are what pin @storyModel@\/@judgeModel@ to
--   concrete types for the rest of this scope, including the 'hspec' call
--   below -- every spec runs against whichever models were actually
--   resolved this run.
--   @storyModel@ backs *both* 'Storyteller.Core.LLM.Role.ProseModel' and
--   'Storyteller.Core.LLM.Role.AgentModel' -- production agents now
--   hardcode their role rather than staying generic (see
--   'Storyteller.Core.LLM.Role.LLMs'), so this suite routes both role
--   proxies to @STORY_MODEL@ via 'reinterpretProse'\/'reinterpretAgent', same mechanism
--   'Storyteller.Core.Runtime.runStoryGit' uses for the CLI -- rather than
--   picking two independent models for the two roles the way production
--   does, since @STORY_MODEL@\/@JUDGE_MODEL@ is this suite's own axis of
--   variation (agent-under-test vs. judge), not per-role model choice.
--   Both @STORY_MODEL@ and @JUDGE_MODEL@ are resolved from
--   'Agent.Integration.Harness.resolveKnownAgentModel' (not the weaker
--   prose-only 'Agent.Integration.Harness.resolveKnownModel') even though
--   'Agent.Integration.Judge.judge' itself only needs 'HasTools': every
--   model here also passes through 'Runix.LLM.Cache.cacheLLM', which
--   requires the full 'HasJSON'\/'HasReasoning' set unconditionally for its
--   own message (de)serialization, regardless of what the judge role
--   actually exercises.
--   @storyKnown@ is unpacked once, via a direct 'KnownAgentModel' pattern
--   match, giving one genuinely fresh existential model type scoped over
--   the whole @do@ block -- then 'modelInterpreter' is called on it twice
--   (once per role) to build two independent 'LLMRunner's. That's the fix
--   for a
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

-- | Whether to regenerate every cached response this run -- @REGENERATE@
--   set to anything but @0@\/@false@\/empty. Wires
--   'Runix.LLM.Cache.regeneratingLookup' in place of
--   'Runix.LLM.Cache.fileSystemLookup' as every 'cacheLLM' call's lookup
--   function below, so every call becomes a live one and
--   'Runix.LLM.Cache.fileSystemStore' overwrites the existing cache file
--   with the fresh response -- for deliberately regenerating a fluke
--   (a model's own non-deterministic miss on one run) rather than having to
--   find and delete the specific cache file(s) under
--   @test/fixtures/llm-agent-cache@ by hand. Off by default: a cache hit is
--   the common case (see @VERBOSE@'s own Haddock above), and always live
--   would defeat this suite's whole point of being replayable without
--   hitting the network on every run.
isRegenerate :: IO Bool
isRegenerate = maybe False (`notElem` ["", "0", "false"]) <$> lookupEnv "REGENERATE"

main :: IO ()
main = do
  -- Both roles are resolved from the agent-eligible table -- see this
  -- function's Haddock for why STORY_MODEL needs it (backs both role
  -- proxies) and why JUDGE_MODEL does too despite 'judge' itself only
  -- needing HasTools (cacheLLM's own unconditional requirement).
  storyKnown <- resolveKnownAgentModel "STORY_MODEL" "qwen35-40b"
  judgeKnown <- resolveKnownAgentModel "JUDGE_MODEL" "deepseek-v4-flash"
  agentCacheDir <- resolveFixture "test/fixtures/llm-agent-cache"
  verbose <- isVerbose
  regenerate <- isRegenerate

  case (storyKnown, judgeKnown) of
    (KnownAgentModel storyID (story :: storyTy) storyConfigs, KnownAgentModel judgeID (judgeVal :: judgeTy) judgeConfigs) -> do
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
              -- 1800s, not 600s: a local model (llama.cpp, CPU-bound) can
              -- take much longer per call than a hosted one, especially for
              -- the bulk/whole-chapter calls with the largest MaxTokens.
              . httpIO (withRequestTimeout 1800)
              . interpretPromptStorageMap mempty
              . runLLMRunner runStoryAgent
              -- '.' -- 'fileSystemLocal' above already chroots to its own
              -- cache dir, so the cache's own lookup/store path is just the
              -- chroot root, per 'Runix.LLM.Cache.fileSystemLookup''s
              -- Haddock.
              . cacheLLM (regeneratingLookup @CacheProject regenerate ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runStoryAgent)
              . reinterpretAgent @storyTy
              . raiseUnder
              . runLLMRunner runStoryProse
              . cacheLLM (regeneratingLookup @CacheProject regenerate ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runStoryProse)
              . reinterpretProse @storyTy
              . raiseUnder
              . runLLMRunner runJudge
              . cacheLLM (regeneratingLookup @CacheProject regenerate ".") (fileSystemStore @CacheProject ".") (llmRunnerModel runJudge)
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
        describe "Agent.Integration.RoleplaySpec"             (Agent.Integration.RoleplaySpec.spec @judgeTy runner)
        describe "Agent.Integration.RoleplayMidStorySpec"     (Agent.Integration.RoleplayMidStorySpec.spec @judgeTy runner)
        describe "Agent.Integration.WorldLoreSpec"            (Agent.Integration.WorldLoreSpec.spec @judgeTy runner)
        describe "Agent.Integration.JournalInstructionSpec"   (Agent.Integration.JournalInstructionSpec.spec @judgeTy runner)
        describe "Agent.Integration.JournalIronySpec"         (Agent.Integration.JournalIronySpec.spec @judgeTy runner)
        describe "Agent.Integration.WriterStyleGuideSpec"        (Agent.Integration.WriterStyleGuideSpec.spec @judgeTy runner)
        describe "Agent.Integration.WriterPinnedContextSpec"     (Agent.Integration.WriterPinnedContextSpec.spec @judgeTy runner)
        describe "Agent.Integration.WriterEarlierChaptersSpec"   (Agent.Integration.WriterEarlierChaptersSpec.spec @judgeTy runner)
        describe "Agent.Integration.WriterConversationHistorySpec" (Agent.Integration.WriterConversationHistorySpec.spec @judgeTy runner)
        describe "Agent.Integration.TasksSteeringSpec"           (Agent.Integration.TasksSteeringSpec.spec @judgeTy runner)
        describe "Agent.Integration.TasksSuggestionQualitySpec"  (Agent.Integration.TasksSuggestionQualitySpec.spec @judgeTy runner)
        describe "Agent.Integration.TasksReconcileSpec"          (Agent.Integration.TasksReconcileSpec.spec @judgeTy runner)
