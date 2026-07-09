{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Building blocks for the agent integration suite's effect stack: real
--   LLM calls, cached to disk via 'Runix.LLM.Cache.cacheLLM' so a recorded
--   response replays without the network on every run after the first. See
--   @../PLAN@ (agent integration test suite) for why this suite exists and
--   is kept separate from @storyteller-test@.
--
--   Neither role (agent-under-test, judge) has a model baked in anywhere in
--   this module: 'Storyteller.Core.LLM.Registry.knownModels' is the one
--   registry of models this suite (and production -- see
--   'Storyteller.Core.LLM.Role') can wire up, and @STORY_MODEL@\/@JUDGE_MODEL@
--   independently pick an entry from it at process startup (@Main.hs@, once,
--   not per spec). Which pairing is "sensible" (different models to avoid
--   same-model judging, same model to test that deliberately, either role
--   using either backend) is a run-time decision, not something this module
--   assumes.
module Agent.Integration.Harness
  ( CacheProject(..)
  , KnownModel(..)
  , LLMRunner(..)
  , Main
  , ModelID(..)
  , Runner
  , knownModels
  , mainBranch
  , modelInterpreter
  , resolveFixture
  , resolveKnownModel
  , runExpect
  , withKnownModel
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Test.Hspec (expectationFailure)

import Paths_storyteller (getDataFileName)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, HasProjectPath(..))
import Runix.Git (Git)
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Storyteller.Common.Splitter (Splitter)
import Storyteller.Core.Git (BranchOp, BranchTag)
import Storyteller.Core.LLM.Registry
  ( KnownModel(..), LLMRunner(..), ModelID(..)
  , knownModels, modelInterpreter, resolveKnownModel, withKnownModel )
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))

-- | Chroot marker for the shared on-disk response cache. A plain
--   'FilePath' isn't reused directly (unlike 'Runix.FileSystem.HasProjectPath's
--   own @FilePath@ instance) so this stays its own distinct effect type.
--   One directory, shared by every model\/role: 'Runix.LLM.Cache'\'s cache
--   key already hashes in the model name, so an agent-role and a
--   judge-role entry (or two different models' entries) never collide on
--   the same key -- nothing here needs to know or care which role is
--   asking.
newtype CacheProject = CacheProject FilePath

instance HasProjectPath CacheProject where
  getProjectPath (CacheProject p) = p

-- | Resolve a path under @test/fixtures/@ to an absolute one, robust to
--   'cabal test' not running with the package root as its working
--   directory (it doesn't) -- same mechanism 'Storyteller.CharGenSpec' uses
--   for @test/fixtures/minimal.yaml@ in the main suite.
resolveFixture :: FilePath -> IO FilePath
resolveFixture = getDataFileName

-- | The one content branch every scenario gets for free, already created
--   before its action runs (see @Main.hs@) -- most agents work against a
--   single branch, so that's the default a scenario starts with. Nothing
--   stops a scenario from opening more branches itself the same way (via
--   'Storyteller.Core.Storage.createBranch' then
--   'Storyteller.Core.Git.runBranchAndFS' @\@SomeOtherTag@) -- 'Git' and
--   'StoryStorage' are already in 'Runner''s row for exactly that.
mainBranch :: BranchName
mainBranch = BranchName "main"

-- | Every effect a scenario runs in: 'LLMs' (both agent roles -- see
--   'Storyteller.Core.LLM.Role' -- rather than one free @storyModel@
--   variable, since production agents ('writeAgent', 'reworkAtom',
--   'splitOutlineAgent', ...) now hardcode their role internally instead of
--   staying generic), the judge's own independent @judgeModel@, prompt
--   overrides, logging, and (see 'mainBranch') git-backed storage --
--   'Git'\/'StoryStorage' directly, plus 'mainBranch''s own already-open
--   'BranchOp'\/'FileSystem' trio. Shared between 'Runner' and 'runExpect'
--   so the two can't drift apart.
type ScenarioEffects judgeModel r =
  ( LLMs r
  , Members
      '[ LLM judgeModel, PromptStorage, Logging
       , Git, StoryStorage, BranchOp Main, Splitter
       , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), FileSystemWrite (BranchTag Main)
       , Fail, Embed IO
       ] r
  )

-- | The fully-built interpreter every spec runs its scenarios through --
--   built exactly once by @Main.hs@ (inside a pair of nested
--   'withKnownModel' calls, which is what pins @storyModel@\/@judgeModel@
--   to concrete types for the rest of that scope) and passed down -- see
--   this module's Haddock. @action@ stays row-polymorphic via 'Members'
--   (the usual Polysemy shape), not pinned to one closed, exactly-ordered
--   effect list -- a spec's @do@ block is free to call into whatever
--   combination of 'writeAgent'\/'reworkAtom'\/'judge'\/branch operations
--   it needs without caring what order this module happens to list the
--   required effects in.
--
--   The @forall r.@ is deliberately scoped *inside* the parens, over just
--   the argument -- same shape as @runST :: (forall s. ST s a) -> a@ --
--   not outside alongside @a@: quantifying @r@ at the same level as @a@
--   makes it a fresh, unconstrained metavariable at every call site
--   (ambiguous -- GHC has nothing to pin it to), since nothing in
--   @IO (Either String a)@ mentions @r@ at all.
type Runner judgeModel
  = forall a. (forall r. ScenarioEffects judgeModel r => Sem r a) -> IO (Either String a)

-- | Run a scenario and turn a 'Fail' into an hspec failure -- the
--   @result <- runner (...); case result of Left err -> expectationFailure
--   err; Right () -> pure ()@ dance every @it@ block otherwise repeats
--   verbatim, since every scenario's assertions already run via 'embed'
--   inside the action itself (see @Agent.Integration.CharContextWriteSpec@\/
--   @Agent.Integration.ReworkAtomSpec@) and so end in @()@.
runExpect
  :: forall judgeModel
  .  Runner judgeModel
  -> (forall r. ScenarioEffects judgeModel r => Sem r ())
  -> IO ()
runExpect runner action = runner action >>= either expectationFailure pure
