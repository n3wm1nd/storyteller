{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared test interpreter stack for Server.Core.Branch and Server.Core.File specs.
module Server.TestStack
  ( TestEffects
  , TestRunner
  , testStack
  , testStackTransactional
  , stubLLM
  ) where

import qualified Data.Map.Strict as Map
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail, failToError)
import Polysemy.State (State, evalState)

import Git.Mock
import Runix.Git (Git)
import Runix.LLM (LLM(..))
import Runix.Logging (Logging, loggingNull)

import Storyteller.Core.Context (ContextStorage, interpretContextStorageMap)
import Storyteller.Core.Git
import Storyteller.Core.LLM.Role (AgentModel, ProseModel)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageMap)
import Storyteller.Core.Storage (StoryStorage)

type TestEffects r =
  StoryStorage : LLM ProseModel : LLM AgentModel : PromptStorage : ContextStorage : Git : State GitState : Logging : Fail : Error String : r

-- | A way to run a whole test action to completion. 'testStack' commits
--   every 'StoryStorage' write eagerly, as it happens. 'testStackTransactional'
--   instead runs the same action inside 'Storyteller.Core.Git.withStorage' —
--   the buffering every real server command's writes go through (see
--   'Server.Writer.Branch.Connection'/'Server.Writer.File.Connection') — so it needs one
--   extra 'StoryStorage' layer for 'withStorage' to buffer through and
--   replay into the real one underneath.
--
--   Every 'Server.BranchSpec'/'Server.FileSpec' test is written once
--   against this and run under both (see 'test/Main.hs'), so a bug that
--   only shows up through the buffered path can't slip past the suite the
--   way one once did: every test called branch operations directly
--   against the eager interpreter, so none of them ran the code path a
--   real client command actually takes.
--
--   This wraps a whole test — a real client instead opens a fresh
--   transaction per command (see 'Server.Writer.Branch.Connection.commandLoop'),
--   so a test with several sequential writes is, under this alone, a
--   coarser scenario than production: everything stays in one buffer the
--   whole time rather than each write round-tripping through git before
--   the next starts. That's still a legitimate thing to check (nothing
--   should depend on writes landing eagerly), but it's not sufficient by
--   itself — see the dedicated single-command/nested-'At' tests in
--   'Server.BranchSpec' that pinned down a real corruption this coarser
--   wrapping alone didn't reproduce.
type TestRunner = forall r a. Sem (StoryStorage : TestEffects r) a -> Sem r (Either String a)

-- | The real interpreter stack for 'TestEffects' itself, shared by both
--   runners below — each just peels the one extra 'StoryStorage' layer
--   'TestRunner' carries differently before handing off to this.
--
--   'LLM'\/'PromptStorage' are interpreted, not just present in the type,
--   so any function requiring 'Storyteller.Core.LLM.Role.LLMs' (e.g.
--   'Server.Writer.Branch.summarize', whose type is the same regardless of
--   which @kind@ a given call actually passes) still type-checks here —
--   'interpretPromptStorageMap' with no overrides falls back to each
--   caller's own compiled-in default, and 'stubLLM' fails loudly if a test
--   path ever does reach a real 'queryLLM' call, which no unit test in this
--   suite is meant to (that's what @agent-integration@ is for).
runTestEffects :: Sem (TestEffects r) a -> Sem r (Either String a)
runTestEffects =
    runError @String
  . failToError id
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . interpretContextStorageMap Map.empty
  . interpretPromptStorageMap Map.empty
  . stubLLM @AgentModel
  . stubLLM @ProseModel
  . runStoryStorageGit

-- | A stub 'LLM' interpreter that always fails -- see 'runTestEffects'.
stubLLM :: forall model r a. Sem (LLM model ': r) a -> Sem r a
stubLLM = interpret $ \case
  QueryLLM _ _ -> pure (Left "LLM not available in the unit-test stack")

testStack :: TestRunner
testStack = runTestEffects . runStoryStorageGit

testStackTransactional :: TestRunner
testStackTransactional = runTestEffects . withStorage
