{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared test interpreter stack for Server.Branch and Server.File specs.
module Server.TestStack
  ( TestEffects
  , TestRunner
  , testStack
  , testStackTransactional
  ) where

import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail, failToError)
import Polysemy.State (State, evalState)

import Git.Mock
import Runix.Git (Git)
import Runix.Logging (Logging, loggingNull)

import Storyteller.Git
import Storyteller.Storage (StoryStorage)

type TestEffects r = StoryStorage : Git : State GitState : Logging : Fail : Error String : r

-- | A way to run a whole test action to completion. 'testStack' commits
--   every 'StoryStorage' write eagerly, as it happens. 'testStackTransactional'
--   instead runs the same action inside 'Storyteller.Git.withStorage' —
--   the buffering every real server command's writes go through (see
--   'Server.Branch.Connection'/'Server.File.Connection') — so it needs one
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
--   transaction per command (see 'Server.Branch.Connection.commandLoop'),
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
runTestEffects :: Sem (TestEffects r) a -> Sem r (Either String a)
runTestEffects =
    runError @String
  . failToError id
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit

testStack :: TestRunner
testStack = runTestEffects . runStoryStorageGit

testStackTransactional :: TestRunner
testStackTransactional = runTestEffects . withStorage
