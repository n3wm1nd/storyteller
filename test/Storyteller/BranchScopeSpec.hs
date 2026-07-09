{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Regression tests for 'Storyteller.Core.Git.runBranchOpGit''s long-lived
--   scope state ('Storage.Core.ScopeState') across several 'runStorage'
--   dispatches on the same branch scope.
--
--   The scope's remap table deliberately accumulates for the whole scope's
--   lifetime (so a caller-held id from before an earlier edit still
--   resolves — see 'Storage.Core.resolveId'). But the interpreter's
--   after-dispatch step gates its cross-branch broadcast (and the
--   subsequent head-re-resolve + ambient-tree reload) on that same
--   *accumulated* table being non-empty — not on this dispatch having
--   added anything to it. Once any dispatch has remapped an id, every
--   later dispatch on the scope therefore (a) re-broadcasts the whole old
--   mapping via @updateReferences@ again, and (b) reloads the ambient
--   tree fresh from the committed head, silently discarding pending,
--   uncommitted ambient writes — including the very write that dispatch
--   itself just made. Exactly the failure mode the interpreter's own
--   comment says the gating exists to avoid.
--
--   Same two-write shape as the notify-staleness tests: a single-write
--   check passes trivially; only a dispatch made *after* the state went
--   stale can see the bug.
module Storyteller.BranchScopeSpec (spec) where

import Prelude hiding (readFile, writeFile)

import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, modify)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Git

import Storyteller.Core.Types (BranchName(..))
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.Git (runBranchOpGit, runStoryStorageGit)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops

-- | Local phantom branch tag, same convention as 'Storyteller.AtGenericSpec'.
data M

branch :: BranchName
branch = BranchName "b"

-- | Forwarding 'Git' interpreter that only tallies 'IsAncestorOfAny' calls.
--   That effect is issued exclusively by the cross-branch cascade
--   ('cascadeReplace' via @updateReferences@), once per branch swept — with
--   a single branch in the store, the count is exactly how many times the
--   interpreter (re-)broadcast a remap.
countCascades :: Members '[Git, State Int] r => Sem (Git : r) a -> Sem r a
countCascades = interpret $ \case
  CreateRef ref h      -> send (CreateRef ref h)
  UpdateRef ref h      -> send (UpdateRef ref h)
  DeleteRef ref        -> send (DeleteRef ref)
  ResolveRef ref       -> send (ResolveRef ref)
  ListRefs p           -> send (ListRefs p)
  ReadCommit h         -> send (ReadCommit h)
  WriteCommit cd       -> send (WriteCommit cd)
  ReadObject h         -> send (ReadObject h)
  WriteObject obj      -> send (WriteObject obj)
  LookupPath t path    -> send (LookupPath t path)
  IsAncestorOfAny ts h -> modify @Int (+ 1) >> send (IsAncestorOfAny ts h)

-- | One branch, one long-lived 'runBranchOpGit' scope, eager 'StoryStorage'.
--   Returns the action's result plus the total cascade sweep count.
runScope
  :: (forall r. Members '[BranchOp M, Fail] r => Sem r a)
  -> Either String (a, Int)
runScope act =
  run
  . runFail
  . evalState emptyGitState
  . evalState (0 :: Int)
  . runGitMock
  . countCascades
  . runStoryStorageGit
  $ do
      _ <- createBranch branch
      a <- runBranchOpGit @M branch act
      n <- get @Int
      return (a, n)

-- | Three dispatches: an in-place edit of an earlier atom (produces a real
--   old->new remap for the edited tick and the replayed tail), then a plain
--   ambient-only write, then a read-back of that write.
remapThenPendingWrite :: Members '[BranchOp M, Fail] r => Sem r Bool
remapThenPendingWrite = do
  (a1, _) <- runStorage @M (Core.store (Core.Atom [] "a.md" [] "A\n"))
  _       <- runStorage @M (Core.store (Core.Atom [] "a.md" [] "B\n"))
  _       <- runStorage @M (Ops.editAtomAt a1 "A2\n")
  _       <- runStorage @M (Core.writeFile "pending.md" "draft")
  (ex, _) <- runStorage @M (Ops.exists "pending.md")
  return ex

spec :: Spec
spec = describe "runBranchOpGit: scope state across dispatches" $ do

  it "a pending ambient write survives later dispatches when no remap ever happened (control)" $ do
    let result = runScope $ do
          _       <- runStorage @M (Core.store (Core.Atom [] "a.md" [] "A\n"))
          _       <- runStorage @M (Core.writeFile "pending.md" "draft")
          (ex, _) <- runStorage @M (Ops.exists "pending.md")
          return ex
    fmap fst result `shouldBe` Right True

  it "a pending ambient write still survives after an earlier dispatch remapped ids" $
    fmap fst (runScope remapThenPendingWrite) `shouldBe` Right True

  it "the remap is broadcast once, by the dispatch that produced it -- not again by every later dispatch" $
    fmap snd (runScope remapThenPendingWrite) `shouldBe` Right 1
