{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Unit tests for 'Storyteller.Core.Git.cascadeReplace' and the
--   'Runix.Git.isAncestorOfAny' reachability pre-check it now filters
--   branches with -- see that module's TODO-turned-fix. Two things need
--   checking, not just one: that a branch actually sharing ancestry with
--   the mapping is still rewritten correctly (the filter must not throw
--   the correct case away), and that a genuinely disjoint branch is
--   skipped *before* any commit in it is ever read (the actual point of
--   the fix -- checked here by counting 'ReadCommit' calls, not just by
--   observing the final ref state).
module Storyteller.GitCascadeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (State, evalState, get, modify, runState)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Git

import Storyteller.Core.Git (cascadeReplace)
import Storyteller.Core.Types (BranchName(..), TickId(..))

-- | Same tallying technique as 'bench/PerfCascade.hs's @countGitOps@,
--   trimmed to just the one constructor this spec's disjoint-branch
--   assertion needs.
countReadCommits :: Members '[Git, State Int] r => Sem (Git : r) a -> Sem r a
countReadCommits = interpret $ \case
  ResolveRef  ref       -> send (ResolveRef ref)
  CreateRef   ref h     -> send (CreateRef ref h)
  UpdateRef   ref h     -> send (UpdateRef ref h)
  DeleteRef   ref       -> send (DeleteRef ref)
  ListRefs    prefix    -> send (ListRefs prefix)
  ReadCommit  h         -> send (ReadCommit h) <* modify (+ (1 :: Int))
  WriteCommit cd        -> send (WriteCommit cd)
  ReadObject  h         -> send (ReadObject h)
  WriteObject obj       -> send (WriteObject obj)
  LookupPath  tree path -> send (LookupPath tree path)
  IsAncestorOfAny targets h -> send (IsAncestorOfAny targets h)

emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

commit :: Member Git r => [ObjectHash] -> Text -> Sem r ObjectHash
commit parents msg =
  writeCommit CommitData
    { commitParents = parents
    , commitTree    = emptyTree
    , commitMessage = msg
    }

branchRef :: Text -> RefName
branchRef name = RefName ("refs/heads/story/" <> name)

spec :: Spec
spec = describe "cascadeReplace" $ do

  it "rewrites a branch whose ancestry contains a superseded commit" $ do
    let result = run . runFail . evalState emptyGitState . runGitMock $ do
          root <- commit [] "root"
          old  <- commit [root] "old"
          headA <- commit [old] "a-tip"
          let newHash = ObjectHash "synthetic-new-hash"
              mapping = Map.fromList [(old, newHash)]
          calls <- runState ([] :: [(BranchName, Maybe TickId)]) $
            cascadeReplace
              [(branchRef "A", headA)]
              (\n t -> modify ((n, t) :))
              mapping
          return (fst calls, headA)
    case result of
      Left err -> expectationFailure err
      Right (calls, headA) -> do
        map fst calls `shouldBe` [BranchName "A"]
        case calls of
          [(_, Just newTick)] -> TickId (unObjectHash headA) `shouldNotBe` newTick
          _ -> expectationFailure ("expected exactly one remap, got: " <> show calls)

  it "leaves a branch untouched, without reading a single one of its commits, when its ancestry is disjoint from the mapping" $ do
    let result = run . runFail . evalState (0 :: Int) . evalState emptyGitState . runGitMock . countReadCommits $ do
          root  <- commit [] "root"
          old   <- commit [root] "old"
          _     <- commit [old] "a-tip"

          otherRoot <- commit [] "other-root"
          headB     <- commit [otherRoot] "b-tip"

          let mapping = Map.fromList [(old, ObjectHash "synthetic-new-hash")]
          calls <- runState ([] :: [(BranchName, Maybe TickId)]) $
            cascadeReplace
              [(branchRef "B", headB)]
              (\n t -> modify ((n, t) :))
              mapping
          reads <- get @Int
          return (fst calls, reads)
    case result of
      Left err -> expectationFailure err
      Right (calls, reads) -> do
        calls `shouldBe` []
        reads `shouldBe` 0

  it "rewrites the reachable branch and skips the disjoint one when both are passed together" $ do
    let result = run . runFail . evalState emptyGitState . runGitMock $ do
          root  <- commit [] "root"
          old   <- commit [root] "old"
          headA <- commit [old] "a-tip"

          otherRoot <- commit [] "other-root"
          headB     <- commit [otherRoot] "b-tip"

          let mapping = Map.fromList [(old, ObjectHash "synthetic-new-hash")]
          runState ([] :: [(BranchName, Maybe TickId)]) $
            cascadeReplace
              [(branchRef "A", headA), (branchRef "B", headB)]
              (\n t -> modify ((n, t) :))
              mapping
    case result of
      Left err -> expectationFailure err
      Right (calls, _) -> map fst calls `shouldBe` [BranchName "A"]
