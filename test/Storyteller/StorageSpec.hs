{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.StorageSpec (spec) where

import Data.List (nub)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git (Git)
import Git.Mock

import Storyteller.Types
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
import Storyteller.Git

-- ---------------------------------------------------------------------------
-- Test runner: mock Git → pure result
-- ---------------------------------------------------------------------------

runTest
  :: Sem '[StoryStorage, StoryBranch, Git, State GitState, Fail] a
  -> Either String a
runTest action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryBranchGit (BranchName "main")
  . runStoryStorageGit
  $ action

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "StoryStorage" $ do
    it "creates a branch with an initial tick" $ do
      let result = runTest $ do
            b <- createBranch (BranchName "novel")
            return (branchName b)
      result `shouldBe` Right (BranchName "novel")

    it "lists created branches" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "novel")
            _ <- createBranch (BranchName "prequel")
            bs <- listBranches
            return (map branchName bs)
      result `shouldBe` Right [BranchName "novel", BranchName "prequel"]

    it "deletes a branch" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "novel")
            _ <- createBranch (BranchName "draft")
            deleteBranch (BranchName "draft")
            bs <- listBranches
            return (map branchName bs)
      result `shouldBe` Right [BranchName "novel"]

  describe "StoryBranch" $ do
    it "store advances the head" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            t1 <- store "first paragraph"
            t2 <- store "second paragraph"
            return (t1 /= t2)
      result `shouldBe` Right True

    it "get returns the most recently stored tick" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            _ <- store "first"
            _ <- store "second"
            tick <- S.get
            return (tickMessage tick)
      result `shouldBe` Right "second"

    it "drop rewinds to the previous tick" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            _ <- store "first"
            _ <- store "second"
            S.drop
            tick <- S.get
            return (tickMessage tick)
      result `shouldBe` Right "first"

    it "drop at root is a no-op" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            t0 <- fmap tickId S.get
            S.drop
            t1 <- fmap tickId S.get
            return (t0 == t1)
      result `shouldBe` Right True

    it "follow collects all messages in order from head" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            _ <- store "one"
            _ <- store "two"
            _ <- store "three"
            S.follow [] $ \acc tick ->
              case tickParent tick of
                Nothing -> (acc, Nothing)
                Just p  -> (tickMessage tick : acc, Just p)
      result `shouldBe` Right ["one", "two", "three"]

    it "stored tick ids are unique" $
      property $ \(Positive n) -> n <= 20 ==>
        let result = runTest $ do
              _ <- createBranch (BranchName "main")
              mapM (\i -> store (T.pack ("paragraph " <> show (i :: Int)))) [1..n]
        in case result of
          Left err  -> counterexample err False
          Right ids -> length ids === length (nub ids)

    it "at rewrites a tick and replays the tail" $ do
      let result = runTest $ do
            _ <- createBranch (BranchName "main")
            t1 <- store "one"
            _  <- store "two"
            _  <- store "three"
            ((), mapping) <- at t1 $ do
              _ <- store "one (revised)"
              return ()
            tick <- S.get
            return (tickMessage tick, length mapping)
      result `shouldBe` Right ("three", 2)
