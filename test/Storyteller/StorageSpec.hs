{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.StorageSpec (spec) where

import Data.List (nub, sort)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git (Git)
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , writeFile, readFile, listFiles, fileExists, createDirectory, remove)
import Prelude hiding (readFile, writeFile, appendFile)

import Git.Mock

import Storyteller.Types
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
import Storyteller.Git

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Append content to a file in the working tree (or create it if absent).
appendFile :: forall branch r. Members '[FileSystemRead (BranchTag branch), FileSystemWrite (BranchTag branch), Fail] r
           => FilePath -> ByteString -> Sem r ()
appendFile path content = do
  existing <- runFail $ readFile @(BranchTag branch) path
  let base = either (const BS.empty) id existing
  writeFile @(BranchTag branch) path (base <> content)

-- ---------------------------------------------------------------------------
-- Phantom branch tag used across all tests
-- ---------------------------------------------------------------------------

data Main

-- ---------------------------------------------------------------------------
-- Test runners
-- ---------------------------------------------------------------------------

-- | Runner for tests that only use StoryStorage and StoryBranch.
runTest
  :: Sem '[StoryStorage, StoryBranch Main, Git, State GitState, State WorkingTree, Fail] a
  -> Either String a
runTest action =
  run
  . runFail
  . evalState emptyWorkingTree
  . evalState emptyGitState
  . runGitMock
  . runStoryBranchGit @Main (BranchName "main")
  . runStoryStorageGit
  $ action

-- | Runner for tests that also use the filesystem effects.
runTestFS
  :: Sem '[ StoryStorage
          , StoryBranch Main
          , FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , Git
          , State GitState
          , State WorkingTree
          , Fail
          ] a
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyWorkingTree
  . evalState emptyGitState
  . runGitMock
  . runStoryFSGit @Main (BranchName "main")
  . runStoryBranchGit @Main (BranchName "main")
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

    it "drop rewinds the tick pointer to the previous tick" $ do
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

  describe "FileSystem" $ do
    it "written file can be read back" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            writeFile @(BranchTag Main) "hello.txt" "hello world"
            readFile  @(BranchTag Main) "hello.txt"
      result `shouldBe` Right "hello world"

    it "file exists after write" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            writeFile @(BranchTag Main) "foo.txt" "content"
            fileExists @(BranchTag Main) "foo.txt"
      result `shouldBe` Right True

    it "file does not exist before write" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            fileExists @(BranchTag Main) "missing.txt"
      result `shouldBe` Right False

    it "listFiles returns written files in directory" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            writeFile @(BranchTag Main) "a.txt" "a"
            writeFile @(BranchTag Main) "b.txt" "b"
            fmap sort $ listFiles @(BranchTag Main) "/"
      result `shouldBe` Right ["a.txt", "b.txt"]

    it "remove deletes a file" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            writeFile  @(BranchTag Main) "gone.txt" "bye"
            remove @(BranchTag Main) False "gone.txt"
            fileExists @(BranchTag Main) "gone.txt"
      result `shouldBe` Right False

    it "createDirectory creates an explicit directory entry" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            createDirectory @(BranchTag Main) False "subdir"
            fileExists @(BranchTag Main) "subdir"
      result `shouldBe` Right True

  describe "FileSystem + StoryBranch interaction" $ do
    it "store persists file content across ticks" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "paragraph one\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "paragraph two\n"
            _ <- store "tick two"
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "paragraph one\nparagraph two\n"

    it "drop preserves working tree (soft reset)" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "paragraph one\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "paragraph two\n"
            _ <- store "tick two"
            S.drop
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "paragraph one\nparagraph two\n"

    it "drop >> store is an amend" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "paragraph one\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "paragraph one-b\n"
            S.drop
            t1' <- store "tick one (amended)"
            tick <- S.get
            return (t1 /= t1', tickMessage tick)
      result `shouldBe` Right (True, "tick one (amended)")

    it "at checks out target tick's filesystem for the inner action" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "p1\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "p2\n"
            _  <- store "tick two"
            (content, _mapping) <- at t1 $ readFile @(BranchTag Main) "scene.md"
            return content
      result `shouldBe` Right "p1\n"

    it "at restores the caller's working tree after completion" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "p1\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "p2\n"
            _  <- store "tick two"
            appendFile @Main "scene.md" "p3\n"
            _  <- store "tick three"
            appendFile @Main "notes.md" "unsaved\n"
            ((), _mapping) <- at t1 $ do
              appendFile @Main "scene.md" "p1-revised\n"
              _ <- store "tick one (revised)"
              return ()
            (,) <$> readFile @(BranchTag Main) "scene.md"
                <*> readFile @(BranchTag Main) "notes.md"
      result `shouldBe` Right ("p1\np2\np3\n", "unsaved\n")

    it "unstored writes are visible before store" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "draft.md" "in progress"
            readFile @(BranchTag Main) "draft.md"
      result `shouldBe` Right "in progress"

    it "reset discards pending changes and restores head tick's tree" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "committed\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "pending\n"
            S.reset
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "committed\n"

    it "drop preserves unstored writes" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "committed\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "uncommitted\n"
            S.drop
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "committed\nuncommitted\n"

    it "store rejects non-append modification" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "original\n"
            _ <- store "tick one"
            writeFile @(BranchTag Main) "scene.md" "replaced\n"
            store "tick two"
      case result of
        Left err -> err `shouldContain` "non-append"
        Right _  -> fail "expected store to fail on non-append"

    it "at fails cleanly when tick is not in branch history" $ do
      let result = runTestFS $ do
            _ <- createBranch (BranchName "main")
            appendFile @Main "scene.md" "p1\n"
            _ <- store "tick one"
            at (TickId "nonexistent") $ return ()
      case result of
        Left err -> err `shouldContain` "not found in branch history"
        Right _  -> fail "expected at to fail on unknown tick"
