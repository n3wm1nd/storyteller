{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'StoryStorage' (branch create\/delete\/list, cross-branch reference
--   cascade, transactional buffering) — always first-order, untouched by
--   the storage-monad migration (see PLAN-storage-monad.md).
--
--   Per-branch tick-chain contract tests (store\/drop\/get\/follow\/at\/
--   withFS\/replace\/append-only) moved to 'Storyteller.StorageMonadSpec',
--   which exercises the same contract against the new
--   'Storyteller.Core.StorageMonad.StorageT' engine instead of the old
--   @StoryBranch@ effect. What's left here is the ambient 'FileSystem'
--   interpreter ('Storyteller.Core.Git.runStoryFSGit'/'runBranchAndFS'),
--   which is a genuinely new interpreter (retargeted from the old shared-
--   'State WorkingTree' plumbing onto 'BranchOp') not covered by
--   'Storyteller.StorageMonadSpec' at all.
module Storyteller.StorageSpec (spec) where

import Data.List (sort)
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git (Git)
import Runix.FileSystem
  (FileSystem, FileSystemRead, FileSystemWrite, writeFile, readFile, listFiles, fileExists, createDirectory, remove, glob)
import Prelude hiding (readFile, writeFile)

import Git.Mock

import Storyteller.Core.Types
import Storyteller.Core.Storage
import Storyteller.Core.Git

-- ---------------------------------------------------------------------------
-- Phantom branch tag used across all tests
-- ---------------------------------------------------------------------------

data Main

-- ---------------------------------------------------------------------------
-- Test runners
-- ---------------------------------------------------------------------------

-- | Runner for tests that only use StoryStorage (no branch or FS effects).
runTest
  :: Sem '[ StoryStorage
          , Git
          , State GitState
          , Fail
          ] a
  -> Either String a
runTest action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ action

-- | Runner for tests that use the ambient FileSystem effects, backed by
--   'BranchOp'.
runTestFS
  :: Sem '[ FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , BranchOp Main
          , StoryStorage
          , Git
          , State GitState
          , Fail
          ] a
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runBranchAndFS @Main (BranchName "main") action

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

  describe "withStorage (transactions)" $ do
    it "discards all ref writes when the wrapped action fails" $ do
      let result = runTest $ do
            _           <- createBranch (BranchName "novel")
            Just before <- getBranch (BranchName "novel")
            eResult <- runFail $ withStorage $ do
              setRef (BranchName "novel") (Just (TickId "should-not-land"))
              fail "boom"
              return ()
            Just after <- getBranch (BranchName "novel")
            return (eResult, branchHead before, branchHead after)
      case result of
        Left err -> expectationFailure err
        Right (eResult, before, after) -> do
          eResult `shouldSatisfy` either (const True) (const False)
          after `shouldBe` before

    it "publishes ref writes once the wrapped action succeeds" $ do
      let result = runTest $ do
            _           <- createBranch (BranchName "novel")
            Just before <- getBranch (BranchName "novel")
            withStorage $ setRef (BranchName "novel") (Just (TickId "new-head"))
            Just after <- getBranch (BranchName "novel")
            return (branchHead before /= branchHead after, branchHead after)
      result `shouldBe` Right (True, TickId "new-head")

    it "withStorageDiscard never publishes ref writes, even on success" $ do
      let result = runTest $ do
            _           <- createBranch (BranchName "novel")
            Just before <- getBranch (BranchName "novel")
            withStorageDiscard $ setRef (BranchName "novel") (Just (TickId "should-not-land"))
            Just after <- getBranch (BranchName "novel")
            return (branchHead before, branchHead after)
      case result of
        Left err -> expectationFailure err
        Right (before, after) -> after `shouldBe` before

  describe "FileSystem (via BranchOp)" $ do
    it "written file can be read back" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "hello.txt" "hello world"
            readFile  @(BranchTag Main) "hello.txt"
      result `shouldBe` Right "hello world"

    it "file exists after write" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "foo.txt" "content"
            fileExists @(BranchTag Main) "foo.txt"
      result `shouldBe` Right True

    it "file does not exist before write" $ do
      let result = runTestFS $ do
            fileExists @(BranchTag Main) "missing.txt"
      result `shouldBe` Right False

    it "listFiles returns written files in directory" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "a.txt" "a"
            writeFile @(BranchTag Main) "b.txt" "b"
            fmap sort $ listFiles @(BranchTag Main) "/"
      result `shouldBe` Right ["a.txt", "b.txt"]

    it "remove deletes a file" $ do
      let result = runTestFS $ do
            writeFile  @(BranchTag Main) "gone.txt" "bye"
            remove @(BranchTag Main) False "gone.txt"
            fileExists @(BranchTag Main) "gone.txt"
      result `shouldBe` Right False

    it "createDirectory creates an explicit directory entry" $ do
      let result = runTestFS $ do
            createDirectory @(BranchTag Main) False "subdir"
            fileExists @(BranchTag Main) "subdir"
      result `shouldBe` Right True

    -- 'Glob' was a stub ("not yet implemented") until it was backed by
    -- 'Storyteller.Core.StorageMonad.listAllFilesS' + 'System.FilePath.Glob'.
    it "glob matches files under the whole tree, not just the top level" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "outline.md" "o"
            writeFile @(BranchTag Main) "chapters/ch1.md" "c1"
            writeFile @(BranchTag Main) "chapters/ch2.md" "c2"
            writeFile @(BranchTag Main) "chapters/ch1.outline.md" "beats"
            fmap sort $ glob @(BranchTag Main) "." "chapters/*.md"
      result `shouldBe` Right (sort ["chapters/ch1.md", "chapters/ch2.md", "chapters/ch1.outline.md"])

    it "glob's ** matches recursively across directories" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "outline.md" "o"
            writeFile @(BranchTag Main) "chapters/ch1.md" "c1"
            fmap sort $ glob @(BranchTag Main) "." "**/*.md"
      result `shouldBe` Right (sort ["outline.md", "chapters/ch1.md"])

    it "glob returns nothing for a pattern that matches no file" $ do
      let result = runTestFS $ do
            writeFile @(BranchTag Main) "outline.md" "o"
            glob @(BranchTag Main) "." "*.txt"
      result `shouldBe` Right []
