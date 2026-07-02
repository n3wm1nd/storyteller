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
import Storyteller.Git hiding (get, drop)

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

-- | Runner for tests that use StoryBranch and filesystem effects.
runTestFS
  :: Sem '[ StoryBranch Main
          , FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , StoryStorage
          , Git
          , State WorkingTree
          , State GitState
          , Fail
          ] a
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . evalState emptyWorkingTree
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runStoryFSGit @Main (BranchName "main")
        . runStoryBranchGit @Main (BranchName "main")
        . subsume_
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

  describe "StoryBranch" $ do
    it "store advances the head" $ do
      let result = runTestFS $ do
            t1 <- store "first paragraph"
            t2 <- store "second paragraph"
            return (t1 /= t2)
      result `shouldBe` Right True

    it "get returns the most recently stored tick" $ do
      let result = runTestFS $ do
            _ <- store "first"
            _ <- store "second"
            tick <- S.get
            return (tickMessage (tickData tick))
      result `shouldBe` Right "second"

    it "drop rewinds the tick pointer to the previous tick" $ do
      let result = runTestFS $ do
            _ <- store "first"
            _ <- store "second"
            S.drop
            tick <- S.get
            return (tickMessage (tickData tick))
      result `shouldBe` Right "first"

    it "drop at root is a no-op" $ do
      let result = runTestFS $ do
            t0 <- fmap tickId S.get
            S.drop
            t1 <- fmap tickId S.get
            return (t0 == t1)
      result `shouldBe` Right True

    it "follow collects all messages in order from head" $ do
      let result = runTestFS $ do
            _ <- store "one"
            _ <- store "two"
            _ <- store "three"
            S.follow [] $ \acc tick ->
              case tickParent tick of
                Nothing -> (acc, Nothing)
                Just p  -> (tickMessage (tickData tick) : acc, Just p)
      result `shouldBe` Right ["one", "two", "three"]

    it "stored tick ids are unique" $
      property $ \(Positive n) -> n <= 20 ==>
        let result = runTestFS $ do
                mapM (\i -> store (T.pack ("paragraph " <> show (i :: Int)))) [1..n]
        in case result of
          Left err  -> counterexample err False
          Right ids -> length ids === length (nub ids)

    it "at rewrites a tick and replays the tail" $ do
      let result = runTestFS $ do
            t1 <- store "one"
            _  <- store "two"
            _  <- store "three"
            ((), mapping) <- at t1 $ do
              _ <- store "one (revised)"
              return ()
            tick <- S.get
            return (tickMessage (tickData tick), length mapping)
      result `shouldBe` Right ("three", 2)

  describe "FileSystem" $ do
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

  describe "FileSystem + StoryBranch interaction" $ do
    it "store persists file content across ticks" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "paragraph one\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "paragraph two\n"
            _ <- store "tick two"
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "paragraph one\nparagraph two\n"

    it "drop preserves working tree (soft reset)" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "paragraph one\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "paragraph two\n"
            _ <- store "tick two"
            S.drop
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "paragraph one\nparagraph two\n"

    it "drop >> store is an amend" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "paragraph one\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "paragraph one-b\n"
            S.drop
            t1' <- store "tick one (amended)"
            tick <- S.get
            return (t1 /= t1', tickMessage (tickData tick))
      result `shouldBe` Right (True, "tick one (amended)")

    it "atWithFS checks out target tick's filesystem for the inner action" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "p1\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "p2\n"
            _  <- store "tick two"
            (content, _mapping) <- atWithFS @Main t1 $
              readFile @(BranchTag Main) "scene.md"
            return content
      result `shouldBe` Right "p1\n"

    it "atWithFS inner filesystem is independent of outer" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "p1\n"
            t1 <- store "tick one"
            appendFile @Main "scene.md" "p2\n"
            _  <- store "tick two"
            appendFile @Main "scene.md" "p3\n"
            _  <- store "tick three"
            appendFile @Main "notes.md" "unsaved\n"
            ((), _mapping) <- atWithFS @Main t1 $ do
              appendFile @Main "scene.md" "p1-revised\n"
              _ <- store "tick one (revised)"
              return ()
            -- outer FS unchanged — independent state
            (,) <$> readFile @(BranchTag Main) "scene.md"
                <*> readFile @(BranchTag Main) "notes.md"
      result `shouldBe` Right ("p1\np2\np3\n", "unsaved\n")

    it "unstored writes are visible before store" $ do
      let result = runTestFS $ do
            appendFile @Main "draft.md" "in progress"
            readFile @(BranchTag Main) "draft.md"
      result `shouldBe` Right "in progress"

    it "reset discards pending changes and restores head tick's tree" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "committed\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "pending\n"
            S.reset
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "committed\n"

    it "drop preserves unstored writes" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "committed\n"
            _ <- store "tick one"
            appendFile @Main "scene.md" "uncommitted\n"
            S.drop
            readFile @(BranchTag Main) "scene.md"
      result `shouldBe` Right "committed\nuncommitted\n"

    it "store rejects non-append modification" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "original\n"
            _ <- store "tick one"
            writeFile @(BranchTag Main) "scene.md" "replaced\n"
            store "tick two"
      case result of
        Left err -> err `shouldContain` "non-append"
        Right _  -> fail "expected store to fail on non-append"

    it "file deleted then recreated: content at each tick is correct" $ do
      -- Check current state and t1 content in one run
      let result1 = runTestFS $ do
            appendFile @Main "outfit.md" "red dress\n"
            t1 <- store "wearing red dress"
            remove @(BranchTag Main) False "outfit.md"
            _  <- store "outfit removed"
            appendFile @Main "outfit.md" "black coat\n"
            _  <- store "wearing black coat"
            current <- readFile @(BranchTag Main) "outfit.md"
            (atT1, _) <- atWithFS @Main t1 $ readFile @(BranchTag Main) "outfit.md"
            return (current, atT1)
      result1 `shouldBe` Right ("black coat\n", "red dress\n")
      -- Check that file was absent at t2 in a separate run
      let result2 = runTestFS $ do
            appendFile @Main "outfit.md" "red dress\n"
            _  <- store "wearing red dress"
            remove @(BranchTag Main) False "outfit.md"
            t2 <- store "outfit removed"
            appendFile @Main "outfit.md" "black coat\n"
            _  <- store "wearing black coat"
            (exists, _) <- atWithFS @Main t2 $ fileExists @(BranchTag Main) "outfit.md"
            return exists
      result2 `shouldBe` Right False

    it "at fails cleanly when tick is not in branch history" $ do
      let result = runTestFS $ do
            appendFile @Main "scene.md" "p1\n"
            _ <- store "tick one"
            at (TickId "nonexistent") $ return ()
      case result of
        Left err -> err `shouldContain` "not found in branch history"
        Right _  -> fail "expected at to fail on unknown tick"

    it "replace amends a tick in place and At replays the tail" $ do
      -- Chain: t1 → t2 → t3, each appending one paragraph.
      -- atWithFS t1 + replace t1: rewrites t1's content, At replays t2 and t3
      -- on top. The mapping records all replayed ticks (t1' from replace,
      -- t2' and t3' from At's rewind). Reading from a fresh FS at head
      -- shows the amended content.
      let result = runTestFS $ do
            appendFile @Main "scene.md" "p1\n"
            t1 <- store "t1"
            appendFile @Main "scene.md" "p2\n"
            _  <- store "t2"
            appendFile @Main "scene.md" "p3\n"
            _  <- store "t3"

            ((), mapping) <- atWithFS @Main t1 $ do
              S.drop
              writeFile @(BranchTag Main) "scene.md" "p1-revised\n"
              _ <- S.replace t1 (draft "t1'")
              return ()

            -- Read head content via a fresh atWithFS at the new head tick.
            headTick <- S.get
            (content, _) <- atWithFS @Main (tickId headTick) $
              readFile @(BranchTag Main) "scene.md"
            return (content, length mapping)
      result `shouldBe` Right ("p1-revised\np2\np3\n", 2)
