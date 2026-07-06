{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Contract tests for 'Storyteller.Core.StorageMonad' — the
--   'interpretH'-free replacement for 'Storyteller.Core.Git's
--   'runStoryBranchGit'\/'runAtH' (see PLAN-storage-monad.md).
--
--   Deliberately mirrors 'Storyteller.StorageSpec's @StoryBranch@ cases
--   one-for-one: same scenarios, same expected results, run through
--   'Storyteller.Core.Git.runBranchOpGit' instead of
--   'Storyteller.Core.Git.runStoryBranchGit'. Matching contracts is the
--   evidence that the new engine is a behavior-preserving replacement, not
--   just a differently-shaped one.
module Storyteller.StorageMonadSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (nub)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail
import Polysemy.State (State, evalState)

import Runix.Git (Git)
import Git.Mock

import Storyteller.Core.Types
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Git (BranchOp, runStorage, runBranchOpGit, runStoryStorageGit)
import qualified Storyteller.Core.StorageMonad as SM

-- ---------------------------------------------------------------------------
-- Phantom branch tag
-- ---------------------------------------------------------------------------

data Main

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

runSM
  :: (forall n. SM.StorageM n => SM.StorageT n a)
  -> Either String a
runSM comp =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runBranchOpGitMain (runStorage @Main comp)

-- | Type-pin 'runBranchOpGit' to the fixed test stack — avoids repeating
--   the full effect row's type at every call site.
runBranchOpGitMain
  :: Sem '[BranchOp Main, StoryStorage, Git, State GitState, Fail] a
  -> Sem '[StoryStorage, Git, State GitState, Fail] a
runBranchOpGitMain = runBranchOpGit (BranchName "main")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

storeMsg :: SM.StorageM m => T.Text -> SM.StorageT m TickId
storeMsg msg = SM.storeTick (draft msg) >>= either fail return

-- | Append content to a file in the working tree (or create it if absent).
appendFileS :: SM.StorageM m => FilePath -> BS.ByteString -> SM.StorageT m ()
appendFileS path content = do
  exists <- SM.fileExistsS path
  base   <- if exists then SM.readFileS path else return BS.empty
  SM.writeFileS path (base <> content)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "StorageT tick chain" $ do
    it "store advances the head" $ do
      let result = runSM $ do
            t1 <- storeMsg "first paragraph"
            t2 <- storeMsg "second paragraph"
            return (t1 /= t2)
      result `shouldBe` Right True

    it "get returns the most recently stored tick" $ do
      let result = runSM $ do
            _ <- storeMsg "first"
            _ <- storeMsg "second"
            tick <- SM.getTick
            return (tickMessage (tickData tick))
      result `shouldBe` Right "second"

    it "drop rewinds the tick pointer to the previous tick" $ do
      let result = runSM $ do
            _ <- storeMsg "first"
            _ <- storeMsg "second"
            SM.dropTick
            tick <- SM.getTick
            return (tickMessage (tickData tick))
      result `shouldBe` Right "first"

    it "drop at root is a no-op" $ do
      let result = runSM $ do
            t0 <- fmap tickId SM.getTick
            SM.dropTick
            t1 <- fmap tickId SM.getTick
            return (t0 == t1)
      result `shouldBe` Right True

    it "follow collects all messages in order from head" $ do
      let result = runSM $ do
            _ <- storeMsg "one"
            _ <- storeMsg "two"
            _ <- storeMsg "three"
            SM.followChain [] $ \acc tick ->
              case tickParent tick of
                Nothing -> (acc, Nothing)
                Just p  -> (tickMessage (tickData tick) : acc, Just p)
      result `shouldBe` Right ["one", "two", "three"]

    it "stored tick ids are unique" $
      property $ \(Positive n) -> n <= 20 ==>
        let result = runSM $
              mapM (\i -> storeMsg (T.pack ("paragraph " <> show (i :: Int)))) [1..n]
        in case result of
          Left err  -> counterexample err False
          Right ids -> length ids === length (nub ids)

  describe "StorageT.at" $ do
    it "at (replay) rewrites a tick and replays the tail" $ do
      let result = runSM $ do
            t1 <- storeMsg "one"
            _  <- storeMsg "two"
            _  <- storeMsg "three"
            eRes <- SM.at True t1 $ storeMsg "one (revised)"
            (_revisedId, mapping) <- either fail return eRes
            tick <- SM.getTick
            return (tickMessage (tickData tick), length mapping)
      result `shouldBe` Right ("three", 2)

    it "readAt (no replay) leaves the chain untouched" $ do
      let result = runSM $ do
            t1 <- storeMsg "one"
            _  <- storeMsg "two"
            headBefore <- fmap tickId SM.getTick
            eRes <- SM.at False t1 SM.getTick
            (readTick, mapping) <- either fail return eRes
            headAfter <- fmap tickId SM.getTick
            return (tickMessage (tickData readTick), mapping, headBefore == headAfter)
      result `shouldBe` Right ("one", [], True)

    it "at fails cleanly when tick is not in branch history" $ do
      let result = runSM $ do
            _ <- storeMsg "tick one"
            SM.at True (TickId "nonexistent") (return ())
      case result of
        Right (Left err) -> err `shouldContain` "not found in branch history"
        _                -> expectationFailure "expected at to fail on unknown tick"

  describe "StorageT.withFS" $ do
    it "atWithFS checks out target tick's filesystem for the inner action" $ do
      let result = runSM $ do
            appendFileS "scene.md" "p1\n"
            t1 <- storeMsg "tick one"
            appendFileS "scene.md" "p2\n"
            _  <- storeMsg "tick two"
            eRes <- SM.at True t1 $ SM.withFS (SM.readFileS "scene.md")
            (content, _mapping) <- either fail return eRes
            return content
      result `shouldBe` Right "p1\n"

    it "atWithFS inner filesystem is independent of outer" $ do
      let result = runSM $ do
            appendFileS "scene.md" "p1\n"
            t1 <- storeMsg "tick one"
            appendFileS "scene.md" "p2\n"
            _  <- storeMsg "tick two"
            appendFileS "scene.md" "p3\n"
            _  <- storeMsg "tick three"
            appendFileS "notes.md" "unsaved\n"
            eRes <- SM.at True t1 $ SM.withFS $ do
              appendFileS "scene.md" "p1-revised\n"
              _ <- storeMsg "tick one (revised)"
              return ()
            (_ :: (), _mapping) <- either fail return eRes
            (,) <$> SM.readFileS "scene.md" <*> SM.readFileS "notes.md"
      result `shouldBe` Right ("p1\np2\np3\n", "unsaved\n")

    it "reset discards pending changes and restores head tick's tree" $ do
      let result = runSM $ do
            appendFileS "scene.md" "committed\n"
            _ <- storeMsg "tick one"
            appendFileS "scene.md" "pending\n"
            SM.resetTree
            SM.readFileS "scene.md"
      result `shouldBe` Right "committed\n"

    it "drop preserves unstored writes" $ do
      let result = runSM $ do
            appendFileS "scene.md" "committed\n"
            _ <- storeMsg "tick one"
            appendFileS "scene.md" "uncommitted\n"
            SM.dropTick
            SM.readFileS "scene.md"
      result `shouldBe` Right "committed\nuncommitted\n"

  describe "StorageT append-only invariant" $ do
    it "store rejects non-append modification" $ do
      let result = runSM $ do
            appendFileS "scene.md" "original\n"
            _ <- storeMsg "tick one"
            SM.writeFileS "scene.md" "replaced\n"
            storeMsg "tick two"
      case result of
        Left err -> err `shouldContain` "non-append"
        Right _  -> expectationFailure "expected store to fail on non-append"

  describe "StorageT.replaceTick" $ do
    it "amends a tick in place and a subsequent at replays the tail" $ do
      -- Chain: t1 -> t2 -> t3, each appending one paragraph.
      -- at True t1 (drop; write; replaceTick) rewrites t1's content, and
      -- the replay on unwind carries t2/t3 forward on top of it. The
      -- mapping records all replayed ticks (t1' from replaceTick, t2' and
      -- t3' from the outer at's own rebuild).
      let result = runSM $ do
            appendFileS "scene.md" "p1\n"
            t1 <- storeMsg "t1"
            appendFileS "scene.md" "p2\n"
            _  <- storeMsg "t2"
            appendFileS "scene.md" "p3\n"
            _  <- storeMsg "t3"

            eRes <- SM.at True t1 $ SM.withFS $ do
              SM.dropTick
              SM.writeFileS "scene.md" "p1-revised\n"
              SM.replaceTick t1 (draft "t1'") >>= either fail return

            (_newT1, mapping) <- either fail return eRes

            newHead <- fmap tickId SM.getTick
            eRead <- SM.at False newHead $ SM.withFS (SM.readFileS "scene.md")
            (content, _) <- either fail return eRead
            return (content, length mapping)
      result `shouldBe` Right ("p1-revised\np2\np3\n", 2)

  describe "StorageT working-tree file access" $ do
    it "written file can be read back" $ do
      let result = runSM $ do
            SM.writeFileS "hello.txt" (BSC.pack "hello world")
            SM.readFileS "hello.txt"
      result `shouldBe` Right (BSC.pack "hello world")

    it "listFilesS returns written files in directory" $ do
      let result = runSM $ do
            SM.writeFileS "a.txt" "a"
            SM.writeFileS "b.txt" "b"
            fmap (nub . (++ [])) $ SM.listFilesS "/"
      case result of
        Left err -> expectationFailure err
        Right fs -> fs `shouldMatchList` ["a.txt", "b.txt"]

    it "removeS deletes a file" $ do
      let result = runSM $ do
            SM.writeFileS "gone.txt" "bye"
            SM.removeS False "gone.txt"
            SM.fileExistsS "gone.txt"
      result `shouldBe` Right False

    it "createDirectoryS creates an explicit directory entry" $ do
      let result = runSM $ do
            SM.createDirectoryS "subdir"
            SM.fileExistsS "subdir"
      result `shouldBe` Right True
