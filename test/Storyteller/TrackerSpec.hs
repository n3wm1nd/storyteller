{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.TrackerSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (State, evalState)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , writeFile, readFile, fileExists )

import Prelude hiding (readFile, writeFile)

import Storyteller.Git hiding (emptyWorkingTree)
import Storyteller.Storage hiding (drop)
import Storyteller.Types
import Storyteller.Agent.Tracker (trackBranch, dropUntilAfterLastSynced)

-- ---------------------------------------------------------------------------
-- Phantoms
-- ---------------------------------------------------------------------------

data Source
data Tracker

-- ---------------------------------------------------------------------------
-- Two-branch runner
-- ---------------------------------------------------------------------------

-- | Run an action with both Source and Tracker branches available.
--   Both share the same WorkingTree and GitState (correct — one in-memory git).
--   Effect row is inferred; interpreters peel from the action outward.
runTwoTrack action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runBranchAndFS @Source  (BranchName "source")
  . runBranchAndFS @Tracker (BranchName "tracker")
  . runStoryStorageGit
  $ action

-- ---------------------------------------------------------------------------
-- Pure tests
-- ---------------------------------------------------------------------------

mkTick :: Int -> [TickId] -> Tick
mkTick n refs = Tick
  { tickId      = TickId (T.pack (show n))
  , tickParent  = if n == 0 then Nothing else Just (TickId (T.pack (show (n-1))))
  , tickRefs    = refs
  , tickMessage = "tick " <> T.pack (show n)
  }

spec :: Spec
spec = do
  describe "dropUntilAfterLastSynced" $ do
    it "no synced refs: returns all ticks" $ do
      let ticks = map (\n -> mkTick n []) [0..2]
      dropUntilAfterLastSynced Set.empty ticks `shouldBe` ticks

    it "all ticks synced: returns empty" $ do
      let ticks = map (\n -> mkTick n []) [0..2]
          synced = Set.fromList (map tickId ticks)
      dropUntilAfterLastSynced synced ticks `shouldBe` []

    it "last two ticks new" $ do
      let ticks = map (\n -> mkTick n []) [0..3]
          synced = Set.singleton (TickId "1")
      dropUntilAfterLastSynced synced ticks `shouldBe` drop 2 ticks

    it "only head synced: returns empty" $ do
      let ticks = map (\n -> mkTick n []) [0..3]
          synced = Set.singleton (TickId "3")
      dropUntilAfterLastSynced synced ticks `shouldBe` []

    it "last synced is second-to-last: returns only last" $ do
      let ticks = map (\n -> mkTick n []) [0..3]
          synced = Set.singleton (TickId "2")
      dropUntilAfterLastSynced synced ticks `shouldBe` [mkTick 3 []]

  describe "trackBranch (effect)" $ do
    it "copies atoms from source to tracker when tracker is empty" $ do
      let result = runTwoTrack $ do
            _ <- createBranch (BranchName "source")
            _ <- createBranch (BranchName "tracker")
            -- Write two atoms to source.
            writeFile @(BranchTag Source) "story.md" "paragraph one"
            _ <- store @Source "atom 1"
            writeFile @(BranchTag Source) "story.md" "paragraph one\n\nparagraph two"
            _ <- store @Source "atom 2"
            -- Track into tracker.
            tids <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                      ["story.md"]
            -- Read tracker result.
            content <- readFile @(BranchTag Tracker) "story.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 2
          content `shouldBe` "paragraph one\n\nparagraph two"

    it "does not re-copy already tracked atoms" $ do
      let result = runTwoTrack $ do
            _ <- createBranch (BranchName "source")
            _ <- createBranch (BranchName "tracker")
            writeFile @(BranchTag Source) "story.md" "atom one"
            _ <- store @Source "atom 1"
            -- First track.
            tids1 <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                       ["story.md"]
            -- Add another atom to source.
            writeFile @(BranchTag Source) "story.md" "atom one\n\natom two"
            _ <- store @Source "atom 2"
            -- Second track: should only copy the new atom.
            tids2 <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                       ["story.md"]
            content <- readFile @(BranchTag Tracker) "story.md"
            return (length tids1, length tids2, content)
      case result of
        Left err -> expectationFailure err
        Right (n1, n2, content) -> do
          n1 `shouldBe` 1
          n2 `shouldBe` 1
          content `shouldBe` "atom one\n\natom two"

    it "tracker with own ticks does not confuse sync state" $ do
      let result = runTwoTrack $ do
            _ <- createBranch (BranchName "source")
            _ <- createBranch (BranchName "tracker")
            writeFile @(BranchTag Source) "story.md" "source atom"
            _ <- store @Source "atom 1"
            -- First track.
            _ <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                   ["story.md"]
            -- Tracker adds its own tick (no ref to source).
            writeFile @(BranchTag Tracker) "notes.md" "author note"
            _ <- store @Tracker "own tick"
            -- Add new source atom.
            writeFile @(BranchTag Source) "story.md" "source atom\n\nnew atom"
            _ <- store @Source "atom 2"
            -- Second track: should only copy new source atom.
            tids <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                      ["story.md"]
            storyContent <- readFile @(BranchTag Tracker) "story.md"
            noteContent  <- fileExists @(BranchTag Tracker) "notes.md"
            return (length tids, storyContent, noteContent)
      case result of
        Left err -> expectationFailure err
        Right (n, storyContent, notesExist) -> do
          n `shouldBe` 1
          storyContent `shouldBe` "source atom\n\nnew atom"
          notesExist `shouldBe` True

    it "nothing to track when source has no new atoms" $ do
      let result = runTwoTrack $ do
            _ <- createBranch (BranchName "source")
            _ <- createBranch (BranchName "tracker")
            writeFile @(BranchTag Source) "story.md" "atom one"
            _ <- store @Source "atom 1"
            _ <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                   ["story.md"]
            -- Track again with no new source atoms.
            tids <- trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker)
                      ["story.md"]
            return (length tids)
      result `shouldBe` Right 0
