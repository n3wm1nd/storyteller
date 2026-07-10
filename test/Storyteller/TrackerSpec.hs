{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.TrackerSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock
import Runix.FileSystem (writeFile, readFile, fileExists)

import Prelude hiding (readFile, writeFile)

import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Types
import Storyteller.Writer.Agent.Tracker (trackBranch, dropUntilAfterLastSynced)
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))
import Server.Writer.Branch (onlyWhilePresent)

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
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "source")
      _ <- createBranch (BranchName "tracker")
      runBranchAndFS @Source  (BranchName "source")
        . runBranchAndFS @Tracker (BranchName "tracker")
        $ action

-- ---------------------------------------------------------------------------
-- Pure tests
-- ---------------------------------------------------------------------------

-- | 'trackBranch's filter argument, set to "keep everything" -- these tests
--   are about the sync/dedup mechanics, not the presence-aware filtering
--   'Server.Writer.Branch.onlyWhilePresent' adds on top for the character
--   use case.
keepAll :: Core.StoreM m => Tick -> Core.StoreT m (Maybe Tick)
keepAll tick = pure (Just tick)

mkTick :: Int -> [TickId] -> Tick
mkTick n refs = Tick
  { tickPos  = TickPos
      { posId     = TickId (T.pack (show n))
      , posParent = if n == 0 then Nothing else Just (TickId (T.pack (show (n-1))))
      , posRefs   = refs
      }
  , tickData = TickData
      { tickRefs    = refs
      , tickFields  = []
      , tickMessage = "tick " <> T.pack (show n)
      }
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
            -- Write two atoms to source.
            _ <- runStorage @Source (Ops.addAtom "story.md" "paragraph one")
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nparagraph two")
            -- Track into tracker.
            tids <- trackBranch @Source @Tracker keepAll
                      ("story.md", "story.md")
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
            _ <- runStorage @Source (Ops.addAtom "story.md" "atom one")
            -- First track.
            tids1 <- trackBranch @Source @Tracker keepAll
                       ("story.md", "story.md")
            -- Add another atom to source.
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\natom two")
            -- Second track: should only copy the new atom.
            tids2 <- trackBranch @Source @Tracker keepAll
                       ("story.md", "story.md")
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
            _ <- runStorage @Source (Ops.addAtom "story.md" "source atom")
            -- First track.
            _ <- trackBranch @Source @Tracker keepAll
                   ("story.md", "story.md")
            -- Tracker adds its own tick (no ref to source).
            writeFile @(BranchTag Tracker) "notes.md" "author note"
            _ <- runStorage @Tracker (Core.store (Core.NonAtom [] "own tick"))
            -- Add new source atom.
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nnew atom")
            -- Second track: should only copy new source atom.
            tids <- trackBranch @Source @Tracker keepAll
                      ("story.md", "story.md")
            storyContent <- readFile @(BranchTag Tracker) "story.md"
            noteContent  <- fileExists @(BranchTag Tracker) "notes.md"
            return (length tids, storyContent, noteContent)
      case result of
        Left err -> expectationFailure err
        Right (n, storyContent, notesExist) -> do
          n `shouldBe` 1
          storyContent `shouldBe` "source atom\n\nnew atom"
          notesExist `shouldBe` True

  describe "trackBranch with onlyWhilePresent (character presence-gated tracking)" $ do
    it "copies only the atom written while the character was present" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- recordPresence @Source "story.md" character Enter
            _ <- runStorage @Source (Ops.addAtom "story.md" "she arrived.")
            _ <- recordPresence @Source "story.md" character Leave
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nmeanwhile, elsewhere.")
            tids <- trackBranch @Source @Tracker (onlyWhilePresent character)
                      ("story.md", "story.md")
            content <- readFile @(BranchTag Tracker) "story.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 1
          content `shouldBe` "she arrived."

    it "copies nothing when the character was never present" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "story.md" "nobody's here.")
            tids <- trackBranch @Source @Tracker (onlyWhilePresent character)
                      ("story.md", "story.md")
            return (length tids)
      result `shouldBe` Right 0

    it "copies everything once the character re-enters, still skipping the absent gap" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "story.md" "absent.")
            _ <- recordPresence @Source "story.md" character Enter
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\npresent.")
            tids <- trackBranch @Source @Tracker (onlyWhilePresent character)
                      ("story.md", "story.md")
            content <- readFile @(BranchTag Tracker) "story.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 1
          content `shouldBe` "\n\npresent."

    it "nothing to track when source has no new atoms" $ do
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "story.md" "atom one")
            _ <- trackBranch @Source @Tracker keepAll
                   ("story.md", "story.md")
            -- Track again with no new source atoms.
            tids <- trackBranch @Source @Tracker keepAll
                      ("story.md", "story.md")
            return (length tids)
      result `shouldBe` Right 0
