{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.TrackerSpec (spec) where

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
import Storyteller.Writer.Agent.Tracker (trackBranch)
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

spec :: Spec
spec = do
  describe "trackBranch (effect), restricted to one file" $ do
    it "copies atoms from source to tracker when tracker is empty" $ do
      let result = runTwoTrack $ do
            -- Write two atoms to source.
            _ <- runStorage @Source (Ops.addAtom "story.md" "paragraph one")
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nparagraph two")
            -- Track into tracker.
            tids <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
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
            tids1 <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            -- Add another atom to source.
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\natom two")
            -- Second track: should only copy the new atom.
            tids2 <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
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
            _ <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            -- Tracker adds its own tick (no ref to source).
            writeFile @(BranchTag Tracker) "notes.md" "author note"
            _ <- runStorage @Tracker (Core.store (Core.NonAtom [] "own tick"))
            -- Add new source atom.
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nnew atom")
            -- Second track: should only copy new source atom.
            tids <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            storyContent <- readFile @(BranchTag Tracker) "story.md"
            noteContent  <- fileExists @(BranchTag Tracker) "notes.md"
            return (length tids, storyContent, noteContent)
      case result of
        Left err -> expectationFailure err
        Right (n, storyContent, notesExist) -> do
          n `shouldBe` 1
          storyContent `shouldBe` "source atom\n\nnew atom"
          notesExist `shouldBe` True

    it "ignores atoms on other source files even though they're new" $ do
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "story.md" "story atom")
            _ <- runStorage @Source (Ops.addAtom "other.md" "other atom")
            tids <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            content <- readFile @(BranchTag Tracker) "story.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 1
          content `shouldBe` "story atom"

  describe "trackBranch (effect), unrestricted (every source file)" $ do
    it "copies atoms from every source file into the same journal file" $ do
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "ch1.md" "chapter one")
            _ <- runStorage @Source (Ops.addAtom "ch2.md" "chapter two")
            tids <- trackBranch @Source @Tracker Nothing keepAll "journal.md"
            content <- readFile @(BranchTag Tracker) "journal.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 2
          content `shouldBe` "chapter onechapter two"

    it "a second unrestricted track only copies what's new across every file" $ do
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "ch1.md" "chapter one")
            tids1 <- trackBranch @Source @Tracker Nothing keepAll "journal.md"
            _ <- runStorage @Source (Ops.addAtom "ch2.md" "chapter two")
            tids2 <- trackBranch @Source @Tracker Nothing keepAll "journal.md"
            content <- readFile @(BranchTag Tracker) "journal.md"
            return (length tids1, length tids2, content)
      case result of
        Left err -> expectationFailure err
        Right (n1, n2, content) -> do
          n1 `shouldBe` 1
          n2 `shouldBe` 1
          content `shouldBe` "chapter onechapter two"

  describe "trackBranch with onlyWhilePresent (character presence-gated tracking)" $ do
    it "copies only the atom written while the character was present" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- recordPresence @Source "story.md" character Enter
            _ <- runStorage @Source (Ops.addAtom "story.md" "she arrived.")
            _ <- recordPresence @Source "story.md" character Leave
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\nmeanwhile, elsewhere.")
            tids <- trackBranch @Source @Tracker (Just "story.md") (onlyWhilePresent character) "story.md"
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
            tids <- trackBranch @Source @Tracker (Just "story.md") (onlyWhilePresent character) "story.md"
            return (length tids)
      result `shouldBe` Right 0

    it "copies everything once the character re-enters, still skipping the absent gap" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- runStorage @Source (Ops.addAtom "story.md" "absent.")
            _ <- recordPresence @Source "story.md" character Enter
            _ <- runStorage @Source (Ops.addAtom "story.md" "\n\npresent.")
            tids <- trackBranch @Source @Tracker (Just "story.md") (onlyWhilePresent character) "story.md"
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
            _ <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            -- Track again with no new source atoms.
            tids <- trackBranch @Source @Tracker (Just "story.md") keepAll "story.md"
            return (length tids)
      result `shouldBe` Right 0

    it "presence doesn't carry across files: a character present at the end of one file starts absent in the next" $ do
      let character = Character (BranchName "tracker")
      let result = runTwoTrack $ do
            _ <- recordPresence @Source "ch1.md" character Enter
            _ <- runStorage @Source (Ops.addAtom "ch1.md" "present in ch1.")
            _ <- runStorage @Source (Ops.addAtom "ch2.md" "not present in ch2.")
            tids <- trackBranch @Source @Tracker Nothing (onlyWhilePresent character) "journal.md"
            content <- readFile @(BranchTag Tracker) "journal.md"
            return (length tids, content)
      case result of
        Left err -> expectationFailure err
        Right (n, content) -> do
          n `shouldBe` 1
          content `shouldBe` "present in ch1."
