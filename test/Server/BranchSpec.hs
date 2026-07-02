{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.BranchSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles, writeFile)

import Storyteller.Git (BranchTag, runBranchAndFS)
import Storyteller.Storage (StoryBranch, createBranch, storeAs, store)
import Storyteller.Types

import Server.TestStack

import Server.Branch
import Server.Protocol (Update(..), WireTick(..))

import Prelude hiding (writeFile)

-- ---------------------------------------------------------------------------
-- Runner
--
-- SessionEffects requires Random/Sleep/Time/LLM which aren't needed by
-- the pure branch operations. We run only what addNote / moveTickInBranch /
-- deleteTickFromBranch / branchState actually touch.
--
-- Branch functions assume their scope ('BranchOpen') is already open, same
-- as a real connection: it's entered once here, wrapping the whole action,
-- rather than per call.
-- ---------------------------------------------------------------------------

withBranch_
  :: BranchName
  -> Sem ( StoryBranch Main
         : FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : TestEffects '[] ) a
  -> Either String a
withBranch_ name action = run $ testStack $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Simulate a write made by a wholly separate connection: opens its own
--   fresh 'Main' scope (its own 'WorkingTree' load + commit + head), nested
--   inside an already-open outer scope. The outer scope's own state was
--   loaded before this runs and, by design, won't see it — see
--   'Storyteller.Git.runStoryBranchGit' and the tests below.
externalWrite :: BranchName -> FilePath -> Sem (TestEffects '[]) TickId
externalWrite name path = runBranchAndFS @Main name $ do
  writeFile @(BranchTag Main) path "content"
  store @Main "external write"

tickIds :: Update -> [T.Text]
tickIds = map wtTickId . updateTicks

tickKinds :: Update -> [T.Text]
tickKinds = map wtKind . updateTicks

headIsIn :: Update -> Bool
headIsIn upd = updateHead upd `elem` tickIds upd

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "branchState" $ do

    it "returns state after branch creation" $
      withBranch_ (BranchName "test") branchState
        `shouldSatisfy` either (const False) (const True)

    it "head is always a member of the tick list" $
      withBranch_ (BranchName "test") branchState
        `shouldSatisfy` either (const False) (headIsIn . snd)

    it "fresh branch contains only the root tick" $ do
      let result = withBranch_ (BranchName "test") branchState
      case result of
        Left err       -> expectationFailure err
        Right (_, upd) -> tickKinds upd `shouldBe` ["root"]

  describe "branchStateSince" $ do

    -- 'runStoryBranchGit' takes its head as a point-in-time snapshot from
    -- whenever the scope was opened and never reaches back out afterwards —
    -- deliberately: it's what makes the 'withStorage' transaction boundary
    -- and this scope's snapshot semantics agree, both syncing exactly once,
    -- at open (see the docs on 'Storyteller.Git.runStoryBranchGit'). So a
    -- still-open scope does not see a write made by a separately-opened
    -- one, for either the raw 'WorkingTree' (already true before) or
    -- 'branchStateSince' (StoryStorage-backed, previously always live).
    -- Freshness comes from reopening the scope, not from re-reading within
    -- an already-open one — see 'Server.Branch.Connection's notifier,
    -- which now reopens per notification for exactly this reason.
    it "a still-open scope does not see a write made by a separately-opened one" $ do
      let result = withBranch_ (BranchName "test") $ do
            -- scope A "opens" here — its head and WorkingTree are snapshotted now
            (before, _) <- branchStateSince Nothing
            -- another connection writes a file via its own, separate scope
            _ <- raise . raise . raise . raise $ externalWrite (BranchName "test") "new.txt"
            stillTree <- listAllFiles @(BranchTag Main) "/"
            (stillSince, _) <- branchStateSince Nothing
            return (before, stillTree, stillSince)
      case result of
        Left err                              -> expectationFailure err
        Right (before, stillTree, stillSince) -> do
          before     `shouldNotContain` ["new.txt"]
          stillTree  `shouldNotContain` ["new.txt"]
          stillSince `shouldNotContain` ["new.txt"]

    it "reopening the scope sees a write made while it was closed" $ do
      let result = run $ testStack $ do
            _ <- createBranch (BranchName "test")
            _ <- externalWrite (BranchName "test") "new.txt"
            runBranchAndFS @Main (BranchName "test") (fst <$> branchStateSince Nothing)
      result `shouldBe` Right ["new.txt"]

  describe "addNote" $ do

    it "fails when the ref tick does not exist" $
      withBranch_ (BranchName "test") (addNote (TickId "nonexistent") "hello")
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a note tick visible in branchState" $ do
      let result = withBranch_ (BranchName "test") $ do
            -- root tick is the only tick; use its id as the ref
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            addNote refId "a note"
            tickKinds . snd <$> branchState
      case result of
        Left err    -> expectationFailure err
        Right kinds -> kinds `shouldContain` ["note"]

    it "head still points to a valid tick after adding a note" $ do
      let result = withBranch_ (BranchName "test") $ do
            (_, upd) <- branchState
            addNote (TickId (updateHead upd)) "note"
            branchState
      case result of
        Left err       -> expectationFailure err
        Right (_, upd) -> headIsIn upd `shouldBe` True

  describe "deleteTickFromBranch" $ do

    it "deleted tick no longer appears in branchState" $ do
      let result = withBranch_ (BranchName "test") $ do
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            noteId <- storeAs @Main (Note refId "to delete")
            deleteTickFromBranch noteId
            tickKinds . snd <$> branchState
      case result of
        Left err    -> expectationFailure err
        Right kinds -> kinds `shouldNotContain` ["note"]

  describe "moveTickInBranch" $ do

    it "chain length is unchanged after a move" $ do
      let result = withBranch_ (BranchName "test") $ do
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            n1 <- storeAs @Main (Note refId "note1")
            _  <- storeAs @Main (Note refId "note2")
            before <- length . updateTicks . snd <$> branchState
            moveTickInBranch n1 Nothing
            after <- length . updateTicks . snd <$> branchState
            return (before, after)
      case result of
        Left err     -> expectationFailure err
        Right (b, a) -> b `shouldBe` a
