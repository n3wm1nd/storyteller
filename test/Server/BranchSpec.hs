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
--   fresh 'Main' scope (its own 'WorkingTree' load + commit), nested inside
--   an already-open outer scope. The outer scope's own 'WorkingTree' was
--   loaded before this runs and — this is exactly the bug 'branchStateSince'
--   guards against — won't see it without an explicit 'reset'.
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

    -- Regression test for a real bug: a connection's long-lived scope loads
    -- its 'WorkingTree' once, at entry, and nothing refreshes it afterwards
    -- except its own writes. A background listener that never writes (like
    -- the notify thread in Server.Branch.Connection) would otherwise push a
    -- file list frozen at whatever it was when the connection opened,
    -- silently missing every file added by other connections after that.
    it "sees files written by another scope after the fact, not just its own" $ do
      let result = withBranch_ (BranchName "test") $ do
            -- scope A "opens" here — its WorkingTree is loaded at this point
            (before, _) <- branchStateSince Nothing
            -- another connection writes a file via its own, separate scope
            _ <- raise . raise . raise . raise $ externalWrite (BranchName "test") "new.txt"
            -- listAllFiles alone reads scope A's now-stale cached WorkingTree
            stale <- listAllFiles @(BranchTag Main) "/"
            (after, _) <- branchStateSince Nothing
            return (before, stale, after)
      case result of
        Left err               -> expectationFailure err
        Right (before, stale, after) -> do
          before `shouldNotContain` ["new.txt"]
          stale  `shouldNotContain` ["new.txt"]
          after  `shouldContain`    ["new.txt"]

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
