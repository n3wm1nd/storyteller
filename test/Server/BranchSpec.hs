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
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Git (BranchTag, runBranchAndFS)
import Storyteller.Storage (StoryBranch, createBranch, storeAs)
import Storyteller.Types

import Server.TestStack

import Server.Branch
import Server.Protocol (Update(..), WireTick(..))

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
