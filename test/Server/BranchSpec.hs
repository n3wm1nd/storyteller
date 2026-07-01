{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.BranchSpec (spec) where

import qualified Data.Text as T
import Data.Maybe (isJust)
import Test.Hspec

import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail)
import Polysemy.State (State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Git
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
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
-- ---------------------------------------------------------------------------


withBranch_ :: BranchName -> Sem (TestEffects '[]) a -> Either String a
withBranch_ name action = run $ testStack (createBranch name >> action)

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

    it "returns Nothing for a non-existent branch" $
      (run $ testStack (branchState (BranchName "missing")))
        `shouldBe` Right Nothing

    it "returns Just after branch creation" $
      withBranch_ (BranchName "test") (branchState (BranchName "test"))
        `shouldSatisfy` either (const False) isJust

    it "head is always a member of the tick list" $
      withBranch_ (BranchName "test") (branchState (BranchName "test"))
        `shouldSatisfy` either (const False) (maybe True (headIsIn . snd))

    it "fresh branch contains only the root tick" $ do
      let result = withBranch_ (BranchName "test") (branchState (BranchName "test"))
      case result of
        Left err -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just (_, upd)) ->
          tickKinds upd `shouldBe` ["root"]

  describe "addNote" $ do

    it "fails when the ref tick does not exist" $
      withBranch_ (BranchName "test")
          (addNote (BranchName "test") (TickId "nonexistent") "hello")
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a note tick visible in branchState" $ do
      let result = withBranch_ (BranchName "test") $ do
            -- root tick is the only tick; use its id as the ref
            mState <- branchState (BranchName "test")
            case mState of
              Nothing -> fail "branch not found"
              Just (_, upd) -> do
                let refId = TickId (updateHead upd)
                addNote (BranchName "test") refId "a note"
                fmap (tickKinds . snd) <$> branchState (BranchName "test")
      case result of
        Left err -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just kinds) -> kinds `shouldContain` ["note"]

    it "head still points to a valid tick after adding a note" $ do
      let result = withBranch_ (BranchName "test") $ do
            mState <- branchState (BranchName "test")
            case mState of
              Nothing -> fail "branch not found"
              Just (_, upd) -> do
                addNote (BranchName "test") (TickId (updateHead upd)) "note"
                branchState (BranchName "test")
      case result of
        Left err -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just (_, upd)) -> headIsIn upd `shouldBe` True

  describe "deleteTickFromBranch" $ do

    it "deleted tick no longer appears in branchState" $ do
      let result = withBranch_ (BranchName "test") $ do
            mState <- branchState (BranchName "test")
            case mState of
              Nothing -> fail "branch not found"
              Just (_, upd) -> do
                let refId = TickId (updateHead upd)
                noteId <- storeAs_ (BranchName "test") (Note refId "to delete")
                deleteTickFromBranch (BranchName "test") noteId
                fmap (tickKinds . snd) <$> branchState (BranchName "test")
      case result of
        Left err -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just kinds) -> kinds `shouldNotContain` ["note"]

  describe "moveTickInBranch" $ do

    it "chain length is unchanged after a move" $ do
      let result = withBranch_ (BranchName "test") $ do
            mState <- branchState (BranchName "test")
            case mState of
              Nothing -> fail "branch not found"
              Just (_, upd) -> do
                let refId = TickId (updateHead upd)
                n1 <- storeAs_ (BranchName "test") (Note refId "note1")
                _  <- storeAs_ (BranchName "test") (Note refId "note2")
                before <- fmap (length . updateTicks . snd) <$> branchState (BranchName "test")
                moveTickInBranch (BranchName "test") n1 Nothing
                after <- fmap (length . updateTicks . snd) <$> branchState (BranchName "test")
                return (before, after)
      case result of
        Left err -> expectationFailure err
        Right (b, a) -> b `shouldBe` a

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- Store via the branch effect directly (bypasses withBranch wrapper).
storeAs_ name note =
  runBranchAndFS @TestBranch name $
    storeAs @TestBranch note

data TestBranch
