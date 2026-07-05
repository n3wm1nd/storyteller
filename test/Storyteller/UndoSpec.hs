{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Core.Undo' pins:
--
--   * every real story-branch ref write appends one undo-log entry;
--   * a write to a non-story ref (the undo log's own ref, in particular)
--     does not itself recurse into another entry;
--   * 'listUndo' walks newest-first, and each entry's 'undoRefs' matches
--     the branch heads at the moment it was recorded;
--   * 'resetToUndo' restores branch refs to a past entry -- including
--     dropping branches created after that point -- and that reset is
--     itself picked up as new entries rather than special-cased.
module Storyteller.UndoSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Data.Maybe (isJust)
import Git.Mock
import Runix.Git (RefName(..), ObjectHash(..), resolveRef)
import Runix.Time (runTimeConst)
import Data.Time (UTCTime(..), fromGregorian)

import Storyteller.Core.Git (runStoryStorageGit, storyRefPrefix, isStoryRef)
import Storyteller.Core.Storage
import Storyteller.Core.Types
import Storyteller.Core.Undo

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2020 1 1) 0

runUndoTest action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runTimeConst testTime
  . runUndoGit storyRefPrefix
  . interceptGitUndoLog isStoryRef
  . runStoryStorageGit
  $ action

branchRef :: BranchName -> RefName
branchRef (BranchName n) = RefName (storyRefPrefix <> n)

spec :: Spec
spec = describe "Undo" $ do

  it "records exactly one entry per real story-branch ref write" $ do
    let result = runUndoTest $ do
          _ <- createBranch (BranchName "main")
          _ <- createBranch (BranchName "other")
          listUndo
    case result of
      Left err      -> expectationFailure err
      Right entries -> length entries `shouldBe` 2

  it "an entry's undoRefs match the branch heads at the moment it was recorded" $ do
    let result = runUndoTest $ do
          _ <- createBranch (BranchName "main")
          Just mainAfterCreate <- getBranch (BranchName "main")
          _ <- createBranch (BranchName "other")
          entries <- listUndo
          return (mainAfterCreate, entries)
    case result of
      Left err -> expectationFailure err
      Right (mainAfterCreate, newest : _) -> do
        -- newest first: the entry recorded right after "other" was
        -- created must still show "main" unchanged from its own creation
        lookup (branchRef (BranchName "main")) (undoRefs newest)
          `shouldBe` Just (ObjectHash (unTickId (branchHead mainAfterCreate)))
        -- and it must also show the just-created "other"
        lookup (branchRef (BranchName "other")) (undoRefs newest)
          `shouldNotBe` Nothing
      Right (_, []) -> expectationFailure "expected at least one undo entry"

  it "resetToUndo restores a branch's head and drops branches created after that point" $ do
    -- Checked against raw refs ('resolveRef'), not 'getBranch': 'resetToUndo'
    -- writes directly through 'Runix.Git', bypassing 'StoryStorage's own
    -- per-transaction ref overlay, so a read through that overlay in the
    -- same scope would still see "other" as created. 'Undo' is deliberately
    -- independent of 'StoryStorage' (see the module haddock) -- this is the
    -- reason why, tested directly.
    let result = runUndoTest $ do
          _ <- createBranch (BranchName "main")
          firstEntryId : _ <- map undoId <$> listUndo
          _ <- createBranch (BranchName "other")
          resetToUndo firstEntryId
          mainRef  <- resolveRef (branchRef (BranchName "main"))
          otherRef <- resolveRef (branchRef (BranchName "other"))
          return (mainRef, otherRef)
    case result of
      Left err                    -> expectationFailure err
      Right (mainRef, otherRef) -> do
        mainRef `shouldSatisfy` isJust
        otherRef `shouldBe` Nothing

  it "resetToUndo's own ref writes are themselves recorded in the log" $ do
    let result = runUndoTest $ do
          _ <- createBranch (BranchName "main")
          firstEntryId : _ <- map undoId <$> listUndo
          _ <- createBranch (BranchName "other")
          countBeforeReset <- length <$> listUndo
          resetToUndo firstEntryId
          countAfterReset <- length <$> listUndo
          return (countBeforeReset, countAfterReset)
    case result of
      Left err                     -> expectationFailure err
      Right (beforeCount, afterCount) -> afterCount `shouldSatisfy` (> beforeCount)
