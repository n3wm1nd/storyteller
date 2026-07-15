{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Storage mechanics of 'Storyteller.Writer.Agent.Tasks' -- sync-marker
--   placement, delta gathering, checkpoint timing. The LLM calls
--   themselves ('tasksReconcileAgent'\/'tasksGenerateAgent') aren't
--   exercised here, same "no agent's real queryLLM call is unit tested"
--   convention as 'Storyteller.ChapterSummarizerSpec' -- 'syncTasksWith'\/
--   'suggestTasksWith' take the LLM step as a parameter for exactly this
--   reason, so a pure stub can stand in.
module Storyteller.TasksSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.Output (Output, runOutputList, output)
import Polysemy.State (evalState)

import Git.Mock
import Runix.FileSystem (readFile)

import Prelude hiding (readFile)

import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Types
import Storyteller.Writer.Agent.Tasks

data TestBranch

-- | Run an action against one branch, plus an 'Output' layer a stub
--   reconcile\/generate function can record every call it received into,
--   purely -- 'runOutputList' peels it back into the ordinary result
--   tuple, same as 'Agent.Integration.Harness.recordToolCalls' does for
--   the real LLM interceptor.
runOne action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  . runOutputList @(Text, Text)
  $ do
      _ <- createBranch (BranchName "branch")
      runBranchAndFS @TestBranch (BranchName "branch") action

-- | A stub reconcile\/generate step: records every @(current, material)@
--   pair it's called with, and returns whatever @respond@ says.
recordingAgent :: Member (Output (Text, Text)) r => Text -> Text -> Text -> Sem r Text
recordingAgent respond current material = do
  output (current, material)
  return respond

keepAll :: FilePath -> Bool
keepAll = const True

spec :: Spec
spec = do
  describe "syncTasksWith" $ do
    it "does nothing when there is no new source material" $
      runOne (syncTasksWith @TestBranch (recordingAgent "unused") keepAll "tasks.md")
        `shouldBe` Right ([], False)

    it "reconciles once when new source material lands, and creates tasks.md fresh" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            changed <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") keepAll "tasks.md"
            content <- readFile @(BranchTag TestBranch) "tasks.md"
            return (changed, content)
      case result of
        Left err -> expectationFailure err
        Right (calls, (changed, content)) -> do
          changed `shouldBe` True
          content `shouldBe` "## Short-term goals\n- find her."
          calls `shouldBe` [("", "she left home.")]

    it "second sync only sends what's new, and folds it against the previous output" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") keepAll "tasks.md"
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "\n\nshe found her.")
            syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n(none -- resolved)") keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          calls `shouldBe`
            [ ("", "she left home.")
            , ("## Short-term goals\n- find her.", "\n\nshe found her.")
            ]

    it "does not resync when nothing changed since the last pass" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") keepAll "tasks.md"
            syncTasksWith @TestBranch (recordingAgent "unused") keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` False
          length calls `shouldBe` 1

    it "old tasks.md content is still reachable after a resync (checkpoint, not overwrite)" $ do
      -- 'Ops.atomHistory' only ever reads the *current* lifetime (it stops
      -- at a removal boundary, by design -- see its own Haddock), so it
      -- can't be the read that proves this; a raw walk of every tick in
      -- the chain, ignoring removal tags entirely, is the one that
      -- actually exercises "checkpointed, not lost".
      let allTasksContent :: Core.StoreM m => Core.StoreT m [Text]
          allTasksContent = Core.follow [] $ \acc _h t -> case t of
            Core.Atom _ p _ content | p == "tasks.md" -> (content : acc, True)
            _                                          -> (acc, True)
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") keepAll "tasks.md"
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "\n\nshe found her.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n(none -- resolved)") keepAll "tasks.md"
            runStorage @TestBranch allTasksContent
      case result of
        Left err -> expectationFailure err
        Right (_, history) -> T.unpack (T.concat history) `shouldContain` "find her."

    it "isSource restricts which files count as new material" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "relevant.")
            _ <- runStorage @TestBranch (Ops.addAtom "scratch.md" "irrelevant.")
            syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- x") (== "journal.md") "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          calls `shouldBe` [("", "relevant.")]

    it "does nothing when the only new material is filtered out" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "scratch.md" "irrelevant.")
            syncTasksWith @TestBranch (recordingAgent "unused") (== "journal.md") "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` False
          calls `shouldBe` []

  describe "suggestTasksWith" $ do
    it "reads the full current content of every source file, not just a delta" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "\n\nshe found her.")
            suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- rebuild her life.") keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          calls `shouldBe` [("", "she left home.\n\nshe found her.")]

    it "a suggest pass still advances the sync marker, so a later sync doesn't reprocess it" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- rebuild her life.") keepAll "tasks.md"
            syncTasksWith @TestBranch (recordingAgent "unused") keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` False
          -- Only the suggest pass's own call is recorded; the follow-up
          -- sync found nothing new and never called its stub.
          length calls `shouldBe` 1

    it "does nothing when there is no source material at all" $
      runOne (suggestTasksWith @TestBranch (recordingAgent "unused") keepAll "tasks.md")
        `shouldBe` Right ([], False)
