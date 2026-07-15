{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Storage mechanics of 'Storyteller.Writer.Agent.Tasks' -- sync-marker
--   placement, delta gathering, checkpoint timing, character-context
--   assembly. The LLM calls themselves ('tasksReconcileAgent'\/
--   'tasksGenerateAgent') aren't exercised here, same "no agent's real
--   queryLLM call is unit tested" convention as
--   'Storyteller.ChapterSummarizerSpec' -- 'syncTasksWith'\/
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
import Runix.Logging (loggingNull)

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
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  . runOutputList @(Text, Text, Text)
  $ do
      _ <- createBranch (BranchName "branch")
      runBranchAndFS @TestBranch (BranchName "branch") action

-- | A stub reconcile\/generate step: records every @(characterName,
--   current, material)@ call it received, and returns whatever @respond@
--   says.
recordingAgent :: Member (Output (Text, Text, Text)) r => Text -> Text -> Text -> Text -> Sem r Text
recordingAgent respond characterName current material = do
  output (characterName, current, material)
  return respond

keepAll :: FilePath -> Bool
keepAll = const True

spec :: Spec
spec = do
  describe "syncTasksWith" $ do
    it "does nothing when there is no new source material" $
      runOne (syncTasksWith @TestBranch (recordingAgent "unused") "Mira" keepAll "tasks.md")
        `shouldBe` Right ([], False)

    it "reconciles once when new source material lands, and creates tasks.md fresh" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            changed <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") "Mira" keepAll "tasks.md"
            content <- readFile @(BranchTag TestBranch) "tasks.md"
            return (changed, content)
      case result of
        Left err -> expectationFailure err
        Right (calls, (changed, content)) -> do
          changed `shouldBe` True
          content `shouldBe` "## Short-term goals\n- find her."
          calls `shouldBe` [("Mira", "", "she left home.")]

    it "second sync only sends what's new, and folds it against the previous output" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") "Mira" keepAll "tasks.md"
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "\n\nshe found her.")
            syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n(none -- resolved)") "Mira" keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          calls `shouldBe`
            [ ("Mira", "", "she left home.")
            , ("Mira", "## Short-term goals\n- find her.", "\n\nshe found her.")
            ]

    it "does not resync when nothing changed since the last pass" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") "Mira" keepAll "tasks.md"
            syncTasksWith @TestBranch (recordingAgent "unused") "Mira" keepAll "tasks.md"
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
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- find her.") "Mira" keepAll "tasks.md"
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "\n\nshe found her.")
            _ <- syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n(none -- resolved)") "Mira" keepAll "tasks.md"
            runStorage @TestBranch allTasksContent
      case result of
        Left err -> expectationFailure err
        Right (_, history) -> T.unpack (T.concat history) `shouldContain` "find her."

    it "isSource restricts which files count as new material" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "relevant.")
            _ <- runStorage @TestBranch (Ops.addAtom "scratch.md" "irrelevant.")
            syncTasksWith @TestBranch (recordingAgent "## Short-term goals\n- x") "Mira" (== "journal.md") "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          calls `shouldBe` [("Mira", "", "relevant.")]

    it "does nothing when the only new material is filtered out" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "scratch.md" "irrelevant.")
            syncTasksWith @TestBranch (recordingAgent "unused") "Mira" (== "journal.md") "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` False
          calls `shouldBe` []

  describe "suggestTasksWith" $ do
    it "does nothing when there is no character context at all" $
      runOne (suggestTasksWith @TestBranch (recordingAgent "unused") "Mira" "tasks.md")
        `shouldBe` Right ([], False)

    it "assembles sheet, other context files, and journal into the material, and passes the character name through" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.saveFile "sheet.md" "Mira is a locksmith's daughter.")
            _ <- runStorage @TestBranch (Ops.addAtom "notes.md" "Owes a debt to Kess.")
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- rebuild her life.") "Mira" "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          case calls of
            [(name, current, material)] -> do
              name `shouldBe` "Mira"
              current `shouldBe` ""
              material `shouldSatisfy` T.isInfixOf "Mira is a locksmith's daughter."
              material `shouldSatisfy` T.isInfixOf "Owes a debt to Kess."
              material `shouldSatisfy` T.isInfixOf "she left home."
            other -> expectationFailure ("expected exactly one call, got " <> show (length other))

    -- Regression: an earlier version read the journal via
    -- 'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal', which
    -- (via 'Storage.Tick.recentAtomsOf's 'carriesUniqueInfo' filter) drops
    -- any journal atom whose content is byte-identical to what it refs --
    -- which is every ordinary 'Storyteller.Writer.Agent.Tracker'-copied
    -- entry, since a plain Track copies a scene atom's content verbatim
    -- alongside a ref back to it. That's the right behaviour for ambient
    -- generation context (the source scene is shown separately there --
    -- see Tasks.hs's own Haddock on 'suggestTasksWith'), but silently
    -- starved a Suggest pass of essentially its entire journal, since nothing
    -- else supplies scene content there. This atom (ref + identical content,
    -- exactly Tracker's own shape -- see 'Storyteller.Writer.Agent.Tracker.
    -- copyAtom') must still reach the material.
    it "includes an ordinary Tracker-copied journal entry (ref + content identical to what it refs)" $ do
      let result = runOne $ do
            sourceHash <- runStorage @TestBranch (Ops.addAtom "scene.md" "she left home, quietly.")
            _ <- runStorage @TestBranch (Ops.addAtomWithRefs [sourceHash] "journal.md" "she left home, quietly.")
            suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- x") "Mira" "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          case calls of
            [(_, _, material)] -> material `shouldSatisfy` T.isInfixOf "she left home, quietly."
            other -> expectationFailure ("expected exactly one call, got " <> show (length other))

    it "never reads tasks.md itself as part of the material" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.saveFile "sheet.md" "Mira is a locksmith's daughter.")
            _ <- runStorage @TestBranch (Ops.addAtom "tasks.md" "STALE PRE-EXISTING TASKS CONTENT")
            suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- x") "Mira" "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` True
          case calls of
            [(_, current, material)] -> do
              -- 'current' is tasks.md's *own* prior content, read separately
              -- (this is what a reconcile\/suggest pass is meant to build
              -- on) -- it's 'material' (the character-context read) that
              -- must never also include it.
              current `shouldBe` "STALE PRE-EXISTING TASKS CONTENT"
              material `shouldSatisfy` T.isInfixOf "locksmith"
              material `shouldNotSatisfy` T.isInfixOf "STALE PRE-EXISTING TASKS CONTENT"
            other -> expectationFailure ("expected exactly one call, got " <> show (length other))

    it "a suggest pass still advances the sync marker, so a later sync doesn't reprocess it" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            _ <- suggestTasksWith @TestBranch (recordingAgent "## Long-term goals\n- rebuild her life.") "Mira" "tasks.md"
            syncTasksWith @TestBranch (recordingAgent "unused") "Mira" keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, changed) -> do
          changed `shouldBe` False
          -- Only the suggest pass's own call is recorded; the follow-up
          -- sync found nothing new and never called its stub.
          length calls `shouldBe` 1

  describe "character name resolution" $ do
    it "syncTasksWith prefers sheet.md's own heading over the fallback name" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.saveFile "sheet.md" "# Rosalind\n\nA locksmith's daughter.")
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            syncTasksWith @TestBranch (recordingAgent "unused") "fallback-guess" keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, _) -> case calls of
          [(name, _, _)] -> name `shouldBe` "Rosalind"
          other           -> expectationFailure ("expected exactly one call, got " <> show (length other))

    it "suggestTasksWith prefers sheet.md's own heading over the fallback name" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.saveFile "sheet.md" "# Rosalind\n\nA locksmith's daughter.")
            suggestTasksWith @TestBranch (recordingAgent "unused") "fallback-guess" "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, _) -> case calls of
          [(name, _, _)] -> name `shouldBe` "Rosalind"
          other           -> expectationFailure ("expected exactly one call, got " <> show (length other))

    it "falls back to the given name when sheet.md has no heading" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.saveFile "sheet.md" "just some prose, no heading at all")
            suggestTasksWith @TestBranch (recordingAgent "unused") "fallback-guess" "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, _) -> case calls of
          [(name, _, _)] -> name `shouldBe` "fallback-guess"
          other           -> expectationFailure ("expected exactly one call, got " <> show (length other))

    it "falls back to the given name when there is no sheet.md at all" $ do
      let result = runOne $ do
            _ <- runStorage @TestBranch (Ops.addAtom "journal.md" "she left home.")
            syncTasksWith @TestBranch (recordingAgent "unused") "fallback-guess" keepAll "tasks.md"
      case result of
        Left err -> expectationFailure err
        Right (calls, _) -> case calls of
          [(name, _, _)] -> name `shouldBe` "fallback-guess"
          other           -> expectationFailure ("expected exactly one call, got " <> show (length other))
