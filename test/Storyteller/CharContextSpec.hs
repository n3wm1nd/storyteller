{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.CharContextSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock
import Runix.FileSystem (writeFile)

import Prelude hiding (writeFile)

import Storage.MockStore (runChain)
import Storage.Ops (addAtom, addAtomWithRefs)
import qualified Storage.FS as FS

import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharContextBlock(..))
import Storyteller.Writer.Agent.CharContext (readCharFiles, charSummaryWithJournal)

data CharBranch

-- | A character branch with a sheet and a journal, so a filter predicate
-- has something real to exclude/keep between.
runCharacter action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "character/alice")
      runBranchAndFS @CharBranch (BranchName "character/alice") $ do
        writeFile @(BranchTag CharBranch) "sheet.md" (TE.encodeUtf8 "# Alice\n")
        writeFile @(BranchTag CharBranch) "journal.md" (TE.encodeUtf8 "dear diary...\n")
        action

spec :: Spec
spec = describe "readCharFiles" $ do

  it "keeps every file when the predicate always matches" $ do
    let result = runCharacter (map fst <$> readCharFiles @(BranchTag CharBranch) (const True))
    result `shouldBe` Right ["journal.md", "sheet.md"]

  it "excludes journal.md when the predicate says so, keeping the rest" $ do
    let result = runCharacter (map fst <$> readCharFiles @(BranchTag CharBranch) (/= "journal.md"))
    result `shouldBe` Right ["sheet.md"]

  describe "charSummaryWithJournal" $ do
    it "renders sheet.md plainly and adds no journal section when the journal is empty" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "sheet.md" "# Alice\n"
            charSummaryWithJournal (/= "journal.md") "journal.md" 30 10 2)
      case result of
        Left err -> expectationFailure err
        Right blocks -> blocks `shouldBe` [CharContextBlock "### sheet.md\n\n# Alice\n"]

    it "folds in a curated journal section after the plain files, labelled as the character's own viewpoint" $ do
      let result = fst <$> runChain (do
            _  <- addAtom "sheet.md" "# Alice\n"
            -- The referenced source atom really lives on a different
            -- branch (the scene, not this character's own); here it's on
            -- the same mock chain only so there's a real, readable hash to
            -- reference -- removed from the ambient tree right after so it
            -- doesn't leak into this branch's own plain-file listing.
            s1 <- addAtom "scene.md" "witnessed line"
            _  <- FS.remove "scene.md"
            _  <- addAtomWithRefs [s1] "journal.md" "witnessed line, but I embellished it"
            charSummaryWithJournal (/= "journal.md") "journal.md" 30 10 2)
      case result of
        Left err -> expectationFailure err
        Right blocks -> do
          length blocks `shouldBe` 2
          blocks !! 0 `shouldBe` CharContextBlock "### sheet.md\n\n# Alice\n"
          let CharContextBlock journalText = blocks !! 1
          journalText `shouldSatisfy` T.isInfixOf "witnessed line, but I embellished it"
          journalText `shouldSatisfy` T.isInfixOf "private viewpoint"

    it "drops a journal entry that's still a verbatim copy of its source, keeping the plain files unaffected" $ do
      let result = fst <$> runChain (do
            _  <- addAtom "sheet.md" "# Alice\n"
            s1 <- addAtom "scene.md" "same content"
            _  <- FS.remove "scene.md"
            _  <- addAtomWithRefs [s1] "journal.md" "same content"
            charSummaryWithJournal (/= "journal.md") "journal.md" 30 10 2)
      case result of
        Left err -> expectationFailure err
        Right blocks -> blocks `shouldBe` [CharContextBlock "### sheet.md\n\n# Alice\n"]
