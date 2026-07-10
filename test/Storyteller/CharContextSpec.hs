{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.CharContextSpec (spec) where

import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock
import Runix.FileSystem (writeFile)

import Prelude hiding (writeFile)

import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent.CharContext (readCharFiles)

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
