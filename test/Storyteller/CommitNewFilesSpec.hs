{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Core.Edit.commitFiles' reconciles a brand-new path (no
-- atom history yet) via 'storeNewFiles' — this pins that each such path
-- gets its own "created" tick (empty content) plus, if the working tree
-- already had content for it (e.g. an upload or Track/CharGen result), a
-- separate "atom" tick for that content — rather than one generic batch
-- tick covering every new file at once with its content baked straight in.
module Storyteller.CommitNewFilesSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, writeFile)
import Prelude hiding (writeFile)

import Git.Mock
import Runix.Git (Git)

import Storyteller.Core.Git
import Storyteller.Core.Storage hiding (get, drop)
import qualified Storyteller.Core.Storage as S
import Storyteller.Core.Types
import Storyteller.Core.Edit (commitFiles)

data Main

runCommit
  :: Sem '[ StoryBranch Main
          , FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , StoryStorage
          , Git
          , State WorkingTree
          , State GitState
          , Fail
          ] a
  -> Either String a
runCommit action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . evalState emptyWorkingTree
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runStoryFSGit @Main (BranchName "main")
        . runStoryBranchGit @Main (BranchName "main")
        . subsume_
        $ action

spec :: Spec
spec = describe "commitFiles on brand-new paths" $ do

  it "a new file with content gets a created tick then an atom tick" $ do
    let result = runCommit $ do
          writeFile @(BranchTag Main) "scene.md" "hello\n"
          _ <- commitFiles @(BranchTag Main) @Main ["scene.md"]
          fileTicks @Main "scene.md"
    case result of
      Left err               -> expectationFailure err
      Right [created, atom]  -> do
        S.ftKind created `shouldBe` "created"
        S.ftContent created `shouldBe` Nothing
        S.ftKind atom    `shouldBe` "atom"
        S.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))

  it "a new file with no content yet gets only a created tick" $ do
    let result = runCommit $ do
          writeFile @(BranchTag Main) "scene.md" ""
          _ <- commitFiles @(BranchTag Main) @Main ["scene.md"]
          fileTicks @Main "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map S.ftKind ticks `shouldBe` ["created"]

  it "multiple new files each get their own created (+ atom) ticks, independently" $ do
    let result = runCommit $ do
          writeFile @(BranchTag Main) "a.md" "A\n"
          writeFile @(BranchTag Main) "b.md" ""
          _ <- commitFiles @(BranchTag Main) @Main ["a.md", "b.md"]
          aTicks <- fileTicks @Main "a.md"
          bTicks <- fileTicks @Main "b.md"
          return (aTicks, bTicks)
    case result of
      Left err -> expectationFailure err
      Right (aTicks, bTicks) -> do
        map S.ftKind aTicks `shouldBe` ["created", "atom"]
        map S.ftKind bTicks `shouldBe` ["created"]
