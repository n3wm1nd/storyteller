{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Files living under a subdirectory (e.g. @chapters/ch1.md@) must behave
-- exactly like root-level files across the storage/edit layer. Two bugs this
-- pins:
--
--   * 'commitFiles' walked historical snapshots with a flat @listFiles "/"@
--     that included directory entries, then 'readFile'd each — which fails
--     with "is a directory" the moment any subdirectory exists. Fixed by
--     recursing (see 'Storyteller.Core.Edit.readSnapshotAt').
--
--   * A plain append + read-back at a subdirectory path must round-trip at
--     that exact path, with no directory component doubled — this is the
--     storage-layer half of the "chapters/chapters/..." investigation: if the
--     path is mangled anywhere at or below 'append'\/'readFile', it shows up
--     here; if these pass, the doubling lives above storage (agent\/handler).
module Storyteller.SubdirSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, readFile, writeFile, listAllFiles)
import Prelude hiding (readFile, writeFile)

import Storyteller.Core.Git
import Storyteller.Core.Storage hiding (get, drop)
import qualified Storyteller.Core.Storage as S
import Storyteller.Core.Types
import Storyteller.Core.Append (append)
import Storyteller.Core.Edit (commitFiles)

data Main

runSub
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
runSub action =
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
spec = do
  describe "subdirectory paths" $ do

    it "append + read-back at a subdir path round-trips at that exact path" $
      runSub (do
        _ <- append @Main "chapters/ch1.outline.md" "beat one\n"
        _ <- append @Main "chapters/ch1.outline.md" "beat two\n"
        S.reset @Main
        readFile @(BranchTag Main) "chapters/ch1.outline.md")
      `shouldBe` Right "beat one\nbeat two\n"

    it "the subdir file appears under its own path in a full file listing (not doubled)" $
      runSub (do
        _ <- append @Main "chapters/ch1.outline.md" "content\n"
        S.reset @Main
        listAllFiles @(BranchTag Main) "/")
      `shouldBe` Right ["chapters/ch1.outline.md"]

    it "commitFiles reconciles a file under a subdirectory (no 'is a directory')" $
      runSub (do
        -- Establish a committed atom under chapters/, so the branch has a
        -- 'chapters' directory in its tree (this is what tripped the flat
        -- listFiles walk in readSnapshotAt).
        _ <- append @Main "chapters/ch1.md" "hello world\n"
        -- Edit the working tree freely, then reconcile just this file.
        writeFile @(BranchTag Main) "chapters/ch1.md" "hello world\nmore\n"
        _ <- commitFiles @(BranchTag Main) @Main ["chapters/ch1.md"]
        S.reset @Main
        readFile @(BranchTag Main) "chapters/ch1.md")
      `shouldBe` Right "hello world\nmore\n"

    it "commitFiles still works with sibling subdirectories present" $
      runSub (do
        _ <- append @Main "chapters/ch1.md" "one\n"
        _ <- append @Main "world/place.md" "somewhere\n"
        writeFile @(BranchTag Main) "chapters/ch1.md" "one\ntwo\n"
        _ <- commitFiles @(BranchTag Main) @Main ["chapters/ch1.md"]
        S.reset @Main
        readFile @(BranchTag Main) "chapters/ch1.md")
      `shouldBe` Right "one\ntwo\n"
