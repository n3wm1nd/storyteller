{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Files living under a subdirectory (e.g. @chapters/ch1.md@) must behave
-- exactly like root-level files across the storage/edit layer. Two bugs this
-- pins:
--
--   * 'commitFiles' walked historical snapshots with a flat @listFiles "/"@
--     that included directory entries, then 'readFile'd each — which fails
--     with "is a directory" the moment any subdirectory exists.
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
import Polysemy.State (evalState)

import Git.Mock
import Runix.Git (ObjectHash(..))

import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Core.Storage (createBranch)
import qualified Storyteller.Core.StorageMonad as SM
import Storyteller.Core.Types

runSub
  :: (forall n. SM.StorageM n => SM.StorageT n a)
  -> Either String a
runSub action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      b <- createBranch (BranchName "main")
      let headHash0 = SM.ObjectHash (unTickId (branchHead b))
      wt0 <- SM.loadWorkingTree headHash0
      fst <$> SM.runStorageT headHash0 wt0 action

spec :: Spec
spec = do
  describe "subdirectory paths" $ do

    it "append + read-back at a subdir path round-trips at that exact path" $
      runSub (do
        _ <- SM.append "chapters/ch1.outline.md" "beat one\n"
        _ <- SM.append "chapters/ch1.outline.md" "beat two\n"
        SM.resetTree
        SM.readFileS "chapters/ch1.outline.md")
      `shouldBe` Right "beat one\nbeat two\n"

    it "the subdir file appears under its own path in a full file listing (not doubled)" $
      runSub (do
        _ <- SM.append "chapters/ch1.outline.md" "content\n"
        SM.resetTree
        SM.listAllFilesS "/")
      `shouldBe` Right ["chapters/ch1.outline.md"]

    it "commitFiles reconciles a file under a subdirectory (no 'is a directory')" $
      runSub (do
        -- Establish a committed atom under chapters/, so the branch has a
        -- 'chapters' directory in its tree (this is what tripped the flat
        -- listFiles walk in readSnapshotAt).
        _ <- SM.append "chapters/ch1.md" "hello world\n"
        -- Edit the working tree freely, then reconcile just this file.
        SM.writeFileS "chapters/ch1.md" "hello world\nmore\n"
        _ <- SM.commitFiles ["chapters/ch1.md"]
        SM.resetTree
        SM.readFileS "chapters/ch1.md")
      `shouldBe` Right "hello world\nmore\n"

    it "commitFiles still works with sibling subdirectories present" $
      runSub (do
        _ <- SM.append "chapters/ch1.md" "one\n"
        _ <- SM.append "world/place.md" "somewhere\n"
        SM.writeFileS "chapters/ch1.md" "one\ntwo\n"
        _ <- SM.commitFiles ["chapters/ch1.md"]
        SM.resetTree
        SM.readFileS "chapters/ch1.md")
      `shouldBe` Right "one\ntwo\n"
