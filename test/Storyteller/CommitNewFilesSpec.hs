{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'Storyteller.Core.StorageMonad.commitFiles' reconciles a brand-new path
-- (no atom history yet) via 'storeNewFiles' — this pins that each such path
-- gets its own empty introduction atom tick plus, if the working tree
-- already had content for it (e.g. an upload or Track/CharGen result), a
-- separate atom tick for that content — rather than one generic batch
-- tick covering every new file at once with its content baked straight in.
module Storyteller.CommitNewFilesSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock

import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Core.Storage (createBranch)
import qualified Storyteller.Core.StorageMonad as SM
import Storyteller.Core.Types

runCommit
  :: (forall n. SM.StorageM n => SM.StorageT n a)
  -> Either String a
runCommit action =
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
spec = describe "commitFiles on brand-new paths" $ do

  it "a new file with content gets an empty introduction atom then a content atom" $ do
    let result = runCommit $ do
          SM.writeFileS "scene.md" "hello\n"
          _ <- SM.commitFiles ["scene.md"]
          SM.fileTicksOf "scene.md"
    case result of
      Left err               -> expectationFailure err
      Right [created, atom]  -> do
        SM.ftKind created `shouldBe` "atom"
        SM.ftContent created `shouldBe` Just ""
        SM.ftKind atom    `shouldBe` "atom"
        SM.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))

  it "a new file with no content yet gets only its introduction atom" $ do
    let result = runCommit $ do
          SM.writeFileS "scene.md" ""
          _ <- SM.commitFiles ["scene.md"]
          SM.fileTicksOf "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map SM.ftKind ticks `shouldBe` ["atom"]

  it "multiple new files each get their own introduction (+ content) ticks, independently" $ do
    let result = runCommit $ do
          SM.writeFileS "a.md" "A\n"
          SM.writeFileS "b.md" ""
          _ <- SM.commitFiles ["a.md", "b.md"]
          aTicks <- SM.fileTicksOf "a.md"
          bTicks <- SM.fileTicksOf "b.md"
          return (aTicks, bTicks)
    case result of
      Left err -> expectationFailure err
      Right (aTicks, bTicks) -> do
        map SM.ftKind aTicks `shouldBe` ["atom", "atom"]
        map SM.ftKind bTicks `shouldBe` ["atom"]
