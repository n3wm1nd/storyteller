{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'Storage.Ops.commitFiles' reconciles a brand-new path (no atom history
-- yet) via 'Storage.Ops.storeNewFile' -- this pins that each such path gets
-- its own empty introduction atom tick plus, if the working tree already
-- had content for it (e.g. an upload or Track/CharGen result), a separate
-- atom tick for that content — rather than one generic batch tick covering
-- every new file at once with its content baked straight in.
module Storyteller.CommitNewFilesSpec (spec) where

import Prelude hiding (writeFile)

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock

import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types

runCommit
  :: (forall n. Core.StoreM n => Core.StoreT n a)
  -> Either String a
runCommit action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      b <- createBranch (BranchName "main")
      let headHash0 = Core.ObjectHash (unTickId (branchHead b))
      fst <$> Core.runStoreT headHash0 action

spec :: Spec
spec = describe "commitFiles on brand-new paths" $ do

  it "a new file with content gets an empty introduction atom then a content atom" $ do
    let result = runCommit $ do
          Core.writeFile "scene.md" "hello\n"
          _ <- Ops.commitFiles ["scene.md"]
          Tick.fileTicksOf "scene.md"
    case result of
      Left err               -> expectationFailure err
      Right [created, atom]  -> do
        Tick.ftKind created `shouldBe` "atom"
        Tick.ftContent created `shouldBe` Just ""
        Tick.ftKind atom    `shouldBe` "atom"
        Tick.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))

  it "a new file with no content yet gets only its introduction atom" $ do
    let result = runCommit $ do
          Core.writeFile "scene.md" ""
          _ <- Ops.commitFiles ["scene.md"]
          Tick.fileTicksOf "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map Tick.ftKind ticks `shouldBe` ["atom"]

  it "multiple new files each get their own introduction (+ content) ticks, independently" $ do
    let result = runCommit $ do
          Core.writeFile "a.md" "A\n"
          Core.writeFile "b.md" ""
          _ <- Ops.commitFiles ["a.md", "b.md"]
          aTicks <- Tick.fileTicksOf "a.md"
          bTicks <- Tick.fileTicksOf "b.md"
          return (aTicks, bTicks)
    case result of
      Left err -> expectationFailure err
      Right (aTicks, bTicks) -> do
        map Tick.ftKind aTicks `shouldBe` ["atom", "atom"]
        map Tick.ftKind bTicks `shouldBe` ["atom"]
