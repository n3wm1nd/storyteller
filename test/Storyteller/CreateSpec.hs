{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'Storyteller.Core.Create.createFile' introduces a path into the tree as
-- its own tick, with empty content — distinct from the first 'Atom' a
-- 'Storyteller.Core.StorageMonad.append' would otherwise land implicitly. Pins:
--
--   * the tick this produces is its own kind ("created"), not an atom;
--   * it carries no content;
--   * content appended afterward lands as an ordinary, separate atom tick.
module Storyteller.CreateSpec (spec) where

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
import Storyteller.Core.Create (createFile)

runTestFS
  :: (forall n. SM.StorageM n => SM.StorageT n a)
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      b <- createBranch (BranchName "main")
      let headHash0 = ObjectHash (unTickId (branchHead b))
      wt0 <- SM.loadWorkingTree headHash0
      fst <$> SM.runStorageT headHash0 wt0 action

spec :: Spec
spec = describe "createFile" $ do

  it "produces exactly one tick, of kind \"created\"" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          SM.fileTicksOf "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map SM.ftKind ticks `shouldBe` ["created"]

  it "the created tick carries no content" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> expectationFailure err
      Right [t]   -> SM.ftContent t `shouldBe` Nothing
      Right ticks -> expectationFailure ("expected 1 tick, got " <> show (length ticks))

  it "content appended after creation lands as a separate atom tick" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          _ <- SM.append "scene.md" "hello\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err            -> expectationFailure err
      Right [created, atom] -> do
        SM.ftKind created `shouldBe` "created"
        SM.ftKind atom    `shouldBe` "atom"
        SM.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))
