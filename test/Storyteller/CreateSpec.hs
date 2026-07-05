{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Core.Create.createFile' introduces a path into the tree as
-- its own tick, with empty content — distinct from the first 'Atom' a
-- 'Storyteller.Core.Append.append' would otherwise land implicitly. Pins:
--
--   * the tick this produces is its own kind ("created"), not an atom;
--   * it carries no content;
--   * content appended afterward lands as an ordinary, separate atom tick.
module Storyteller.CreateSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Core.Git
import Storyteller.Core.Storage hiding (get, drop)
import qualified Storyteller.Core.Storage as S
import Storyteller.Core.Types
import Storyteller.Core.Append (append)
import Storyteller.Core.Create (createFile)

data Main

runTestFS
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
runTestFS action =
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
spec = describe "createFile" $ do

  it "produces exactly one tick, of kind \"created\"" $ do
    let result = runTestFS $ do
          _ <- createFile @Main "scene.md"
          fileTicks @Main "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map S.ftKind ticks `shouldBe` ["created"]

  it "the created tick carries no content" $ do
    let result = runTestFS $ do
          _ <- createFile @Main "scene.md"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> expectationFailure err
      Right [t]   -> S.ftContent t `shouldBe` Nothing
      Right ticks -> expectationFailure ("expected 1 tick, got " <> show (length ticks))

  it "content appended after creation lands as a separate atom tick" $ do
    let result = runTestFS $ do
          _ <- createFile @Main "scene.md"
          _ <- append @Main "scene.md" "hello\n"
          fileTicks @Main "scene.md"
    case result of
      Left err            -> expectationFailure err
      Right [created, atom] -> do
        S.ftKind created `shouldBe` "created"
        S.ftKind atom    `shouldBe` "atom"
        S.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))
