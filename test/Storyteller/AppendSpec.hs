{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.AppendSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem
  (FileSystem, FileSystemRead, FileSystemWrite, writeFile, readFile, fileExists)

import Prelude hiding (readFile, writeFile)

import Storyteller.Core.Append (appendAtom)
import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch, get, readAt, readAtWithFS)
import Storyteller.Core.Types (BranchName(..), tickParent)

data Main

runAppend
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
runAppend action =
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
spec = describe "appendAtom" $ do

  it "commits the appended content and it's readable back" $ do
    let result = runAppend $ do
          _ <- appendAtom @Main "a.md" "hello"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "hello"

  it "a second append builds on the first, within the same scope" $ do
    let result = runAppend $ do
          _ <- appendAtom @Main "a.md" "hello"
          _ <- appendAtom @Main "a.md" " world"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "hello world"

  -- Regression: 'appendAtom' used to commit whatever the *whole* ambient
  -- working tree looked like, not just the one file it claims to append
  -- to. A foreign, not-yet-committed edit to an unrelated file sitting in
  -- the same scope would silently ride along in the same git tree, even
  -- though the tick's own message only ever mentions the appended file.
  -- Two things must hold: the foreign pending edit survives, untouched,
  -- in the live scope (still there to commit separately later), and the
  -- atom's own commit must not have picked it up at all.
  it "does not fold an unrelated file's pending edit into the atom's commit" $ do
    let result = runAppend $ do
          -- Pending, uncommitted edit to an unrelated file.
          writeFile @(BranchTag Main) "b.md" "pending"
          newTid    <- appendAtom @Main "a.md" "hello"
          ambientB  <- readFile @(BranchTag Main) "b.md"
          existsAtNewTick <- readAtWithFS @Main newTid (fileExists @(BranchTag Main) "b.md")
          return (ambientB, existsAtNewTick)
    result `shouldBe` Right ("pending", False)

  it "a foreign pending edit to the same file's sibling atom is unaffected by an unrelated append" $ do
    let result = runAppend $ do
          _        <- appendAtom @Main "a.md" "first"
          writeFile @(BranchTag Main) "b.md" "pending"
          newTid   <- appendAtom @Main "a.md" " second"
          tick     <- readAt @Main newTid (get @Main)
          case tickParent tick of
            Nothing  -> fail "expected a parent"
            Just pid -> do
              existedBefore <- readAtWithFS @Main pid (fileExists @(BranchTag Main) "b.md")
              existsNow     <- readAtWithFS @Main newTid (fileExists @(BranchTag Main) "b.md")
              ambientB      <- readFile @(BranchTag Main) "b.md"
              return (existedBefore, existsNow, ambientB)
    result `shouldBe` Right (False, False, "pending")

  -- Deliberate design choice, not an oversight: 'appendAtom' never checks
  -- whether the target file already had its own pending edit before this
  -- call. It just appends on top of whatever's there. The atom's own
  -- commit still only ever contains this one clean append onto HEAD's own
  -- value for the file (built in isolation, per the tests above) — so the
  -- working tree ends up holding more than that one commit's message
  -- claims, which is exactly the ordinary "pending diff against HEAD"
  -- situation reconciliation already handles, not a new failure mode.
  it "still succeeds, unconditionally, when the target file already has a pending edit" $ do
    let result = runAppend $ do
          writeFile @(BranchTag Main) "a.md" "pending edit"
          _ <- appendAtom @Main "a.md" " plus atom"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "pending edit plus atom"
