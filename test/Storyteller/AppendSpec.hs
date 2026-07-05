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
import qualified Data.Text.Encoding as TE
import Runix.FileSystem
  (FileSystem, FileSystemRead, FileSystemWrite, appendFile, writeFile, readFile, fileExists)

import Prelude hiding (appendFile, readFile, writeFile)

import Storyteller.Core.Append (appendAtom)
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch, get, readAt, readAtWithFS, storeAs)
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

  it "isolated atom is correct even with a mid-file deletion of prior committed content sitting in the ambient tree" $ do
    let result = runAppend $ do
          _        <- appendAtom @Main "a.md" "hello world"
          writeFile @(BranchTag Main) "a.md" "hello"   -- dirty delete of " world"
          newTid   <- appendAtom @Main "a.md" " again"
          isolated <- readAtWithFS @Main newTid (readFile @(BranchTag Main) "a.md")
          ambient  <- readFile @(BranchTag Main) "a.md"
          return (isolated, ambient)
    result `shouldBe` Right ("hello world again", "hello again")

  -- If the ambient tree already has its own pending *append* past HEAD (not
  -- just an unrelated edit), the two appends can't both keep their original
  -- relative order — there's no principled way to interleave "pending, not
  -- yet an atom" bytes with "this atom's" bytes once they're both just
  -- trailing text. The chosen (and only sensible) semantic: whatever's
  -- already at the tail of the ambient tree stays exactly where it is, and
  -- this atom's content always lands after it — 'appendAtom's ambient write
  -- is a plain append, full stop, never an insert. The isolated commit
  -- itself is unaffected either way (see the mid-file-deletion case above):
  -- it only ever builds from HEAD's own clean value, so this ordering
  -- question is purely about the ambient tree's transient state pending
  -- reconciliation, not about what gets committed.
  it "a pending tail-append already in the ambient tree ends up before, not after, the new atom" $ do
    let result = runAppend $ do
          _        <- appendAtom @Main "a.md" "H1"
          writeFile @(BranchTag Main) "a.md" "H1 pending"  -- uncommitted append past HEAD
          newTid   <- appendAtom @Main "a.md" "H2"
          isolated <- readAtWithFS @Main newTid (readFile @(BranchTag Main) "a.md")
          ambient  <- readFile @(BranchTag Main) "a.md"
          return (isolated, ambient)
    result `shouldBe` Right ("H1H2", "H1 pendingH2")

  -- A hand-rolled atom commit (raw 'appendFile' + 'storeAs', bypassing every
  -- helper this module offers) that claims less content than it actually
  -- added must be rejected by the storage layer itself, not merely by
  -- caller discipline. Without this, a tree that already carries some other
  -- uncommitted but still append-compatible addition would let the naive
  -- prefix-only append-only check pass (the tree only ever grew), silently
  -- producing a tick whose message understates its own diff. 'Store' now
  -- checks atom ticks specifically for exact equality (parent content <>
  -- message == new content), so this is rejected outright instead — this
  -- pins that rejection as a property of 'Store' itself, independent of any
  -- particular helper function in this module.
  it "an atom commit whose message doesn't match its actual diff is rejected by Store itself" $ do
    let result = runAppend $ do
          _        <- appendAtom @Main "a.md" "hello"
          writeFile @(BranchTag Main) "a.md" "hello WIP"  -- uncommitted, append-compatible dirt
          appendFile @(BranchTag Main) "a.md" (TE.encodeUtf8 " done")
          storeAs @Main (Atom "a.md" " done")
    result `shouldBe` Left "Store: atom message for a.md does not match its actual diff"
