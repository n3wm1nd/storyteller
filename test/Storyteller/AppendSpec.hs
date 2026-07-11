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
  (FileSystem, FileSystemRead, FileSystemWrite, writeFile, readFile)

import Data.Text (Text)

import Prelude hiding (readFile, writeFile)

import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryStorage, createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (BranchName(..), TickId(..), tickParent)

data Main

runAppend
  :: Sem '[ FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , BranchOp Main
          , StoryStorage
          , Git
          , State GitState
          , Fail
          ] a
  -> Either String a
runAppend action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runBranchAndFS @Main (BranchName "main") action

-- | 'appendAtom' returning a 'TickId', matching this spec's own vocabulary
--   -- 'Storage.Ops.addAtom' returns 'Core.ObjectHash' directly.
appendAtom :: Member (BranchOp Main) r => FilePath -> Text -> Sem r TickId
appendAtom path content = do
  h <- runStorage @Main (Ops.addAtom path content)
  return (TickId (Core.unObjectHash h))

toHash :: TickId -> Core.ObjectHash
toHash (TickId t) = Core.ObjectHash t

spec :: Spec
spec = describe "appendAtom" $ do

  it "commits the appended content and it's readable back" $ do
    let result = runAppend $ do
          _ <- appendAtom "a.md" "hello"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "hello"

  it "a second append builds on the first, within the same scope" $ do
    let result = runAppend $ do
          _ <- appendAtom "a.md" "hello"
          _ <- appendAtom "a.md" " world"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "hello world"

  -- Regression: an atom commit used to be built from whatever the *whole*
  -- ambient working tree looked like, not just the one file it claims to
  -- append to. A foreign, not-yet-committed edit to an unrelated file
  -- sitting in the same scope would silently ride along in the same git
  -- tree, even though the tick's own message only ever mentions the
  -- appended file. Two things must hold: the foreign pending edit
  -- survives, untouched, in the live scope (still there to commit
  -- separately later), and the atom's own commit must not have picked it
  -- up at all -- 'Storage.Core.store' always builds an atom's new tree
  -- from HEAD's own committed tree, never the ambient one, so this is
  -- structural, not merely tested-for.
  it "does not fold an unrelated file's pending edit into the atom's commit" $ do
    let result = runAppend $ do
          -- Pending, uncommitted edit to an unrelated file.
          writeFile @(BranchTag Main) "b.md" "pending"
          newTid          <- appendAtom "a.md" "hello"
          ambientB        <- readFile @(BranchTag Main) "b.md"
          existsAtNewTick <- runStorage @Main (Core.inWorktree (Ops.exists "b.md") `atHash` toHash newTid)
          return (ambientB, existsAtNewTick)
    result `shouldBe` Right ("pending", False)

  it "a foreign pending edit to the same file's sibling atom is unaffected by an unrelated append" $ do
    let result = runAppend $ do
          _        <- appendAtom "a.md" "first"
          writeFile @(BranchTag Main) "b.md" "pending"
          newTid   <- appendAtom "a.md" " second"
          tick <- runStorage @Main (Tick.readTypesTick (toHash newTid))
          case tickParent tick of
            Nothing  -> fail "expected a parent"
            Just pid -> do
              existedBefore <- runStorage @Main (Core.inWorktree (Ops.exists "b.md") `atHash` toHash pid)
              existsNow <- runStorage @Main (Core.inWorktree (Ops.exists "b.md") `atHash` toHash newTid)
              ambientB           <- readFile @(BranchTag Main) "b.md"
              return (existedBefore, existsNow, ambientB)
    result `shouldBe` Right (False, False, "pending")

  -- Deliberate design choice, not an oversight: an append never checks
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
          _ <- appendAtom "a.md" " plus atom"
          readFile @(BranchTag Main) "a.md"
    result `shouldBe` Right "pending edit plus atom"

  it "isolated atom is correct even with a mid-file deletion of prior committed content sitting in the ambient tree" $ do
    let result = runAppend $ do
          _        <- appendAtom "a.md" "hello world"
          writeFile @(BranchTag Main) "a.md" "hello"   -- dirty delete of " world"
          newTid   <- appendAtom "a.md" " again"
          isolated <- runStorage @Main (Core.inWorktree (Core.readFile "a.md") `atHash` toHash newTid)
          ambient  <- readFile @(BranchTag Main) "a.md"
          return (isolated, ambient)
    result `shouldBe` Right ("hello world again", "hello again")

  -- If the ambient tree already has its own pending *append* past HEAD (not
  -- just an unrelated edit), the two appends can't both keep their original
  -- relative order — there's no principled way to interleave "pending, not
  -- yet an atom" bytes with "this atom's" bytes once they're both just
  -- trailing text. The chosen (and only sensible) semantic: whatever's
  -- already at the tail of the ambient tree stays exactly where it is, and
  -- this atom's content always lands after it — an append's ambient write
  -- is a plain append, full stop, never an insert. The isolated commit
  -- itself is unaffected either way (see the mid-file-deletion case above):
  -- it only ever builds from HEAD's own clean value, so this ordering
  -- question is purely about the ambient tree's transient state pending
  -- reconciliation, not about what gets committed.
  it "a pending tail-append already in the ambient tree ends up before, not after, the new atom" $ do
    let result = runAppend $ do
          _        <- appendAtom "a.md" "H1"
          writeFile @(BranchTag Main) "a.md" "H1 pending"  -- uncommitted append past HEAD
          newTid   <- appendAtom "a.md" "H2"
          isolated <- runStorage @Main (Core.inWorktree (Core.readFile "a.md") `atHash` toHash newTid)
          ambient  <- readFile @(BranchTag Main) "a.md"
          return (isolated, ambient)
    result `shouldBe` Right ("H1H2", "H1 pendingH2")
  where
    -- Read @action@'s result as of @target@'s own committed snapshot,
    -- without disturbing head or the ambient tree -- 'Core.readAt' plus
    -- 'Core.inWorktree' composed the way every historical-peek call site
    -- here needs them.
    atHash action target = Core.readAt target action
