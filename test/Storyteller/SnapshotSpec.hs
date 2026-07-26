{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Core.Snapshot': positioned, read-only filesystem access,
--   the scope-level counterpart to 'Storage.Core.readAt'.
--
--   Two things are worth pinning here, and they're different in kind.
--   The first is ordinary equivalence: a snapshot read must give what a
--   branch scope's read gives, or porting a call site onto it changes
--   behaviour. The second is the *distinguishing* property, and it's the
--   one a reviewer would otherwise have to take on faith: a position
--   names committed content, so a snapshot read is unaffected by a
--   branch scope's pending, uncommitted ambient edits — and, being taken
--   at a hash rather than a branch name, it keeps answering for that
--   position after the branch itself has moved on.
module Storyteller.SnapshotSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy (Sem, run)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, glob, listFiles, readFile)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Snapshot (Snapshot, readSnapshotFile, runSnapshotFS)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Prelude hiding (readFile)

type Stack a =
  Sem ( FileSystemWrite (BranchTag Main)
      : FileSystemRead  (BranchTag Main)
      : FileSystem      (BranchTag Main)
      : BranchOp Main
      : StoryStorage
      : TestEffects '[] ) a

withStory :: Stack a -> Either String a
withStory action = run $ testStack $ do
  _ <- createBranch (BranchName "story")
  runBranchAndFS @Main (BranchName "story") action

-- | The branch's current head, as a position.
headPosition :: Stack Core.ObjectHash
headPosition = do
  mb <- getBranch (BranchName "story")
  case mb of
    Nothing -> fail "branch vanished"
    Just b  -> pure (Core.ObjectHash (unTickId (branchHead b)))

spec :: Spec
spec = describe "Storyteller.Core.Snapshot" $ do

  describe "readSnapshotFile" $ do

    it "reads a committed file's content at a position" $ do
      let result = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "notes.md" "hello\n")
            pos <- headPosition
            readSnapshotFile pos "notes.md"
      result `shouldBe` Right (Just "hello\n")

    it "answers Nothing for a path that isn't there" $ do
      let result = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "notes.md" "hello\n")
            pos <- headPosition
            readSnapshotFile pos "nope.md"
      result `shouldBe` Right Nothing

    -- The distinguishing property, half one: a position is a *committed*
    -- snapshot. An ambient write that hasn't been reconciled into the
    -- chain is real, and visible to the branch scope that made it, but it
    -- is not part of any commit — so a snapshot read must not see it.
    it "reads committed content, not a branch scope's pending ambient edit" $ do
      let result = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "notes.md" "committed\n")
            pos <- headPosition
            -- A pending ambient edit: written to the working tree, never
            -- committed. The branch scope itself sees it...
            runStorage @Main (Core.writeFile "notes.md" "pending\n")
            ambient  <- runStorage @Main (Core.readFile "notes.md")
            -- ...the snapshot at the same position does not.
            snapshot <- readSnapshotFile pos "notes.md"
            pure (ambient, snapshot)
      result `shouldBe` Right ("pending\n", Just "committed\n")

    -- Half two: the position is a hash, not a branch name, so it stays
    -- meaningful after the branch moves — which is exactly what lets a
    -- caller resolve several branches up front and read them one after
    -- another without any of the answers shifting underneath it.
    it "keeps answering for its own position after the branch has advanced" $ do
      let result = withStory $ do
            _     <- runStorage @Main (Ops.addAtom "notes.md" "first\n")
            early <- headPosition
            _     <- runStorage @Main (Ops.addAtom "notes.md" "second\n")
            now   <- headPosition
            (,) <$> readSnapshotFile early "notes.md"
                <*> readSnapshotFile now   "notes.md"
      result `shouldBe` Right (Just "first\n", Just "first\nsecond\n")

  describe "runSnapshotFS" $ do

    -- Equivalence with the branch scope it replaces: same file, same
    -- bytes, read through the identical 'Runix.FileSystem' vocabulary,
    -- differing only in how the position was supplied.
    it "answers readFile the same way the branch's own scope does" $ do
      let result = withStory $ do
            _        <- runStorage @Main (Ops.addAtom "lore/world.md" "the world\n")
            pos      <- headPosition
            viaScope <- readFile @(BranchTag Main) "lore/world.md"
            viaSnap  <- runSnapshotFS pos (readFile @Snapshot "lore/world.md")
            pure (viaScope, viaSnap)
      case result of
        Left err          -> expectationFailure err
        Right (a, b)      -> b `shouldBe` a

    it "lists a directory's direct children" $ do
      let result = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "lore/a.md" "a\n")
            _   <- runStorage @Main (Ops.addAtom "lore/b.md" "b\n")
            _   <- runStorage @Main (Ops.addAtom "top.md"    "t\n")
            pos <- headPosition
            runSnapshotFS pos (listFiles @Snapshot "lore")
      fmap (fmap T.pack) result `shouldBe` Right ["lore/a.md", "lore/b.md"]

    it "globs against the snapshot's own tree" $ do
      let result = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "lore/a.md"  "a\n")
            _   <- runStorage @Main (Ops.addAtom "lore/b.txt" "b\n")
            pos <- headPosition
            runSnapshotFS pos (glob @Snapshot "/" "lore/*.md")
      result `shouldBe` Right ["lore/a.md"]

    -- Same failure vocabulary as the ambient interpreter, so a caller's
    -- error handling doesn't change shape depending on which one served
    -- the read.
    it "fails on a missing path the same way the branch scope does" $ do
      let readVia f = withStory $ do
            _   <- runStorage @Main (Ops.addAtom "notes.md" "hello\n")
            pos <- headPosition
            f pos
          viaScope = readVia (\_   -> TE.decodeUtf8 <$> readFile @(BranchTag Main) "nope.md")
          viaSnap  = readVia (\pos -> runSnapshotFS pos (TE.decodeUtf8 <$> readFile @Snapshot "nope.md"))
      viaSnap `shouldBe` viaScope
