{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.FileSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryStorage, createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storyteller.Common.Swipe as Swipe
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Types

import Server.Core.File
import Server.Core.Protocol (Update(..), WireTick(..))
import Server.TestStack

-- ---------------------------------------------------------------------------
-- Helpers
--
-- File functions assume their scope ('FileOpen') is already open, same as a
-- real connection: it's entered once here, wrapping the whole action,
-- rather than per call.
--
-- 'runner' (a 'TestRunner', see 'Server.TestStack') is threaded through so
-- every test below runs under both the eager and the 'withStorage'-
-- buffered interpreter (see 'test/Main.hs') without being written twice.
--
-- Atoms are stored via 'Storyteller.Core.Append.appendAtom' — the real
-- library primitive, not a hand-rolled write+store pair — since 'appendFile'
-- already handles a file's first write the same as any later one, a single
-- helper covers both 'storeAtom' and 'appendAtom''s old roles here.
-- ---------------------------------------------------------------------------

withFile_
  :: TestRunner
  -> BranchName
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withFile_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

headIsIn :: Update -> Bool
headIsIn upd = null (updateTicks upd) || updateHead upd `elem` map wtTickId (updateTicks upd)

appendAtom
  :: Member (BranchOp Main) r
  => FilePath -> T.Text -> Sem r TickId
appendAtom path content = do
  h <- runStorage @Main (Ops.addAtom path content)
  return (TickId (Core.unObjectHash h))

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: TestRunner -> Spec
spec runner = do

  describe "fileState" $ do

    it "returns an empty update for a branch with no file ticks" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` \case
          Right upd -> null (updateTicks upd)
          _         -> False

    it "head is valid or empty when file has no ticks" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` either (const False) headIsIn

    -- Regression: 'walkFileTicks' filters the branch chain down to one
    -- file's ticks, but originally left 'ftParent' pointing at the tick's
    -- true git-chain parent — which may not be in the filtered set at all
    -- (a tick with no refs and no matching "file" field, e.g. a 'presence'
    -- tick from Storyteller.Writer.Types, interleaved between two atoms of
    -- this file). A client walking '.parent' from HEAD then stopped dead at
    -- that gap, silently truncating everything older than the most recent
    -- excluded tick — "the file only renders after the last unrelated
    -- tick." Fixed by relinking each returned tick's parent to the nearest
    -- *included* ancestor. This test constructs that shape directly
    -- (an unrelated, file-less, ref-less tick between two atoms) without
    -- depending on Storyteller.Writer, which Server.Core must not import.
    it "an unrelated tick with no refs and no file field does not break the parent chain" $ do
      let result = withFile_ runner (BranchName "b") $ do
            t1 <- appendAtom "story.md" "first"
            _  <- runStorage @Main (Core.store (Core.NonAtom [] "type:presence\n\nunrelated standalone tick"))
            t2 <- appendAtom "story.md" " second"
            fileState "story.md" >>= \upd -> return (t1, t2, upd)
      case result of
        Left err -> expectationFailure err
        Right (t1, t2, upd) -> do
          let ids = map wtTickId (updateTicks upd)
          ids `shouldContain` [unTickId t1]
          ids `shouldContain` [unTickId t2]
          -- The projection must be self-contained: every non-root parent
          -- pointer resolves to another tick in this same list, so a
          -- client's '.parent' walk from HEAD never falls out of it.
          all (\t -> maybe True (`elem` ids) (wtParent t)) (updateTicks upd) `shouldBe` True

  describe "deleteFileAtom" $ do

    it "deleted atom no longer appears in fileState" $ do
      let result = withFile_ runner (BranchName "b") $ do
            tid <- appendAtom "f.md" "hello"
            deleteFileAtom tid
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> updateTicks upd `shouldBe` []

    -- A single 'deleteFileAtom' still delegates to 'deleteFileAtoms' with
    -- one target, so this doubles as the one-tick-batch case of the tests
    -- below.
    it "deleting a mixed atom-and-note batch, given earliest-in-chain first, still removes every target" $ do
      -- 'Server.Core.File.deleteFileAtoms' sorts internally (see its own
      -- Haddock: furthest-in-chain first, for replay cost, not
      -- correctness) -- a caller passing ids in *any* order, like this
      -- earliest-first list, still gets every target removed. Also mixes
      -- in a non-atom tick (a note): 'deleteFileAtom'/'deleteFileAtoms'
      -- are generic over any tick kind, not just atoms -- see
      -- 'Storage.Ops.deleteTick'.
      let result = withFile_ runner (BranchName "b") $ do
            tidA <- appendAtom "f.md" "atom A"
            chatNote "a remark" [tidA]
            noteTid <- TickId . Core.unObjectHash <$> runStorage @Main Core.headHash
            tidB <- appendAtom "f.md" "atom B"
            deleteFileAtoms [tidA, noteTid, tidB]
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> updateTicks upd `shouldBe` []

  describe "deleteFile" $ do

    -- Deletion is a forward event (see 'Storyteller.Core.Create's module
    -- Haddock), not a rebase: the tick that recorded it, and everything
    -- before it, stays exactly where it was in *raw* history -- nothing is
    -- excised. But 'fileState' (via 'Tick.fileTicksOf') is a current-
    -- lifetime-scoped *view*, not raw history: a currently-deleted,
    -- not-yet-recreated file has no current lifetime at all, so it reads
    -- exactly like a path that never existed -- empty ticks, empty head --
    -- not "the file's whole history, ending in a deletion tick".
    it "leaves fileState empty once deleted, same as a path that never existed" $ do
      let result = withFile_ runner (BranchName "b") $ do
            _ <- appendAtom "f.md" "first"
            _ <- appendAtom "f.md" "second"
            deleteFile "f.md"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> do
          updateTicks upd `shouldBe` []
          updateHead upd `shouldBe` ""

    -- Deleting one file must not disturb another file's own chain —
    -- 'Storage.Ops.deleteFile' commits a single tick scoped to its own
    -- path.
    it "does not affect another file's ticks" $ do
      let result = withFile_ runner (BranchName "b") $ do
            _   <- appendAtom "keep.md" "kept content"
            _   <- appendAtom "gone.md" "doomed content"
            deleteFile "gone.md"
            fileState "keep.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> length (updateTicks upd) `shouldBe` 1

    it "fails when the file isn't currently present" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (deleteFile "nope.md"))
        `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

    -- End-to-end reproduction of the original bug report: deleting a file
    -- appeared to succeed but left it uncreatable at the same path
    -- afterward. Now that both 'createFile' and 'deleteFile' guard on tree
    -- presence ('Storage.Ops.exists'), not tick history -- a deleted path
    -- genuinely has no ticks of its own excised, they're kept for history,
    -- see 'Storyteller.CreateSpec' -- recreating it succeeds. 'fileState'
    -- only shows the *new* lifetime's own tick (the second 'createFile') --
    -- the old creation-then-deletion belongs to a now-closed, unrelated
    -- lifetime and correctly doesn't bleed into the recreated file's
    -- current state, matching 'atomHistory' (which likewise stops folding
    -- at the deletion marker) not carrying the old content forward either.
    it "a path can be recreated after being deleted, with no leftover tick blocking it" $ do
      let result = withFile_ runner (BranchName "b") $ do
            createFile "f.md"
            deleteFile "f.md"
            createFile "f.md"
            (,) <$> fileState "f.md" <*> runStorage @Main (Ops.atomHistory "f.md")
      case result of
        Left err -> expectationFailure err
        Right (upd, history) -> do
          length (updateTicks upd) `shouldBe` 1
          mconcat (map snd history) `shouldBe` ""

  describe "renameFile" $ do

    it "moves the file's content to the new path" $ do
      let result = withFile_ runner (BranchName "b") $ do
            _ <- appendAtom "f.md" "hello"
            renameFile "f.md" "g.md"
            (,) <$> fileState "f.md" <*> fileState "g.md"
      case result of
        Left err -> expectationFailure err
        Right (oldState, newState) -> do
          updateTicks oldState `shouldBe` []
          length (updateTicks newState) `shouldBe` 1

    it "fails when the source file isn't currently present" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (renameFile "nope.md" "g.md"))
        `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

    it "fails when the destination path already exists" $ do
      let result = withFile_ runner (BranchName "b") $ do
            _ <- appendAtom "f.md" "hello"
            _ <- appendAtom "g.md" "already here"
            renameFile "f.md" "g.md"
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected renameFile to fail when the destination already exists"

  describe "checkpointFile" $ do

    -- 'fileState' (built on 'Tick.fileTicksOf') deliberately walks every
    -- lifetime, not just the current one -- so the raw tick *count* is
    -- expected to grow (the old atoms, the checkpoint's own deletion
    -- marker, and the fresh clones are all still in there). What actually
    -- has to stay unchanged is the file's own committed content -- read
    -- via 'Ops.atomHistory' (the current lifetime's own committed fold),
    -- not the ambient tree, which 'checkpointFile' never touches at all
    -- (nothing about the ambient tree needs to change; only 'renameFile'
    -- needs that sync, since a rename actually changes the path).
    it "leaves the file's own committed content unchanged" $ do
      let result = withFile_ runner (BranchName "b") $ do
            _      <- appendAtom "f.md" "p1"
            _      <- appendAtom "f.md" "p2"
            before <- runStorage @Main (Ops.atomHistory "f.md")
            checkpointFile "f.md"
            after  <- runStorage @Main (Ops.atomHistory "f.md")
            return (map snd before, map snd after)
      case result of
        Left err -> expectationFailure err
        Right (before, after) -> after `shouldBe` before

    it "fails when the path isn't currently present" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (checkpointFile "nope.md"))
        `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

  describe "moveFileAtom" $ do

    it "moving a single atom to front is a no-op on chain length" $ do
      let result = withFile_ runner (BranchName "b") $ do
            t1 <- appendAtom "f.md" "atom1"
            before <- length . updateTicks <$> fileState "f.md"
            moveFileAtom t1 Nothing
            after <- length . updateTicks <$> fileState "f.md"
            return (before, after)
      case result of
        Left err     -> expectationFailure err
        Right (b, a) -> b `shouldBe` a

  describe "editFileAtom" $ do

    it "edit changes the content of the atom" $ do
      let result = withFile_ runner (BranchName "b") $ do
            tid <- appendAtom "f.md" "original"
            editFileAtom "f.md" tid "edited"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd ->
          -- content changed; atom count stays the same
          length (updateTicks upd) `shouldBe` 1

    -- The only way an atom edit may fail is if the targeted atom doesn't
    -- exist. Any replacement content — shorter, longer, unrelated — must be
    -- accepted for an atom that does exist. Reproduces a bug where editing
    -- a non-last atom with content shorter than the original trips the
    -- storage layer's append-only check, since editAtom overwrote the whole
    -- file blob with just the new atom bytes instead of appending them
    -- after the preceding atoms' content.
    it "edit succeeds for any existing atom regardless of new content length" $ do
      let result = withFile_ runner (BranchName "b") $ do
            t1 <- appendAtom "f.md" "first atom text\n"
            _  <- appendAtom "f.md" "second\n"
            editFileAtom "f.md" t1 "x\n"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> length (updateTicks upd) `shouldBe` 2

  describe "cycleAtomSwipe" $ do

    it "fails when the atom has no alternates" $ do
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (do
        tid <- appendAtom "f.md" "only"
        cycleAtomSwipe tid))
        `shouldSatisfy` \case
          Left _  -> True
          Right _ -> False

    it "rotates a pushed alternate into the atom, keeping the tick count the same" $ do
      let result = withFile_ runner (BranchName "b") $ do
            a <- appendAtom "f.md" "original"
            _ <- runStorage @Main (Swipe.pushSwipe (Core.ObjectHash (unTickId a)) "fresh")
            before <- fileState "f.md"
            cycleAtomSwipe a
            after <- fileState "f.md"
            return (before, after)
      case result of
        Left err -> expectationFailure err
        Right (before, after) -> do
          -- Same total tick count -- the atom's content rotated in place,
          -- the displaced content became the new swipe; nothing added or
          -- removed.
          length (updateTicks after) `shouldBe` length (updateTicks before)

    -- Reproduces 'Server.Writer.File.chatConverseSwipe''s exact shape: a
    -- prompt tick rebased in place (via a separate 'runStorage' dispatch,
    -- same as 'editChatPrompt') *before* the atom that follows it is
    -- pushed a swipe -- using the atom id captured *before* that rebase,
    -- same as the caller (which only ever has the pre-rebase id the
    -- client sent). The prompt's own rebase replays (and so re-hashes)
    -- the atom sitting after it; pushSwipe must still land on the atom's
    -- *current* position via 'resolveId', not silently miss and land a
    -- stray new atom instead. A *second*, direct edit lands on the atom
    -- itself (still via the same pre-rebase id) so the content actually
    -- being displaced differs from what that stale id's own, never-
    -- replayed commit still says -- otherwise this test can't tell "reads
    -- the atom's current content" apart from "reads whatever the stale id
    -- happens to still say", which coincide whenever only the atom's
    -- *position* (not its content) was rebased out from under it.
    it "pushSwipe still finds the atom after an earlier rebase changed its id" $ do
      let result = withFile_ runner (BranchName "b") $ do
            promptTid <- runStorage @Main (Core.store (Core.NonAtom [] "type:prompt\n\nhi"))
            atomTid   <- runStorage @Main (Ops.addAtom "chat/f.md" "first reply")
            -- Rebase the prompt in place, exactly like 'editChatPrompt' --
            -- this replays (re-hashes) the atom after it, even though the
            -- atom's own content is untouched by this step.
            _ <- runStorage @Main $ Core.at promptTid $ Core.editTick $ \case
              Core.NonAtom refs _ -> return (Core.NonAtom refs "type:prompt\n\nhi (edited)")
              other               -> return other
            -- Edit the atom itself too, still via the *stale* id --
            -- 'Ops.editAtomAt' resolves it internally, so this correctly
            -- lands on the atom's current position: its content is now
            -- "second reply", even though the stale id's own unreplayed
            -- commit still (and only ever) says "first reply".
            _ <- runStorage @Main (Ops.editAtomAt atomTid "second reply")
            -- Use the same stale atom id one more time, same as the
            -- caller. The content displaced into the swipe must be
            -- "second reply" -- what the atom actually, currently holds.
            _ <- runStorage @Main (Swipe.pushSwipe atomTid "third reply")
            fileState "chat/f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> do
          let ticks = updateTicks upd
              kinds = map wtKind ticks
          -- Exactly one atom (edited in place, twice) and one swipe
          -- holding the displaced content -- not two/three atoms.
          length (filter (== "atom") kinds) `shouldBe` 1
          length (filter (== "swipe") kinds) `shouldBe` 1
          [swipeMsg] <- return [wtMessage t | t <- ticks, wtKind t == "swipe"]
          swipeMsg `shouldBe` "second reply"
