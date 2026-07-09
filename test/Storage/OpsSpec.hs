{-# LANGUAGE OverloadedStrings #-}

-- | Quick sanity tests for "Storage.Ops" -- addAtom, findAtom, editAtom,
--   replaceAtom -- against 'Storage.MockStore', the same mock
--   "Storage.CoreSpec" uses.
module Storage.OpsSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.ByteString as BS

import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.MockStore

spec :: Spec
spec = do
  describe "addAtom" $ do
    it "commits the content as a new atom, readable back from the chain" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            committedContent "scene.md")
      result `shouldBe` Right "p1\n"

    it "also lands the same content in the ambient tree" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            readFile "scene.md")
      result `shouldBe` Right "p1\n"

    it "a second addAtom builds on the first, in both the chain and the ambient tree" $ do
      let result = fst <$> runChain (do
            _       <- addAtom "scene.md" "p1\n"
            _       <- addAtom "scene.md" "p2\n"
            chain   <- committedContent "scene.md"
            ambient <- readFile "scene.md"
            return (chain, ambient))
      result `shouldBe` Right ("p1\np2\n", "p1\np2\n")

    it "doesn't disturb an unrelated pending ambient write" $ do
      let result = fst <$> runChain (do
            writeFile "scratch.md" "pending"
            _       <- addAtom "scene.md" "p1\n"
            ambient <- readFile "scratch.md"
            return ambient)
      result `shouldBe` Right "pending"

  describe "findAtom" $ do
    it "returns the start itself when it's already an atom" $ do
      let result = runChain (do
            t1 <- store (Atom [] "scene.md" [] "p1\n")
            found <- findAtom t1
            return (found == t1))
      case result of
        Left err -> expectationFailure err
        Right (isSelf, _finalState) -> isSelf `shouldBe` True

    it "walks back past NonAtoms to the nearest preceding atom" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" [] "p1\n")
            _  <- store (NonAtom [] "type:note\na note")
            _  <- store (NonAtom [] "type:note\nanother note")
            h  <- headHash
            found <- findAtom h
            return (found == t1)
      case result of
        Left err -> expectationFailure err
        Right (isT1, _finalState) -> isT1 `shouldBe` True

    it "fails when there's no atom anywhere in history" $ do
      let result = fst <$> runChain (do
            _ <- store (NonAtom [] "type:note\njust a note")
            h <- headHash
            findAtom h)
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected findAtom to fail with no atom in history"

  describe "editAtom / replaceAtom" $ do
    it "editAtom applies the mapping function to the nearest atom's content" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- editAtom (<> " (edited)")
            committedContent "scene.md")
      result `shouldBe` Right "p1\n (edited)"

    it "editAtom finds the nearest atom through intervening NonAtoms" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- addAtom "scene.md" "p2\n"
            _ <- store (NonAtom [] "type:note\nabout p2")
            _ <- editAtom (const "p2-revised\n")
            committedContent "scene.md")
      result `shouldBe` Right "p1\np2-revised\n"

    it "replaceAtom is editAtom with a constant function" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- replaceAtom "replaced\n"
            committedContent "scene.md")
      result `shouldBe` Right "replaced\n"

    it "editAtom preserves the edited atom's own refs" $ do
      let result = runChain $ do
            r  <- store (NonAtom [] "type:note\na referenced tick")
            t1 <- store (Atom [r] "scene.md" [] "p1\n")
            _  <- editAtom (const "p1-revised\n")
            h  <- headHash
            tick <- lift (readTick h)
            return (tickRefs tick, t1)
      case result of
        Left err -> expectationFailure err
        Right ((refs, _t1), _finalState) -> length refs `shouldBe` 1

  describe "setAtomHidden" $ do
    it "tags the atom hidden without touching its content" $ do
      let result = fst <$> runChain (do
            t1 <- addAtom "scene.md" "p1\n"
            _  <- setAtomHidden t1 True
            h  <- headHash
            lift (readTick h))
      result `shouldBe` Right (Atom [] "scene.md" [("hide", "true")] "p1\n")

    it "unhiding clears the tag again" $ do
      let result = fst <$> runChain (do
            t1 <- addAtom "scene.md" "p1\n"
            _  <- setAtomHidden t1 True
            _  <- setAtomHidden t1 False
            h  <- headHash
            lift (readTick h))
      result `shouldBe` Right (Atom [] "scene.md" [] "p1\n")

    it "hiding an atom earlier in the chain doesn't disturb a later atom's own content" $ do
      let result = fst <$> runChain (do
            t1 <- addAtom "scene.md" "p1\n"
            _  <- addAtom "scene.md" "p2\n"
            _  <- setAtomHidden t1 True
            committedContent "scene.md")
      result `shouldBe` Right "p1\np2\n"

  describe "addBinary" $ do
    it "commits a path-aware Binary tick, not an Atom" $ do
      let bytes = BS.pack [0xFF, 0xFE, 0x00]
      let result = fst <$> runChain (do
            _ <- addBinary "portrait.png" bytes
            h <- headHash
            lift (readTick h))
      result `shouldBe` Right (Binary [] "portrait.png")

    it "lands the exact bytes in the committed tree" $ do
      let bytes = BS.pack [0xFF, 0xFE, 0x00]
      let result = fst <$> runChain (do
            _ <- addBinary "portrait.png" bytes
            committedContent "portrait.png")
      result `shouldBe` Right bytes

    it "never registers as atom-tracked" $ do
      let result = fst <$> runChain (do
            _ <- addBinary "portrait.png" (BS.pack [0xFF])
            hasAnyAtom "portrait.png")
      result `shouldBe` Right False

    -- Binary's own 'store' case reads the *ambient* tree to decide its
    -- content -- correct for a fresh commit (see 'addBinary'), but a
    -- rebase (here, 'deleteTick' removing an unrelated earlier atom)
    -- replays this same tick later by re-'store'-ing it, at which point
    -- ambient state has nothing to do with what this tick originally
    -- committed. Reproduces exactly that: ambient is deliberately
    -- clobbered with different bytes (an unrelated pending edit) before
    -- the rebase runs, and the replayed Binary tick must still reproduce
    -- its own *original* content, not whatever ambient now holds.
    it "a rebase replaying past a Binary tick still reproduces its own original content" $ do
      let original = BS.pack [0xFF, 0xFE, 0x00]
          unrelated = BS.pack [0x11, 0x22, 0x33, 0x44]
      let result = fst <$> runChain (do
            t1 <- addAtom "a.md" "hello\n"
            _  <- addBinary "portrait.png" original
            writeFile "portrait.png" unrelated
            _  <- addAtom "a.md" " world\n"
            deleteTick t1
            committedContent "portrait.png")
      result `shouldBe` Right original

  describe "findCreationTick" $ do
    it "finds the file's only atom when it was never deleted" $ do
      let result = fst <$> runChain (do
            t1 <- addAtom "scene.md" "p1\n"
            _  <- addAtom "scene.md" "p2\n"
            found <- findCreationTick "scene.md"
            return (found == t1))
      result `shouldBe` Right True

    -- The file's *current* lifetime -- not its very first one -- is what
    -- a caller renaming "the file as it stands" actually wants.
    it "finds the most recent creation, not the original one, after a delete-and-recreate" $ do
      let result = fst <$> runChain (do
            _   <- addAtom "scene.md" "old life\n"
            _   <- deleteFile "scene.md"
            t2  <- addAtom "scene.md" "new life\n"
            found <- findCreationTick "scene.md"
            return (found == t2))
      result `shouldBe` Right True

  describe "renameFile" $ do
    it "moves a single atom's content to the new path, nothing left at the old one" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            renameFile "scene.md" "chapter1.md"
            old <- Storage.Ops.exists "scene.md"
            new <- committedContent "chapter1.md"
            return (old, new))
      result `shouldBe` Right (False, "p1\n")

    -- The whole point: a rename at the creation tick propagates through
    -- every later atom on the old path as it replays, not just the first.
    it "propagates through every atom appended after creation" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- addAtom "scene.md" "p2\n"
            _ <- addAtom "scene.md" "p3\n"
            renameFile "scene.md" "chapter1.md"
            committedContent "chapter1.md")
      result `shouldBe` Right "p1\np2\np3\n"

    -- Disambiguation: an earlier, already-deleted life of the same path
    -- must be left alone -- 'findCreationTick' targets the *current*
    -- lifetime, so the rename never even reaches the old one's ticks.
    it "does not touch an earlier, already-deleted life of the same path" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "old life\n"
            _ <- deleteFile "scene.md"
            _ <- addAtom "scene.md" "new life\n"
            renameFile "scene.md" "chapter1.md"
            old     <- Storage.Ops.exists "scene.md"
            renamed <- committedContent "chapter1.md"
            return (old, renamed))
      result `shouldBe` Right (False, "new life\n")

    -- Mirrors the delete-and-recreate case above, but the old path is
    -- freed up by a *rename* instead of an explicit 'deleteFile': the old
    -- atoms' own 'atomPath' becomes the new name, so nothing with the old
    -- path is left in the chain at all, and a later reuse of that name is
    -- an entirely fresh, unrelated lifetime.
    it "a path freed by rename can be reused for an unrelated new file, and the new one renames independently" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "file1" "old life\n"
            _ <- addAtom "file1" "old life 2\n"
            renameFile "file1" "file2"
            _ <- addAtom "file1" "new life\n"
            _ <- addAtom "file1" "new life 2\n"
            renameFile "file1" "file3"
            oldGone <- Storage.Ops.exists "file1"
            file2   <- committedContent "file2"
            file3   <- committedContent "file3"
            return (oldGone, file2, file3))
      result `shouldBe` Right (False, "old life\nold life 2\n", "new life\nnew life 2\n")

    it "an atom edited (not just appended) after creation is also renamed correctly" $ do
      let result = fst <$> runChain (do
            t1 <- addAtom "scene.md" "p1\n"
            _  <- addAtom "scene.md" "p2\n"
            _  <- editAtomAt t1 "p1-revised\n"
            renameFile "scene.md" "chapter1.md"
            committedContent "chapter1.md")
      result `shouldBe` Right "p1-revised\np2\n"
