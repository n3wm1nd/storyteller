{-# LANGUAGE OverloadedStrings #-}

-- | Sanity tests for "Storage.Ops"'s chain-editing operations
--   (chainPositions, deleteTick, moveTick, mergeAtoms, splitTick) --
--   ported from "Storyteller.Core.StorageMonad"'s tested behavior, but
--   against 'Storage.MockStore' and 'Storage.Core.ObjectHash' rather than
--   'Storyteller.Core.Types.TickId'.
module Storage.ChainEditSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.MockStore

-- | Three atoms on the same path, oldest first -- @content = "abc"@.
threeAtoms :: StoreM m => StoreT m (ObjectHash, ObjectHash, ObjectHash)
threeAtoms = do
  a <- addAtom "f.md" "a"
  b <- addAtom "f.md" "b"
  c <- addAtom "f.md" "c"
  return (a, b, c)

spec :: Spec
spec = do
  describe "chainPositions" $ do
    it "resolves each id's oldest-first position, root excluded" $ do
      let result = runChain $ do
            (a, b, c) <- threeAtoms
            chainPositions [a, b, c]
      case result of
        Left err -> expectationFailure err
        Right (positions, _finalState) -> map snd positions `shouldBe` [0, 1, 2]

  describe "deleteTick" $ do
    it "removes the tick and its content, tail replayed on top" $ do
      let result = fst <$> runChain (do
            (_, b, _) <- threeAtoms
            deleteTick b
            committedContent "f.md")
      result `shouldBe` Right "ac"

    it "a deleted id is no longer found by chainPositions" $ do
      let result = runChain $ do
            (_, b, _) <- threeAtoms
            deleteTick b
            chainPositions [b]
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected chainPositions to fail on a deleted id"

  describe "moveTick" $ do
    it "moves the last tick to the front" $ do
      let result = fst <$> runChain (do
            (_, _, c) <- threeAtoms
            _ <- moveTick c Nothing
            committedContent "f.md")
      result `shouldBe` Right "cab"

    it "moves the first tick to after the middle one" $ do
      let result = fst <$> runChain (do
            (a, b, _) <- threeAtoms
            _ <- moveTick a (Just b)
            committedContent "f.md")
      result `shouldBe` Right "bac"

    it "returns the moved tick's new id, resolvable via resolveId from the old one" $ do
      let result = runChain $ do
            (_, _, c) <- threeAtoms
            newC <- moveTick c Nothing
            resolved <- resolveId c
            return (resolved == newC)
      case result of
        Left err -> expectationFailure err
        Right (matches, _finalState) -> matches `shouldBe` True

    it "rejects moving a tick before its own reference" $ do
      -- note references a; moving note to the very front would place it
      -- before the thing it refers to.
      let result = runChain $ do
            (a, _, _) <- threeAtoms
            note <- store (NonAtom [a] "type:note\nabout a")
            moveTick note Nothing
      case result of
        Left err -> err `shouldContain` "before its own reference"
        Right _  -> expectationFailure "expected moveTick to reject this order"

    it "rejects moving a tick past something that references it" $ do
      -- note references a; moving a to right after note would place a
      -- after its own referencer.
      let result = runChain $ do
            (a, _, _) <- threeAtoms
            note <- store (NonAtom [a] "type:note\nabout a")
            moveTick a (Just note)
      case result of
        Left err -> err `shouldContain` "cannot move tick after tick that references it"
        Right _  -> expectationFailure "expected moveTick to reject this order"

  describe "mergeAtoms" $ do
    it "merges a contiguous run into one atom with concatenated content" $ do
      let result = fst <$> runChain (do
            (a, b, c) <- threeAtoms
            _ <- mergeAtoms [a, b, c]
            committedContent "f.md")
      result `shouldBe` Right "abc"

    it "merging leaves the tail intact" $ do
      let result = fst <$> runChain (do
            (a, b, _) <- threeAtoms
            _ <- addAtom "f.md" "d"
            _ <- mergeAtoms [a, b]
            committedContent "f.md")
      result `shouldBe` Right "abcd"

    it "fails with fewer than two ids" $ do
      let result = runChain (threeAtoms >>= \(a, _, _) -> mergeAtoms [a])
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected mergeAtoms to reject a single id"

    it "fails when the selection isn't contiguous" $ do
      let result = runChain $ do
            (a, _, c) <- threeAtoms
            mergeAtoms [a, c]
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected mergeAtoms to reject a gapped selection"

    it "fails when the atoms belong to different files" $ do
      let result = runChain $ do
            a <- addAtom "f.md" "a"
            b <- addAtom "g.md" "b"
            mergeAtoms [a, b]
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected mergeAtoms to reject atoms from different files"

  describe "splitTick" $ do
    it "splits one atom into pieces in place, preserving file content" $ do
      let result = fst <$> runChain (do
            _    <- addAtom "f.md" "x"
            abc  <- addAtom "f.md" "abc"
            _    <- addAtom "f.md" "y"
            _    <- splitTick abc ["a", "b", "c"]
            committedContent "f.md")
      result `shouldBe` Right "xabcy"

    it "the first piece inherits the original tick's incoming refs" $ do
      let result = runChain $ do
            abc      <- addAtom "f.md" "abc"
            referrer <- store (NonAtom [abc] "type:note\nabout abc")
            pieces   <- splitTick abc ["a", "b", "c"]
            case pieces of
              (firstPiece : _) -> do
                referrerTick <- readAt referrer (drop)
                return (tickRefs referrerTick == [firstPiece])
              [] -> fail "splitTick returned no pieces"
      case result of
        Left err -> expectationFailure err
        Right (matches, _finalState) -> matches `shouldBe` True

    it "fails on a single piece" $ do
      let result = runChain (addAtom "f.md" "abc" >>= \tid -> splitTick tid ["only"])
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected splitTick to reject a single piece"
