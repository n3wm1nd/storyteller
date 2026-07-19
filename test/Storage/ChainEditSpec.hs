{-# LANGUAGE OverloadedStrings #-}

-- | Sanity tests for "Storage.Ops"'s chain-editing operations
--   (chainPositions, deleteTick, moveTick, mergeAtoms, splitTick) --
--   ported from "Storyteller.Core.StorageMonad"'s tested behavior, but
--   against 'Storage.MockStore' and 'Storage.Core.ObjectHash' rather than
--   'Storyteller.Core.Types.TickId'.
module Storage.ChainEditSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.Text as T
import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.MockStore
import Storage.OpCounting

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

  describe "descendantsFirst" $ do
    it "an empty batch needs no lookup at all" $ do
      let result = fst <$> runChain (descendantsFirst [])
      result `shouldBe` Right []

    -- A single candidate is trivially "sorted" -- nothing to compare it
    -- against -- so this must skip the ancestry search entirely, not just
    -- happen to return quickly: a hash that was never actually written
    -- (so 'readCommit' on it would fail with 'Left') still comes back
    -- cleanly as 'Right', which only holds if no lookup was ever
    -- attempted on it at all.
    it "a single candidate comes back untouched, without even one lookup" $ do
      let bogus = ObjectHash "not-a-real-object"
          result = fst <$> runChain (descendantsFirst [bogus])
      result `shouldBe` Right [bogus]

    -- The fast path: candidates given oldest-first (the ordinary shape
    -- for a caller built from 'contentChain'-derived data, e.g. a
    -- client's own selection) are recognized as already-sorted, once
    -- reversed, via one combined descent -- not the general search's one
    -- independent walk per candidate. This doesn't observe the cheaper
    -- cost directly, only that the answer is still correct when the fast
    -- path's own hypothesis holds on the first try.
    it "recognizes an oldest-first input as already sorted once reversed" $ do
      let result = runChain $ do
            (a, b, c) <- threeAtoms
            ordered <- descendantsFirst [a, b, c]
            return (ordered, [c, b, a])
      case result of
        Left err -> expectationFailure err
        Right ((ordered, expected), _) -> ordered `shouldBe` expected

    it "orders three related ticks descendant-first, regardless of input order" $ do
      let result = runChain $ do
            (a, b, c) <- threeAtoms
            ordered <- descendantsFirst [a, c, b]
            return (ordered, [c, b, a])
      case result of
        Left err -> expectationFailure err
        Right ((ordered, expected), _) -> ordered `shouldBe` expected

    -- The actual reason this ordering matters: deleting in the order
    -- 'descendantsFirst' returns must never hit a 'deleteTick' failure
    -- from an id an earlier delete in the same batch already remapped
    -- away -- deleting in the *opposite* (ancestor-first) order would.
    it "deleting in the returned order never trips over a batch member's own remapped id" $ do
      let result = fst <$> runChain (do
            (a, b, _c) <- threeAtoms
            -- a and b together: a is b's own ancestor, so deleting a
            -- first (the wrong order) would remap b's id out from under
            -- the very next 'deleteTick' call in an unsorted 'mapM_'.
            ordered <- descendantsFirst [a, b]
            mapM_ deleteTick ordered
            committedContent "f.md")
      result `shouldBe` Right "c"

    -- Two ticks on entirely separate, unrelated chains (neither reachable
    -- from the other, no shared head) -- deleting one can never remap the
    -- other, so there's nothing for their relative order to protect
    -- against; this just pins that 'descendantsFirst' doesn't require (or
    -- fail without) a common chain to compare them against at all.
    it "two ticks on unrelated chains both come back, with no ordering imposed between them" $ do
      let result = runChain $ do
            treeH <- lift (writeObject (TreeObject []))
            chain1Root <- lift (writeCommit CommitData { commitParents = [], commitTree = treeH, commitMessage = "chain 1" })
            chain2Root <- lift (writeCommit CommitData { commitParents = [], commitTree = treeH, commitMessage = "chain 2" })
            ordered <- descendantsFirst [chain1Root, chain2Root]
            return (ordered, [chain1Root, chain2Root])
      case result of
        Left err -> expectationFailure err
        Right ((ordered, both), _) -> ordered `shouldMatchList` both

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

  describe "deleteTicks (general entry point: any order, any combination of chains)" $ do
    it "removes every target regardless of input order, same as looping deleteTick would" $ do
      let result = fst <$> runChain (do
            (a, b, c) <- threeAtoms
            deleteTicks [a, c]  -- given ancestor-first (the wrong order for a naive loop) -- must still work
            committedContent "f.md")
      result `shouldBe` Right "b"

    -- Every real caller (Server.Core.File.deleteFileAtoms and friends)
    -- only ever passes candidates already filtered from *one* connection's
    -- own tick map, so in practice every candidate in a single call is
    -- always reachable from that same scope's current head -- the "several
    -- components" case 'descendantsFirstGrouped' supports is a defensive
    -- correctness property against genuinely foreign input, not something
    -- real usage exercises. 'at'-based deletion is inherently scope-relative
    -- (it can only ever reach what's reachable from *this* scope's own
    -- head), so a target on a truly separate, unrelated chain (a
    -- parentless second commit tree with no ref of its own, same shape
    -- 'descendantsFirst's own "two unrelated chains" test uses) can never
    -- legitimately be deleted this way, grouping or not -- this pins that
    -- 'deleteTicks' fails the *whole* batch loudly rather than silently
    -- applying only the valid part, the same all-or-nothing behavior
    -- every other multi-target batch op in this codebase already has.
    it "fails the whole batch, not just the invalid part, when a target is on a genuinely unreachable chain" $ do
      let result = runChain (do
            (a1, _b1, c1) <- threeAtoms
            treeH <- lift (writeObject (TreeObject []))
            foreignRoot <- lift (writeCommit CommitData { commitParents = [], commitTree = treeH, commitMessage = "x" })
            deleteTicks [foreignRoot, a1, c1])
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected deleteTicks to fail on an unreachable target"

    -- The actual point: nesting 'at' instead of looping it means one
    -- continuous dive-and-replay instead of one independent round trip
    -- per target, each of which would otherwise re-walk (and re-replay)
    -- whatever the previous one just finished replaying. A chain with
    -- real filler between each target (so there's real replay work to
    -- redo, not just adjacent no-op hops) makes the difference concrete
    -- rather than asserted.
    it "costs strictly fewer store operations than looping deleteTick over the same targets" $ do
      let gap = 5 :: Int
          k   = 4 :: Int
          scenario = mapM_
            (\i -> mapM_ (\j -> () <$ addAtom "f.md" (T.pack (show i <> "-" <> show j))) [1 .. gap]
                     >> () <$ addAtom "f.md" "TARGET")
            [1 .. k]
          -- Discovered fresh within the *measured* phase itself, walking
          -- from head, never smuggled out of the uninstrumented setup --
          -- same discipline "Storage.StoreOpCountSpec" already follows.
          -- Identical in both variants below, so this walk's own cost
          -- cancels out of the comparison either way.
          findTargets = headHash >>= go
            where
              go h = do
                (cd, t) <- lift (readCommitTick h)
                let isTarget = case t of Atom _ _ _ c -> c == "TARGET"; _ -> False
                rest <- case commitParents cd of
                  []      -> return []
                  (p : _) -> go p
                return (if isTarget then h : rest else rest)
          naive = snd <$> runMeasuring scenario (findTargets >>= mapM_ deleteTick)
          fast  = snd <$> runMeasuring scenario (findTargets >>= deleteTicks)
      case (naive, fast) of
        (Right nc, Right fc) ->
          (ocReads fc + ocWrites fc) `shouldSatisfy` (< ocReads nc + ocWrites nc)
        _ -> expectationFailure ("measurement failed: naive=" <> show naive <> " fast=" <> show fast)

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
