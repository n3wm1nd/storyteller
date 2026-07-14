{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Storyteller.Common.Swipe" -- the alternate-generation
--   ("swipe") ring: pushing a new alternate, and cycling through however
--   many already exist. Same lightweight harness as
--   "Storage.ChainEditSpec" ('Storage.MockStore') -- these functions only
--   need 'StoreM'\/'StoreT', no Polysemy effect stack.
module Storyteller.Common.SwipeSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Data.List (sort)
import Test.Hspec

import Storage.Core
import Storage.Ops (addAtom, editAtomAt)
import Storage.MockStore
import Storyteller.Common.Swipe

spec :: Spec
spec = do
  describe "pushSwipe" $ do
    it "replaces the atom's content and keeps the old content as a swipe" $ do
      let result = runChain $ do
            a <- addAtom "f.md" "original"
            newTid <- pushSwipe a "fresh"
            content <- committedContent "f.md"
            return (newTid, content)
      case result of
        Left err -> expectationFailure err
        Right ((_newTid, content), _) -> content `shouldBe` "fresh"

    it "the displaced content lands as a swipe immediately after the atom" $ do
      -- With nothing else in the chain, the swipe 'pushSwipe' inserts
      -- right after the atom is itself left as head -- read it directly.
      let result = runChain $ do
            a <- addAtom "f.md" "original"
            _ <- pushSwipe a "fresh"
            drop
      case result of
        Left err -> expectationFailure err
        Right (t, _) -> case t of
          NonAtom _ msg -> msg `shouldBe` "type:swipe\n\noriginal"
          _             -> expectationFailure "expected the tick right after the atom to be a swipe"

    it "reads the *current* content to displace, even when called with a stale (already-superseded) id" $ do
      -- 'cycleSwipe' resolves its argument via 'resolveId' before doing
      -- anything else (see its own Haddock) precisely because a caller may
      -- be holding an id an earlier edit in the same scope has since
      -- replaced. 'pushSwipe'/'swapAtomContent' makes no such promise: it
      -- reads the old content straight off the (possibly-stale) id it's
      -- given. Edit the atom once behind the caller's back, then push a
      -- swipe on the now-stale original id -- the content that edit
      -- produced ("v1") should land as the preserved alternate, not the
      -- content from before it.
      let result = runChain $ do
            a0 <- addAtom "f.md" "v0"
            _  <- editAtomAt a0 "v1"          -- caller's `a0` is now stale
            _  <- pushSwipe a0 "v2"           -- pushed against the stale id
            content <- committedContent "f.md"
            drop >>= \t -> return (content, t)
      case result of
        Left err -> expectationFailure err
        Right ((content, tick), _) -> do
          content `shouldBe` "v2"
          case tick of
            NonAtom _ msg -> msg `shouldBe` "type:swipe\n\nv1"
            _             -> expectationFailure "expected a swipe preserving \"v1\""

    it "leaves everything already after the atom untouched" $ do
      let result = fst <$> runChain (do
            a <- addAtom "f.md" "original"
            _ <- addAtom "g.md" "unrelated"
            _ <- pushSwipe a "fresh"
            committedContent "g.md")
      result `shouldBe` Right "unrelated"

  describe "cycleSwipe" $ do
    it "fails when the atom has no alternates" $ do
      let result = runChain (addAtom "f.md" "only" >>= cycleSwipe)
      case result of
        Left err -> err `shouldContain` "no alternates"
        Right _  -> expectationFailure "expected cycleSwipe to fail with no alternates"

    it "with one alternate, two cycles round-trip back to the original content" $ do
      let result = runChain $ do
            a <- addAtom "f.md" "original"
            a1 <- pushSwipe a "alt"
            _  <- cycleSwipe a1
            firstCycle <- committedContent "f.md"
            _  <- cycleSwipe a1
            secondCycle <- committedContent "f.md"
            return (firstCycle, secondCycle)
      case result of
        Left err -> expectationFailure err
        Right ((first, second), _) -> do
          first `shouldBe` "original"
          second `shouldBe` "alt"

    it "rotates through three alternates like a ring, never losing one" $ do
      let result = runChain $ do
            a0 <- addAtom "f.md" "v0"
            a1 <- pushSwipe a0 "v1"
            a2 <- pushSwipe a1 "v2"
            a3 <- pushSwipe a2 "v3"
            -- Live content is now "v3"; the carousel holds v2, v1, v0
            -- (nearest-atom first). Cycle four times -- one full loop over
            -- four total generations (v0..v3) -- and confirm we're back to
            -- "v3", visiting every other value exactly once along the way.
            -- 'cycleSwipe' resolves its argument itself, so the same
            -- (by-now-stale) id can be reused on every step.
            seen <- sequence [cycleSwipe a3 >> committedContent "f.md" | _ <- [1 :: Int, 2, 3, 4]]
            final <- committedContent "f.md"
            return (seen, final)
      case result of
        Left err -> expectationFailure err
        Right ((seen, final), _) -> do
          sort seen `shouldBe` sort ["v0", "v1", "v2", "v3"]
          final `shouldBe` "v3"

    it "survives a later, unrelated tick after the atom's own carousel" $ do
      let result = runChain $ do
            a  <- addAtom "f.md" "original"
            a1 <- pushSwipe a "alt"
            _  <- addAtom "g.md" "later and unrelated"
            _  <- cycleSwipe a1
            (,) <$> committedContent "f.md" <*> committedContent "g.md"
      case result of
        Left err -> expectationFailure err
        Right ((fContent, gContent), _) -> do
          fContent `shouldBe` "original"
          gContent `shouldBe` "later and unrelated"
