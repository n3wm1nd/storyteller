{-# LANGUAGE OverloadedStrings #-}

-- | Sanity tests for "Storage.Tick" -- the bridge between "Storage.Core"'s
--   Atom\/NonAtom vocabulary and "Storyteller.Core.Types"'s typed ticks.
module Storage.TickSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.Tick
import Storage.MockStore

import Storyteller.Core.Types (BranchName(..), Root(..), TickId(..), fromTick, tickData, tickMessage, tickFields, tickPos, posParent, posRefs)

spec :: Spec
spec = do
  describe "storeAs / getTypesTick round trip for a non-atom tick" $ do
    it "stores and decodes back to the original typed value" $ do
      let result = fst <$> runChain (do
            _ <- storeAs (Root (BranchName "main"))
            t <- getTypesTick
            return (fromTick t :: Maybe Root))
      case result of
        Right (Just (Root (BranchName n))) -> n `shouldBe` "main"
        other -> expectationFailure ("expected a decoded Root, got " <> show (fmap (const ()) <$> other))

    it "round-trips the raw message verbatim, unaffected by decodeTickData's field-line quirk" $ do
      -- 'decodeTickData' (copied from 'Storyteller.Core.StorageMonad',
      -- unchanged) only recognizes a real field block when it's followed
      -- by a blank line; a message with no such separator can still have
      -- its first line misparsed as a stray field if a later line happens
      -- to lack its own colon (as "main" does here, no different from
      -- StorageMonad's own pre-existing behavior) -- but the message text
      -- itself always survives whole, which is what 'fromTick' actually
      -- depends on.
      let result = fst <$> runChain (do
            _ <- storeAs (Root (BranchName "main"))
            t <- getTypesTick
            return (tickMessage (tickData t)))
      result `shouldBe` Right "type:root\nmain"

  describe "readTypesTick for an atom" $ do
    it "reconstructs the \"file\" field and \"type:atom\" tag Storage.Core strips off" $ do
      let result = fst <$> runChain (do
            h <- addAtom "scene.md" "p1\n"
            t <- readTypesTick h
            return (tickFields (tickData t), tickMessage (tickData t)))
      result `shouldBe` Right ([("file", "scene.md")], "type:atom\np1\n")

  describe "tick position" $ do
    it "posParent of the second atom is the first atom's own id" $ do
      let result = fst <$> runChain (do
            h1 <- addAtom "scene.md" "p1\n"
            h2 <- addAtom "scene.md" "p2\n"
            t2 <- readTypesTick h2
            return (posParent (tickPos t2) == Just (TickId (unObjectHash h1))))
      result `shouldBe` Right True

    it "posRefs reflects the tick's own cross-branch refs" $ do
      let result = fst <$> runChain (do
            h1 <- addAtom "scene.md" "p1\n"
            h2 <- store (NonAtom [h1] "type:note\nabout p1")
            t2 <- readTypesTick h2
            return (posRefs (tickPos t2) == [TickId (unObjectHash h1)]))
      result `shouldBe` Right True

  describe "fileTicksOf" $ do
    it "returns oldest-first atoms whose own content reconstructs the file" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- addAtom "scene.md" "p2\n"
            fileTicksOf "scene.md")
      case result of
        Left err -> expectationFailure err
        Right ticks -> do
          length ticks `shouldBe` 2
          map ftContent ticks `shouldBe` [Just "p1\n", Just "p2\n"]

    it "excludes atoms on a different file" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- addAtom "other.md" "unrelated\n"
            fileTicksOf "scene.md")
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "p1\n"]

    it "includes a note that references one of the file's own atoms" $ do
      let result = runChain $ do
            h <- addAtom "scene.md" "p1\n"
            _ <- store (NonAtom [h] "type:note\nabout p1")
            fileTicksOf "scene.md"
      case result of
        Left err -> expectationFailure err
        Right (ticks, _finalState) -> map ftKind ticks `shouldBe` ["atom", "note"]
