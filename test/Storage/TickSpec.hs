{-# LANGUAGE LambdaCase #-}
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

import Storyteller.Core.Types
  ( BranchName(..), Root(..), TickId(..)
  , fromTick, tickData, tickMessage, tickFields, tickPos, posParent, posRefs
  )
import qualified Storyteller.Core.Types as ST
import Storyteller.Common.Types (Note(..))

spec :: Spec
spec = do
  describe "encodeTickData" $ do
    -- The header block is one line per field ("key:value", joined by "\n",
    -- see encodeTickData's own Haddock) -- an embedded newline in a field's
    -- own key or value would either get silently dropped (a colon-less
    -- continuation line the decoder can't parse as a field) or, worse, a
    -- field value containing a literal blank line would produce a spurious
    -- "\n\n" the decoder mistakes for the real header/payload boundary,
    -- swallowing every later field and the real message into what it
    -- thinks is this field's own tail. Rejecting outright at encode time
    -- turns that into a loud failure instead of a silent corruption.
    it "fails when a field value contains a newline" $ do
      let bad = ST.TickData { ST.tickRefs = [], ST.tickFields = [("question", "line one\nline two")], ST.tickMessage = "" }
          result = fst <$> runChain (encodeTickData bad)
      result `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "fails when a field value contains a blank line (two consecutive newlines)" $ do
      let bad = ST.TickData { ST.tickRefs = [], ST.tickFields = [("question", "para one\n\npara two")], ST.tickMessage = "" }
          result = fst <$> runChain (encodeTickData bad)
      result `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "fails when a field key contains a newline" $ do
      let bad = ST.TickData { ST.tickRefs = [], ST.tickFields = [("bad\nkey", "value")], ST.tickMessage = "" }
          result = fst <$> runChain (encodeTickData bad)
      result `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "succeeds when no field contains a newline, even if the message itself does" $ do
      let ok = ST.TickData { ST.tickRefs = [], ST.tickFields = [("a", "b")], ST.tickMessage = "hello\n\nworld" }
          result = fst <$> runChain (encodeTickData ok)
      result `shouldSatisfy` \case { Left _ -> False; Right _ -> True }
  describe "decodeTickData / a payload with its own blank line" $ do
    -- Regression: the header/payload boundary used to be found by
    -- scanning for the first *blank* line, which misfired the moment a
    -- fieldless tick's own multi-paragraph payload contained one --
    -- mistaking it for the header separator, corrupting the tick to
    -- decode with no recognizable "type" at all. The boundary is now
    -- unambiguous ('encodeDraft' always folds "type" into the header, so
    -- there's always a real blank line before the payload, and it's
    -- always the first one) regardless of how many more blank lines the
    -- payload itself goes on to contain.
    it "a multi-paragraph Note payload round-trips whole, and still decodes as a Note" $ do
      let paragraphs = "First paragraph.\n\nSecond paragraph, after a blank line.\n\nThird."
          result = fst <$> runChain (do
            _ <- storeAs (Note [] paragraphs)
            t <- getTypesTick
            return (fromTick t :: Maybe Note))
      result `shouldBe` Right (Just (Note [] paragraphs))

  describe "storeAs / getTypesTick round trip for a non-atom tick" $ do
    it "stores and decodes back to the original typed value" $ do
      let result = fst <$> runChain (do
            _ <- storeAs (Root (BranchName "main"))
            t <- getTypesTick
            return (fromTick t :: Maybe Root))
      case result of
        Right (Just (Root (BranchName n))) -> n `shouldBe` "main"
        other -> expectationFailure ("expected a decoded Root, got " <> show (fmap (const ()) <$> other))

    it "round-trips the raw message as pure payload, with the type tag folded into fields" $ do
      -- The type tag lives in 'tickFields' (as an ordinary @"type"@ entry,
      -- always first -- see 'Storyteller.Core.Types.encodeDraft') rather
      -- than embedded in 'tickMessage' itself, so the message decodes back
      -- to exactly the payload that was stored, with nothing left to
      -- strip off it.
      let result = fst <$> runChain (do
            _ <- storeAs (Root (BranchName "main"))
            t <- getTypesTick
            return (tickFields (tickData t), tickMessage (tickData t)))
      result `shouldBe` Right ([("type", "root")], "main")

  describe "readTypesTick for an atom" $ do
    it "reconstructs the \"file\" field and folds in a \"type\":\"atom\" field, message as pure content" $ do
      let result = fst <$> runChain (do
            h <- addAtom "scene.md" "p1\n"
            t <- readTypesTick h
            return (tickFields (tickData t), tickMessage (tickData t)))
      result `shouldBe` Right ([("type", "atom"), ("file", "scene.md")], "p1\n")

  describe "readTypesTick for Binary and Opaque" $ do
    -- Both are content-free at this layer by design (see Storage.Core's
    -- own Tick Haddock) -- the point of these tests is just that decoding
    -- one doesn't crash with a non-exhaustive pattern match, the way it
    -- did before Binary/Opaque were added here.
    it "decodes a Binary tick without crashing, carrying its own path as a field" $ do
      let result = fst <$> runChain (do
            writeFile "portrait.png" "\xFF\xFE\x00"
            h <- store (Binary [] "portrait.png")
            t <- readTypesTick h
            return (tickFields (tickData t), tickMessage (tickData t)))
      result `shouldBe` Right ([("type", "binary"), ("file", "portrait.png")], "")

    it "decodes an Opaque tick without crashing, carrying no fields at all" $ do
      -- No "type" field either -- 'Opaque' is the fallthrough for content
      -- we don't own (an external edit, legacy data, ...), not a real
      -- registered 'TickType', so nothing should claim it decodes as one.
      let result = fst <$> runChain (do
            h <- store (Opaque [])
            t <- readTypesTick h
            return (tickFields (tickData t), tickMessage (tickData t)))
      result `shouldBe` Right ([], "")

  describe "fileTicksOf alongside a Binary/Opaque tick" $ do
    it "walks past a Binary tick for an unrelated path without crashing" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            writeFile "portrait.png" "\xFF\xFE\x00"
            _ <- store (Binary [] "portrait.png")
            fileTicksOf "scene.md")
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "p1\n"]

    it "walks past an Opaque tick without crashing" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            _ <- store (Opaque [])
            fileTicksOf "scene.md")
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "p1\n"]

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
            _ <- store (NonAtom [h] "type:note\n\nabout p1")
            fileTicksOf "scene.md"
      case result of
        Left err -> expectationFailure err
        Right (ticks, _finalState) -> map ftKind ticks `shouldBe` ["atom", "note"]
