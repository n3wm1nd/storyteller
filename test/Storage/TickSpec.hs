{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sanity tests for "Storage.Tick" -- the bridge between "Storage.Core"'s
--   Atom\/NonAtom vocabulary and "Storyteller.Core.Types"'s typed ticks.
module Storage.TickSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import qualified Data.Text as T
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

  describe "recentAtomsOf" $ do
    it "keeps every atom when all are reference-free and both bounds have room to spare" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "journal.md" "a1"
            _ <- addAtom "journal.md" "a2"
            _ <- addAtom "journal.md" "a3"
            recentAtomsOf "journal.md" 30 10 1)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "a1", Just "a2", Just "a3"]

    it "drops an atom that's a verbatim, unedited copy of its reference (padding 0)" $ do
      let result = fst <$> runChain (do
            h1 <- addAtom "scene.md" "witness content"
            _  <- addAtomWithRefs [h1] "journal.md" "witness content"
            recentAtomsOf "journal.md" 30 10 0)
      result `shouldBe` Right []

    it "keeps an atom whose content has diverged from its reference" $ do
      let result = fst <$> runChain (do
            h1 <- addAtom "scene.md" "orig"
            _  <- addAtomWithRefs [h1] "journal.md" "orig, but embellished"
            recentAtomsOf "journal.md" 30 10 0)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "orig, but embellished"]

    it "keeps an atom whose reference target no longer resolves to matching content, even with no divergence check possible (empty refs after deletion is out of scope; this checks non-atom refs are simply never a match)" $ do
      -- A reference to a *non-atom* tick (nothing this scheme could ever
      -- copy verbatim from) can never equal this atom's own content, so it
      -- always counts as diverged -- same as having no reference at all.
      let result = fst <$> runChain (do
            noteId <- store (NonAtom [] "type:note\n\nsome note")
            _      <- addAtomWithRefs [noteId] "journal.md" "original journal text"
            recentAtomsOf "journal.md" 30 10 0)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "original journal text"]

    it "pulls in padding neighbours (otherwise-excluded copies) on both sides of a diverged atom" $ do
      let result = fst <$> runChain (do
            s1 <- addAtom "scene.md" "s1"
            _  <- addAtomWithRefs [s1] "journal.md" "s1"                 -- unqualified copy
            s2 <- addAtom "scene.md" "s2"
            _  <- addAtomWithRefs [s2] "journal.md" "s2, edited"          -- qualifies
            s3 <- addAtom "scene.md" "s3"
            _  <- addAtomWithRefs [s3] "journal.md" "s3"                 -- unqualified copy
            recentAtomsOf "journal.md" 30 10 1)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "s1", Just "s2, edited", Just "s3"]

    it "without padding, only the diverged atom itself survives from the same sequence" $ do
      let result = fst <$> runChain (do
            s1 <- addAtom "scene.md" "s1"
            _  <- addAtomWithRefs [s1] "journal.md" "s1"
            s2 <- addAtom "scene.md" "s2"
            _  <- addAtomWithRefs [s2] "journal.md" "s2, edited"
            s3 <- addAtom "scene.md" "s3"
            _  <- addAtomWithRefs [s3] "journal.md" "s3"
            recentAtomsOf "journal.md" 30 10 0)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "s2, edited"]

    it "caps output at maxOut, keeping the most recent atoms" $ do
      let result = fst <$> runChain (do
            mapM_ (\n -> addAtom "journal.md" (T.pack ("a" <> show n))) [1 .. (5 :: Int)]
            recentAtomsOf "journal.md" 30 2 1)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "a4", Just "a5"]

    it "stops examining once lookback on-path atoms have been seen, never reaching further back" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "journal.md" "too-old-1"
            _ <- addAtom "journal.md" "too-old-2"
            _ <- addAtom "journal.md" "recent-1"
            _ <- addAtom "journal.md" "recent-2"
            recentAtomsOf "journal.md" 2 10 0)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "recent-1", Just "recent-2"]

    it "skips atoms on other paths without counting them against lookback" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "journal.md" "j1"
            _ <- addAtom "scene.md" "unrelated"
            _ <- addAtom "journal.md" "j2"
            recentAtomsOf "journal.md" 2 10 0)
      case result of
        Left err -> expectationFailure err
        Right ticks -> map ftContent ticks `shouldBe` [Just "j1", Just "j2"]
