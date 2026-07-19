{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.SummarySpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary
import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types
import Storyteller.Writer.Agent.Summarizer (runSummarizer, runSummarizerForPath)

-- ---------------------------------------------------------------------------
-- Phantom + single-branch runner. There's no alt branch to open -- an
-- alternate chain has no ref of its own (see Storyteller.Common.Summary's
-- module Haddock); 'runSummarizer' extends it purely by hash.
-- ---------------------------------------------------------------------------

data Source

runOne action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "source")
      runBranchAndFS @Source (BranchName "source") action

-- | Always writes the same one file, so 'runSummarizer' has something to
--   commit every time it's called.
generateFixed :: Text -> [Tick] -> Sem r (Map.Map FilePath Text)
generateFixed content _ticks = pure (Map.singleton "story-so-far.md" content)

-- | Writes two files at once -- for checking that a whole batch lands in
--   a single alt-chain commit, not one per file.
generateTwoFiles :: Text -> Text -> [Tick] -> Sem r (Map.Map FilePath Text)
generateTwoFiles a b _ticks = pure (Map.fromList [("a.md", a), ("b.md", b)])

spec :: Spec
spec = do
  describe "Summary (TickType round-trip)" $ do
    it "toDraft/fromTick round-trips kind and altHead" $ do
      let s = Summary { summaryKind = "prose/chapter", summaryAltHead = TickId "deadbeef" }
      fromTick @Summary (Tick (TickPos (TickId "x") Nothing []) (toDraft s)) `shouldBe` Just s

  describe "runSummarizer" $ do
    it "extends the alternate chain and records a Summary tick on source" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "some narrative"))
            mtid <- runSummarizer @Source "prose/chapter"
              (generateFixed "chapter one, summarized")
            sums <- runStorage @Source (availableSummaries Nothing)
            content <- case sums of
              [(_, s)] -> runStorage @Source (summaryContent s "story-so-far.md")
              _        -> return Nothing
            return (mtid, content, map (summaryKind . snd) sums)
      case result of
        Left err -> expectationFailure err
        Right (mtid, content, kinds) -> do
          mtid `shouldNotBe` Nothing
          content `shouldBe` Just "chapter one, summarized"
          kinds `shouldBe` ["prose/chapter"]

    it "returns Nothing when there is nothing new to summarize" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "some narrative"))
            _ <- runSummarizer @Source "prose/chapter" (generateFixed "first pass")
            runSummarizer @Source "prose/chapter" (generateFixed "should never run")
      result `shouldBe` Right Nothing

    it "a second summarization appends: content updates, but the earlier commit stays reachable through its own Summary tick" $ do
      let result = runOne $ do
            _     <- runStorage @Source (Core.store (Core.NonAtom [] "part one"))
            mtid1 <- runSummarizer @Source "prose/chapter" (generateFixed "summary v1")
            _     <- runStorage @Source (Core.store (Core.NonAtom [] "part two"))
            mtid2 <- runSummarizer @Source "prose/chapter" (generateFixed "summary v2")

            sums <- runStorage @Source (availableSummaries (Just "prose/chapter"))
            -- most-recent-first: the live content is whichever the *newest* tick names.
            liveContent <- case sums of
              (( _, newest) : _) -> runStorage @Source (summaryContent newest "story-so-far.md")
              _                  -> return Nothing

            -- The first Summary tick's own recorded altHead must still
            -- resolve to what it pointed at when written -- the newer
            -- tick supersedes it as *the* live one, but nothing about the
            -- earlier commit was rewritten or dropped.
            oldContent <- case mtid1 of
              Nothing   -> return Nothing
              Just tid1 -> do
                tick1 <- runStorage @Source (Tick.readTypesTick (Core.ObjectHash (unTickId tid1)))
                case fromTick @Summary tick1 of
                  Nothing       -> return Nothing
                  Just summary1 -> runStorage @Source (summaryContent summary1 "story-so-far.md")

            return (mtid1, mtid2, liveContent, oldContent)
      case result of
        Left err -> expectationFailure err
        Right (mtid1, mtid2, liveContent, oldContent) -> do
          mtid1 `shouldNotBe` Nothing
          mtid2 `shouldNotBe` Nothing
          liveContent `shouldBe` Just "summary v2"
          oldContent `shouldBe` Just "summary v1"

  describe "ticksSinceLastSummary" $ do
    it "is every tick since root (root's own tick included) when nothing has been summarized yet" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "one"))
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "two"))
            runStorage @Source (ticksSinceLastSummary "prose/chapter")
      case result of
        Left err -> expectationFailure err
        Right ticks -> length ticks `shouldBe` 3 -- root + "one" + "two"

    it "only covers what's new since the last summary of that kind" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "one"))
            _ <- runSummarizer @Source "prose/chapter" (generateFixed "s1")
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "two"))
            runStorage @Source (ticksSinceLastSummary "prose/chapter")
      case result of
        Left err -> expectationFailure err
        Right ticks -> length ticks `shouldBe` 1

  describe "summaryTickFor" $ do
    it "finds the exact Summary tick that produced a batch's alt-chain commit, covering every file the batch wrote" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "narrative"))
            mtid <- runSummarizer @Source "prose/chapter"
              (generateTwoFiles "summary of a" "summary of b")
            case mtid of
              Nothing  -> return (mtid, Nothing)
              Just tid -> do
                tick <- runStorage @Source (Tick.readTypesTick (Core.ObjectHash (unTickId tid)))
                case fromTick @Summary tick of
                  Nothing -> return (mtid, Nothing)
                  Just s  -> do
                    let altHash = Core.ObjectHash (unTickId (summaryAltHead s))
                    found <- runStorage @Source (summaryTickFor altHash)
                    return (mtid, fst <$> found)
      case result of
        Left err -> expectationFailure err
        Right (mtid, foundId) -> do
          mtid `shouldNotBe` Nothing
          foundId `shouldBe` mtid

    it "returns Nothing for a commit hash no Summary tick points at" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Core.store (Core.NonAtom [] "narrative"))
            _ <- runSummarizer @Source "prose/chapter" (generateFixed "s1")
            runStorage @Source (summaryTickFor (Core.ObjectHash "not-a-real-alt-commit"))
      result `shouldBe` Right Nothing

  describe "summariesTouching" $ do
    it "finds one occurrence after a single real runSummarizerForPath pass" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Ops.addAtom "a.md" "para one. ")
            _ <- runStorage @Source (Ops.addAtom "a.md" "para two.")
            _ <- runSummarizerForPath @Source "prose/chapter" "a.md" (\_ -> pure "summary of a")
            runStorage @Source (summariesTouching "prose/chapter" "a.md")
      case result of
        Left err -> expectationFailure err
        Right occs -> case occs of
          [occ] -> do
            summaryKind (occSummary occ) `shouldBe` "prose/chapter"
            occLowerBound occ `shouldBe` Nothing
            occPrevAltHead occ `shouldBe` Nothing
          _ -> expectationFailure ("expected exactly one occurrence, got " <> show (length occs))

    it "shared kind across two files: each file's own call returns only its own occurrence" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Ops.addAtom "a.md" "a content")
            _ <- runStorage @Source (Ops.addAtom "b.md" "b content")
            _ <- runSummarizerForPath @Source "prose/chapter" "a.md" (\_ -> pure "summary of a")
            _ <- runSummarizerForPath @Source "prose/chapter" "b.md" (\_ -> pure "summary of b")
            occsA <- runStorage @Source (summariesTouching "prose/chapter" "a.md")
            occsB <- runStorage @Source (summariesTouching "prose/chapter" "b.md")
            return (occsA, occsB)
      case result of
        Left err -> expectationFailure err
        Right (occsA, occsB) -> do
          length occsA `shouldBe` 1
          length occsB `shouldBe` 1

    it "two sequential passes of the same kind on the same file both appear, oldest-first, each anchored to the atom present at that pass" $ do
      let result = runOne $ do
            h1  <- runStorage @Source (Ops.addAtom "a.md" "para one.")
            _   <- runSummarizerForPath @Source "prose/chapter" "a.md" (\_ -> pure "summary v1")
            h2  <- runStorage @Source (Ops.addAtom "a.md" "para two.")
            _   <- runSummarizerForPath @Source "prose/chapter" "a.md" (\_ -> pure "summary v2")
            occs <- runStorage @Source (summariesTouching "prose/chapter" "a.md")
            return (TickId (Core.unObjectHash h1), TickId (Core.unObjectHash h2), occs)
      case result of
        Left err -> expectationFailure err
        Right (tid1, tid2, occs) -> case occs of
          [occ1, occ2] -> do
            summaryKind (occSummary occ1) `shouldBe` "prose/chapter"
            summaryKind (occSummary occ2) `shouldBe` "prose/chapter"
            occAnchor occ1 `shouldBe` tid1
            occAnchor occ2 `shouldBe` tid2
            -- The first occurrence has no older occurrence to bound it;
            -- the second's own lower bound is exactly the first's anchor
            -- -- handed back directly, not something a client re-derives.
            -- Same for the alt-chain's own boundary (occPrevAltHead): the
            -- second pass's own delta starts right where the first pass's
            -- own alt-chain commit left off.
            occLowerBound occ1 `shouldBe` Nothing
            occLowerBound occ2 `shouldBe` Just tid1
            occPrevAltHead occ1 `shouldBe` Nothing
            occPrevAltHead occ2 `shouldBe` Just (summaryAltHead (occSummary occ1))
          _ -> expectationFailure ("expected exactly two occurrences, got " <> show (length occs))

    it "excludes a pure carry-forward pass that never actually touched path" $ do
      let result = runOne $ do
            _ <- runStorage @Source (Ops.addAtom "a.md" "a content")
            _ <- runStorage @Source (Ops.addAtom "b.md" "b content")
            _ <- runSummarizerForPath @Source "prose/chapter" "a.md" (\_ -> pure "summary of a")
            -- A second same-kind pass touching only b.md -- a.md's own
            -- compression is carried forward unchanged, so this must not
            -- show up as a second occurrence for a.md.
            _ <- runSummarizerForPath @Source "prose/chapter" "b.md" (\_ -> pure "summary of b")
            runStorage @Source (summariesTouching "prose/chapter" "a.md")
      case result of
        Left err -> expectationFailure err
        Right occs -> length occs `shouldBe` 1
