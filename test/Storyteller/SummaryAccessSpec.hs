{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.SummaryAccessSpec (spec) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (evalState)
import Runix.Git (Git)

import Git.Mock (emptyGitState, runGitMock)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runStorage, runStoryStorageGit)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..), Tick)
import Storyteller.Common.Summary (lastSummaryOf)
import Storyteller.Writer.Agent.ChapterSummarizer (unitSummaryCandidates)
import Storyteller.Writer.Agent.Summarizer (runSummarizer, runSummarizerForPath)
import Storyteller.Writer.Agent.SummaryAccess (densest, densestWithin, rawContent)

data Source

runOne action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "book1/chapter3")
      runBranchAndFS @Source (BranchName "book1/chapter3") action

spec :: Spec
spec = do
  densestSpec
  staleTickSpec
  densestWithinSpec
  freshnessContractSpec
  perPathSpec

-- | The real per-domain shape ('Storyteller.Writer.Agent.ChapterSummarizer.
-- chapterSummaryGenerate's own two steps: 'unitSummaryCandidates' decides
-- *which* paths changed, then each one's *current, full* content is read
-- fresh via 'rawContent' -- never the candidate ticks' own delta text,
-- which is only ever a trigger check, not what gets fed to a real model
-- (see that function's own Haddock for why: a summary has to be a pure
-- function of current content, not of how much changed since last time).
-- Stubbed only at the actual LLM call, same "no agent's real 'queryLLM'
-- is unit tested" convention as every other summarizer spec -- this is
-- the one place the real two-step shape's *consequences across a
-- multi-file batch* get pinned end-to-end, not just
-- 'unitSummaryCandidates'own pure output shape (see
-- 'Storyteller.ChapterSummarizerSpec' for that).
stubGenerate :: Members '[BranchOp Source, Git, StoryStorage, Fail] r => [Tick] -> Sem r (Map.Map FilePath T.Text)
stubGenerate candidates =
  Map.fromList <$> mapM summarizeOne (Map.keys (unitSummaryCandidates candidates))
  where
    summarizeOne path = do
      content <- fromMaybe "" <$> rawContent @Source path
      return (path, "compressed:" <> content)

-- | The three states 'summarize' is meant to leave any one file in,
-- pinned together so they read as one contract rather than three
-- unrelated tests: (a) never summarized -> gets a first summary; (b)
-- stale (new raw content since its own last pass) -> gets a fresh one,
-- appended, never amended; (c) already covers everything -> genuinely a
-- no-op, and -- the part a naive per-file trigger would get wrong --
-- running the *kind* against a batch that only actually stale-ifies
-- *one* file leaves every other file's own existing summary completely
-- untouched, not needlessly regenerated just because the kind ran.
freshnessContractSpec :: Spec
freshnessContractSpec = describe "summarize freshness contract (across a multi-file batch)" $ do
  it "(a) a never-summarized file gets its first summary the moment its kind runs" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "raw one.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate
          densest @Source ["prose/chapter"] "chapters/ch1.md"
    result `shouldBe` Right "compressed:raw one."

  it "(c) a file with nothing new since its own last pass is left genuinely untouched by a later batch" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "ch1 raw.")
          _ <- runStorage @Source (Ops.addAtom "chapters/ch2.md" "ch2 raw.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate
          Just (tickAfterFirstPass, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          -- Only ch1 changes; ch2 has nothing new relative to the pass
          -- that already covered it.
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "\n\nch1 raw v2.")
          wrote <- runSummarizer @Source "prose/chapter" stubGenerate
          ch2Content <- densest @Source ["prose/chapter"] "chapters/ch2.md"
          Just (tickAfterSecondPass, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          return (wrote, ch2Content, tickAfterFirstPass == tickAfterSecondPass)
    case result of
      Left err -> expectationFailure err
      Right (wrote, ch2Content, sameTick) -> do
        -- A second pass genuinely ran (ch1 was stale) -- a new Summary
        -- tick was minted, never amended in place (see
        -- 'Storyteller.Writer.Agent.Summarizer.runSummarizer's own
        -- Haddock) -- so the *kind*'s own most recent tick did advance...
        wrote `shouldSatisfy` (/= Nothing)
        sameTick `shouldBe` False
        -- ...but ch2's own content, read back through that newer tick,
        -- is exactly what the first pass produced -- never touched by a
        -- batch that had no new material for it at all.
        ch2Content `shouldBe` "compressed:ch2 raw."

  it "(b) a stale file (new raw content since its own last pass) is regenerated wholesale from current content" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "ch1 raw v1.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "\n\nch1 raw v2.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate
          densest @Source ["prose/chapter"] "chapters/ch1.md"
    -- Regenerated from the *whole* current content (both atoms), not
    -- folded onto the first pass's own output -- see
    -- 'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate's
    -- own Haddock for why a summary is always a pure function of current
    -- content, never a fold of a prior compression.
    result `shouldBe` Right "compressed:ch1 raw v1.\n\nch1 raw v2."

  it "a no-op call (nothing new for the whole kind) writes no new Summary tick at all" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapters/ch1.md" "raw one.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate
          Just (firstTick, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          wrote <- runSummarizer @Source "prose/chapter" stubGenerate
          Just (secondTick, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          return (wrote, firstTick == secondTick)
    result `shouldBe` Right (Nothing, True)

stubGenerateOne :: Applicative f => T.Text -> f T.Text
stubGenerateOne content = pure ("compressed:" <> content)

-- | 'runSummarizerForPath' -- summarizing exactly one file by hand,
-- without forcing every other stale file of the same kind through a
-- pass it was never asked for.
perPathSpec :: Spec
perPathSpec = describe "runSummarizerForPath" $ do
  it "(a) never summarized -> generates a first summary" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 raw.")
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          densest @Source ["prose/chapter"] "ch1.md"
    result `shouldBe` Right "compressed:ch1 raw."

  it "(b) stale -> regenerates wholesale from current full content" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 v1.")
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "\n\nch1 v2.")
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          densest @Source ["prose/chapter"] "ch1.md"
    result `shouldBe` Right "compressed:ch1 v1.\n\nch1 v2."

  it "(c) already up to date -> genuine no-op, no new tick" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 raw.")
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          Just (firstTick, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          wrote <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          Just (secondTick, _) <- runStorage @Source (lastSummaryOf "prose/chapter")
          return (wrote, firstTick == secondTick)
    result `shouldBe` Right (Nothing, True)

  it "summarizing one file never touches another stale file of the same kind" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 raw.")
          _ <- runStorage @Source (Ops.addAtom "ch2.md" "ch2 raw.")
          -- Both are equally never-summarized; only ch1 is asked for.
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          ch1 <- densest @Source ["prose/chapter"] "ch1.md"
          -- ch2 has no Summary tick at all yet -- 'densest' with no
          -- coverage at all just falls through to raw content.
          ch2 <- densest @Source ["prose/chapter"] "ch2.md"
          return (ch1, ch2)
    result `shouldBe` Right ("compressed:ch1 raw.", "ch2 raw.")

  it "positions the new tick at the file's own last atom, not current head -- a later batch still catches other files staled *before* that point but not yet processed" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 v1.")
          _ <- runStorage @Source (Ops.addAtom "ch2.md" "ch2 v1.")
          _ <- runSummarizer @Source "prose/chapter" stubGenerate  -- S1: first-ever pass, covers both
          -- ch1 goes stale, then (chronologically later) ch2 also goes
          -- stale -- but only ch1 is summarized by hand here.
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "\n\nch1 v2.")
          _ <- runStorage @Source (Ops.addAtom "ch2.md" "\n\nch2 v2.")
          _ <- runSummarizerForPath @Source "prose/chapter" "ch1.md" stubGenerateOne
          -- If the new tick had been minted at current head (after ch2's
          -- own later edit) instead of rebased to ch1's own last atom
          -- (before it), a later batch pass's "since last summary of
          -- this kind" boundary would start counting from *after* ch2's
          -- edit too -- silently treating it as already covered, even
          -- though nothing ever actually summarized it.
          wroteBatch <- runSummarizer @Source "prose/chapter" stubGenerate
          ch1 <- densest @Source ["prose/chapter"] "ch1.md"
          ch2 <- densest @Source ["prose/chapter"] "ch2.md"
          return (wroteBatch, ch1, ch2)
    case result of
      Left err -> expectationFailure err
      Right (wroteBatch, ch1, ch2) -> do
        wroteBatch `shouldSatisfy` (/= Nothing)
        -- ch1: untouched by the batch pass (already fresh from the
        -- file-scoped call moments earlier).
        ch1 `shouldBe` "compressed:ch1 v1.\n\nch1 v2."
        -- ch2: genuinely picked up and compressed by the batch pass --
        -- not left as raw, unsummarized content forever.
        ch2 `shouldBe` "compressed:ch2 v1.\n\nch2 v2."

densestSpec :: Spec
densestSpec = describe "densest" $ do
  it "is just the raw content when nothing has ever been summarized" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapter3.md" "para one.")
          densest @Source ["prose/chapter"] "chapter3.md"
    result `shouldBe` Right "para one."

  it "appends whatever's been written since the last summary onto the coarsest summary that covers it" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapter3.md" "para one.")
          _ <- runSummarizer @Source "prose/chapter"
            (\_ -> pure (Map.singleton "chapter3.md" "condensed para one."))
          -- Written *after* the summarizer ran -- not yet folded into
          -- the alternate chain's own content.
          _ <- runStorage @Source (Ops.addAtom "chapter3.md" "\n\npara two, not yet summarized.")
          densest @Source ["prose/chapter"] "chapter3.md"
    result `shouldBe` Right "condensed para one.\n\npara two, not yet summarized."

  it "returns just the summary with no appended tail when the summarizer already covers everything" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "chapter3.md" "para one.")
          _ <- runSummarizer @Source "prose/chapter"
            (\_ -> pure (Map.singleton "chapter3.md" "condensed para one."))
          densest @Source ["prose/chapter"] "chapter3.md"
    result `shouldBe` Right "condensed para one."

staleTickSpec :: Spec
staleTickSpec = describe "the per-file staleness boundary (not just the most recent pass of a kind)" $ do
  it "doesn't drop a gap when the most recent pass of a kind didn't touch this file at all" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 para one.")
          -- Pass 1: only ch1.md exists yet, so only it gets summarized.
          _ <- runSummarizer @Source "prose/chapter"
            (\_ -> pure (Map.singleton "ch1.md" "ch1 condensed v1"))
          _ <- runStorage @Source (Ops.addAtom "ch2.md" "ch2 para one.")
          -- Written *before* pass 2, but pass 2 (below) won't touch ch1.md at all.
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "\n\nch1 para two, added between passes.")
          -- Pass 2: only regenerates ch2.md -- ch1.md's entry just rides
          -- forward unchanged in the alternate chain's cumulative tree.
          _ <- runSummarizer @Source "prose/chapter"
            (\_ -> pure (Map.singleton "ch2.md" "ch2 condensed"))
          densest @Source ["prose/chapter"] "ch1.md"
    -- The naive "stop at the most recent pass of this kind" boundary
    -- would use pass 2 as the cutoff -- but pass 2 ran *after* "para two"
    -- was already written, so nothing would look new, and "para two"
    -- would be silently dropped entirely. The correct boundary is pass 1
    -- (the actual point ch1.md's compression is as fresh as), which
    -- correctly still counts "para two" as an unsummarized tail.
    result `shouldBe` Right "ch1 condensed v1\n\nch1 para two, added between passes."

  it "keeps each file's own staleness boundary correct when one pass summarizes several files together" $ do
    let result = runOne $ do
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "ch1 para one.")
          _ <- runStorage @Source (Ops.addAtom "ch2.md" "ch2 para one.")
          -- One pass touching both files: each lands in its own
          -- alternate-chain commit ('Storage.Ops.addAtom' per file, see
          -- 'Storyteller.Writer.Agent.Summarizer.replaceWithAtom'),
          -- chained together under the one 'Summary' tick this call
          -- records (which, alphabetically, ends up being ch2.md's own
          -- commit -- ch1.md's is the earlier, non-final one).
          _ <- runSummarizer @Source "prose/chapter"
            (\_ -> pure (Map.fromList [("ch1.md", "ch1 condensed"), ("ch2.md", "ch2 condensed")]))
          _ <- runStorage @Source (Ops.addAtom "ch1.md" "\n\nch1 para two, not yet summarized.")
          ch1 <- densest @Source ["prose/chapter"] "ch1.md"
          ch2 <- densest @Source ["prose/chapter"] "ch2.md"
          return (ch1, ch2)
    -- Before 'summaryTickFor' matched by reachability instead of exact
    -- hash equality, ch1.md's own (non-final) commit would never match
    -- any recorded 'summaryAltHead' at all -- 'unsummarizedTailSince'
    -- would then fall back to walking the *whole* source history instead
    -- of stopping right after the summarizer ran, duplicating "ch1 para
    -- one." ahead of the already-condensed text.
    result `shouldBe` Right ("ch1 condensed\n\nch1 para two, not yet summarized.", "ch2 condensed")

densestWithinSpec :: Spec
densestWithinSpec = describe "densestWithin (the cost-function generalization)" $ do
  let setUp = do
        _ <- runStorage @Source (Ops.addAtom "chapter3.md" "para one, quite long indeed.")
        runSummarizer @Source "prose/chapter"
          (\_ -> pure (Map.singleton "chapter3.md" "condensed."))

  it "const True picks the raw, unsummarized level -- the 'lightest' case" $ do
    let result = runOne $ do
          _ <- setUp
          densestWithin @Source ["prose/chapter"] (const True) "chapter3.md"
    result `shouldBe` Right ("para one, quite long indeed.", True)

  it "const False always falls through to the coarsest level -- same answer as densest" $ do
    let result = runOne $ do
          _ <- setUp
          densestWithin @Source ["prose/chapter"] (const False) "chapter3.md"
    result `shouldBe` Right ("condensed.", False)

  it "a real predicate (word count) picks the finest level that still satisfies it" $ do
    let wordCount = length . T.words
    let result = runOne $ do
          _ <- setUp
          densestWithin @Source ["prose/chapter"] ((<= 1) . wordCount) "chapter3.md"
    result `shouldBe` Right ("condensed.", True)
