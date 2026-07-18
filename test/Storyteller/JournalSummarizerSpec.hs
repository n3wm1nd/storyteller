{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 'Storyteller.Writer.Agent.JournalSummarizer.journalSummarize' takes
-- its compression step as a parameter, same "no agent's real 'queryLLM'
-- call is unit tested" convention as
-- 'Storyteller.Writer.Agent.Tasks.syncTasksWith' (see
-- @test.Storyteller.TasksSpec@'s own Haddock) -- a pure, deterministic
-- stub stands in here, so what's actually pinned is the recursive
-- walk\/chunk-boundary\/idempotency machinery this module owns:
--
--   * a chunk only forms once a full group of items exists, never early;
--   * the resulting tree is the same whether driven by one big call or
--     many small ones (the core idempotency claim);
--   * calling again with nothing new is a genuine no-op;
--   * a tier only gets its own attempt once the tier below it actually
--     wrote something.
module Storyteller.JournalSummarizerSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail

import Git.Mock (emptyGitState, runGitMock)
import Polysemy.State (evalState)
import Runix.Logging (loggingNull)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Common.Summary (lastSummaryOf, summaryContent)
import Storyteller.Writer.Agent.JournalSummarizer

data TestBranch

runOne action =
  run
  . runFail
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "branch")
      runBranchAndFS @TestBranch (BranchName "branch") action

-- | A deterministic stand-in for the real LLM call: joins its items with a
--   marker unlikely to appear in test fixtures, so a test can tell exactly
--   which raw (or child-tier) items landed in which group just by reading
--   the written content back.
stubCompress :: [Text] -> Sem r Text
stubCompress items = pure ("C[" <> T.intercalate "," items <> "]")

addEntries :: Member (BranchOp TestBranch) r => [Text] -> Sem r ()
addEntries = mapM_ (\e -> runStorage @TestBranch (Ops.addAtom "journal.md" e))

tierZeroContent :: Member (BranchOp TestBranch) r => Sem r (Maybe Text)
tierZeroContent = do
  mSum <- runStorage @TestBranch (lastSummaryOf (journalKindFor 0))
  case mSum of
    Nothing     -> return Nothing
    Just (_, s) -> runStorage @TestBranch (summaryContent s "journal.md")

tierOneContent :: Member (BranchOp TestBranch) r => Sem r (Maybe Text)
tierOneContent = do
  mSum <- runStorage @TestBranch (lastSummaryOf (journalKindFor 1))
  case mSum of
    Nothing     -> return Nothing
    Just (_, s) -> runStorage @TestBranch (summaryContent s "journal.md")

spec :: Spec
spec = do
  describe "journalGrowth" $ do
    it "is the trailing append when seen is a genuine prefix" $
      journalGrowth "C[a,b]" "C[a,b]C[c,d]" `shouldBe` "C[c,d]"

    it "is the whole current content when nothing has been seen yet" $
      journalGrowth "" "C[a,b]" `shouldBe` "C[a,b]"

    it "falls back to the whole current content if seen isn't actually a prefix" $
      journalGrowth "mismatch" "C[a,b]" `shouldBe` "C[a,b]"

  describe "journalSummarize" $ do
    it "writes nothing when there are fewer than a full group's worth of entries" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize - 1 :: Int])
            wrote <- journalSummarize @TestBranch stubCompress 0
            content <- tierZeroContent
            return (wrote, content)
      result `shouldBe` Right (False, Nothing)

    it "writes exactly one chunk once a full group accumulates, from raw entries in order" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize :: Int])
            wrote <- journalSummarize @TestBranch stubCompress 0
            content <- tierZeroContent
            return (wrote, content)
      result `shouldBe` Right (True, Just "C[1,2,3,4,5,6,7,8,9,10]")

    it "a second call with nothing new is a genuine no-op" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize :: Int])
            _      <- journalSummarize @TestBranch stubCompress 0
            wrote2 <- journalSummarize @TestBranch stubCompress 0
            content <- tierZeroContent
            return (wrote2, content)
      result `shouldBe` Right (False, Just "C[1,2,3,4,5,6,7,8,9,10]")

    it "one big backlog produces the same tree as many small incremental calls (idempotency)" $ do
      let n = defaultJournalGroupSize
          bulkResult = runOne $ do
            addEntries (map (T.pack . show) [1 .. 2 * n :: Int])
            _ <- journalSummarize @TestBranch stubCompress 0
            tierZeroContent
          incrementalResult = runOne $ do
            mapM_ (\i -> addEntries [T.pack (show i)] >> journalSummarize @TestBranch stubCompress 0) [1 .. 2 * n :: Int]
            tierZeroContent
      bulkResult `shouldBe` incrementalResult
      bulkResult `shouldBe` Right (Just "C[1,2,3,4,5,6,7,8,9,10]C[11,12,13,14,15,16,17,18,19,20]")

    it "a leftover under one group's size survives to the next call untouched" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n + 3 :: Int])
            _  <- journalSummarize @TestBranch stubCompress 0
            -- Only one full group (1..10) should have been consumed; the
            -- trailing 11,12,13 must still show up as candidates now.
            addEntries (map (T.pack . show) [14 .. n + 10 :: Int])
            _ <- journalSummarize @TestBranch stubCompress 0
            tierZeroContent
      result `shouldBe` Right (Just "C[1,2,3,4,5,6,7,8,9,10]C[11,12,13,14,15,16,17,18,19,20]")

    it "tier 1 only gets a chunk once tier 0 has produced a full group's worth of its own chunks" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. (n * n) - 1 :: Int]) -- one raw entry short of a full tier-1 group
            _ <- journalSummarize @TestBranch stubCompress 0
            tierOneContent
      result `shouldBe` Right Nothing

    it "tier 1 forms once enough raw entries exist for a full group of tier-0 chunks, folding each chunk's own growth" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n * n :: Int])
            _ <- journalSummarize @TestBranch stubCompress 0
            tierOneContent
      case result of
        Left err -> expectationFailure err
        Right mContent -> do
          mContent `shouldSatisfy` maybe False (const True)
          let Just content = mContent
          -- Ten tier-0 chunks folded into one tier-1 chunk -- the stub
          -- compress joins its ten inputs with commas, each input itself
          -- one tier-0 chunk's own "C[...]" text.
          T.isPrefixOf "C[C[1,2,3,4,5,6,7,8,9,10]," content `shouldBe` True
