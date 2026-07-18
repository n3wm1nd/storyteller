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

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Common.Summary (Summary(..), lastSummaryOf, summaryContent, summariesTouching)
import Storyteller.Writer.Library (journalPath)
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
  mSum <- runStorage @TestBranch (lastSummaryOf journalKind)
  case mSum of
    Nothing     -> return Nothing
    Just (_, s) -> runStorage @TestBranch (summaryContent s "journal.md")

-- | Tier 1 no longer lives on the same (real) branch under a different
--   kind suffix -- it lives one alternate chain deeper, on tier 0's own
--   alt-chain, under the exact same 'journalKind'. Finding it means
--   descending into tier 0's own 'summaryAltHead' first (via
--   'Storage.Core.readAt', a read-only jump valid for any commit this
--   store can read -- see its own Haddock) and asking the identical
--   question again from there.
tierOneContent :: Member (BranchOp TestBranch) r => Sem r (Maybe Text)
tierOneContent = do
  mTierZero <- runStorage @TestBranch (lastSummaryOf journalKind)
  case mTierZero of
    Nothing     -> return Nothing
    Just (_, s0) -> runStorage @TestBranch $
      Core.readAt (Core.ObjectHash (unTickId (summaryAltHead s0))) $ do
        mTierOne <- lastSummaryOf journalKind
        case mTierOne of
          Nothing      -> return Nothing
          Just (_, s1) -> summaryContent s1 "journal.md"

spec :: Spec
spec = do
  describe "journalSummarize" $ do
    it "writes nothing when there are fewer than a full group's worth of entries" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize - 1 :: Int])
            wrote <- journalSummarize @TestBranch stubCompress
            content <- tierZeroContent
            return (wrote, content)
      result `shouldBe` Right (False, Nothing)

    it "writes exactly one chunk once a full group accumulates, from raw entries in order" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize :: Int])
            wrote <- journalSummarize @TestBranch stubCompress
            content <- tierZeroContent
            return (wrote, content)
      result `shouldBe` Right (True, Just "C[1,2,3,4,5,6,7,8,9,10]")

    it "a second call with nothing new is a genuine no-op" $ do
      let result = runOne $ do
            addEntries (map (T.pack . show) [1 .. defaultJournalGroupSize :: Int])
            _      <- journalSummarize @TestBranch stubCompress
            wrote2 <- journalSummarize @TestBranch stubCompress
            content <- tierZeroContent
            return (wrote2, content)
      result `shouldBe` Right (False, Just "C[1,2,3,4,5,6,7,8,9,10]")

    it "one big backlog produces the same tree as many small incremental calls (idempotency)" $ do
      let n = defaultJournalGroupSize
          bulkResult = runOne $ do
            addEntries (map (T.pack . show) [1 .. 2 * n :: Int])
            _ <- journalSummarize @TestBranch stubCompress
            tierZeroContent
          incrementalResult = runOne $ do
            mapM_ (\i -> addEntries [T.pack (show i)] >> journalSummarize @TestBranch stubCompress) [1 .. 2 * n :: Int]
            tierZeroContent
      bulkResult `shouldBe` incrementalResult
      bulkResult `shouldBe` Right (Just "C[1,2,3,4,5,6,7,8,9,10]C[11,12,13,14,15,16,17,18,19,20]")

    it "a leftover under one group's size survives to the next call untouched" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n + 3 :: Int])
            _  <- journalSummarize @TestBranch stubCompress
            -- Only one full group (1..10) should have been consumed; the
            -- trailing 11,12,13 must still show up as candidates now.
            addEntries (map (T.pack . show) [14 .. n + 10 :: Int])
            _ <- journalSummarize @TestBranch stubCompress
            tierZeroContent
      result `shouldBe` Right (Just "C[1,2,3,4,5,6,7,8,9,10]C[11,12,13,14,15,16,17,18,19,20]")

    it "tier 1 only gets a chunk once tier 0 has produced a full group's worth of its own chunks" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. (n * n) - 1 :: Int]) -- one raw entry short of a full tier-1 group
            _ <- journalSummarize @TestBranch stubCompress
            tierOneContent
      result `shouldBe` Right Nothing

    it "tier 1 forms once enough raw entries exist for a full group of tier-0 chunks, folding each chunk's own growth" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n * n :: Int])
            _ <- journalSummarize @TestBranch stubCompress
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

    it "pins the structural claim: tier 1 lives one alternate chain deeper than tier 0, not on the real branch under a different kind" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n * n :: Int])
            _      <- journalSummarize @TestBranch stubCompress
            zero   <- tierZeroContent
            one    <- tierOneContent
            -- The real branch's own most recent "journal" tick is still
            -- tier 0's: its content is a flat run of tier-0 chunks, never
            -- itself wrapping another "C[...]" -- if tier 1 had instead
            -- been minted back onto the real branch under this same kind,
            -- this would be tier 1's own (nested-looking) content instead.
            return (zero, one)
      case result of
        Left err -> expectationFailure err
        Right (mZero, mOne) -> do
          mZero `shouldSatisfy` maybe False (T.isPrefixOf "C[1,2,3,4,5,6,7,8,9,10]C[11")
          mOne  `shouldSatisfy` maybe False (T.isPrefixOf "C[C[1,2,3,4,5,6,7,8,9,10],")

    it "summariesTouching finds a nested tier-1 occurrence once tier 1 has formed, by walking the currently-open chain -- no materialization needed" $ do
      let n = defaultJournalGroupSize
          result = runOne $ do
            addEntries (map (T.pack . show) [1 .. n * n :: Int])
            _ <- journalSummarize @TestBranch stubCompress
            mTierZero <- runStorage @TestBranch (lastSummaryOf "journal")
            case mTierZero of
              Nothing      -> return Nothing
              Just (_, s0) -> runStorage @TestBranch $
                -- Descending into tier 0's own alt-chain head is exactly
                -- how a client's nested connection (openTarget's own
                -- "#tid" hop) reaches a deeper tier -- calling
                -- 'summariesTouching' again from there is the whole
                -- nesting story, no separate tree-walking machinery.
                Core.readAt (Core.ObjectHash (unTickId (summaryAltHead s0))) $
                  Just <$> summariesTouching "journal" journalPath
      case result of
        Left err -> expectationFailure err
        Right mOccs -> case mOccs of
          Nothing   -> expectationFailure "expected tier 0 to exist"
          Just occs -> length occs `shouldBe` 1
