{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.SummaryAccessSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy (run)
import Polysemy.Fail (runFail)
import Polysemy.State (evalState)

import Git.Mock (emptyGitState, runGitMock)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchAndFS, runStorage, runStoryStorageGit)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent.Summarizer (runSummarizer)
import Storyteller.Writer.Agent.SummaryAccess (densest, densestWithin)

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
