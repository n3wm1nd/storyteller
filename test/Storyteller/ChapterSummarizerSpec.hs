{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | 'Storyteller.Writer.Agent.ChapterSummarizer.unitSummaryCandidates' is
-- the pure half of the @"prose/chapter"@ summarizer (per the project's
-- "extract pure before wiring" convention -- the LLM-calling half,
-- 'chapterSummaryGenerate'\/'chapterSummaryAgent', isn't unit-tested here,
-- same as no other agent's real 'queryLLM' call is). Pins:
--
--   * only 'Storyteller.Writer.Library.Unit'-classified paths are picked up,
--     not every atom-touched path on the chain (unlike
--     'Server.Writer.Branch.passthroughGenerate');
--   * multiple atoms on the same path concatenate, oldest-first;
--   * non-atom ticks are ignored.
module Storyteller.ChapterSummarizerSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Types
import Storyteller.Writer.Agent.ChapterSummarizer (unitSummaryCandidates)

-- | A minimal, otherwise-unused chain position -- 'unitSummaryCandidates'
-- never looks at 'tickPos', only 'tickData', so every fixture tick here
-- shares one placeholder position.
pos :: TickPos
pos = TickPos { posId = TickId "t", posParent = Nothing, posRefs = [] }

atomTick :: FilePath -> Text -> Tick
atomTick path msg = Tick pos (toDraft @Atom (Atom path msg))

noteTick :: Text -> Tick
noteTick msg = Tick pos (draft msg)

spec :: Spec
spec = describe "unitSummaryCandidates" $ do
  it "picks up an atom on a recognized chapter path" $
    unitSummaryCandidates [atomTick "chapters/ch1.md" "hello"]
      `shouldBe` Map.fromList [("chapters/ch1.md", "hello")]

  it "ignores an atom on a path with no marker word" $
    unitSummaryCandidates [atomTick "notes.md" "hello"] `shouldBe` Map.empty

  it "ignores non-atom ticks entirely" $
    unitSummaryCandidates [noteTick "just a note"] `shouldBe` Map.empty

  it "concatenates multiple atoms on the same path, oldest first" $
    unitSummaryCandidates [atomTick "chapters/ch1.md" "one ", atomTick "chapters/ch1.md" "two"]
      `shouldBe` Map.fromList [("chapters/ch1.md", "one two")]

  it "keeps separate chapters separate, and drops unrelated files in between" $
    unitSummaryCandidates
      [ atomTick "chapters/ch1.md" "a"
      , atomTick "notes.md" "irrelevant"
      , atomTick "chapters/ch2.md" "b"
      ]
      `shouldBe` Map.fromList [("chapters/ch1.md", "a"), ("chapters/ch2.md", "b")]
