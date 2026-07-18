{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | 'Storyteller.Writer.Agent.LoreSummarizer.loreSummaryCandidates' is the
-- pure half of the @"lore/article"@ summarizer -- same "extract pure
-- before wiring" convention 'Storyteller.ChapterSummarizerSpec' already
-- pins for @"prose/chapter"@. Pins:
--
--   * only 'Storyteller.Writer.Lore.isLoreEligible' paths are picked up;
--   * @sheet.md@\/@journal.md@ and @chat/*@ (excluded by
--     'isLoreEligible', not just a Unit-classified path) are ignored, same
--     as any other non-eligible path;
--   * multiple atoms on the same path concatenate, oldest-first;
--   * non-atom ticks are ignored.
module Storyteller.LoreSummarizerSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Types
import Storyteller.Writer.Agent.LoreSummarizer (loreSummaryCandidates)

pos :: TickPos
pos = TickPos { posId = TickId "t", posParent = Nothing, posRefs = [] }

atomTick :: FilePath -> Text -> Tick
atomTick path msg = Tick pos (toDraft @Atom (Atom path msg))

noteTick :: Text -> Tick
noteTick msg = Tick pos (draft msg)

spec :: Spec
spec = describe "loreSummaryCandidates" $ do
  it "picks up an atom on an eligible lore path" $
    loreSummaryCandidates [atomTick "world/city.md" "hello"]
      `shouldBe` Map.fromList [("world/city.md", "hello")]

  it "ignores a chapter path (recognized prose, not lore)" $
    loreSummaryCandidates [atomTick "chapters/ch1.md" "hello"] `shouldBe` Map.empty

  it "ignores sheet.md and journal.md" $
    loreSummaryCandidates [atomTick "sheet.md" "a", atomTick "journal.md" "b"] `shouldBe` Map.empty

  it "ignores chat scratch space" $
    loreSummaryCandidates [atomTick "chat/scratch.md" "hello"] `shouldBe` Map.empty

  it "ignores non-atom ticks entirely" $
    loreSummaryCandidates [noteTick "just a note"] `shouldBe` Map.empty

  it "concatenates multiple atoms on the same path, oldest first" $
    loreSummaryCandidates [atomTick "world/city.md" "one ", atomTick "world/city.md" "two"]
      `shouldBe` Map.fromList [("world/city.md", "one two")]

  it "keeps separate articles separate, and drops unrelated files in between" $
    loreSummaryCandidates
      [ atomTick "world/city.md" "a"
      , atomTick "chapters/ch1.md" "irrelevant"
      , atomTick "world/king.md" "b"
      ]
      `shouldBe` Map.fromList [("world/city.md", "a"), ("world/king.md", "b")]
