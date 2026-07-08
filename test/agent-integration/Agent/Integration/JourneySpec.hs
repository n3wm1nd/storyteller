{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does a whole outline -> beat sheets -> chapters session actually work,
--   end to end, the way a Writer-tab user would run it? 'Agent.Integration.Journey.runJourney'
--   does the driving; this spec is just the assertions on top of it --
--   structural (every step produced the files the next step needs, in the
--   conventional WRITER.md layout) plus one qualitative spot check (the
--   first chapter's prose actually realizes its own beat sheet, not some
--   other one) via 'Agent.Integration.Judge.judge'.
--
--   Three real 'Storyteller.Core.Runtime.StoryModel'-shaped calls at
--   minimum (outline, split, one chapter per emitted beat sheet), all
--   cached under test/fixtures/llm-agent-cache/agent/ like every other spec
--   in this suite.
module Agent.Integration.JourneySpec (spec) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Journey (JourneyResult(..), runJourney)
import Agent.Integration.Judge (Verdict(..), judge)
import Storyteller.Writer.Agent.Outline (BeatSheet(..), ChapterBeats(..))

spec
  :: forall storyModel judgeModel
  .  ( HasTools storyModel, SupportsSystemPrompt (ProviderOf storyModel)
     , HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel) )
  => Runner storyModel judgeModel -> Spec
spec runner = describe "a full outline -> beat sheets -> chapters session (real LLM, cached)" $
  it "produces a coherent chapter-by-chapter draft from a one-line pitch" $
    runExpect @storyModel @judgeModel runner $ do
      result <- runJourney @storyModel []
      info $ "journey outline:\n" <> jrOutline result
      embed $ do
        jrOutline result `shouldNotBe` ""

        -- Reasonably short: it's a chapter-heading-plus-a-few-sentences
        -- planning document, not prose -- 'storyPremise' asks for one
        -- heading per chapter with a few sentences under each, so even at
        -- eight chapters this should stay well under a single chapter's
        -- own word budget (below).
        wordCount (jrOutline result) `shouldSatisfy` (<= 900)

        -- Asked for five chapters; models don't always land on the exact
        -- count, but wildly off means the split step ignored the outline
        -- rather than dividing it.
        length (jrChapters result) `shouldSatisfy` \n -> n >= 3 && n <= 8

        -- Every beat sheet landed at its declared, conventional path, with
        -- actual content -- not a call the model "made" without a body.
        mapM_ (\(ChapterBeats path (BeatSheet sheet)) -> do
                 path `shouldSatisfy` ("chapters/" `isPrefixOf`)
                 sheet `shouldNotBe` "") (jrChapters result)

        -- One chapter of prose per beat sheet, each with content distinct
        -- from its own outline -- catches a chapter step that just echoed
        -- the beat sheet back instead of writing prose from it. Length is
        -- checked against 'writeAgent'\'s own length hint (WordCount 300,
        -- via 'Storyteller.Writer.Agent.Write.writeAgent') with generous
        -- slack either side -- models routinely miss a word-count hint by
        -- 2-3x, so this is a sanity bound (catches a one-line non-answer or
        -- a runaway multi-chapter wall of text), not a precision check.
        length (jrProse result) `shouldBe` length (jrChapters result)
        mapM_ (\((_, BeatSheet sheet), (_, prose)) -> do
                 prose `shouldNotBe` ""
                 prose `shouldNotBe` sheet
                 wordCount prose `shouldSatisfy` \n -> n >= 100 && n <= 1200)
              (zip (map (\cb -> (cbPath cb, cbSheet cb)) (jrChapters result)) (jrProse result))

        -- Every beat sheet and its chapter file both actually landed on
        -- disk -- reads 'jrFiles' (a plain filesystem listing) rather than
        -- trusting 'jrChapters'\/'jrProse', so this would catch e.g. a write
        -- silently landing at the wrong path even if the agents' own return
        -- values looked fine. 'jrProse'\'s paths are the actual chapter
        -- paths 'runJourney' wrote to, in the same order as 'jrChapters'.
        mapM_ (\(ChapterBeats sheetPath _, (chapterPath, _)) -> do
                 sheetPath   `shouldSatisfy` (`elem` jrFiles result)
                 chapterPath `shouldSatisfy` (`elem` jrFiles result))
              (zip (jrChapters result) (jrProse result))

      case (jrChapters result, jrProse result) of
        ((ChapterBeats _ (BeatSheet sheet1)) : _, (_, prose1) : _) -> do
          info ("chapter 1 prose:\n" <> prose1)
          -- Both texts go in the artifact, not the question -- see below for
          -- why that alone isn't enough.
          let artifact = T.unlines
                [ "Beat sheet:", "", sheet1, "", "---", "", "Chapter prose:", "", prose1 ]
          -- "Keep your reason to one sentence" is load-bearing, not
          -- style advice: with a beat sheet this long in the artifact, the
          -- judge model's first instinct is to itemize every beat in its
          -- 'reason' tool-call argument one by one, which reliably overran
          -- deepseek-v4-flash's 1024-token cap ('Agent.Integration.Harness.knownModels')
          -- mid-string and produced truncated, unparseable JSON on a live
          -- run. A short reason fits comfortably inside the cap.
          Verdict pass reason <- judge @judgeModel artifact $ T.unwords
            [ "Above is a chapter beat sheet, then the chapter prose written from"
            , "it. Does the prose realize the events, characters, and progression"
            , "described in the beat sheet (not just share a similar tone)? Answer"
            , "no if the prose follows a different sequence of events than the beat"
            , "sheet describes. Keep your reason to one sentence."
            ]
          info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
          embed $ pass `shouldBe` True
        _ -> embed $ expectationFailure "journey produced no chapters to check"

wordCount :: T.Text -> Int
wordCount = length . T.words
