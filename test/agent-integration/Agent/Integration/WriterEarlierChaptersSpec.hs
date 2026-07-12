{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does 'writeAgent''s @earlierChapters@ argument -- prior chapters' full
--   prose, placed as their own early messages (see its Haddock) -- actually
--   keep a *later* chapter consistent with something only an earlier one
--   established? 'Agent.Integration.JourneySpec' already exercises this
--   argument mechanically (it's how @runJourney@ threads chapters together)
--   but only checks that a chapter realizes its own beat sheet, never
--   whether it stays consistent with an earlier chapter's own established
--   fact -- this scenario isolates exactly that.
--
--   Same shape as 'Agent.Integration.CharContextWriteSpec' (a planted,
--   checkable fact; an instruction that never repeats it; a judge that asks
--   whether the reaction is consistent with it) but the fact lives in a
--   prior chapter's prose instead of a character sheet, so it's this
--   argument -- not @chars@ -- doing the work if the model gets it right.
--   A real LLM call, cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.WriterEarlierChaptersSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import Storyteller.Writer.Agent (Instruction(..), Prose(..))
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)

earlierChapter :: T.Text
earlierChapter = T.unwords
  [ "Elena's left forearm still bears the burn scar from the fire two"
  , "winters ago -- a wide, mottled patch of ruined skin from wrist to"
  , "elbow. She has worn long sleeves every day since, in every season,"
  , "and flinches from any conversation that gets near it. No one outside"
  , "her family has seen it in years, and she intends to keep it that way."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write the scene where the tailor holds up a sleeveless summer dress"
  , "and cheerfully tells Elena it would suit her perfectly, urging her to"
  , "try it on right there in the shop."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does Elena react to being offered the sleeveless dress with"
  , "hesitation, discomfort, or a polite refusal/deflection consistent with"
  , "not wanting to bare her arms -- rather than happily agreeing to try it"
  , "on? The text does not need to explain why or mention any scar"
  , "explicitly -- a consistent reluctance about the sleeveless cut alone is"
  , "enough to pass. Answer no if she agrees eagerly with no hesitation at"
  , "all."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "earlier-chapter continuity reaching the writer (real LLM, cached)" $
  it "keeps a new chapter consistent with a fact only an earlier chapter established" $
    runExpect @judgeModel runner $ do
      Prose text <- writeAgent [] [] [] [] [earlierChapter] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
