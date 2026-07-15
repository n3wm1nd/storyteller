{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The second of the two questions the tasks.md feature exists to answer
--   (see 'Storyteller.Writer.Agent.Tasks'): given a real journal, can
--   'Storyteller.Writer.Agent.Tasks.tasksGenerateAgent' actually propose
--   something worthwhile -- concrete, specific to *this* character's
--   demonstrated personality and behavior -- or does it default to the
--   generic, could-apply-to-any-character mood LLM-generated stories tend
--   toward ("find happiness", "grow as a person")?
--
--   The journal fixture is written to make concreteness checkable: three
--   specific, nameable threads (a recurring visit to a specific bridge, a
--   quiet pattern of petty theft, a flinch at a specific trigger) rather
--   than one diffuse arc -- a generic suggestion has nothing to snag on;
--   a grounded one should visibly pick at least one of them up. Also
--   checks the aversions section actually lands as an anti-goal (a
--   specific outcome steered away from) rather than a bare trait/dislike --
--   the distinction the module's system prompt was written to draw.
--
--   Direct 'tasksGenerateAgent' call, no branch/storage needed -- same
--   "call the LLM function directly" shape
--   'Agent.Integration.OutlineSplitQualitySpec' uses for
--   'Storyteller.Writer.Agent.Outline.splitOutlineAgent'. Real LLM call,
--   cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.TasksSuggestionQualitySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import Storyteller.Writer.Agent.Tasks (tasksGenerateAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

-- | Doran's journal: three separate, specific, nameable threads a generic
--   suggestion has no reason to land on by chance. No goals or aversions
--   are stated outright anywhere in it -- everything here is behavior for
--   the agent to actually read and infer from, not restate.
doranJournal :: T.Text
doranJournal = T.unlines
  [ "Walked the long way home again, over the Ashford bridge. Third time this"
  , "week. I keep telling myself it's just the shorter route to the tannery,"
  , "but the tannery road is shorter. I stood there a while, looking at the"
  , "water. Didn't used to be able to look at that water at all."
  , ""
  , "Took two coppers from the till again tonight. Not enough that Harun would"
  , "ever notice -- I made sure of that, same as always. I don't even spend"
  , "it. It's just sitting in the tin under my floorboard with the rest."
  , ""
  , "A watch patrol passed the tavern tonight, just walking their round,"
  , "nothing to do with me. My hands were shaking before I even registered"
  , "why. Had to go stand in the back until they'd gone. Nobody noticed. I"
  , "made sure of that too."
  ]

judgeQuestionConcreteness :: T.Text
judgeQuestionConcreteness = T.unwords
  [ "This is a proposed tasks.md for a character named Doran, generated from"
  , "his journal. His journal shows, without ever stating it outright: he"
  , "keeps returning to a specific bridge (the Ashford bridge) where"
  , "something clearly happened to him involving water; he's been quietly"
  , "and deliberately stealing small amounts of money without spending it;"
  , "and he has a strong physical fear reaction specifically to the City"
  , "Watch. Does the proposed tasks.md pick up on at least one of these"
  , "specific, concrete threads by name or clear reference (the bridge, the"
  , "hoarded stolen money, the fear of the Watch) rather than only offering"
  , "generic goals that could apply to any troubled character regardless of"
  , "what's actually in this journal (e.g. \"find inner peace\", \"become a"
  , "better person\", \"overcome his past\") with no concrete tie to what the"
  , "journal actually shows?"
  ]

judgeQuestionAversionShape :: T.Text
judgeQuestionAversionShape = T.unwords
  [ "Look only at the \"## Aversions\" section of this proposed tasks.md."
  , "Does every entry in it describe a specific outcome or event the"
  , "character is steering away from (an anti-goal -- something that could"
  , "actually happen in the story and be avoided or not) rather than a bare"
  , "personality trait, dislike, or feeling stated on its own (e.g. \"is"
  , "anxious\" or \"dislikes crowds\" would fail this, but \"being caught and"
  , "losing the only trust Harun still has in him\" would pass)? Answer no if"
  , "the Aversions section is empty, or if any entry is just a trait/dislike"
  , "with no concrete outcome attached."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "tasksGenerateAgent (real LLM, cached)" $
  it "proposes tasks concretely grounded in a real journal, not generic mood" $
    runExpect @judgeModel runner $ do
      content <- tasksGenerateAgent "" doranJournal
      info ("tasksGenerateAgent output:\n" <> content)
      embed $ do
        content `shouldNotBe` ""
        content `shouldSatisfy` T.isInfixOf "## Short-term goals"
        content `shouldSatisfy` T.isInfixOf "## Long-term goals"
        content `shouldSatisfy` T.isInfixOf "## Aversions"

      Verdict concrete concreteReason <- judge @judgeModel content judgeQuestionConcreteness
      info ("concreteness verdict: " <> T.pack (show concrete) <> " -- " <> concreteReason)
      embed $ concrete `shouldBe` True

      Verdict shaped shapedReason <- judge @judgeModel content judgeQuestionAversionShape
      info ("aversion-shape verdict: " <> T.pack (show shaped) <> " -- " <> shapedReason)
      embed $ shaped `shouldBe` True
