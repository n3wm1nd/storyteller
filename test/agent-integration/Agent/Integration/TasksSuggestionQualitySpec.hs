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
--   checks the passive-goals section actually lands as a conditional
--   "if X happens, character does/becomes Y" reaction to a possible scene
--   development, specific to this character, rather than a bare trait,
--   dislike, or feeling stated on its own -- the distinction
--   'Storyteller.Writer.Agent.Tasks.tasksFormatNote' was written to draw.
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

judgeQuestionPassiveShape :: T.Text
judgeQuestionPassiveShape = T.unwords
  [ "Look only at the \"## Passive goals\" section of this proposed"
  , "tasks.md. Does every entry in it describe a specific, possible scene"
  , "development and this particular character's own reaction to it (an"
  , "\"if X happens, character does/becomes Y\" shape, e.g. \"if Harun"
  , "notices the missing coppers, claim it was a mistake\") rather than a"
  , "bare personality trait, dislike, or feeling stated on its own with no"
  , "triggering situation attached (e.g. \"is anxious\" or \"dislikes"
  , "crowds\" would fail this)? Answer no if the Passive goals section is"
  , "empty, or if any entry is just a trait/feeling with no conditional"
  , "situation attached."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "tasksGenerateAgent (real LLM, cached)" $
  it "proposes tasks concretely grounded in a real journal, not generic mood" $
    runExpect @judgeModel runner $ do
      content <- tasksGenerateAgent "Doran" "" doranJournal
      info ("tasksGenerateAgent output:\n" <> content)
      embed $ do
        content `shouldNotBe` ""
        content `shouldSatisfy` T.isInfixOf "## Short-term goals"
        content `shouldSatisfy` T.isInfixOf "## Long-term goals"
        content `shouldSatisfy` T.isInfixOf "## Passive goals"

      Verdict concrete concreteReason <- judge @judgeModel content judgeQuestionConcreteness
      info ("concreteness verdict: " <> T.pack (show concrete) <> " -- " <> concreteReason)
      embed $ concrete `shouldBe` True

      Verdict shaped shapedReason <- judge @judgeModel content judgeQuestionPassiveShape
      info ("passive-goal-shape verdict: " <> T.pack (show shaped) <> " -- " <> shapedReason)
      embed $ shaped `shouldBe` True
