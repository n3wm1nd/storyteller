{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Closes the loop the other two tasks.md scenarios open
--   ('Agent.Integration.TasksSteeringSpec',
--   'Agent.Integration.TasksSuggestionQualitySpec'): once a goal is
--   resolved by what actually happens, does
--   'Storyteller.Writer.Agent.Tasks.tasksReconcileAgent' actually drop it,
--   while leaving unrelated tasks -- including an aversion that never came
--   up at all -- untouched? A reconciler that rewrites everything from
--   scratch on every pass (drifting wording, dropping unrelated tasks) is
--   exactly as broken as one that never updates anything; this checks both
--   directions in the same pass.
--
--   Direct 'tasksReconcileAgent' call, no branch/storage needed, same shape
--   as 'Agent.Integration.TasksSuggestionQualitySpec'. Real LLM call,
--   cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.TasksReconcileSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import Storyteller.Writer.Agent.Tasks (tasksReconcileAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

currentTasks :: T.Text
currentTasks = T.unlines
  [ "## Short-term goals"
  , "- Get back the ledger Tomas took from their father's desk."
  , "- Repay Kess the silver she's owed."
  , ""
  , "## Long-term goals"
  , "- Reclaim the family's seat on the merchant council."
  , ""
  , "## Aversions"
  , "- Ending up cast out of the guild the way their father was."
  ]

newMaterial :: T.Text
newMaterial = T.unwords
  [ "Tomas finally handed over the ledger this morning, shame-faced and"
  , "silent. I didn't gloat -- just took it and walked away."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "The new material shows a character got back a stolen ledger from their"
  , "brother Tomas. Compare the original tasks.md to the updated one. Does"
  , "the updated tasks.md no longer list \"get back the ledger from Tomas\""
  , "(or an equivalent worded task) as an open, unresolved short-term goal --"
  , "either removed entirely or clearly marked done -- while every other"
  , "original task (repaying Kess, reclaiming the council seat, and the"
  , "aversion about being cast out of the guild) is still present with"
  , "essentially the same meaning, not reworded into something different or"
  , "dropped?"
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "tasksReconcileAgent (real LLM, cached)" $
  it "drops a task the new material resolves while leaving unrelated tasks untouched" $
    runExpect @judgeModel runner $ do
      updated <- tasksReconcileAgent "Lena" currentTasks newMaterial
      info ("tasksReconcileAgent output:\n" <> updated)
      embed $ updated `shouldNotBe` ""

      let artifact = T.unlines
            [ "### Original tasks.md", "", currentTasks
            , "### New material", "", newMaterial
            , "### Updated tasks.md", "", updated
            ]
      Verdict pass reason <- judge @judgeModel artifact judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ do
        pass `shouldBe` True
        updated `shouldSatisfy` T.isInfixOf "Kess"
        updated `shouldSatisfy` T.isInfixOf "council"
