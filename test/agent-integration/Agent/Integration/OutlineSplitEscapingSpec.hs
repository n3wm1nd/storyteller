{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Isolated check for @emit_beat_sheet@'s JSON escaping reliability,
--   separated on purpose from 'Agent.Integration.OutlineSplitQualitySpec'.
--   That spec's fixtures deliberately invite the model to invent plausible
--   technical color (a file path, a UNC share) while filling in beats the
--   outline only sketches -- which means a raw backslash showing up in its
--   output is expected, not a bug, and a check that flagged it would be
--   testing against the very instructions the spec itself gives (see
--   @../PLAN.md@). This spec asks the escaping question on its own, with an
--   outline that has nothing legitimate to escape at all: a plain pirate
--   adventure, five chapters, no technical content of any kind. Any
--   backslash-escape artifact in the result here has no excuse, so it's an
--   unambiguous finding rather than something a human has to eyeball.
--
--   Only runs the split step, not full chapter prose -- generating five
--   chapters of pirate fiction would add nothing to this specific question
--   and just cost more time for no more signal.
module Agent.Integration.OutlineSplitEscapingSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)

import Agent.Integration.Harness (Runner, assertToolCallBudget, runExpect)
import Agent.Integration.ToolCallQuality
  (TurnReport(..), escapingArtifacts, reportTurn, stringArguments)
import Storyteller.Core.LLM.Role (AgentModel)
import Storyteller.Writer.Agent.Outline (OutlineDoc(..), splitOutlineAgent)

spec
  :: forall judgeModel
  .  Runner judgeModel -> Spec
spec runner = describe "splitOutlineAgent tool-call escaping, isolated from content that could excuse it" $
  it "produces beat sheets with no escaping artifacts, for an outline with nothing legitimate to escape" $
    runExpect @judgeModel runner $ do
      (sheets, turns) <- assertToolCallBudget @AgentModel 2 (splitOutlineAgent [] (OutlineDoc pirateOutline))
      let artifacts =
            [ (name, issues)
            | tr           <- map reportTurn turns
            , (name, args) <- trValidCalls tr
            , str          <- stringArguments args
            , let issues = escapingArtifacts str
            , not (null issues)
            ]
      info $ "split step: " <> T.pack (show (length turns)) <> " turn(s), "
           <> T.pack (show (length sheets)) <> " sheet(s), "
           <> T.pack (show (length artifacts)) <> " escaping artifact(s)"
      mapM_ (\(name, issues) -> info $ name <> " escaping artifacts: " <> T.pack (show issues)) artifacts

      embed $ sheets `shouldSatisfy` (not . null)
      -- The whole point: no legitimate reason for any of these to appear in
      -- a plain adventure outline, so any hit here is a real finding about
      -- the configured model's JSON reliability, not a heuristic to second-
      -- guess (contrast 'Agent.Integration.OutlineSplitQualitySpec').
      embed $ artifacts `shouldBe` []

pirateOutline :: T.Text
pirateOutline = T.unlines
  [ "A five-chapter pirate adventure."
  , ""
  , "Chapter 1: The Marked Chart"
  , "A young sailor named Cato wins a torn half of a treasure chart in a"
  , "dockside card game, not realizing what he's holding until the captain"
  , "who lost it comes looking, furious and armed."
  , ""
  , "Chapter 2: Signing the Articles"
  , "To escape the captain's men, Cato signs aboard the first ship leaving"
  , "port -- which turns out to be crewed by pirates who recognize the chart"
  , "the moment they see it and press him into finding the other half."
  , ""
  , "Chapter 3: The Storm and the Split"
  , "A storm nearly sinks the ship; in the chaos, half the crew mutinies,"
  , "convinced Cato is hiding the second half of the chart for himself."
  , ""
  , "Chapter 4: The Island"
  , "The surviving crew reaches the island the chart points to and finds"
  , "the other half already claimed by a rival captain, forcing an uneasy"
  , "alliance neither side trusts."
  , ""
  , "Chapter 5: What the Chart Was Really For"
  , "The treasure isn't gold but a hidden cove that lets a ship vanish from"
  , "pursuit entirely -- and Cato has to decide who deserves to know it."
  ]
