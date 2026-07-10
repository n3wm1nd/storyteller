{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Companion to 'Agent.Integration.OutlineSplitQualitySpec', answering a
--   deliberately different question: not "does the split step behave
--   correctly end to end against messy input" (that spec nudges the model
--   toward concise beat sheets via a 'Storyteller.Core.Prompt.PromptStorage'
--   override, since it's about pipeline correctness, not full-scale
--   content) but "are the settings we actually *ship*
--   ('Storyteller.Writer.Agent.Outline.defaultSplitConfig', unmodified)
--   sufficient for a real, unnudged outline against whichever model is
--   configured." No prompt override here, on purpose -- see @../PLAN.md@.
--
--   Reuses 'Agent.Integration.OutlineSplitQualitySpec.messyOutlines' rather
--   than inventing new fixtures: the same realistic irregularities are the
--   relevant input either way, only the instructions differ between the two
--   specs. A budget of 2 (not 0): this isn't measuring first-try
--   reliability, it's measuring whether the shipped config lets the model
--   finish at all -- see 'Agent.Integration.Journey.runJourney', which
--   tolerates the same budget for the same reason.
module Agent.Integration.OutlineSplitDefaultsSpec (spec) where

import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)

import Agent.Integration.Harness (Runner, assertToolCallBudget, runExpect)
import Agent.Integration.OutlineSplitQualitySpec (messyOutlines)
import Storyteller.Core.LLM.Role (AgentModel)
import Storyteller.Writer.Agent.Outline (ChapterBeats(..), OutlineDoc(..), splitOutlineAgent)

spec
  :: forall judgeModel
  .  Runner judgeModel -> Spec
spec runner = describe "splitOutlineAgent against shipped defaults, no test-only nudging" $
  mapM_ (\(name, outline) -> it name (checkOutline outline)) messyOutlines
  where
    checkOutline outline = runExpect @judgeModel runner $ do
      (sheets, turns) <- assertToolCallBudget @AgentModel 2 (splitOutlineAgent [] (OutlineDoc outline))
      info $ "split step: " <> T.pack (show (length turns)) <> " turn(s), "
           <> T.pack (show (length sheets)) <> " sheet(s)"

      embed $ do
        -- The whole point here: did the shipped defaults let the model
        -- produce *something* for every chapter, or did generation die mid
        -- call (truncation, malformed JSON) before reaching a usable
        -- result? Content fidelity is 'OutlineSplitQualitySpec's job, not
        -- this spec's -- this only checks the defaults don't strand the
        -- model partway through.
        sheets `shouldSatisfy` (not . null)
        mapM_ (\(ChapterBeats path _) ->
                 path `shouldSatisfy` \p -> "chapters/" `isPrefixOf` p && ".outline.md" `isSuffixOf` p)
              sheets
