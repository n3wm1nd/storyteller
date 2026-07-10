{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Same experiment as 'Agent.Integration.OutlineSplitFreeformSpec' (same
--   fixtures, same check, via 'Agent.Integration.OutlineSplitCheck'), but
--   against 'Storyteller.Writer.Agent.Outline.splitOutlineBulk' -- one
--   single response covering every chapter, split deterministically on a
--   @---@ delimiter, instead of one chapter per conversational turn.
--   Directly comparable to the freeform spec's results, model for model,
--   fixture for fixture -- see @FINDINGS.md@ for what the comparison
--   actually shows once it's been run.
module Agent.Integration.OutlineSplitBulkSpec (spec) where

import Agent.Integration.Harness (Runner)
import Agent.Integration.OutlineSplitCheck (splitAgainstMessyOutlines)
import Storyteller.Writer.Agent.Outline (splitOutlineBulk)
import Test.Hspec (Spec)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec = splitAgainstMessyOutlines @judgeModel
  "splitOutlineBulk against the same messy outlines, one response, --- delimited"
  splitOutlineBulk
