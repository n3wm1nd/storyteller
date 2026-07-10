{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The experiment 'Storyteller.Writer.Agent.Outline.splitOutlineFreeform'
--   exists to run: same messy fixtures
--   'Agent.Integration.OutlineSplitQualitySpec' uses, same content-fidelity
--   judge check ('Agent.Integration.OutlineSplitCheck'), but driving the
--   split as a plain conversation instead of a tool-call loop -- so a run
--   of this spec is directly comparable to a run of
--   'Agent.Integration.OutlineSplitBulkSpec' (same check, single-response
--   variant) and to the tool-call version, model for model, fixture for
--   fixture. See 'Storyteller.Writer.Agent.Outline.splitOutlineFreeform's
--   Haddock and @../PLAN.md@ for why this exists at all: a raw probe
--   against gpt-oss-20b found the tool-call loop reliably
--   duplicating/omitting/garbling chapters where a plain-conversation
--   prompt got the same breakdown perfectly right.
module Agent.Integration.OutlineSplitFreeformSpec (spec) where

import Agent.Integration.Harness (Runner)
import Agent.Integration.OutlineSplitCheck (splitAgainstMessyOutlines)
import Storyteller.Writer.Agent.Outline (splitOutlineFreeform)
import Test.Hspec (Spec)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec = splitAgainstMessyOutlines @judgeModel
  "splitOutlineFreeform against the same messy outlines, no tool-call loop"
  splitOutlineFreeform
