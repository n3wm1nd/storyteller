{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Are 'Storyteller.Writer.Agent.ReplaceTool.reworkAtom''s edits actually
--   targeted, and does it know when to leave well enough alone?
--
--   Two real LLM calls, against whichever model @STORY_MODEL@ resolved to
--   (see 'Agent.Integration.Harness.knownModels'), cached under
--   test/fixtures/llm-agent-cache/agent/:
--
--   * positive -- an atom with a planted continuity error plus a correcting
--     instruction. A merely-mechanical pass (some tool call happens) isn't
--     enough here; the proposal has to actually fix the specific detail
--     without rewriting the rest of the sentence.
--   * negative -- the same instruction against an atom that's already
--     consistent. This is a check on the fixer prompt's restraint: does it
--     resist "fixing" something that isn't broken.
module Agent.Integration.ReworkAtomSpec (spec) where

import qualified Data.Text as T
import Polysemy (embed)
import Test.Hspec

import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)
import Storyteller.Writer.Agent (Instruction(..))
import Storyteller.Writer.Agent.ReplaceTool (ReplaceProposal(..), reworkAtom)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

instruction :: Instruction
instruction = Instruction
  "Continuity fix: earlier chapters establish that her eyes are green, not blue. \
  \Correct this atom if it conflicts with that."

wrongEyeColor :: T.Text
wrongEyeColor = "Her eyes were a deep blue, the same shade as her mother's."

rightEyeColor :: T.Text
rightEyeColor = "Her eyes were a deep green, the same shade as her mother's."

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "reworkAtom (real LLM, cached)" $ do

  it "corrects a planted continuity error without rewriting the rest of the sentence" $
    runExpect @judgeModel runner $ do
      mProposal <- reworkAtom [] wrongEyeColor instruction
      info ("reworkAtom proposal: " <> T.pack (show mProposal))
      case mProposal of
        Nothing -> embed $ expectationFailure
          "reworkAtom declined to propose a change for a genuine continuity error"
        Just (ReplaceProposal newText _) -> do
          verdict@(Verdict pass _) <- judge @judgeModel newText
            "Compared to the original sentence \"Her eyes were a deep blue, the \
            \same shade as her mother's.\", does this text correct the eye color \
            \to green while leaving the rest of the sentence (the comparison to \
            \her mother's eyes) intact? Answer no if it rewrites more than the \
            \color, or if it fails to correct the color at all."
          info ("judge verdict: " <> T.pack (show verdict))
          embed $ do
            newText `shouldSatisfy` (T.isInfixOf "green" . T.toLower)
            newText `shouldSatisfy` (not . T.isInfixOf "blue" . T.toLower)
            pass `shouldBe` True

  it "declines to change an atom that's already consistent with the instruction" $
    runExpect @judgeModel runner $ do
      mProposal <- reworkAtom [] rightEyeColor instruction
      info ("reworkAtom proposal: " <> T.pack (show mProposal))
      embed $ mProposal `shouldSatisfy` \case
        Nothing -> True
        Just _  -> False
