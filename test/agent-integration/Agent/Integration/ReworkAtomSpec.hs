{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)
import Test.Hspec

import Runix.LLM (LLM)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Writer.Agent (Instruction(..))
import Storyteller.Writer.Agent.ReplaceTool (ReplaceProposal(..), reworkAtom)

import Agent.Integration.Harness (Runner)
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
  :: forall storyModel judgeModel
  .  ( HasTools storyModel, SupportsSystemPrompt (ProviderOf storyModel)
     , HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel) )
  => Runner storyModel judgeModel -> Spec
spec runner = describe "reworkAtom (real LLM, cached)" $ do

  it "corrects a planted continuity error without rewriting the rest of the sentence" $ do
    -- 'action' needs its own standalone, explicitly rank-2-quantified
    -- signature: 'runner' itself is rank-2 polymorphic in the effect row,
    -- and GHC can't infer that row for a bare do-block passed directly as
    -- its argument (nothing pins 'r' down without an expected type to
    -- check against up front).
    let action :: forall r. Members '[LLM storyModel, LLM judgeModel, PromptStorage, Fail] r
               => Sem r (Maybe ReplaceProposal, Maybe Verdict)
        action = do
          -- Empty config list: the model's own defaults (from
          -- 'Agent.Integration.Harness.knownModels') already came baked
          -- into the interpreter 'runner' wraps, so there's nothing to
          -- add per-call here.
          mProposal <- reworkAtom @storyModel [] wrongEyeColor instruction
          case mProposal of
            Nothing -> pure (Nothing, Nothing)
            Just proposal@(ReplaceProposal newText _) -> do
              verdict <- judge @judgeModel newText
                "Compared to the original sentence \"Her eyes were a deep blue, the \
                \same shade as her mother's.\", does this text correct the eye color \
                \to green while leaving the rest of the sentence (the comparison to \
                \her mother's eyes) intact? Answer no if it rewrites more than the \
                \color, or if it fails to correct the color at all."
              pure (Just proposal, Just verdict)

    result <- runner action

    case result of
      Left err -> expectationFailure err
      Right (mProposal, mVerdict) -> do
        putStrLn "\n=== reworkAtom proposal (continuity error case) ==="
        print mProposal
        putStrLn "\n=== judge verdict ==="
        print mVerdict

        case (mProposal, mVerdict) of
          (Just (ReplaceProposal newText _), Just (Verdict pass _)) -> do
            newText `shouldSatisfy` (T.isInfixOf "green" . T.toLower)
            newText `shouldSatisfy` (not . T.isInfixOf "blue" . T.toLower)
            pass `shouldBe` True
          _ -> expectationFailure "reworkAtom declined to propose a change for a genuine continuity error"

  it "declines to change an atom that's already consistent with the instruction" $ do
    let action :: forall r. Members '[LLM storyModel, LLM judgeModel, PromptStorage, Fail] r
               => Sem r (Maybe ReplaceProposal)
        action = reworkAtom @storyModel [] rightEyeColor instruction

    result <- runner action

    case result of
      Left err -> expectationFailure err
      Right mProposal -> do
        putStrLn "\n=== reworkAtom proposal (already-consistent case) ==="
        print mProposal
        mProposal `shouldSatisfy` \case
          Nothing -> True
          Just _  -> False
