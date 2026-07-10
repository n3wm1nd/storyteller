{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | LLM-as-judge: turns a fuzzy "is this output good" question into a
--   structured pass\/fail, via a forced tool call rather than prose-sniffing
--   -- the same pattern
--   'Storyteller.Writer.Agent.ReplaceTool.reworkAtom' uses to get a reliable
--   'ReplaceProposal' back instead of parsing free text.
--
--   __Same-model caveat__: 'judge' is generic over the judging model
--   precisely so it doesn't have to be 'Storyteller.Core.Runtime.StoryModel'
--   -- asking a model to judge its own output risks a blind spot shared
--   between generator and judge (it may wave through exactly the kind of
--   mistake it's prone to making itself). 'Agent.Integration.Harness' wires
--   the agent under test and the judge as two independently configured
--   'LLM' effects (different model types, different credentials) for
--   exactly this reason; see its Haddock for how the judge model is chosen.
--   Whatever the choice, the underlying mitigation is still visibility, not
--   the verdict alone: every spec prints the full artifact and
--   verdict\/reason to stdout, so during the prompt-tuning loop a human is
--   actually reading what the judge approved.
module Agent.Integration.Judge
  ( Verdict(..)
  , judge
  , judgeOrFail
  ) where

import Autodocodec (HasCodec(..), object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import Runix.Logging (Logging, info)
import Test.Hspec (expectationFailure)
import UniversalLLM (Message(..), ModelConfig(..), ProviderOf, HasTools, SupportsSystemPrompt)
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

-- | A judge's verdict on one rubric question: whether the artifact passes,
--   and why -- the reason is what makes a verdict inspectable rather than
--   an opaque bit.
data Verdict = Verdict
  { vPass   :: Bool
  , vReason :: T.Text
  } deriving (Show, Eq)

instance HasCodec Verdict where
  codec = object "Verdict" $
    Verdict
      <$> requiredField "pass"   "whether the artifact satisfies the question" .= vPass
      <*> requiredField "reason" "brief explanation for the verdict, quoting the artifact where relevant" .= vReason

instance ToolParameter Verdict where
  paramName = "verdict"
  paramDescription = "the judge's pass/fail verdict and reason"

submitVerdict :: forall r. Bool -> T.Text -> Sem r Verdict
submitVerdict pass reason = pure (Verdict pass reason)

-- | Ask a model a yes\/no rubric question about an artifact, forced through
--   a tool call so the answer is parsed as data. Generic over the judging
--   model -- callers pick which one with a type application, e.g.
--   @judge \@JudgeModel@ (see 'Agent.Integration.Harness').
--
--   Not calling @submit_verdict@ at all isn't an answer this accepts: a
--   model that responds with plain text, or a call that fails to parse,
--   gets the failure fed back and another chance, the same
--   execute-and-recurse shape 'Storyteller.Writer.Agent.Outline.splitOutlineAgent'
--   uses for its own tool loop -- bounded by 'maxTurns' so a model that
--   never calls it can't hang the caller forever, at which point this fails
--   for real. A verdict is exactly one 'Bool', not free text a caller has to
--   sniff for "yes"/"no"; there's no fallback reading to fall back to.
judge
  :: forall model r
  .  ( Members '[LLM model, Fail] r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => T.Text   -- ^ the artifact under review (agent output, proposed edit, etc.)
  -> T.Text   -- ^ the rubric question, phrased so a "yes" means success
  -> Sem r Verdict
judge artifact question = loop maxTurns [UserText prompt]
  where
    tool = mkToolWithMeta
             "submit_verdict"
             "Submit your verdict on whether the artifact satisfies the question. Always call this -- never answer in plain text."
             (submitVerdict @r)
             "pass"   "true if the artifact satisfies the question, false otherwise"
             "reason" "brief explanation for the verdict, quoting the artifact where relevant"
    tools = [LLMTool tool]
    configs =
      [ SystemPrompt "You are a precise, skeptical reviewer. Judge only what is asked; do not reward effort or length."
      , Tools (map llmToolToDefinition tools)
      ]
    prompt = T.unlines
      [ "## Artifact under review"
      , ""
      , artifact
      , ""
      , "## Question"
      , ""
      , question
      , ""
      , "Call submit_verdict with your answer."
      ]

    -- Bound on retries for a model that won't call submit_verdict at all,
    -- or keeps calling it wrong -- generous enough for a model to recover
    -- from one bad attempt, not so generous it silently burns turns on a
    -- model that fundamentally won't cooperate.
    maxTurns = 4 :: Int

    loop 0 _ = fail "judge: model did not call submit_verdict after several attempts"
    loop budget history = do
      response <- queryLLM @model configs history
      case [tc | AssistantTool tc <- response] of
        (call : _) -> do
          result <- executeToolCallFromList tools call
          case toolResultOutput result of
            Right value -> case parseEither parseJSONViaCodec value of
              Right verdict -> return verdict
              Left err -> retry budget history response
                [ToolResultMsg result, UserText $
                  "That didn't parse as a valid verdict (" <> T.pack err <> "). Call submit_verdict again, correctly."]
            Left err -> retry budget history response
              [ToolResultMsg result, UserText $
                "That call failed (" <> err <> "). Call submit_verdict again, correctly."]
        [] -> retry budget history response
          [UserText "You must call submit_verdict -- plain text isn't an accepted answer. Call it now."]

    retry budget history response extra = loop (budget - 1) (history <> response <> extra)

-- | 'judge', but a failing verdict fails the scenario directly with the
--   judge's own reason attached -- not a bare @pass \`shouldBe\` True@,
--   which only ever tells a reader "expected: True, got: False" and leaves
--   them to scroll back through the run's logs to find out why. Every spec
--   in this suite was repeating that same "call judge, log the verdict,
--   assert pass" dance by hand; this is that pattern, written once. Still
--   logs the verdict via 'Runix.Logging.info' on a pass, same as before --
--   only a failure's reason needs to reach the hspec failure message
--   itself, since a pass doesn't need to be dug back out of anything.
judgeOrFail
  :: forall model r
  .  ( Members '[LLM model, Fail, Logging, Embed IO] r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => T.Text   -- ^ the artifact under review
  -> T.Text   -- ^ the rubric question, phrased so a "yes" means success
  -> Sem r ()
judgeOrFail artifact question = do
  Verdict pass reason <- judge @model artifact question
  if pass
    then info ("judge verdict: True -- " <> reason)
    else embed $ expectationFailure ("judge: " <> T.unpack reason)
