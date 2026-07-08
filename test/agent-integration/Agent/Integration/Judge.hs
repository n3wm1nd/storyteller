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
  ) where

import Autodocodec (HasCodec(..), object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
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
judge
  :: forall model r
  .  ( Members '[LLM model, Fail] r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => T.Text   -- ^ the artifact under review (agent output, proposed edit, etc.)
  -> T.Text   -- ^ the rubric question, phrased so a "yes" means success
  -> Sem r Verdict
judge artifact question = do
  let tool = mkToolWithMeta
               "submit_verdict"
               "Submit your verdict on whether the artifact satisfies the question. Always call this -- never answer in plain text."
               (submitVerdict @r)
               "pass"   "true if the artifact satisfies the question, false otherwise"
               "reason" "brief explanation for the verdict, quoting the artifact where relevant"
      tools = [LLMTool tool]
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

  response <- queryLLM @model
    [ SystemPrompt "You are a precise, skeptical reviewer. Judge only what is asked; do not reward effort or length."
    , Tools (map llmToolToDefinition tools)
    ]
    [UserText prompt]
  case [tc | AssistantTool tc <- response] of
    (call : _) -> do
      result <- executeToolCallFromList tools call
      case toolResultOutput result of
        Right value -> case parseEither parseJSONViaCodec value of
          Right verdict -> return verdict
          Left err      -> fail ("judge: could not parse verdict: " <> err)
        Left err -> fail ("judge: tool execution failed: " <> T.unpack err)
    [] -> fail "judge: model did not call submit_verdict"
