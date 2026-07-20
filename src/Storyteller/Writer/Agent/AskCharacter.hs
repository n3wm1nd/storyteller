{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Ask-a-character agent: answer a question grounded only in one
-- character's own branch (sheet, journal, anything else tracked there) --
-- not the scene being written, not any other character's material. See the
-- design conversation this closes: dropping the journal from the writer's
-- own ambient context (too long, too narratively-derived, possibly stale)
-- in favor of a real per-character query that can only see what that
-- character could actually know.
module Storyteller.Writer.Agent.AskCharacter
  ( askCharacterAgent
  ) where

import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.Git (BranchOp)
import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Context (ContextStorage, resolveContextQuery, runContextBinding1, runContextValue)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Writer.Agent (CharContextBlock(..), flattenCharSummary)

-- | Answer @question@ using only what's readable from @charname@'s own
--   branch -- deliberately effect-minimal like
--   'Storyteller.Writer.Agent.Continuation.proseAgent' otherwise: no
--   world-lore lookup (deferred -- see the design conversation). Resolves
--   @context.character@ (a branch override on the @contexts@ branch, then
--   'Storyteller.Context.DSL.Library.contextCharacterDefault' as fallback
--   -- see 'Storyteller.Core.Context.resolveContextQuery') and reads its
--   @"journalFull"@ bucket -- everything, uncurated, same as this agent
--   always wanted -- which crosses to @charname@'s own branch itself --
--   unlike the old 'Storyteller.Writer.Agent.CharContext.charSummaryAgent'-
--   backed version, the caller (see 'Server.Writer.File.askCharacter') no
--   longer needs to open that branch's filesystem first.
askCharacterAgent
  :: forall branch r
  .  (LLMs r, Members '[BranchOp branch, Git, StoryStorage, ContextStorage, PromptStorage, Fail, Logging] r)
  => T.Text -> T.Text -> Sem r T.Text
askCharacterAgent charname question = do
  charBinding <- resolveContextQuery "context.character" (CtxLibrary.toBinding1 CtxLibrary.contextCharacterDefault) Nothing
  charVal     <- runContextBinding1 @branch charBinding charname
  summary     <- runContextValue @branch (CtxLibrary.characterSummaryOf "journalFull" charVal)
  let blocks = flattenCharSummary summary
  configsWithPrompt <- getConfigWithPrompt "agent.ask-character" defaultAskSystemPrompt defaultAskConfig
  let userMsg = renderAskPrompt blocks question
  info "askCharacterAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText userMsg]
  return (mconcat [ t | AssistantText t <- response ])

renderAskPrompt :: [CharContextBlock] -> T.Text -> T.Text
renderAskPrompt blocks question = T.intercalate "\n\n" (preamble : map unBlock blocks ++ [askedAs])
  where
    unBlock (CharContextBlock b) = b
    preamble = "Below is everything you (this character) know about yourself, from your own sheet and journal. \
               \Answer the question as this character would, using only this material. If it doesn't say, answer \
               \honestly that you don't know rather than inventing details."
    askedAs = "Question: " <> question

-- | Fallback for @agent.ask-character@ -- see 'Storyteller.Core.Prompt' on
--   why the namespace root is implicitly the system prompt/config.
defaultAskSystemPrompt :: Prompt
defaultAskSystemPrompt =
  "You are answering in character, grounded strictly in the material you're given about yourself."

-- | Compiled-in sampling default for @agent.ask-character@ -- a short,
--   answer-the-question call, not prose generation: low temperature (a
--   consistent, grounded answer, not a creative one). @MaxTokens@ is well
--   above what the answer itself needs, though -- 'AgentModel' declares
--   'UniversalLLM.HasReasoning' (see "Storyteller.Core.LLM.Role"), and
--   when the assigned model has reasoning enabled, its thinking tokens are
--   drawn from this same budget before any answer text -- a tight cap
--   sized only for the visible answer left nothing for the answer once
--   reasoning ran (see "Storyteller.Core.LLM.Settings"'s @asReasoning@ for
--   where that's toggled per-role).
defaultAskConfig :: [ModelConfig AgentModel]
defaultAskConfig = [MaxTokens 3000, Temperature 0.4]
