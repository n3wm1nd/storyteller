{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Ask-a-character agent: answer a question grounded only in one
-- character's own branch (sheet, journal, anything else tracked there) --
-- not the scene being written, not any other character's material.
--
-- This exists because the journal was dropped from the writer's own ambient
-- context: too long, too narratively-derived, and possibly stale by the
-- time it's read. A per-character query that can only see what that
-- character could actually know replaces it, and is more useful than the
-- ambient version was — it answers a question rather than padding a prompt.
module Storyteller.Writer.Agent.AskCharacter
  ( askCharacterAgent
  ) where

import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt)
import Storyteller.Context.DSL.Value (messageText)
import Storyteller.Context.DSL.Rendering (Context, namedChild, contextAllMessages)
import Storyteller.Writer.Agent.Context (CharacterContext(..))

-- | Answer @question@ as the character whose 'CharacterContext' this is --
--   deliberately effect-minimal like
--   'Storyteller.Writer.Agent.Continuation.proseAgent' otherwise: no
--   world-lore lookup (deferred: a character answering about themselves
--   shouldn't reach for material they'd have no way to know).
--
--   Takes the character's context already resolved, so this needs no
--   storage capability at all -- not 'Storyteller.Core.Branch.BranchOp',
--   not 'Storyteller.Core.Branch.Branches', not
--   'Storyteller.Core.Context.ContextStorage'. Whoever dispatches resolved
--   @context.character@ (a branch override on the @contexts@ branch, then
--   'Storyteller.Context.DSL.Library.contextCharacter' as fallback) and
--   crossed to that character's own branch to do it; see
--   'Server.Writer.File.askCharacter'. All that's left here is "which
--   buckets, in what order, framed how," which is this agent's actual
--   subject.
askCharacterAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => CharacterContext -> T.Text -> Sem r T.Text
askCharacterAgent (CharacterContext charContext) question = do
  configsWithPrompt <- getConfigWithPrompt "agent.ask-character" defaultAskSystemPrompt defaultAskConfig
  let userMsg = renderAskPrompt charContext question
  info "askCharacterAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText userMsg]
  return (mconcat [ t | AssistantText t <- response ])

-- | Sheet, then the character's own wider context, then their journal
--   uncurated (@"journalFull"@, everything, not the curated recent window
--   the writer's ambient context gets) -- reached by 'namedChild', so a
--   missing bucket is simply absent rather than a failure.
--
--   Each bucket is read with 'contextAllMessages', not 'renderText':
--   @"full"@ is a @for@ loop over the character's files, so its content is
--   in its child entries and its own default is empty. See
--   'contextAllMessages' -- this is the exact case it exists for.
renderAskPrompt :: Context -> T.Text -> T.Text
renderAskPrompt charContext question =
    T.intercalate "\n\n" (preamble : buckets ++ [askedAs])
  where
    buckets =
      [ body
      | name <- ["sheet", "full", "journalFull"]
      , Just sub <- [namedChild name charContext]
      , body <- map messageText (contextAllMessages sub)
      , not (T.null (T.strip body))
      ]
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
