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
import Runix.FileSystem (FileSystem, FileSystemRead)
import Runix.LLM (queryLLM)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt)
import Storyteller.Writer.Agent (CharContextBlock(..))
import Storyteller.Writer.Agent.CharContext (charSummaryAgent)

-- | Answer @question@ using only what's readable from @project@'s
--   filesystem -- deliberately effect-minimal like
--   'Storyteller.Writer.Agent.Continuation.proseAgent': no branch name, no
--   dynamic FS open, no world-lore lookup (deferred -- see the design
--   conversation). Opening the right character's branch is the caller's
--   job (see 'Server.Writer.File.askCharacter'), same seam
--   'Storyteller.Writer.Agent.CharContext.charSummaryAgent' itself already
--   draws, which this reuses directly for the read.
askCharacterAgent
  :: forall project r
  .  (LLMs r, Members '[FileSystem project, FileSystemRead project, PromptStorage, Fail] r)
  => T.Text -> Sem r T.Text
askCharacterAgent question = do
  blocks <- charSummaryAgent @project
  configsWithPrompt <- getConfigWithPrompt "agent.ask-character" defaultAskSystemPrompt defaultAskConfig
  let userMsg = renderAskPrompt blocks question
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
--   answer-the-question call, not prose generation: low budget, low
--   temperature (a consistent, grounded answer, not a creative one).
defaultAskConfig :: [ModelConfig AgentModel]
defaultAskConfig = [MaxTokens 500, Temperature 0.4]
