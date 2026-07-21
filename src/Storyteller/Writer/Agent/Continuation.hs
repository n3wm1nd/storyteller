{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Prose generation agents.
--
-- 'proseAgent' is the pure LLM core: given assembled context, produce new
-- text. No filesystem access, no Context DSL awareness at all -- context
-- arrives as real, already-bound @['UniversalLLM.Message' 'ProseModel']@,
-- easy to extend with new effects (style guides, persona, etc.) in
-- isolation. Every real caller assembles that context via the Context DSL
-- (see CONTEXT-DSL.md and 'Server.Writer.File.flatMainMessages'), but
-- binds it to 'ProseModel' at its own call site, right where a
-- 'Storyteller.Context.DSL.Context.Context' gets forced -- 'proseAgent'
-- itself is always 'ProseModel', unconditionally, so there's no
-- flexibility lost by settling that there rather than re-deferring it in
-- here. (Contrast 'Storyteller.Writer.Agent.Roleplay.askCharacter''s own
-- @sceneContext@, which genuinely feeds both a 'ProseModel' and an
-- 'AgentModel' call from one resolved value -- that one *does* stay
-- model-agnostic until each of its own two call sites.) This module used
-- to also export 'gatherFileContext' for context assembly, removed once
-- its last caller migrated to the DSL.
module Storyteller.Writer.Agent.Continuation
  ( proseAgent
  , defaultWriterSystemPrompt
  , defaultWriterConfig
  ) where

import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharContextBlock(..), ExistingContent(..), WordCount(..))
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfigWithPrompt)

-- | Ask the LLM to produce new prose given fully assembled context.
--   No filesystem access — all inputs are explicit.
--   This is the composition point for cross-cutting concerns: style guides,
--   persona, tone constraints, etc. can be added here as new effects.
--
--   Always the 'ProseModel' role -- see 'Storyteller.Core.LLM.Role.LLMs'.
--
--   Logs immediately before the one 'queryLLM' call it makes -- this is the
--   single call single-shot 'Storyteller.Writer.Agent.Outline' generation
--   (@chapterProse@\/@reconcileChapter@) funnels through, so without it a
--   slow model response looks identical, from the log, to a hang: nothing
--   at all between whatever "starting" line the caller logged and either
--   the result or a very long silence. 'Storyteller.Writer.Agent.Write.
--   writeAgent' makes its own separate 'queryLLM' call now (a real
--   per-chapter @[Message]@, not this module's single flattened user
--   message) -- it logs the same way at its own call site, but shares
--   'defaultWriterSystemPrompt'\/'defaultWriterConfig' with this module so
--   the two prose personas stay one persona under one @"agent.writer"@
--   prompt-storage key.
proseAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Maybe WordCount        -- ^ approximate desired output length
  -> [CharContextBlock]     -- ^ character context blocks
  -> [Message ProseModel]   -- ^ branch context, already bound -- a caller's own 'Storyteller.Context.DSL.Context.Context', forced and rendered via 'Storyteller.Context.DSL.Render.dslMessageToLLM' at its own call site (see this module's own Haddock)
  -> ExistingContent        -- ^ current content of the file being continued
  -> Instruction
  -> Sem r Prose
proseAgent outputHint charContexts context (ExistingContent existing) (Instruction instruction) = do
  configsWithPrompt <- getConfigWithPrompt "agent.writer" defaultWriterSystemPrompt defaultWriterConfig
  Prompt extraInstructions <- getPrompt "agent.writer.instructions" defaultWriterInstructions

  let trailingMsg = writerTrailingMessage charContexts existing extraInstructions instruction outputHint

  info "proseAgent: querying model..."
  response <- queryLLM configsWithPrompt (context ++ [UserText trailingMsg])
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | Fallback for @agent.writer@ (the namespace root is implicitly the system
--   prompt/config -- see 'Storyteller.Core.Prompt'), used until an override is committed
--   to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultWriterSystemPrompt :: Prompt
defaultWriterSystemPrompt =
  "You are a creative writing assistant. Output only prose, nothing else. \
  \Never restate the instruction back as narration -- absorb what it asks for and \
  \render it in the story's own voice, as events, dialogue, and detail, not as a \
  \paraphrase of the request. Any word count given is a rough target, not a hard \
  \limit -- go over or under it when the scene calls for it. Do not wrap up, resolve, \
  \or otherwise end the story in this section; leave threads open for what comes next."

-- | Compiled-in sampling default for @agent.writer@ (also backing
--   'Storyteller.Writer.Agent.Outline.chapterProse'\/'chapterProseByBeat'\/
--   'reconcileChapter'\/'reconcileChapterByBeat', all thin wrappers over this
--   same 'proseAgent') -- see @$key.llmsettings.yaml@ overrides via
--   'getConfig'. Prose generation is the one call that can run to a whole
--   chapter (up to ~1200 words, see 'Storyteller.Writer.Agent.Outline.
--   chapterProse'), so it gets the most headroom of any agent's default, and
--   a higher temperature than the tool-calling roles: creative variation is
--   wanted here, not a liability.
defaultWriterConfig :: [ModelConfig ProseModel]
defaultWriterConfig = [MaxTokens 3000, Temperature 0.9]

-- | Fallback for @agent.writer.instructions@: standing instructions appended
--   to every writer prompt (house style, voice, recurring constraints), on
--   top of the per-call 'Instruction'. Empty by default — a project opts in
--   by committing an override to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultWriterInstructions :: Prompt
defaultWriterInstructions = ""

-- | The one message sent *after* @context@'s own real messages -- character
--   info, the file being continued, standing instructions, and the actual
--   instruction, none of which are DSL-produced conversational structure
--   (they're always exactly one turn's worth of static framing), so
--   there's no role fidelity to lose by keeping them as one string here.
--   Assembled directly rather than through a named-placeholder template:
--   the section order and headers are fixed by this function, so there's
--   no way for a caller to typo a slot name that silently drops a
--   section. The one piece of free text a project can still override is
--   @extraInstructions@ (see 'defaultWriterInstructions'), inserted
--   verbatim at a single fixed point.
writerTrailingMessage
  :: [CharContextBlock]
  -> T.Text            -- ^ existing file content
  -> T.Text            -- ^ extra standing instructions (may be empty)
  -> T.Text            -- ^ per-call instruction
  -> Maybe WordCount
  -> T.Text
writerTrailingMessage charContexts existing extraInstructions instruction outputHint =
  mconcat
    [ charSection
    , existingSection
    , extraInstructionsSection
    , "## Instruction\n\n" <> instruction <> "\n\n"
    , lengthHint
    , "Write only the new text to append. Do not repeat or summarise existing content."
    ]
  where
    charSection
      | null charContexts = ""
      | otherwise =
          "Character information:\n\n"
          <> T.intercalate "\n\n" [ t | CharContextBlock t <- charContexts ]
          <> "\n\n"

    existingSection
      | T.null existing = "The file is currently empty.\n\n"
      | otherwise        = "File to continue:\n\n" <> existing <> "\n\n"

    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"

    lengthHint = case outputHint of
      Nothing            -> ""
      Just (WordCount n) -> "Aim for roughly " <> T.pack (show n) <> " words -- a guideline, not a hard cutoff.\n"

