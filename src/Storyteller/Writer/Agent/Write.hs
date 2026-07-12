{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Write agent: continue one chapter as a real conversation, not a single
-- flattened prompt.
--
-- Where 'Storyteller.Writer.Agent.Continuation.proseAgent' assembles
-- everything into one 'UniversalLLM.UserText', this builds a @['Message']@
-- shaped the way an LLM API actually wants one: world lore and earlier
-- chapters as stable early history, this chapter's own back-and-forth
-- reconstructed as alternating turns (see
-- 'Storyteller.Writer.Agent.Chat.historyFromFileTicks' -- exactly the
-- Prompt\/Atom-tick pairing chat already uses, reused here because a
-- chapter's tick chain has the identical shape), and only the volatile
-- pieces -- the character journal excerpt, pinned context, and the new
-- instruction -- placed near the end. 'UniversalLLM' handles prompt-cache
-- placement on its own once the shape is real messages (see the Anthropic
-- provider's @addConversationCacheControl@\/system-block caching, always
-- on); nothing here manages a cache breakpoint by hand.
--
-- No filesystem, no branch, no splitter -- everything is already-gathered
-- data. Reading it (per-character summaries, world lore, earlier chapters,
-- the current chapter's own ticks) and appending the result are the
-- caller's job, same split 'Storyteller.Writer.Agent.Continuation' already
-- draws between reading and generating.
module Storyteller.Writer.Agent.Write
  ( writeAgent
  , buildChapterMessages
  , flattenCharBlocks
  ) where

import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storage.Tick (FileTick)

import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Writer.Agent
  ( Instruction(..), Prose(..), CharContextBlock(..), CharLabel(..), CharSummary(..)
  , ContextBlock(..) )
import Storyteller.Writer.Agent.Chat (historyFromFileTicks)
import Storyteller.Writer.Agent.Continuation (defaultWriterSystemPrompt, defaultWriterConfig)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfig)

-- | Continue one chapter, given everything already gathered for it.
--
--   Always the 'Storyteller.Core.LLM.Role.ProseModel' role, same
--   @"agent.writer"@ prompt-storage key 'Storyteller.Writer.Agent.
--   Continuation.proseAgent' uses -- one persona, two shapes of call.
--
--   Message order (each a real list entry, not a section of one string):
--
--     1. World lore (if any) -- one message, stable across an entire
--        story.
--     2. Earlier chapters, oldest first -- one message each, each one's
--        full current prose verbatim (no prompts, no instructions: a
--        chapter file's working-tree content already is just its prose).
--     3. This chapter's "identity" block -- every active character's
--        'csSheet'\/'csContext', under a @"## Character: {name}"@ header
--        each (see 'flattenCharBlocks') -- mostly stable for the whole
--        chapter, so it sits once near its start.
--     4. This chapter's own conversation so far, reconstructed via
--        'historyFromFileTicks' -- alternating 'UserText' (what was
--        asked) and 'AssistantText' (what got written), oldest first.
--     5. A shallow splice -- pinned\/short-term context plus every active
--        character's 'csJournal' excerpt -- one message, placed a beat
--        before the final instruction rather than folded into it, so the
--        model reads it as background for this turn without mistaking it
--        for the thing to continue writing.
--     6. The new instruction -- always the last message.
writeAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [ContextBlock]              -- ^ world lore, already rendered (see 'Storyteller.Writer.Agent.WorldContext.WorldLore')
  -> [ContextBlock]              -- ^ standing style guide, already rendered (see 'Storyteller.Writer.Agent.WorldContext.SystemContext') -- appended to the system prompt, not a message
  -> [(CharLabel, CharSummary)]  -- ^ every active character's summary
  -> [ContextBlock]              -- ^ pinned/short-term context (e.g. the user's own selection)
  -> [T.Text]                    -- ^ earlier chapters, oldest-first, full prose
  -> [FileTick]                  -- ^ this chapter's own tick history so far, oldest-first
  -> Instruction
  -> Sem r Prose
writeAgent lore style chars pinned earlierChapters currentTicks instruction = do
  Prompt sysPrompt <- getPrompt "agent.writer" defaultWriterSystemPrompt
  configs          <- getConfig "agent.writer" defaultWriterConfig
  let styleText        = renderContextBlocks style
      sysText
        | T.null styleText = sysPrompt
        | otherwise         = sysPrompt <> "\n\n" <> styleText
      configsWithPrompt = SystemPrompt sysText : configs
      messages          = buildChapterMessages lore chars pinned earlierChapters currentTicks instruction

  info "writeAgent: querying model..."
  response <- queryLLM configsWithPrompt messages
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | The pure heart of 'writeAgent': everything about message order,
--   what's included, and what's dropped when empty, with no LLM effect
--   attached -- so the ordering this module's whole design turns on (see
--   'writeAgent's own Haddock for the numbered list) can be asserted
--   directly rather than only observed through a real model call.
buildChapterMessages
  :: forall m
  .  [ContextBlock]              -- ^ world lore, already rendered
  -> [(CharLabel, CharSummary)]  -- ^ every active character's summary
  -> [ContextBlock]              -- ^ pinned/short-term context
  -> [T.Text]                    -- ^ earlier chapters, oldest-first, full prose
  -> [FileTick]                  -- ^ this chapter's own tick history so far, oldest-first
  -> Instruction
  -> [Message m]
buildChapterMessages lore chars pinned earlierChapters currentTicks (Instruction instr) =
  loreMsgs ++ earlierMsgs ++ chapterStartMsgs ++ conversationMsgs ++ spliceMsgs ++ [instructionMsg]
  where
    loreMsgs = [ UserText (renderContextBlocks lore) | not (null lore) ]

    earlierMsgs = [ UserText c | c <- earlierChapters, not (T.null c) ]

    -- 'flattenCharBlocks' always prepends a header per entry, so a
    -- character with nothing under 'csSheet'\/'csContext' has to be
    -- dropped here -- not passed through with an empty blocks list -- or
    -- it'd surface as a header with nothing under it.
    identityBlocks = flattenCharBlocks
      [ (label, blocks) | (label, cs) <- chars, let blocks = csSheet cs ++ csContext cs, not (null blocks) ]
    chapterStartMsgs = [ UserText (renderCharBlocks identityBlocks) | not (null identityBlocks) ]

    conversationMsgs = historyFromFileTicks currentTicks

    journalBlocks = flattenCharBlocks [ (label, csJournal cs) | (label, cs) <- chars, not (null (csJournal cs)) ]
    spliceText = T.intercalate "\n\n" (filter (not . T.null) [renderContextBlocks pinned, renderCharBlocks journalBlocks])
    spliceMsgs = [ UserText spliceText | not (T.null spliceText) ]

    instructionMsg = UserText $ mconcat
      [ "## Instruction\n\n", instr, "\n\n"
      , "Write approximately 300 words.\n"
      , "Write only the new text to append. Do not repeat or summarise existing content."
      ]

renderContextBlocks :: [ContextBlock] -> T.Text
renderContextBlocks blocks = T.intercalate "\n\n" [ t | ContextBlock t <- blocks ]

renderCharBlocks :: [CharContextBlock] -> T.Text
renderCharBlocks blocks = T.intercalate "\n\n" [ t | CharContextBlock t <- blocks ]

-- | @(label, resolved blocks)@ per active character branch, flattened into
--   the plain 'CharContextBlock' list a rendered message actually takes --
--   each branch's blocks preceded by a @"## Character: {name}"@ header
--   block. Shared with 'Storyteller.Writer.Agent.Outline''s reconciliation
--   calls (which still take this same flattened shape directly) and with
--   both 'writeAgent's own identity and journal splits above.
flattenCharBlocks :: [(CharLabel, [CharContextBlock])] -> [CharContextBlock]
flattenCharBlocks = concatMap
  (\(CharLabel name, blocks) -> CharContextBlock ("## Character: " <> name) : blocks)
