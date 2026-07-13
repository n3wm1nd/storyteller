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
--
-- Two things the caller ('Server.Writer.File.chatWriter') has to get right
-- for the cache-prefix discipline below to actually hold, neither enforced
-- by this module's own types:
--
--   * @currentTicks@ must be fetched /before/ this turn's own prompt is
--     stored -- otherwise the not-yet-answered prompt shows up twice, once
--     via 'historyFromFileTicks' and once as 'buildChapterMessages'\'s own
--     trailing instruction message.
--   * The instruction message is now literally @UserText instr@, the raw
--     prompt text, unwrapped -- deliberately identical to what
--     'historyFromFileTicks' will later replay a @\"prompt\"@ tick as, so a
--     turn's own final message is already exactly what a later call
--     reconstructs it to be. Per-turn boilerplate ("write ~300 words",
--     "only the new text") that used to live in that wrapper now lives in
--     the system prompt instead ('chapterContinuationNote') -- it never
--     varies, so there's no reason to pay to resend it as a user message on
--     every turn when the provider already caches the system block.
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

import Storage.Tick (FileTick(..))

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
--        asked) and 'AssistantText' (what got written), oldest first, split
--        at a depth between 'recentWindowMin' and 'recentWindowMax' turns
--        from the end: the older side sits here, before the splice; the
--        recent side sits after it (see step 5a). All of it, on one side or
--        the other, when the splice has nothing to say.
--     5. A shallow splice -- pinned\/short-term context plus every active
--        character's 'csJournal' excerpt -- one message, inserted mid-depth
--        rather than at either end (see 'splitByTurnWindow's Haddock).
--     5a. The recent tail of this chapter's conversation -- same source as
--        step 4, just the turns inside the depth window.
--     6. The new instruction -- always the last message, literally
--        @UserText instr@ now (no per-message boilerplate -- see
--        'chapterContinuationNote').
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
      sysText           = T.intercalate "\n\n" (filter (not . T.null) [sysPrompt, styleText, chapterContinuationNote])
      configsWithPrompt = SystemPrompt sysText : configs
      messages          = buildChapterMessages lore chars pinned earlierChapters currentTicks instruction

  info "writeAgent: querying model..."
  response <- queryLLM configsWithPrompt messages
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | Standing per-turn instruction that used to be templated into every
--   single instruction message ('buildChapterMessages'\'s old
--   @"## Instruction..."@ wrapper). It never varies call to call, so it
--   belongs in the system prompt -- cached there by the provider once,
--   same as 'defaultWriterSystemPrompt' itself -- rather than being resent
--   (and re-priced, and re-breaking the instruction message's byte-identity
--   with its own later replay as history) on every single turn.
chapterContinuationNote :: T.Text
chapterContinuationNote =
  "Write approximately 300 words per turn. Write only the new text to \
  \append. Do not repeat or summarise existing content."

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
  loreMsgs ++ earlierMsgs ++ chapterStartMsgs ++ olderMsgs ++ spliceMsgs ++ recentMsgs ++ [instructionMsg]
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

    journalBlocks = flattenCharBlocks [ (label, csJournal cs) | (label, cs) <- chars, not (null (csJournal cs)) ]
    spliceText = T.intercalate "\n\n" (filter (not . T.null) [renderContextBlocks pinned, renderCharBlocks journalBlocks])
    spliceMsgs = [ UserText spliceText | not (T.null spliceText) ]

    -- Only split when there's actually something to insert at the split
    -- point -- an empty splice has nowhere to sit, so there's no reason to
    -- pay for the scan (or to disturb 'historyFromFileTicks'\'s single pass
    -- over the whole history) for nothing.
    (olderMsgs, recentMsgs)
      | null spliceMsgs = ([], historyFromFileTicks currentTicks)
      | otherwise =
          let (olderTicks, recentTicks) = splitByTurnWindow recentWindowMin recentWindowMax currentTicks
          in (historyFromFileTicks olderTicks, historyFromFileTicks recentTicks)

    instructionMsg = UserText instr

-- | The splice's depth window: at least @recentWindowMin@, at most
--   @recentWindowMax@ turns stay after it. Kept as a real range rather than
--   a single fixed depth (that's just the @min == max@ special case) so the
--   split point can hold /still/ across a stretch of turns instead of
--   moving on every single one -- see 'splitByTurnWindow's Haddock for why
--   that's what actually buys back cache hits, not just bounds the miss.
recentWindowMin, recentWindowMax :: Int
recentWindowMin = 2
recentWindowMax = 4

-- | Split @ticks@ at a boundary chosen so the "recent" (post-splice) side
--   holds between @lo@ and @hi@ turns -- inclusive, a "turn" counted by
--   prompt ticks so a prompt and the atom(s) that answered it always land
--   on the same side of the cut.
--
--   The boundary is *not* recomputed as "always exactly N turns back from
--   the end" -- that moves by one turn on every single call, which means
--   the tick immediately before the splice is a different tick every turn,
--   which means the splice (and everything after it) never lines up with
--   what a previous call actually sent, so nothing after
--   'buildChapterMessages'\'s @olderMsgs@ segment could ever be served from
--   cache. Instead the boundary only advances once the recent side would
--   exceed @hi@ turns, and when it does, it jumps forward by exactly
--   @hi - lo + 1@ turns -- landing the recent side back at @lo@, not @0@.
--   That means the boundary (and therefore every message before and
--   including the splice) is byte-for-byte identical across a whole
--   @(hi - lo + 1)@-turn stretch: for those turns, 'buildChapterMessages'
--   only ever *appends* to the message list a previous call already sent,
--   which is exactly the shape a provider's prefix cache can serve for
--   free. One turn in every @(hi - lo + 1)@ pays the reset; the rest are
--   free rides. @lo == hi@ degenerates to the old "always exactly N deep"
--   behaviour -- a full reset (and full miss on the recent side) every
--   turn, the least favourable point on this same spectrum, not a
--   different mechanism.
splitByTurnWindow :: Int -> Int -> [FileTick] -> ([FileTick], [FileTick])
splitByTurnWindow lo hi ticks
  | boundary == 0 = ([], ticks)
  | otherwise     = splitAt (promptIdxs !! boundary) ticks
  where
    promptIdxs = [ i | (i, ft) <- zip [0 :: Int ..] ticks, ftKind ft == "prompt" ]
    total      = length promptIdxs
    boundary   = turnWindowBoundary lo hi total

-- | The pure arithmetic 'splitByTurnWindow' turns into a tick index: how
--   many turns sit before the split, given @total@ turns exist and the
--   recent side must hold between @lo@ and @hi@. See 'splitByTurnWindow's
--   Haddock for the shape this produces (a step function, flat across each
--   @period@-turn stretch, jumping by @period@ at each reset).
turnWindowBoundary :: Int -> Int -> Int -> Int
turnWindowBoundary lo hi total
  | total <= lo = 0
  | otherwise   = total - (((total - lo) `mod` period) + lo)
  where period = hi - lo + 1

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
