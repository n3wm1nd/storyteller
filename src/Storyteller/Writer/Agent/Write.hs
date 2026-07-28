{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- __Gathers its own agent-owned context now__ (chapters, style, and who's
-- present plus their own summaries) rather than receiving them
-- pre-assembled -- see the project chat that settled this: @writeAgent@
-- decides both *where* each piece of content goes in the final message
-- sequence and *what counts* as each agent-owned piece, reading @path@ and
-- the branch directly for anything it can figure out on its own (earlier
-- chapters from the file's own chain, who's present from presence ticks,
-- their own context from their branches). 'Lore' and 'Other' stay real
-- parameters -- which lore\/"other" files are *relevant* to this call is a
-- judgment @writeAgent@ has no way to make on its own; a caller (typically
-- 'Server.Writer.File.chatWriter', resolving a client's own
-- @context.lore@\/@context.other@ override or the compiled-in default)
-- supplies each, already resolved. This is the same "agent does its own
-- reading" shape 'Storyteller.Writer.Agent.Continuation.proseAgent'
-- already has for the single-shot case; @writeAgent@ didn't need a
-- second, LLM-only core to share with it, since the two build genuinely
-- different message shapes (a reconstructed multi-turn chapter
-- conversation vs. one flattened trailing message) -- it just needed to
-- stop being handed its own gathering as parameters, and to stop
-- bundling 'Lore' together with agent-derived content into one anonymous
-- blob the way an earlier pass here did.
-- 'Server.Writer.File.chatWriter' correspondingly shrank to: stage a
-- lore\/other override if the caller sent one, resolve each, then call
-- this with @path@\/@lore@\/@other@\/@instruction@\/the other
-- caller-suppliable slots.
--
-- One thing the caller still has to get right for the cache-prefix
-- discipline below to actually hold, not enforced by this module's own
-- types: this turn's own prompt must be stored /after/ this call returns
-- -- otherwise the not-yet-answered prompt shows up twice, once via
-- 'historyFromFileTicks' (read internally now, at call time) and once as
-- 'buildChapterMessages'\'s own trailing instruction message. The
-- instruction message is literally @UserText instr@, the raw prompt text,
-- unwrapped -- deliberately identical to what 'historyFromFileTicks' will
-- later replay a @\"prompt\"@ tick as, so a turn's own final message is
-- already exactly what a later call reconstructs it to be. Per-turn
-- boilerplate ("write ~300 words", "only the new text") lives in the
-- system prompt instead ('chapterContinuationNote') -- it never varies, so
-- there's no reason to pay to resend it as a user message on every turn
-- when the provider already caches the system block.
module Storyteller.Writer.Agent.Write
  ( writeAgent
  , buildChapterMessages
  , flattenCharBlocks
  , activeCharacterContext
  ) where

import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import qualified Storage.Tick as Tick
import Storage.Tick (FileTick)

import Storyteller.Context.DSL.Rendering (renderContext, renderMessages, renderText)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.Branch (BranchOp, Branches, Visited, runStorage, withBranch)
import Storyteller.Core.Context (ContextStorage, resolveContext0, resolveContext1, runContextValue)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Writer.Agent
  ( Instruction(..), Prose(..), CharLabel(..), CharSummary(..), PastChaptersMode(..) )
import qualified Storyteller.Context.DSL.Value as DSL
import Storyteller.Context.DSL.Render (dslMessageToLLM)
import Storyteller.Writer.Agent.Context (Lore(..), Other(..), PinnedContext(..))
import Storyteller.Writer.Agent.Chat (historyFromFileTicks)
import Storyteller.Writer.Agent.MessageWindow (injectAtWindow)
import Storyteller.Writer.Agent.Continuation (defaultWriterSystemPrompt, defaultWriterConfig)
import Storyteller.Writer.Agent.Tasks (readTasksFile)
import Storyteller.Writer.Branches (branchDisplayName)
import Storyteller.Writer.Presence (activeCharactersFor)
import Storyteller.Writer.Types (Character(..))
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfig)

-- | Continue one chapter, given everything already gathered for it.
--
--   Always the 'Storyteller.Core.LLM.Role.ProseModel' role, same
--   @"agent.writer"@ prompt-storage key 'Storyteller.Writer.Agent.
--   Continuation.proseAgent' uses -- one persona, two shapes of call.
--
--   Message order (each a real list entry, not a section of one string):
--
--     1. World context -- lore, earlier chapters (each as a short
--        'UserText' naming the chapter's path, then its full current
--        prose as 'AssistantText' -- framed as something the assistant
--        already wrote, not reference material the user is presenting),
--        everything else -- already ordered and already built as real
--        messages by whatever assembled the caller's own context
--        (typically @'Storyteller.Context.DSL.Library.contextWriter'@,
--        composing @'Storyteller.Context.DSL.Library.contextChapters'@'s
--        own @> read f@ pairing -- see 'Storyteller.Context.DSL.AST.Expr'\'s
--        @EAssistant@ haddock) and spliced straight through unchanged.
--        This module no longer knows or cares which part of it is "lore"
--        versus "a chapter" -- that distinction lived here only because
--        this module used to reassemble the two from separate parameters;
--        now it's exactly one already-ordered stream. Resolved via
--        @context.chaptersWithout@\/@context.chaptersCompressedWithout@
--        (not the bare @context.chapters@\/@context.chaptersCompressed@),
--        excluding @path@ -- the chapter under active development belongs
--        in step 3's reconstructed conversation, never here as "earlier"
--        content; folding it in here would also change on every single
--        turn (this chapter's own prose keeps growing), breaking the
--        provider's cache prefix for everything after it in the sequence.
--     2. This chapter's "identity" block -- every active character's
--        'csSheet'\/'csContext', under a @"## Character: {name}"@ header
--        each (see 'flattenCharBlocks') -- mostly stable for the whole
--        chapter, so it sits once near its start.
--     3. This chapter's own conversation so far, reconstructed via
--        'historyFromFileTicks' -- alternating 'UserText' (what was
--        asked) and 'AssistantText' (what got written), oldest first, split
--        at a depth between 'recentWindowMin' and 'recentWindowMax' turns
--        from the end: the older side sits here, before the splice; the
--        recent side sits after it (see step 5). All of it, on one side or
--        the other, when the splice has nothing to say.
--     4. A shallow splice -- pinned\/short-term context plus every active
--        character's 'csJournal' excerpt -- one message, inserted mid-depth
--        rather than at either end (see 'Storyteller.Writer.Agent.
--        MessageWindow.injectAtWindow's Haddock), followed by a short
--        synthetic 'AssistantText' acknowledgment ('spliceAck'). Every
--        message the splice contributes is 'UserText', and whatever
--        follows it -- the next reconstructed turn's own 'UserText', or
--        step 6's instruction if the splice landed at the tail -- is
--        'UserText' too; without something 'Assistant'-roled between them
--        a provider has no turn boundary to key on and folds the two into
--        one blended user turn, silently losing the distinction between
--        "ambient journal context" and "what was actually asked". The ack
--        is what step 1's chapters get for free from their own
--        @> read f@ User\/Assistant pairing (see 'writeAgent's own
--        Haddock); the splice has no such natural bracket, so this
--        supplies one.
--     5. The recent tail of this chapter's conversation -- same source as
--        step 3, just the turns inside the depth window.
--     6. The new instruction -- always the last message, literally
--        @UserText instr@ now (no per-message boilerplate -- see
--        'chapterContinuationNote').
--
--   Every parameter here is one a *caller* can meaningfully supply: @path@
--   (which file), @lore@ (already-resolved @context.lore@ -- a caller like
--   'Server.Writer.File.chatWriter' stages any client override via
--   'Storyteller.Core.Context.setContextOverride' and resolves it before
--   calling this, the same "just data, not something this agent
--   assembles" contract 'PinnedContext' already had), @other@ ('Lore''s own
--   twin for @context.other@, identical resolution discipline), @pinned@
--   (the user's own explicit selection plus resolved pinned programs, also
--   already-resolved), @chaptersMode@ (the one structural toggle a
--   caller gets), and @instruction@. Everything else this function's own
--   Haddock lists above -- earlier chapters' content, style, which
--   characters are present and their own summaries -- is agent-owned:
--   resolved here, from @path@ and the branch, the same way
--   'Storyteller.Writer.Agent.Continuation.proseAgent' already reads
--   nothing it wasn't handed but needs no *caller* to have gathered any
--   of this either.
writeAgent
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, ContextStorage, BranchOp branch, Branches, Fail, Logging] r)
  => FilePath
  -> Lore                        -- ^ already resolved (branch override or compiled-in default; see 'Server.Writer.File.chatWriter')
  -> Other                       -- ^ 'Lore''s own twin for @context.other@, already resolved the same way
  -> PastChaptersMode            -- ^ full vs. compressed chapter framing
  -> PinnedContext                -- ^ pinned/short-term context: the user's own explicit selection plus resolved pinned programs
  -> Instruction
  -> Sem r Prose
writeAgent path (Lore lore) (Other other) chaptersMode (PinnedContext pinned) instruction = do
  Prompt sysPrompt <- getPrompt "agent.writer" defaultWriterSystemPrompt
  configs          <- getConfig "agent.writer" defaultWriterConfig

  chaptersV <- case chaptersMode of
    FullChapters       -> resolveContext1 @branch "context.chaptersWithout" (T.pack path)
    CompressedChapters -> resolveContext1 @branch "context.chaptersCompressedWithout" (T.pack path)
  styleV <- resolveContext0 @branch "context.style"
  (chapters, style) <- runContextValue @branch $ do
    c <- renderContext chaptersV
    s <- renderContext styleV
    pure (c, s)

  chars <- activeCharacterContext @branch path
  tasks <- tasksForActiveCharacters @branch path

  currentTicks <- runStorage @branch (Tick.fileTicksOf path)

  let contextMsgs      = map dslMessageToLLM lore ++ renderMessages chapters ++ map dslMessageToLLM other
      styleText        = renderText style
      pinnedBlocks      = pinned
      sysText           = T.intercalate "\n\n" (filter (not . T.null) [sysPrompt, styleText, chapterContinuationNote])
      configsWithPrompt = SystemPrompt sysText : configs
      messages          = buildChapterMessages contextMsgs chars tasks pinnedBlocks currentTicks instruction

  info "writeAgent: querying model..."
  response <- queryLLM configsWithPrompt messages
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | Every currently-active character's context, in the shape
--   'buildChapterMessages' already accepts -- presence ticks
--   ('Storyteller.Writer.Presence.activeCharactersFor') are the sole
--   source of truth for "who's in this scene". Resolves @context.character@
--   (a branch override on the @contexts@ branch, then
--   'Storyteller.Context.DSL.Library.contextCharacter' as fallback) per
--   active character, then reshapes it via
--   'Storyteller.Context.DSL.Library.characterSummaryOf' (curated
--   @"journal"@ bucket -- ambient generation wants the deduped slice, not
--   a present character's full self-knowledge).
activeCharacterContext
  :: forall branch r
  .  Members '[BranchOp branch, Branches, ContextStorage, Fail] r
  => FilePath -> Sem r [(CharLabel, CharSummary)]
activeCharacterContext path = do
  active <- activeCharactersFor @branch path
  mapM summarize active
  where
    summarize (Character (BranchName name)) = do
      let ident = branchDisplayName name
      charVal <- resolveContext1 @branch "context.character" ident
      summary <- runContextValue @branch (CtxLibrary.characterSummaryOf "journal" charVal)
      pure (CharLabel ident, summary)

-- | Every currently-active character's @tasks.md@, verbatim, as it stands
--   right now -- reading it straight off the character's own branch via
--   'withBranch'\/'readTasksFile' rather than through @context.character@\/
--   @resolveContext1@ like 'activeCharacterContext' does for sheet\/
--   context\/journal. Deliberately not DSL-routed: unlike a character's
--   summary (a project can override @context.character@ wholesale) or
--   even the journal curation inside it (still reached through that same
--   overridable name, even though the curation logic itself lives in
--   Haskell), there is no user-influenceable step here at all -- "read
--   this branch's own tasks.md" has nothing for an override to mean, so
--   it stays a direct effectful call rather than composing through a
--   layer whose whole purpose is exposing an override surface.
tasksForActiveCharacters
  :: forall branch r
  .  Members '[BranchOp branch, Branches, Fail] r
  => FilePath -> Sem r [(CharLabel, DSL.Message)]
tasksForActiveCharacters path = do
  active <- activeCharactersFor @branch path
  fmap concat . mapM tasksOf $ active
  where
    tasksOf (Character bname@(BranchName name)) = do
      let ident = branchDisplayName name
      mtasks <- withBranch @Visited bname (runStorage @Visited (readTasksFile "tasks.md"))
      pure [ (CharLabel ident, DSL.User t) | Just t <- [mtasks], not (T.null t) ]

-- | Standing per-turn instruction that used to be templated into every
--   single instruction message ('buildChapterMessages'\'s old
--   @"## Instruction..."@ wrapper). It never varies call to call, so it
--   belongs in the system prompt -- cached there by the provider once,
--   same as 'defaultWriterSystemPrompt' itself -- rather than being resent
--   (and re-priced, and re-breaking the instruction message's byte-identity
--   with its own later replay as history) on every single turn.
chapterContinuationNote :: T.Text
chapterContinuationNote =
  "Aim for roughly 300 words per turn, as a guideline rather than a hard \
  \limit. Write only the new text to append -- do not repeat or summarise \
  \existing content."

-- | The pure heart of 'writeAgent': everything about message order,
--   what's included, and what's dropped when empty, with no LLM effect
--   attached -- so the ordering this module's whole design turns on (see
--   'writeAgent's own Haddock for the numbered list) can be asserted
--   directly rather than only observed through a real model call.
buildChapterMessages
  :: forall m
  .  [Message m]                 -- ^ world context -- lore, earlier chapters, everything else, already ordered and already built as real messages -- see 'writeAgent's own Haddock: this used to be two separate parameters (flattened lore text, plus a pre-built earlier-chapters list) reassembled here; now it's whatever assembled the caller's own context (typically @'Storyteller.Context.DSL.Library.contextWriter'@) handed through as one already-ordered stream
  -> [(CharLabel, CharSummary)]  -- ^ every active character's summary
  -> [(CharLabel, DSL.Message)]  -- ^ every active character's current @tasks.md@, one message each (see 'tasksForActiveCharacters') -- deliberately not part of 'CharSummary': unlike sheet\/context\/journal, reading it has no overridable step for a 'CharSummary'-shaped resolution to sit behind
  -> [DSL.Message]               -- ^ pinned/short-term context
  -> [FileTick]                  -- ^ this chapter's own tick history so far, oldest-first
  -> Instruction
  -> [Message m]
buildChapterMessages context chars tasks pinned currentTicks (Instruction instr) =
  context ++ chapterStartMsgs ++ conversationMsgs ++ [instructionMsg]
  where
    -- 'flattenCharBlocks' always prepends a header per entry, so a
    -- character with nothing under 'csSheet'\/'csContext' has to be
    -- dropped here -- not passed through with an empty blocks list -- or
    -- it'd surface as a header with nothing under it.
    --
    -- Each block stays its own message rather than being joined into one
    -- string: the boundaries are what a provider caches on, and a block
    -- that arrived as 'DSL.Assistant' keeps that role instead of being
    -- flattened into user-role text.
    identityBlocks = flattenCharBlocks
      [ (label, blocks) | (label, cs) <- chars, let blocks = csSheet cs ++ csContext cs, not (null blocks) ]
    chapterStartMsgs = map dslMessageToLLM identityBlocks

    journalBlocks = flattenCharBlocks [ (label, csJournal cs) | (label, cs) <- chars, not (null (csJournal cs)) ]
    taskBlocks = flattenCharBlocks [ (label, [msg]) | (label, msg) <- tasks ]
    -- Journal and tasks are each their own bracketed block, in that
    -- order, rather than joined into one -- tasks land as their own
    -- embedded block right after the journal's (see 'tasksForActiveCharacters's
    -- own Haddock: a separate read, so it gets a separate splice entry),
    -- not folded into it, so an edit to one doesn't reshape the other's
    -- boundaries. 'spliceAck' only follows real content -- an empty
    -- section contributes nothing at all ('injectAtWindow''s own
    -- empty-input no-op covers the case where everything here is empty).
    ackIfNonEmpty msgs = if null msgs then [] else map dslMessageToLLM msgs ++ [spliceAck]
    spliceMsgs = ackIfNonEmpty pinned ++ ackIfNonEmpty journalBlocks ++ ackIfNonEmpty taskBlocks

    -- The splice sits at a depth inside the reconstructed conversation
    -- rather than at either end -- see 'Storyteller.Writer.Agent.
    -- MessageWindow.injectAtWindow's own Haddock for why. 'isUserTurn'
    -- marks a turn boundary the way 'historyFromFileTicks' actually
    -- produces one: every @"prompt"@ tick becomes exactly one leading
    -- 'UserText'.
    conversationMsgs =
      injectAtWindow isUserTurn recentWindowMin recentWindowMax spliceMsgs
        (historyFromFileTicks currentTicks)

    instructionMsg = UserText instr

isUserTurn :: Message m -> Bool
isUserTurn (UserText _) = True
isUserTurn _            = False

-- | Closes the splice with an 'Assistant'-roled turn so it never sits
--   directly against the 'UserText' that follows it -- either the next
--   reconstructed turn (mid-history) or 'instructionMsg' itself (tail) --
--   see 'buildChapterMessages'\'s own Haddock, step 4, for why an
--   unbracketed splice would otherwise blend into that message.
spliceAck :: Message m
spliceAck = AssistantText "Noted."

-- | The splice's depth window: at least @recentWindowMin@, at most
--   @recentWindowMax@ turns stay after it. Kept as a real range rather than
--   a single fixed depth (that's just the @min == max@ special case) so the
--   injection point can hold /still/ across a stretch of turns instead of
--   moving on every single one -- see 'Storyteller.Writer.Agent.
--   MessageWindow.injectAtWindow's Haddock for why that's what actually
--   buys back cache hits, not just bounds the miss.
recentWindowMin, recentWindowMax :: Int
recentWindowMin = 2
recentWindowMax = 4

-- | @(label, resolved blocks)@ per active character branch, flattened into
--   the plain message list a rendered call actually takes --
--   each branch's blocks preceded by a @"## Character: {name}"@ header
--   block. Shared with 'Storyteller.Writer.Agent.Outline''s reconciliation
--   calls (which still take this same flattened shape directly) and with
--   both 'writeAgent's own identity and journal splits above.
flattenCharBlocks :: [(CharLabel, [DSL.Message])] -> [DSL.Message]
flattenCharBlocks = concatMap
  (\(CharLabel name, blocks) -> DSL.User ("## Character: " <> name) : blocks)
