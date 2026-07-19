{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | File-level business logic specific to the Writer application: this is
-- the seam between routing ('Server.Writer.File.Dispatch') and the
-- Writer/FlowWriter/Fixer prose agents, which are themselves effect-minimal
-- (LLM-only where possible). Gathering file/character context, appending
-- generated prose, and choosing Writer vs. Fixer when there are no targets
-- all happen here rather than inside the agents — same shape as
-- 'Server.Core.File': plain 'Sem' functions, no JSON/WebSocket.
module Server.Writer.File
  ( chatWriter
  , roleplayWriter
  , chatFixer
  , chatConverse
  , chatConverseSwipe
  , editChatPrompt
  , chatChapterRegen
  , chatSplitOutline
  , RegenMode(..)
  , setPresence
  , askCharacter
  , correctGroup
  , fileStateWithSummaries
  , summarizePath
  , summarizePathManual
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (isSuffixOf)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (info, Logging)
import Runix.FileSystem (fileExists, readFile, writeFile)

import Server.Core.File (FileOpen, deleteFileTicks)
import qualified Server.Core.File as Core (fileStateSince)
import Server.Core.Run (SessionEffects)
import Server.Core.Protocol (WireTick(..), Update(..))
import Server.Writer.File.Protocol (ContextItem(..))

import UniversalLLM (Message(..))

import Storyteller.Writer.Agent (Prompt(..), Instruction(..), ContextBlock(..), Prose(..), CharLabel(..), CharSummary, flattenCharSummary, WordCount(..), renderEmbeddedFile)
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.ContextFilter (ContextLayout, hideBinaryFiles, hideChapters, hideLore, classifyPath)
import Storyteller.Writer.Library (journalPath)
import qualified Storyteller.Writer.Library as Library (LibraryKind(..), classifyPath)
import Storyteller.Writer.Lore (isLoreEligible)
import Storyteller.Writer.Agent.JournalSummarizer (journalKind, journalSummarize, journalCreateManual, journalChunkAgent, currentSheet)
import Storyteller.Writer.Agent.ChapterSummarizer (chapterSummaryAgent)
import Storyteller.Writer.Agent.LoreSummarizer (loreSummaryAgent)
import Storyteller.Writer.Agent.Summarizer (runSummarizerForPath)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Storage (StoryStorage)
import qualified Storyteller.Writer.Agent.SummaryAccess as SummaryAccess
import qualified Storyteller.Common.Summary as Summary
import Storyteller.Writer.Agent.Chat (chatAgent, historyFromFileTicks)
import Storyteller.Writer.Agent.CharContext (charSummaryWithJournal, charSummaryFull)
import qualified Storyteller.Writer.Agent.WorldContext as WorldContext
import qualified Storyteller.Writer.Agent.ChapterContext as ChapterContext
import Storyteller.Writer.Agent.AskCharacter (askCharacterAgent)
import Storyteller.Writer.Agent.Write (writeAgent, flattenCharBlocks)
import Storyteller.Writer.Agent.Roleplay (roleplayAgent, characterReflectAgent)
import Storyteller.Writer.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Writer.Agent.Fix (fixAgent)
import Storyteller.Writer.Agent.Outline
  ( BeatSheet(..), CurrentProse(..), OutlineDoc(..), ChapterBeats(..)
  , reconcileChapter, reconcileChapterByBeat, splitOutlineFreeform )
import Storyteller.Writer.Branches (branchDisplayName)
import Storyteller.Writer.Presence (recordPresence, activeCharactersFor)
import Storyteller.Writer.Types (Character(..), CharacterAnswer(..), PresenceEvent)
import Storyteller.Core.Runtime (Main)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import qualified Storyteller.Common.Swipe as Swipe
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick, tickParent)
import Storyteller.Core.Git (BranchOp, BranchTag, runBranchAndFS, runBranchOpGit, runStorage, atGeneric)

import Prelude hiding (readFile, writeFile)

-- | A phantom tag for opening one active character branch's filesystem at
--   a time, dynamically -- same role 'Server.Writer.Branch.CharBranch'
--   plays there, just local to this module since nothing outside it needs
--   to name the tag.
data ActiveChar

-- | Every currently-active character's context, in the shape 'writeAgent'\/
--   'flowWriteAgent' already accept -- presence ticks
--   ('Storyteller.Writer.Presence.activeCharactersFor') are the sole source
--   of truth for "who's in this scene"; there is no separate client-supplied
--   signal, so this is the one place that decides which character branches
--   an agent sees. Each active branch is opened dynamically (same pattern
--   'Server.Writer.Branch.charGen'\/'trackFiles' use for a runtime-named
--   branch, minus the 'FileSystem' effects neither this nor
--   'charSummaryWithJournal' needs) and summarized via
--   'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal' -- one
--   'runStorage' dispatch per character, not two.
--
--   @sheet.md@ and @journal.md@ are never lore-gated by @charLayouts@ --
--   both are excluded from 'Storyteller.Writer.Lore.isLoreEligible' (so
--   they never even appear as a codex entry a user could toggle), and this
--   function enforces the same two facts unconditionally at the read
--   layer, independent of whatever a user has curated for everything else:
--   the sheet is core identity, always sent verbatim; the journal is long,
--   mostly a copy of what the scene's own history already says, and not
--   written for a narrator to read, so the plain read always excludes it
--   -- but a curated slice of only its *unique* recent content (see
--   'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal' and
--   'Storage.Tick.recentAtomsOf') is folded back in, separately labelled as
--   the character's own viewpoint. Full, uncurated journal access is still
--   available on request, via
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent' -- the
--   sidebar's Ask panel. Everything else on the branch is real codex
--   content: an absent or empty entry in @charLayouts@ for a branch means
--   "no override configured", which reads as "show everything" -- the same
--   convention 'Storyteller.Writer.Agent.Continuation.gatherFileContext'
--   already uses for an empty layout, and exactly today's behavior for
--   anyone who has never opened the per-character context UI. A non-empty
--   entry is fully authoritative over that branch's extra files, via
--   'Storyteller.Writer.Agent.ContextFilter.classifyPath' -- the identical
--   picker semantics 'chatWriter's own @layout@ parameter already applies
--   to the story branch itself, through 'gatherFileContext'.
--   Returns the full 'CharSummary' split per character, not a flattened
--   list -- 'chatWriter', still a single-shot prompt, collapses it back via
--   'flattenCharSummary' at its own call site; a future per-chapter
--   '[Message]' assembly (see 'Storyteller.Writer.Agent.CharSummary's own
--   Haddock) is what actually wants 'csSheet'\/'csContext'\/'csJournal'
--   placed independently, and can consume this function's result directly.
activeCharacterContext :: (FileOpen r, SessionEffects r) => Map.Map T.Text ContextLayout -> FilePath -> Sem r [(CharLabel, CharSummary)]
activeCharacterContext charLayouts path = do
  active <- activeCharactersFor @Main path
  mapM summarize active
  where
    summarize (Character (BranchName name)) = do
      let layout = fromMaybe [] (Map.lookup name charLayouts)
          keep p
            | null layout = True
            | otherwise   = isJust (classifyPath layout p)
      summary <- runBranchOpGit @ActiveChar (BranchName name) $
        runStorage @ActiveChar (charSummaryWithJournal "sheet.md" journalPath keep journalLookback journalMaxOut journalPadding)
      let label = branchDisplayName name
      pure (CharLabel label, summary)

-- | Bounds for the curated journal slice 'activeCharacterContext' folds
--   back into ambient context -- see 'Storyteller.Writer.Agent.CharContext.
--   charSummaryWithJournal'\/'Storage.Tick.recentAtomsOf' for what each
--   knob actually does.
journalLookback, journalMaxOut, journalPadding :: Int
journalLookback = 30
journalMaxOut   = 10
journalPadding  = 2

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Writer.Agent.FlowWrite'). Context (world lore, earlier
--   chapters, character summaries, pinned/short-term context, and this
--   chapter's own tick history) is gathered here and handed to the agents
--   as plain data -- see 'Storyteller.Writer.Agent.Write.writeAgent' for
--   how it turns into a real @['UniversalLLM.Message']@ rather than one
--   flattened string. Agents append nothing themselves, so appending the
--   result is done here too.
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> ContextLayout -> Maybe TickId -> Map.Map T.Text ContextLayout -> Sem r ()
chatWriter path prompt context layout mFlowTid charLayouts = do
  (_existing, fileCtx) <- hideLore @(BranchTag Main) (hideChapters @(BranchTag Main) (hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) layout path)))
  (loreBlocks, styleBlocks, earlierChapters) <- runStorage @Main $ do
    (WorldContext.WorldLore lore, WorldContext.SystemContext style) <- WorldContext.worldContextOf
    earlier <- ChapterContext.earlierChaptersOf path
    return (lore, style, earlier)
  charBlocks <- activeCharacterContext charLayouts path
  let pinned      = toContextBlocks context <> fileCtx
      instruction = Instruction prompt
      -- Storing this turn's prompt tick has to wait until every branch
      -- below has already read whatever tick history it needs -- both
      -- 'writeAgent's own 'currentTicks' fetch and 'flowWriteAgent's
      -- internal one ('Storyteller.Writer.Agent.FlowWrite.flowWriteAgent')
      -- -- otherwise the not-yet-answered prompt shows up twice: once via
      -- that history, once as 'Storyteller.Writer.Agent.Write.
      -- buildChapterMessages'\'s own trailing instruction message, which
      -- also permanently breaks that turn's cache-prefix match against
      -- whatever a later turn reconstructs as history (see
      -- 'Storyteller.Writer.Agent.Write''s module Haddock). Same
      -- read-before-store discipline 'chatConverse' already follows.
      storePrompt = runStorage @Main (Tick.storeAs (Prompt path prompt))
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      (_reworked, Prose generated) <- flowWriteAgent @Main path flowTid loreBlocks styleBlocks charBlocks pinned earlierChapters instruction
      _ <- storePrompt
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      currentTicks <- runStorage @Main (Tick.fileTicksOf path)
      Prose generated <- writeAgent loreBlocks styleBlocks charBlocks pinned earlierChapters currentTicks instruction
      _ <- storePrompt
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "writer agent done: " <> T.pack path

-- | The roleplay writer: rather than one call producing the scene directly
--   (the shape 'chatWriter' uses), every character present is interrogated
--   for what they'd do or say (via 'Storyteller.Writer.Agent.Roleplay.
--   roleplayAgent''s own @ask_character@ tool loop) before one coherent
--   scene gets written and appended -- see that module's Haddock for the
--   two-tier design. Presence ('activeCharactersFor') is, same as
--   'activeCharacterContext', the sole source of truth for who's "in this
--   scene" and thus askable; there is no separate client-supplied roster.
--
--   Once the scene lands, every present character -- whether or not the
--   orchestrator ever asked them anything -- gets one post-scene
--   'Storyteller.Writer.Agent.Roleplay.characterReflectAgent' pass: their
--   own account of what just happened, filtered to what they could
--   plausibly perceive, appended to their own @journal.md@ with a ref back
--   to the scene atom it's about (same cross-branch-ref convention
--   'Storyteller.Writer.Agent.Tracker.trackBranch' already uses). A scene
--   that produced no atoms at all (an empty generation) has nothing for
--   anyone to react to, so the reflection pass is skipped entirely rather
--   than running against nothing.
roleplayWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> Sem r ()
roleplayWriter path prompt = do
  (_existing, fileCtx) <- hideLore @(BranchTag Main) (hideChapters @(BranchTag Main) (hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) [] path)))
  (loreBlocks, earlierBlocks) <- runStorage @Main $ do
    (WorldContext.WorldLore lore, _style) <- WorldContext.worldContextOf
    earlier <- ChapterContext.earlierChaptersOf path
    return (lore, [ ContextBlock (renderEmbeddedFile p t) | (p, t) <- earlier ])
  active <- activeCharactersFor @Main path
  let characters    = [ (CharLabel (characterLabel c), c) | c <- active ]
      sceneContext  = loreBlocks <> earlierBlocks <> fileCtx
  info $ "roleplay agent starting: " <> T.pack path
  Prose narrative <- roleplayAgent sceneContext characters prompt
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  sceneRefs <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms narrative
  info $ "roleplay agent done: " <> T.pack path
  case sceneRefs of
    [] -> pure ()
    _  -> mapM_ (reflectFor narrative (last sceneRefs)) active
  where
    characterLabel (Character (BranchName name)) = branchDisplayName name

      -- Full, uncurated branch context (sheet, whole journal, notes.md if
      -- any) -- not the windowed 'charSummaryWithJournal' slice
      -- 'activeCharacterContext' uses for ambient generation context.
      -- Reflecting on a scene just witnessed needs everything this
      -- character actually knows going in, the same reasoning
      -- 'Storyteller.Writer.Agent.Roleplay.askCharacterImpl' already
      -- applies to interrogation. One scope covers the whole pass --
      -- context read, 'characterReflectAgent''s own tool calls (it may
      -- update characters/*.md or add a thought based on what actually
      -- happened, not just what was planned), and the final ref-carrying
      -- journal commit -- since all three need this same branch's effects
      -- live at once.
    reflectFor narrative sceneRef character@(Character branch) =
      runBranchAndFS @ActiveChar branch $ do
        ownContext <- charSummaryFull @(BranchTag ActiveChar) (const True)
        entry <- characterReflectAgent @(BranchTag ActiveChar) (characterLabel character) ownContext narrative
        void $ runStorage @ActiveChar (Ops.addAtomWithRefs [sceneRef] journalPath entry)

-- | "Correct this": delete an instruction group -- 'promptTid' (a
--   'Prompt' tick) and every atom it produced ('targets') -- then
--   regenerate from 'prompt' via 'chatWriter', landing back in the same
--   position, all as one transaction (the caller's own 'withStorage', see
--   'Server.Writer.File.Dispatch').
--
--   The regeneration is rebased at 'promptTid''s *parent*, not
--   'promptTid' itself: 'promptTid' is one of the ticks this same call
--   deletes, so by the time 'atGeneric' would try to resolve it as a
--   pivot it no longer exists anywhere in the chain (a plain delete drops
--   a tick rather than replacing it -- see 'Storage.Ops.deleteTick' --
--   so it never gains a remap entry pointing anywhere). The parent is
--   read up front, before either delete runs, so this doesn't depend on
--   deletion order the way resolving it afterward would.
--
--   Deleting first (rather than letting 'atGeneric' pop the group as part
--   of its own tail) is what keeps the group *out* of the tail it
--   replays back on top of the fresh generation -- see 'atGeneric's own
--   Haddock: popped ticks are replayed verbatim, so anything still
--   present when it starts winding back would simply reappear.
correctGroup :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> TickId -> [TickId] -> T.Text -> [ContextItem] -> ContextLayout -> Map.Map T.Text ContextLayout -> Sem r ()
correctGroup path promptTid targets prompt context layout charLayouts = do
  typed <- runStorage @Main (Tick.readTypesTick (Ops.ObjectHash (unTickId promptTid)))
  case tickParent typed of
    Nothing -> fail "correctGroup: prompt tick has no parent to rebase onto"
    Just parentTid -> do
      deleteFileTicks (promptTid : targets)
      atGeneric @Main parentTid (chatWriter path prompt context layout Nothing charLayouts)

-- | Store a prompt tick then run the Fixer agent against the given targets.
--   With no targets, there's nothing to rework — that's a different policy
--   ("just write") from the Fixer's, so it's handled here as a fall through
--   to the same Writer path 'chatWriter' takes, rather than living inside
--   'Storyteller.Writer.Agent.Fix.fixAgent'.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context [] = chatWriter path prompt context [] Nothing Map.empty
chatFixer path prompt _context targets = do
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  info $ "fixer agent starting: " <> T.pack path
  _ <- fixAgent @Main path targets (Instruction prompt)
  info $ "fixer agent done: " <> T.pack path

-- | Discuss, don't write: run the chat agent against this file's own
--   conversation history (its 'Prompt'/'Atom' ticks, oldest first — see
--   'historyFromFileTicks'), then store the new message and append the
--   reply as a single atom. No splitter — a chat turn is one atom, unlike
--   generated prose.
--
--   No context is gathered up front — the agent sees only the conversation
--   and reaches for other branch files itself, via tool calls, if it needs
--   to (see 'Storyteller.Writer.Agent.Chat').
--
--   'chatAgent' itself doesn't know about "chat turns" or replies — it just
--   hands back every message it produced servicing this call, tool calls
--   and results included (see its own Haddock). This is where that gets
--   turned into what a chat file actually is: only the 'AssistantText'
--   pieces get concatenated into the atom, same as before — the tool
--   exploration stays out of the persisted chain.
--
--   History is read before the new prompt tick is stored, so it never
--   includes the message currently being answered.
chatConverse :: (FileOpen r, SessionEffects r) => FilePath -> T.Text -> Sem r ()
chatConverse path prompt = do
  ticks <- runStorage @Main (Tick.fileTicksOf path)
  let history = historyFromFileTicks ticks
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  info $ "chat agent starting: " <> T.pack path
  added <- chatAgent @(BranchTag Main) (history ++ [UserText prompt])
  let reply = mconcat [t | AssistantText t <- added]
  _ <- runStorage @Main (Ops.append path reply)
  info $ "chat agent done: " <> T.pack path

-- | Regenerate the reply to a chat exchange, keeping the old reply as a
--   swipe (a cycle-able alternate, see 'Storyteller.Common.Swipe') instead
--   of discarding it — unlike 'chatConverse', this never stores a new
--   'Prompt' tick: the prompt is edited in place ('editChatPrompt', a
--   no-op rebase when the text is unchanged, which is the common case —
--   the plain Regenerate button always resends the current text) and the
--   history handed to the agent is everything strictly before it, so
--   re-running the same prompt (or an edited one) reproduces exactly what
--   'chatConverse' would have seen the first time.
--
--   Only ever meaningful for the *last* exchange — regenerating an
--   earlier one would leave a later reply answering a prompt that no
--   longer matches what's above it. Enforcing that is the caller's job
--   (see 'frontend/src/app/fileview.actions.ts''s 'chatConverseRegen').
--
--   History is read *before* 'editChatPrompt' runs, same discipline as
--   'chatConverse' ("History is read before the new prompt tick is
--   stored, so it never includes the message currently being answered") —
--   'editChatPrompt' rebases the prompt tick, giving it a new id, so
--   finding the history boundary by @promptTid@ only works against the
--   tick list fetched before that rebase; fetching it after would never
--   find a match (the id it's searching for is already gone) and silently
--   fall back to "everything", including the very reply this call exists
--   to replace.
chatConverseSwipe :: (FileOpen r, SessionEffects r) => FilePath -> TickId -> TickId -> T.Text -> Sem r ()
chatConverseSwipe path promptTid atomTid newPromptText = do
  ticks <- runStorage @Main (Tick.fileTicksOf path)
  let (before, _fromPrompt) = span ((/= unTickId promptTid) . Tick.ftTickId) ticks
      history = historyFromFileTicks before
  editChatPrompt promptTid newPromptText
  info $ "chat agent regenerating (swipe): " <> T.pack path
  added <- chatAgent @(BranchTag Main) (history ++ [UserText newPromptText])
  let reply = mconcat [t | AssistantText t <- added]
  _ <- runStorage @Main (Swipe.pushSwipe (Ops.ObjectHash (unTickId atomTid)) reply)
  info $ "chat agent regen (swipe) done: " <> T.pack path

-- | Edit a chat prompt's text in place. A 'Prompt' is not an atom — its
--   message carries no filesystem footprint -- so this edits the raw
--   'NonAtom' message directly rather than going through 'editFileAtom'.
--
--   Goes through 'Prompt's own 'TickType' instance to rebuild the message —
--   decode the current one back into a 'Prompt' (recovering its "file"
--   field), substitute the new text, then re-encode it fresh via
--   'Tick.editTickAs' — rather than hand-editing the raw tagged string in
--   place. That used to seem like the smaller change (find the tag, keep
--   it, splice in new text) but "the tag" isn't reliably at a fixed
--   offset: whether it's on the first line at all depends on whether the
--   tick carries fields (a 'Prompt' always does, via its own "file"
--   field), so a fixed-offset splice silently corrupted the header the
--   moment that assumption didn't hold — dropping the "type:prompt" tag
--   entirely and leaving the tick undecodable as anything but "unknown"
--   kind ever after. Round-tripping through the same encode\/decode the
--   tick kind's own 'TickType' instance already defines doesn't have that
--   failure mode: whatever it puts in the header is exactly what it'll
--   expect back out -- and 'Tick.editTickAs' is what makes that the only
--   way to write it, never a hand-rolled 'Storage.Core.NonAtom' with
--   already-encoded text spliced in by the caller.
editChatPrompt :: FileOpen r => TickId -> T.Text -> Sem r ()
editChatPrompt (TickId tid) content = do
  typed <- runStorage @Main (Tick.readTypesTick (Ops.ObjectHash tid))
  case fromTick @Prompt typed of
    Nothing -> fail "editChatPrompt: not a chat prompt"
    Just (Prompt file _) ->
      void $ runStorage @Main (Tick.editTickAs (Ops.ObjectHash tid) (Prompt file content))

-- | Which reconciliation driver 'chatChapterRegen' runs — the whole-chapter
--   single call or the beat-by-beat loop (see
--   'Storyteller.Writer.Agent.Outline'). Chosen by the client per command.
data RegenMode = RegenWhole | RegenByBeat
  deriving (Show, Eq)

-- | Regenerate a chapter to fit its beat sheet, respecting the user's steer.
--
--   A /reconciliation/, not a rewrite-from-scratch: the current chapter prose
--   is fed to the agent as reference (preserve what works, fix what
--   contradicts the outline), together with the beat sheet and the user's
--   instruction. The new chapter is written into the working tree and
--   reconciled against the atom chain with 'commitFiles', so prose that
--   didn't need to change keeps its atom ids and non-atom ticks (presence,
--   notes) are left untouched — only genuinely changed atoms get rewritten.
--
--   The beat sheet path is derived from the chapter path by the WRITER.md
--   convention (@chapters/ch{N}.md@ → @chapters/ch{N}.outline.md@); a missing
--   beat sheet is a clear failure, not a silent no-op.
chatChapterRegen :: (FileOpen r, SessionEffects r) => RegenMode -> FilePath -> T.Text -> [ContextItem] -> Sem r ()
chatChapterRegen mode path prompt context = do
  let sheetPath = beatSheetPathFor path
  haveSheet <- fileExists @(BranchTag Main) sheetPath
  if not haveSheet
    then fail ("no beat sheet at " <> sheetPath <> " — generate one before regenerating from outline")
    else do
      _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
      sheet   <- BeatSheet . TE.decodeUtf8 <$> readFile @(BranchTag Main) sheetPath
      current <- CurrentProse . TE.decodeUtf8 <$> readFile @(BranchTag Main) path
      (_, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) [] path)
      charBlocks <- flattenCharBlocks . map (fmap flattenCharSummary) <$> activeCharacterContext Map.empty path
      let extraContext = toContextBlocks context <> fileCtx
          instruction  = Instruction prompt
      info $ "chapter regen (" <> T.pack (show mode) <> ") starting: " <> T.pack path
      Prose regenerated <- case mode of
        RegenWhole  -> reconcileChapter (Just (WordCount 1200))
                         charBlocks extraContext current instruction sheet
        RegenByBeat -> reconcileChapterByBeat (Just (WordCount 300))
                         charBlocks extraContext current instruction sheet maxBeats
      -- Overwrite the file in the working tree, then reconcile against the
      -- chain: unchanged atoms keep their ids, changed atoms replace in place,
      -- removed prose drops, new prose appends. commitFiles broadcasts its own
      -- ref mapping, so nothing to push explicitly here.
      writeFile @(BranchTag Main) path (TE.encodeUtf8 regenerated)
      _ <- runStorage @Main (Ops.commitFiles [path])
      info $ "chapter regen done: " <> T.pack path
  where
    maxBeats = 40 :: Int

-- | Split a whole-story outline (this file, by convention @outline.md@) into
--   per-chapter beat sheets. The model decides the chapter breakdown; the
--   output paths (@chapters/ch1.outline.md@, …) are assigned by
--   'splitOutlineFreeform' itself, one per chapter in reading order, and
--   each is written as its own file (atomized by the splitter, same as
--   generated prose).
--
--   Unlike the tool-call-driven 'splitOutlineAgent' this used to call, this
--   is /not/ safe to re-run on a story that's already partially split:
--   'splitOutlineFreeform' always starts from chapter 1 and has no
--   "skip chapters that already have a beat sheet" logic (see its Haddock
--   and @../PLAN.md@ in the agent-integration suite for why the tool-call
--   loop it replaces here still exists, unremoved, for exactly that
--   incremental-fill use case). Chosen for this handler anyway because it's
--   substantially more reliable at getting the chapter breakdown right in
--   the first place — see @FINDINGS.md@ in the same suite for the measured
--   comparison.
chatSplitOutline :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> Sem r ()
chatSplitOutline path = do
  outline <- OutlineDoc . TE.decodeUtf8 <$> readFile @(BranchTag Main) path
  (_, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) [] path)
  info $ "outline split starting: " <> T.pack path
  sheets <- splitOutlineFreeform fileCtx outline
  mapM_ writeSheet sheets
  info $ "outline split done: " <> T.pack path <> " (" <> T.pack (show (length sheets)) <> " chapters)"
  where
    writeSheet (ChapterBeats sheetPath (BeatSheet body)) = do
      info $ "  beat sheet: " <> T.pack sheetPath
      mapM_ (\c -> runStorage @Main (Ops.append sheetPath c)) =<< splitAtoms body

-- | @chapters/ch1.md@ → @chapters/ch1.outline.md@; any @.md@ path → its
--   @.outline.md@ sibling (see WRITER.md). A path without @.md@ just gets the
--   suffix appended, which will then fail the existence check with a clear
--   message rather than silently mis-targeting.
beatSheetPathFor :: FilePath -> FilePath
beatSheetPathFor path
  | ".md" `isSuffixOf` path = take (length path - length (".md" :: String)) path <> ".outline.md"
  | otherwise               = path <> ".outline.md"

toContextBlocks :: [ContextItem] -> [ContextBlock]
toContextBlocks = map (ContextBlock . ciContent)

-- | Record a character entering or leaving the scene on @path@ — presence
--   is scoped to the file (the scene), not the whole branch, see
--   'Storyteller.Writer.Types.Presence' and WRITER.md.
setPresence :: FileOpen r => FilePath -> Character -> PresenceEvent -> Sem r ()
setPresence path character event = void $ recordPresence @Main path character event

-- | Ask @character@ a question, answered from only their own branch (see
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent') -- not the
--   scene, not any other character. 'askCharacterAgent' itself only reads
--   and answers; it stores nothing, same as any other agent here
--   ('proseAgent', 'chatAgent', ...) -- this function, the caller, is where
--   the result gets recorded, and it goes on @Main@ (the scene's own
--   chain), not the character's: asking a question is something that
--   happened during *this* writing, not a new memory for the character --
--   see 'Storyteller.Writer.Types.CharacterAnswer'.
askCharacter :: (FileOpen r, SessionEffects r) => FilePath -> Character -> T.Text -> Sem r T.Text
askCharacter path character@(Character branch) question = do
  answer <- runBranchAndFS @ActiveChar branch (askCharacterAgent @(BranchTag ActiveChar) question)
  _ <- runStorage @Main (Tick.storeAs (CharacterAnswer character question answer (Just path)))
  return answer

-- | Which summary kinds, if any, apply to @path@ -- a static,
--   server-authoritative fact of this app's own file conventions,
--   independently mirrored client-side for the UI toggle (see
--   WS-PROTOCOL.md's "Backend-authoritative vs. frontend-advisory
--   duplication": something server-side acts on this, namely this very
--   function deciding what to push, so the server's copy has to be
--   authoritative regardless of what the client assumes). Same three-way
--   split 'Server.Writer.Branch.summarize's kind dispatch already uses.
--   Each entry is one independent family's own plain kind label -- not a
--   pre-enumerated tier list: a recursive family like @journal@ shares one
--   label across however many tiers it actually has, depth being a
--   structural fact discovered by walking (see
--   'Storyteller.Common.Summary.summariesTouching', called again from a
--   nested connection for a deeper tier), never declared here.
summaryKindsFor :: FilePath -> [T.Text]
summaryKindsFor path
  | Library.classifyPath path == Library.Unit = ["prose/chapter"]
  | path == journalPath                       = [journalKind]
  | isLoreEligible path                        = ["lore/article"]
  | otherwise                                  = []

-- | Summarize exactly @path@ -- unlike 'Server.Writer.Branch.summarize'
--   (which runs a whole @kind@ across the branch, regenerating every
--   file of it that happens to be stale), this touches only @path@
--   itself. There is no guarantee every other stale file of the same
--   kind gets updated by this call, only @path@ -- summarizing one
--   chapter by hand must never force every other chapter through a pass
--   it was never asked for (see
--   'Storyteller.Writer.Agent.Summarizer.runSummarizerForPath's own
--   Haddock for the full argument, including why the new 'Summary' tick
--   has to be positioned at @path@'s own last atom rather than wherever
--   the branch's head currently sits).
--
--   Journal is the one kind this doesn't need a file-scoped path for at
--   all: @journal.md@ is already the only file 'journalSummarize' ever
--   touches, so it's dispatched unchanged. @"prose/chapter"@ and
--   @"lore/article"@ route through 'runSummarizerForPath' with their
--   existing single-file agents ('chapterSummaryAgent'\/'loreSummaryAgent')
--   directly as the generation hook -- both already take "one file's
--   current content in, its compression out", exactly the shape
--   'runSummarizerForPath' wants.
summarizePath
  :: (LLMs r, Members '[BranchOp Main, Git, StoryStorage, PromptStorage, Logging, Fail] r)
  => FilePath -> Sem r (Maybe TickId)
summarizePath path = case summaryKindsFor path of
  [] -> return Nothing
  (kind : _)
    | path == journalPath      -> do
        sheet <- currentSheet @Main
        Nothing <$ journalSummarize @Main (journalChunkAgent sheet)
    | kind == "prose/chapter"  -> runSummarizerForPath @Main kind path chapterSummaryAgent
    | kind == "lore/article"   -> runSummarizerForPath @Main kind path loreSummaryAgent
    | otherwise                 -> return Nothing

-- | Manual creation: an empty occurrence, positioned exactly where an
--   automatic 'summarizePath' pass would have landed one (its own
--   coverage-finding machinery, unchanged -- 'runSummarizerForPath's
--   per-path freshness check for a whole-file kind,
--   'journalCreateManual's own forced flush for the incremental one), but
--   with no LLM call: the generated content is always @""@, for the user
--   to write into directly via the split view. Routed through the
--   identical 'Server.Writer.File.Protocol.At' wrapping every mutating
--   command already gets, so "at the current cursor" needs nothing of its
--   own here -- see WS-PROTOCOL.md and 'Server.Writer.File.Dispatch.
--   runCommand's own @At@ case.
--
--   Collapses 'summarizePath's own three-way kind dispatch to two: which
--   per-domain LLM agent to call is the only thing that ever
--   distinguished @"prose/chapter"@ from @"lore/article"@, and manual
--   creation calls none of them -- both go through the identical
--   const-empty generation hook.
summarizePathManual
  :: Members '[BranchOp Main, Git, StoryStorage, Logging, Fail] r
  => FilePath -> Sem r (Maybe TickId)
summarizePathManual path = case summaryKindsFor path of
  [] -> return Nothing
  (kind : _)
    | path == journalPath -> Nothing <$ journalCreateManual @Main
    | otherwise            -> runSummarizerForPath @Main kind path (const (pure ""))

-- | @path@'s summary history, as synthetic 'WireTick's riding along in
--   the ordinary push -- not a request/response command. A
--   'Storyteller.Common.Summary.Summary' carries no @file@ field (one
--   alternate-chain commit can cover many files in a single pass), so it
--   never appears in this file's own 'Server.Core.File.fileState' the way
--   a real atom does; this is what makes it visible there instead.
--
--   'fileStateWithSummaries' folds the returned ticks' own ids into a
--   signature 'Server.Writer.File.Connection.pushIncremental' compares
--   alongside the raw atom head, so a summarize pass -- which advances
--   the *branch's* head without ever touching this file's own atom head
--   -- still triggers a fresh push to an already-open connection; seeing
--   this only on reconnect would be exactly the "not implementing the WS
--   interface correctly" gap WS-PROTOCOL.md's push-everything model rules
--   out.
--
--   One real 'WireTick' per historical occurrence, not one synthetic tick
--   per family: each occurrence is anchored via 'wtRefs' (exactly
--   @[anchor]@ -- the last real atom on @path@ it covers) and carries its
--   own two boundaries as plain fields (@lowerBound@: the previous
--   occurrence's anchor; @prevAltHead@: the previous occurrence's
--   alt-chain tip -- each absent when there is no older occurrence), so a
--   client can render every pass as its own inline annotation positioned
--   at the right spot, and open its own view (both "what real content led
--   to this" and "what did this pass itself add") by direct lookup against
--   this one tick -- never by searching/sorting across every occurrence of
--   the kind to guess which is "previous." 'wtTickId' is that occurrence's
--   own real 'Summary' tick id, so the client's ordinary upsert-by-id
--   model dedupes/updates each one independently -- and a later pass
--   re-minting an ancestor tick (see
--   'Storyteller.Writer.Agent.Summarizer.extendNestedAltChain') still
--   surfaces as a genuinely new tick id, keeping 'fileStateWithSummaries'
--   own id-based signature a correct staleness check with no changes.
--
--   'wtContent'/'wtMessage' are this occurrence's own delta (see
--   'Storyteller.Common.Summary.occurrenceDelta') -- what this specific
--   pass actually added, the same sense an ordinary atom's own content is
--   just what it appended, not the whole file it's attached to. Not
--   'Storyteller.Common.Summary.summaryContent' (the alt-chain's current
--   full accumulated text) -- that answers a different question, and
--   would make every occurrence's own annotation/preview repeat every
--   earlier pass's text verbatim.
--
--   'wtParent' is always 'Nothing', regardless of depth -- 'Storage.Tick.
--   fileTicksOf'/'relatedTicksOf' (which built whatever chain this tick
--   rides alongside, at *any* scope -- a plain branch or an alt-chain
--   connection alike, both go through the same 'Server.Core.File.fileState')
--   already "relinks around" any tick with no real file footprint, so a
--   Summary tick's own *actual* git parent is never a valid position within
--   that relinked view -- exposing it doesn't let a client's tick-chain
--   walk find this tick at all (nothing in the relinked chain ever has it
--   as *their* parent), it just breaks the walk. A client positions this
--   tick by its own 'wtRefs' anchor against the atoms already present in
--   that relinked chain, same as it already does for a top-level occurrence.
summaryTicksFor :: FileOpen r => FilePath -> Sem r [WireTick]
summaryTicksFor path = concat <$> mapM oneKind (summaryKindsFor path)
  where
    oneKind kind = do
      occurrences <- SummaryAccess.summariesTouchingFor @Main kind path
      mapM (toWireTick kind) occurrences

    toWireTick kind occ = do
      delta <- runStorage @Main (Summary.occurrenceDelta occ path)
      return WireTick
        { wtTickId  = unTickId (Summary.occTickId occ)
        , wtKind    = "summary"
        -- Only the anchor lives in 'wtRefs' -- that's the one generic
        -- positioning mechanism the client already has (fileview.tsx's
        -- annotationsFor matches refs against real atom ids, same as a
        -- Note). The two per-occurrence boundaries are structured
        -- metadata only summary-specific code reads, so they ride in
        -- 'wtFields' like every other field -- putting them in refs too
        -- would force the generic scan to guess which ref is the anchor.
        , wtRefs    = [unTickId (Summary.occAnchor occ)]
        , wtFields  = ("kind", kind)
            :  [ ("lowerBound",  unTickId tid) | Just tid <- [Summary.occLowerBound  occ] ]
            ++ [ ("prevAltHead", unTickId tid) | Just tid <- [Summary.occPrevAltHead occ] ]
        , wtMessage = delta
        , wtContent = Just delta
        , wtParent  = Nothing
        }

-- | 'Server.Core.File.fileStateSince' plus @path@'s current summary tiers
--   folded in (see 'summaryTicksFor'), paired with a signature of the
--   summary state right now -- an order-independent join of each
--   occurrence's id *and current delta text*, not ids alone: an edit
--   through a summary connection changes an occurrence's content without
--   changing its (stable, base-tick) id -- see
--   'Storyteller.Common.Summary.summariesTouching' on superseding runs --
--   and must still push. 'Server.Writer.File.Connection.pushIncremental'
--   threads this signature alongside 'updateHead' in its own "since"
--   cursor so a summarize pass is never invisible to an already-open
--   connection (see 'summaryTicksFor's own Haddock for why that matters).
fileStateWithSummaries :: FileOpen r => FilePath -> Maybe T.Text -> Sem r (Update, T.Text)
fileStateWithSummaries path since = do
  upd   <- Core.fileStateSince path since
  extra <- summaryTicksFor path
  let sig = T.intercalate "," (List.sort (map (\wt -> wtTickId wt <> "=" <> wtMessage wt) extra))
  return (upd { updateTicks = updateTicks upd ++ extra }, sig)
