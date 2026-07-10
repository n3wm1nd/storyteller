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
  , chatFixer
  , chatConverse
  , chatConverseSwipe
  , editChatPrompt
  , chatChapterRegen
  , chatSplitOutline
  , RegenMode(..)
  , setPresence
  , askCharacter
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (isSuffixOf)
import Polysemy (Member, Sem)
import Runix.Logging (info)
import Runix.FileSystem (fileExists, readFile, writeFile)

import Server.Core.File (FileOpen)
import Server.Core.Run (SessionEffects)
import Server.Writer.File.Protocol (ContextItem(..))

import UniversalLLM (Message(..))

import Storyteller.Writer.Agent (Prompt(..), Instruction(..), ContextBlock(..), Prose(..), CharContextBlock, CharLabel(..), WordCount(..))
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.ContextFilter (ContextLayout, hideBinaryFiles)
import Storyteller.Writer.Agent.Chat (chatAgent, historyFromFileTicks)
import Storyteller.Writer.Agent.CharContext (charSummaryAgent)
import Storyteller.Writer.Agent.AskCharacter (askCharacterAgent)
import Storyteller.Writer.Agent.Write (writeAgent, flattenCharBlocks)
import Storyteller.Writer.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Writer.Agent.Fix (fixAgent)
import Storyteller.Writer.Agent.Outline
  ( BeatSheet(..), CurrentProse(..), OutlineDoc(..), ChapterBeats(..)
  , reconcileChapter, reconcileChapterByBeat, splitOutlineFreeform )
import Storyteller.Writer.Presence (recordPresence, activeCharactersFor)
import Storyteller.Writer.Types (Character(..), CharacterAnswer(..), PresenceEvent)
import Storyteller.Core.Runtime (Main)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import qualified Storyteller.Common.Swipe as Swipe
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick, toDraft)
import Storyteller.Core.Git (BranchTag, runBranchAndFS, runStorage)

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
--   an agent sees. Each active branch is opened dynamically (same
--   'runBranchAndFS' pattern 'Server.Writer.Branch.charGen'\/'trackFiles'
--   use for a runtime-named branch) and summarized via 'charSummaryAgent'
--   -- excluding @journal.md@ (see 'journalPath'): it's long, mostly a copy
--   of what the scene's own history already says, sometimes stale or
--   contradictory, and not written for a narrator to read. Full journal
--   access is still available on request, via
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent'
--   (@\/ask@\/the sidebar's Ask panel) -- deliberately not wired into
--   generation itself yet.
activeCharacterContext :: (FileOpen r, SessionEffects r) => FilePath -> Sem r [(CharLabel, [CharContextBlock])]
activeCharacterContext path = do
  active <- activeCharactersFor @Main path
  mapM summarize active
  where
    summarize (Character (BranchName name)) = do
      blocks <- runBranchAndFS @ActiveChar (BranchName name) (charSummaryAgent @(BranchTag ActiveChar) (/= journalPath))
      let label = maybe name id (T.stripPrefix "character/" name)
      pure (CharLabel label, blocks)

-- | A character branch's own account, in fiction-time order (see
--   WRITER.md's "Character structure" section) -- excluded from
--   generation's ambient context (see 'activeCharacterContext'), included
--   in full for an explicit 'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent'
--   query.
journalPath :: FilePath
journalPath = "journal.md"

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Writer.Agent.FlowWrite'). Context (existing file content,
--   branch files, character summaries) is gathered here and handed to the
--   agents as plain data; they append nothing themselves, so appending the
--   result is done here too.
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> ContextLayout -> Maybe TickId -> Sem r ()
chatWriter path prompt context layout mFlowTid = do
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  (existing, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) layout path)
  charBlocks <- activeCharacterContext path
  let extraContext = toContextBlocks context <> fileCtx
      instruction  = Instruction prompt
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      (_reworked, Prose generated) <- flowWriteAgent @Main path flowTid existing extraContext instruction charBlocks
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      Prose generated <- writeAgent existing extraContext instruction charBlocks
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "writer agent done: " <> T.pack path

-- | Store a prompt tick then run the Fixer agent against the given targets.
--   With no targets, there's nothing to rework — that's a different policy
--   ("just write") from the Fixer's, so it's handled here as a fall through
--   to the same Writer path 'chatWriter' takes, rather than living inside
--   'Storyteller.Writer.Agent.Fix.fixAgent'.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context [] = chatWriter path prompt context [] Nothing
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
  (ticks, _) <- runStorage @Main (Tick.fileTicksOf path)
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
  (ticks, _) <- runStorage @Main (Tick.fileTicksOf path)
  let (before, _fromPrompt) = span ((/= unTickId promptTid) . Tick.ftTickId) ticks
      history = historyFromFileTicks before
  editChatPrompt promptTid newPromptText
  info $ "chat agent regenerating (swipe): " <> T.pack path
  added <- chatAgent @(BranchTag Main) (history ++ [UserText newPromptText])
  let reply = mconcat [t | AssistantText t <- added]
  _ <- runStorage @Main (Swipe.pushSwipe (Core.ObjectHash (unTickId atomTid)) reply)
  info $ "chat agent regen (swipe) done: " <> T.pack path

-- | Edit a chat prompt's text in place. A 'Prompt' is not an atom — its
--   message carries no filesystem footprint -- so this edits the raw
--   'NonAtom' message directly rather than going through 'editFileAtom'.
--
--   Goes through 'Prompt's own 'TickType' instance to rebuild the message —
--   decode the current one back into a 'Prompt' (recovering its "file"
--   field), substitute the new text, then 'toDraft'\/'Tick.encodeTickData'
--   it fresh — rather than hand-editing the raw tagged string in place.
--   That used to seem like the smaller change (find the tag, keep it,
--   splice in new text) but "the tag" isn't reliably at a fixed offset:
--   whether it's on the first line at all depends on whether the tick
--   carries fields (a 'Prompt' always does, via its own "file" field), so a
--   fixed-offset splice silently corrupted the header the moment that
--   assumption didn't hold — dropping the "type:prompt" tag entirely and
--   leaving the tick undecodable as anything but "unknown" kind ever after.
--   Round-tripping through the same encode\/decode the tick kind's own
--   'TickType' instance already defines doesn't have that failure mode:
--   whatever it puts in the header is exactly what it'll expect back out.
editChatPrompt :: FileOpen r => TickId -> T.Text -> Sem r ()
editChatPrompt (TickId tid) content = do
  (typed, _) <- runStorage @Main (Tick.readTypesTick (Core.ObjectHash tid))
  case fromTick @Prompt typed of
    Nothing -> fail "editChatPrompt: not a chat prompt"
    Just (Prompt file _) -> do
      let newMsg = Tick.encodeTickData (toDraft (Prompt file content))
      void $ runStorage @Main $ Core.at (Core.ObjectHash tid) $ Core.editTick $ \case
        Core.NonAtom refs _ -> return (Core.NonAtom refs newMsg)
        _                    -> fail "editChatPrompt: not a chat prompt (it's an atom)"

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
      charBlocks <- flattenCharBlocks <$> activeCharacterContext path
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
