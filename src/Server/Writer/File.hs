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
  , editChatPrompt
  , chatChapterRegen
  , chatSplitOutline
  , RegenMode(..)
  , setPresence
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

import Storyteller.Writer.Agent (Prompt(..), Instruction(..), ContextBlock(..), Prose(..), CharContextBlock, WordCount(..))
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)
import Storyteller.Writer.Agent.Chat (chatAgent, historyFromFileTicks)
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Writer.Agent.Fix (fixAgent)
import Storyteller.Writer.Agent.Outline
  ( BeatSheet(..), CurrentProse(..), OutlineDoc(..), ChapterBeats(..)
  , reconcileChapter, reconcileChapterByBeat, splitOutlineAgent )
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (PresenceEvent)
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Runtime (Main, StoryModel)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (BranchName, TickId(..))
import Storyteller.Core.Git (BranchTag, runStorage)

import Prelude hiding (readFile, writeFile)

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Writer.Agent.FlowWrite'). Context (existing file content,
--   branch files, character summaries) is gathered here and handed to the
--   agents as plain data; they append nothing themselves, so appending the
--   result is done here too. No character branches are wired into a scene
--   yet, hence the empty list below — see WRITER.md presence conventions
--   for where that's headed.
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> Maybe TickId -> Sem r ()
chatWriter path prompt context mFlowTid = do
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  (existing, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) path)
  let extraContext = toContextBlocks context <> fileCtx
      instruction  = Instruction prompt
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      (_reworked, Prose generated) <- flowWriteAgent @StoryModel @StoryModel @Main modelConfigs modelConfigs path flowTid existing extraContext instruction []
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      Prose generated <- writeAgent @StoryModel modelConfigs existing extraContext instruction []
      _ <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms generated
      info $ "writer agent done: " <> T.pack path

-- | Store a prompt tick then run the Fixer agent against the given targets.
--   With no targets, there's nothing to rework — that's a different policy
--   ("just write") from the Fixer's, so it's handled here as a fall through
--   to the same Writer path 'chatWriter' takes, rather than living inside
--   'Storyteller.Writer.Agent.Fix.fixAgent'.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context [] = chatWriter path prompt context Nothing
chatFixer path prompt _context targets = do
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  info $ "fixer agent starting: " <> T.pack path
  _ <- fixAgent @StoryModel @Main modelConfigs path targets (Instruction prompt)
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
  added <- chatAgent @(BranchTag Main) modelConfigs (history ++ [UserText prompt])
  let reply = mconcat [t | AssistantText t <- added]
  _ <- runStorage @Main (Ops.append path reply)
  info $ "chat agent done: " <> T.pack path

-- | Edit a chat prompt's text in place. A 'Prompt' is not an atom — its
--   message carries no filesystem footprint -- so this edits the raw
--   'NonAtom' message directly (keeping its own @"type:<tag>\n"@ line)
--   rather than going through 'editFileAtom'.
editChatPrompt :: FileOpen r => TickId -> T.Text -> Sem r ()
editChatPrompt (TickId tid) content =
  void $ runStorage @Main $ Core.at (Core.ObjectHash tid) $ Core.editTick $ \case
    Core.NonAtom refs msg -> return (Core.NonAtom refs (setPayload msg))
    _                      -> fail "editChatPrompt: not a chat prompt (it's an atom)"
  where
    setPayload msg = let (tag, _) = T.breakOn "\n" msg in tag <> "\n" <> content

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
      (_, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) path)
      let extraContext = toContextBlocks context <> fileCtx
          instruction  = Instruction prompt
          noChars      = [] :: [CharContextBlock]
      info $ "chapter regen (" <> T.pack (show mode) <> ") starting: " <> T.pack path
      Prose regenerated <- case mode of
        RegenWhole  -> reconcileChapter @StoryModel modelConfigs (Just (WordCount 1200))
                         noChars extraContext current instruction sheet
        RegenByBeat -> reconcileChapterByBeat @StoryModel modelConfigs (Just (WordCount 300))
                         noChars extraContext current instruction sheet maxBeats
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
--   per-chapter beat sheets. The model decides the chapter breakdown and the
--   output paths — the chapter files needn't exist yet — and each returned
--   beat sheet is written as its own file (atomized by the splitter, same as
--   generated prose). Existing beat-sheet files are left alone: writing only
--   happens for paths the model emits, and a path it re-emits appends rather
--   than clobbers, so this is safe to re-run.
chatSplitOutline :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> Sem r ()
chatSplitOutline path = do
  outline <- OutlineDoc . TE.decodeUtf8 <$> readFile @(BranchTag Main) path
  (_, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) path)
  info $ "outline split starting: " <> T.pack path
  sheets <- splitOutlineAgent modelConfigs fileCtx outline
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
setPresence :: FileOpen r => FilePath -> BranchName -> PresenceEvent -> Sem r ()
setPresence path character event = void $ recordPresence @Main path character event
