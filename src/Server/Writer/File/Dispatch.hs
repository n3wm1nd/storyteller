{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
--
-- Routing only: decode FileCommand → call Server.Core.File /
-- Server.Writer.File. No business logic lives here. Runs against the
-- ambient, already-open branch scope ('FileOpen') — see
-- 'Server.Writer.File.Connection' for where that scope is entered.
--
-- 'ChatAppend'/'EditAtom'/'DeleteAtom'/'MoveAtom'/'ChatNote' are generic
-- atom-chain operations, so they call straight into 'Server.Core.File'.
-- 'ChatWriter'/'ChatFixer'/'EnterScene'/'LeaveScene' are Writer-specific, so
-- they call 'Server.Writer.File' instead — this module is where the two
-- layers actually get assembled into one protocol. 'At' is generic either
-- way — it just recurses back into this same dispatch for whichever inner
-- command, which is what lets 'EnterScene'/'LeaveScene' be sent rebased at
-- a client's rebase marker for free.
--
-- Successful mutations reach the client via the ref-move notification, same
-- as anyone else's write — this just runs the mutation. Throws
-- (Error String) on failure — the caller catches it and turns it into a
-- FileError push rather than ending the connection.
module Server.Writer.File.Dispatch
  ( runCommand
  , atBranches
  ) where

import Polysemy (Member, Sem, raise)
import qualified Data.Text as T

import Server.Core.File (FileOpen, createFile, deleteFile, renameFile, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom, mergeFileAtoms, splitFileAtoms, hideFileAtoms, unhideFileAtoms, chatNote, cycleAtomSwipe)
import Server.Writer.File (chatWriter, chatFixer, chatConverse, chatConverseSwipe, editChatPrompt, chatChapterRegen, chatSplitOutline, RegenMode(..), setPresence, askCharacter, correctGroup)
import Server.Writer.File.Protocol (FileCommand(..), FileEvent(..), AtBranch(..))
import Server.Core.Run (SessionEffects)
import Storyteller.Common.Splitter (Splitter)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Git (atGeneric, runBranchAndFS)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

-- | Phantom tag for opening one connected branch (a character's journal —
--   see 'AtBranch') at a time, dynamically. The scopes 'atBranches' opens
--   nest, but each 'atGeneric'\/'runStorage' call is lexically inside
--   exactly one 'runBranchAndFS' layer, so a single shared tag is fine —
--   same role 'Server.Writer.Branch.CharBranch' plays there.
data ConnectedBranch

runCommand :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> FileCommand -> Sem r [FileEvent]
runCommand path cmd = case cmd of

  CreateFile _mid ->
    [] <$ createFile path

  ChatAppend _mid content ->
    [] <$ appendToFile path content

  Delete _mid ->
    [] <$ deleteFile path

  Rename _mid newPath ->
    [] <$ renameFile path (T.unpack newPath)

  EditAtom _mid tid content ->
    [] <$ editFileAtom path (TickId tid) content

  EditPrompt _mid tid content ->
    [] <$ editChatPrompt (TickId tid) content

  DeleteAtom _mid tid ->
    [] <$ deleteFileAtom (TickId tid)

  MoveAtom _mid tid mAfter ->
    [] <$ moveFileAtom (TickId tid) (TickId <$> mAfter)

  MergeAtoms _mid targets ->
    [] <$ mergeFileAtoms (map TickId targets)

  SplitAtoms _mid targets ->
    [] <$ splitFileAtoms (map TickId targets)

  HideAtoms _mid targets ->
    [] <$ hideFileAtoms (map TickId targets)

  UnhideAtoms _mid targets ->
    [] <$ unhideFileAtoms (map TickId targets)

  ChatWriter _mid prompt context layout flowTid charLayouts ->
    [] <$ chatWriter path prompt context layout (TickId <$> flowTid) charLayouts

  ChatFixer _mid prompt context targets ->
    [] <$ chatFixer path prompt context (map TickId targets)

  ChatRegen _mid prompt context byBeat ->
    [] <$ chatChapterRegen (if byBeat then RegenByBeat else RegenWhole) path prompt context

  CorrectGroup _mid promptTid targets prompt context layout charLayouts ->
    [] <$ correctGroup path (TickId promptTid) (map TickId targets) prompt context layout charLayouts

  ChatConverse _mid prompt ->
    [] <$ chatConverse path prompt

  ChatConverseSwipe _mid promptTid atomTid prompt ->
    [] <$ chatConverseSwipe path (TickId promptTid) (TickId atomTid) prompt

  CycleSwipe _mid tid ->
    [] <$ cycleAtomSwipe (TickId tid)

  ChatOutline _mid ->
    [] <$ chatSplitOutline path

  ChatNote _mid text targets ->
    [] <$ chatNote text (map TickId targets)

  EnterScene _mid character ->
    [] <$ setPresence path (Character (BranchName character)) Enter

  LeaveScene _mid character ->
    [] <$ setPresence path (Character (BranchName character)) Leave

  -- The one command whose result isn't just a mutation another connection's
  -- ref-move notification would surface: the answer lands on the
  -- character's own branch, not this file's, so it's returned here to be
  -- pushed straight back to the asking connection instead.
  AskCharacter mid character question -> do
    answer <- askCharacter path (Character (BranchName character)) question
    return [CharacterAnswered mid character question answer]

  -- Rebase 'inner' at 'tid': wind the chain back, run it against that
  -- tick's filesystem snapshot, then replay the tail on top of whatever it
  -- produced. 'atGeneric' is the one operation still built on generic
  -- recursion rather than a single closed-form "Storage.Core" computation,
  -- since 'inner' can recurse into arbitrary Writer commands (LLM calls,
  -- other effects) — see 'Storyteller.Core.Git.atGeneric'.
  --
  -- 'branches' (see 'AtBranch') are connected branches -- e.g. an active
  -- character's journal -- the client has picked its own explicit position
  -- for (there is no reliable way to infer one server-side: story time and
  -- a character branch's own position aren't in lock-step). They are wound
  -- back *around* the main rebase — see 'atBranches' — so 'inner' runs in
  -- a world where every chosen branch, opened or not, sits at its chosen
  -- point.
  At _mid tid inner branches ->
    atBranches branches $ atGeneric @Main (TickId tid) (runCommand path inner)

-- | Run @act@ with every listed connected branch (see 'AtBranch') wound
--   back to its user-chosen position: each is wound down *before* @act@
--   runs and its tail replayed after @act@ returns, nesting around the
--   main branch's own rebase — so an agent @act@ invokes that opens one of
--   these branches by name sees it at the chosen point (the wound-back
--   head is visible through the transaction's ref overlay), not at its
--   final head. The branch needn't be "open" anywhere: a fresh scope is
--   entered here per branch, by name.
--
--   Cross-branch ref fixup is no concern of this function's (it used to
--   be — a seeded fixup replay per listed branch): every rename any of
--   these replays records lands in the transaction's shared remap table,
--   and the transaction boundary applies the lot to *all* branches,
--   listed here or not. Exported for 'Server.Writer.Branch.Dispatch's own
--   'At' case, which needs exactly the same handling.
atBranches :: SessionEffects r => [AtBranch] -> Sem r a -> Sem r a
atBranches [] act = act
atBranches (AtBranch name tid : rest) act =
  runBranchAndFS @ConnectedBranch (BranchName name) $
    atGeneric @ConnectedBranch (TickId tid) $
      raise $ raise $ raise $ raise $ atBranches rest act
