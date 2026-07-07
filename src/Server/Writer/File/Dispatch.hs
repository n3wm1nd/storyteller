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
  ) where

import Polysemy (Member, Sem)
import Polysemy.Error (throw)

import Server.Core.File (FileOpen, createFile, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom, mergeFileAtoms, splitFileAtoms, hideFileAtoms, unhideFileAtoms, chatNote)
import Server.Writer.File (chatWriter, chatFixer, chatConverse, editChatPrompt, chatChapterRegen, chatSplitOutline, RegenMode(..), setPresence)
import Server.Writer.File.Protocol (FileCommand(..))
import Server.Core.Run (SessionEffects)
import Storyteller.Common.Splitter (Splitter)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Git (atGeneric)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Types (PresenceEvent(..))

runCommand :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> FileCommand -> Sem r ()
runCommand path cmd = case cmd of

  CreateFile _mid ->
    createFile path

  ChatAppend _mid content ->
    appendToFile path content

  Delete _mid ->
    throw @String "file delete not yet implemented"

  EditAtom _mid tid content ->
    editFileAtom path (TickId tid) content

  EditPrompt _mid tid content ->
    editChatPrompt (TickId tid) content

  DeleteAtom _mid tid ->
    deleteFileAtom (TickId tid)

  MoveAtom _mid tid mAfter ->
    moveFileAtom (TickId tid) (TickId <$> mAfter)

  MergeAtoms _mid targets ->
    mergeFileAtoms (map TickId targets)

  SplitAtoms _mid targets ->
    splitFileAtoms (map TickId targets)

  HideAtoms _mid targets ->
    hideFileAtoms (map TickId targets)

  UnhideAtoms _mid targets ->
    unhideFileAtoms (map TickId targets)

  ChatWriter _mid prompt context flowTid ->
    chatWriter path prompt context (TickId <$> flowTid)

  ChatFixer _mid prompt context targets ->
    chatFixer path prompt context (map TickId targets)

  ChatRegen _mid prompt context byBeat ->
    chatChapterRegen (if byBeat then RegenByBeat else RegenWhole) path prompt context

  ChatConverse _mid prompt ->
    chatConverse path prompt

  ChatOutline _mid ->
    chatSplitOutline path

  ChatNote _mid text targets ->
    chatNote text (map TickId targets)

  EnterScene _mid character ->
    setPresence path (BranchName character) Enter

  LeaveScene _mid character ->
    setPresence path (BranchName character) Leave

  -- Rebase 'inner' at 'tid': wind the chain back, run it against that
  -- tick's filesystem snapshot, then replay the tail on top of whatever it
  -- produced. 'atGeneric' is the one operation still built on generic
  -- recursion rather than a single closed-form "Storage.Core" computation,
  -- since 'inner' can recurse into arbitrary Writer commands (LLM calls,
  -- other effects) — see 'Storyteller.Core.Git.atGeneric'. Each of its own
  -- navigation steps is its own dispatch, so each already broadcasts its
  -- own remap entry as it goes — nothing left to do here once it returns.
  At _mid tid inner ->
    atGeneric @Main (TickId tid) (runCommand path inner)
