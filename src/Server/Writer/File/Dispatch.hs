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
  , runConnectedBranch
  ) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Polysemy (Member, Sem)
import qualified Data.Text as T

import Server.Core.File (FileOpen, createFile, deleteFile, renameFile, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom, mergeFileAtoms, splitFileAtoms, hideFileAtoms, unhideFileAtoms, chatNote, cycleAtomSwipe)
import Server.Writer.File (chatWriter, chatFixer, chatConverse, chatConverseSwipe, editChatPrompt, chatChapterRegen, chatSplitOutline, RegenMode(..), setPresence)
import Server.Writer.File.Protocol (FileCommand(..), AtBranch(..))
import Server.Core.Run (SessionEffects)
import qualified Storage.Core as Core
import Storyteller.Common.Splitter (Splitter)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Git (atGenericSeeded, runBranchAndFS)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Types (PresenceEvent(..))

-- | Phantom tag for opening one connected branch (a character's journal —
--   see 'AtBranch') at a time, dynamically. Only ever used one branch at a
--   time, sequentially (see 'runConnectedBranch'), so a single shared tag
--   is fine — same role 'Server.Writer.Branch.CharBranch' plays there.
data ConnectedBranch

runCommand :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> FileCommand -> Sem r ()
runCommand path cmd = case cmd of

  CreateFile _mid ->
    createFile path

  ChatAppend _mid content ->
    appendToFile path content

  Delete _mid ->
    deleteFile path

  Rename _mid newPath ->
    renameFile path (T.unpack newPath)

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

  ChatWriter _mid prompt context layout flowTid ->
    chatWriter path prompt context layout (TickId <$> flowTid)

  ChatFixer _mid prompt context targets ->
    chatFixer path prompt context (map TickId targets)

  ChatRegen _mid prompt context byBeat ->
    chatChapterRegen (if byBeat then RegenByBeat else RegenWhole) path prompt context

  ChatConverse _mid prompt ->
    chatConverse path prompt

  ChatConverseSwipe _mid promptTid atomTid prompt ->
    chatConverseSwipe path (TickId promptTid) (TickId atomTid) prompt

  CycleSwipe _mid tid ->
    cycleAtomSwipe (TickId tid)

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
  -- produced. 'atGenericSeeded' (unseeded here — 'Map.empty' — for the
  -- main branch, which is never itself a "connected" branch relative to
  -- anything) is the one operation still built on generic recursion rather
  -- than a single closed-form "Storage.Core" computation, since 'inner' can
  -- recurse into arbitrary Writer commands (LLM calls, other effects) — see
  -- 'Storyteller.Core.Git.atGenericSeeded'. Each of its own navigation
  -- steps is its own dispatch, so each already broadcasts its own remap
  -- entry as it goes — nothing left to do for the main branch itself once
  -- it returns.
  --
  -- 'branches' (see 'AtBranch') are connected branches -- e.g. an active
  -- character's journal -- the client has picked its own explicit position
  -- for (there is no reliable way to infer one server-side: story time and
  -- a character branch's own position aren't in lock-step). Each is wound
  -- back and replayed in turn, seeded with the mapping the main branch's
  -- own rebase just produced, so any of that branch's cross-branch refs
  -- (see 'Storyteller.Writer.Agent.Tracker') into the now-rebased region
  -- get corrected as its own tail replays — no inner command runs there,
  -- since there's nothing else to do.
  At _mid tid inner branches -> do
    (_, mainMapping) <- atGenericSeeded @Main Map.empty (TickId tid) (runCommand path inner)
    mapM_ (runConnectedBranch mainMapping) branches

-- | Wind back and replay one connected branch (see 'AtBranch') at its own
--   given position, seeded with @mainMapping@ so any of its own
--   cross-branch refs into the rebase that just happened get fixed up.
--   Exported for 'Server.Writer.Branch.Dispatch's own 'At' case, which needs
--   exactly the same connected-branch handling.
runConnectedBranch :: SessionEffects r => Map.Map Core.ObjectHash Core.ObjectHash -> AtBranch -> Sem r ()
runConnectedBranch mainMapping (AtBranch name tid) =
  void $ runBranchAndFS @ConnectedBranch (BranchName name)
       $ atGenericSeeded @ConnectedBranch mainMapping (TickId tid) (pure ())
