{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
--
-- Routing only: decode FileCommand → call Server.File. No business logic
-- lives here. Runs against the ambient, already-open branch scope
-- ('FileOpen') — see 'Server.File.Connection' for where that scope is
-- entered.
--
-- Successful mutations reach the client via the ref-move notification, same
-- as anyone else's write — this just runs the mutation. Throws
-- (Error String) on failure — the caller catches it and turns it into a
-- FileError push rather than ending the connection.
module Server.File.Dispatch
  ( runCommand
  ) where

import Polysemy (Member, Sem)
import Polysemy.Error (throw)

import Server.File (FileOpen, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom, chatWriter, chatFixer)
import Server.File.Protocol (FileCommand(..))
import Server.Run (SessionEffects)
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.Runtime (Main)
import qualified Storyteller.Storage as Storage
import Storyteller.Types (TickId(..))

runCommand :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> FileCommand -> Sem r ()
runCommand path cmd = case cmd of

  ChatAppend _mid content ->
    appendToFile path content

  Delete _mid ->
    throw @String "file delete not yet implemented"

  EditAtom _mid tid content ->
    editFileAtom path (TickId tid) content

  DeleteAtom _mid tid ->
    deleteFileAtom (TickId tid)

  MoveAtom _mid tid mAfter ->
    moveFileAtom (TickId tid) (TickId <$> mAfter)

  ChatWriter _mid prompt context flowTid ->
    chatWriter path prompt context (TickId <$> flowTid)

  ChatFixer _mid prompt context targets ->
    chatFixer path prompt context (map TickId targets)

  -- Rebase 'inner' at 'tid': wind the chain back, run it against that
  -- tick's filesystem snapshot, then replay the tail on top of whatever it
  -- produced. 'reset' reloads the working tree from the (now rebased) head,
  -- since 'atWithFS' only restores the pre-call tree, not the post-rebase
  -- one — same pattern 'editAtom'/'moveTick' use after their own 'at' calls.
  -- 'atWithFS' broadcasts the mapping itself, so nothing left to do here.
  At _mid tid inner -> do
    _ <- Storage.atWithFS @Main (TickId tid) (runCommand path inner)
    Storage.reset @Main
