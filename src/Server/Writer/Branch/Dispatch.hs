{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Routing only: decode BranchCommand → call Server.Core.Branch /
-- Server.Writer.Branch → return events. No business logic lives here. Runs
-- against the ambient, already-open branch scope ('BranchOpen') — see
-- 'Server.Writer.Branch.Connection' for where that scope is entered.
--
-- 'MoveTick'/'DeleteTick'/'AddNote' are generic tick-chain operations, so
-- they call straight into 'Server.Core.Branch'. 'Track'/'CharGen' are
-- Writer-specific, so they call 'Server.Writer.Branch' instead — this
-- module is where the two layers actually get assembled into one protocol.
-- 'At' is generic either way — it just recurses back into this same
-- dispatch for whichever inner command, same shape as
-- 'Server.Writer.File.Dispatch's 'At' case. Scene presence
-- (enter.scene/leave.scene) is not a 'BranchCommand' at all — it lives on
-- 'Server.Writer.File.Dispatch' instead, since presence is scoped to one
-- file (a scene), not the whole branch — see WRITER.md.
--
-- Returns only what a ref-move notification can't convey: structural events
-- (FileAdded) for this specific command. Successful mutations otherwise
-- reach the client via the ref-move notification, same as anyone else's
-- write. Throws (Error String) on failure — the caller catches it and turns
-- it into a BranchError push rather than ending the connection.
module Server.Writer.Branch.Dispatch
  ( runCommand
  ) where

import qualified Data.Text as T
import Polysemy (Sem)

import Server.Core.Branch (Main, BranchOpen, addNote, moveTickInBranch, deleteTickFromBranch)
import Server.Writer.Branch (trackFiles, charGen)
import Server.Writer.Branch.Protocol
import Server.Writer.File.Dispatch (atBranches)
import Server.Core.Run (SessionEffects)
import Storyteller.Core.Git (atGeneric)
import Storyteller.Core.Types (BranchName(..), TickId(..))

runCommand
  :: (BranchOpen r, SessionEffects r)
  => T.Text -> BranchCommand -> Sem r [BranchEvent]
runCommand branch cmd =
  let name = BranchName branch
  in case cmd of

    Track mid source onlyFile toFile -> do
      path <- trackFiles name (BranchName source) onlyFile toFile
      return [FileAdded mid path]

    CharGen mid path scenario seed -> do
      charGen name path scenario seed
      return [FileAdded mid path]

    AddNote _mid refTickId text -> do
      addNote [TickId refTickId] text
      return []

    MoveTick _mid tid mAfter -> do
      moveTickInBranch (TickId tid) (TickId <$> mAfter)
      return []

    DeleteTick _mid tid -> do
      deleteTickFromBranch (TickId tid)
      return []

    -- Rebase 'inner' at 'tid': wind the chain back, run it against that
    -- tick's filesystem snapshot, then replay the tail on top of whatever it
    -- produced, same shape as 'Server.Writer.File.Dispatch's own 'At' case
    -- (including 'branches': connected branches wound to their chosen
    -- points around the whole thing -- see 'atBranches').
    At _mid tid inner branches ->
      atBranches branches $ atGeneric @Main (TickId tid) (runCommand branch inner)
