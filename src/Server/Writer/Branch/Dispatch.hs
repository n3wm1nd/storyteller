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
import Server.Core.Run (SessionEffects)
import qualified Storyteller.Core.Storage as Storage
import Storyteller.Core.Types (BranchName(..), TickId(..))

runCommand
  :: (BranchOpen r, SessionEffects r)
  => T.Text -> BranchCommand -> Sem r [BranchEvent]
runCommand branch cmd =
  let name = BranchName branch
  in case cmd of

    Track mid source files ->
      map (FileAdded mid) <$> trackFiles name (BranchName source) (map toPair files)

    CharGen mid path scenario seed -> do
      charGen name path scenario seed
      return [FileAdded mid path]

    AddNote _mid refTickId text -> do
      addNote @Main [TickId refTickId] text
      return []

    MoveTick _mid tid mAfter -> do
      moveTickInBranch (TickId tid) (TickId <$> mAfter)
      return []

    DeleteTick _mid tid -> do
      deleteTickFromBranch (TickId tid)
      return []

    -- Rebase 'inner' at 'tid': wind the chain back, run it against that
    -- tick's filesystem snapshot, then replay the tail on top of whatever it
    -- produced. 'reset' reloads the working tree from the (now rebased)
    -- head, since 'atWithFS' only restores the pre-call tree, not the
    -- post-rebase one — same pattern 'Server.Writer.File.Dispatch's 'At'
    -- case uses. 'atWithFS' broadcasts its own mapping, so nothing left to
    -- do with the discarded tuple component here.
    At _mid tid inner -> do
      (evts, _mapping) <- Storage.atWithFS @Main (TickId tid) (runCommand branch inner)
      Storage.reset @Main
      return evts

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toPair :: TrackFile -> (FilePath, FilePath)
toPair tf = (trackFrom tf, trackTo tf)
