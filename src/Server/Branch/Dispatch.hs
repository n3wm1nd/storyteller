{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Routing only: decode BranchCommand → call Server.Branch → return events.
-- No business logic lives here. Runs against the ambient, already-open
-- branch scope ('BranchOpen') — see 'Server.Branch.Connection' for where
-- that scope is entered.
--
-- Returns only what a ref-move notification can't convey: structural events
-- (FileAdded) for this specific command. Successful mutations otherwise
-- reach the client via the ref-move notification, same as anyone else's
-- write. Throws (Error String) on failure — the caller catches it and turns
-- it into a BranchError push rather than ending the connection.
module Server.Branch.Dispatch
  ( runCommand
  ) where

import qualified Data.Text as T
import Polysemy (Sem)

import Server.Branch (Main, BranchOpen, addNote, moveTickInBranch, deleteTickFromBranch,
                      trackFiles, charGen)
import Server.Branch.Protocol
import Server.Run (SessionEffects)
import Storyteller.Types (BranchName(..), TickId(..))

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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toPair :: TrackFile -> (FilePath, FilePath)
toPair tf = (trackFrom tf, trackTo tf)
