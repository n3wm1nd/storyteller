{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Branch-level business logic that isn't specific to any one application:
-- state queries and tick-chain mutations that make sense for any app built
-- on top of the branch/tick storage model.
--
-- These functions assume the branch's storage/filesystem scope ('BranchOpen')
-- is already live in the ambient stack — the connection (e.g.
-- 'Server.Writer.Branch.Connection') reopens that scope fresh around each
-- command, nested inside a 'Storyteller.Core.Git.withStorage' transaction, so a
-- command's writes are all-or-nothing and visible immediately, not just at
-- disconnect — these functions don't need to know that; they just see
-- 'BranchOpen' as already open.
--
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.Core.Branch
  ( Main
  , BranchOpen
  , branchState
  , branchStateSince
  , addNote
  , moveTickInBranch
  , deleteTickFromBranch
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles)

import Server.Core.Protocol (Update(..), tickToWireTick)

import qualified Storyteller.Common.Annotation as Annotation
import Storyteller.Core.Git (BranchTag, BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage)
import qualified Storage.FS as FS
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (TickId(..), tickId, unTickId)

data Main

-- | The effects live once a branch connection has entered its branch's
--   scope — reopened fresh per command by the connection handler, not held
--   for the connection's whole lifetime (see the module comment).
type BranchOpen r =
  Members '[ BranchOp Main
           , StoryStorage
           , FileSystemWrite (BranchTag Main)
           , FileSystemRead  (BranchTag Main)
           , FileSystem      (BranchTag Main)
           , Fail
           ] r

-- ---------------------------------------------------------------------------
-- State query
-- ---------------------------------------------------------------------------

-- | Full branch state: all ticks and current HEAD id.
branchState :: BranchOpen r => Sem r ([FilePath], Update)
branchState = branchStateSince Nothing

-- | Branch state, optionally incremental. When 'since' names a tick still
--   reachable from HEAD, only ticks newer than it are included — the common
--   case for keeping an already-caught-up connection informed of new writes.
--   When 'since' is 'Nothing', or no longer reachable (e.g. a move/replace
--   rewrote history out from under it), the walk runs all the way to root
--   and the full chain is returned — the correct, if pricier, fallback.
--
--   HEAD is derived from this same walk (its last, most-recent element)
--   rather than a separate resolution — a second, independent HEAD
--   resolution could race a concurrent rebase and return a different,
--   incompatible position than the one the walk was just done from, sending
--   a HEAD the client can't fully resolve against the ticks in the very
--   same update. One walk, one resolution: whatever HEAD was at the moment
--   the walk resolved it, that's what both 'ticks' and the reported head
--   describe.
--
--   Resets the working tree first: 'listAllFiles' reads the in-memory
--   working tree, which this connection's long-lived stack loaded once at
--   scope-entry and never otherwise refreshes. Ticks/HEAD are read straight
--   from git and are always current regardless — only the file list needs
--   this to see writes made by other connections since we last synced.
branchStateSince :: BranchOpen r => Maybe TickId -> Sem r ([FilePath], Update)
branchStateSince since = do
  _ <- runStorage @Main FS.reset
  files <- listAllFiles @(BranchTag Main) "/"
  (_, ticks) <- runStorage @Main $
    Tick.newTypesTicksSince (Ops.ObjectHash . unTickId <$> since)
  case (reverse ticks, since) of
    (headTk : _, _) ->
      return (files, Update (map tickToWireTick ticks) (unTickId (tickId headTk)))
    ([], Just s) ->
      -- Nothing past 'since': HEAD hasn't moved from what the caller
      -- already has, no need to resolve it again.
      return (files, Update [] (unTickId s))
    ([], Nothing) ->
      fail "branchStateSince: branch has no ticks"

-- ---------------------------------------------------------------------------
-- Mutations on the already-open branch
-- ---------------------------------------------------------------------------

-- | Add an annotation note referencing zero or more existing ticks.
addNote :: BranchOpen r => [TickId] -> T.Text -> Sem r ()
addNote refs text = void $ runStorage @Main (Annotation.addNote refs text)

-- | Move a tick to a new position in the chain.
moveTickInBranch :: BranchOpen r => TickId -> Maybe TickId -> Sem r ()
moveTickInBranch tid mAfter =
  void $ runStorage @Main (Ops.moveTick (toHash tid) (toHash <$> mAfter))

-- | Delete a tick from the chain.
deleteTickFromBranch :: BranchOpen r => TickId -> Sem r ()
deleteTickFromBranch tid =
  void $ runStorage @Main (Ops.deleteTick (toHash tid))

toHash :: TickId -> Ops.ObjectHash
toHash (TickId t) = Ops.ObjectHash t
