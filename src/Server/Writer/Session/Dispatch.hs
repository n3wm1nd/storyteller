{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /session connections.
--
-- Routing only: decode SessionCommand → call Storyteller.Core.Storage → push the
-- event. Runs against the ambient, already-open session scope — see
-- 'Server.Writer.Session.Connection' for where that scope is entered. Throws
-- (Error String) on failure — the caller catches it and turns it into a
-- SessionError push rather than ending the connection.
module Server.Writer.Session.Dispatch
  ( runCommand
  , characterSummaries
  , branchNames
  , undoLogEntries
  ) where

import Data.Aeson (encode)
import Data.List (find, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Error (throw)
import Runix.FileSystem (fileExists, readFile)
import Runix.Git (ObjectHash(..))

import Server.Core.Run (SessionEffects)
import Server.Writer.Session.Protocol
import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Storage (listBranches, createBranch, getBranch, deleteBranch)
import Storyteller.Core.Types (BranchName(..), branchName)
import Storyteller.Core.Undo (UndoEntry(..), listUndo, resetToUndo)

import Prelude hiding (readFile)

runCommand :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> SessionCommand -> Sem r ()
runCommand conn cmd = case cmd of

  -- No direct branch.created/branch.deleted confirmation: same
  -- one-list-no-round-trip shape as 'UndoReset' below — the ref write also
  -- reaches the notifier (see Server.Writer.Session.Connection), which
  -- re-pushes the same 'BranchList' to every connection including this one;
  -- pushing it here too just spares the initiator that round trip, with no
  -- separate incremental event for a client to reconcile against it.
  CreateBranch _mid branch -> do
    let name = BranchName branch
    getBranch name >>= \case
      Just _  -> throw @String ("branch already exists: " <> T.unpack branch)
      Nothing -> do
        _ <- createBranch name
        branchNames >>= push . BranchList
        if "character/" `T.isPrefixOf` branch then characterSummaries >>= push . CharacterList else return ()

  DeleteBranch _mid branch -> do
    let name = BranchName branch
    getBranch name >>= \case
      Nothing -> throw @String ("branch not found: " <> T.unpack branch)
      Just _  -> do
        deleteBranch name
        branchNames >>= push . BranchList
        if "character/" `T.isPrefixOf` branch then characterSummaries >>= push . CharacterList else return ()

  -- No direct confirmation event: restoring refs re-triggers the same
  -- RefMoved-driven notifier every other write does (see
  -- Server.Writer.Session.Connection's onBranchMove), which re-pushes
  -- UndoLog to every connection including this one — pushing it again here
  -- too just means the initiator doesn't have to wait on that round trip.
  UndoReset _mid entryId -> do
    resetToUndo (ObjectHash entryId)
    entries <- undoLogEntries
    push (UndoLog entries)

  where
    push = embed . WS.sendTextData conn . encode

data SummaryBranch

-- | Every branch name — shared by the connection's initial push and its
--   notifier (see 'Server.Writer.Session.Connection'), which re-pushes this
--   same list whenever any branch ref moves.
branchNames :: SessionEffects r => Sem r [T.Text]
branchNames = map (unBranchName . branchName) <$> listBranches

-- | Every 'character/*' branch, each with its raw @sheet.md@ content (if
--   any) — shared by the connection's initial push and its notifier (see
--   'Server.Writer.Session.Connection'), which re-pushes this same list
--   whenever a matching branch ref moves. Opens each branch's own transient
--   FS scope to read its sheet, the same way 'Server.Writer.Branch.trackFiles'
--   opens a scope for a branch other than the one already ambiently open.
characterSummaries :: SessionEffects r => Sem r [CharacterSummary]
characterSummaries = do
  names <- filter ("character/" `T.isPrefixOf`) <$> branchNames
  mapM readSummary names
  where
    readSummary branch = runBranchAndFS @SummaryBranch (BranchName branch) $ do
      sheet <- fileExists @(BranchTag SummaryBranch) "sheet.md" >>= \case
        False -> return Nothing
        True  -> Just . TE.decodeUtf8 <$> readFile @(BranchTag SummaryBranch) "sheet.md"
      return (CharacterSummary branch sheet)

-- | The undo log, wire-shaped and chronological (oldest first) — shared by
--   the connection's initial push and its notifier (see
--   'Server.Writer.Session.Connection'), which re-pushes this same list
--   whenever any branch ref moves (every real write grows the log).
undoLogEntries :: SessionEffects r => Sem r [WireUndoEntry]
undoLogEntries = annotateReverts . reverse <$> listUndo

-- | Mark every entry whose recorded ref state exactly repeats an earlier
--   entry's — i.e. the point a 'Storyteller.Core.Undo.resetToUndo' landed.
--   The underlying log never branches or removes anything (see 'Undo.hs's
--   own haddock) — it just keeps growing, so a "jump back" only shows up as
--   a later entry duplicating an earlier one's snapshot. That's the entire
--   signal a client needs to draw the jump as an abandoned offshoot instead
--   of a plain continuation: everything strictly between the repeated
--   entry and this one was left behind. Pure and small enough to stay here
--   rather than in 'Storyteller.Core.Undo', which knows nothing of clients
--   or rendering.
annotateReverts :: [UndoEntry] -> [WireUndoEntry]
annotateReverts entries = zipWith toWire entries (map revertsTo [0 ..])
  where
    canonical = map (sortOn fst . undoRefs) entries
    revertsTo i =
      find (\j -> canonical !! j == canonical !! i) [i - 1, i - 2 .. 0]
    toWire entry mj = WireUndoEntry
      { weId        = unObjectHash (undoId entry)
      , weTime      = undoTime entry
      , weRevertsTo = unObjectHash . undoId . (entries !!) <$> mj
      }
