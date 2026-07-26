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
  , undoLog
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Error (throw)
import Runix.Git (ObjectHash(..))

import qualified Storage.Core as Core

import Server.Core.Run (SessionEffects)
import Server.Writer.Branch (importCharacterCard)
import Server.Writer.Character (CharacterState(..), characterState)
import Server.Writer.Env (ServerEnv, requestCancel)
import Server.Writer.Session.Protocol
import Storyteller.Core.Snapshot (Snapshot, runSnapshotFS)
import Storyteller.Core.Storage (listBranches, createBranch, getBranch, deleteBranch)
import Storyteller.Core.Types (BranchName(..), branchHead, branchName, unTickId)
import Storyteller.Core.Undo (UndoEntry(..), listUndo, resetToUndo)
import Storyteller.Writer.Branches (BranchKind(..), classifyBranch)

import Prelude hiding (readFile)

runCommand :: (SessionEffects r, Member (Embed IO) r) => ServerEnv -> WS.Connection -> SessionCommand -> Sem r ()
runCommand env conn cmd = case cmd of

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
        if classifyBranch branch == Character then characterSummaries >>= push . CharacterList else return ()

  DeleteBranch _mid branch -> do
    let name = BranchName branch
    getBranch name >>= \case
      Nothing -> throw @String ("branch not found: " <> T.unpack branch)
      Just _  -> do
        deleteBranch name
        branchNames >>= push . BranchList
        if classifyBranch branch == Character then characterSummaries >>= push . CharacterList else return ()

  -- No direct confirmation event: restoring refs re-triggers the same
  -- RefMoved-driven notifier every other write does (see
  -- Server.Writer.Session.Connection's onBranchMove), which re-pushes
  -- UndoLog to every connection including this one — pushing it again here
  -- too just means the initiator doesn't have to wait on that round trip.
  UndoReset _mid entryId -> do
    resetToUndo (ObjectHash entryId)
    push =<< undoLog

  Cancel _mid targetId -> embed (() <$ requestCancel env targetId)

  -- Same "branch must not already exist" guard as 'CreateBranch' above,
  -- and the same no-round-trip BranchList/CharacterList push — an import
  -- always targets a character branch by construction (the frontend only
  -- ever sends 'character/...' here), so both are pushed unconditionally
  -- rather than gated on 'classifyBranch' the way CreateBranch/DeleteBranch
  -- gate it (those two accept any branch name, this one doesn't).
  --
  -- 'scAvatar', when present, is decoded here rather than in
  -- 'Server.Writer.Branch.importCharacterCard' -- base64 is a wire-format
  -- concern of this JSON protocol, not something the storage-layer
  -- function should know about. A malformed payload throws (caught the
  -- same way any other command failure is, see 'Server.Writer.Session.
  -- Connection') rather than silently dropping the avatar.
  ImportCharacterCard _mid branch files avatarB64 note -> do
    let name = BranchName branch
    avatar <- case avatarB64 of
      Nothing -> return Nothing
      Just b64 -> case B64.decode (TE.encodeUtf8 b64) of
        Left err    -> throw @String ("invalid avatar data: " <> err)
        Right bytes -> return (Just ("avatar.png", bytes))
    getBranch name >>= \case
      Just _  -> throw @String ("branch already exists: " <> T.unpack branch)
      Nothing -> do
        importCharacterCard name [(cfPath f, cfContent f) | f <- files] avatar note
        branchNames >>= push . BranchList
        characterSummaries >>= push . CharacterList

  where
    push = embed . WS.sendTextData conn . encode

-- | Every branch name — shared by the connection's initial push and its
--   notifier (see 'Server.Writer.Session.Connection'), which re-pushes this
--   same list whenever any branch ref moves.
branchNames :: SessionEffects r => Sem r [T.Text]
branchNames = map (unBranchName . branchName) <$> listBranches

-- | Every 'character/*' branch, each with its raw @sheet.md@ content (if
--   any) — shared by the connection's initial push and its notifier (see
--   'Server.Writer.Session.Connection'), which re-pushes this same list
--   whenever a matching branch ref moves.
--
--   Reuses 'Server.Writer.Character.characterState' rather than restating
--   it: this used to carry its own copy of the same two 'fileExists' and
--   one 'readFile' against @sheet.md@\/@avatar.png@, producing a record
--   with the same three fields under different names. Only 'csBranch'
--   genuinely differs -- the wire wants the raw branch name here, where
--   the sidebar's own view wants the display name -- so that one field is
--   taken from the caller and the two file-derived ones come from the
--   shared read.
--
--   Read at each character branch's own position rather than by opening
--   its scope. The previous shape ran 'runBranchAndFS' once per character,
--   inside a 'mapM' over every character branch -- so each iteration built
--   a whole mutable ambient working tree ('Storage.Core.freshScope'), took
--   a 'Storyteller.Core.Branch.BranchOp' scope with the full mutation
--   apparatus behind it (head re-resolution, a 'setRef' publish if head
--   moved, a closing 'flushRemaps' that can cascade and notify), and threw
--   all of it away after reading two files. None of that is reachable now:
--   'Runix.FileSystem.FileSystemWrite' isn't in the row a snapshot
--   interpreter supplies, so a push that reads the cast can't move a ref.
characterSummaries :: SessionEffects r => Sem r [CharacterSummary]
characterSummaries = do
  names <- filter ((== Character) . classifyBranch) <$> branchNames
  mapM readSummary names
  where
    readSummary branch = getBranch (BranchName branch) >>= \case
      -- Listed a moment ago by 'branchNames'; gone by now only if
      -- something deleted it in between. An empty summary is the same
      -- answer a branch with no sheet already gets.
      Nothing -> pure (CharacterSummary branch Nothing False)
      Just b  -> do
        st <- runSnapshotFS (commitOf b) (characterState @Snapshot branch)
        pure (CharacterSummary branch (charSheet st) (charHasAvatar st))

    commitOf b = Core.ObjectHash (unTickId (branchHead b))

-- | The undo log, wire-shaped and chronological (oldest first) — shared by
--   the connection's initial push and its notifier (see
--   'Server.Writer.Session.Connection'), which re-pushes this same event
--   whenever any branch ref moves (every real write grows it; a jump
--   doesn't touch it at all — see 'Storyteller.Core.Undo'). Capped to the
--   most recent 'undoLogLimit' entries: the underlying log itself is never
--   trimmed (it's still the full, real history, walked in full by every
--   'Storyteller.Core.Undo.resetToUndo'), only what crosses the wire is —
--   sending the thousands more a long session accumulates is pure waste
--   with no consumer today. Sized to comfortably overflow the timeline
--   strip's own width even on a large display (rather than to exactly fit
--   any one screen), so the row's fade-out mask always has real content to
--   fade rather than running out and hard-clipping. Revisit if a client
--   ever wants to page back further than this covers.
undoLog :: SessionEffects r => Sem r SessionEvent
undoLog = do
  entries <- reverse . take undoLogLimit <$> listUndo
  return $ UndoLog
    [ WireUndoEntry { weId = unObjectHash (undoId e), weTime = undoTime e, weKind = undoKind e }
    | e <- entries
    ]

undoLogLimit :: Int
undoLogLimit = 150
