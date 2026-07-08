{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | File-level business logic that isn't specific to any one application:
-- state queries and atom-chain mutations that make sense for any app built
-- on top of the tick/atom storage model.
--
-- These functions assume the branch's storage/filesystem scope ('FileOpen')
-- is already live in the ambient stack. The connection (e.g.
-- 'Server.Writer.File.Connection') reopens that scope fresh around each
-- command, nested inside a 'Storyteller.Core.Git.withStorage' transaction, so a
-- command's writes are all-or-nothing and visible immediately, not just at
-- disconnect — these functions don't need to know that; they just see
-- 'FileOpen' as already open.
--
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.Core.File
  ( FileOpen
  , fileState
  , fileStateSince
  , createFile
  , deleteFile
  , renameFile
  , appendToFile
  , editFileAtom
  , deleteFileAtom
  , moveFileAtom
  , mergeFileAtoms
  , splitFileAtoms
  , hideFileAtoms
  , unhideFileAtoms
  , chatNote
  , readFileContent
  ) where

import Control.Monad (void)
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (Logging, info)

import Server.Core.Protocol (Update(..), toWireTick)
import Server.Core.Run (SessionEffects)
import Server.Core.Util (withBranch)

import qualified Storyteller.Core.Create as Create
import qualified Storyteller.Common.Annotation as Annotation
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Runtime (Main)
import qualified Storyteller.Core.Storage as Storage
import Storyteller.Core.Storage (StoryStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storage.Tick (FileTick)
import Storyteller.Core.Types (TickId(..), fromTick)
import Storyteller.Core.Git (BranchTag, BranchOp, runStorage)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import qualified Runix.FileSystem as FS

-- | The effects live once a file connection has entered its branch's scope —
--   one 'BranchOp'/filesystem instance for the connection's whole
--   lifetime, not reopened per command.
type FileOpen r =
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

-- | Full file state: all ticks for this path and current HEAD id.
--   An absent file (no ticks yet) is an empty 'Update' (head = "").
fileState :: FileOpen r => FilePath -> Sem r Update
fileState path = fileStateSince path Nothing

-- | File state, optionally incremental. When 'since' names a tick still
--   present in this file's chain, only ticks after it are included. When
--   'since' is 'Nothing' or no longer present (rewritten out from under it),
--   the full chain is returned.
fileStateSince :: FileOpen r => FilePath -> Maybe T.Text -> Sem r Update
fileStateSince path since = fileUpdateSince since . fst <$> runStorage @Main (Tick.fileTicksOf path)

-- ---------------------------------------------------------------------------
-- Mutations on the already-open branch
-- ---------------------------------------------------------------------------

-- | Introduce @path@ into the tree, empty, as its own tick — distinct from
-- whatever content 'appendToFile' (or an agent) lands on it afterward.
-- Fails on a path that's currently present -- this is creation, not
-- truncation. Checked against the *tree* ('Storage.Ops.exists'), not tick
-- history ('Storyteller.Core.StorageMonad.Tick.fileTicksOf'): a deleted
-- path still has ticks (deletion is a forward event, not a rebase -- see
-- 'Storyteller.Core.Create's Haddock), so it must still be creatable
-- again.
createFile :: (FileOpen r, Member Logging r) => FilePath -> Sem r ()
createFile path = do
  (already, _) <- runStorage @Main (Ops.exists path)
  if already
    then fail ("createFile: already exists: " <> path)
    else do
      info $ "creating file: " <> T.pack path
      void $ runStorage @Main (Create.createFile path)

-- | Commit @path@'s deletion -- see 'Storage.Ops.deleteFile'.
-- Fails on a path that isn't currently present -- this is deletion, not a
-- no-op on something already absent. Checked against the tree, same
-- reasoning as 'createFile's own guard.
deleteFile :: (FileOpen r, Member Logging r) => FilePath -> Sem r ()
deleteFile path = do
  (present, _) <- runStorage @Main (Ops.exists path)
  if not present
    then fail ("deleteFile: no such file: " <> path)
    else do
      info $ "deleting file: " <> T.pack path
      void $ runStorage @Main (Ops.deleteFile path)

-- | Rename @path@'s current lifetime to @newPath@ -- see
-- 'Storage.Ops.renameFile'. Fails if @path@ isn't currently present, or
-- @newPath@ already is (this is a move, not a merge/overwrite). Both
-- checked against the tree, same reasoning as 'createFile'/'deleteFile's
-- own guards.
renameFile :: (FileOpen r, Member Logging r) => FilePath -> FilePath -> Sem r ()
renameFile path newPath = do
  (present, _)    <- runStorage @Main (Ops.exists path)
  (newTaken, _)   <- runStorage @Main (Ops.exists newPath)
  if not present
    then fail ("renameFile: no such file: " <> path)
    else if newTaken
      then fail ("renameFile: already exists: " <> newPath)
      else do
        info $ "renaming file: " <> T.pack path <> " -> " <> T.pack newPath
        void $ runStorage @Main (Ops.renameFile path newPath)

-- | Append content to a file as a single atom — the caller (someone typing
--   and appending their own text) already chose exactly what they wanted
--   stored; paragraph-splitting is for generated prose, not for this.
appendToFile :: (FileOpen r, SessionEffects r) => FilePath -> T.Text -> Sem r ()
appendToFile path content = do
  info $ "appending to: " <> T.pack path
  void $ runStorage @Main (Ops.append path content)
  info $ "append done: " <> T.pack path

-- | Replace an atom's content in-place.
editFileAtom :: FileOpen r => FilePath -> TickId -> T.Text -> Sem r ()
editFileAtom _path tid content =
  void $ runStorage @Main (Ops.editAtomAt (toHash tid) content)

-- | Delete an atom from the file's chain.
deleteFileAtom :: FileOpen r => TickId -> Sem r ()
deleteFileAtom tid =
  void $ runStorage @Main (Ops.deleteTick (toHash tid))

-- | Move an atom to a new position in the file's chain.
moveFileAtom :: FileOpen r => TickId -> Maybe TickId -> Sem r ()
moveFileAtom tid mAfter =
  void $ runStorage @Main (Ops.moveTick (toHash tid) (toHash <$> mAfter))

-- | Merge a contiguous run of one file's atoms into a single atom.
mergeFileAtoms :: FileOpen r => [TickId] -> Sem r ()
mergeFileAtoms tids =
  void $ runStorage @Main (Ops.mergeAtoms (map toHash tids))

-- | Re-run the splitter over each of the given atoms' own content, in place.
--   Processed latest-in-chain-first: splitting an earlier atom rebases (and
--   so renumbers) everything after it, including any other target still
--   pending in this same batch, so working backward guarantees every
--   not-yet-processed id is still valid when its turn comes.
splitFileAtoms :: (FileOpen r, Member Splitter r) => [TickId] -> Sem r ()
splitFileAtoms tids = do
  (positioned, _) <- runStorage @Main (Ops.chainPositions (map toHash tids))
  mapM_ splitOne (map (fromHash . fst) (sortOn (Down . snd) positioned))
  where
    splitOne tid = do
      (tick, _) <- runStorage @Main (Core.readAt (toHash tid) Tick.getTypesTick)
      case fromTick @Atom tick of
        Nothing -> fail ("splitFileAtoms: not an atom: " <> T.unpack (unTickId tid))
        Just (Atom _path msg) -> do
          pieces <- splitAtoms msg
          case pieces of
            (_ : _ : _) -> void $ runStorage @Main (Ops.splitTick (toHash tid) pieces)
            _           -> return ()

-- | Hide (or unhide) a batch of atoms in place -- same "furthest-in-chain
--   first" discipline as 'splitFileAtoms': each rebase renumbers everything
--   after it, so processing back-to-front keeps every not-yet-processed
--   target's id valid when its own turn comes.
setFileAtomsHidden :: FileOpen r => [TickId] -> Bool -> Sem r ()
setFileAtomsHidden tids hidden = do
  (positioned, _) <- runStorage @Main (Ops.chainPositions (map toHash tids))
  mapM_ (\tid -> void $ runStorage @Main (Ops.setAtomHidden (toHash tid) hidden))
        (map (fromHash . fst) (sortOn (Down . snd) positioned))

-- | Hide a batch of atoms from an agent's ambient context without
--   deleting them -- see 'Storage.Ops.setAtomHidden'.
hideFileAtoms :: FileOpen r => [TickId] -> Sem r ()
hideFileAtoms tids = setFileAtomsHidden tids True

-- | The inverse of 'hideFileAtoms'.
unhideFileAtoms :: FileOpen r => [TickId] -> Sem r ()
unhideFileAtoms tids = setFileAtomsHidden tids False

toHash :: TickId -> Core.ObjectHash
toHash (TickId t) = Core.ObjectHash t

fromHash :: Core.ObjectHash -> TickId
fromHash (Core.ObjectHash t) = TickId t

-- | Attach a note referencing @targets@ — zero or more atoms; empty is a
--   free-floating remark rather than a comment on any specific one.
chatNote :: FileOpen r => T.Text -> [TickId] -> Sem r ()
chatNote text targets = void $ runStorage @Main (Annotation.addNote targets text)

-- | Raw current content of a file, for the HTTP download/embed endpoint —
--   a one-shot fetch outside any connection's lifetime, so (unlike the
--   other operations here) it opens its own branch scope rather than
--   assuming 'FileOpen' is already live.
readFileContent :: (Members '[StoryStorage, Error String, Git, Fail] r) => T.Text -> FilePath -> Sem r BS.ByteString
readFileContent branch path = withBranch @Main branch (FS.readFile @(BranchTag Main) path)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

fileUpdateSince :: Maybe T.Text -> [FileTick] -> Update
fileUpdateSince since ticks = Update
  { updateTicks = map toWireTick (Storage.ticksSince since ticks)
  , updateHead  = case reverse ticks of
                    []    -> ""
                    (t:_) -> Tick.ftTickId t
  }
