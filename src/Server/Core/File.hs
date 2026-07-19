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
  , checkpointFile
  , appendToFile
  , editFileAtom
  , deleteFileTick
  , deleteFileTicks
  , moveFileTick
  , mergeFileAtoms
  , splitFileAtoms
  , hideFileAtoms
  , unhideFileAtoms
  , chatNote
  , cycleAtomSwipe
  , referenceImage
  , readFileContent
  ) where

import Control.Monad (void)
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
import qualified Storyteller.Common.Swipe as Swipe
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Image (Image(..))
import Storyteller.Core.Runtime (Main)
import qualified Storyteller.Core.Storage as Storage
import Storyteller.Core.Storage (StoryStorage)
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
--
--   'Tick.relatedTicksOf' on top of 'Tick.fileTicksOf' -- not
--   'fileTicksOf' alone -- since the file view is the one real consumer of
--   the reference-expansion (notes, fixups, swipes rendered alongside the
--   atoms they're attached to); see 'Tick.relatedTicksOf's own Haddock
--   for why that's not folded into 'fileTicksOf' itself.
fileStateSince :: FileOpen r => FilePath -> Maybe T.Text -> Sem r Update
fileStateSince path since = fileUpdateSince since <$> runStorage @Main
  (Tick.fileTicksOf path >>= Tick.relatedTicksOf path)

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
  already <- runStorage @Main (Ops.exists path)
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
  present <- runStorage @Main (Ops.exists path)
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
  present  <- runStorage @Main (Ops.exists path)
  newTaken <- runStorage @Main (Ops.exists newPath)
  if not present
    then fail ("renameFile: no such file: " <> path)
    else if newTaken
      then fail ("renameFile: already exists: " <> newPath)
      else do
        info $ "renaming file: " <> T.pack path <> " -> " <> T.pack newPath
        void $ runStorage @Main (Ops.renameFile path newPath)

-- | Freeze @path@'s current lifetime and clone it in full onto a fresh one
-- -- see 'Storage.Ops.checkpointFile'. From here on, an atom edit\/delete
-- issued on @path@ can only reach the new copies; everything before this
-- point stays exactly as it was, just no longer reachable through ordinary
-- editing. Fails on an absent path, same reasoning as 'deleteFile'\/
-- 'renameFile's own guards -- there's no current lifetime to checkpoint.
checkpointFile :: (FileOpen r, Member Logging r) => FilePath -> Sem r ()
checkpointFile path = do
  present <- runStorage @Main (Ops.exists path)
  if not present
    then fail ("checkpointFile: no such file: " <> path)
    else do
      info $ "checkpointing file: " <> T.pack path
      void $ runStorage @Main (Ops.checkpointFile path)

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

-- | Delete a tick from the branch's chain -- generic over any kind, not
--   just atoms: 'deleteFileTicks' with one target -- safe to define this
--   way (unlike an earlier version of this function built on
--   'Storage.Ops.chainPositions', which required every target to still be
--   reachable from the *current* head, so a caller looping this over
--   several ids could pass one a previous iteration's own delete had
--   already remapped away): 'Storage.Ops.descendantsFirst' needs no head
--   and walks only a candidate's own ancestry, so it stays well-defined
--   regardless of what else has happened to the live chain since.
deleteFileTick :: FileOpen r => TickId -> Sem r ()
deleteFileTick tid = deleteFileTicks [tid]

-- | Delete a batch of ticks in one transaction (one open scope, one
--   resulting ref-move notification, instead of one per target -- pass
--   every known target together, never loop 'deleteFileTick' over them).
--   'Storage.Ops.deleteTicks' does the real work: sorts and groups by
--   connected component, then removes each component in exactly one
--   wind-back-and-replay (nesting 'at', not one independent round trip
--   per target -- each independent round trip would otherwise replay a
--   tail that includes *other* targets in this same batch, work
--   immediately thrown away the moment their own turn comes). Genuinely
--   generic over tick kind -- an annotation (note, prompt, summary
--   occurrence, ask, image) is a real chain-position tick exactly like an
--   atom is, and 'Server.Writer.File.correctGroup' already relies on this
--   to drop a stale prompt tick alongside the atoms it produced.
deleteFileTicks :: FileOpen r => [TickId] -> Sem r ()
deleteFileTicks tids = runStorage @Main (Ops.deleteTicks (map toHash tids))

-- | Move a tick to a new position in the branch's chain -- generic over
--   any kind, not just atoms: 'Storage.Ops.moveTick'\/'checkMoveOrder'
--   work purely off 'Storyteller.Core.Types.tickRefs' and chain position,
--   no kind-specific logic anywhere, so a note or prompt moves under
--   exactly the same rule an atom does -- it may only land somewhere that
--   keeps every reference it makes behind it, and doesn't jump past
--   anything that references it.
moveFileTick :: FileOpen r => TickId -> Maybe TickId -> Sem r ()
moveFileTick tid mAfter =
  void $ runStorage @Main (Ops.moveTick (toHash tid) (toHash <$> mAfter))

-- | Merge a contiguous run of one file's atoms into a single atom.
mergeFileAtoms :: FileOpen r => [TickId] -> Sem r ()
mergeFileAtoms tids =
  void $ runStorage @Main (Ops.mergeAtoms (map toHash tids))

-- | Re-run the splitter over each of the given atoms' own content, in
--   place. Processed descendants-first ('Storage.Ops.descendantsFirst'):
--   splitting an earlier atom rebases (and so remaps) everything after
--   it, including any other target still pending in this same batch, so
--   working from the descendants inward guarantees every not-yet-processed
--   id is still valid when its turn comes.
splitFileAtoms :: (FileOpen r, Member Splitter r) => [TickId] -> Sem r ()
splitFileAtoms tids = do
  ordered <- runStorage @Main (Ops.descendantsFirst (map toHash tids))
  mapM_ (splitOne . fromHash) ordered
  where
    splitOne tid = do
      tick <- runStorage @Main (Ops.readAt (toHash tid) Tick.getTypesTick)
      case fromTick @Atom tick of
        Nothing -> fail ("splitFileAtoms: not an atom: " <> T.unpack (unTickId tid))
        Just (Atom _path msg) -> do
          pieces <- splitAtoms msg
          case pieces of
            (_ : _ : _) -> void $ runStorage @Main (Ops.splitTick (toHash tid) pieces)
            _           -> return ()

-- | Hide (or unhide) a batch of atoms in place -- same descendants-first
--   discipline as 'splitFileAtoms': each rebase remaps everything after
--   it, so processing descendants first keeps every not-yet-processed
--   target's id valid when its own turn comes.
setFileAtomsHidden :: FileOpen r => [TickId] -> Bool -> Sem r ()
setFileAtomsHidden tids hidden = do
  ordered <- runStorage @Main (Ops.descendantsFirst (map toHash tids))
  mapM_ (\h -> void $ runStorage @Main (Ops.setAtomHidden h hidden)) ordered

-- | Hide a batch of atoms from an agent's ambient context without
--   deleting them -- see 'Storage.Ops.setAtomHidden'.
hideFileAtoms :: FileOpen r => [TickId] -> Sem r ()
hideFileAtoms tids = setFileAtomsHidden tids True

-- | The inverse of 'hideFileAtoms'.
unhideFileAtoms :: FileOpen r => [TickId] -> Sem r ()
unhideFileAtoms tids = setFileAtomsHidden tids False

-- | Rotate an atom's own alternates forward one step — see
--   'Storyteller.Common.Swipe.cycleSwipe'. App-agnostic (any atom, any
--   app), unlike landing a *new* alternate: that always comes from
--   whichever agent generated it, so it's Writer-specific business logic
--   (see 'Server.Writer.File.chatConverseSwipe').
cycleAtomSwipe :: FileOpen r => TickId -> Sem r ()
cycleAtomSwipe tid = void $ runStorage @Main (Swipe.cycleSwipe (toHash tid))

toHash :: TickId -> Ops.ObjectHash
toHash (TickId t) = Ops.ObjectHash t

fromHash :: Ops.ObjectHash -> TickId
fromHash (Ops.ObjectHash t) = TickId t

-- | Attach a note referencing @targets@ — zero or more atoms; empty is a
--   free-floating remark rather than a comment on any specific one.
chatNote :: FileOpen r => T.Text -> [TickId] -> Sem r ()
chatNote text targets = void $ runStorage @Main (Annotation.addNote targets text)

-- | Attach an image already sitting in the branch to @path@'s timeline, by
-- pointing an 'Image' tick at its existing asset path — unlike
-- 'Server.Writer.Branch.attachImage' (the raw-bytes HTTP upload path), this
-- deposits no new asset: for an image dragged in from the file tree, whose
-- bytes are already stored, minting a duplicate copy would just be waste.
-- Fails on an asset that isn't currently present, same tree-existence
-- discipline as 'createFile'/'renameFile'.
referenceImage :: (FileOpen r, Member Logging r) => FilePath -> FilePath -> T.Text -> Sem r ()
referenceImage path assetPath caption = do
  present <- runStorage @Main (Ops.exists assetPath)
  if not present
    then fail ("referenceImage: no such asset: " <> assetPath)
    else do
      info $ "referencing image: " <> T.pack assetPath <> " -> " <> T.pack path
      void $ runStorage @Main (Tick.storeAs (Image path assetPath caption))

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
