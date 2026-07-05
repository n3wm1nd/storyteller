{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , appendToFile
  , editFileAtom
  , deleteFileAtom
  , moveFileAtom
  , mergeFileAtoms
  , splitFileAtoms
  , chatNote
  , readFileContent
  ) where

import Control.Monad (void)
import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (info)

import Server.Core.Protocol (Update(..), toWireTick)
import Server.Core.Run (SessionEffects)
import Server.Core.Util (withBranch)

import Storyteller.Core.Append (append)
import qualified Storyteller.Core.Create as Create
import Storyteller.Common.Annotation (addNote)
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Runtime (Main)
import qualified Storyteller.Core.Storage as Storage
import Storyteller.Core.Storage (FileTick, StoryBranch, StoryStorage, fileTicks, readAt, get)
import Storyteller.Core.Edit (deleteTick, editAtom, moveTick, mergeAtoms, splitTick, chainPositions)
import Storyteller.Core.Types (TickId(..), fromTick)
import Storyteller.Core.Git (BranchTag)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import qualified Runix.FileSystem as FS

-- | The effects live once a file connection has entered its branch's scope —
--   one 'StoryBranch'/filesystem instance for the connection's whole
--   lifetime, not reopened per command.
type FileOpen r =
  Members '[ StoryBranch Main
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
fileStateSince path since = fileUpdateSince since <$> fileTicks @Main path

-- ---------------------------------------------------------------------------
-- Mutations on the already-open branch
-- ---------------------------------------------------------------------------

-- | Introduce @path@ into the tree, empty, as its own tick — distinct from
-- whatever content 'appendToFile' (or an agent) lands on it afterward.
-- Fails on a path that already has ticks — this is creation, not truncation.
createFile :: (FileOpen r, SessionEffects r) => FilePath -> Sem r ()
createFile path = do
  existing <- fileTicks @Main path
  case existing of
    [] -> do
      info $ "creating file: " <> T.pack path
      void $ Create.createFile @Main path
    _  -> fail ("createFile: already exists: " <> path)

-- | Append content to a file as a single atom — the caller (someone typing
--   and appending their own text) already chose exactly what they wanted
--   stored; paragraph-splitting is for generated prose, not for this.
appendToFile :: (FileOpen r, SessionEffects r) => FilePath -> T.Text -> Sem r ()
appendToFile path content = do
  info $ "appending to: " <> T.pack path
  void $ append @Main path content
  info $ "append done: " <> T.pack path

-- | Replace an atom's content in-place. 'editAtom' broadcasts its own
--   old->new mapping (including the edited tick's own pivot pair) via
--   'Storyteller.Core.Storage.at', so there's nothing left to do here.
editFileAtom :: FileOpen r => FilePath -> TickId -> T.Text -> Sem r ()
editFileAtom path tid content = void $ editAtom @Main tid path (TE.encodeUtf8 content)

-- | Delete an atom from the file's chain. 'deleteTick' broadcasts its own
--   mapping via 'Storyteller.Core.Storage.at', so there's nothing left to do here.
deleteFileAtom :: FileOpen r => TickId -> Sem r ()
deleteFileAtom tid = void $ deleteTick @Main tid

-- | Move an atom to a new position in the file's chain.
moveFileAtom :: FileOpen r => TickId -> Maybe TickId -> Sem r ()
moveFileAtom tid mAfter = void $ moveTick @Main tid mAfter

-- | Merge a contiguous run of one file's atoms into a single atom.
--   'mergeAtoms' broadcasts its own mapping via 'Storyteller.Core.Storage.at',
--   so there's nothing left to do here.
mergeFileAtoms :: FileOpen r => [TickId] -> Sem r ()
mergeFileAtoms tids = void $ mergeAtoms @Main tids

-- | Re-run the splitter over each of the given atoms' own content, in place.
--   Processed latest-in-chain-first: splitting an earlier atom rebases (and
--   so renumbers) everything after it, including any other target still
--   pending in this same batch, so working backward guarantees every
--   not-yet-processed id is still valid when its turn comes.
splitFileAtoms :: (FileOpen r, Member Splitter r) => [TickId] -> Sem r ()
splitFileAtoms tids = do
  positioned <- chainPositions @Main tids
  mapM_ splitOne (map fst (sortOn (Down . snd) positioned))
  where
    splitOne tid = do
      tick <- readAt @Main tid (get @Main)
      case fromTick @Atom tick of
        Nothing -> fail ("splitFileAtoms: not an atom: " <> T.unpack (unTickId tid))
        Just (Atom _path msg) -> do
          pieces <- splitAtoms msg
          case pieces of
            (_ : _ : _) -> void $ splitTick @Main tid pieces
            _           -> return ()

-- | Attach a note referencing @targets@ — zero or more atoms; empty is a
--   free-floating remark rather than a comment on any specific one.
chatNote :: FileOpen r => T.Text -> [TickId] -> Sem r ()
chatNote text targets = addNote @Main targets text

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
                    (t:_) -> Storage.ftTickId t
  }
