{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Undo tree: an append-only log of whole-repository snapshots.
--
-- Deliberately independent of 'Storyteller.Core.Storage.StoryStorage' and
-- 'Storyteller.Core.Git' — this module knows nothing about branches,
-- ticks, or the story ref convention. It works purely in 'Runix.Git'
-- vocabulary (refs and object hashes) and takes "which refs count" as a
-- parameter from its caller, so it stays reusable and, later, promotable
-- to a user-facing control surface (list/reset-to) without pulling any
-- story-specific type along with it.
--
-- 'interceptGitUndoLog' is the auto half: it wraps a computation so every
-- tracked ref write made anywhere inside it — via plain 'Runix.Git', not
-- through any particular effect — appends a new log entry right after.
-- 'withUndoLog' packages that together with 'runUndoGit' into one
-- self-contained wrapper that introduces and discharges 'Undo' internally,
-- so a caller never needs 'Undo' in its own effect row unless it wants to
-- expose the control API ('snapshotUndo'/'listUndo'/'resetToUndo') itself.
module Storyteller.Core.Undo
  ( UndoEntry(..)
  , Undo(..)
  , snapshotUndo
  , listUndo
  , resetToUndo

    -- * Interpreter / interceptor
  , runUndoGit
  , interceptGitUndoLog
  , withUndoLog
  ) where

import Data.Kind (Type)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import Polysemy
import Polysemy.Fail

import Runix.Git
import Runix.Time (Time, getCurrentTime)

-- ---------------------------------------------------------------------------
-- Effect
-- ---------------------------------------------------------------------------

-- | One entry in the undo log: every tracked ref's target at the moment it
--   was recorded, plus that moment's wall-clock time. Addressed by the
--   hash of the commit that stores it.
data UndoEntry = UndoEntry
  { undoId   :: ObjectHash
  , undoTime :: UTCTime
  , undoRefs :: [(RefName, ObjectHash)]
  } deriving (Show, Eq)

data Undo (m :: Type -> Type) a where
  -- | Record every tracked ref's current target as a new log entry.
  Snapshot :: Undo m ()
  -- | The full log, newest entry first.
  ListUndo :: Undo m [UndoEntry]
  -- | Restore every tracked ref to the state recorded by the given entry:
  --   any tracked ref that entry doesn't mention (created afterward) is
  --   deleted, and every ref it does mention is set back to that hash.
  --   Appends its own new log entry afterward — undoing an undo is
  --   recorded, not special-cased. (Done explicitly by the interpreter,
  --   not by relying on 'interceptGitUndoLog' to notice these writes: an
  --   interceptor only rewrites ops appearing in the computation it was
  --   given, and 'runUndoGit' -- necessarily applied outside/after it, to
  --   consume 'Undo' -- generates these ref writes itself while already
  --   handling 'ResetTo', too late for that same interceptor to see them.)
  ResetTo :: ObjectHash -> Undo m ()

snapshotUndo :: Member Undo r => Sem r ()
snapshotUndo = send Snapshot

listUndo :: Member Undo r => Sem r [UndoEntry]
listUndo = send ListUndo

resetToUndo :: Member Undo r => ObjectHash -> Sem r ()
resetToUndo = send . ResetTo

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

-- | Ref holding the undo log itself: a linear chain of commits, each
--   snapshotting every tracked ref at one moment, oldest at the root.
--   Outside @refs/heads/@ entirely, so no branch listing ever picks it up.
undoLogRef :: RefName
undoLogRef = RefName "refs/undo/log"

emptyTreeHash :: ObjectHash
emptyTreeHash = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- | Interpret 'Undo' against git: 'Snapshot' and 'ResetTo' write real
--   commits/refs; 'ListUndo' walks the chain back from 'undoLogRef'.
--   @prefix@ selects which refs ('Runix.Git.listRefs') a snapshot covers.
runUndoGit :: Members '[Git, Time, Fail] r => Text -> Sem (Undo : r) a -> Sem r a
runUndoGit prefix = interpret $ \case
  Snapshot -> recordUndoSnapshot prefix
  ListUndo -> walkUndoLog
  ResetTo entryId -> do
    entry   <- readUndoEntry entryId
    current <- listRefs prefix
    let restored = map fst (undoRefs entry)
    mapM_ deleteRef [ ref | (ref, _) <- current, ref `notElem` restored ]
    mapM_ (uncurry updateRef) (undoRefs entry)
    recordUndoSnapshot prefix

-- | Wrap a computation so every ref write matching @isTracked@ (create,
--   update, or delete — via plain 'Runix.Git', from anywhere inside
--   @action@) also appends a fresh undo-log entry immediately after it
--   lands. Intercepts 'Git' directly rather than any higher-level storage
--   effect, so it applies uniformly to every ref-mutating path reachable
--   from @action@ -- though not to 'runUndoGit's own writes (see 'ResetTo').
interceptGitUndoLog :: Members '[Git, Undo] r => (RefName -> Bool) -> Sem r a -> Sem r a
interceptGitUndoLog isTracked = intercept $ \case
  CreateRef ref hash | isTracked ref -> send (CreateRef ref hash) <* send Snapshot
  UpdateRef ref hash | isTracked ref -> send (UpdateRef ref hash) <* send Snapshot
  DeleteRef ref      | isTracked ref -> send (DeleteRef  ref)     <* send Snapshot
  ResolveRef ref       -> send (ResolveRef ref)
  CreateRef  ref hash  -> send (CreateRef  ref hash)
  UpdateRef  ref hash  -> send (UpdateRef  ref hash)
  DeleteRef  ref       -> send (DeleteRef  ref)
  ListRefs   p         -> send (ListRefs   p)
  ReadCommit hash      -> send (ReadCommit hash)
  WriteCommit cd       -> send (WriteCommit cd)
  ReadObject hash      -> send (ReadObject hash)
  WriteObject obj      -> send (WriteObject obj)
  LookupPath tree path -> send (LookupPath tree path)

-- | Self-contained: introduces 'Undo' and discharges it again around
--   @action@, so a caller never needs 'Undo' in its own effect row just to
--   get auto-snapshotting. @prefix@/@isTracked@ both describe the same set
--   of refs (@prefix@ for 'Runix.Git.listRefs', @isTracked@ for per-write
--   matching) — kept as two parameters since 'listRefs' and per-ref
--   predicates aren't always the same shape, but callers tracking one
--   simple prefix (as every caller today does) pass the matching pair.
withUndoLog :: Members '[Git, Time, Fail] r => Text -> (RefName -> Bool) -> Sem r a -> Sem r a
withUndoLog prefix isTracked = runUndoGit prefix . interceptGitUndoLog isTracked . raise

-- | Snapshot every tracked ref's current target into a new commit appended
--   to the undo log, parented on the log's previous entry (rootless for
--   the first). The commit's tree is always empty — this is a pointer
--   log, not a content snapshot.
recordUndoSnapshot :: Members '[Git, Time, Fail] r => Text -> Sem r ()
recordUndoSnapshot prefix = do
  now     <- getCurrentTime
  refs    <- listRefs prefix
  parent  <- resolveRef undoLogRef
  newHash <- writeCommit CommitData
    { commitParents = maybe [] (: []) parent
    , commitTree    = emptyTreeHash
    , commitMessage = encodeUndoEntry now refs
    }
  updateRef undoLogRef newHash

-- | Walk the undo log back from its head, newest first.
walkUndoLog :: Members '[Git, Fail] r => Sem r [UndoEntry]
walkUndoLog = resolveRef undoLogRef >>= maybe (return []) walkFrom
  where
    walkFrom hash = do
      cd   <- readCommit hash
      rest <- case commitParents cd of
        (p : _) -> walkFrom p
        []      -> return []
      return (decodeUndoEntry hash cd : rest)

readUndoEntry :: Members '[Git, Fail] r => ObjectHash -> Sem r UndoEntry
readUndoEntry hash = decodeUndoEntry hash <$> readCommit hash

-- | @time:<ISO8601>@ followed by one @<ref>:<hash>@ line per tracked ref.
encodeUndoEntry :: UTCTime -> [(RefName, ObjectHash)] -> Text
encodeUndoEntry now refs =
  T.unlines $ ("time:" <> T.pack (iso8601Show now)) : map refLine refs
  where
    refLine (RefName ref, hash) = ref <> ":" <> unObjectHash hash

decodeUndoEntry :: ObjectHash -> CommitData -> UndoEntry
decodeUndoEntry hash cd =
  UndoEntry
    { undoId   = hash
    , undoTime = fromMaybe (posixSecondsToUTCTime 0)
                   (listToMaybe timeLines >>= iso8601ParseM . T.unpack . T.drop 5)
    , undoRefs = [ (RefName k, ObjectHash (T.drop 1 v))
                 | l <- refLines
                 , let (k, v) = T.breakOn ":" l
                 , not (T.null v)
                 ]
    }
  where
    ls                    = T.lines (commitMessage cd)
    (timeLines, refLines) = partition ("time:" `T.isPrefixOf`) ls
    listToMaybe []      = Nothing
    listToMaybe (x : _) = Just x
