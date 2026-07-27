{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Read-only filesystem access at a position supplied as a *value* --
--   the scope-level counterpart to "Storage.Core"'s own 'Storage.Core.readAt'.
--
--   'Storage.Core' already draws this distinction inside the chain:
--   'Storage.Core.at' is a rebase (descend one parent at a time, run the
--   action, replay the whole tail, log a remap per replayed tick), while
--   'Storage.Core.readAt' declares itself read-only and, precisely
--   because of that declaration, can jump straight to its target and
--   restore afterward with nothing to rebuild. The declaration is what
--   buys the implementation, and what lets a reader of the call site
--   conclude -- locally, without reading the body -- that no id went
--   stale and no cascade will fire.
--
--   That distinction was lost one level up.
--   'Storyteller.Core.Branch.BranchOp' has exactly one constructor,
--   @RunStorage@, and its git interpreter treats every dispatch as
--   potentially mutating: it re-resolves head through the shared remap
--   table on the way in, publishes the head via @onAdvance@\/'setRef' if
--   it moved, and closes with a 'flushRemaps' transaction boundary that
--   may cascade across every branch and notify every client. A dispatch
--   that only *reads* needs none of that -- but there was no way to say
--   so, and (since @StoreM@ carries @writeCommit@\/@writeObject@\/
--   @recordRemap@) no way to be held to it either. Opening a branch scope
--   at all additionally builds a whole mutable ambient 'Core.WorkingTree'
--   up front ('Core.freshScope'), for the benefit of writes that, on a
--   read-only path, never come.
--
--   So this module is that missing declaration, made structural rather
--   than conventional:
--
--   * Only 'FileSystemRead'\/'FileSystem' are interpreted --
--     'Runix.FileSystem.FileSystemWrite' is a separate effect and simply
--     isn't in the row, so "this cannot write" is a fact about the type,
--     not a promise in a comment.
--   * No 'Storyteller.Core.Branch.BranchOp', no
--     'Core.ScopeState', no head: there is nothing here that *could*
--     advance, so nothing to publish, nothing to flush, and no
--     transaction boundary to reason about. Safe to run outside any
--     'Storyteller.Core.Git.withStorage' scope, because it contributes
--     nothing to one.
--   * No 'Core.StoreT' either. 'Core.readPathAt'\/'Core.loadWorkingTree'
--     are plain 'Core.MonadStore' operations; only the ambient-tree
--     machinery ever needed the transformer.
--
--   The other half of why this exists is positioning. @Runix.FileSystem@'s
--   own @project@ phantom is fixed when an interpreter is wired
--   (@runStoryFSGit \@branch@) -- right for "the one named branch I keep
--   reading from, always its live state," wrong for a caller whose branch
--   is only discovered at runtime ('Storyteller.Writer.Cast.knownCast'
--   iterating @listBranches@; the context DSL resolving @charname | branch@
--   mid-evaluation). That doesn't need a *second read vocabulary* the way
--   the @TreeAccess@ effect this replaced assumed -- it
--   needs the same one, discharged per position with the position as an
--   ordinary value argument. Callers are sequential (each scope is discharged
--   before the next opens), so one fixed 'Snapshot' tag serves all of
--   them; nothing here mints a type at runtime.
--
--   The two are complementary, not competing:
--
--   >                   position        content              writable
--   >  runStoryFSGit    wire-time       live ambient scope   yes
--   >  runSnapshotFS    runtime value   committed snapshot   no
--
--   A snapshot read never sees another scope's pending, uncommitted
--   ambient edits -- it reads what @commit@ actually committed. That's
--   the point of a position, not a limitation of it.
module Storyteller.Core.Snapshot
  ( -- * The capability to enter another version
    --
    -- $entering
    Snapshot
  , runSnapshotGit
    -- * Reading one, once entered
  , SnapshotTag(..)
  , runSnapshotFS
  , runTextSnapshotFS
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Polysemy
import Polysemy.Fail (Fail)

import Runix.Git (Git)
import Runix.FileSystem (FileSystem(..), FileSystemRead(..))
import qualified System.FilePath.Glob as Glob

import qualified Data.Map.Strict as Map

import qualified Storage.Core as Core
import qualified Storage.Query as Query
import qualified Storage.FS as FS
import Storyteller.Core.Storage (StoryStorage)
-- For the @'Core.MonadStore' ('Sem' r)@ instance alone (nothing else from
-- this module is used). That instance is where "an effect row that can do
-- git object I\/O is a content-addressed store" is actually established,
-- so a git-backed interpreter is exactly the kind of module entitled to
-- depend on it -- same as 'Storyteller.Core.Git.runStoryFSGit', its
-- ambient sibling, which lives in that module for the same reason.
import Storyteller.Core.Git ()

-- $entering
--
-- 'Snapshot' is the capability to __enter__ another version of the story.
-- It is not the ability to read one -- that is
-- 'Runix.FileSystem.FileSystemRead', which you get /inside/ a
-- 'runSnapshotFS', and which is what every function that merely reads
-- files should be written against. Keeping those apart is the whole point
-- of this module: a function typed @Members '[FileSystemRead p, ...]@ runs
-- against a real directory, a test filesystem, or a historical commit
-- without knowing or caring which, while a function typed @Member Snapshot
-- r@ is saying precisely one thing -- "I may need to go look at a
-- different version."
--
-- __The constructors are deliberately not exported, and neither are
-- @makeSem@ smart constructors for them.__ Ideally this effect would hand
-- back a 'Runix.FileSystem.FileSystem' interpreter through 'send' and
-- expose nothing else at all; that isn't expressible, so it exposes the
-- two primitives an interpreter genuinely needs -- read the tree as of a
-- position, read a blob by id -- and hides them. 'runSnapshotFS' and
-- 'runTextSnapshotFS', in this module, are their only callers.
--
-- That hiding is load-bearing rather than tidiness. Those two operations
-- can serve a file read, so if they were reachable, reads would start
-- getting written against them -- and any function that did so would
-- acquire a 'Snapshot' dependency for what is only a file read, welding it
-- to snapshots and to nothing else. Summarizing a character would come to
-- depend on the ability to travel between versions. Unexported
-- constructors turn "don't do that" from a comment into a type error: the
-- only door from 'Snapshot' to a file is 'runSnapshotFS'.

-- | The two primitives an interpreter needs to serve a read-only
--   filesystem as of a position, and nothing more. Internal by
--   construction -- see the note above on why the constructors stay in
--   this module.
data Snapshot (m :: Type -> Type) a where
  -- | Every path as of @commit@, structure only. @live@ applies the
  --   readable-content (atom-tracked) filter; see 'runTextSnapshotFS'.
  ReadTree :: Bool -> Core.ObjectHash -> Snapshot m Core.WorkingTree
  -- | One blob by its own id, as handed out by 'ReadTree'.
  ReadBlob :: Core.ObjectHash -> Snapshot m (Either String ByteString)

-- | The git-backed interpreter -- the /only/ place in the snapshot stack
--   that mentions 'Git' or 'StoryStorage'. Wired once, project-wide,
--   alongside 'Storyteller.Core.Git.runBranchOpGit'; everything above it
--   (every 'runSnapshotFS', and every function reading through the
--   filesystem effects it provides) is backend-agnostic.
runSnapshotGit :: Members '[Git, StoryStorage, Fail] r => Sem (Snapshot ': r) a -> Sem r a
runSnapshotGit = interpret $ \case
  ReadTree live commit
    | live      -> fst <$> Core.runStoreTFrom (commit, Core.emptyWorkingTree)
                             (Query.liveWorkingTree commit)
    | otherwise -> Core.loadWorkingTree commit
  ReadBlob h -> Core.readObject h >>= \case
    Core.BlobObject bs -> pure (Right bs)
    Core.TreeObject _  -> pure (Left "is a directory")

-- | The @project@ phantom for a positioned, read-only filesystem view,
--   carrying the commit it reads at -- so @'Runix.FileSystem.getFileSystem'
--   \@SnapshotTag@ reports the real position, the same way
--   'Storyteller.Core.Git.BranchTag' reports a real branch name.
--
--   One tag serves every snapshot scope rather than a phantom per
--   position: two scopes are never simultaneously live (each
--   'runSnapshotFS' is discharged before the next opens), and a *nested*
--   one simply shadows its parent, which is the right @local@-style
--   semantics for @in (charname | branch): ...@ anyway. If a caller ever
--   genuinely needs two addressable at once, that's the point to add a
--   phantom -- not before.
newtype SnapshotTag = SnapshotTag Core.ObjectHash
  deriving (Eq, Ord, Show)

-- | Run @action@'s reads against @commit@'s own committed tree.
--
--   Needs only 'Snapshot' -- no 'Git', no 'StoryStorage', not even 'Fail'.
--   That is the point: a caller holding 'Snapshot' can open a filesystem
--   at any position it can name, while everything it then runs inside is
--   ordinary 'Runix.FileSystem.FileSystem' \/
--   'Runix.FileSystem.FileSystemRead' code that would run just as well
--   against a live branch or a test filesystem.
--
--   The tree is read once, on entry, and every operation -- listings and
--   'ReadFile' alike -- is served from it, so a read is one blob fetch and
--   a structural question is a pure lookup ("Storage.FS"'s own query
--   core). An earlier version deferred the tree and served 'ReadFile' by
--   a direct positioned path walk, which made a read O(path depth) with no
--   tree at all; that is the better trade only for a scope that reads one
--   or two known files and never lists. Every caller that matters here
--   lists first and then reads much of what it listed, which paid for the
--   tree anyway and then re-walked per read.
runSnapshotFS
  :: forall r a
  .  Member Snapshot r
  => Core.ObjectHash
  -> Sem (FileSystemRead SnapshotTag ': FileSystem SnapshotTag ': r) a
  -> Sem r a
runSnapshotFS = snapshotFSAt False

-- | 'runSnapshotFS' with every never-atom-tracked path hidden -- an
--   uploaded portrait, or anything else that opted out of atom tracking,
--   is simply not there: absent from any listing, and a 'ReadFile' on one
--   answers "not found" exactly as a genuinely absent path does.
--
--   This is the common shape behind "I need filesystem access, but
--   whatever consumes it isn't prepared to deal with binary content."
--   Deciding which paths those are is a real storage fact -- a chain walk,
--   not something derivable from a path's own text -- so a consumer can't
--   answer it, and shouldn't have to know the question exists. Wiring this
--   instead of 'runSnapshotFS' settles it once, at the boundary, and the
--   consumer stays a plain @Members '[FileSystem project, FileSystemRead
--   project]@ function that runs anywhere.
--
--   Filtered *at @commit@*, which is the whole reason this is an
--   interpreter and not a filter stacked on an existing scope. The
--   interceptor this replaces (@ContextFilter.hideBinaryFiles@, deleted
--   along with its module) asked 'Storage.Query.atomTrackedAmong'
--   from whatever branch scope happened to be ambient -- fine for its one
--   caller, which was always filtering that same branch's live filesystem,
--   but silently the wrong question for any other position: filtering a
--   snapshot of branch B while scoped to branch A asked A's history about
--   B's paths, and filtering B's own older commit asked about B as it
--   stands now. 'Storage.Query.liveWorkingTree' resolves the tracked set
--   at the commit being read, so the answer belongs to the position it
--   describes.
--
--   The tracked set costs a chain walk (worst case, for a path that never
--   had an atom, all the way to root -- precisely the binaries this is
--   filtering), which is the one real cost difference from 'runSnapshotFS';
--   both otherwise behave identically, and both pay for it once at entry
--   rather than per call.
runTextSnapshotFS
  :: forall r a
  .  Member Snapshot r
  => Core.ObjectHash
  -> Sem (FileSystemRead SnapshotTag ': FileSystem SnapshotTag ': r) a
  -> Sem r a
runTextSnapshotFS = snapshotFSAt True

-- | 'runSnapshotFS' and 'runTextSnapshotFS' differ only in whether the
--   tree they serve has the readable-content filter applied, so they share
--   everything else here rather than restating two nearly-identical
--   interpreter pairs that could drift apart.
--
--   Both 'Snapshot' operations are used from exactly this function, which
--   is the whole reason its constructors are not exported: a file read
--   should be reached through 'Runix.FileSystem.readFile' at the
--   filesystem this provides, never by a caller sending 'ReadBlob' itself.
snapshotFSAt
  :: forall r a
  .  Member Snapshot r
  => Bool
  -> Core.ObjectHash
  -> Sem (FileSystemRead SnapshotTag ': FileSystem SnapshotTag ': r) a
  -> Sem r a
snapshotFSAt live commit action = do
  tree <- send (ReadTree live commit)
  interpretFS tree (interpretRead tree action)
  where
    interpretRead
      :: Member Snapshot r'
      => Core.WorkingTree -> Sem (FileSystemRead SnapshotTag ': r') b -> Sem r' b
    interpretRead tree = interpret $ \case
      ReadFile path -> case Map.lookup path tree of
        Just (Core.FSFile h) -> either (\e -> Left (path <> ": " <> e)) Right
                                  <$> send (ReadBlob h)
        Just Core.FSDir      -> pure (Left (path <> ": is a directory"))
        -- Under 'runTextSnapshotFS' a hidden binary is reported exactly as
        -- a genuinely absent path: the point of that view is that it isn't
        -- there.
        Nothing              -> pure (Left (path <> ": not found"))

    -- No effect access at all: given the tree, every structural question
    -- is a pure lookup ("Storage.FS"'s own query core).
    interpretFS :: Core.WorkingTree -> Sem (FileSystem SnapshotTag ': r') b -> Sem r' b
    interpretFS tree = interpret $ \case
      GetFileSystem    -> pure (SnapshotTag commit)
      GetCwd           -> pure (Right "/")
      ListFiles dir    -> pure (Right (FS.listChildrenIn dir tree))
      IsDirectory path -> pure (Right (FS.isDirectoryIn path tree))
      -- 'existsIn' answers for files only, matching 'Storage.FS.exists';
      -- a directory counts as existing here, same as 'runStoryFSGit'.
      FileExists path  -> pure (Right (FS.existsIn path tree || FS.isDirectoryIn path tree))
      -- Working-tree paths carry a leading @/@ but glob patterns are
      -- written relative, so the slash is stripped for the match only --
      -- identical to 'runStoryFSGit''s own 'Glob' case.
      Glob base pat    -> pure . Right
        . filter (Glob.match (Glob.compile pat) . dropWhile (== '/'))
        $ FS.listUnderIn base tree
