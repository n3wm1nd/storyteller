{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
--   is only discovered at runtime ('Storyteller.Core.ContentEffects.runCast'
--   iterating @listBranches@; the context DSL resolving @charname | branch@
--   mid-evaluation). That doesn't need a *second read vocabulary* the way
--   'Storyteller.Core.ContentEffects.TreeAccess' assumed -- it needs the
--   same one, discharged per position with the position as an ordinary
--   value argument. Callers are sequential (each scope is discharged
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
  ( Snapshot(..)
  , runSnapshotFS
  , runTextSnapshotFS
  , readSnapshotFile
  ) where

import Data.ByteString (ByteString)
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

-- | The @project@ phantom for a positioned, read-only filesystem view,
--   carrying the commit it reads at -- so @'Runix.FileSystem.getFileSystem'
--   \@Snapshot@ reports the real position, the same way
--   'Storyteller.Core.Git.BranchTag' reports a real branch name.
--
--   One tag serves every snapshot scope rather than a phantom per
--   position: two scopes are never simultaneously live (each
--   'runSnapshotFS' is discharged before the next opens), and a *nested*
--   one simply shadows its parent, which is the right @local@-style
--   semantics for @in (charname | branch): ...@ anyway. If a caller ever
--   genuinely needs two addressable at once, that's the point to add a
--   phantom -- not before.
newtype Snapshot = Snapshot Core.ObjectHash

-- | Run @action@'s reads against @commit@'s own committed tree.
--
--   'ReadFile' is a direct 'Core.readPathAt' -- a walk down the path's
--   own segments, cost proportional to its depth, never a whole-tree
--   materialization and never a history walk, since a commit already
--   carries its complete tree snapshot.
--
--   The listing operations do need the whole tree, so they load one --
--   but only when actually asked, per call. That's already the inversion
--   that matters versus a branch scope, where 'Core.freshScope'
--   materializes the tree eagerly at entry whether or not anything asks:
--   a scope that only ever reads files by name (every caller today) never
--   pays for a tree at all. It is deliberately *not* memoized across
--   calls within one scope -- that needs a 'State' layer threaded under
--   two effects, and no caller yet lists more than once, so it would be
--   machinery bought against a cost nobody has measured. If the context
--   DSL's port makes repeated listings real, the natural fix is a
--   'Polysemy.Reader.Reader' established once by the caller, not a memo
--   hidden in here.
runSnapshotFS
  :: forall r a
  .  Members '[Git, StoryStorage, Fail] r
  => Core.ObjectHash
  -> Sem (FileSystemRead Snapshot ': FileSystem Snapshot ': r) a
  -> Sem r a
runSnapshotFS commit = interpretFS . interpretRead
  where
    tree :: Members '[Git, StoryStorage, Fail] r' => Sem r' Core.WorkingTree
    tree = Core.loadWorkingTree commit

    interpretRead
      :: Members '[Git, StoryStorage, Fail] r'
      => Sem (FileSystemRead Snapshot ': r') b
      -> Sem r' b
    interpretRead = interpret $ \case
      ReadFile path -> Core.readPathAt commit path >>= \case
        Just bs -> pure (Right bs)
        -- Only the *miss* path pays for a tree, and only to tell "it's a
        -- directory" from "it isn't there" -- the same two answers
        -- 'Storyteller.Core.Git.runStoryFSGit' distinguishes, so an error
        -- message doesn't change shape depending on which interpreter
        -- happened to serve the read.
        Nothing -> do
          wt <- tree
          pure . Left $
            if FS.isDirectoryIn path wt
              then path <> ": is a directory"
              else path <> ": not found"

    interpretFS
      :: Members '[Git, StoryStorage, Fail]  r'
      => Sem (FileSystem Snapshot ': r') b
      -> Sem r' b
    interpretFS = interpret $ \case
      GetFileSystem     -> pure (Snapshot commit)
      GetCwd            -> pure (Right "/")
      ListFiles dir     -> Right . FS.listChildrenIn dir <$> tree
      IsDirectory path  -> Right . FS.isDirectoryIn path <$> tree
      -- 'existsIn' answers for files only, matching 'Storage.FS.exists';
      -- a directory counts as existing here, same as 'runStoryFSGit'.
      FileExists path   -> do
        wt <- tree
        pure (Right (FS.existsIn path wt || FS.isDirectoryIn path wt))
      -- Working-tree paths carry a leading @/@ but glob patterns are
      -- written relative, so the slash is stripped for the match only --
      -- identical to 'runStoryFSGit''s own 'Glob' case.
      Glob base pat     ->
        Right . filter (Glob.match (Glob.compile pat) . dropWhile (== '/'))
          . FS.listUnderIn base <$> tree

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
--   Unlike 'runSnapshotFS', the tree is loaded *eagerly*, once, and every
--   operation is served from it -- including 'ReadFile', which therefore
--   loses that function's O(path depth) property. That's not an oversight:
--   no question in this view can be answered without the tracked set, and
--   the tracked set costs a chain walk (worst case, for a path that never
--   had an atom, all the way to root -- precisely the binaries this is
--   filtering). Deferring it per call would mean re-walking history on
--   every read. A caller that doesn't need the policy should use
--   'runSnapshotFS' and keep the cheap reads.
runTextSnapshotFS
  :: forall r a
  .  Members '[Git, StoryStorage, Fail] r
  => Core.ObjectHash
  -> Sem (FileSystemRead Snapshot ': FileSystem Snapshot ': r) a
  -> Sem r a
runTextSnapshotFS commit action = do
  -- Seeded with an empty ambient tree rather than via 'Core.runStoreT',
  -- whose own 'Core.freshScope' would load the whole tree a second time
  -- just to populate ambient state this computation never touches.
  (visible, _) <- Core.runStoreTFrom (commit, Core.emptyWorkingTree)
                    (Query.liveWorkingTree commit)
  interpretFS visible (interpretRead visible action)
  where
    interpretRead
      :: Members '[Git, StoryStorage, Fail] r'
      => Core.WorkingTree -> Sem (FileSystemRead Snapshot ': r') b -> Sem r' b
    interpretRead visible = interpret $ \case
      ReadFile path -> case Map.lookup path visible of
        Just (Core.FSFile h) -> Core.readObject h >>= \case
          Core.BlobObject bs -> pure (Right bs)
          Core.TreeObject _  -> pure (Left (path <> ": is a directory"))
        Just Core.FSDir      -> pure (Left (path <> ": is a directory"))
        -- A hidden binary is reported exactly as a genuinely absent path:
        -- the point of this view is that it isn't there.
        Nothing              -> pure (Left (path <> ": not found"))

    -- No store access at all: given the already-filtered tree, every
    -- structural question is a pure lookup ("Storage.FS"'s own query core).
    interpretFS :: Core.WorkingTree -> Sem (FileSystem Snapshot ': r') b -> Sem r' b
    interpretFS visible = interpret $ \case
      GetFileSystem    -> pure (Snapshot commit)
      GetCwd           -> pure (Right "/")
      ListFiles dir    -> pure (Right (FS.listChildrenIn dir visible))
      IsDirectory path -> pure (Right (FS.isDirectoryIn path visible))
      FileExists path  -> pure (Right (FS.existsIn path visible || FS.isDirectoryIn path visible))
      Glob base pat    -> pure . Right
        . filter (Glob.match (Glob.compile pat) . dropWhile (== '/'))
        $ FS.listUnderIn base visible

-- | Read one file at @commit@, without opening a scope of any kind --
--   'runSnapshotFS' at its smallest useful size, for the very common
--   "resolve a branch, read one known file off it" shape
--   ('Storyteller.Core.ContentEffects.runCast', the HTTP asset endpoint).
--   'Nothing' for an absent path, rather than 'Fail': every caller of
--   this shape so far treats a missing file as a legitimate empty answer,
--   not an error.
readSnapshotFile
  :: Members '[Git, StoryStorage, Fail] r
  => Core.ObjectHash -> FilePath -> Sem r (Maybe ByteString)
readSnapshotFile commit path = Core.readPathAt commit path
