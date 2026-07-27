{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The Polysemy effect boundary for "Storage.Core" ('StoreT') —
--   backend-agnostic, mirroring how 'Storyteller.Core.Storage' declares
--   @StoryStorage@ separately from its git interpreter
--   ('Storyteller.Core.Git.runStoryStorageGit'). Nothing in this module
--   mentions git: 'BranchOp'\'s one constructor carries only a closed
--   'Core.StoreT' computation, dispatched to whatever branch-scoped
--   interpreter is in scope. The one interpreter this codebase has today
--   is 'Storyteller.Core.Git.runBranchOpGit', which supplies the concrete
--   'Core.MonadStore' instance (over real git) that makes running the
--   computation possible at all.
module Storyteller.Core.Branch
  ( BranchOp(..)
  , runStorage

    -- * Entering a branch named at runtime
  , Branches
  , Visited
  , withBranch
  ) where

import Polysemy
import Polysemy.Scoped (Scoped, scoped)

import qualified Storage.Core as Core

import Storyteller.Core.Types (BranchName)

-- | A single first-order effect boundary per branch scope: the
--   constructor's argument is rank-2-polymorphic in an independent monad
--   @n@ (never @m@, the ambient effect monad — the argument never mentions
--   the surrounding 'Sem' stack), so Polysemy interprets this with plain
--   'interpret' — no 'interpretH', no reification of a continuation. A
--   whole rebase, however many ticks deep, is one dispatch; everything
--   inside it is ordinary 'Storage.Core.StoreT' recursion, which costs one
--   plain monadic bind per level rather than one Polysemy effect
--   interpretation per level.
--
--   This used to also return the old->new id mapping the computation
--   produced, for the caller (or the interpreter) to propagate by hand.
--   It doesn't anymore, because nothing needs propagating by hand: every
--   rename a computation makes lands in the transaction's shared remap
--   table as it happens ('Storage.Core.logRemap' bottoms out in
--   'Storyteller.Core.Storage.updateReferences'), where every reader
--   already resolves against it and the transaction boundary applies it —
--   see 'Storyteller.Core.Storage.StoryStorage'.
data BranchOp (branch :: k) m a where
  RunStorage :: (forall n. Core.StoreM n => Core.StoreT n a) -> BranchOp branch m a

-- | Run a "Storage.Core" computation against the named branch. The whole
--   computation — however many nested 'Core.at' calls it makes — is
--   dispatched as a single 'BranchOp' effect.
runStorage
  :: forall branch r a
  .  Member (BranchOp branch) r
  => (forall n. Core.StoreM n => Core.StoreT n a) -> Sem r a
runStorage comp = send @(BranchOp branch) (RunStorage comp)

-- | The capability to __enter__ a branch whose name is only known at
--   runtime -- a door, not an operation.
--
--   'BranchOp' says "I am working in /the/ branch scope that is open."
--   Plenty of code genuinely needs that and no more. But some code has to
--   visit branches it can only name at runtime, and often several of them:
--   'Storyteller.Core.ContentEffects.runCast' walks every @character/*@
--   branch; the context DSL's @in (charname | branch): ...@ resolves a
--   character branch mid-evaluation. There is no way to express that with
--   'BranchOp' alone, since a row fixes how many scopes exist at compile
--   time -- and the shape it pushed people toward instead was reaching
--   past the abstraction for the storage backend, which is how @Git@ ends
--   up in signatures that only wanted to read a file.
--
--   So this is the missing door. Holding it means exactly one thing: "I
--   may need to go work in a branch other than the one I'm in." What you
--   get on the other side is an ordinary 'BranchOp' scope -- and, via
--   'Storyteller.Core.Git.runStoryFSGit' applied inside it (itself needing
--   nothing but 'BranchOp'), ordinary 'Runix.FileSystem' vocabulary. So
--   everything written against those stays reusable against any branch,
--   any snapshot, or a test filesystem, exactly as before; only the code
--   that genuinely /travels/ declares that it does.
--
--   Compare 'Storyteller.Core.Snapshot.Snapshot', the same pattern for a
--   different destination: that one enters another /version/ and is
--   read-only, this one enters another /branch/ at its current head and
--   is not. Both are cashed in for filesystem effects; neither exposes a
--   storage primitive to the code that walks through it.
--
--   __Unparameterized, deliberately.__ "I may enter other branches" has no
--   tag in it. An earlier version was @Branches branch@, which really said
--   "...and I will address the scope as @branch@" -- a call-site concern
--   wearing the capability's name, and one that cost a separate interpreter
--   wiring per tag. Which tag a caller addresses the opened scope by is
--   'withBranch's business, not this one's; 'Visited' below is only the
--   tag this effect happens to carry internally.
type Branches = Scoped BranchName (BranchOp Visited)

-- | The tag 'Branches' carries internally -- an implementation detail of
--   the door, not something a caller has to name.
--
--   A dedicated tag rather than reusing a caller's own: the scope
--   'withBranch' opens is always a fresh one, so the tag is a label for
--   disambiguation on the row, never an identity. Reusing @Main@ would
--   both misdescribe it (the branch entered is usually a character's) and
--   force a choice between the two @Main@ tags this codebase used to have.
--   Nested entries shadow, same as
--   'Storyteller.Core.Snapshot.SnapshotTag'.
data Visited

-- | Run @action@ in @name@'s own branch scope, addressed by whichever
--   @branch@ tag the caller wants. The interpreter that discharges
--   'Branches' decides what that costs and what backs it (see
--   'Storyteller.Core.Git.runBranchesGit'); nothing here knows.
--
--   The retag is free and total: 'BranchOp'\'s parameter is a genuine
--   phantom -- 'RunStorage' carries only a closed 'Core.StoreT'
--   computation, and no field of it mentions @branch@ -- so relabelling
--   the scope cannot change what any operation does. That is what lets one
--   unparameterized 'Branches' serve both a connection handler addressing
--   its branch as @Main@ and the context DSL stepping into a character
--   branch, without a wiring line each.
--
--   Two simultaneously-live scopes stay distinct at the type level exactly
--   when their callers choose different tags, which is the property the old
--   @Branches branch@ provided by construction and this provides by
--   convention. Nesting shadows either way, so the inner scope is always
--   the one an inner call reaches.
withBranch
  :: forall branch r a
  .  Member Branches r
  => BranchName -> Sem (BranchOp branch ': r) a -> Sem r a
withBranch name = scoped @BranchName @(BranchOp Visited) name . retag
  where
    retag :: Sem (BranchOp branch ': r') x -> Sem (BranchOp Visited ': r') x
    retag = reinterpret (\case RunStorage comp -> send @(BranchOp Visited) (RunStorage comp))
