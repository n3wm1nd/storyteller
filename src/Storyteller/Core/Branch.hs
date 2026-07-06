{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The Polysemy effect boundary for 'Storyteller.Core.StorageMonad'
--   ('StorageT') — backend-agnostic, mirroring how
--   'Storyteller.Core.Storage' declares @StoryStorage@ separately from its
--   git interpreter ('Storyteller.Core.Git.runStoryStorageGit'). Nothing in
--   this module mentions git: 'BranchOp's constructors carry only a closed
--   'SM.StorageT' computation, dispatched to whatever branch-scoped
--   interpreter is in scope. The one interpreter this codebase has today is
--   'Storyteller.Core.Git.runBranchOpGit', which supplies the concrete
--   'SM.MonadGit' instance (over real git) that makes running the
--   computation possible at all.
module Storyteller.Core.Branch
  ( BranchOp(..)
  , runStorage
  , runStorageEdit
  ) where

import Polysemy

import Storyteller.Core.Types (TickId)
import qualified Storyteller.Core.StorageMonad as SM

-- | A single first-order effect boundary per branch scope: both
--   constructors' arguments are rank-2-polymorphic in an independent monad
--   @n@ (never @m@, the ambient effect monad — the argument never mentions
--   the surrounding 'Sem' stack), so Polysemy interprets this with plain
--   'interpret' — no 'interpretH', no reification of a continuation. A
--   whole rebase, however many ticks deep, is one dispatch; everything
--   inside it is ordinary 'Storyteller.Core.StorageMonad.StorageT'
--   recursion, which costs one plain monadic bind per level rather than
--   one Polysemy effect interpretation per level.
--
--   'RunStorageEdit' is 'RunStorage' plus broadcasting the returned
--   mapping — kept as its own constructor (rather than a Polysemy-level
--   wrapper function around 'RunStorage') specifically so its interpreter
--   can call @updateReferences@\/@getBranch@ using the concrete branch
--   name already captured in its own closure, instead of requiring every
--   call site to know and pass that name down just for this.
data BranchOp (branch :: k) m a where
  RunStorage     :: (forall n. SM.StorageM n => SM.StorageT n a) -> BranchOp branch m a
  RunStorageEdit :: (forall n. SM.StorageM n => SM.StorageT n (a, [(TickId, TickId)])) -> BranchOp branch m (a, [(TickId, TickId)])

-- | Run a 'Storyteller.Core.StorageMonad.StorageT' computation against the
--   named branch. The whole computation — however many nested 'SM.at'
--   calls it makes — is dispatched as a single 'BranchOp' effect.
--   Doesn't broadcast any returned mapping — see 'runStorageEdit' for
--   editing operations that need to.
runStorage
  :: forall branch r a
  .  Member (BranchOp branch) r
  => (forall n. SM.StorageM n => SM.StorageT n a) -> Sem r a
runStorage comp = send @(BranchOp branch) (RunStorage comp)

-- | Like 'runStorage', but for the chain-editing operations in
--   'Storyteller.Core.StorageMonad' (@moveTick@, @mergeAtoms@,
--   @splitTick@, @deleteTick@, @editAtom@, @commitFiles@\/
--   @commitWorkingTree@, ...) that return an old->new id mapping needing
--   to be broadcast across branches: runs the computation, broadcasts its
--   mapping via @updateReferences@, then re-syncs this scope's own (head,
--   tree) state from the branch's current ref — the cascade that
--   broadcast triggers can rewrite *this* branch a second time (e.g.
--   'Storyteller.Core.StorageMonad.mergeAtoms': a tick between the merged
--   run and the original head can carry a ref into the merged range,
--   fixed up only by this cascade), which this scope's own cached state
--   has no way to observe on its own. See
--   'Storyteller.Core.Git.runBranchOpGit' for where this is actually
--   interpreted.
runStorageEdit
  :: forall branch r a
  .  Member (BranchOp branch) r
  => (forall n. SM.StorageM n => SM.StorageT n (a, [(TickId, TickId)])) -> Sem r (a, [(TickId, TickId)])
runStorageEdit comp = send @(BranchOp branch) (RunStorageEdit comp)
