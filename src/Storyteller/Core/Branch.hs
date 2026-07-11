{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  ) where

import Polysemy

import qualified Storage.Core as Core

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
