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

import Storyteller.Core.Types (TickId)
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
--   Always returns the old->new id mapping alongside the result —
--   'Storage.Core.ScopeState''s own remap table is populated by every
--   'Storage.Core.StoreT' computation regardless of what it did (empty
--   when nothing changed), so unlike the old @RunStorage@\/@RunStorageEdit@
--   split this replaces, there's no separate "editing" mode to opt into:
--   a caller that doesn't need the mapping just ignores it, and the
--   interpreter (see 'Storyteller.Core.Git.runBranchOpGit') always
--   broadcasts it via @updateReferences@ regardless.
data BranchOp (branch :: k) m a where
  RunStorage :: (forall n. Core.StoreM n => Core.StoreT n a) -> BranchOp branch m (a, [(TickId, TickId)])

-- | Run a "Storage.Core" computation against the named branch. The whole
--   computation — however many nested 'Core.at' calls it makes — is
--   dispatched as a single 'BranchOp' effect.
runStorage
  :: forall branch r a
  .  Member (BranchOp branch) r
  => (forall n. Core.StoreM n => Core.StoreT n a) -> Sem r (a, [(TickId, TickId)])
runStorage comp = send @(BranchOp branch) (RunStorage comp)
