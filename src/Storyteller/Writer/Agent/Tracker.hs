{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Character/entity tracker agent.
--
-- Copies new atoms from a trackee branch into a tracker branch, one tracker
-- atom per trackee atom. Cross-branch refs record the source atom id.
module Storyteller.Writer.Agent.Tracker
  ( trackBranch
  , dropUntilAfterLastSynced
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Polysemy

import Storyteller.Core.Atom (contentFor)
import Storyteller.Core.Git (BranchOp, runStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (Tick(..), TickId(..), tickId, tickParent)

-- | Copy atoms from @trackeeBranch@ to @trackerBranch@ for a single file pair.
--   Each new trackee atom produces exactly one tracker atom referencing it.
--   Returns the list of created tracker tick ids.
trackBranch
  :: forall trackeeBranch trackerBranch r
  .  Members '[BranchOp trackeeBranch, BranchOp trackerBranch] r
  => (FilePath, FilePath)   -- ^ (source file on trackee, dest file on tracker)
  -> Sem r [TickId]
trackBranch (fromFile, toFile) = do
  (trackeeTicks, _) <- runStorage @trackeeBranch $ do
    hashes <- Core.follow [] $ \acc h _t -> (h : acc, True)
    mapM Tick.readTypesTick hashes

  -- 'Core.follow' hands back a raw 'Storage.Core.Tick', whose 'tickRefs'
  -- are 'Core.ObjectHash', not the 'TickId' this module (and
  -- 'dropUntilAfterLastSynced') deal in -- coerce at the one point they
  -- meet (same underlying 'Text', see "Storage.Tick").
  (syncedRefs, _) <- runStorage @trackerBranch $
    Core.follow Set.empty $ \acc _h t ->
      (foldr (Set.insert . coerceRef) acc (Core.tickRefs t), True)

  let contentTicks = filter ((/= Nothing) . tickParent) trackeeTicks
      newTicks     = dropUntilAfterLastSynced syncedRefs contentTicks

  mapM (copyAtom @trackerBranch fromFile toFile) newTicks
  where
    coerceRef (Core.ObjectHash h) = TickId h

-- | Drop everything up to and including the last synced tick; return the rest.
dropUntilAfterLastSynced :: Set.Set TickId -> [Tick] -> [Tick]
dropUntilAfterLastSynced synced ticks =
  case listToMaybe [ i | (i, t) <- reverse (zip [0..] ticks)
                       , Set.member (tickId t) synced ] of
    Nothing  -> ticks
    Just idx -> drop (idx + 1) ticks

-- | Copy one trackee atom into the tracker branch.
--   The trackee tick's own contribution lives verbatim in its commit
--   message (see 'Storyteller.Core.Atom.contentFor'), so no filesystem
--   access to the trackee branch is needed to recover it. The tracker's own
--   copy is committed as a real 'Atom' too (same invariant applies on this
--   side of the copy), with the cross-branch ref folded onto that draft
--   rather than dropped in favor of a plain, untagged message -- via
--   'Ops.addAtomWithRefs' rather than the raw working-tree primitives.
copyAtom
  :: forall trackerBranch r
  .  Member (BranchOp trackerBranch) r
  => FilePath -> FilePath -> Tick -> Sem r TickId
copyAtom fromFile toFile tick = do
  let content = contentFor fromFile tick
      ref     = Core.ObjectHash (unTickId (tickId tick))
  (newHash, _) <- runStorage @trackerBranch (Ops.addAtomWithRefs [ref] toFile content)
  return (TickId (Core.unObjectHash newHash))
