{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Character/entity tracker agent.
--
-- Copies new atoms from a trackee branch into a tracker branch, one tracker
-- atom per trackee atom. Cross-branch refs record the source atom id.
module Storyteller.Writer.Agent.Tracker
  ( trackBranch
  ) where

import Data.Maybe (catMaybes, mapMaybe)
import Polysemy

import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, runStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (Tick(..), TickId(..), fromTick, tickId, tickParent)

-- | Copy atoms from @trackeeBranch@ into a single @toFile@ on
--   @trackerBranch@, after running each candidate tick through
--   @atomFilter@ -- 'Nothing' drops it, 'Just' keeps it (optionally
--   changed) as what actually gets copied. Runs in the trackee branch's
--   own 'BranchOp' scope, so it can read anything about the trackee (e.g.
--   'Storyteller.Writer.Presence.presentOn', for a caller that only wants
--   to track ticks written while some character was present) without a
--   second dispatch. Each surviving tick produces exactly one tracker
--   atom referencing it. Returns the list of created tracker tick ids.
--
--   @onlyFile@ restricts to one trackee file (the shape a manual,
--   user-triggered track wants -- limited to whatever's actually open, so
--   there's nothing surprising to reason about); 'Nothing' tracks every
--   file on the trackee branch into the same @toFile@ (a running journal
--   assembled from every scene, the shape an automatic, write-triggered
--   track wants -- so nothing written to a chapter the user has since
--   moved on from is ever lost).
--
--   Only ever walks as far as whatever's actually new. The tracker
--   branch's own most recent 'Storage.Core.Atom' carrying a ref *is* the
--   last-synced trackee tick -- every tracker atom this module ever
--   produces is appended in the same relative order the trackee ticks
--   were found in ('copyAtom' below always extends the tracker's current
--   head), so there's no need to accumulate every ref the tracker branch
--   has ever seen (as an earlier version of this did) just to test
--   membership: the single newest one already settles it. Walking the
--   tracker branch back to find it skips right over any of the tracker's
--   own unrelated ticks (author notes, fixups -- anything that isn't
--   itself a 'Storage.Core.Atom' with a ref) without being confused by
--   them, since those never carry a cross-branch ref pointing back at the
--   trackee in the first place. Walking the trackee branch back then
--   stops the instant it reaches that hash, rather than reading every
--   tick the trackee has ever recorded -- so a normal, incremental track
--   call costs only whatever's actually accumulated since the last one,
--   on both sides, not the full history of either branch. The one case
--   that *does* walk deep: nothing's ever been synced yet (first run), or
--   the most recent tracker ref doesn't actually appear in this trackee
--   branch at all (tracking from a different source than last time,  or
--   some other mismatch) -- either way, walking all the way to root and
--   concluding "everything is new" is the correct, if expensive, answer.
trackBranch
  :: forall trackeeBranch trackerBranch r
  .  Members '[BranchOp trackeeBranch, BranchOp trackerBranch] r
  => Maybe FilePath
     -- ^ restrict to one trackee file; 'Nothing' tracks every file
  -> (forall m. Core.StoreM m => Tick -> Core.StoreT m (Maybe Tick))
     -- ^ per-tick filter\/transform, run against the trackee branch
  -> FilePath   -- ^ destination file on the tracker branch
  -> Sem r [TickId]
trackBranch onlyFile atomFilter toFile = do
  lastSynced <- runStorage @trackerBranch $
    Core.follow Nothing $ \acc _h t -> case t of
      Core.Atom (r : _) _ _ _ -> (Just r, False)
      _                       -> (acc, True)

  -- FIXME(tracker-resync): a dangling 'lastSynced' makes this walk run to
  -- root and conclude "everything is new" -- the Haddock's own first-run
  -- case, and the expense is acceptable there. What isn't: the copy loop
  -- below then re-copies the whole filtered history into an
  -- already-populated journal as duplicates. Remaps cover most id churn
  -- (the ref is read resolved, and the boundary cascade rewrites it
  -- physically), but 'Ops.deleteTick' of exactly the last-synced trackee
  -- tick logs no replacement, so its ref goes dangling for good. Before
  -- bulk-copying on a miss, check candidates against refs already present
  -- in @toFile@ -- or sync nothing and surface the mismatch.
  --
  -- One read per commit, not two: 'Tick.newTypesTicksSince' decodes each
  -- typed tick inline during the same backward walk that finds
  -- 'lastSynced', via 'Storage.Core.followC' -- no separate hash
  -- collection pass for 'readTypesTick' to redundantly re-read afterward.
  newTicks <- runStorage @trackeeBranch (Tick.newTypesTicksSince lastSynced)

  let contentTicks = filter ((/= Nothing) . tickParent) newTicks
      onOwnFile t   = case fromTick @Atom t of
        Just (Atom file _) | maybe True (== file) onlyFile -> Just (file, t)
        _                                                    -> Nothing
      candidates    = mapMaybe onOwnFile contentTicks

  kept <- runStorage @trackeeBranch
    (catMaybes <$> mapM (\(file, t) -> fmap (file,) <$> atomFilter t) candidates)
  mapM (\(file, t) -> copyAtom @trackerBranch file toFile t) kept

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
  newHash <- runStorage @trackerBranch (Ops.addAtomWithRefs [ref] toFile content)
  return (TickId (Core.unObjectHash newHash))
