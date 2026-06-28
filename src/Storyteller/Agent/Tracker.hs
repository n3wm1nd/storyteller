{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Character/entity tracker agent.
--
-- Copies new atoms from a trackee branch into a tracker branch, using
-- cross-branch refs to determine what has already been copied.
--
-- "Already copied" is determined by refs declared in tracker ticks: if a
-- tracker tick has a trackee tick ID in its draftRefs, that atom was copied.
-- Everything in the trackee chain after the last such ref is new.
--
-- The tracker branch may freely diverge — it can have its own ticks, edits,
-- and additions. Only the sourced ref IDs matter for sync state.
--
-- Type parameters throughout:
--   @trackeeBranch@   — StoryBranch phantom for the source  (e.g. @Source@)
--   @trackeeProject@  — FS phantom for the source           (e.g. @BranchTag Source@)
--   @trackerBranch@   — StoryBranch phantom for the dest    (e.g. @Tracker@)
--   @trackerProject@  — FS phantom for the dest             (e.g. @BranchTag Tracker@)
-- where @trackeeProject ~ BranchTag trackeeBranch@
--   and @trackerProject ~ BranchTag trackerBranch@.
module Storyteller.Agent.Tracker
  ( trackBranch
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , fileExists, readFile, writeFile )
import Storyteller.Git (BranchTag(..))
import Storyteller.Storage (StoryBranch, StoryStorage, follow, at, storeDraft)
import Storyteller.Types (Tick(..), TickDraft(..), TickId(..))

import Prelude hiding (readFile, writeFile)

trackBranch
  :: forall trackeeBranch trackeeProject trackerBranch trackerProject r
  .  ( trackeeProject ~ BranchTag trackeeBranch
     , trackerProject ~ BranchTag trackerBranch
     , Members '[ StoryBranch trackeeBranch
                , FileSystem     trackeeProject
                , FileSystemRead trackeeProject
                , FileSystem     trackerProject
                , FileSystemRead trackerProject
                , FileSystemWrite trackerProject
                , StoryBranch trackerBranch
                , StoryStorage
                , Fail
                ] r )
  => [FilePath]
  -> Sem r [TickId]
trackBranch files = do
  -- Collect trackee chain oldest-first.
  trackeeTicks <- follow @trackeeBranch [] $ \acc tick ->
    (tick : acc, tickParent tick)

  -- Collect all trackee tick IDs already referenced in the tracker chain.
  syncedRefs <- follow @trackerBranch Set.empty $ \acc tick ->
    (foldr Set.insert acc (tickRefs tick), tickParent tick)

  let newTicks = dropUntilAfterLastSynced syncedRefs trackeeTicks

  fmap concat $ mapM
    (copyAtom @trackeeBranch @trackeeProject @trackerProject @trackerBranch files)
    newTicks

-- | Drop everything up to and including the last synced tick; return the rest.
dropUntilAfterLastSynced :: Set.Set TickId -> [Tick] -> [Tick]
dropUntilAfterLastSynced synced ticks =
  case listToMaybe [ i | (i, t) <- reverse (zip [0..] ticks)
                       , Set.member (tickId t) synced ] of
    Nothing  -> ticks
    Just idx -> drop (idx + 1) ticks

-- | Copy one trackee atom into the tracker branch.
--
-- Uses 'At' to position the trackee FS at this atom and its parent, computes
-- the per-file deltas (what this atom added), appends them to the tracker,
-- and commits one tracker tick with a ref to the source atom.
copyAtom
  :: forall trackeeBranch trackeeProject trackerProject trackerBranch r
  .  ( trackeeProject ~ BranchTag trackeeBranch
     , trackerProject ~ BranchTag trackerBranch
     , Members '[ StoryBranch trackeeBranch
                , FileSystem     trackeeProject
                , FileSystemRead trackeeProject
                , FileSystem     trackerProject
                , FileSystemRead trackerProject
                , FileSystemWrite trackerProject
                , StoryBranch trackerBranch
                , StoryStorage
                , Fail
                ] r )
  => [FilePath]
  -> Tick
  -> Sem r [TickId]
copyAtom files tick = do
  -- Read file contents at this atom using At on the trackee branch.
  (atContent, _) <- at @trackeeBranch (tickId tick) $
    mapM (\f -> (f,) <$> readFromBranch @trackeeProject f) files

  -- Read file contents at the parent atom (what existed before this atom).
  parentContents <- case tickParent tick of
    Nothing  -> return [ (f, BS.empty) | f <- files ]
    Just pid -> fmap fst $ at @trackeeBranch pid $
      mapM (\f -> (f,) <$> readFromBranch @trackeeProject f) files

  -- The delta for each file: bytes this atom added beyond its parent.
  let deltas = [ (f, BS.drop (BS.length parentBytes) atomBytes)
               | ((f, atomBytes), (_, parentBytes)) <- zip atContent parentContents
               ]

  -- Append deltas to tracker files.
  anyWritten <- fmap or $ mapM (appendDelta @trackerProject) deltas

  if not anyWritten
    then return []
    else do
      tid <- storeDraft @trackerBranch TickDraft
        { draftRefs    = [tickId tick]
        , draftMessage = "track from " <> unTickId (tickId tick)
        }
      return [tid]

appendDelta
  :: forall trackerProject r
  .  Members '[ FileSystem trackerProject
              , FileSystemRead trackerProject
              , FileSystemWrite trackerProject
              , Fail ] r
  => (FilePath, ByteString)
  -> Sem r Bool
appendDelta (_, delta) | BS.null delta = return False
appendDelta (path, delta) = do
  existing <- readFromBranch @trackerProject path
  writeFile @trackerProject path (existing <> delta)
  return True

readFromBranch
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath
  -> Sem r ByteString
readFromBranch path = do
  exists <- fileExists @project path
  if exists then readFile @project path else return BS.empty
