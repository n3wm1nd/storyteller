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
module Storyteller.Agent.Tracker
  ( trackBranch
  , dropUntilAfterLastSynced
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , fileExists, readFile, writeFile )
import Runix.Git (Git, ObjectHash(..), readBlob)
import Storyteller.Git (BranchTag(..), loadWorkingTree, FSNode(..), WorkingTree)
import Storyteller.Storage (StoryBranch, StoryStorage, follow, storeDraft)
import Storyteller.Types (Tick(..), TickDraft(..), TickId(..))

import Prelude hiding (readFile, writeFile)

trackBranch
  :: forall trackeeBranch trackeeProject trackerBranch trackerProject r
  .  ( trackeeProject ~ BranchTag trackeeBranch
     , trackerProject ~ BranchTag trackerBranch
     , Members '[ StoryBranch trackeeBranch
                , FileSystem     trackerProject
                , FileSystemRead trackerProject
                , FileSystemWrite trackerProject
                , StoryBranch trackerBranch
                , StoryStorage
                , Git
                , Fail
                ] r )
  => [FilePath]
  -> Sem r [TickId]
trackBranch files = do
  trackeeTicks <- follow @trackeeBranch [] $ \acc tick ->
    (tick : acc, tickParent tick)

  syncedRefs <- follow @trackerBranch Set.empty $ \acc tick ->
    (foldr Set.insert acc (tickRefs tick), tickParent tick)

  let newTicks = dropUntilAfterLastSynced syncedRefs trackeeTicks

  fmap concat $ mapM
    (copyAtom @trackerProject @trackerBranch files)
    newTicks

-- | Drop everything up to and including the last synced tick; return the rest.
dropUntilAfterLastSynced :: Set.Set TickId -> [Tick] -> [Tick]
dropUntilAfterLastSynced synced ticks =
  case listToMaybe [ i | (i, t) <- reverse (zip [0..] ticks)
                       , Set.member (tickId t) synced ] of
    Nothing  -> ticks
    Just idx -> drop (idx + 1) ticks

-- | Copy one trackee atom's delta into the tracker branch.
--   Reads this tick's tree and its parent's tree directly from git to compute
--   the per-file delta, then appends to the tracker branch.
copyAtom
  :: forall trackerProject trackerBranch r
  .  Members '[ FileSystem     trackerProject
              , FileSystemRead trackerProject
              , FileSystemWrite trackerProject
              , StoryBranch trackerBranch
              , StoryStorage
              , Git
              , Fail
              ] r
  => [FilePath]
  -> Tick
  -> Sem r [TickId]
copyAtom files tick = do
  thisWt   <- loadWorkingTree (ObjectHash (unTickId (tickId tick)))
  parentWt <- case tickParent tick of
    Nothing  -> return Map.empty
    Just pid -> loadWorkingTree (ObjectHash (unTickId pid))

  thisContents   <- readFiles thisWt   files
  parentContents <- readFiles parentWt files

  let deltas = [ (f, BS.drop (BS.length parentBytes) thisBytes)
               | f <- files
               , let thisBytes   = Map.findWithDefault BS.empty f thisContents
               , let parentBytes = Map.findWithDefault BS.empty f parentContents
               ]

  anyWritten <- fmap or $ mapM (appendDelta @trackerProject) deltas

  if not anyWritten
    then return []
    else do
      tid <- storeDraft @trackerBranch TickDraft
        { draftRefs    = [tickId tick]
        , draftMessage = "track from " <> unTickId (tickId tick)
        }
      return [tid]

readFiles
  :: Members '[Git, Fail] r
  => WorkingTree -> [FilePath] -> Sem r (Map FilePath ByteString)
readFiles wt files = fmap Map.fromList $ mapM (\f -> (f,) <$> readFromWT wt f) files

readFromWT :: Members '[Git, Fail] r => WorkingTree -> FilePath -> Sem r ByteString
readFromWT wt path = case Map.lookup path wt of
  Just (FSFile hash) -> readBlob hash
  _                  -> return BS.empty

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
