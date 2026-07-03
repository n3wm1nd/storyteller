{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Character/entity tracker agent.
--
-- Copies new atoms from a trackee branch into a tracker branch, one tracker
-- atom per trackee atom. Cross-branch refs record the source atom id.
module Storyteller.Agent.Tracker
  ( trackBranch
  , dropUntilAfterLastSynced
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Polysemy
import Polysemy.Fail
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , appendFile, readFile, fileExists )
import Storyteller.Core.Git (BranchTag(..))
import Storyteller.Core.Storage ( StoryBranch, StoryStorage, follow, storeData, readAtWithFS )
import Storyteller.Core.Types ( Tick(..), TickData(..), TickId(..), tickId, tickParent )

import Prelude hiding (appendFile, readFile)

-- | Copy atoms from @trackeeBranch@ to @trackerBranch@ for a single file pair.
--   Each new trackee atom produces exactly one tracker atom referencing it.
--   Returns the list of created tracker tick ids.
trackBranch
  :: forall trackeeBranch trackerBranch r
  .  Members '[ StoryBranch trackeeBranch
              , FileSystem     (BranchTag trackeeBranch)
              , FileSystemRead (BranchTag trackeeBranch)
              , FileSystem     (BranchTag trackerBranch)
              , FileSystemRead (BranchTag trackerBranch)
              , FileSystemWrite (BranchTag trackerBranch)
              , StoryBranch trackerBranch
              , StoryStorage
              , Fail
              ] r
  => (FilePath, FilePath)   -- ^ (source file on trackee, dest file on tracker)
  -> Sem r [TickId]
trackBranch (fromFile, toFile) = do
  trackeeTicks <- follow @trackeeBranch [] $ \acc tick ->
    (tick : acc, tickParent tick)

  syncedRefs <- follow @trackerBranch Set.empty $ \acc tick ->
    (foldr Set.insert acc (tickRefs (tickData tick)), tickParent tick)

  let contentTicks = filter ((/= Nothing) . tickParent) trackeeTicks
      newTicks     = dropUntilAfterLastSynced syncedRefs contentTicks

  mapM (copyAtom @trackeeBranch @trackerBranch fromFile toFile) newTicks

-- | Drop everything up to and including the last synced tick; return the rest.
dropUntilAfterLastSynced :: Set.Set TickId -> [Tick] -> [Tick]
dropUntilAfterLastSynced synced ticks =
  case listToMaybe [ i | (i, t) <- reverse (zip [0..] ticks)
                       , Set.member (tickId t) synced ] of
    Nothing  -> ticks
    Just idx -> drop (idx + 1) ticks

-- | Copy one trackee atom into the tracker branch.
--   Reads the file content at the trackee tick and at its parent to compute
--   the delta, appends it to the tracker branch, and commits with a ref
--   back to the source atom.
copyAtom
  :: forall trackeeBranch trackerBranch r
  .  Members '[ StoryBranch trackeeBranch
              , FileSystem     (BranchTag trackeeBranch)
              , FileSystemRead (BranchTag trackeeBranch)
              , FileSystem     (BranchTag trackerBranch)
              , FileSystemRead (BranchTag trackerBranch)
              , FileSystemWrite (BranchTag trackerBranch)
              , StoryBranch trackerBranch
              , StoryStorage
              , Fail
              ] r
  => FilePath -> FilePath -> Tick -> Sem r TickId
copyAtom fromFile toFile tick = do
  thisContent <- readAtWithFS @trackeeBranch (tickId tick) $
    readFileOrEmpty @(BranchTag trackeeBranch) fromFile
  parentContent <- case tickParent tick of
    Nothing  -> return BS.empty
    Just pid -> readAtWithFS @trackeeBranch pid
      (readFileOrEmpty @(BranchTag trackeeBranch) fromFile)

  let delta = BS.drop (BS.length parentContent) thisContent

  appendFile @(BranchTag trackerBranch) toFile delta
  storeData @trackerBranch TickData
    { tickRefs    = [tickId tick]
    , tickFields  = []
    , tickMessage = "track"
    }

readFileOrEmpty
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath -> Sem r BS.ByteString
readFileOrEmpty path =
  fileExists @project path >>= \case
    False -> return BS.empty
    True  -> readFile @project path
