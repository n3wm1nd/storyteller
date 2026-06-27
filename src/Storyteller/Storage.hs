{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.Storage
  ( -- * Branch-level effect
    StoryBranch(..)
  , store
  , drop
  , get
  , at
  , follow

    -- * Storage-level effect
  , StoryStorage(..)
  , createBranch
  , deleteBranch
  , listBranches
  , updateReferences
  ) where

import Prelude hiding (drop)
import Polysemy
import Data.Text (Text)
import Storyteller.Types (TickId, BranchName, Branch, Tick)

-- | Operations on a single branch (a named chain of ticks).
--   Branches always have at least one tick (the empty initial one from 'createBranch').
--   Rebases triggered by 'at' produce a [(TickId, TickId)] mapping
--   of old to new ids, which the caller should propagate via 'updateReferences'.
data StoryBranch m a where
  -- | Save current filesystem state as a new tick at head.
  Store :: Text         -- ^ message
        -> StoryBranch m TickId

  -- | Rewind to the previous tick. Dropping the initial tick is a no-op.
  Drop :: StoryBranch m ()

  -- | Walk the chain from head backwards. The step function returns the updated
  --   accumulator and the next tick to visit — Nothing to stop, Just id to jump
  --   (normally the parent, but can skip ahead via summary references).
  Follow :: b -> (b -> Tick -> (b, Maybe TickId)) -> StoryBranch m b

  -- | Read the tick at head.
  Get   :: StoryBranch m Tick

  -- | Run branch operations at the given position, then return to head.
  --   Returns the result of the inner action and the old→new id mapping
  --   produced by replaying everything after the target position.
  At    :: TickId
        -> m a
        -> StoryBranch m (a, [(TickId, TickId)])

makeSem ''StoryBranch

-- | Operations across all branches.
data StoryStorage m a where
  -- | Create a new empty branch, returning it with its initial tick.
  CreateBranch     :: BranchName -> StoryStorage m Branch

  -- | Delete a branch.
  DeleteBranch     :: BranchName -> StoryStorage m ()

  -- | List all branches with their current head.
  ListBranches     :: StoryStorage m [Branch]

  -- | Batch-update tick references across all branches (after a rebase).
  UpdateReferences :: [(TickId, TickId)] -> StoryStorage m ()

makeSem ''StoryStorage
