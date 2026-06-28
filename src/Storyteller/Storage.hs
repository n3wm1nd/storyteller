{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.Storage
  ( -- * Branch-level effect
    StoryBranch(..)
  , store
  , storeDraft
  , drop
  , get
  , reset
  , at
  , follow

    -- * Storage-level effect
  , StoryStorage(..)
  , createBranch
  , getBranch
  , deleteBranch
  , listBranches
  , updateReferences
  ) where

import Prelude hiding (drop)
import Data.List (find)
import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import Storyteller.Types (TickId, BranchName(..), Branch(..), Tick, TickDraft(..), draft)

-- | Operations on a single named branch (a chain of ticks).
--   The @branch@ type parameter is a phantom used to disambiguate multiple
--   branches on the same effect stack — e.g. @StoryBranch "main"@ vs
--   @StoryBranch "draft"@, or caller-defined types like @data Main@.
data StoryBranch (branch :: k) m a where
  -- | Save current filesystem state as a new tick at head.
  --   Returns 'Left' if any file violates the append-only invariant.
  Store  :: TickDraft -> StoryBranch branch m (Either String TickId)

  -- | Rewind the tick pointer to the previous tick. Working tree is untouched.
  --   Dropping the root tick is a no-op.
  Drop   :: StoryBranch branch m ()

  -- | Walk the chain from head backwards.
  Follow :: b -> (b -> Tick -> (b, Maybe TickId)) -> StoryBranch branch m b

  -- | Read the tick at head.
  Get    :: StoryBranch branch m Tick

  -- | Discard pending working-tree changes, restoring the head tick's state.
  Reset  :: StoryBranch branch m ()

  -- | Run branch operations at the given position, save/restore working tree,
  --   then replay the tail. Returns 'Left' if the target tick is not in the
  --   branch history; otherwise the inner result and the old→new id mapping.
  At     :: TickId -> m a -> StoryBranch branch m (Either String (a, [(TickId, TickId)]))

-- | Store the current working tree as a new tick with a plain message.
--   Fails if any file is not a pure append of its previous content.
store :: forall branch r. Members '[StoryBranch branch, Fail] r => Text -> Sem r TickId
store msg = storeDraft @branch (draft msg)

-- | Store with a full 'TickDraft' — use when cross-branch refs must be declared.
storeDraft :: forall branch r. Members '[StoryBranch branch, Fail] r => TickDraft -> Sem r TickId
storeDraft d = send @(StoryBranch branch) (Store d) >>= either fail return

drop :: forall branch r. Member (StoryBranch branch) r => Sem r ()
drop = send @(StoryBranch branch) Drop

follow :: forall branch r b. Member (StoryBranch branch) r
       => b -> (b -> Tick -> (b, Maybe TickId)) -> Sem r b
follow seed step = send @(StoryBranch branch) (Follow seed step)

get :: forall branch r. Member (StoryBranch branch) r => Sem r Tick
get = send @(StoryBranch branch) Get

reset :: forall branch r. Member (StoryBranch branch) r => Sem r ()
reset = send @(StoryBranch branch) Reset

at :: forall branch r a. Members '[StoryBranch branch, Fail] r
   => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
at tid action = send @(StoryBranch branch) (At tid action) >>= either fail return

-- | Operations across all branches.
data StoryStorage m a where
  CreateBranch     :: BranchName -> StoryStorage m Branch
  DeleteBranch     :: BranchName -> StoryStorage m ()
  ListBranches     :: StoryStorage m [Branch]
  UpdateReferences :: [(TickId, TickId)] -> StoryStorage m ()

createBranch :: Member StoryStorage r => BranchName -> Sem r Branch
createBranch name = send (CreateBranch name)

deleteBranch :: Member StoryStorage r => BranchName -> Sem r ()
deleteBranch name = send (DeleteBranch name)

listBranches :: Member StoryStorage r => Sem r [Branch]
listBranches = send ListBranches

getBranch :: Member StoryStorage r => BranchName -> Sem r (Maybe Branch)
getBranch name = find ((== name) . branchName) <$> listBranches

updateReferences :: Member StoryStorage r => [(TickId, TickId)] -> Sem r ()
updateReferences mapping = send (UpdateReferences mapping)
