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
  , storeData
  , storeAs
  , replace
  , drop
  , get
  , reset
  , at
  , withFS
  , atWithFS
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
import Storyteller.Types (TickId, BranchName(..), Branch(..), Tick, TickData(..), TickType(..), draft)

-- | Operations on a single named branch (a chain of ticks).
--   The @branch@ type parameter is a phantom used to disambiguate multiple
--   branches on the same effect stack — e.g. @StoryBranch "main"@ vs
--   @StoryBranch "draft"@, or caller-defined types like @data Main@.
data StoryBranch (branch :: k) m a where
  -- | Save current filesystem state as a new tick at head.
  --   Returns 'Left' if any file violates the append-only invariant.
  Store  :: TickData -> StoryBranch branch m (Either String TickId)

  -- | Rewind the tick pointer to the previous tick. Working tree is untouched.
  --   Dropping the root tick is a no-op.
  Drop   :: StoryBranch branch m ()

  -- | Walk the chain from head backwards.
  Follow :: b -> (b -> Tick -> (b, Maybe TickId)) -> StoryBranch branch m b

  -- | Read the tick at head.
  Get    :: StoryBranch branch m Tick

  -- | Discard pending working-tree changes, restoring the head tick's state.
  Reset  :: StoryBranch branch m ()

  -- | Replace the given tick with the current working tree state, recording
  --   the supersession so that all branches referencing the old id are updated.
  --   The old tick's parent becomes the new tick's parent — the new tick takes
  --   the old one's position in the chain. Returns 'Left' if the append-only
  --   invariant is violated or the old tick is not in the branch history.
  Replace :: TickId -> TickData -> StoryBranch branch m (Either String TickId)

  -- | Run branch operations at the given position, save/restore working tree,
  --   then replay the tail. Returns 'Left' if the target tick is not in the
  --   branch history; otherwise the inner result and the old→new id mapping.
  At      :: TickId -> m a -> StoryBranch branch m (Either String (a, [(TickId, TickId)]))

  -- | Initialise the filesystem to the current head tick's snapshot, run the
  --   inner action, then restore the outer filesystem state.  Compose with
  --   'at' to get historical filesystem access: @at tid (withFS action)@.
  WithFS  :: m a -> StoryBranch branch m a

-- | Store the current working tree as a new tick with a plain message.
--   Fails if any file is not a pure append of its previous content.
store :: forall branch r. Members '[StoryBranch branch, Fail] r => Text -> Sem r TickId
store msg = storeData @branch (draft msg)

-- | Store with a full 'TickData' — use when cross-branch refs must be declared.
storeData :: forall branch r. Members '[StoryBranch branch, Fail] r => TickData -> Sem r TickId
storeData d = send @(StoryBranch branch) (Store d) >>= either fail return

-- | Store a typed tick. The draft is derived via 'toDraft'.
storeAs :: forall branch a r. (TickType a, Members '[StoryBranch branch, Fail] r) => a -> Sem r TickId
storeAs = storeData @branch . toDraft

-- | Replace an existing tick with new working tree content. The new tick takes
--   the old one's position in the chain; all cross-branch references to the
--   old tick are updated via 'UpdateReferences'.
replace :: forall branch r. Members '[StoryBranch branch, Fail] r => TickId -> TickData -> Sem r TickId
replace old d = send @(StoryBranch branch) (Replace old d) >>= either fail return

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

-- | Initialise the filesystem to the current head tick's snapshot, run the
--   action, then restore the outer filesystem state.
withFS :: forall branch r a. Member (StoryBranch branch) r => Sem r a -> Sem r a
withFS action = send @(StoryBranch branch) (WithFS action)

-- | Run an action at a historical tick position with the filesystem
--   initialised to that tick's snapshot.  Equivalent to @at tid (withFS action)@.
atWithFS :: forall branch r a. Members '[StoryBranch branch, Fail] r
         => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
atWithFS tid action = at @branch tid (withFS @branch action)

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
