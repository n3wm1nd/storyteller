{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- | Operations across all branches (create/delete/list, cross-branch
--   reference cascade). Per-branch tick-chain operations used to live here
--   too (@StoryBranch@), but that whole family — including the higher-order
--   @At@\/@WithFS@ constructors that made it expensive to interpret — has
--   been replaced by 'Storyteller.Core.StorageMonad.StorageT' plus
--   'Storyteller.Core.Branch.BranchOp' (see PLAN-storage-monad.md).
--   'StoryStorage' itself was always first-order and untouched by that
--   migration.
module Storyteller.Core.Storage
  ( StoryStorage(..)
  , createBranch
  , getBranch
  , deleteBranch
  , listBranches
  , updateReferences
  , setRef

    -- * File tick projection helper
  , ticksSince
  ) where

import Data.List (find)
import Polysemy
import Data.Text (Text)
import Storyteller.Core.Types (TickId, BranchName(..), Branch(..))
import Storyteller.Core.StorageMonad (FileTick(..))

-- | Operations across all branches.
data StoryStorage m a where
  CreateBranch     :: BranchName -> StoryStorage m Branch
  DeleteBranch     :: BranchName -> StoryStorage m ()
  ListBranches     :: StoryStorage m [Branch]
  UpdateReferences :: [(TickId, TickId)] -> StoryStorage m ()

  -- | Set a branch's ref directly to the given tick, or delete it
  --   (@Nothing@). This is the one place a "ref" is ever named outside of
  --   the git interpreter — in storage terms it's just @BranchName -> Maybe
  --   TickId@, with no git vocabulary involved.
  SetRef :: BranchName -> Maybe TickId -> StoryStorage m ()

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

setRef :: Member StoryStorage r => BranchName -> Maybe TickId -> Sem r ()
setRef name mtid = send (SetRef name mtid)

-- | Drop everything up to and including the tick named by @since@. If it
--   isn't found (e.g. rewritten away by a move/replace), return everything —
--   the correct fallback when we can't tell what's actually new/in-flight.
ticksSince :: Maybe Text -> [FileTick] -> [FileTick]
ticksSince Nothing ticks = ticks
ticksSince (Just tid) ticks = case break ((== tid) . ftTickId) ticks of
  (_, _ : rest) -> rest
  (_, [])       -> ticks
