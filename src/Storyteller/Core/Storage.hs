{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- | Operations across all branches (create/delete/list, and the shared
--   remap table every rebase feeds — see 'UpdateReferences'/'ResolveTick'/
--   'FlushRemaps'). Per-branch tick-chain operations used to live here
--   too (@StoryBranch@), but that whole family — including the higher-order
--   @At@\/@WithFS@ constructors that made it expensive to interpret — has
--   been replaced by "Storage.Core"'s @StoreT@ plus
--   'Storyteller.Core.Branch.BranchOp' (see PLAN-storage-monad.md).
module Storyteller.Core.Storage
  ( StoryStorage(..)
  , createBranch
  , getBranch
  , deleteBranch
  , listBranches
  , updateReferences
  , resolveTick
  , flushRemaps
  , setRef

    -- * File tick projection helper
  , ticksSince
  ) where

import Data.List (find)
import Polysemy
import Data.Text (Text)
import Storyteller.Core.Types (TickId, BranchName(..), Branch(..))
import Storage.Tick (FileTick(..))

-- | Operations across all branches.
--
--   The three remap operations together own the one old->new tick-id
--   table a whole transaction shares (this is what "Storage.Core"'s
--   'Storage.Core.resolveHash'\/'Storage.Core.recordRemap' bottom out in
--   — see 'Storyteller.Core.Git''s @MonadStore@ instance). The division
--   of labor: 'UpdateReferences' only ever makes renames *pending*,
--   'ResolveTick' reads pending renames back, and 'FlushRemaps' marks a
--   boundary where pending renames may be physically applied. Nothing is
--   rewritten anywhere until a flush reaches the root interpreter — until
--   then a rename exists only in the table, and every read that matters
--   resolves through it.
data StoryStorage m a where
  CreateBranch     :: BranchName -> StoryStorage m Branch
  DeleteBranch     :: BranchName -> StoryStorage m ()
  ListBranches     :: StoryStorage m [Branch]

  -- | Record that each @old@ id has become @new@ — pending, in the
  --   nearest enclosing scope's table, transitively closed. No cascade,
  --   no ref write, no notification happens here, ever; those belong to
  --   'FlushRemaps'. (This used to cascade eagerly on every call — the
  --   split is what lets a transaction's many renames be applied once.)
  UpdateReferences :: [(TickId, TickId)] -> StoryStorage m ()

  -- | What @tid@ has become, per every pending (and, at the root, every
  --   already-applied) rename this scope can see — @tid@ itself if never
  --   renamed. A buffered scope that misses locally asks its parent, so
  --   nesting is transparent.
  ResolveTick :: TickId -> StoryStorage m TickId

  -- | A transaction boundary: everything pending may now be physically
  --   applied. Buffered scopes ('Storyteller.Core.Git.withStorage') treat
  --   this as a no-op — their boundary is their own exit, where they fold
  --   everything into the parent and flush *there*. The root interpreter
  --   answers it for real: one 'Storyteller.Core.Git.cascadeReplace' over
  --   every story branch with the whole pending table, ref updates for
  --   whatever that rewrites, one notification carrying the pending
  --   entries plus the cascade's own discoveries — then nothing is
  --   pending anymore. Idempotent and cheap when nothing is pending.
  FlushRemaps :: StoryStorage m ()

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

resolveTick :: Member StoryStorage r => TickId -> Sem r TickId
resolveTick tid = send (ResolveTick tid)

flushRemaps :: Member StoryStorage r => Sem r ()
flushRemaps = send FlushRemaps

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
