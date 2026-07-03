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
  , sneakyAt
  , readAt
  , withFS
  , atWithFS
  , sneakyAtWithFS
  , readAtWithFS
  , follow
  , fileTicks
  , ticksSince

    -- * File tick projection
  , FileTick(..)

    -- * Storage-level effect
  , StoryStorage(..)
  , createBranch
  , getBranch
  , deleteBranch
  , listBranches
  , updateReferences
  , setRef
  ) where

import Prelude hiding (drop)
import Data.List (find)
import Polysemy
import Polysemy.Fail
import Data.Text (Text)
import Storyteller.Types (TickId, BranchName(..), Branch(..), Tick, TickData(..), TickType(..), draft)

-- | A single tick entry from the file-tick projection of a branch.
--   Oldest-first when returned by 'fileTicks'.
--   Atoms have 'ftContent = Just blobSuffix'; non-atom ticks have 'Nothing'.
data FileTick = FileTick
  { ftTickId  :: Text
  , ftKind    :: Text           -- "atom", "note", "prompt", etc.
  , ftRefs    :: [Text]
  , ftFields  :: [(Text, Text)]
  , ftMessage :: Text
  , ftContent :: Maybe Text     -- Just for atoms, Nothing otherwise
  , ftParent  :: Maybe Text
  } deriving (Show, Eq)

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

  -- | Run branch operations at the given position, save/restore working tree.
  --   With @replay = True@, the tail is rewritten on top of whatever the
  --   inner action left, producing an old→new id mapping for every rewritten
  --   tick. With @replay = False@, nothing is written and the branch ref
  --   ends up back where it started — no mapping is ever produced, so the
  --   inner action's own writes (if it defies the read-only contract and
  --   calls 'Store'/'Replace' anyway) are silently orphaned when the ref is
  --   rolled back. Returns 'Left' if the target tick is not in the branch
  --   history; otherwise the inner result and the mapping.
  At      :: Bool -> TickId -> m a -> StoryBranch branch m (Either String (a, [(TickId, TickId)]))

  -- | Initialise the filesystem to the current head tick's snapshot, run the
  --   inner action, then restore the outer filesystem state.  Compose with
  --   'at' to get historical filesystem access: @at tid (withFS action)@.
  WithFS    :: m a -> StoryBranch branch m a

  -- | Walk the branch history from HEAD and extract all ticks relevant to @path@.
  --   A tick is included if it is an atom (changed the blob at that path) or
  --   if any of its refs point to an atom for that path.
  --   Returns oldest-first; the list is empty if the file has never existed.
  FileTicks :: FilePath -> StoryBranch branch m [FileTick]

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

-- | Run branch operations at the given position, save/restore working tree,
--   then replay the tail — without broadcasting the resulting id mapping via
--   'updateReferences'. For callers that need to combine this mapping with
--   something else before broadcasting once (see 'Storyteller.Edit'). Most
--   callers want 'at' instead, which broadcasts automatically; for reads
--   that make no changes worth keeping, use 'readAt' instead, which skips
--   the replay entirely rather than just deferring its broadcast.
sneakyAt :: forall branch r a. Members '[StoryBranch branch, Fail] r
         => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
sneakyAt tid action = send @(StoryBranch branch) (At True tid action) >>= either fail return

-- | Run branch operations at the given position, save/restore working tree,
--   then replay the tail, broadcasting the resulting old→new id mapping via
--   'updateReferences' so cross-branch references and tracked ids stay in
--   sync. Use 'sneakyAt' instead when the mapping needs to be combined with
--   something else before a single broadcast, or 'readAt' when the action
--   is read-only and no replay is needed at all.
at :: forall branch r a. Members '[StoryBranch branch, StoryStorage, Fail] r
   => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
at tid action = do
  result@(_, mapping) <- sneakyAt @branch tid action
  updateReferences mapping
  return result

-- | Run branch operations at the given historical position without
--   replaying the tail: no commits are written, no tick ids change, and the
--   branch ref ends up back where it started. Use for read-only inner
--   actions — a historical filesystem peek, an ancestor walk — where 'at's
--   replay (and the id mapping it produces, even if every entry maps an id
--   to itself) would be pure waste. The inner action must not itself
--   'store'/'replace': doing so still writes a real commit and moves the
--   ref, but that write is then silently discarded when the ref rolls back.
readAt :: forall branch r a. Members '[StoryBranch branch, Fail] r
       => TickId -> Sem r a -> Sem r a
readAt tid action =
  send @(StoryBranch branch) (At False tid action) >>= either fail (return . fst)

-- | Initialise the filesystem to the current head tick's snapshot, run the
--   action, then restore the outer filesystem state.
withFS :: forall branch r a. Member (StoryBranch branch) r => Sem r a -> Sem r a
withFS action = send @(StoryBranch branch) (WithFS action)

-- | Run an action at a historical tick position with the filesystem
--   initialised to that tick's snapshot.  Equivalent to @at tid (withFS action)@.
atWithFS :: forall branch r a. Members '[StoryBranch branch, StoryStorage, Fail] r
         => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
atWithFS tid action = at @branch tid (withFS @branch action)

-- | Like 'atWithFS' but skips the auto-broadcast — see 'sneakyAt'.
sneakyAtWithFS :: forall branch r a. Members '[StoryBranch branch, Fail] r
               => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
sneakyAtWithFS tid action = sneakyAt @branch tid (withFS @branch action)

-- | Read-only historical filesystem access: no replay, no mapping — see 'readAt'.
readAtWithFS :: forall branch r a. Members '[StoryBranch branch, Fail] r
             => TickId -> Sem r a -> Sem r a
readAtWithFS tid action = readAt @branch tid (withFS @branch action)

-- | Extract the file-relevant tick list for @path@ from the branch history (oldest-first).
fileTicks :: forall branch r. Member (StoryBranch branch) r => FilePath -> Sem r [FileTick]
fileTicks path = send @(StoryBranch branch) (FileTicks path)

-- | Drop everything up to and including the tick named by @since@. If it
--   isn't found (e.g. rewritten away by a move/replace), return everything —
--   the correct fallback when we can't tell what's actually new/in-flight.
ticksSince :: Maybe Text -> [FileTick] -> [FileTick]
ticksSince Nothing ticks = ticks
ticksSince (Just tid) ticks = case break ((== tid) . ftTickId) ticks of
  (_, _ : rest) -> rest
  (_, [])       -> ticks

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
