{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | The operation that creates 'Storyteller.Writer.Types.Presence' ticks —
-- same relationship to 'Storyteller.Writer.Types' as
-- 'Storyteller.Common.Annotation' has to 'Storyteller.Common.Types'.
module Storyteller.Writer.Presence
  ( entered
  , enters
  , leaves
  , recordPresenceForFile
  , activeCharacters
  , activeCharactersFor
  , presentOn
  , presentAt
  ) where

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage, getBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storage.Tick (FileTick(..))
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick)
import Storyteller.Writer.Types (Character(..), Presence(..), PresenceEvent(..))

-- | Record @character@ entering, already present as of *before* the
--   branch's current position (the ambient 'BranchOp' head — see the
--   module Haddock's own note on positioning) — the backfill shape:
--   "as of right here, they were already in the scene," not "they walk in
--   right now." A caller wanting this at a specific historical point (not
--   the live head) wraps the call in
--   'Storyteller.Core.Timetravel.at'\/'Storyteller.Core.Timetravel.atAtomText'
--   itself; this function has no position parameter of its own and never
--   will — see the module Haddock.
--
--   Resolved via the raw chain parent of the current head (read straight
--   off its own 'Storage.Core.CommitData', the same field
--   'Storage.Core.at''s own tail-replay walk already follows), since there
--   is nowhere on *this* position to insert "before" other than one step
--   back on the branch's whole chain — fails outright only if head is the
--   branch's very root (nothing precedes it at all).
entered
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => FilePath -> Character -> Sem r (Maybe TickId)
entered file character =
  withCharacterBranch character $ runStorage @branch $ do
    current <- Ops.headHash
    (cd, _) <- lift (Core.readCommitTick current)
    case Core.commitParents cd of
      []      -> fail "entered: no position precedes the branch's own root"
      (p : _) -> Ops.at p (writeIfChanged p file character Enter)

-- | Record @character@ entering the scene right now, at the branch's
--   current position — the live-write shape
--   'Server.Writer.File.setPresence' already uses turn by turn, appended
--   plainly at head. See 'entered' for "already present as of here"
--   instead.
enters
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => FilePath -> Character -> Sem r (Maybe TickId)
enters = presenceEventAtHead @branch Enter

-- | Record @character@ leaving the scene right now, at the branch's
--   current position — see 'enters'.
leaves
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => FilePath -> Character -> Sem r (Maybe TickId)
leaves = presenceEventAtHead @branch Leave

presenceEventAtHead
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => PresenceEvent -> FilePath -> Character -> Sem r (Maybe TickId)
presenceEventAtHead event file character =
  withCharacterBranch character $ runStorage @branch (do
    mTrailing <- trailingPresenceFor file character
    priorActive <- case mTrailing of
      Nothing  -> presentOn file character
      Just tid -> do
        _ <- Ops.deleteTick (toHash tid)
        presentOn file character
    let wantsActive = event == Enter
    if wantsActive == priorActive
      then pure Nothing
      else Just . toTickId <$> Tick.storeAs (Presence file character event))

-- | The write 'entered' makes once positioned at the right anchor —
--   redundancy-checked the same way 'presenceEventAtHead' checks it at
--   head, just against @anchor@'s own state instead: collapses to a no-op
--   if the character is already present as of @anchor@, since nothing
--   would change. Unlike 'presenceEventAtHead', there is no "trailing tick
--   since head" to delete here — an insertion strictly before the current
--   head has no such notion, since everything already after @anchor@ is
--   real, already-written history this must never disturb.
writeIfChanged :: Ops.StoreM m => Ops.ObjectHash -> FilePath -> Character -> PresenceEvent -> Ops.StoreT m (Maybe TickId)
writeIfChanged anchor file character event = do
  priorActive <- presentAsOf anchor file character
  let wantsActive = event == Enter
  if wantsActive == priorActive
    then pure Nothing
    else Just . toTickId <$> Tick.storeAs (Presence file character event)

-- | Shared branch-existence guard every write here needs — a character is
--   referenced by branch name, not by tick id (see
--   'Storyteller.Writer.Types.Presence'), so there's no chain-walk
--   integrity check to lean on; this is the one check available.
withCharacterBranch
  :: forall r a
  .  Members '[StoryStorage, Fail] r
  => Character -> Sem r a -> Sem r a
withCharacterBranch (Character branch) action =
  getBranch branch >>= \case
    Nothing -> fail ("character branch not found: " <> T.unpack (unBranchName branch))
    Just _  -> action

toHash :: TickId -> Ops.ObjectHash
toHash (TickId t) = Ops.ObjectHash t

toTickId :: Ops.ObjectHash -> TickId
toTickId (Ops.ObjectHash t) = TickId t

-- ---------------------------------------------------------------------------
-- Bulk update: several decisions against one file, one StoreT dispatch
-- ---------------------------------------------------------------------------

-- | Apply several presence decisions to @file@ in one go — what a caller
--   retroactively tagging a whole scene's worth of entrances\/exits wants
--   (e.g. 'Storyteller.Writer.Agent.PresenceTrack.trackPresenceFor'), as a
--   single 'BranchOp' dispatch instead of one round trip per decision. Each
--   entry names the atom the decision is about directly (a
--   'Storage.Tick.FileTick', e.g. one already picked out by
--   'Storage.Tick.atomMatchingText') -- resolving *which* atom a decision
--   refers to is entirely the caller's job, this only ever places relative
--   to one already given. An @Enter@ lands strictly *before* its atom (the
--   character is already present as of that atom), a @Leave@ strictly
--   *after* it (still present *in* that atom, only gone as of the next
--   one) — the same direction 'entered'\/'enters'\/'leaves' each commit to
--   by name, decided here from the event instead since one call handles
--   both.
--
--   Applied oldest-anchor-first via nested 'Storage.Ops.at' calls, each one
--   built on the last -- the same "insert once, replay the tail forward"
--   mechanics 'entered'\/'presenceEventAtHead' each use singly, just kept
--   inside one 'StoreT' computation (and so one commit-tail replay overall)
--   rather than triggering @n@ independent replays for @n@ decisions
--   against the same file. A caller applying several decisions through
--   'entered'\/'enters'\/'leaves' one at a time would instead pay for @n@
--   separate tail rebases, each redoing work the previous one already did —
--   this is the efficient path 'Storage.Core.at's own nesting already makes
--   possible, exposed here as the one function that actually needs it.
recordPresenceForFile
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => FilePath -> [(FileTick, Character, PresenceEvent)] -> Sem r [Maybe TickId]
recordPresenceForFile file decisions = do
  mapM_ (\(_, character, _) -> withCharacterBranch character (pure ())) decisions
  runStorage @branch (mapM applyOne decisions)
  where
    applyOne (atom, character, event) = do
      let atTick = Ops.ObjectHash (ftTickId atom)
      anchor <- case event of
        Leave -> pure atTick
        Enter -> do
          (cd, _) <- lift (Core.readCommitTick atTick)
          case Core.commitParents cd of
            (p : _) -> pure p
            []      -> fail "recordPresenceForFile: no position precedes the branch's own root"
      Ops.at anchor (writeIfChanged anchor file character event)

-- | Every character active as of the end of @ticks@ -- the "list everyone
--   present" counterpart to 'presentOn's "is this one character present":
--   genuinely needs to fold every character's own state, since the answer
--   *is* the set of characters. Mirrors the frontend's
--   'activeCharacterBranches' (@lib/utils.ts@).
activeCharacters :: [FileTick] -> Set.Set Character
activeCharacters = foldl' step Set.empty
  where
    step acc ft
      | not (Tick.ftIsType @Presence ft) = acc
      | otherwise = case (lookup "character" (ftFields ft), lookup "event" (ftFields ft)) of
          (Just charT, Just "enter") -> Set.insert (Character (BranchName charT)) acc
          (Just charT, Just "leave") -> Set.delete (Character (BranchName charT)) acc
          _                          -> acc

-- | Every character currently active on @file@ -- the single source of
--   truth an agent should read to decide who's "in the scene". Off the
--   server's own tick read, not whatever the client already has in memory.
activeCharactersFor
  :: forall branch r
  .  Members '[BranchOp branch, Fail] r
  => FilePath -> Sem r [Character]
activeCharactersFor file = do
  ticks <- runStorage @branch (Tick.fileTicksOf file)
  pure (Set.toList (activeCharacters ticks))

-- | Is @character@ currently present on @file@ -- a universal, composable
--   "is this character here" building block, usable from *inside* a bare
--   'Ops.StoreT' computation already reading @file@'s own branch (unlike
--   'activeCharactersFor', which needs a full 'BranchOp' dispatch). Not
--   specific to any one caller -- 'Storyteller.Writer.Agent.Tracker' and
--   anything else asking "is X here right now" reach for this the same way.
--
--   A real transaction, not a query bolted on after one: walks one commit
--   at a time from head backward (see 'presentAsOf'), reading and decoding
--   only as many ticks as it takes to find this character's own most
--   recent word -- unlike going through 'Tick.fileTicksOf' first, whose
--   own walk reads and decodes *every* tick in the file's history before
--   handing any of them back, no matter how early the answer was actually
--   sitting. See 'presentAt' for the point-in-time variant.
presentOn :: Ops.StoreM m => FilePath -> Character -> Ops.StoreT m Bool
presentOn file character = do
  h <- Ops.headHash
  presentAsOf h file character

-- | Like 'presentOn', but starting the backward walk from an arbitrary
--   historical tick instead of head -- what a caller replaying several
--   ticks from the same file needs (see
--   'Server.Writer.Branch.onlyWhilePresent'): a single tracking pass can
--   span an Enter\/Leave gap (the character present for an early atom,
--   gone by a later one in the same pass), so asking 'presentOn' once per
--   atom would answer every one of them identically, using whatever the
--   *current* state happens to be -- wrong for exactly the atoms this
--   exists to tell apart.
presentAt :: Ops.StoreM m => TickId -> FilePath -> Character -> Ops.StoreT m Bool
presentAt (TickId tid) = presentAsOf (Ops.ObjectHash tid)

-- | The walk both 'presentOn' and 'presentAt' are built on, via
--   'Tick.findTickFrom': stop at the first presence tick naming
--   @character@ on @file@ (Enter -> present, Leave -> not) -- nothing
--   earlier can still be relevant since a later word always wins. Running
--   out of chain (root, no parent) without finding one means not present
--   either -- a fresh file starts with nobody in it (see
--   'Storyteller.Writer.Types.Presence'), so there is no earlier state to
--   fall back to, only empty. Each step is exactly one
--   'Tick.readTypesTick' -- no upfront pass over the rest of the chain, so
--   a character whose last word was one tick back costs one read, not a
--   read of everything that ever happened on @file@.
presentAsOf :: Ops.StoreM m => Ops.ObjectHash -> FilePath -> Character -> Ops.StoreT m Bool
presentAsOf start file character = fromMaybe False <$> Tick.findTickFrom start step
  where
    step _ t = case fromTick @Presence t of
      Just p | presenceFile p == file, presenceCharacter p == character ->
        Just (presenceEvent p == Enter)
      _ -> Nothing

-- | The most recent presence tick for @character@ on this file, if nothing
--   since it has actually changed the file's content — i.e. it's still the
--   "trailing" event for this character since the last atom. Walks newest
--   to oldest (via 'Tick.findTick'): stops (no trailing tick) the moment
--   an atom on @file@ is hit, since that means real content was written
--   after whatever this character's state last was; stops (found) the
--   moment a presence tick for this exact character on @file@ is hit;
--   skips anything else (atoms on other files, other characters' presence
--   ticks, prompts, notes — none of them represent "an atom happened on
--   this file" or say anything about this character) and keeps walking.
trailingPresenceFor :: Ops.StoreM m => FilePath -> Character -> Ops.StoreT m (Maybe TickId)
trailingPresenceFor file character = join <$> Tick.findTick step
  where
    step h t
      | Just a <- fromTick @Atom t, atomFile a == file = Just Nothing
      | Just p <- fromTick @Presence t
      , presenceFile p == file, presenceCharacter p == character
      = Just (Just (TickId (Ops.unObjectHash h)))
      | otherwise = Nothing
