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
  ( recordPresence
  , recordPresenceAtAtom
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

-- | Record a character entering or leaving the scene on @file@, on
--   @branch@'s chain — see 'Storyteller.Writer.Types.Presence' for why this
--   is file-scoped (an association, not a hard reference — see
--   DATA-MODEL.md's "Associations" section), not branch-global. Fails if
--   the character branch doesn't exist — this is a reference to another
--   branch, not a tick within this one, so there's no chain-walk integrity
--   check to lean on; this is the one check available.
--
--   Guards against chain noise from redundant/colliding events: an Enter
--   for an already-active character, a Leave for an already-inactive one,
--   or an Enter/Leave pair with no atom written in between (nothing
--   narrative happened while that state was nominally in effect) all
--   collapse to a no-op — 'Nothing' is returned and no new tick is written.
--   A pending tick that becomes redundant this way (the "trailing" event
--   for this character since the file's last atom, if any) is deleted
--   rather than left in the chain; see 'trailingPresenceFor'.
recordPresence
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => FilePath -> Character -> PresenceEvent -> Sem r (Maybe TickId)
recordPresence file character event =
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

-- | Like 'recordPresence', but placed relative to a specific atom
--   (@atTick@, e.g. one 'Storage.Tick.FileTick.ftTickId' from
--   'Storage.Tick.fileTicksOf') instead of always at the branch's current
--   head. What retroactive, per-atom ingestion needs (see
--   'Storyteller.Writer.Agent.PresenceTrack''s module Haddock for the full
--   argument): a character who enters partway through an
--   already-fully-written, multi-atom scene has to be marked present
--   starting exactly at the atom where they actually appear, not from the
--   file's start, or a caller replaying history at an earlier atom (via
--   'presentAt') would see them there too, when they aren't.
--
--   The direction follows from @event@, not a caller-supplied choice — an
--   @Enter@ has to land strictly *before* @atTick@ (the character is
--   already present as of that atom, so a reader replaying history at
--   @atTick@ itself must see it), an @Leave@ strictly *after* it (they're
--   still present *in* that atom, only gone as of the next one). Getting
--   this backwards would put an Enter one atom too late or a Leave one atom
--   too early, silently wrong for exactly the atom this call is about — see
--   'presentAt'.
--
--   "Before @atTick@" has nowhere on this file's own lifetime to anchor
--   after if @atTick@ is the file's very first tick (no earlier position on
--   *this file* exists) — resolved via the raw chain parent (read straight
--   off @atTick@'s own 'Storage.Core.CommitData', the same field
--   'Storage.Core.at''s own tail-replay walk already follows) instead,
--   whatever tick precedes @atTick@ on the branch overall, file-relevant or
--   not; 'Storage.Ops.at' operates on the whole chain, not just this file's
--   own ticks, so anchoring there is just as valid. Fails outright only if
--   @atTick@ is the branch's very root (nothing precedes it at all) — a
--   character can't be present before the branch itself begins.
--
--   Built on 'Storage.Ops.at' -- the same "insert a new tick right after
--   this position, replaying everything after it forward" primitive
--   'Storage.Reconcile.emitStandaloneGap' already uses for inserting a
--   standalone gap-atom mid-chain, not a fresh mechanism. Redundancy is
--   checked the same way 'recordPresence' checks it at head -- collapsed to
--   a no-op if @event@ wouldn't actually change this character's state as
--   of the resolved anchor -- except there is no "trailing tick since head"
--   to delete here: an insertion strictly before the current head has no
--   such notion, since anything already after the anchor is real,
--   already-written history this call must never disturb, only insert
--   relative to.
recordPresenceAtAtom
  :: forall branch r
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => TickId -> FilePath -> Character -> PresenceEvent -> Sem r (Maybe TickId)
recordPresenceAtAtom (TickId atTick) file character event =
  withCharacterBranch character $ runStorage @branch $ do
    anchor <- case event of
      Leave -> pure (Ops.ObjectHash atTick)
      Enter -> do
        (cd, _) <- lift (Core.readCommitTick (Ops.ObjectHash atTick))
        case Core.commitParents cd of
          (p : _) -> pure p
          []      -> fail "recordPresenceAtAtom: no position precedes the branch's own root"
    Ops.at anchor (do
      priorActive <- presentAsOf anchor file character
      let wantsActive = event == Enter
      if wantsActive == priorActive
        then pure Nothing
        else Just . toTickId <$> Tick.storeAs (Presence file character event))

-- | Shared branch-existence guard 'recordPresence'\/'recordPresenceAt' both
--   need — a character is referenced by branch name, not by tick id (see
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

-- | Every character active as of the end of @ticks@ -- the "list everyone
--   present" counterpart to 'presentOn's "is this one character present":
--   genuinely needs to fold every character's own state, since the answer
--   *is* the set of characters. Mirrors the frontend's
--   'activeCharacterBranches' (@lib/utils.ts@).
activeCharacters :: [FileTick] -> Set.Set Character
activeCharacters = foldl' step Set.empty
  where
    step acc ft
      | ftKind ft /= "presence" = acc
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
