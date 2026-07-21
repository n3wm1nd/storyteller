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
  , activeCharacters
  , activeCharactersFor
  , presentOn
  , presentAt
  ) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage, getBranch)
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
recordPresence file character@(Character branch) event =
  getBranch branch >>= \case
    Nothing -> fail ("character branch not found: " <> T.unpack (unBranchName branch))
    Just _  -> runStorage @branch (do
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
  where
    toHash (TickId t) = Ops.ObjectHash t
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
