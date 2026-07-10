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
  , activeCharactersFor
  , presentOn
  , presentAt
  ) where

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
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Types (Presence(..), PresenceEvent(..))

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
  => FilePath -> BranchName -> PresenceEvent -> Sem r (Maybe TickId)
recordPresence file character event =
  getBranch character >>= \case
    Nothing -> fail ("character branch not found: " <> T.unpack (unBranchName character))
    Just _  -> do
      (ticks, _) <- runStorage @branch (Tick.fileTicksOf file)
      priorActive <- case trailingPresenceFor character ticks of
        Nothing  -> pure (presentIn character ticks)
        Just tid -> do
          _           <- runStorage @branch (Ops.deleteTick (toHash tid))
          (ticks', _) <- runStorage @branch (Tick.fileTicksOf file)
          pure (presentIn character ticks')
      let wantsActive = event == Enter
      if wantsActive == priorActive
        then pure Nothing
        else Just . fst <$> runStorage @branch (fmap toTickId (Tick.storeAs (Presence file character event)))
  where
    toHash (TickId t) = Core.ObjectHash t
    toTickId (Core.ObjectHash t) = TickId t

-- | Whether @character@ is present as of the end of @ticks@ (already
--   whatever file *and* point in that file's history the caller cares
--   about -- see 'presentOn'\/'presentAt', which supply both), found by
--   walking backward until the first presence tick that mentions this
--   character specifically. Enter means present, Leave means not; running
--   out of ticks without finding one means not present either -- a fresh
--   file starts with nobody in it (see 'Storyteller.Writer.Types.Presence'),
--   so there is no earlier state to fall back to, only empty.
--
--   Deliberately single-character: answering for one character only ever
--   needs that character's own last word, not a fold that has to also
--   track every other character's state to get there (contrast
--   'activeCharacters', which genuinely needs all of them, for the
--   "list everyone present" query 'activeCharactersFor' answers).
presentIn :: BranchName -> [FileTick] -> Bool
presentIn character = go . reverse
  where
    go [] = False
    go (ft : rest)
      | ftKind ft /= "presence"                                        = go rest
      | lookup "character" (ftFields ft) /= Just (unBranchName character) = go rest
      | otherwise = lookup "event" (ftFields ft) == Just "enter"

-- | Every character active as of the end of @ticks@ -- the "list everyone
--   present" counterpart to 'presentIn's "is this one character present":
--   genuinely needs to fold every character's own state, since the answer
--   *is* the set of characters. Mirrors the frontend's
--   'activeCharacterBranches' (@lib/utils.ts@).
activeCharacters :: [FileTick] -> Set.Set BranchName
activeCharacters = foldl' step Set.empty
  where
    step acc ft
      | ftKind ft /= "presence" = acc
      | otherwise = case (lookup "character" (ftFields ft), lookup "event" (ftFields ft)) of
          (Just charT, Just "enter") -> Set.insert (BranchName charT) acc
          (Just charT, Just "leave") -> Set.delete (BranchName charT) acc
          _                          -> acc

-- | Every character currently active on @file@ -- the single source of
--   truth an agent should read to decide who's "in the scene". Off the
--   server's own tick read, not whatever the client already has in memory.
activeCharactersFor
  :: forall branch r
  .  Members '[BranchOp branch, Fail] r
  => FilePath -> Sem r [BranchName]
activeCharactersFor file = do
  (ticks, _) <- runStorage @branch (Tick.fileTicksOf file)
  pure (Set.toList (activeCharacters ticks))

-- | Is @character@ currently present on @file@ -- a universal, composable
--   "is this character here" building block, usable from *inside* a bare
--   'Core.StoreT' computation already reading @file@'s own branch (unlike
--   'activeCharactersFor', which needs a full 'BranchOp' dispatch). Not
--   specific to any one caller -- 'Storyteller.Writer.Agent.Tracker' and
--   anything else asking "is X here right now" reach for this the same way.
--   As of head, same as 'activeCharactersFor'; see 'presentAt' for the
--   point-in-time variant a caller walking several historical ticks needs
--   instead.
presentOn :: Core.StoreM m => FilePath -> Core.StoreT m (BranchName -> Bool)
presentOn file = do
  ticks <- Tick.fileTicksOf file
  pure (`presentIn` ticks)

-- | Like 'presentOn', but answering for an arbitrary historical tick on
--   @file@ instead of head -- what a caller replaying several ticks from
--   the same file needs (see 'Server.Writer.Branch.onlyWhilePresent'):
--   a single tracking pass can span an Enter\/Leave gap (the character
--   present for an early atom, gone by a later one in the same pass), so
--   asking 'presentOn' once per atom would answer every one of them
--   identically, using whatever the *final* state happens to be -- wrong
--   for exactly the atoms this exists to tell apart. Folding only the
--   presence ticks at-or-before each queried tick's own position is what
--   makes the answer differ within a single pass the way it should.
presentAt :: Core.StoreM m => FilePath -> Core.StoreT m (TickId -> BranchName -> Bool)
presentAt file = do
  ticks <- Tick.fileTicksOf file
  pure (\tid character -> presentIn character (upToAndIncluding tid ticks))
  where
    upToAndIncluding (TickId target) = go
      where
        go [] = []
        go (ft : rest)
          | ftTickId ft == target = [ft]
          | otherwise             = ft : go rest

-- | The most recent presence tick for @character@ on this file, if nothing
--   since it has actually changed the file's content — i.e. it's still the
--   "trailing" event for this character since the last atom. Walks newest
--   to oldest: stops (no trailing tick) the moment an atom is hit, since
--   that means real content was written after whatever this character's
--   state last was; stops (found) the moment a presence tick for this
--   exact character is hit; skips anything else (other characters'
--   presence ticks, prompts, notes — none of them represent "an atom
--   happened" or say anything about this character).
trailingPresenceFor :: BranchName -> [FileTick] -> Maybe TickId
trailingPresenceFor character ticks = go (reverse ticks)
  where
    go [] = Nothing
    go (ft : rest)
      | ftKind ft == "atom" = Nothing
      | ftKind ft == "presence"
      , lookup "character" (ftFields ft) == Just (unBranchName character)
      = Just (TickId (ftTickId ft))
      | otherwise = go rest
