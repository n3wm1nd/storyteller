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
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick, tickParent)
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
presentIn :: Character -> [FileTick] -> Bool
presentIn (Character branch) = go . reverse
  where
    go [] = False
    go (ft : rest)
      | ftKind ft /= "presence"                                        = go rest
      | lookup "character" (ftFields ft) /= Just (unBranchName branch) = go rest
      | otherwise = lookup "event" (ftFields ft) == Just "enter"

-- | Every character active as of the end of @ticks@ -- the "list everyone
--   present" counterpart to 'presentIn's "is this one character present":
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
  (ticks, _) <- runStorage @branch (Tick.fileTicksOf file)
  pure (Set.toList (activeCharacters ticks))

-- | Is @character@ currently present on @file@ -- a universal, composable
--   "is this character here" building block, usable from *inside* a bare
--   'Core.StoreT' computation already reading @file@'s own branch (unlike
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
presentOn :: Core.StoreM m => FilePath -> Character -> Core.StoreT m Bool
presentOn file character = do
  h <- Core.headHash
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
presentAt :: Core.StoreM m => TickId -> FilePath -> Character -> Core.StoreT m Bool
presentAt (TickId tid) = presentAsOf (Core.ObjectHash tid)

-- | The walk both 'presentOn' and 'presentAt' are built on: read @start@'s
--   own tick, and if it's a presence tick naming @character@ on @file@,
--   that's the answer (Enter -> present, Leave -> not) -- stop right there,
--   nothing earlier can still be relevant since a later word always wins.
--   Otherwise follow 'tickParent' to the previous tick and repeat; running
--   out of chain (root, no parent) means not present -- a fresh file
--   starts with nobody in it (see 'Storyteller.Writer.Types.Presence'),
--   so there is no earlier state to fall back to, only empty. Each step is
--   exactly one 'Tick.readTypesTick' -- no upfront pass over the rest of
--   the chain, so a character whose last word was one tick back costs one
--   read, not a read of everything that ever happened on @file@.
presentAsOf :: Core.StoreM m => Core.ObjectHash -> FilePath -> Character -> Core.StoreT m Bool
presentAsOf start file character = go start
  where
    go h = do
      t <- Tick.readTypesTick h
      case fromTick @Presence t of
        Just p | presenceFile p == file, presenceCharacter p == character ->
          pure (presenceEvent p == Enter)
        _ -> case tickParent t of
          Nothing          -> pure False
          Just (TickId ph) -> go (Core.ObjectHash ph)

-- | The most recent presence tick for @character@ on this file, if nothing
--   since it has actually changed the file's content — i.e. it's still the
--   "trailing" event for this character since the last atom. Walks newest
--   to oldest: stops (no trailing tick) the moment an atom is hit, since
--   that means real content was written after whatever this character's
--   state last was; stops (found) the moment a presence tick for this
--   exact character is hit; skips anything else (other characters'
--   presence ticks, prompts, notes — none of them represent "an atom
--   happened" or say anything about this character).
trailingPresenceFor :: Character -> [FileTick] -> Maybe TickId
trailingPresenceFor (Character branch) ticks = go (reverse ticks)
  where
    go [] = Nothing
    go (ft : rest)
      | ftKind ft == "atom" = Nothing
      | ftKind ft == "presence"
      , lookup "character" (ftFields ft) == Just (unBranchName branch)
      = Just (TickId (ftTickId ft))
      | otherwise = go rest
