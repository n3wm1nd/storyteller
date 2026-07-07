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
  ) where

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
        Nothing  -> pure (isActive character ticks)
        Just tid -> do
          _           <- runStorage @branch (Ops.deleteTick (toHash tid))
          (ticks', _) <- runStorage @branch (Tick.fileTicksOf file)
          pure (isActive character ticks')
      let wantsActive = event == Enter
      if wantsActive == priorActive
        then pure Nothing
        else Just . fst <$> runStorage @branch (fmap toTickId (Tick.storeAs (Presence file character event)))
  where
    toHash (TickId t) = Core.ObjectHash t
    toTickId (Core.ObjectHash t) = TickId t

-- | Whether @character@ is active as of the end of @ticks@, folding
--   presence events oldest-first (as returned by 'fileTicks').
isActive :: BranchName -> [FileTick] -> Bool
isActive character = go False
  where
    go acc [] = acc
    go acc (ft : rest)
      | ftKind ft /= "presence"                                        = go acc rest
      | lookup "character" (ftFields ft) /= Just (unBranchName character) = go acc rest
      | otherwise = go (lookup "event" (ftFields ft) == Just "enter") rest

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
