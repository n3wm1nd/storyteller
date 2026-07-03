{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tick kinds specific to the Writer app's story-writing vocabulary — as
-- opposed to 'Storyteller.Common.Types', which holds kinds any app on this
-- storage model might want. 'Presence' encodes the "character" concept
-- (the @character/{id}@ branch-naming convention documented in WRITER.md),
-- which only means something to this app.
module Storyteller.Writer.Types
  ( Presence(..)
  , PresenceEvent(..)
  ) where

import Data.Text (Text)

import Storyteller.Core.Types
  ( BranchName(..), TickType(..), TickData(..), Tick(..)
  , encodeDraft, decodePayload
  )

-- | Whether a character is entering or leaving the scene at this point in
--   the story.
data PresenceEvent = Enter | Leave
  deriving (Show, Eq)

-- | A tick recording that a character is present (or no longer present) in
--   the scene, as of this point in a story branch's chain. The character is
--   referenced by branch name, not by tick id — unlike 'Storyteller.Common.Types.Note',
--   this doesn't point at a tick within the same chain, it points at another
--   branch entirely, so there is nothing for rebase fixups to keep in sync.
--   "Who's currently active" is derived by folding these ticks from root to
--   whatever point in the chain is in view — see WRITER.md.
data Presence = Presence
  { presenceCharacter :: BranchName
  , presenceEvent     :: PresenceEvent
  } deriving (Show, Eq)

instance TickType Presence where
  tickTypeName = "presence"

  toDraft (Presence branch event) =
    encodeDraft @Presence [] [("character", unBranchName branch), ("event", eventText event)] ""

  fromTick t = do
    _        <- decodePayload @Presence t
    let fields = tickFields (tickData t)
    charName <- lookup "character" fields
    evText   <- lookup "event" fields
    event    <- parseEvent evText
    Just Presence { presenceCharacter = BranchName charName, presenceEvent = event }

eventText :: PresenceEvent -> Text
eventText Enter = "enter"
eventText Leave = "leave"

parseEvent :: Text -> Maybe PresenceEvent
parseEvent "enter" = Just Enter
parseEvent "leave" = Just Leave
parseEvent _       = Nothing
