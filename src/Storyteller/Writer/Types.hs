{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tick kinds specific to the Writer app's story-writing vocabulary — as
-- opposed to 'Storyteller.Common.Types', which holds kinds any app on this
-- storage model might want. 'Presence' encodes the "character" concept
-- (the @character/{id}@ branch-naming convention documented in WRITER.md),
-- which only means something to this app.
module Storyteller.Writer.Types
  ( Character(..)
  , Presence(..)
  , PresenceEvent(..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)

import Storyteller.Core.Types
  ( BranchName(..), TickType(..), TickData(..), Tick(..)
  , encodeDraft, decodePayload
  )

-- | A character, identified by its @character/{id}@ branch (see WRITER.md).
--   Wraps 'BranchName' rather than reusing it bare: a bare 'BranchName'
--   parameter answers equally well to "the tracker branch", "the source
--   branch", or any other branch a caller might have in scope, so a
--   function that only makes sense for *this one specific kind* of branch
--   (e.g. "is this character present?" -- nonsense to ask of an arbitrary
--   branch) should say so in its type, not just its argument name.
newtype Character = Character { unCharacter :: BranchName }
  deriving (Show, Eq, Ord)

-- | Whether a character is entering or leaving the scene at this point in
--   the story.
data PresenceEvent = Enter | Leave
  deriving (Show, Eq)

-- | A tick recording that a character is present (or no longer present) in
--   the scene, as of this point in one file's own chain. Presence is scoped
--   per file, not to the whole branch: a scene is a file, and a fresh file
--   implicitly starts with nobody in it — writing a scratch scene in a
--   separate file should never inherit whoever happened to be present in
--   the last file worked on. 'presenceFile' plays the same role as
--   'Storyteller.Writer.Agent.Prompt's "file" field: a hint 'walkFileTicks'
--   (Storyteller.Core.Git) uses to fold this tick into that file's
--   projection, not a hard reference — expect its representation to change
--   if/when file association stops being a plain field.
--
--   The character itself is referenced by branch name, not by tick id —
--   unlike 'Storyteller.Common.Types.Note', this doesn't point at a tick
--   within the same chain, it points at another branch entirely, so there
--   is nothing for rebase fixups to keep in sync. "Who's currently active"
--   is derived by folding these ticks from root to whatever point in the
--   file's chain is in view — see WRITER.md.
data Presence = Presence
  { presenceFile      :: FilePath
  , presenceCharacter :: Character
  , presenceEvent     :: PresenceEvent
  } deriving (Show, Eq)

instance TickType Presence where
  tickTypeName = "presence"

  toDraft (Presence file (Character branch) event) =
    encodeDraft @Presence [] [("file", T.pack file), ("character", unBranchName branch), ("event", eventText event)] ""

  fromTick t = do
    _        <- decodePayload @Presence t
    let fields = tickFields (tickData t)
    file     <- lookup "file" fields
    charName <- lookup "character" fields
    evText   <- lookup "event" fields
    event    <- parseEvent evText
    Just Presence { presenceFile = T.unpack file, presenceCharacter = Character (BranchName charName), presenceEvent = event }

eventText :: PresenceEvent -> Text
eventText Enter = "enter"
eventText Leave = "leave"

parseEvent :: Text -> Maybe PresenceEvent
parseEvent "enter" = Just Enter
parseEvent "leave" = Just Leave
parseEvent _       = Nothing
