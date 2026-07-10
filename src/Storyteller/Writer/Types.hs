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
  , CharacterAnswer(..)
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

-- | A recorded "ask this character a question" exchange -- see
-- 'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent'. Stored on the
-- branch of whoever asked (the scene being written), *not* the character's
-- own branch: this is a record of something that happened during the
-- current writing (which character was consulted, and what they said),
-- not a memory the character themselves now has -- asking a question
-- doesn't add to what a character knows, it only reads what they already
-- do. 'caFile' plays the same role as 'Presence's own "file" field when
-- there is one -- a hint 'walkFileTicks' folds this into that file's
-- projection by -- but unlike 'Presence' it's optional: a @\/ask@ command
-- fired from a file connection always has one, but an agent asking mid
-- generation isn't necessarily bound to any single file (deferred for now
-- -- see WRITER.md), so the type has to allow for "no file" from the
-- start rather than assume one always exists. Note the asymmetry with
-- 'Presence': that lives on the scene but names a character elsewhere by
-- branch reference; this lives on the scene *and* is entirely about a
-- character elsewhere, but neither points into the character's own chain
-- -- there's nothing there for a rebase to keep in sync either way.
data CharacterAnswer = CharacterAnswer
  { caCharacter :: Character
  , caQuestion  :: Text
  , caAnswer    :: Text
  , caFile      :: Maybe FilePath
  } deriving (Show, Eq)

-- | 'caQuestion' is free-form, possibly multi-line text -- exactly what
--   'Storage.Tick.encodeTickData's own invariant forbids in a field (see
--   its Haddock), so it can't sit in 'tickFields' next to 'caCharacter'\/
--   'caFile' the way an earlier version of this instance had it. Both
--   'caQuestion' and 'caAnswer' go into the message instead, joined by a
--   single NUL character: not a delimiter either side could plausibly
--   produce itself (unlike a chosen text delimiter, which a question or
--   answer could in principle contain), and safe to split on across any
--   consumer of this wire format, Haskell or otherwise, since it's one
--   literal character rather than a count that depends on how "length"
--   is defined for the text's encoding.
questionAnswerSep :: Text
questionAnswerSep = "\NUL"

instance TickType CharacterAnswer where
  tickTypeName = "character-answer"

  toDraft (CharacterAnswer (Character branch) question answer mFile) =
    encodeDraft @CharacterAnswer
      []
      (("character", unBranchName branch) : maybe [] (\f -> [("file", T.pack f)]) mFile)
      (question <> questionAnswerSep <> answer)

  fromTick t = do
    _        <- decodePayload @CharacterAnswer t
    let fields = tickFields (tickData t)
    charName <- lookup "character" fields
    let mFile = T.unpack <$> lookup "file" fields
        (question, rest) = T.breakOn questionAnswerSep (tickMessage (tickData t))
    answer <- T.stripPrefix questionAnswerSep rest
    Just CharacterAnswer
      { caCharacter = Character (BranchName charName)
      , caQuestion  = question
      , caAnswer    = answer
      , caFile      = mFile
      }
