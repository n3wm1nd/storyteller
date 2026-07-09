{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tick kinds that aren't foundational (unlike 'Storyteller.Core.Types.Root'
-- — every branch needs one regardless of app) but also aren't specific to
-- any one application: any agent, in any app built on this storage model,
-- might plausibly want to attach a user comment or record its own reasoning
-- for a change.
module Storyteller.Common.Types
  ( Note(..)
  , Fixup(..)
  , Swipe(..)
  ) where

import Data.Text (Text)

import Storyteller.Core.Types (TickId, TickType(..), Tick(..), TickData(..), encodeDraft, decodePayload)

-- | An annotation attached to zero or more existing ticks — a comment on a
--   specific set of atoms, or (with no refs) a free-floating remark on the
--   file/story so far.
data Note = Note
  { noteRefs :: [TickId]
  , noteBody :: Text
  } deriving (Show, Eq)

instance TickType Note where
  tickTypeName = "note"

  toDraft (Note refs body) = encodeDraft @Note refs [] body

  fromTick t = do
    body <- decodePayload @Note t
    Just Note { noteRefs = tickRefs (tickData t), noteBody = body }

-- | An agent's own record of why it changed something — distinct from
--   'Note' (user-authored commentary): a 'Fixup' is agent-authored, tied to
--   the specific atom(s) it just replaced, kept so the reasoning behind a
--   change can be traced back later.
data Fixup = Fixup
  { fixupRefs   :: [TickId]
  , fixupReason :: Text
  } deriving (Show, Eq)

instance TickType Fixup where
  tickTypeName = "fixup"

  toDraft (Fixup refs reason) = encodeDraft @Fixup refs [] reason

  fromTick t = do
    reason <- decodePayload @Fixup t
    Just Fixup { fixupRefs = tickRefs (tickData t), fixupReason = reason }

-- | An alternate generation for a single atom — the atom it's an alternate
--   for is 'swipeOf', declared via 'tickRefs' like any other reference (so
--   a rebase of that atom correctly cascades into this tick too). Not a
--   chain-editing primitive on its own — see
--   'Storyteller.Common.Swipe.pushSwipe'/'cycleSwipe' for how a swipe
--   actually gets swapped into and out of its atom's own content.
data Swipe = Swipe
  { swipeOf      :: TickId
  , swipeContent :: Text
  } deriving (Show, Eq)

instance TickType Swipe where
  tickTypeName = "swipe"

  toDraft (Swipe of_ content) = encodeDraft @Swipe [of_] [] content

  fromTick t = do
    content <- decodePayload @Swipe t
    case tickRefs (tickData t) of
      [of_] -> Just Swipe { swipeOf = of_, swipeContent = content }
      _     -> Nothing
