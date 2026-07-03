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
