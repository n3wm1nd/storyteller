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
  , hiddenTagKey
  , setHidden
  , ftHidden
  ) where

import Data.Text (Text)

import Storage.Core (ObjectHash, StoreM, StoreT)
import qualified Storage.Ops as Ops
import Storage.Tick (FileTick(..))
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

-- ---------------------------------------------------------------------------
-- Hiding
-- ---------------------------------------------------------------------------

-- | The atom tag marking a tick the user has hidden.
--
--   Not a tick kind of its own but a flag on an existing one, and not a
--   storage concept either — "Storage.Core" never acts on this key the way
--   it acts on 'Storage.Core.removedTagKey'. It sits here for the same
--   reason 'Note' does: an app built on this storage model might plausibly
--   want it, none of them have to.
--
--   The tick stays in the chain and in the file; what hiding actually costs
--   it is inclusion in the conversation an agent is sent (see
--   'Storyteller.Writer.Conversation.turnsFromFileTicks' — the one place
--   the flag has real consequences, rather than merely affecting how the
--   UI draws the tick).
hiddenTagKey :: Text
hiddenTagKey = "hide"

-- | Hide or unhide one atom, by id.
setHidden :: StoreM m => ObjectHash -> Bool -> StoreT m ObjectHash
setHidden target hidden =
  Ops.setAtomTag target hiddenTagKey (if hidden then Just "true" else Nothing)

-- | Is this tick hidden? Absent on every kind but an atom, which reads as
--   not hidden — a note or a presence tick is never something the user
--   hid.
ftHidden :: FileTick -> Bool
ftHidden ft = lookup hiddenTagKey (ftFields ft) == Just "true"
