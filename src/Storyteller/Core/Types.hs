{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Core.Types
  ( -- * Identity
    TickId(..)
  , BranchName(..)
  , Branch(..)

    -- * Storage-layer tick
  , TickPos(..)
  , TickData(..)
  , Tick(..)
  , tickId
  , tickParent
  , draft

    -- * Semantic tick typeclass
  , TickType(..)
  , encodeDraft
  , decodePayload

    -- * Built-in tick kinds
  , Root(..)

    -- * Utilities
  , tickTypeOf
  ) where

import Data.Text (Text)

-- | Opaque identity of a tick — valid within the scope of an operation;
--   rebases produce new ids.
newtype TickId = TickId { unTickId :: Text }
  deriving (Show, Eq, Ord)

-- | A named chain of ticks.
newtype BranchName = BranchName { unBranchName :: Text }
  deriving (Show, Eq, Ord)

-- | A named branch: a pointer to the current head tick.
data Branch = Branch
  { branchName :: BranchName
  , branchHead :: TickId
  } deriving (Show, Eq)

-- | Position of a tick in the chain — assigned by the storage layer.
--   Not valid across rebases; treat as ephemeral beyond the current operation.
data TickPos = TickPos
  { posId     :: TickId
  , posParent :: Maybe TickId
  , posRefs   :: [TickId]
  } deriving (Show, Eq)

-- | The data of a tick — everything the author or agent supplies.
--   The storage layer fills in 'TickPos' when committed.
--
--   'tickRefs' must list every TickId referenced. Embedding tick IDs in
--   'tickMessage' or 'tickFields' violates the invariant that all references
--   be declared (needed for rebase fixups to be complete).
data TickData = TickData
  { tickRefs    :: [TickId]
  , tickFields  :: [(Text, Text)]  -- ^ structured key/value metadata (e.g. tree ref, file hints)
  , tickMessage :: Text
  } deriving (Show, Eq)

-- | A stored tick: a 'TickData' paired with its chain position.
data Tick = Tick
  { tickPos  :: TickPos
  , tickData :: TickData
  } deriving (Show, Eq)

-- | Convenience accessors for the most commonly needed tick fields.
tickId :: Tick -> TickId
tickId = posId . tickPos

tickParent :: Tick -> Maybe TickId
tickParent = posParent . tickPos

-- | Convenience: a plain message with no refs or fields.
draft :: Text -> TickData
draft msg = TickData { tickRefs = [], tickFields = [], tickMessage = msg }

-- ---------------------------------------------------------------------------
-- Semantic tick typeclass
-- ---------------------------------------------------------------------------

-- | Types that can be encoded into and decoded from a 'Tick'.
--   Implement this to define a new tick kind.
--
--   The type tag lives as an ordinary @"type"@ entry in 'tickFields'
--   (always first, prepended by 'encodeDraft') — never embedded in
--   'tickMessage' itself. 'tickMessage' is the payload, verbatim, with
--   nothing to strip back off it: no fixed-offset assumption about where
--   a tag "line" ends and payload begins can survive a payload that
--   contains its own blank lines or colons, which is why the tag isn't
--   folded into the same text as the payload the way an early version of
--   this module did.
--
--   Law: fromTick t == Just a  whenever t was produced by storing (toDraft a)
class TickType a where
  -- | Stable string identifier for this tick kind.
  tickTypeName :: Text

  -- | Encode to a 'TickData'.
  toDraft :: a -> TickData

  -- | Decode from a full 'Tick'. Returns 'Nothing' if the type tag does not
  --   match, or if the payload/refs are malformed.
  --   Receives the full 'Tick' so types backed by a tree ref can use 'tickPos'.
  fromTick :: Tick -> Maybe a

-- | Build a 'TickData' with the type tag (folded into 'tickFields' as an
--   ordinary @"type"@ entry, always first), refs, extra fields, and the
--   payload untouched. Use @encodeDraft \@MyType refs fields payload@ in
--   'toDraft' implementations.
encodeDraft :: forall a. TickType a => [TickId] -> [(Text, Text)] -> Text -> TickData
encodeDraft refs fields payload = TickData
  { tickRefs    = refs
  , tickFields  = ("type", tickTypeName @a) : fields
  , tickMessage = payload
  }

-- | The payload of a tick whose @"type"@ field matches @a@'s
--   'tickTypeName' — just 'tickMessage', unmodified; there's nothing left
--   to strip once the tag lives in 'tickFields' rather than in the same
--   text as the payload. 'Nothing' if the tag doesn't match.
decodePayload :: forall a. TickType a => Tick -> Maybe Text
decodePayload t
  | tickTypeOf t == Just (tickTypeName @a) = Just (tickMessage (tickData t))
  | otherwise                              = Nothing

-- | The @"type"@ field of any tick, without knowing the type. 'Nothing'
--   for an untagged tick.
tickTypeOf :: Tick -> Maybe Text
tickTypeOf t = lookup "type" (tickFields (tickData t))

-- ---------------------------------------------------------------------------
-- Built-in tick kinds
-- ---------------------------------------------------------------------------

-- | The root tick: the first commit on a branch, carrying no content.
--   Created by 'createBranch'; never visible in normal chain walks.
data Root = Root
  { rootBranch :: BranchName
  } deriving (Show, Eq)

instance TickType Root where
  tickTypeName = "root"
  toDraft (Root name) = encodeDraft @Root [] [] (unBranchName name)
  fromTick t = Root . BranchName <$> decodePayload @Root t

-- Other built-in tick kinds ('Note', 'Fixup') live in 'Storyteller.Common.Types'
-- — not foundational the way 'Root' is (every branch needs a root tick
-- regardless of app), but not specific to any one app either.
