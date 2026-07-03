{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Types
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
  , Note(..)

    -- * Utilities
  , tickTypeOf
  ) where

import Data.Text (Text)
import qualified Data.Text as T

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
--   Message format: @"type:\<tickTypeName\>\n\<payload\>"@.
--   Use 'encodeDraft' and 'decodePayload' to handle this consistently.
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

-- | Build a 'TickData' with the type tag, refs, extra fields, and payload.
--   Use @encodeDraft \@MyType refs fields payload@ in 'toDraft' implementations.
encodeDraft :: forall a. TickType a => [TickId] -> [(Text, Text)] -> Text -> TickData
encodeDraft refs fields payload = TickData
  { tickRefs    = refs
  , tickFields  = fields
  , tickMessage = "type:" <> tickTypeName @a <> "\n" <> payload
  }

-- | Extract the payload from a tick whose tag matches @a@'s 'tickTypeName'.
--   Returns 'Nothing' if the tag does not match.
decodePayload :: forall a. TickType a => Tick -> Maybe Text
decodePayload t = case T.lines (tickMessage (tickData t)) of
  (tag : rest) | tag == "type:" <> tickTypeName @a
               -> Just (T.intercalate "\n" rest)
  _            -> Nothing

-- | Extract the type tag from any tick, without knowing the type.
--   Returns 'Nothing' for untagged ticks.
tickTypeOf :: Tick -> Maybe Text
tickTypeOf t = case T.lines (tickMessage (tickData t)) of
  (l : _) | "type:" `T.isPrefixOf` l -> Just (T.drop 5 l)
  _                                    -> Nothing

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
