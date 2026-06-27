{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storyteller.Types
  ( TickId(..)
  , BranchName(..)
  , Branch(..)
  , Tick(..)
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

-- | A tick: the smallest unit of chain advancement the storage layer knows about.
--   Higher layers interpret the message to determine what kind of tick this is
--   (prose atom, summary, etc.).
data Tick = Tick
  { tickId      :: TickId
  , tickParent  :: Maybe TickId  -- ^ Nothing only for the initial tick
  , tickRefs    :: [TickId]      -- ^ Cross-branch references (e.g. story→entity copies)
  , tickMessage :: Text
  } deriving (Show, Eq)
