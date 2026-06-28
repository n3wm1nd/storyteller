{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storyteller.Types
  ( TickId(..)
  , BranchName(..)
  , Branch(..)
  , Tick(..)
  , TickDraft(..)
  , draft
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

-- | The caller-supplied part of a tick — everything the author or agent decides.
--   The storage layer fills in 'tickId' and 'tickParent' when the draft is committed.
--
--   'draftRefs' must list every cross-branch TickId referenced. Embedding tick IDs
--   in 'draftMessage' or anywhere else violates the invariant that all references
--   be declared (needed for rebase fixups to be complete).
data TickDraft = TickDraft
  { draftRefs    :: [TickId]  -- ^ Cross-branch references declared by the author/agent
  , draftMessage :: Text
  } deriving (Show, Eq)

-- | Convenience: a plain message draft with no cross-branch references.
draft :: Text -> TickDraft
draft msg = TickDraft { draftRefs = [], draftMessage = msg }
