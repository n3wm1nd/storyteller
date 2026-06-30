{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Storyteller.Types
  ( TickId(..)
  , BranchName(..)
  , Branch(..)
  , Tick(..)
  , TickKind(..)
  , tickKind
  , noteText
  , TickDraft(..)
  , draft
  , noteDraft
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

-- | What kind of tick this is, derived from the commit message prefix.
data TickKind
  = AtomTick  -- ^ Prose/content atom: no special prefix
  | NoteTick  -- ^ Annotation: message starts with "note: "; refs holds the annotated tick
  deriving (Show, Eq)

-- | Derive the kind of a tick from its message.
tickKind :: Text -> TickKind
tickKind msg
  | "note: " `T.isPrefixOf` msg = NoteTick
  | otherwise                    = AtomTick

-- | Extract the annotation text from a note tick's message (strips the prefix).
noteText :: Text -> Text
noteText = T.drop (T.length "note: ")

-- | A tick: the smallest unit of chain advancement the storage layer knows about.
--   Higher layers interpret the message to determine what kind of tick this is
--   (prose atom, annotation, etc.).
data Tick = Tick
  { tickId      :: TickId
  , tickParent  :: Maybe TickId  -- ^ Nothing only for the initial tick
  , tickRefs    :: [TickId]      -- ^ Cross-branch references (e.g. story→entity copies, note→atom)
  , tickMessage :: Text
  } deriving (Show, Eq)

-- | The caller-supplied part of a tick — everything the author or agent decides.
--   The storage layer fills in 'tickId' and 'tickParent' when the draft is committed.
--
--   'draftRefs' must list every TickId referenced. Embedding tick IDs in
--   'draftMessage' or anywhere else violates the invariant that all references
--   be declared (needed for rebase fixups to be complete).
data TickDraft = TickDraft
  { draftRefs    :: [TickId]  -- ^ References declared by the author/agent
  , draftMessage :: Text
  } deriving (Show, Eq)

-- | Convenience: a plain message draft with no references.
draft :: Text -> TickDraft
draft msg = TickDraft { draftRefs = [], draftMessage = msg }

-- | Convenience: a note draft annotating a specific tick.
--   The annotated tick id goes in refs; the message carries the human text.
noteDraft :: TickId -> Text -> TickDraft
noteDraft ref text = TickDraft { draftRefs = [ref], draftMessage = "note: " <> text }
