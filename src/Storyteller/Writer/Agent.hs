{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared vocabulary for agent pipelines.
--
-- Newtypes here are for values that cross agent boundaries — either passed
-- from the server dispatch layer into an agent, or forwarded from one agent
-- to another. Single-agent-local types stay in their own modules.
module Storyteller.Writer.Agent
  ( UserInput(..)
  , Instruction(..)
  , Prompt(..)
  , Prose(..)
  , CharContextBlock(..)
  , CharLabel(..)
  , CharSummary(..)
  , flattenCharSummary
  , ContextBlock(..)
  , ExistingContent(..)
  , WordCount(..)
  , renderEmbeddedFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Core.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

-- | Raw text from the user — no semantic assumption about intent.
--   The agent or dispatch layer decides what to do with it.
newtype UserInput = UserInput Text
  deriving (Show, Eq)

-- | A directive to an agent: tells it what to do.
--   Produced by the dispatch layer from a 'UserInput' (or hardcoded by a UI
--   action). Agents receive 'Instruction', never 'UserInput' directly.
newtype Instruction = Instruction Text
  deriving (Show, Eq)

-- | The full intent package from the user, as sent to the backend.
--   Captures what the user typed and the context they were working in.
--   Persisted as a tick before any agent runs.
--
--   Will grow to include active characters, selected ticks, goggles, etc.
data Prompt = Prompt FilePath Text
  deriving (Show, Eq)

instance TickType Prompt where
  tickTypeName = "prompt"

  toDraft (Prompt file text) = encodeDraft @Prompt [] [("file", T.pack file)] text

  fromTick t = do
    text <- decodePayload @Prompt t
    file <- lookup "file" (tickFields (tickData t))
    Just (Prompt (T.unpack file) text)

-- | A chunk of prose — story content to be appended to a file.
newtype Prose = Prose Text
  deriving (Show, Eq)

-- | A formatted block of character information, ready for inclusion in an
--   LLM prompt. Produced by 'charSummaryAgent'; passed to 'proseAgent' as a list.
newtype CharContextBlock = CharContextBlock Text
  deriving (Show, Eq)

-- | Display label for a character — used as a section header in LLM prompts.
--   Distinct from 'BranchName': a character branch may have a label that
--   differs from its branch name, and labels are presentation-only.
newtype CharLabel = CharLabel Text
  deriving (Show, Eq)

-- | A character's context, kept in the three shapes a chapter-aware
--   '[Message]' assembly needs to place independently rather than one flat
--   list a caller would otherwise have to re-derive positions from:
--
--     * 'csSheet' and 'csContext' are mostly-stable -- they may still be
--       edited, or grow as a new character enters the scene, but don't
--       change every call the way 'csJournal' does -- so they belong once,
--       near a chapter's own start, where an edit only costs reprocessing
--       from that point forward rather than the whole history.
--     * 'csJournal' is recomputed fresh every call (see
--       'Storage.Tick.recentAtomsOf') and belongs at a shallow depth near
--       the live edge of the conversation -- close enough to inform the
--       current turn, not so close it reads as the thing to continue
--       writing.
--
--   A caller that only wants a character reduced to its bare minimum can
--   take just 'csSheet' -- but see 'Storyteller.Writer.Agent.CharContext.
--   charSummaryWithJournal's own Haddock: if you didn't already need the
--   rest, call the cheaper read directly instead of computing all three and
--   throwing two away.
data CharSummary = CharSummary
  { csSheet   :: [CharContextBlock]
  , csContext :: [CharContextBlock]
  , csJournal :: [CharContextBlock]
  } deriving (Show, Eq)

-- | Collapse a 'CharSummary' back into the flat list shape a single-shot
--   prompt (everything in one message) still expects -- a transitional
--   adapter for callers not yet rebuilt around a real per-chapter
--   '[Message]' history. Order matches 'CharSummary's own field order:
--   sheet, then other context, then journal.
flattenCharSummary :: CharSummary -> [CharContextBlock]
flattenCharSummary (CharSummary sheet ctx journal) = sheet ++ ctx ++ journal

-- | A formatted block of branch context (see 'renderEmbeddedFile'), ready
--   for inclusion in an LLM prompt alongside character and file content.
newtype ContextBlock = ContextBlock Text
  deriving (Show, Eq)

-- | Render a branch file's raw content as an explicitly fenced block --
--   distinct from a live instruction even if a provider concatenates this
--   message with an adjacent one of the same role (some do, once several
--   'UserText' messages end up back to back -- see
--   'Storyteller.Writer.Agent.Write.buildChapterMessages'). The @path@
--   attribute is what tells the model *what* this is without needing prose
--   framing repeated around every single entry; the tags are what tell it
--   this is embedded reference data, not something to act on directly.
--   What 'Storyteller.Context.DSL.Render' reaches for whenever a
--   DSL-produced 'Storyteller.Context.DSL.Value.FileRead' message gets
--   flattened into a prompt -- the one place raw file content gets
--   embedded this way now.
renderEmbeddedFile :: FilePath -> Text -> Text
renderEmbeddedFile path content = T.concat
  [ "<context-file path=\"", T.pack path, "\">\n"
  , content
  , "\n</context-file>"
  ]

-- | The current content of the file being continued.
--   Empty when the file does not yet exist.
newtype ExistingContent = ExistingContent Text
  deriving (Show, Eq)

-- | Approximate desired output length in words. Passed as a sizing hint to
--   the continuation agent; it may be ignored if the model disregards it.
newtype WordCount = WordCount Int
  deriving (Show, Eq, Ord, Num)
