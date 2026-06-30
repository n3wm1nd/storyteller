{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared vocabulary for agent pipelines.
--
-- Newtypes here are for values that cross agent boundaries — either passed
-- from the server dispatch layer into an agent, or forwarded from one agent
-- to another. Single-agent-local types stay in their own modules.
module Storyteller.Agent
  ( UserInput(..)
  , Instruction(..)
  , Prompt(..)
  , Prose(..)
  , CharContextBlock(..)
  , WordCount(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

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
--   LLM prompt. Produced by 'CharContext.loadCharContext'; passed to
--   'Continuation.continuationAgent' as a list.
newtype CharContextBlock = CharContextBlock Text
  deriving (Show, Eq)

-- | Approximate desired output length in words. Passed as a sizing hint to
--   the continuation agent; it may be ignored if the model disregards it.
newtype WordCount = WordCount Int
  deriving (Show, Eq, Ord, Num)
