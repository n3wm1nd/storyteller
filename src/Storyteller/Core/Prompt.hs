{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Prompt storage: lets agent-facing text (system prompts, templates) be
-- overridden by the user without a rebuild, while every call site still
-- carries a working default (see the @default...@ bindings next to each
-- 'Storyteller.Writer.Agent.Prompt.getPrompt' call in
-- @Storyteller.Writer.Agent.Continuation@/@Storyteller.Writer.Agent.ReplaceTool@)
-- so the system behaves identically until someone actually commits an
-- override.
--
-- Storage is a single, dedicated 'Prompts' branch — project-scoped, not tied
-- to any content or character branch (a "text.summarization" prompt might be
-- read by several unrelated agents, so it can't live under any one of them).
-- A key like @"agent.writer.system"@ doubles as a file path
-- (@/agent/writer/system.md@) in that branch, so overriding a prompt is just
-- committing a markdown file there — the same versioned-git-data model as
-- everything else in this project.
--
-- Template substitution ('applyTemplate') is deliberately kept out of the
-- effect: there is no global namespace of slots for it to resolve against,
-- so it is a plain pure function over whatever slot values the caller
-- already has in hand.
module Storyteller.Core.Prompt
  ( PromptKey(..)
  , Prompt(..)
  , PromptStorage(..)
  , getPrompt
  , applyTemplate
  , interpretPromptStorageFS
  , interpretPromptStorageMap
  ) where

import Control.Monad (void)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (fileExists, readFile)
import Runix.Git (Git)

import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Runtime (Prompts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))

import Prelude hiding (readFile)

-- | A dotted lookup key, e.g. @"agent.writer.system"@. Doubles as a file
--   path in the 'Prompts' branch: dots become path separators. Detached from
--   any one agent's name on purpose — a shared key like
--   @"text.summarization"@ is just as valid as an agent-specific one.
newtype PromptKey = PromptKey Text
  deriving (Eq, Ord, IsString)

-- | Text sourced from (or destined for) prompt storage. Kept distinct from
--   the many other 'Text'-shaped values passed around here ('Instruction',
--   'ExistingContent', 'Prose', ...) so it can't be mixed up with them.
newtype Prompt = Prompt Text
  deriving (Eq, Show, IsString, Semigroup, Monoid)

data PromptStorage (m :: Type -> Type) a where
  GetPrompt :: PromptKey -> Prompt -> PromptStorage m Prompt

makeSem ''PromptStorage

-- | Fill @"{{slot}}"@ placeholders in a template with the given values.
--   Plain text substitution — no lookup or namespace involved, the caller
--   supplies every slot it wants filled.
applyTemplate :: Prompt -> [(Text, Prompt)] -> Prompt
applyTemplate (Prompt template) slots =
  Prompt $ foldl (\acc (name, Prompt v) -> T.replace ("{{" <> name <> "}}") v acc) template slots

promptsBranchName :: BranchName
promptsBranchName = BranchName "prompts"

-- | Real interpreter: reads overrides from the dedicated 'Prompts' branch,
--   creating it on first use. A key resolves to @/\<dots-as-slashes\>.md@;
--   a missing file falls back to the caller's default, so the system works
--   with no branch content at all until someone commits an override.
interpretPromptStorageFS
  :: Members '[Git, StoryStorage, Fail] r
  => Sem (PromptStorage ': r) a
  -> Sem r a
interpretPromptStorageFS action = do
  getBranch promptsBranchName >>= \case
    Just _  -> return ()
    Nothing -> void (createBranch promptsBranchName)
  interpret (\case
    GetPrompt (PromptKey key) def -> runBranchAndFS @Prompts promptsBranchName $ do
      let path = "/" <> T.unpack (T.replace "." "/" key) <> ".md"
      fileExists @(BranchTag Prompts) path >>= \case
        True  -> Prompt . TE.decodeUtf8 <$> readFile @(BranchTag Prompts) path
        False -> return def
    ) action

-- | Test/pure interpreter: resolves from a fixed map, falling back to the
--   caller's default on miss. No filesystem or branch involved.
interpretPromptStorageMap
  :: Map PromptKey Prompt
  -> Sem (PromptStorage ': r) a
  -> Sem r a
interpretPromptStorageMap overrides = interpret $ \case
  GetPrompt key def -> return (Map.findWithDefault def key overrides)
