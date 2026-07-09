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
{-# LANGUAGE TypeFamilies #-}
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
-- A key like @"agent.writer"@ doubles as a file path (@/agent/writer.md@) in
-- that branch, so overriding a prompt is just committing a markdown file
-- there — the same versioned-git-data model as everything else in this
-- project.
--
-- A key's namespace root is implicitly /the/ system prompt and its sampling
-- config — there is no separate @.system@ leaf. So @"agent.writer"@ is the
-- writer's system prompt (@/agent/writer.md@) and config
-- (@/agent/writer.llmsettings.yaml@ — see 'getConfig'), while a secondary
-- prompt like standing instructions gets its own nested key,
-- @"agent.writer.instructions"@ (@/agent/writer/instructions.md@) — a file
-- and a same-named subdirectory coexisting under @/agent/@ is ordinary git,
-- not a conflict.
--
-- User-facing overrides are never slotted templates: an override is either
-- the whole system prompt, or one plain free-text piece an agent splices
-- into a message it otherwise builds itself (e.g.
-- 'Storyteller.Writer.Agent.Continuation.proseAgent's @agent.writer.
-- instructions@, or 'Storyteller.Writer.Agent.ReplaceTool.reworkAtom's
-- @agent.fixer.instructions@) — never a string with @{{slot}}@ placeholders
-- an editor would have to know the names of. The structure around that text
-- (where it sits relative to the file's content, an instruction, retrieved
-- context, ...) stays fixed Haskell code, typechecked against whatever
-- values the agent actually has in hand.
module Storyteller.Core.Prompt
  ( PromptKey(..)
  , Prompt(..)
  , PromptStorage(..)
  , getPrompt
  , getConfig
  , getConfigWithPrompt
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
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic, Rep)

import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)

import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.LLM.Settings (RoleSettings)
import Storyteller.Core.Runtime (Prompts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Types (BranchName(..))
import UniversalLLM (ModelConfig(..), ProviderOf, SupportsSystemPrompt)
import UniversalLLM.Settings (GApplySettings, toModelConfigs)

-- | A dotted lookup key, e.g. @"agent.writer"@ (a namespace root, implicitly
--   the system prompt/config) or @"agent.writer.instructions"@ (a secondary
--   prompt nested under it). Doubles as a file path in the 'Prompts' branch:
--   dots become path separators. Detached from
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
  -- | Same key convention as 'GetPrompt', resolved against
  --   @/\<dots-as-slashes\>.llmsettings.yaml@ instead of @.md@: a sparse
  --   'RoleSettings' record decoded from that file is turned into
  --   @['ModelConfig' model]@ and placed ahead of the caller's defaults, so
  --   an override wins (every provider's config fold is first-match-wins --
  --   see 'UniversalLLM.Providers.OpenAI.handleBase') without needing any
  --   merge logic here. @model@ (and therefore which settings fields are
  --   even expressible) is pinned by the type of @defaults@ at the call
  --   site, exactly like every agent already threads @['ModelConfig'
  --   ProseModel]@\/@['ModelConfig' AgentModel]@ through today.
  GetConfig
    :: ( Generic (RoleSettings model)
       , GApplySettings (Rep (RoleSettings model)) model
       , Yaml.FromJSON (RoleSettings model) )
    => PromptKey -> [ModelConfig model] -> PromptStorage m [ModelConfig model]

makeSem ''PromptStorage

-- | Fetch both the system prompt and the config overrides filed under the
--   same key, and fold the prompt in as the config list's leading
--   'SystemPrompt' -- the one config field 'getConfig' itself can't cover,
--   since it isn't 'RoleSettings'-overridable Text-under-a-YAML-key the way
--   sampling knobs are, but is still logically "this call's config." Trivial
--   composition of 'getPrompt' and 'getConfig', nothing else: every agent
--   that currently does @SystemPrompt sys : configs@ by hand after its own
--   'getPrompt' call is exactly this, written out.
getConfigWithPrompt
  :: ( Member PromptStorage r
     , SupportsSystemPrompt (ProviderOf model)
     , Generic (RoleSettings model)
     , GApplySettings (Rep (RoleSettings model)) model
     , Yaml.FromJSON (RoleSettings model) )
  => PromptKey -> Prompt -> [ModelConfig model] -> Sem r [ModelConfig model]
getConfigWithPrompt key defaultPrompt defaultConfigs = do
  Prompt sys <- getPrompt key defaultPrompt
  configs    <- getConfig key defaultConfigs
  pure (SystemPrompt sys : configs)

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
    GetPrompt (PromptKey key) def -> runBranchOpGit @Prompts promptsBranchName $ do
      let path = "/" <> T.unpack (T.replace "." "/" key) <> ".md"
      fst <$> runStorage @Prompts (do
        exists <- Ops.exists path
        if exists
          then Prompt . TE.decodeUtf8 <$> Core.readFile path
          else return def)
    GetConfig (PromptKey key) (defaults :: [ModelConfig model]) -> runBranchOpGit @Prompts promptsBranchName $ do
      let path = "/" <> T.unpack (T.replace "." "/" key) <> ".llmsettings.yaml"
      fst <$> runStorage @Prompts (do
        exists <- Ops.exists path
        if exists
          then do
            bytes <- Core.readFile path
            case Yaml.decodeEither' @(RoleSettings model) bytes of
              Left _          -> return defaults
              Right overrides -> return (toModelConfigs overrides ++ defaults)
          else return defaults)
    ) action

-- | Test/pure interpreter: resolves from a fixed map, falling back to the
--   caller's default on miss. No filesystem or branch involved.
interpretPromptStorageMap
  :: Map PromptKey Prompt
  -> Sem (PromptStorage ': r) a
  -> Sem r a
interpretPromptStorageMap overrides = interpret $ \case
  GetPrompt key def -> return (Map.findWithDefault def key overrides)
  GetConfig _key defaults -> return defaults
