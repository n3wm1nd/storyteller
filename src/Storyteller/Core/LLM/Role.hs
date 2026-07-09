{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Fixed, named LLM roles, and the machinery that lets each be assigned an
--   independently-chosen concrete model at runtime (see
--   'Storyteller.Core.LLM.Registry') without that choice ever showing up as
--   a type variable in agent or handler code.
--
--   Every agent module (`Write`, `Fix`, `ReplaceTool`, `Chat`, `Outline`,
--   `FlowWrite`, `Continuation`) only ever requires 'HasTools' and
--   'SupportsSystemPrompt' on its model type variable; the shared sampling
--   defaults ('Storyteller.Core.CLI.Env.modelConfigs') additionally use
--   'SupportsMaxTokens'\/'SupportsTemperature'. That's the complete
--   capability set a role needs, and the bar a registry entry
--   ('Storyteller.Core.LLM.Registry.KnownModel') must clear to be assignable
--   to any role.
--
--   Each role gets its own phantom "tag" type (`ProseRole`, `AgentRole`, ...)
--   paired with 'ProxyProvider' into an ullm @Model roleTag ProxyProvider@ --
--   structurally identical to a real model (e.g. today's
--   @Storyteller.Core.Runtime.StoryModel@), just never actually serialized:
--   'reinterpretRole' intercepts every 'Runix.LLM.LLM' call made against a
--   role's proxy type and re-tags it as the real, runtime-chosen model
--   before delegating. The *only* reason two roles need distinct types at
--   all (rather than everyone sharing one proxy) is so two roles can be
--   simultaneously live in one Polysemy effect row without ambiguity --
--   see e.g. 'Storyteller.Writer.Agent.FlowWrite', which already needs a
--   prose model and an agent model side by side. Adding a new role is one
--   @data XRole@ + one @type XModel@ line; the instances below are already
--   generic over the tag.
module Storyteller.Core.LLM.Role
  ( -- * Proxy plumbing
    ProxyProvider
  , reinterpretRole

    -- * Roles
  , ProseRole
  , ProseModel
  , AgentRole
  , AgentModel
  , LLMs
  ) where

import Polysemy (Member, Members, Sem, interpret, send)

import Runix.LLM (LLM(..), Message(..), ModelConfig(..))
import UniversalLLM
  ( HasTools(..), Model(..), ProviderOf
  , SupportsMaxTokens, SupportsSystemPrompt, SupportsTemperature )

-- ---------------------------------------------------------------------------
-- Proxy provider: never actually dispatched over the wire. Its only job is
-- to give role tag types somewhere to hang the capability instances agent
-- code needs to typecheck against; 'reinterpretRole' converts every message
-- and config away from it before a real request is ever built.
-- ---------------------------------------------------------------------------

data ProxyProvider

instance SupportsSystemPrompt ProxyProvider
instance SupportsMaxTokens ProxyProvider
instance SupportsTemperature ProxyProvider

-- | One instance, generic over any role tag -- adding a role never means
--   adding another copy of this.
instance HasTools (Model roleTag ProxyProvider) where
  type ToolState (Model roleTag ProxyProvider) = ()
  withTools = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

-- ---------------------------------------------------------------------------
-- Roles
-- ---------------------------------------------------------------------------

-- | For agents whose main output is prose text -- no tool calls, judged on
--   how good the writing is. 'Storyteller.Writer.Agent.Continuation.proseAgent'
--   and everything built on it ('Write', 'Outline'\'s prose drivers).
data ProseRole
type ProseModel = Model ProseRole ProxyProvider

-- | For agents whose main activity is calling tools rather than producing
--   prose -- the fixer ('Storyteller.Writer.Agent.ReplaceTool', via its
--   @replace_atom@ tool), the chat agent ('Storyteller.Writer.Agent.Chat',
--   via @glob@\/@read_file@\/@sed_print@), and the outline splitter
--   ('Storyteller.Writer.Agent.Outline.splitOutlineAgent', via
--   @emit_beat_sheet@): none of these are judged on prose quality the way
--   'ProseModel' is, and a model tuned for reliable tool use over one tuned
--   for prose is a genuinely different selection criterion, which is what
--   this role exists to let an operator route independently. Split off from
--   'ProseModel' also proves a second role is cheap to add, and
--   'Storyteller.Writer.Agent.FlowWrite' already needs a prose model and an
--   agent model simultaneously live -- see this module's Haddock.
data AgentRole
type AgentModel = Model AgentRole ProxyProvider

-- | Every role's 'LLM' effect, bundled. Agents require this in full rather
--   than picking out just the one role they use (see
--   'Storyteller.Writer.Agent.Write.writeAgent', which only ever queries
--   'ProseModel') -- deliberately: once an agent calls an LLM at all it's
--   already Storyteller-specific (its role's proxy types aren't reusable
--   elsewhere), so precisely tracking "this agent needs Prose but not
--   Agent" buys nothing except one more type variable per agent to plumb.
--   Extra unused members in a 'Members' constraint cost nothing at
--   runtime -- this is exactly the "not from the ullm library, just
--   routing to one of them" set described in this module's Haddock.
type LLMs r = Members '[LLM ProseModel, LLM AgentModel] r

-- ---------------------------------------------------------------------------
-- Reinterpretation
-- ---------------------------------------------------------------------------

-- | Re-tag a 'Message' from one model to another. Purely a change of phantom
--   type -- no constructor here carries model-specific data -- except that
--   each capability-gated constructor needs the target to actually have that
--   capability. The constructors gated on capabilities no role proxy
--   declares ('UserImage', 'UserRequestJSON', 'AssistantJSON',
--   'AssistantReasoning') are unreachable: a role proxy's own instance set
--   (just 'HasTools'\/'SupportsSystemPrompt') means no @Message
--   (Model roleTag ProxyProvider)@ value can ever actually be one of them.
convertMessage :: forall b a. HasTools b => Message a -> Message b
convertMessage = \case
  UserText t         -> UserText t
  AssistantText t    -> AssistantText t
  SystemText t        -> SystemText t
  AssistantTool tc    -> AssistantTool tc
  ToolResultMsg tr    -> ToolResultMsg tr
  UserImage _ _       -> unreachable
  UserRequestJSON _ _ -> unreachable
  AssistantJSON _     -> unreachable
  AssistantReasoning _ -> unreachable
  where
    unreachable = error "Storyteller.Core.LLM.Role: role proxy models never construct this capability"

-- | Re-tag a 'ModelConfig' the same way -- see 'convertMessage'.
convertConfig :: forall b a. (HasTools b, SupportsSystemPrompt (ProviderOf b), SupportsMaxTokens (ProviderOf b), SupportsTemperature (ProviderOf b)) => ModelConfig a -> ModelConfig b
convertConfig = \case
  Temperature t   -> Temperature t
  MaxTokens n     -> MaxTokens n
  SystemPrompt t  -> SystemPrompt t
  Tools ts        -> Tools ts
  Seed _          -> unreachable
  Stop _          -> unreachable
  Reasoning _     -> unreachable
  ReasoningEffort _ -> unreachable
  where
    unreachable = error "Storyteller.Core.LLM.Role: role proxy models never construct this capability"

-- | Interpret a role's proxy 'LLM' effect by delegating to the real,
--   runtime-chosen model's own already-interpreted 'LLM' effect further down
--   the row -- built once at startup (see 'Storyteller.Core.LLM.Registry.
--   withKnownModel') and reused across requests via
--   'Server.Writer.Env.ServerEnv'.
reinterpretRole
  :: forall roleTag chosenModel r a.
     ( HasTools chosenModel, SupportsSystemPrompt (ProviderOf chosenModel)
     , SupportsMaxTokens (ProviderOf chosenModel), SupportsTemperature (ProviderOf chosenModel)
     , Member (LLM chosenModel) r )
  => Sem (LLM (Model roleTag ProxyProvider) : r) a -> Sem r a
reinterpretRole = interpret $ \case
  QueryLLM configs msgs -> do
    result <- send (QueryLLM @chosenModel (map (convertConfig @chosenModel) configs) (map (convertMessage @chosenModel) msgs))
    pure (fmap (map (convertMessage @(Model roleTag ProxyProvider))) result)
