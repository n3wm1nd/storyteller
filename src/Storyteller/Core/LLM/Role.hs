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
--   A role proxy's declared capabilities are only meaningful if they're
--   backed by a matching, compiler-checked fact about whatever real model
--   ends up assigned to that role -- an instance like @HasReasoning
--   ProseModel@ is worthless on its own, since it's just an assertion we
--   wrote, disconnected from what the runtime-chosen backing model actually
--   supports. 'reinterpretProse'\/'reinterpretAgent' close that gap: their
--   own type signature requires @chosenModel@ (the real, registry-resolved
--   model) to have every capability its role proxy claims. That constraint
--   is checked wherever a concrete model gets plugged in (see
--   'Storyteller.Core.LLM.Registry.resolveRoleRunner'), so a real model
--   lacking a capability its assigned role promises is a compile error,
--   not a runtime surprise -- restoring the actual point of a typesafe LLM
--   library. Since 'Storyteller.Core.LLM.Registry.KnownModel' already
--   requires every registrable entry to have 'HasTools'\/'HasJSON'\/
--   'HasReasoning', both roles below declare that same set: there is
--   currently no narrower, still-honest set to give either role while the
--   registry itself doesn't distinguish "text-only" from "reasoning-
--   capable" entries. A future registry split (some entries opt out of
--   HasReasoning) is what would let 'ProseModel' legitimately narrow again.
--
--   Each role still gets its own proxy /provider/ type ('ProseProxyProvider',
--   'AgentProxyProvider', paired with a role tag into an ullm @Model roleTag
--   provider@) so two roles can be simultaneously live in one Polysemy
--   effect row without ambiguity -- see e.g.
--   'Storyteller.Writer.Agent.FlowWrite', which needs a prose model and an
--   agent model side by side. That's a distinctness requirement, not a
--   capability one; nothing stops a future role from genuinely diverging in
--   capability once the registry can express it.
module Storyteller.Core.LLM.Role
  ( -- * Proxy plumbing
    ProseProxyProvider
  , AgentProxyProvider
  , reinterpretProse
  , reinterpretAgent

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
  ( HasTools(..), HasJSON(..), HasReasoning(..), Model(..), ProviderOf
  , SupportsMaxTokens, SupportsSystemPrompt, SupportsTemperature )

-- ---------------------------------------------------------------------------
-- Prose role: proseAgent's whole output shape is plain text and tool calls,
-- but the proxy still needs to declare HasJSON/HasReasoning (see this
-- module's Haddock) so 'reinterpretProse' can convert whatever a real
-- reasoning-capable model actually sends back.
-- ---------------------------------------------------------------------------

data ProseProxyProvider

instance SupportsSystemPrompt ProseProxyProvider
instance SupportsMaxTokens ProseProxyProvider
instance SupportsTemperature ProseProxyProvider

data ProseRole
type ProseModel = Model ProseRole ProseProxyProvider

instance HasTools ProseModel where
  type ToolState ProseModel = ()
  withTools = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

instance HasJSON ProseModel where
  type JSONState ProseModel = ()
  withJSON = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

instance HasReasoning ProseModel where
  type ReasoningState ProseModel = ()
  withReasoning = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

-- ---------------------------------------------------------------------------
-- Agent role: for agents whose main activity is calling tools rather than
-- producing prose -- the fixer ('Storyteller.Writer.Agent.ReplaceTool', via
-- its @replace_atom@ tool), the chat agent ('Storyteller.Writer.Agent.Chat',
-- via @glob@\/@read_file@\/@sed_print@), and the outline splitter
-- ('Storyteller.Writer.Agent.Outline.splitOutlineAgent', via
-- @emit_beat_sheet@).
-- ---------------------------------------------------------------------------

data AgentProxyProvider

instance SupportsSystemPrompt AgentProxyProvider
instance SupportsMaxTokens AgentProxyProvider
instance SupportsTemperature AgentProxyProvider

data AgentRole
type AgentModel = Model AgentRole AgentProxyProvider

instance HasTools AgentModel where
  type ToolState AgentModel = ()
  withTools = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

instance HasJSON AgentModel where
  type JSONState AgentModel = ()
  withJSON = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

instance HasReasoning AgentModel where
  type ReasoningState AgentModel = ()
  withReasoning = error "Storyteller.Core.LLM.Role: role proxy models are never interpreted directly"

-- | Every role's 'LLM' effect, bundled. Agents require this in full rather
--   than picking out just the one role they use (see
--   'Storyteller.Writer.Agent.Write.writeAgent', which only ever queries
--   'ProseModel') -- deliberately: once an agent calls an LLM at all it's
--   already Storyteller-specific (its role's proxy types aren't reusable
--   elsewhere), so precisely tracking "this agent needs Prose but not
--   Agent" buys nothing except one more type variable per agent to plumb.
--   Extra unused members in a 'Members' constraint cost nothing at
--   runtime.
type LLMs r = Members '[LLM ProseModel, LLM AgentModel] r

-- ---------------------------------------------------------------------------
-- Reinterpretation
-- ---------------------------------------------------------------------------

-- | Re-tag a 'Message' between any two types that both have the full role
--   capability set ('HasTools'\/'HasJSON'\/'HasReasoning') -- both role
--   proxies now declare exactly this set, and so does every
--   'Storyteller.Core.LLM.Registry.KnownModel' entry, so this one function
--   covers all four directions ('ProseModel'\/'AgentModel' <-> a chosen
--   model). Purely a change of phantom type -- no constructor here carries
--   model-specific data. Only 'UserImage' ('HasVision') stays unreachable,
--   since no role or registry entry currently requires it.
convertMessage :: forall b a. (HasTools b, HasJSON b, HasReasoning b) => Message a -> Message b
convertMessage = \case
  UserText t           -> UserText t
  AssistantText t      -> AssistantText t
  SystemText t         -> SystemText t
  AssistantTool tc     -> AssistantTool tc
  ToolResultMsg tr     -> ToolResultMsg tr
  AssistantReasoning t -> AssistantReasoning t
  AssistantJSON v      -> AssistantJSON v
  UserRequestJSON q s  -> UserRequestJSON q s
  UserImage _ _        -> unreachable
  where
    unreachable = error "Storyteller.Core.LLM.Role: no role or known model requires HasVision"

-- | Re-tag a 'ModelConfig' the same way -- see 'convertMessage'. 'Seed'\/
--   'Stop' stay unreachable: gated by 'SupportsSeed'\/'SupportsStop' on the
--   /provider/, which no role proxy provider declares, and (unlike
--   'Message') a 'ModelConfig' is never round-tripped back from a response,
--   so this can't be forced the way 'AssistantReasoning'\/'AssistantJSON'
--   were.
convertConfig
  :: forall b a
  .  (HasTools b, HasReasoning b, SupportsSystemPrompt (ProviderOf b), SupportsMaxTokens (ProviderOf b), SupportsTemperature (ProviderOf b))
  => ModelConfig a -> ModelConfig b
convertConfig = \case
  Temperature t     -> Temperature t
  MaxTokens n       -> MaxTokens n
  SystemPrompt t    -> SystemPrompt t
  Tools ts          -> Tools ts
  Reasoning b       -> Reasoning b
  ReasoningEffort t -> ReasoningEffort t
  Seed _            -> unreachable
  Stop _            -> unreachable
  where
    unreachable = error "Storyteller.Core.LLM.Role: no role proxy provider declares SupportsSeed/SupportsStop"

-- | Interpret 'ProseModel'\'s proxy 'LLM' effect by delegating to the real,
--   runtime-chosen model's own already-interpreted 'LLM' effect further
--   down the row -- built once at startup (see 'Storyteller.Core.LLM.Registry.
--   resolveRoleRunner') and reused across requests via
--   'Server.Writer.Env.ServerEnv'. The constraint list on @chosenModel@ is
--   exactly 'ProseModel'\'s own declared capability set -- see this
--   module's Haddock for why that match matters.
reinterpretProse
  :: forall chosenModel r a.
     ( HasTools chosenModel, HasJSON chosenModel, HasReasoning chosenModel
     , SupportsSystemPrompt (ProviderOf chosenModel), SupportsMaxTokens (ProviderOf chosenModel), SupportsTemperature (ProviderOf chosenModel)
     , Member (LLM chosenModel) r )
  => Sem (LLM ProseModel : r) a -> Sem r a
reinterpretProse = interpret $ \case
  QueryLLM configs msgs -> do
    result <- send (QueryLLM @chosenModel (map (convertConfig @chosenModel) configs) (map (convertMessage @chosenModel) msgs))
    pure (fmap (map convertMessage) result)

-- | Same as 'reinterpretProse', for 'AgentModel'.
reinterpretAgent
  :: forall chosenModel r a.
     ( HasTools chosenModel, HasJSON chosenModel, HasReasoning chosenModel
     , SupportsSystemPrompt (ProviderOf chosenModel), SupportsMaxTokens (ProviderOf chosenModel), SupportsTemperature (ProviderOf chosenModel)
     , Member (LLM chosenModel) r )
  => Sem (LLM AgentModel : r) a -> Sem r a
reinterpretAgent = interpret $ \case
  QueryLLM configs msgs -> do
    result <- send (QueryLLM @chosenModel (map (convertConfig @chosenModel) configs) (map (convertMessage @chosenModel) msgs))
    pure (fmap (map convertMessage) result)
