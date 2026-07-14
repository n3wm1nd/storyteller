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

-- 'reinterpretProse'\/'reinterpretAgent's capability constraints on
-- @chosenModel@ are load-bearing even though 'convertMessage'\/'convertConfig'
-- (both 'unsafeCoerce') never use them in the function body -- they're the
-- compile-time check that a real model has at least what its role
-- declares. See this module's Haddock.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Fixed, named LLM roles, and the machinery that lets each be assigned an
--   independently-chosen concrete model at runtime (see
--   'Storyteller.Core.LLM.Registry') without that choice ever showing up as
--   a type variable in agent or handler code.
--
--   A role proxy's declared capabilities should reflect exactly what the
--   real model it proxies needs: 'ProseModel' only ever constructs and
--   consumes plain text (every prose agent -- 'Storyteller.Writer.Agent.
--   Continuation.proseAgent', 'Storyteller.Writer.Agent.Write.writeAgent',
--   'Storyteller.Writer.Agent.ChapterSummarizer' -- reads back only
--   'Runix.LLM.AssistantText' and never sends a 'Tools' config), so it
--   declares nothing beyond the config triad; 'AgentModel' declares
--   'HasTools'\/'HasJSON'\/'HasReasoning' since its tool-heavy workflows
--   actually use all three. 'reinterpretProse'\/'reinterpretAgent' require @chosenModel@
--   (the real, registry-resolved model) to have at least that same set --
--   checked wherever a concrete model gets plugged in (see
--   'Storyteller.Core.LLM.Registry.resolveRoleRunner') -- so a model
--   lacking a capability its assigned role needs is a compile error.
--
--   That constraint is necessarily a lower bound, not an exact match:
--   Haskell has no way to say "@chosenModel@ must NOT have 'HasReasoning'",
--   so nothing stops a model that happens to have /more/ than its role
--   requires from being assigned there, and its response could genuinely
--   contain a constructor the role's own type doesn't declare (a reasoning
--   model assigned to Prose spontaneously emitting 'AssistantReasoning',
--   say). No capability requirement on @chosenModel@ closes that gap --
--   it's inherent to only being able to require a lower bound.
--
--   The actual fix is a different, more basic fact: a role never needs
--   capability evidence for a constructor nothing downstream ever consumes.
--   'convertMessage'\/'convertConfig' are 'unsafeCoerce' here, but that's
--   an implementation shortcut, not the reason this is safe -- a
--   hand-written, capability-checked version that pattern-matched every
--   constructor and *dropped* (rather than errored on) the ones a narrow
--   role can't represent would have exactly the same property. 'unsafeCoerce'
--   is sound and equivalent to that "match and drop" version specifically
--   because every constructor in both GADTs is phantom in the model
--   parameter -- no constructor stores a value of that type, so there's no
--   data an explicit drop would discard that a relabel doesn't already
--   silently carry past whatever pattern-match downstream doesn't ask for
--   it (exactly what already happens for any constructor a caller's list
--   comprehension doesn't extract). Reconstructing via capability-checked
--   pattern match and *erroring* on the rest -- the original version of
--   this module -- was the actual mistake: it turned "a constructor nobody
--   consumes" into a crash instead of a no-op.
--
--   Each role still gets its own proxy /provider/ type ('ProseProxyProvider',
--   'AgentProxyProvider', paired with a role tag into an ullm @Model roleTag
--   provider@) so two roles can be simultaneously live in one Polysemy
--   effect row without ambiguity -- see e.g.
--   'Storyteller.Writer.Agent.FlowWrite', which needs a prose model and an
--   agent model side by side.
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

import Unsafe.Coerce (unsafeCoerce)

import Polysemy (Member, Members, Sem, interpret, send)

import Runix.LLM (LLM(..), Message, ModelConfig)
import UniversalLLM
  ( HasTools(..), HasJSON(..), HasReasoning(..), Model(..), ProviderOf
  , SupportsMaxTokens, SupportsSystemPrompt, SupportsTemperature )

-- ---------------------------------------------------------------------------
-- Prose role: narrow. 'Storyteller.Writer.Agent.Continuation.proseAgent'
-- and everything built on it only ever construct and consume plain text --
-- no reason to declare more.
-- ---------------------------------------------------------------------------

data ProseProxyProvider

instance SupportsSystemPrompt ProseProxyProvider
instance SupportsMaxTokens ProseProxyProvider
instance SupportsTemperature ProseProxyProvider

data ProseRole
type ProseModel = Model ProseRole ProseProxyProvider

-- ---------------------------------------------------------------------------
-- Agent role: for agents whose main activity is calling tools rather than
-- producing prose -- the fixer ('Storyteller.Writer.Agent.ReplaceTool', via
-- its @replace_atom@ tool), the chat agent ('Storyteller.Writer.Agent.Chat',
-- via @glob@\/@read_file@\/@sed_print@), and the outline splitter
-- ('Storyteller.Writer.Agent.Outline.splitOutlineAgent', via
-- @emit_beat_sheet@). Also declares 'HasJSON'\/'HasReasoning': a real
-- capability these tool-heavy agents may go on to use outbound, not just a
-- response-side allowance.
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

-- | Re-tag a 'Message' between any two model types -- see this module's
--   Haddock for why this is safe: every constructor is phantom in the model
--   parameter, so this is a relabel, not a reconstruction.
convertMessage :: Message a -> Message b
convertMessage = unsafeCoerce

-- | Re-tag a 'ModelConfig' the same way -- see 'convertMessage'.
convertConfig :: ModelConfig a -> ModelConfig b
convertConfig = unsafeCoerce

-- | Interpret 'ProseModel'\'s proxy 'LLM' effect by delegating to the real,
--   runtime-chosen model's own already-interpreted 'LLM' effect further
--   down the row -- built once at startup (see 'Storyteller.Core.LLM.Registry.
--   resolveRoleRunner') and reused across requests via
--   'Server.Writer.Env.ServerEnv'. The constraint list on @chosenModel@ is
--   exactly 'ProseModel'\'s own declared capability set -- see this
--   module's Haddock for why that match (and its limits) matters.
reinterpretProse
  :: forall chosenModel r a.
     ( SupportsSystemPrompt (ProviderOf chosenModel), SupportsMaxTokens (ProviderOf chosenModel), SupportsTemperature (ProviderOf chosenModel)
     , Member (LLM chosenModel) r )
  => Sem (LLM ProseModel : r) a -> Sem r a
reinterpretProse = interpret $ \case
  QueryLLM configs msgs -> do
    result <- send (QueryLLM @chosenModel (map convertConfig configs) (map convertMessage msgs))
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
    result <- send (QueryLLM @chosenModel (map convertConfig configs) (map convertMessage msgs))
    pure (fmap (map convertMessage) result)
