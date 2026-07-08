{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Registry of the ullm models Storyteller knows how to wire up, and the
--   env-var-driven, existential-CPS machinery to resolve one by name at
--   process startup without its concrete Haskell type ever escaping.
--
--   This is the shared home for what
--   @test/agent-integration/Agent/Integration/Harness.hs@ originally grew
--   for its own two-model (story\/judge) test suite -- moved here so
--   production code (currently 'Server.Writer.Env.loadServerEnv', resolving
--   one entry per named role -- see 'Storyteller.Core.LLM.Role') and the
--   test harness share one list and one resolution path instead of keeping
--   two copies in sync by hand. 'Agent.Integration.Harness' now imports the
--   'ModelID'\/'KnownModel'\/'resolveKnownModel'\/'LLMRunner'\/
--   'modelInterpreter'\/'withKnownModel' machinery from here and keeps only
--   its own env-var names and its own 'knownModels' entries (a
--   suite-specific concern). Its scenarios never surface streaming preview
--   chunks, so it uses the plain (non-streaming) builders; the server does,
--   so it uses 'resolveRoleRunner'\/'SomeLLMRunner' instead.
module Storyteller.Core.LLM.Registry
  ( ModelID(..)
  , KnownModel(..)
  , knownModels
  , resolveKnownModel
  , LLMRunner(..)
  , modelInterpreter
  , withKnownModel
  , SomeLLMRunner(..)
  , resolveRoleRunner
  ) where

import Data.Default (Default)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.Fail (Fail)
import System.Environment (lookupEnv)

import Runix.Config (Config)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLM, interpretLLMWith, LlamaCppAuth(..), OpenRouterAuth(..))
import Runix.LLM.Streaming (llmStreamingRestAPI, StreamEvent, StreamingEnabled)
import Runix.RestAPI (RestEndpoint, RestAPI, restapiHTTP, llmRetry)
import Runix.StreamChunk (StreamChunk)
import Runix.Time (Time, Sleep)
import UniversalLLM
  ( EnableStreaming, HasJSON, HasReasoning, HasTools, Model(..), ModelConfig(..)
  , ModelName, Provider, ProviderOf, Routing, RoutingState
  , SupportsMaxTokens, SupportsSystemPrompt, SupportsTemperature, route )
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_40B(..))
import UniversalLLM.Models.DeepSeek.DeepSeek (DeepSeekV4Flash(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))

-- | Which auth backend a model needs -- the one place that knows which env
--   vars a given backend reads.
data ModelID = ViaLlamaCpp | ViaOpenRouter

-- | One model Storyteller knows how to wire up: its capability instances
--   (everything 'modelInterpreter'\/'resolveRoleRunner' and any agent it
--   could be assigned to collectively need), which auth backend it uses,
--   the model value itself, and its default configs. Existential because
--   each entry is a genuinely different Haskell type -- this is what lets
--   one list mix them.
data KnownModel where
  KnownModel :: ( ModelName m, Provider m, Routing m, Default (RoutingState m)
                , HasTools m, HasJSON m, HasReasoning m, SupportsSystemPrompt (ProviderOf m)
                , SupportsMaxTokens (ProviderOf m), SupportsTemperature (ProviderOf m)
                , EnableStreaming m )
             => ModelID -> m -> [ModelConfig m] -> KnownModel

-- | Every model Storyteller can wire up to a role, keyed by the name used in
--   e.g. @ROLE_PROSE_MODEL@\/@ROLE_FIXER_MODEL@. Add a model by adding one
--   entry here (and, if it's new to @universal-llm@, the capability
--   instances 'KnownModel' requires) -- nothing else changes.
knownModels :: [(String, KnownModel)]
knownModels =
  [ ("qwen35-40b",        KnownModel ViaLlamaCpp   (Model Qwen35_40B LlamaCpp)        [MaxTokens 2048, Temperature 0.8])
  , ("deepseek-v4-flash", KnownModel ViaOpenRouter (Model DeepSeekV4Flash OpenRouter) [MaxTokens 1024])
  ]

-- | Resolve an env var to a 'KnownModel', falling back to @defaultName@ if
--   unset. Errors immediately on an unrecognised name rather than silently
--   defaulting, so a typo doesn't quietly run the wrong model.
resolveKnownModel :: String -> String -> IO KnownModel
resolveKnownModel envVar defaultName = do
  name <- fromMaybe defaultName <$> lookupEnv envVar
  case lookup name knownModels of
    Just km -> pure km
    Nothing -> error $
      envVar <> "=" <> name <> " is not a known model. Known: "
      <> intercalate ", " (map fst knownModels)

-- | Wrapped in a newtype-shaped record (rather than a bare @forall a.@
--   function) so 'modelInterpreter' can return it inside 'IO' without
--   hitting GHC's impredicativity restriction. 'llmRunnerModel' rides along
--   too, for callers that need the plain model *value*, not just its type.
data LLMRunner model r = LLMRunner
  { llmRunnerModel :: model
  , runLLMRunner   :: forall a. Sem (LLM model : r) a -> Sem r a
  }

-- | Build the plain (non-streaming) 'LLM' interpreter for one model, reading
--   whatever env vars its 'ModelID' needs. Suitable for batch\/test use; see
--   'resolveRoleRunner' for interactive use that also needs to push
--   streaming preview chunks.
modelInterpreter
  :: forall model r
  .  ( ModelName model, Provider model, Routing model, Default (RoutingState model)
     , Members '[HTTP, Fail, Time, Sleep] r )
  => ModelID -> model -> [ModelConfig model] -> IO (LLMRunner model r)
modelInterpreter ViaLlamaCpp model configs = do
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  pure (LLMRunner model (interpretLLMWith (LlamaCppAuth endpoint) (route @model) model configs))
modelInterpreter ViaOpenRouter model configs = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  key <- case mKey of
    Just k  -> pure k
    Nothing -> error "OPENROUTER_API_KEY is not set"
  pure (LLMRunner model (interpretLLMWith (OpenRouterAuth key) (route @model) model configs))

-- | Resolve one role's 'KnownModel' into a stored, reusable interpreter --
--   same interpretation chain @actionStack@ used to build inline for the
--   single global @StoryModel@ before this, generalized to any registered
--   model. Unlike 'modelInterpreter'\/'LLMRunner' (used by the plain,
--   non-streaming, single-position 'withKnownModel'), 'SomeLLMRunner's own
--   field stays polymorphic in the surrounding effect row @r@ -- required
--   here because two roles' proxy 'Runix.LLM.LLM' effects can be
--   simultaneously live in one row (see 'Storyteller.Core.LLM.Role'), so
--   each role's stored runner has to be usable at whatever row position it
--   ends up peeled at, not just the one position it happened to be built
--   for.
resolveRoleRunner :: KnownModel -> IO SomeLLMRunner
resolveRoleRunner (KnownModel ViaLlamaCpp model configs) = do
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  pure (SomeLLMRunner (streamingChain (LlamaCppAuth endpoint) model configs))
resolveRoleRunner (KnownModel ViaOpenRouter model configs) = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  key <- case mKey of
    Just k  -> pure k
    Nothing -> error "OPENROUTER_API_KEY is not set"
  pure (SomeLLMRunner (streamingChain (OpenRouterAuth key) model configs))

streamingChain
  :: forall model p r a
  .  ( ModelName model, Provider model, Routing model, Default (RoutingState model), EnableStreaming model
     , RestEndpoint p, Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled] r )
  => p -> model -> [ModelConfig model] -> Sem (LLM model : r) a -> Sem r a
streamingChain auth model configs =
    restapiHTTP auth
  . llmStreamingRestAPI @model auth
  . llmRetry @p
  . interpretLLM @p (route @model) model configs
  . raiseUnder @(RestAPI p)

-- | Unpack a 'KnownModel' and build its plain 'LLMRunner', continuation-style
--   -- existentials can only be consumed within a scope, so the model's own
--   type never escapes as a return value; @k@ runs with it bound as a fresh
--   (but fully capability-equipped) type variable, same shape as e.g.
--   @Data.Some.withSome@.
withKnownModel
  :: Members '[HTTP, Fail, Time, Sleep] r
  => KnownModel
  -> (forall model. ( ModelName model, Provider model, Routing model, Default (RoutingState model)
                     , HasTools model, HasJSON model, HasReasoning model, SupportsSystemPrompt (ProviderOf model)
                     , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
      => LLMRunner model r -> IO x)
  -> IO x
withKnownModel (KnownModel modelID model configs) k = do
  runner <- modelInterpreter modelID model configs
  k runner

-- | An 'LLM' interpreter with its model type hidden -- what lets a caller
--   store "the runner resolved for this role" as a plain value (e.g. a
--   'Server.Writer.Env.ServerEnv' field) without that type leaking into
--   every signature that threads the value through. Only 'HasTools'\/
--   'SupportsSystemPrompt' are captured here because those are the only
--   capabilities any Storyteller agent (or 'Storyteller.Core.CLI.Env.modelConfigs')
--   currently needs -- see 'Storyteller.Core.LLM.Role'. The interpreter
--   field itself stays @forall r a.@ (not fixed to one @r@, unlike
--   'LLMRunner') so it can be applied at whatever position in the effect
--   row it ends up peeled at -- see 'resolveRoleRunner'.
data SomeLLMRunner where
  SomeLLMRunner :: ( HasTools model, SupportsSystemPrompt (ProviderOf model)
                   , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
                => (forall r a. Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled] r
                    => Sem (LLM model : r) a -> Sem r a)
                -> SomeLLMRunner
