{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.ByteString as BS
import Polysemy
import Polysemy.Fail (Fail)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)

import Runix.Config (Config)
import Runix.FileSystem (FileSystemWrite(..))
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLM, interpretLLMWith, LlamaCppAuth(..), OpenRouterAuth(..))
import Runix.Logging (Logging)
import Runix.LLM.Streaming (llmStreamingRestAPI, StreamEvent, StreamingEnabled)
import Runix.RestAPI (RestEndpoint, RestAPI, restapiHTTP, llmRetry)
import Runix.StreamChunk (StreamChunk)
import Runix.Time (Time, Sleep)
import Runix.Tracing.FileLog (logHTTPStreamingRequests)
import UniversalLLM
  ( EnableStreaming, HasJSON, HasReasoning, HasTools, Model(..), ModelConfig(..)
  , ModelName, Provider, ProviderOf, Routing, RoutingState
  , SupportsMaxTokens, SupportsSystemPrompt, SupportsTemperature, route )
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_40B(..))
import UniversalLLM.Models.DeepSeek.DeepSeek (DeepSeekV4Flash(..))
import UniversalLLM.Models.OpenAI.GPT (GPTOSS20B(..))
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
--   e.g. @ROLE_PROSE_MODEL@\/@ROLE_AGENT_MODEL@. Add a model by adding one
--   entry here (and, if it's new to @universal-llm@, the capability
--   instances 'KnownModel' requires) -- nothing else changes.
knownModels :: [(String, KnownModel)]
knownModels =
  [ ("qwen35-40b",        KnownModel ViaLlamaCpp   (Model Qwen35_40B LlamaCpp)        [MaxTokens 2048, Temperature 0.8])
  , ("deepseek-v4-flash", KnownModel ViaOpenRouter (Model DeepSeekV4Flash OpenRouter) [MaxTokens 1024])
  -- "-openrouter" suffix is deliberate, not decorative: GPT-OSS-20B is also
  -- available locally via llama.cpp, so a bare "gpt-oss-20b" would be
  -- ambiguous about which backend it resolves to.
  , ("gpt-oss-20b-openrouter", KnownModel ViaOpenRouter (Model GPTOSS20B OpenRouter) [MaxTokens 2048, Temperature 0.7])
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
     , Members '[HTTP, Fail, Time, Sleep, Logging] r )
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
--   'mLogDir', when given, is a directory (expected to be repo-local and
--   never committed -- see 'Server.Writer.Env.loadServerEnv') that every
--   raw HTTP request/response this role's model makes gets dumped into as
--   one JSON file per call. Occasional-check-in tooling, not a
--   permanent-monitoring one: see 'loggingStreamingChain'.
resolveRoleRunner :: Maybe FilePath -> KnownModel -> IO SomeLLMRunner
resolveRoleRunner mLogDir (KnownModel ViaLlamaCpp model configs) = do
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  pure (SomeLLMRunner (loggingStreamingChain mLogDir (LlamaCppAuth endpoint) model configs))
resolveRoleRunner mLogDir (KnownModel ViaOpenRouter model configs) = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  key <- case mKey of
    Just k  -> pure k
    Nothing -> error "OPENROUTER_API_KEY is not set"
  pure (SomeLLMRunner (loggingStreamingChain mLogDir (OpenRouterAuth key) model configs))

streamingChain
  :: forall model p r a
  .  ( ModelName model, Provider model, Routing model, Default (RoutingState model), EnableStreaming model
     , RestEndpoint p, Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled, Logging] r )
  => p -> model -> [ModelConfig model] -> Sem (LLM model : r) a -> Sem r a
streamingChain auth model configs =
    restapiHTTP auth
  . llmStreamingRestAPI @model auth
  . llmRetry @p
  . interpretLLM @p (route @model) model configs
  . raiseUnder @(RestAPI p)

-- | A repo-local directory used only as 'Runix.FileSystem.HasProjectPath'
--   chroot root for request-log JSON files -- a distinct type (rather than
--   reusing 'FilePath' itself, which already has an instance) so this
--   filesystem scope can never be confused with some other 'FilePath'
--   -tagged effect that happens to be live in the same row.
data RequestLogProject

-- | Direct real-disk interpreter for 'RequestLogProject', rooted at a
--   fixed directory -- deliberately not 'Runix.FileSystem.fileSystemLocal'
--   (the general chroot/path-translation machinery 'Server.Core.File'-style
--   code uses): 'logHTTPStreamingRequests' only ever writes flat,
--   already-sanitized filenames of its own choosing, so there's no path
--   traversal to guard against and no read/list/chroot machinery to pull
--   in for it.
runRequestLogFS :: Member (Embed IO) r => FilePath -> Sem (FileSystemWrite RequestLogProject : r) a -> Sem r a
runRequestLogFS dir = interpret $ \case
  WriteFile p d        -> embed (ioResult (BS.writeFile (dir </> p) d))
  CreateDirectory cp p -> embed (ioResult (createDirectoryIfMissing cp (dir </> p)))
  Remove _ p           -> embed (ioResult (removeFile (dir </> p)))
  where
    ioResult act = either (Left . show) Right <$> tryIOError act

-- | 'streamingChain', optionally wrapped with a raw request/response JSON
--   dump of every HTTP call the model makes (see
--   'Runix.Tracing.FileLog.logHTTPStreamingRequests') -- kept as a thin
--   wrapper, applied once at startup per role, rather than folded into
--   'streamingChain' itself, so the common (logging off) path never pays
--   for the extra 'FileSystemWrite' interpretation layer, and so
--   'streamingChain' itself stays usable without the extra 'Embed IO'
--   requirement logging needs.
loggingStreamingChain
  :: forall model p r a
  .  ( ModelName model, Provider model, Routing model, Default (RoutingState model), EnableStreaming model
     , RestEndpoint p, Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled, Logging, Embed IO] r )
  => Maybe FilePath -> p -> model -> [ModelConfig model] -> Sem (LLM model : r) a -> Sem r a
loggingStreamingChain Nothing       auth model configs action = streamingChain auth model configs action
loggingStreamingChain (Just logDir) auth model configs action =
    runRequestLogFS logDir
  . logHTTPStreamingRequests @RequestLogProject
  . raise
  $ streamingChain auth model configs action

-- | Unpack a 'KnownModel' and build its plain 'LLMRunner', continuation-style
--   -- existentials can only be consumed within a scope, so the model's own
--   type never escapes as a return value; @k@ runs with it bound as a fresh
--   (but fully capability-equipped) type variable, same shape as e.g.
--   @Data.Some.withSome@.
withKnownModel
  :: Members '[HTTP, Fail, Time, Sleep, Logging] r
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
--   every signature that threads the value through. 'HasTools'\/
--   'HasJSON'\/'HasReasoning'\/'SupportsSystemPrompt'\/'SupportsMaxTokens'\/
--   'SupportsTemperature' are captured here because those are exactly what
--   'Storyteller.Core.LLM.Role.reinterpretProse'\/'reinterpretAgent' need on
--   @chosenModel@ (matching what every 'KnownModel' entry already
--   guarantees) -- see 'Storyteller.Core.LLM.Role'. The interpreter field
--   itself stays @forall r a.@ (not fixed to one @r@, unlike 'LLMRunner')
--   so it can be applied at whatever position in the effect row it ends up
--   peeled at -- see 'resolveRoleRunner'.
data SomeLLMRunner where
  SomeLLMRunner :: ( HasTools model, HasJSON model, HasReasoning model
                   , SupportsSystemPrompt (ProviderOf model)
                   , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
                => (forall r a. Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled, Logging, Embed IO] r
                    => Sem (LLM model : r) a -> Sem r a)
                -> SomeLLMRunner
