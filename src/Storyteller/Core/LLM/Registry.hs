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
--   so it uses 'resolveProseRoleRunner'\/'resolveAgentRoleRunner' instead.
--
--   __Two tables, not one (2026-07-14).__ 'KnownModel' and 'KnownAgentModel'
--   are genuinely different sets, not one list with a shared capability
--   ceiling: 'Storyteller.Core.LLM.Role.ProseModel' only ever constructs
--   and consumes plain text, needing nothing beyond the config triad
--   ('SupportsSystemPrompt'\/'SupportsMaxTokens'\/'SupportsTemperature'),
--   while 'Storyteller.Core.LLM.Role.AgentModel' additionally needs
--   'HasTools'\/'HasJSON'\/'HasReasoning' for its tool-heavy workflows.
--   (The agent-integration suite's judge, see 'Agent.Integration.Judge.judge',
--   only needs 'HasTools' by itself, but is resolved from the agent table
--   anyway -- see @test/agent-integration/Main.hs@ for why.) Requiring the
--   agent set on every entry (the original design) meant a model missing
--   just 'HasJSON' couldn't be used for prose either, even though prose
--   never touches it -- forcing a choice between faking capabilities that
--   don't exist or leaving a perfectly-good prose model out of the table
--   entirely. A model that qualifies for 'KnownAgentModel' always also
--   qualifies for 'KnownModel' (superset), so nothing is lost by keeping
--   them separate; see the two lists' own Haddocks for which entries land
--   in one table, the other, or both (as two independent entries -- the
--   existential nature of both GADTs means there's no way to derive one
--   table from the other without re-listing).
module Storyteller.Core.LLM.Registry
  ( ModelID(..)
  , KnownModel(..)
  , knownModels
  , resolveKnownModel
  , KnownAgentModel(..)
  , knownAgentModels
  , resolveKnownAgentModel
  , LLMRunner(..)
  , modelInterpreter
  , withKnownModel
  , SomeProseLLMRunner(..)
  , resolveProseRoleRunner
  , SomeAgentLLMRunner(..)
  , resolveAgentRoleRunner
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
import Runix.LLM.Interpreter (interpretLLM, interpretLLMWith, LlamaCppAuth(..), OpenRouterAuth(..), AnthropicAPIKeyAuth(..))
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
import UniversalLLM.Models.DeepSeek.DeepSeek (DeepSeekV4Flash(..), DeepSeekV4Pro(..))
import UniversalLLM.Models.Moonshot.Kimi (KimiK25(..))
import UniversalLLM.Models.Minimax.M (MinimaxM25(..))
import UniversalLLM.Models.ZhipuAI.GLM (GLM47(..))
import UniversalLLM.Models.Anthropic.Claude (ClaudeFable5(..))
import UniversalLLM.Models.OpenAI.GPT (GPTOSS20B(..), GPT54(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import UniversalLLM.Providers.Anthropic (Anthropic(..))

-- | Which auth backend a model needs -- the one place that knows which env
--   vars a given backend reads.
data ModelID = ViaLlamaCpp | ViaOpenRouter | ViaAnthropic

-- | A model usable as 'Storyteller.Core.LLM.Role.ProseModel' -- needs
--   nothing beyond the config triad, since prose only ever constructs and
--   consumes plain text. Existential because each entry is a genuinely
--   different Haskell type -- this is what lets one list mix them.
data KnownModel where
  KnownModel :: ( ModelName m, Provider m, Routing m, Default (RoutingState m)
                , SupportsSystemPrompt (ProviderOf m)
                , SupportsMaxTokens (ProviderOf m), SupportsTemperature (ProviderOf m)
                , EnableStreaming m )
             => ModelID -> m -> [ModelConfig m] -> KnownModel

-- | Every model Storyteller can wire up to the prose role, keyed by the
--   name used in @ROLE_PROSE_MODEL@. Add a model by adding one entry here
--   (and, if it's new to @universal-llm@, the capability instances
--   'KnownModel' requires) -- nothing else changes. See 'knownAgentModels'
--   for the separate, stricter agent-eligible table (which the
--   agent-integration suite's @JUDGE_MODEL@ also draws from, despite
--   'Agent.Integration.Judge.judge' itself only needing 'HasTools' -- see
--   this module's Haddock), and this module's Haddock for why they're two
--   lists rather than one.
knownModels :: [(String, KnownModel)]
knownModels =
  [ ("qwen35-40b",        KnownModel ViaLlamaCpp   (Model Qwen35_40B LlamaCpp)        [MaxTokens 2048, Temperature 0.8])
  , ("deepseek-v4-flash", KnownModel ViaOpenRouter (Model DeepSeekV4Flash OpenRouter) [MaxTokens 1024])
  -- "-openrouter" suffix is deliberate, not decorative: GPT-OSS-20B is also
  -- available locally via llama.cpp, so a bare "gpt-oss-20b" would be
  -- ambiguous about which backend it resolves to.
  , ("gpt-oss-20b-openrouter", KnownModel ViaOpenRouter (Model GPTOSS20B OpenRouter) [MaxTokens 2048, Temperature 0.7])

  -- Prose-oriented additions (2026-07-14): rounding out the two proof-of-
  -- interchangeability models above into a real minimum viable lineup for
  -- prose generation. All of these are prose-only here specifically
  -- because they lack a 'HasJSON' instance in universal-llm -- see
  -- 'knownAgentModels' for the ones with the fuller capability set.
  , ("deepseek-v4-pro",   KnownModel ViaOpenRouter (Model DeepSeekV4Pro OpenRouter)   [MaxTokens 2048])
  , ("gpt-5.4",           KnownModel ViaOpenRouter (Model GPT54 OpenRouter)           [MaxTokens 2048, Temperature 0.8])
  , ("kimi-k2.5",         KnownModel ViaOpenRouter (Model KimiK25 OpenRouter)         [MaxTokens 2048])
  , ("minimax-m2.5",      KnownModel ViaOpenRouter (Model MinimaxM25 OpenRouter)      [MaxTokens 2048])
  , ("glm-4.7",           KnownModel ViaOpenRouter (Model GLM47 OpenRouter)           [MaxTokens 2048])
  , ("claude-fable-5",    KnownModel ViaAnthropic  (Model ClaudeFable5 Anthropic)     [MaxTokens 2048])

  -- Not yet in universal-llm at all -- stubbed so a typo'd env var points at
  -- the actual gap instead of "unknown model name".
  , ("mistral-large", error "mistral-large is not yet added to universal-llm -- see UniversalLLM.Models.Mistral (does not exist yet); add a model module there first, then a real KnownModel entry here")
  , ("llama",          error "llama is not yet added to universal-llm -- see UniversalLLM.Models.Meta (does not exist yet); add a model module there first, then a real KnownModel entry here")
  ]

-- | A model usable as 'Storyteller.Core.LLM.Role.AgentModel' -- adds
--   'HasJSON'\/'HasReasoning' on top of 'KnownModel''s set, for the
--   tool-heavy agents ('Storyteller.Writer.Agent.ReplaceTool',
--   'Storyteller.Writer.Agent.Chat', 'Storyteller.Writer.Agent.Outline.splitOutlineAgent')
--   that may go on to use them outbound. A superset of 'KnownModel' by
--   construction, so every entry here could equally be a 'KnownModel'
--   entry -- but since the two GADTs are existential, that has to be
--   re-declared per model rather than derived automatically; see this
--   module's Haddock.
data KnownAgentModel where
  KnownAgentModel :: ( ModelName m, Provider m, Routing m, Default (RoutingState m)
                     , HasTools m, HasJSON m, HasReasoning m, SupportsSystemPrompt (ProviderOf m)
                     , SupportsMaxTokens (ProviderOf m), SupportsTemperature (ProviderOf m)
                     , EnableStreaming m )
                  => ModelID -> m -> [ModelConfig m] -> KnownAgentModel

-- | Every model Storyteller can wire up to the agent role, keyed by the
--   name used in e.g. @ROLE_AGENT_MODEL@\/@STORY_MODEL@ (the
--   agent-integration suite's @STORY_MODEL@ backs *both* proxy roles at
--   once -- see @test/agent-integration/Main.hs@ -- so it has to be drawn
--   from here, not 'knownModels', even though it's also used as prose).
knownAgentModels :: [(String, KnownAgentModel)]
knownAgentModels =
  [ ("qwen35-40b",        KnownAgentModel ViaLlamaCpp   (Model Qwen35_40B LlamaCpp)        [MaxTokens 2048, Temperature 0.8])
  , ("deepseek-v4-flash", KnownAgentModel ViaOpenRouter (Model DeepSeekV4Flash OpenRouter) [MaxTokens 1024])
  , ("gpt-oss-20b-openrouter", KnownAgentModel ViaOpenRouter (Model GPTOSS20B OpenRouter) [MaxTokens 2048, Temperature 0.7])
  , ("deepseek-v4-pro",   KnownAgentModel ViaOpenRouter (Model DeepSeekV4Pro OpenRouter)   [MaxTokens 2048])
  , ("gpt-5.4",           KnownAgentModel ViaOpenRouter (Model GPT54 OpenRouter)           [MaxTokens 2048, Temperature 0.8])

  -- Prose-only: exist in 'knownModels' but lack 'HasJSON'\/'HasReasoning'
  -- instances, so they can't satisfy 'KnownAgentModel'. Stubbed here too
  -- (rather than just omitted) so setting e.g. @ROLE_AGENT_MODEL=kimi-k2.5@
  -- fails with a reason instead of "unknown model name", which would
  -- wrongly suggest the name itself is the problem.
  , ("kimi-k2.5",      error "KimiK25 via OpenRouter has no HasJSON/HasReasoning instance in universal-llm -- prose-only, see the 'kimi-k2.5' entry in knownModels; add capability instances in UniversalLLM.Models.Moonshot.Kimi to make it agent-eligible")
  , ("minimax-m2.5",   error "MinimaxM25 via OpenRouter has no HasJSON/HasReasoning instance in universal-llm -- prose-only, see the 'minimax-m2.5' entry in knownModels; add capability instances in UniversalLLM.Models.Minimax.M to make it agent-eligible")
  , ("glm-4.7",        error "GLM47 via OpenRouter has no HasJSON/HasReasoning instance in universal-llm -- prose-only, see the 'glm-4.7' entry in knownModels; add capability instances in UniversalLLM.Models.ZhipuAI.GLM to make it agent-eligible")
  , ("claude-fable-5", error "ClaudeFable5 via Anthropic has no HasJSON/HasReasoning instance in universal-llm -- prose-only, see the 'claude-fable-5' entry in knownModels; add capability instances in UniversalLLM.Models.Anthropic.Claude to make it agent-eligible")

  , ("mistral-large", error "mistral-large is not yet added to universal-llm -- see UniversalLLM.Models.Mistral (does not exist yet); add a model module there first, then a real KnownAgentModel entry here")
  , ("llama",          error "llama is not yet added to universal-llm -- see UniversalLLM.Models.Meta (does not exist yet); add a model module there first, then a real KnownAgentModel entry here")
  ]

-- | Resolve an env var to a value from a named table, falling back to
--   @defaultName@ if unset. Errors immediately on an unrecognised name
--   rather than silently defaulting, so a typo doesn't quietly run the
--   wrong model. Shared by 'resolveKnownModel'\/'resolveKnownAgentModel' --
--   the two tables differ in what a model needs to qualify, not in how a
--   name gets looked up.
resolveNamed :: [(String, a)] -> String -> String -> IO a
resolveNamed table envVar defaultName = do
  name <- fromMaybe defaultName <$> lookupEnv envVar
  case lookup name table of
    Just v  -> pure v
    Nothing -> error $
      envVar <> "=" <> name <> " is not a known model. Known: "
      <> intercalate ", " (map fst table)

-- | Resolve an env var to a 'KnownModel' -- see 'resolveNamed'.
resolveKnownModel :: String -> String -> IO KnownModel
resolveKnownModel = resolveNamed knownModels

-- | Resolve an env var to a 'KnownAgentModel' -- see 'resolveNamed'.
resolveKnownAgentModel :: String -> String -> IO KnownAgentModel
resolveKnownAgentModel = resolveNamed knownAgentModels

-- | Wrapped in a newtype-shaped record (rather than a bare @forall a.@
--   function) so 'modelInterpreter' can return it inside 'IO' without
--   hitting GHC's impredicativity restriction. 'llmRunnerModel' rides along
--   too, for callers that need the plain model *value*, not just its type.
data LLMRunner model r = LLMRunner
  { llmRunnerModel :: model
  , runLLMRunner   :: forall a. Sem (LLM model : r) a -> Sem r a
  }

-- | A 'RestEndpoint' auth value with its concrete type hidden -- what lets
--   'resolveAuth' return one of three genuinely different auth types
--   ('LlamaCppAuth'\/'OpenRouterAuth'\/'AnthropicAPIKeyAuth') from a single
--   function, keyed only by 'ModelID'. 'ModelID' says which backend a model
--   needs; this is where that maps to an actual auth value.
data SomeAuth where
  SomeAuth :: RestEndpoint p => p -> SomeAuth

-- | Resolve the env var(s) one 'ModelID' backend needs into an auth value --
--   the one place that knows which env var name goes with which backend.
--   Shared by 'modelInterpreter'\/'resolveProseRoleRunner'\/
--   'resolveAgentRoleRunner': all three just differ in what they build with
--   the resulting auth, not in how they get it.
resolveAuth :: ModelID -> IO SomeAuth
resolveAuth ViaLlamaCpp = do
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  pure (SomeAuth (LlamaCppAuth endpoint))
resolveAuth ViaOpenRouter = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  key <- case mKey of
    Just k  -> pure k
    Nothing -> error "OPENROUTER_API_KEY is not set"
  pure (SomeAuth (OpenRouterAuth key))
resolveAuth ViaAnthropic = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  key <- case mKey of
    Just k  -> pure k
    Nothing -> error "ANTHROPIC_API_KEY is not set"
  pure (SomeAuth (AnthropicAPIKeyAuth key))

-- | Build the plain (non-streaming) 'LLM' interpreter for one model, reading
--   whatever env vars its 'ModelID' needs. Suitable for batch\/test use; see
--   'resolveProseRoleRunner'\/'resolveAgentRoleRunner' for interactive use
--   that also needs to push streaming preview chunks.
modelInterpreter
  :: forall model r
  .  ( ModelName model, Provider model, Routing model, Default (RoutingState model)
     , Members '[HTTP, Fail, Time, Sleep, Logging] r )
  => ModelID -> model -> [ModelConfig model] -> IO (LLMRunner model r)
modelInterpreter modelID model configs = do
  SomeAuth auth <- resolveAuth modelID
  pure (LLMRunner model (interpretLLMWith auth (route @model) model configs))

-- | Resolve the prose role's 'KnownModel' into a stored, reusable
--   interpreter -- same interpretation chain @actionStack@ used to build
--   inline for the single global @StoryModel@ before this, generalized to
--   any registered model. Unlike 'modelInterpreter'\/'LLMRunner' (used by
--   the plain, non-streaming, single-position 'withKnownModel'),
--   'SomeProseLLMRunner's own field stays polymorphic in the surrounding
--   effect row @r@ -- required here because two roles' proxy 'Runix.LLM.LLM'
--   effects can be simultaneously live in one row (see
--   'Storyteller.Core.LLM.Role'), so each role's stored runner has to be
--   usable at whatever row position it ends up peeled at, not just the one
--   position it happened to be built for.
--   'mLogDir', when given, is a directory (expected to be repo-local and
--   never committed -- see 'Server.Writer.Env.loadServerEnv') that every
--   raw HTTP request/response this role's model makes gets dumped into as
--   one JSON file per call. Occasional-check-in tooling, not a
--   permanent-monitoring one: see 'loggingStreamingChain'.
resolveProseRoleRunner :: Maybe FilePath -> KnownModel -> IO SomeProseLLMRunner
resolveProseRoleRunner mLogDir (KnownModel modelID model configs) = do
  SomeAuth auth <- resolveAuth modelID
  pure (SomeProseLLMRunner (loggingStreamingChain mLogDir auth model configs))

-- | Same as 'resolveProseRoleRunner', for the agent role's 'KnownAgentModel'.
resolveAgentRoleRunner :: Maybe FilePath -> KnownAgentModel -> IO SomeAgentLLMRunner
resolveAgentRoleRunner mLogDir (KnownAgentModel modelID model configs) = do
  SomeAuth auth <- resolveAuth modelID
  pure (SomeAgentLLMRunner (loggingStreamingChain mLogDir auth model configs))

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
--   (but capability-equipped, matching 'KnownModel''s own set) type
--   variable, same shape as e.g. @Data.Some.withSome@.
withKnownModel
  :: Members '[HTTP, Fail, Time, Sleep, Logging] r
  => KnownModel
  -> (forall model. ( ModelName model, Provider model, Routing model, Default (RoutingState model)
                     , SupportsSystemPrompt (ProviderOf model)
                     , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
      => LLMRunner model r -> IO x)
  -> IO x
withKnownModel (KnownModel modelID model configs) k = do
  runner <- modelInterpreter modelID model configs
  k runner

-- | An 'LLM' interpreter with its model type hidden -- what lets a caller
--   store "the runner resolved for the prose role" as a plain value (e.g. a
--   'Server.Writer.Env.ServerEnv' field) without that type leaking into
--   every signature that threads the value through.
--   'SupportsSystemPrompt'\/'SupportsMaxTokens'\/'SupportsTemperature' are
--   captured here because those are exactly what
--   'Storyteller.Core.LLM.Role.reinterpretProse' needs on @chosenModel@
--   (matching what every 'KnownModel' entry already guarantees) -- see
--   'Storyteller.Core.LLM.Role'. The interpreter field itself stays
--   @forall r a.@ (not fixed to one @r@, unlike 'LLMRunner') so it can be
--   applied at whatever position in the effect row it ends up peeled at --
--   see 'resolveProseRoleRunner'.
data SomeProseLLMRunner where
  SomeProseLLMRunner :: ( SupportsSystemPrompt (ProviderOf model)
                        , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
                     => (forall r a. Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled, Logging, Embed IO] r
                         => Sem (LLM model : r) a -> Sem r a)
                     -> SomeProseLLMRunner

-- | Same as 'SomeProseLLMRunner', for the agent role: adds 'HasJSON'\/
--   'HasReasoning', matching what 'Storyteller.Core.LLM.Role.reinterpretAgent'
--   needs on @chosenModel@ and what every 'KnownAgentModel' entry
--   guarantees -- see 'resolveAgentRoleRunner'.
data SomeAgentLLMRunner where
  SomeAgentLLMRunner :: ( HasTools model, HasJSON model, HasReasoning model
                        , SupportsSystemPrompt (ProviderOf model)
                        , SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model) )
                     => (forall r a. Members '[HTTP, HTTPStreaming, StreamChunk StreamEvent, Fail, Time, Sleep, Config StreamingEnabled, Logging, Embed IO] r
                         => Sem (LLM model : r) a -> Sem r a)
                     -> SomeAgentLLMRunner
