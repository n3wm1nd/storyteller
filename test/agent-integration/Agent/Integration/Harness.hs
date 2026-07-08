{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Building blocks for the agent integration suite's effect stack: real
--   LLM calls, cached to disk via 'Runix.LLM.Cache.cacheLLM' so a recorded
--   response replays without the network on every run after the first. See
--   @../PLAN@ (agent integration test suite) for why this suite exists and
--   is kept separate from @storyteller-test@.
--
--   Neither role (agent-under-test, judge) has a model baked in anywhere
--   in this module: 'knownModels' is the one registry of models this
--   suite can wire up, and @STORY_MODEL@\/@JUDGE_MODEL@ independently pick
--   an entry from it at process startup (@Main.hs@, once, not per spec).
--   Which pairing is "sensible" (different models to avoid same-model
--   judging, same model to test that deliberately, either role using
--   either backend) is a run-time decision, not something this module
--   assumes.
module Agent.Integration.Harness
  ( CacheProject(..)
  , KnownModel(..)
  , LLMRunner(..)
  , Main
  , ModelID(..)
  , Runner
  , knownModels
  , mainBranch
  , modelInterpreter
  , resolveFixture
  , resolveKnownModel
  , runExpect
  , withKnownModel
  ) where

import Data.Default (Default)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.Fail (Fail)
import System.Environment (lookupEnv)
import Test.Hspec (expectationFailure)

import Paths_storyteller (getDataFileName)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, HasProjectPath(..))
import Runix.Git (Git)
import Runix.HTTP (HTTP)
import Runix.LLM (LLM)
import Runix.Logging (Logging)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..), OpenRouterAuth(..))
import Runix.Time (Time, Sleep)
import UniversalLLM
  ( HasJSON, HasReasoning, HasTools, Model(..), ModelConfig(..)
  , ModelName, Provider, ProviderOf, Routing, RoutingState, SupportsSystemPrompt, route )
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_40B(..))
import UniversalLLM.Models.DeepSeek.DeepSeek (DeepSeekV4Flash(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))

import Storyteller.Core.Git (BranchOp, BranchTag)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))

-- | Chroot marker for the shared on-disk response cache. A plain
--   'FilePath' isn't reused directly (unlike 'Runix.FileSystem.HasProjectPath's
--   own @FilePath@ instance) so this stays its own distinct effect type.
--   One directory, shared by every model\/role: 'Runix.LLM.Cache'\'s cache
--   key already hashes in the model name, so an agent-role and a
--   judge-role entry (or two different models' entries) never collide on
--   the same key -- nothing here needs to know or care which role is
--   asking.
newtype CacheProject = CacheProject FilePath

instance HasProjectPath CacheProject where
  getProjectPath (CacheProject p) = p

-- | Which auth backend a model needs -- the one place that knows which
--   env vars a given backend reads.
data ModelID = ViaLlamaCpp | ViaOpenRouter

-- | One model this suite knows how to wire up: its capability instances
--   (everything 'modelInterpreter', 'Runix.LLM.Cache.cacheLLM', and the
--   agents under test collectively need), which auth backend it uses, the
--   model value itself, and its default configs. Existential because each
--   entry in 'knownModels' is a genuinely different Haskell type -- this
--   is what lets one list mix them.
data KnownModel where
  KnownModel :: ( ModelName m, Provider m, Routing m, Default (RoutingState m)
                , HasTools m, HasJSON m, HasReasoning m, SupportsSystemPrompt (ProviderOf m) )
             => ModelID -> m -> [ModelConfig m] -> KnownModel

-- | Every model this suite can wire up as either role, keyed by the name
--   used in @STORY_MODEL@\/@JUDGE_MODEL@. Add a model by adding one entry
--   here (and, if it's new to @universal-llm@, the capability instances
--   'KnownModel' requires) -- nothing else in this module changes.
knownModels :: [(String, KnownModel)]
knownModels =
  [ ("qwen35-40b",        KnownModel ViaLlamaCpp   (Model Qwen35_40B LlamaCpp)     [MaxTokens 2048, Temperature 0.8])
  , ("deepseek-v4-flash", KnownModel ViaOpenRouter (Model DeepSeekV4Flash OpenRouter) [MaxTokens 1024])
  ]

-- | Resolve a @STORY_MODEL@\/@JUDGE_MODEL@-shaped env var to a
--   'KnownModel', falling back to @defaultName@ if unset. Errors
--   immediately on an unrecognised name rather than silently defaulting,
--   so a typo doesn't quietly run the wrong model.
resolveKnownModel :: String -> String -> IO KnownModel
resolveKnownModel envVar defaultName = do
  name <- fromMaybe defaultName <$> lookupEnv envVar
  case lookup name knownModels of
    Just km -> pure km
    Nothing -> error $
      envVar <> "=" <> name <> " is not a known model. Known: "
      <> intercalate ", " (map fst knownModels)

-- | 'runLLMRunner' is wrapped in a newtype so 'modelInterpreter' can
--   return a @forall a.@ interpreter inside 'IO' without hitting GHC's
--   impredicativity restriction (@IO (forall a. ...)@ applies @IO@
--   directly to a polymorphic type, which plain 'RankNTypes' doesn't
--   allow; hiding the forall behind a newtype field sidesteps that the
--   same way e.g. @ST@'s own runners commonly do). 'llmRunnerModel' rides
--   along too -- callers (see @Main.hs@'s @cacheLLM@ wiring) need the
--   plain model *value*, not just its type, and this is the one place
--   that has it once a 'KnownModel' has been unpacked.
data LLMRunner model r = LLMRunner
  { llmRunnerModel :: model
  , runLLMRunner   :: forall a. Sem (LLM model : r) a -> Sem r a
  }

-- | Build the 'LLM' interpreter for one model, reading whatever env vars
--   its 'ModelID' needs -- only reached on a cache miss, so a spec whose
--   scenario is fully cached never touches the environment at all.
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
    Nothing -> error "OPENROUTER_API_KEY is not set (needed on a cache miss)"
  pure (LLMRunner model (interpretLLMWith (OpenRouterAuth key) (route @model) model configs))

-- | Unpack a 'KnownModel' and build its 'LLMRunner', continuation-style --
--   existentials can only be consumed within a scope, so the model's own
--   type never escapes as a return value; @k@ runs with it bound as a
--   fresh (but fully capability-equipped) type variable, same shape as
--   e.g. @Data.Some.withSome@.
withKnownModel
  :: Members '[HTTP, Fail, Time, Sleep] r
  => KnownModel
  -> (forall model. ( ModelName model, Provider model, Routing model, Default (RoutingState model)
                     , HasTools model, HasJSON model, HasReasoning model, SupportsSystemPrompt (ProviderOf model) )
      => LLMRunner model r -> IO x)
  -> IO x
withKnownModel (KnownModel modelID model configs) k = do
  runner <- modelInterpreter modelID model configs
  k runner

-- | Resolve a path under @test/fixtures/@ to an absolute one, robust to
--   'cabal test' not running with the package root as its working
--   directory (it doesn't) -- same mechanism 'Storyteller.CharGenSpec' uses
--   for @test/fixtures/minimal.yaml@ in the main suite.
resolveFixture :: FilePath -> IO FilePath
resolveFixture = getDataFileName

-- | The one content branch every scenario gets for free, already created
--   before its action runs (see @Main.hs@) -- most agents work against a
--   single branch, so that's the default a scenario starts with. Nothing
--   stops a scenario from opening more branches itself the same way (via
--   'Storyteller.Core.Storage.createBranch' then
--   'Storyteller.Core.Git.runBranchAndFS' @\@SomeOtherTag@) -- 'Git' and
--   'StoryStorage' are already in 'Runner''s row for exactly that.
mainBranch :: BranchName
mainBranch = BranchName "main"

-- | Every effect a scenario runs in: both models, prompt overrides,
--   logging, and (see 'mainBranch') git-backed storage -- 'Git'\/
--   'StoryStorage' directly, plus 'mainBranch''s own already-open
--   'BranchOp'\/'FileSystem' trio. Shared between 'Runner' and 'runExpect'
--   so the two can't drift apart.
type ScenarioEffects storyModel judgeModel =
  '[ LLM storyModel, LLM judgeModel, PromptStorage, Logging
   , Git, StoryStorage, BranchOp Main
   , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), FileSystemWrite (BranchTag Main)
   , Fail, Embed IO
   ]

-- | The fully-built interpreter every spec runs its scenarios through --
--   built exactly once by @Main.hs@ (inside a pair of nested
--   'withKnownModel' calls, which is what pins @storyModel@\/@judgeModel@
--   to concrete types for the rest of that scope) and passed down -- see
--   this module's Haddock. @action@ stays row-polymorphic via 'Members'
--   (the usual Polysemy shape), not pinned to one closed, exactly-ordered
--   effect list -- a spec's @do@ block is free to call into whatever
--   combination of 'writeAgent'\/'reworkAtom'\/'judge'\/branch operations
--   it needs without caring what order this module happens to list the
--   required effects in.
--
--   The @forall r.@ is deliberately scoped *inside* the parens, over just
--   the argument -- same shape as @runST :: (forall s. ST s a) -> a@ --
--   not outside alongside @a@: quantifying @r@ at the same level as @a@
--   makes it a fresh, unconstrained metavariable at every call site
--   (ambiguous -- GHC has nothing to pin it to), since nothing in
--   @IO (Either String a)@ mentions @r@ at all.
type Runner storyModel judgeModel
  = forall a. (forall r. Members (ScenarioEffects storyModel judgeModel) r => Sem r a) -> IO (Either String a)

-- | Run a scenario and turn a 'Fail' into an hspec failure -- the
--   @result <- runner (...); case result of Left err -> expectationFailure
--   err; Right () -> pure ()@ dance every @it@ block otherwise repeats
--   verbatim, since every scenario's assertions already run via 'embed'
--   inside the action itself (see @Agent.Integration.CharContextWriteSpec@\/
--   @Agent.Integration.ReworkAtomSpec@) and so end in @()@.
runExpect
  :: forall storyModel judgeModel
  .  Runner storyModel judgeModel
  -> (forall r. Members (ScenarioEffects storyModel judgeModel) r => Sem r ())
  -> IO ()
runExpect runner action = runner action >>= either expectationFailure pure

