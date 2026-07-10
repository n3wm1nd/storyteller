{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Building blocks for the agent integration suite's effect stack: real
--   LLM calls, cached to disk via 'Runix.LLM.Cache.cacheLLM' so a recorded
--   response replays without the network on every run after the first. See
--   @../PLAN.md@ (agent integration test suite) for why this suite exists
--   and is kept separate from @storyteller-test@.
--
--   Neither role (agent-under-test, judge) has a model baked in anywhere in
--   this module: 'Storyteller.Core.LLM.Registry.knownModels' is the one
--   registry of models this suite (and production -- see
--   'Storyteller.Core.LLM.Role') can wire up, and @STORY_MODEL@\/@JUDGE_MODEL@
--   independently pick an entry from it at process startup (@Main.hs@, once,
--   not per spec). Which pairing is "sensible" (different models to avoid
--   same-model judging, same model to test that deliberately, either role
--   using either backend) is a run-time decision, not something this module
--   assumes.
module Agent.Integration.Harness
  ( CacheProject(..)
  , KnownModel(..)
  , LLMRunner(..)
  , Main
  , ModelID(..)
  , Runner
  , assertToolCallBudget
  , knownModels
  , loggingPretty
  , mainBranch
  , modelInterpreter
  , quietSetup
  , recordToolCalls
  , withPromptOverride
  , resolveFixture
  , resolveKnownModel
  , runExpect
  , withKnownModel
  ) where

import Data.List (intercalate)
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.Output (Output, runOutputList, output)
import Test.Hspec (expectationFailure)

import Paths_storyteller (getDataFileName)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, HasProjectPath(..))
import Runix.Git (Git)
import Runix.LLM (LLM(..), Message(..))
import Runix.Logging (Level(..), Logging(..), info)
import UniversalLLM (ToolCall(..))

import Agent.Integration.ToolCallQuality
  (TurnReport(..), invalidCallsSinceLastUser, reportTurn)
import Storyteller.Common.Splitter (Splitter)
import Storyteller.Core.Git (BranchOp, BranchTag)
import Storyteller.Core.LLM.Registry
  ( KnownModel(..), LLMRunner(..), ModelID(..)
  , knownModels, modelInterpreter, resolveKnownModel, withKnownModel )
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (Prompt, PromptKey, PromptStorage(..))
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

-- | Every effect a scenario runs in: 'LLMs' (both agent roles -- see
--   'Storyteller.Core.LLM.Role' -- rather than one free @storyModel@
--   variable, since production agents ('writeAgent', 'reworkAtom',
--   'splitOutlineAgent', ...) now hardcode their role internally instead of
--   staying generic), the judge's own independent @judgeModel@, prompt
--   overrides, logging, and (see 'mainBranch') git-backed storage --
--   'Git'\/'StoryStorage' directly, plus 'mainBranch''s own already-open
--   'BranchOp'\/'FileSystem' trio. Shared between 'Runner' and 'runExpect'
--   so the two can't drift apart.
type ScenarioEffects judgeModel r =
  ( LLMs r
  , Members
      '[ LLM judgeModel, PromptStorage, Logging
       , Git, StoryStorage, BranchOp Main, Splitter
       , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), FileSystemWrite (BranchTag Main)
       , Fail, Embed IO
       ] r
  )

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
type Runner judgeModel
  = forall a. (forall r. ScenarioEffects judgeModel r => Sem r a) -> IO (Either String a)

-- | Run a scenario and turn a 'Fail' into an hspec failure -- the
--   @result <- runner (...); case result of Left err -> expectationFailure
--   err; Right () -> pure ()@ dance every @it@ block otherwise repeats
--   verbatim, since every scenario's assertions already run via 'embed'
--   inside the action itself (see @Agent.Integration.CharContextWriteSpec@\/
--   @Agent.Integration.ReworkAtomSpec@) and so end in @()@.
runExpect
  :: forall judgeModel
  .  Runner judgeModel
  -> (forall r. ScenarioEffects judgeModel r => Sem r ())
  -> IO ()
runExpect runner action = runner action >>= either expectationFailure pure

-- | Log one response message's tool call, if it has one -- a call that
--   parsed (name and arguments) or one that didn't (name and parse error).
--   Long-running, real-LLM scenarios should keep the caller up to date via
--   'Runix.Logging.info'\/'Runix.Logging.warning' rather than going silent
--   until the final result (see @../PLAN.md@); this is the one place that
--   applies to tool calls specifically, shared by 'recordToolCalls' and
--   'assertGoodToolCalls' so every caller of either gets it for free instead
--   of having to remember to layer on a separate logging wrapper.
logToolCall :: Member Logging r => Message model -> Sem r ()
logToolCall (AssistantTool (ToolCall _ name args)) =
  info $ "tool call: " <> name <> " " <> T.pack (show args)
logToolCall (AssistantTool (InvalidToolCall _ name _raw err)) =
  info $ "invalid tool call to " <> name <> ": " <> err
logToolCall _ = pure ()

-- | Run @action@ while recording every raw response 'Runix.LLM.QueryLLM'
--   returns for @model@'s role, oldest turn first -- so a spec can inspect
--   turn-by-turn tool-call validity (retries, malformed JSON) that a
--   production agent's own return type intentionally doesn't expose (e.g.
--   'Storyteller.Writer.Agent.Outline.splitOutlineAgent' returns only
--   @['Storyteller.Writer.Agent.Outline.ChapterBeats']@, the end result, not
--   how many tries it took to get there -- see 'Agent.Integration.ToolCallQuality').
--   Also logs each turn's tool call(s) via 'logToolCall' as they arrive, so a
--   live run's output shows what the model actually tried, not just the
--   eventual result -- useful for a long generation where the next visible
--   output might otherwise be minutes away.
--
--   Built the same way 'Runix.Logging.loggingList' captures logs: 'intercept'
--   keeps 'LLM' @model@ live in the row (each call still falls through to the
--   real interpreter via the inner 'send', exactly like
--   'Runix.LLM.Cache.cacheLLM'), while 'output' each response into a fresh
--   'Polysemy.Output.Output' layer that 'runOutputList' collects and peels
--   back off -- no mutable state, no new member in 'ScenarioEffects'.
recordToolCalls
  :: forall model r a
  .  Members '[LLM model, Logging] r
  => Sem r a -> Sem r (a, [[Message model]])
recordToolCalls action = do
  (turns, result) <- runOutputList $ intercept @(LLM model) recorder (raise action)
  return (result, turns)
  where
    recorder :: forall m x. LLM model m x -> Sem (Output [Message model] : r) x
    recorder (QueryLLM configs msgs) = do
      resp <- send (QueryLLM configs msgs)
      case resp of
        Right msgs' -> mapM_ logToolCall msgs' >> output msgs' >> pure resp
        Left _       -> pure resp

-- | Like 'recordToolCalls', but fails (via 'Polysemy.Fail.Fail') from
--   inside the interceptor itself, the moment the cumulative count of
--   malformed calls exceeds @budget@ -- instead of letting the rest of the
--   turn-by-turn loop, including retries beyond what's tolerable, run to
--   completion before anything notices. A model recovering after one or two
--   retries is legitimate self-correction, not a free pass forever: past
--   @budget@ the retry loop isn't "working as designed" any more, it's
--   masking a model that can't reliably format the call, and that should
--   fail the same way giving up immediately would ('budget' @0@ -- see
--   @Agent.Integration.OutlineSplitQualitySpec@, which wants exactly that,
--   a call that isn't right on the first try). Logs each turn the same way
--   'recordToolCalls' does, so the log line for a bad turn is visible even
--   though 'fail' may abort the run right after.
--
--   Only unparseable JSON counts as malformed -- a call that parsed fine is
--   never flagged on its *content*, however unusual (a file path, a
--   made-up UNC share, anything). An earlier version of this also flagged
--   'Agent.Integration.ToolCallQuality.escapingArtifacts' hits as
--   over-escaping, but that check can't coexist with fixtures that
--   deliberately invite the model to invent plausible technical color (see
--   @../PLAN.md@): you cannot both dare a model to write something that
--   might contain a path and then fail it for writing one. A dedicated,
--   *controlled* round-trip check (hand the model an exact literal string
--   in the outline, assert that exact string comes back unchanged) would be
--   the honest way to test escaping specifically, if that's ever wanted --
--   not a heuristic scan over freely-generated prose.
--
--   No separate counter needed to track "how many retries so far": each
--   'Runix.LLM.QueryLLM' call already carries the full growing history as
--   its argument, so 'Agent.Integration.ToolCallQuality.invalidCallsSinceLastUser'
--   reads the retry count straight off it.
assertToolCallBudget
  :: forall model r a
  .  Members '[LLM model, Logging, Fail] r
  => Int  -- ^ malformed calls tolerated before this fails the run
  -> Sem r a -> Sem r (a, [[Message model]])
assertToolCallBudget budget action = do
  (turns, result) <- runOutputList $ intercept @(LLM model) recorder (raise action)
  return (result, turns)
  where
    recorder :: forall m x. LLM model m x -> Sem (Output [Message model] : r) x
    recorder (QueryLLM configs msgs) = do
      resp <- send (QueryLLM configs msgs)
      case resp of
        Right msgs' -> do
          mapM_ logToolCall msgs'
          checkTurn msgs msgs'
          output msgs'
          pure resp
        Left _       -> pure resp

    checkTurn :: [Message model] -> [Message model] -> Sem (Output [Message model] : r) ()
    checkTurn history msgs' = do
      let tr = reportTurn msgs'
          total = invalidCallsSinceLastUser history + length (trInvalidCalls tr)
      case (total > budget, trInvalidCalls tr) of
        (True, (name, err) : _) ->
          fail $ T.unpack ("malformed tool call to " <> name <> " (" <> T.pack (show total)
            <> " so far, budget " <> T.pack (show budget) <> "): " <> err)
        _ -> pure ()

-- | Run @setup@ with its 'Runix.Logging.Logging' output dropped, then
--   continue as normal -- for reusing an already-tested flow (e.g.
--   'Agent.Integration.Journey.runJourney', or just its outline step) as
--   cheap setup for a *different* scenario. Thanks to 'Runix.LLM.Cache.cacheLLM',
--   replaying a previously-recorded flow to reach some starting state is
--   near-instant -- no live LLM round trip -- but its own step-by-step
--   logging ("journey: generating outline.md", ...) is noise once it's not
--   the thing actually under test; only the scenario that's actually being
--   exercised should show up in a run's output. Built the same way
--   'assertToolCallBudget' taps 'LLM': 'intercept' keeps 'Logging' live in
--   the row (so anything *after* @setup@ still logs normally), it just
--   drops every call made *during* @setup@ instead of forwarding it to the
--   real interpreter underneath.
quietSetup :: forall r a. Member Logging r => Sem r a -> Sem r a
quietSetup = intercept @Logging $ \case
  Log _ _ _ -> pure ()

-- | Local, suite-specific replacement for 'Runix.Logging.loggingIO' --
--   indented and dimmed\/colored (every line of a multi-line message, not
--   just the first) so log output stays visually distinct from whatever
--   else shares the terminal. Kept in this module rather than changed
--   upstream in 'Runix.Logging': that interpreter is shared by every
--   'Runix.Logging.Logging' user across both projects (server, CLI, ...),
--   and a look this specific to "interspersed with hspec output in one
--   test binary" has no business being everyone's default.
loggingPretty :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingPretty = interpret $ \case
  Log level _cs m -> embed $ putStrLn $ colorize level $ indent $ prefix level <> T.unpack m
  where
    prefix Info    = "info: "
    prefix Warning = "warn: "
    prefix Error   = " err: "
    indent = intercalate "\n" . map ("  " <>) . lines
    colorize Info s    = "\ESC[90m" <> s <> "\ESC[0m"    -- dim gray
    colorize Warning s = "\ESC[1;33m" <> s <> "\ESC[0m"  -- bold yellow, more visible
    colorize Error s   = "\ESC[1;31m" <> s <> "\ESC[0m"  -- bold red

-- | Override one 'Storyteller.Core.Prompt.PromptStorage' key for the
--   duration of @action@, whatever the shared runner's own
--   'Storyteller.Core.Prompt.interpretPromptStorageMap' (built once in
--   @Main.hs@, normally empty) would otherwise have returned. For nudging
--   an agent toward test-appropriate output without touching its shipped
--   default -- e.g. @Agent.Integration.OutlineSplitQualitySpec@ asking for
--   concise beat sheets so a scenario stays fast, while
--   @Agent.Integration.OutlineSplitDefaultsSpec@ deliberately applies no
--   override at all, to check the shipped default itself is sufficient
--   (see @../PLAN.md@ on why those are two separate questions). Built the
--   same way 'quietSetup' taps 'Runix.Logging.Logging': 'intercept' keeps
--   'PromptStorage' live in the row, forwarding every other key through to
--   the real interpreter unchanged.
withPromptOverride :: forall r a. Member PromptStorage r => PromptKey -> Prompt -> Sem r a -> Sem r a
withPromptOverride key override = intercept @PromptStorage $ \case
  GetPrompt k def
    | k == key  -> pure override
    | otherwise -> send (GetPrompt k def)
  GetConfig k defaults -> send (GetConfig k defaults)
