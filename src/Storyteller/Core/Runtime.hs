{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared runtime: model, interpreters, and IO effect stacks.
module Storyteller.Core.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Branch phantoms
  , Main
  , Prompts
  , Contexts

    -- * Runners
  , runInfrastructure
  , runInfrastructureWith
  , runInfrastructureWithCancellation
  , runStoryGit

    -- * Re-exported for custom stacks
  , module Storyteller.Core.Git
  , Git
  ) where

import Control.Concurrent.STM (TVar)
import Control.Monad (void)
import Polysemy
import Polysemy.Fail
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, runResource)
import Runix.Cancellation (Cancellation, runCancellationSTM)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.Random (Random, randomIO)
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.HTTP (HTTP, HTTPStreaming, httpIOStreaming, httpIOStreamingWithCancellation)
import Runix.Time (Time, Sleep, timeIO, sleepIO)
import Runix.Logging (Logging)

import Runix.Git (Git, runGitFFIPerCall, withGitCache)
import Storyteller.Core.LLM.Role (ProseModel, AgentModel, reinterpretProse, reinterpretAgent)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Undo (Undo, withUndoLog)

import UniversalLLM (Model(..), ModelConfig, Routing(..))
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_40B(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

type StoryModel = Model Qwen35_40B LlamaCpp

storyModel :: StoryModel
storyModel = Model Qwen35_40B LlamaCpp

-- ---------------------------------------------------------------------------
-- Phantom tag
-- ---------------------------------------------------------------------------

data Main

-- | Phantom for the dedicated, project-scoped branch backing
--   'Storyteller.Core.Prompt.PromptStorage'. Not a content branch — it holds
--   only prompt/template overrides, keyed by path, independent of any story
--   or character branch.
data Prompts

-- | Phantom for the dedicated, project-scoped branch backing
--   'Storyteller.Core.Context.ContextStorage' -- the same "override, keyed
--   by path, independent of any story/character branch" shape as 'Prompts',
--   just holding Context DSL definition source instead of prompt text.
data Contexts

-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

newtype StoryLlamaCppAuth = StoryLlamaCppAuth LlamaCppAuth

instance RestEndpoint StoryLlamaCppAuth where
  apiroot    (StoryLlamaCppAuth a) = apiroot a
  authheaders _                    = []
  useragent  _                     = "storyteller/0.1"

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | The 'Git'-independent middle of the infrastructure stack: random,
--   http, sleep/time. Factored out so a caller can supply its own 'Git'
--   interpreter underneath (a fresh per-call reader for CLI executables,
--   or the shared git-storage worker for the server -- see
--   PLAN-git-storage-worker.md) without duplicating this part.
--
--   The supplied interpreter still sees 'Fail' in its row (via the
--   'Members' constraint) rather than consuming it -- whatever eliminates
--   'Fail' (e.g. 'failLog') sits further out, wrapped around this whole
--   call, same as it always has.
--
--   Wraps @action@ in 'withUndoLog' before anything else runs, so every
--   story-branch ref write made anywhere inside it -- regardless of
--   whether the caller is 'runStoryGit', the server, or a CLI executable
--   -- feeds the undo tree. A single install point here beats repeating it
--   at each of those call sites; 'Storyteller.Core.Undo' itself stays
--   completely unaware of the story ref convention ('isStoryRef'/
--   'storyRefPrefix' are supplied here, from 'Storyteller.Core.Git').
--
--   @action@ carries 'Undo' in its own row (rather than being isolated from
--   it) so a caller that wants the control API itself -- e.g. a server
--   session handler exposing undo/reset over a WS connection -- can just
--   use 'Storyteller.Core.Undo.listUndo'/'resetToUndo' directly, alongside
--   getting every other write auto-snapshotted. A caller with no interest
--   in it (every CLI executable today) can 'Polysemy.raise' a plain action
--   into this row for free -- 'Undo' sits at the very front for exactly
--   that reason: inserting a new effect under a plain, unqualified 'raise'
--   only ever adds it at the head, not at some arbitrary position deeper in
--   the row.
runInfrastructureWith
  :: Members '[Fail, Embed IO] r
  => (Sem (Git : r) a -> Sem r a)
  -> Sem (Undo : Random : HTTP : HTTPStreaming : Sleep : Time : Git : r) a
  -> Sem r a
runInfrastructureWith runGit action =
    runGit
  . timeIO
  . sleepIO
  . httpIOStreaming (withRequestTimeout 600)
  . httpIO (withRequestTimeout 600)
  . randomIO
  $ withUndoLog storyRefPrefix isStoryRef action

-- | Same as 'runInfrastructureWith', but backs 'HTTPStreaming' with
--   'httpIOStreamingWithCancellation' instead of 'httpIOStreaming' —
--   between-chunk fetches stop early once 'cancelFlag' is set, the same
--   way a naturally-ended stream would (see that function's Haddock).
--
--   'Runix.Cancellation.Cancellation' is introduced and eliminated
--   entirely within this one composition (@raiseUnder \@Cancellation@
--   inserts it right under 'HTTPStreaming' for
--   'httpIOStreamingWithCancellation' to use; 'runCancellationSTM'
--   consumes it immediately after) — it never appears in @action@'s row,
--   so nothing above this call (agents, handlers, 'SessionEffects' itself)
--   ever needs to know cancellation exists. That's what makes this safe
--   to swap in for one caller ('Server.Writer.Run.actionStack', the
--   interactive server) while every other 'runInfrastructureWith' caller
--   (CLI executables, tests) stays untouched.
runInfrastructureWithCancellation
  :: Members '[Fail, Embed IO] r
  => TVar Bool
  -> (Sem (Git : r) a -> Sem r a)
  -> Sem (Undo : Random : HTTP : HTTPStreaming : Sleep : Time : Git : r) a
  -> Sem r a
runInfrastructureWithCancellation cancelFlag runGit action =
    runGit
  . timeIO
  . sleepIO
  . runCancellationSTM cancelFlag
  . httpIOStreamingWithCancellation (withRequestTimeout 600)
  . raiseUnder @Cancellation
  . httpIO (withRequestTimeout 600)
  . randomIO
  $ withUndoLog storyRefPrefix isStoryRef action

-- | Shared infrastructure interpreters: git, http, time, logging, error.
--   Every CLI executable uses this as its base; branch/storage/LLM go on
--   top. The server uses 'runInfrastructureWith' directly instead, with
--   'Server.Writer.GitWorker.runGitViaWorker' in place of the
--   'runGitFFIPerCall' below -- see PLAN-git-storage-worker.md.
--
--   'runGitFFIPerCall' opens the repo via libgit2 (no subprocess, no
--   @git@ binary dependency at all -- see 'Runix.Git.FFI') and closes it
--   when this call finishes ('Resource'/'bracket', see
--   'Runix.Git.runGitFFIPerCall') -- scoped to one 'runInfrastructure'
--   invocation, not shared across calls; fine for a short-lived CLI
--   process, which is all that still uses this function.
--   'runResource' interprets that here so nothing above this layer
--   (agents, handlers, executables) needs to know 'Resource' exists.
--   'runGitFFIPerCall' converts every failure into 'Fail' rather than a
--   raw IO exception (see its module), which is what lets 'runResource's
--   purely-'Sem'-level bracket -- it has no IO awareness at all -- still
--   guarantee the repo handle gets closed on that path.
runInfrastructure
  :: Members '[Error String, Embed IO] r
  => FilePath
  -> String
  -> Sem (Random : HTTP : HTTPStreaming : Sleep : Time : Git : Resource : Fail : Logging : r) a
  -> Sem r a
runInfrastructure repoPath _endpoint =
    loggingIO
  . failLog
  . runResource
  . runInfrastructureWith (runGitFFIPerCall repoPath . withGitCache)
  -- No CLI executable needs 'Undo' in its own row -- 'raise' adds it at the
  -- head for free, matching where 'runInfrastructureWith' now expects it.
  . raise

-- | One branch, storage, LLM. Creates the branch if it doesn't exist.
--
--   The CLI has no per-role configuration -- every role resolves to the
--   same 'storyModel'. Agents now unconditionally require
--   'Storyteller.Core.LLM.Role.LLMs' (both roles live in the row at once),
--   so both proxy effects get routed to that one real model via
--   'reinterpretProse'\/'reinterpretAgent', through two independent
--   interpreter instances (same mechanism the server uses per-role, just
--   both pointed at one target instead of two independently-chosen ones).
runStoryGit
  :: FilePath
  -> String
  -> BranchName
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[ LLM ProseModel
                           , LLM AgentModel
                           , FileSystem      (BranchTag Main)
                           , FileSystemRead  (BranchTag Main)
                           , FileSystemWrite (BranchTag Main)
                           , BranchOp Main
                           , StoryStorage
                           , Git
                           , Logging, Fail ] r
       => Sem r a )
  -> IO (Either String a)
runStoryGit repoPath endpoint branch configs action =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runStoryStorageGit
  . runBranchAndFS @Main branch
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  . reinterpretAgent @StoryModel
  . raiseUnder
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  . reinterpretProse @StoryModel
  . raiseUnder
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action
