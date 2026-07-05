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

    -- * Runners
  , runInfrastructure
  , runInfrastructureWith
  , runStoryGit

    -- * Re-exported for custom stacks
  , module Storyteller.Core.Git
  , runStoryStorageGit
  , Git
  ) where

import Control.Monad (void)
import Polysemy
import Polysemy.Fail
import Polysemy.Error (Error, runError)
import Polysemy.Resource (Resource, runResource)
import Runix.Cmd (Cmds, Cmd, cmdsIO, interpretCmd)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.Random (Random, randomIO)
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.HTTP (HTTP, HTTPStreaming, httpIOStreaming)
import Runix.Time (Time, Sleep, timeIO, sleepIO)
import Runix.Logging (Logging)

import Runix.Git (Git, runGitIOPerCall, withGitCache)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Core.Git
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch, getBranch)
import Storyteller.Core.Undo (withUndoLog)

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
runInfrastructureWith
  :: Members '[Fail, Embed IO] r
  => (Sem (Git : r) a -> Sem r a)
  -> Sem (Random : HTTP : HTTPStreaming : Sleep : Time : Git : r) a
  -> Sem r a
runInfrastructureWith runGit action =
    runGit
  . timeIO
  . sleepIO
  . httpIOStreaming (withRequestTimeout 600)
  . httpIO (withRequestTimeout 600)
  . randomIO
  $ withUndoLog storyRefPrefix isStoryRef action

-- | Shared infrastructure interpreters: git, http, time, logging, error.
--   Every CLI executable uses this as its base; branch/storage/LLM go on
--   top. The server uses 'runInfrastructureWith' directly instead, with
--   'Server.Writer.GitWorker.runGitViaWorker' in place of the
--   'runGitIOPerCall' below -- see PLAN-git-storage-worker.md.
--
--   'runGitIOPerCall' opens a persistent @git cat-file --batch@ process
--   for reads and closes it when this call finishes ('Resource'/'bracket',
--   see 'Runix.Git.runGitIOPerCall') -- scoped to one 'runInfrastructure'
--   invocation, not shared across calls; fine for a short-lived CLI
--   process, which is all that still uses this function.
--   'runResource' interprets that here so nothing above this layer
--   (agents, handlers, executables) needs to know 'Resource' exists.
--   'runGitIOPerCall' converts every failure from the reader into 'Fail'
--   rather than a raw IO exception (see its module), which is what lets
--   'runResource's purely-'Sem'-level bracket -- it has no IO awareness at
--   all -- still guarantee the reader gets closed on that path.
runInfrastructure
  :: Members '[Error String, Embed IO] r
  => FilePath
  -> String
  -> Sem (Random : HTTP : HTTPStreaming : Sleep : Time : Git : Cmd "git" : Cmds : Resource : Fail : Logging : r) a
  -> Sem r a
runInfrastructure repoPath _endpoint =
    loggingIO
  . failLog
  . runResource
  . cmdsIO
  . interpretCmd @"git"
  . runInfrastructureWith (runGitIOPerCall repoPath . withGitCache)

-- | One branch, storage, LLM. Creates the branch if it doesn't exist.
runStoryGit
  :: FilePath
  -> String
  -> BranchName
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[ LLM StoryModel
                           , FileSystem      (BranchTag Main)
                           , FileSystemRead  (BranchTag Main)
                           , FileSystemWrite (BranchTag Main)
                           , StoryBranch Main
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
  $ do
      getBranch branch >>= \case
        Nothing -> void $ createBranch branch
        Just _  -> return ()
      action
