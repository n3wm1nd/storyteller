{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared runtime: model, interpreter, and IO effect stack for all executables.
module Storyteller.Runtime
  ( -- * Model
    StoryModel
  , storyModel

    -- * Filesystem tag
  , StoryFS(..)

    -- * IO runner
  , runStoryIO
  ) where

import Polysemy
import Polysemy.Fail
import Polysemy.Error (runError)
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , HasProjectPath(..), fileSystemLocal )
import Runix.FileSystem.System (filesystemReadIO, filesystemWriteIO)
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMWith, LlamaCppAuth(..))
import Runix.RestAPI (RestEndpoint(..))
import Runix.Runner (httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.Time (timeIO, sleepIO)
import Runix.Logging (Logging)

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
-- Filesystem tag — roots all file access to the story directory
-- ---------------------------------------------------------------------------

newtype StoryFS = StoryFS FilePath

instance HasProjectPath StoryFS where
  getProjectPath (StoryFS path) = path

-- ---------------------------------------------------------------------------
-- Auth
-- ---------------------------------------------------------------------------

newtype StoryLlamaCppAuth = StoryLlamaCppAuth LlamaCppAuth

instance RestEndpoint StoryLlamaCppAuth where
  apiroot    (StoryLlamaCppAuth a) = apiroot a
  authheaders _                    = []
  useragent  _                     = "storyteller/0.1"

-- ---------------------------------------------------------------------------
-- IO runner
-- ---------------------------------------------------------------------------

-- | Run a story action down to IO.
--   Provides: LLM StoryModel, FileSystem StoryFS, Logging, Fail.
--   All file access is chrooted to @rootDir@.
runStoryIO
  :: String                    -- ^ llama-cpp base URL, e.g. "http://localhost:8080/v1"
  -> FilePath                  -- ^ root directory for all file access
  -> [ModelConfig StoryModel]
  -> ( forall r. Members '[LLM StoryModel, FileSystem StoryFS, FileSystemRead StoryFS, FileSystemWrite StoryFS, Logging, Fail] r
       => Sem r a )
  -> IO (Either String a)
runStoryIO endpoint rootDir configs action =
  runM
  . runError
  . loggingIO
  . failLog
  . filesystemReadIO
  . filesystemWriteIO
  . fileSystemLocal (StoryFS rootDir)
  . timeIO
  . sleepIO
  . httpIO (withRequestTimeout 600)
  . interpretLLMWith (StoryLlamaCppAuth (LlamaCppAuth endpoint)) (route @StoryModel) storyModel configs
  $ action
