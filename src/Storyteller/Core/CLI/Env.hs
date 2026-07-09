{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common ENV-variable configuration for all CLI tools.
--
-- All tools read from the same set of ENV vars so they share context
-- without requiring repeated arguments. Individual tools may require
-- additional vars that are documented per-tool.
module Storyteller.Core.CLI.Env
  ( StoryEnv(..)
  , loadEnv
  , requireEnv

    -- * Re-export for tool entry points
  , modelConfigs
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import UniversalLLM (ModelConfig(..), ProviderOf, SupportsMaxTokens, SupportsTemperature)

data StoryEnv = StoryEnv
  { envRepo        :: FilePath  -- ^ STORY_REPO: path to git repository
  , envBranch      :: Text      -- ^ STORY_BRANCH: story branch name
  , envEndpoint    :: String    -- ^ LLAMACPP_ENDPOINT (default: http://localhost:8080/v1)
  , envActiveChars :: [Text]    -- ^ ACTIVE_CHARS: comma-separated character branch names
  }

-- | Load common ENV vars. Exits with a clear message if required vars are missing.
loadEnv :: IO StoryEnv
loadEnv = do
  repo     <- requireEnv "STORY_REPO"
  branch   <- requireEnv "STORY_BRANCH"
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  chars    <- maybe [] (filter (not . T.null) . map T.strip . T.splitOn "," . T.pack)
              <$> lookupEnv "ACTIVE_CHARS"
  return StoryEnv
    { envRepo        = repo
    , envBranch      = T.pack branch
    , envEndpoint    = endpoint
    , envActiveChars = chars
    }

requireEnv :: String -> IO String
requireEnv var = do
  mv <- lookupEnv var
  case mv of
    Just v  -> return v
    Nothing -> do
      hPutStrLn stderr $ "Error: " <> var <> " is not set"
      exitFailure

-- | Base config for 'Storyteller.Core.Runtime.runStoryGit'\'s one real,
--   physically-resolved @StoryModel@ (the concrete model every proxy role
--   delegates to in the CLI, unlike the server's independently-chosen
--   per-role models) -- an interpreter-level setting, not a per-agent one.
--   Each agent's own sampling default (what actually reaches the model on a
--   given call) is owned by that agent, right alongside its system prompt --
--   see e.g. 'Storyteller.Writer.Agent.Continuation.defaultWriterConfig' --
--   and layered on top of whatever this sets up, via
--   'Storyteller.Core.Prompt.getConfig'. Polymorphic in @model@ (rather than
--   pinned to one type) so the same base config works for every role's proxy
--   model -- see 'Storyteller.Core.LLM.Role'.
modelConfigs :: (SupportsMaxTokens (ProviderOf model), SupportsTemperature (ProviderOf model)) => [ModelConfig model]
modelConfigs =
  [ MaxTokens 2048
  , Temperature 0.8
  ]
