{-# LANGUAGE OverloadedStrings #-}

-- | Common ENV-variable configuration for all CLI tools.
--
-- All tools read from the same set of ENV vars so they share context
-- without requiring repeated arguments. Individual tools may require
-- additional vars that are documented per-tool.
module Storyteller.CLI.Env
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

import UniversalLLM (ModelConfig(..))
import Storyteller.Runtime (StoryModel)

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

modelConfigs :: [ModelConfig StoryModel]
modelConfigs =
  [ SystemPrompt "You are a creative writing assistant. Write only what is asked. Output only prose, nothing else."
  , MaxTokens 2048
  , Temperature 0.8
  ]
