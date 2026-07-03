{-# LANGUAGE OverloadedStrings #-}

-- | Server environment: static config + shared STM state.
--
-- 'ServerEnv' is built once at startup and threaded through every request
-- via a Polysemy 'Reader' effect. Handlers spin up a fresh interpreter stack
-- per request; anything that must survive across requests lives in 'appState'.
module Server.Writer.Env
  ( ServerEnv(..)
  , AppState(..)
  , emptyAppState
  , loadServerEnv
  ) where

import Control.Concurrent.STM (TVar, TChan, newTVarIO, newBroadcastTChanIO)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Server.Writer.Notification (BranchNotification)

-- | Mutable shared state across requests. Starts empty; extended as needed.
data AppState = AppState
  -- placeholder — nothing needs cross-request state yet
  deriving (Show)

emptyAppState :: AppState
emptyAppState = AppState

data ServerEnv = ServerEnv
  { envRepoPath    :: FilePath                    -- ^ STORY_REPO
  , envLLMEndpoint :: String                      -- ^ LLAMACPP_ENDPOINT
  , envPort        :: Int                         -- ^ PORT (default 8090)
  , appState       :: TVar AppState
  , envNotifyChan  :: TChan BranchNotification    -- ^ broadcast channel; connections dupTChan to subscribe
  }

loadServerEnv :: IO ServerEnv
loadServerEnv = do
  repo     <- requireEnv "STORY_REPO"
  endpoint <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  port     <- maybe 8090 read <$> lookupEnv "PORT"
  state    <- newTVarIO emptyAppState
  notify   <- newBroadcastTChanIO
  return ServerEnv
    { envRepoPath    = repo
    , envLLMEndpoint = endpoint
    , envPort        = port
    , appState       = state
    , envNotifyChan  = notify
    }

requireEnv :: String -> IO String
requireEnv var = do
  mv <- lookupEnv var
  case mv of
    Just v  -> return v
    Nothing -> hPutStrLn stderr ("Error: " <> var <> " is not set") >> exitFailure
