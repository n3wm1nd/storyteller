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
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Server.Writer.GitWorker (GitWorkerQueue, startGitWorker)
import Server.Writer.Notification (BranchNotification)
import Storyteller.Core.LLM.Registry (SomeLLMRunner, resolveKnownModel, resolveRoleRunner)

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
  , envStaticDir   :: Maybe FilePath              -- ^ STATIC_DIR (optional; see app/Server.hs's httpApp)
  , appState       :: TVar AppState
  , envNotifyChan  :: TChan BranchNotification    -- ^ broadcast channel; connections dupTChan to subscribe
  , envGitWorker   :: GitWorkerQueue              -- ^ the process's one git-storage worker; see PLAN-git-storage-worker.md
  , envProseRunner :: SomeLLMRunner                -- ^ ROLE_PROSE_MODEL, resolved once at startup — see Storyteller.Core.LLM.Role
  , envAgentRunner :: SomeLLMRunner                -- ^ ROLE_AGENT_MODEL, resolved once at startup
  }

loadServerEnv :: IO ServerEnv
loadServerEnv = do
  repo      <- requireEnv "STORY_REPO"
  endpoint  <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  port      <- maybe 8090 read <$> lookupEnv "PORT"
  staticDir <- lookupEnv "STATIC_DIR"
  state     <- newTVarIO emptyAppState
  notify    <- newBroadcastTChanIO
  worker    <- startGitWorker repo notify
  mLogDir   <- llmLogDir repo
  proseKnown  <- resolveKnownModel "ROLE_PROSE_MODEL" "qwen35-40b"
  agentKnown  <- resolveKnownModel "ROLE_AGENT_MODEL" "qwen35-40b"
  proseRunner <- resolveRoleRunner mLogDir proseKnown
  agentRunner <- resolveRoleRunner mLogDir agentKnown
  return ServerEnv
    { envRepoPath    = repo
    , envLLMEndpoint = endpoint
    , envPort        = port
    , envStaticDir   = staticDir
    , appState       = state
    , envNotifyChan  = notify
    , envGitWorker   = worker
    , envProseRunner = proseRunner
    , envAgentRunner = agentRunner
    }

requireEnv :: String -> IO String
requireEnv var = do
  mv <- lookupEnv var
  case mv of
    Just v  -> return v
    Nothing -> hPutStrLn stderr ("Error: " <> var <> " is not set") >> exitFailure

-- | Where to dump raw LLM request/response JSON when 'LLM_LOG_REQUESTS' is
--   set (any non-empty value) -- @<repo>\/.git\/runix-request-logs@, so
--   logs from different checkouts never mix and nothing here is ever
--   committed (@.git@ itself is never tracked). Created eagerly, once, so
--   the first request doesn't race the directory's own creation.
llmLogDir :: FilePath -> IO (Maybe FilePath)
llmLogDir repo = do
  enabled <- maybe False (not . null) <$> lookupEnv "LLM_LOG_REQUESTS"
  if not enabled then return Nothing else do
    let dir = repo </> ".git" </> "runix-request-logs"
    createDirectoryIfMissing True dir
    return (Just dir)
