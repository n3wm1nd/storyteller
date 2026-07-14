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
  , registerCancel
  , unregisterCancel
  , requestCancel
  ) where

import Control.Concurrent.STM (TVar, TChan, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar, newBroadcastTChanIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Server.Writer.GitWorker (GitWorkerQueue, startGitWorker)
import Server.Writer.Notification (BranchNotification)
import Storyteller.Core.LLM.Registry (SomeLLMRunner, resolveKnownModel, resolveRoleRunner)

-- | Mutable shared state across requests.
data AppState = AppState
  { cancelFlags :: Map.Map T.Text (TVar Bool)
    -- ^ One entry per in-flight, cancelable command, keyed by its own
    -- wire id (the 'fcId'\/'bcId' the client sent it with) — see
    -- 'registerCancel'\/'unregisterCancel'\/'requestCancel'. Lets the
    -- always-listening \/session connection reach a cancel flag owned by
    -- some other connection's command loop.
  }

emptyAppState :: AppState
emptyAppState = AppState { cancelFlags = Map.empty }

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

-- | Publish a command's cancel flag under its own wire id, so a later
--   'requestCancel' for that id (arriving on some other connection, e.g.
--   \/session) can find it. Overwrites any existing entry for that id —
--   ids are only ever reused across genuinely sequential commands on the
--   same connection (see 'Server.Writer.File.Connection'\/'Branch.Connection'),
--   never concurrently.
registerCancel :: ServerEnv -> T.Text -> TVar Bool -> IO ()
registerCancel env cid flag =
  atomically $ modifyTVar' (appState env) (\s -> s { cancelFlags = Map.insert cid flag (cancelFlags s) })

-- | Drop a command's cancel-flag registration once it's finished (success
--   or error) — a stale id left behind would let a late cancel silently
--   flip a flag nobody's reading anymore, harmless but pointless.
unregisterCancel :: ServerEnv -> T.Text -> IO ()
unregisterCancel env cid =
  atomically $ modifyTVar' (appState env) (\s -> s { cancelFlags = Map.delete cid (cancelFlags s) })

-- | Set the cancel flag registered under 'cid', if any is still live.
--   Returns 'False' for an unknown id — already finished, or never
--   cancelable — which the caller treats as a harmless no-op, not an
--   error: the client can't generally know whether its cancel raced the
--   command's own completion.
requestCancel :: ServerEnv -> T.Text -> IO Bool
requestCancel env cid = do
  s <- readTVarIO (appState env)
  case Map.lookup cid (cancelFlags s) of
    Just flag -> atomically (writeTVar flag True) >> return True
    Nothing   -> return False

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
