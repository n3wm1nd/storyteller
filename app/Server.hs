{-# LANGUAGE OverloadedStrings #-}

-- | story-server entry point.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--   PORT                (optional, default 8090)
--
-- WebSocket endpoints:
--   /session          — storage-level session (branch management)
--   /branch/{name}    — branch session (file operations, agents)
--
-- HTTP endpoints (alongside):
--   /data/{hash}/{filename}   — binary blob by content hash
--   /                         — static assets (placeholder)
module Main (main) where

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setTimeout, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, PendingConnection(..), RequestHead(..), defaultConnectionOptions, acceptRequest, withPingThread, rejectRequest)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Char8 as BC
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import Server.Env (ServerEnv, loadServerEnv, envPort)
import Server.Branch.Connection (runBranch)
import Server.Session.Connection (runSession)

main :: IO ()
main = do
  env <- loadServerEnv
  let port = envPort env
  hPutStrLn stderr $ "storyteller-server listening on port " <> show port
  let settings = setPort port . setTimeout 0 $ defaultSettings
  runSettings settings (websocketsOr defaultConnectionOptions (wsRouter env) httpApp)

wsRouter :: ServerEnv -> ServerApp
wsRouter env pending =
  case BC.split '/' . BC.dropWhile (== '/') . Network.WebSockets.requestPath $ pendingRequest pending of
    ["session"]          -> accept $ runSession env
    ["branch", name]     -> accept $ runBranch  env (T.pack (BC.unpack name))
    _                    -> rejectRequest pending "not found"
  where
    accept handler = do
      conn <- acceptRequest pending
      withPingThread conn 30 (return ()) (handler conn)

httpApp :: Application
httpApp _req respond =
  respond $ responseLBS status200 [] "storyteller"
