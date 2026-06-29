{-# LANGUAGE OverloadedStrings #-}

-- | story-server: WebSocket server for storyteller.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--   PORT                (optional, default 8090)
--
-- Connections: each WebSocket connection is a branch session.
-- HTTP is served alongside for static assets and binary blob retrieval.
module Main (main) where

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, defaultConnectionOptions, acceptRequest, withPingThread)
import Network.HTTP.Types (status200)
import System.IO (hPutStrLn, stderr)

import Server.Env (ServerEnv, loadServerEnv, envPort)
import Server.Session (runSession)

main :: IO ()
main = do
  env <- loadServerEnv
  let port = envPort env
  hPutStrLn stderr $ "storyteller-server listening on port " <> show port
  run port $ websocketsOr defaultConnectionOptions (wsApp env) httpApp

wsApp :: ServerEnv -> ServerApp
wsApp env pending = do
  conn <- acceptRequest pending
  withPingThread conn 30 (return ()) $
    runSession env conn

-- Placeholder: will serve static files and /data/{hash}/{name} blobs
httpApp :: Application
httpApp _req respond =
  respond $ responseLBS status200 [] "storyteller"
