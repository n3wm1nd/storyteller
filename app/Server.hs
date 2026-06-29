{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | story-server: HTTP REST API for storyteller.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--   PORT                (optional, default 8090)
module Main (main) where

import Network.Wai.Handler.Warp (run)
import Servant (serve, hoistServer)
import System.IO (hPutStrLn, stderr)

import Server.API (api)
import Server.Env (loadServerEnv, envPort)
import Server.Handlers (server)

main :: IO ()
main = do
  env <- loadServerEnv
  let port = envPort env
  hPutStrLn stderr $ "storyteller-server listening on port " <> show port
  run port $ serve api (server env)
