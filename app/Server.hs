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
--   /character/{name} — character branch's sidebar-facing state (read-only)
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
import Network.HTTP.Types (urlDecode)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import Server.Writer.Env (ServerEnv, loadServerEnv, envPort)
import Server.Writer.Branch.Connection (runBranch)
import Server.Writer.File.Connection (runFile)
import Server.Writer.Character.Connection (runCharacter)
import Server.Writer.Session.Connection (runSession)

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
    ["session"]              -> accept $ runSession env
    ["branch", name]         -> accept $ runBranch  env (T.pack (BC.unpack (urlDecode False name)))
    ("branch" : name : path) -> accept $ runFile env
                                           (T.pack (BC.unpack (urlDecode False name)))
                                           (foldl1 (\a b -> a <> "/" <> b) (map (BC.unpack . urlDecode False) path))
    ["character", name]      -> accept $ runCharacter env (T.pack (BC.unpack (urlDecode False name)))
    _                        -> rejectRequest pending "not found"
  where
    accept handler = do
      conn <- acceptRequest pending
      withPingThread conn 30 (return ()) (handler conn)

httpApp :: Application
httpApp _req respond =
  respond $ responseLBS status200 [] "storyteller"
