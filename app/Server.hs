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
--   /library/{name}   — writer-facing book/chapter/scene tree for a branch
--
-- HTTP endpoints (alongside):
--   GET /branch/{name}/{path}   — a branch file's current raw content, for
--                                 direct download or embedding (e.g. <img>)
--                                 instead of tunneling bytes through the WS
--                                 connection just to simulate one
--   PUT /branch/{name}/{path}   — upload/replace a file's content from the
--                                 request body directly (see
--                                 'Server.Writer.Branch.uploadFile') — the
--                                 only way to upload now; there is no WS
--                                 command for this anymore
--   /                           — static assets (placeholder)
module Main (main) where

import Network.Wai (Application, Request(pathInfo, requestMethod), responseLBS, strictRequestBody)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setTimeout, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, PendingConnection(..), RequestHead(..), defaultConnectionOptions, acceptRequest, withPingThread, rejectRequest)
import Network.HTTP.Types (Header, status200, status400, status404, hContentType, methodGet, methodPut, methodOptions)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types (urlDecode)
import System.FilePath (takeExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import Server.Core.File (readFileContent)
import Server.Writer.Branch (uploadFile)
import Server.Writer.Env (ServerEnv, loadServerEnv, envPort)
import Server.Writer.Branch.Connection (runBranch)
import Server.Writer.File.Connection (runFile)
import Server.Writer.ContextView.Connection (runContextView)
import Server.Writer.Character.Connection (runCharacter)
import Server.Writer.Library.Connection (runLibrary)
import Server.Writer.Run (runAction)
import Server.Writer.Session.Connection (runSession)

main :: IO ()
main = do
  env <- loadServerEnv
  let port = envPort env
  hPutStrLn stderr $ "storyteller-server listening on port " <> show port
  let settings = setPort port . setTimeout 0 $ defaultSettings
  runSettings settings (websocketsOr defaultConnectionOptions (wsRouter env) (httpApp env))

wsRouter :: ServerEnv -> ServerApp
wsRouter env pending =
  case BC.split '/' . BC.dropWhile (== '/') . Network.WebSockets.requestPath $ pendingRequest pending of
    ["session"]              -> accept $ runSession env
    ["branch", name]         -> accept $ runBranch  env (T.pack (BC.unpack (urlDecode False name)))
    -- Reserved segment ahead of the generic file-path catch-all below —
    -- "$context" can't collide with a real file path, and this can move
    -- wholesale once /branch's routing is reworked. See
    -- Server.Writer.ContextView.Connection.
    ("branch" : name : "$context" : path) -> accept $ runContextView env
                                           (T.pack (BC.unpack (urlDecode False name)))
                                           (joinPath path)
    ("branch" : name : path) -> accept $ runFile env
                                           (T.pack (BC.unpack (urlDecode False name)))
                                           (joinPath path)
    ["character", name]      -> accept $ runCharacter env (T.pack (BC.unpack (urlDecode False name)))
    ["library", name]        -> accept $ runLibrary   env (T.pack (BC.unpack (urlDecode False name)))
    _                        -> rejectRequest pending "not found"
  where
    accept handler = do
      conn <- acceptRequest pending
      withPingThread conn 30 (return ()) (handler conn)

    joinPath []     = ""
    joinPath (p:ps) = foldl (\a b -> a <> "/" <> b) (BC.unpack (urlDecode False p)) (map (BC.unpack . urlDecode False) ps)

-- | The frontend (a separate origin/port — see WRITER.md/frontend's
--   NEXT_PUBLIC_WS_URL) calls GET/PUT here with @fetch@, unlike the
--   same-origin-exempt WS connections, so every response needs CORS headers
--   and a PUT needs its preflight OPTIONS answered. Reflects the caller's
--   own Origin rather than a fixed one — there's no cookie/credential-based
--   auth here for a reflected origin to weaken, and the server doesn't know
--   the frontend's deployed origin(s) ahead of time.
httpApp :: ServerEnv -> Application
httpApp env req respond
  | requestMethod req == methodOptions =
      respond $ responseLBS status200 (corsHeaders req) ""
  | otherwise = case (requestMethod req, pathInfo req) of
      (m, "branch" : name : path@(_:_)) | m == methodGet -> do
        let filePath = T.unpack (T.intercalate "/" path)
        result <- runAction env (readFileContent name filePath)
        case result of
          Left _    -> respond $ responseLBS status404 (corsHeaders req) "not found"
          Right raw -> respond $ responseLBS status200
            ((hContentType, mimeType filePath) : corsHeaders req)
            (LBS.fromStrict raw)

      (m, "branch" : name : path@(_:_)) | m == methodPut -> do
        let filePath = T.unpack (T.intercalate "/" path)
        body   <- strictRequestBody req
        result <- runAction env (uploadFile name filePath (LBS.toStrict body))
        case result of
          Left err -> respond $ responseLBS status400 (corsHeaders req) (LBC.pack err)
          Right () -> respond $ responseLBS status200 (corsHeaders req) ""

      _ -> respond $ responseLBS status200 (corsHeaders req) "storyteller"

corsHeaders :: Request -> [Header]
corsHeaders req =
  [ ("Access-Control-Allow-Origin",  maybe "*" id (lookup "Origin" (Wai.requestHeaders req)))
  , ("Access-Control-Allow-Methods", "GET, PUT, OPTIONS")
  , ("Access-Control-Allow-Headers", "Content-Type")
  ]

-- | Best-effort content type from extension — good enough for the
--   image/document types this endpoint exists to serve; anything
--   unrecognized falls back to a generic binary type rather than guessing.
mimeType :: FilePath -> BC.ByteString
mimeType path = case takeExtension (takeFileName path) of
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".gif"  -> "image/gif"
  ".webp" -> "image/webp"
  ".svg"  -> "image/svg+xml"
  ".pdf"  -> "application/pdf"
  ".txt"  -> "text/plain; charset=utf-8"
  ".json" -> "application/json"
  ".md"   -> "text/markdown; charset=utf-8"
  _       -> "application/octet-stream"
