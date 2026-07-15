{-# LANGUAGE OverloadedStrings #-}

-- | story-server entry point.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--   PORT                (optional, default 8090)
--   STATIC_DIR          (optional) a built frontend (frontend/`npm run
--                        build:static`) to serve alongside the API, for a
--                        single-process production deployment. Unset in
--                        dev — the frontend runs as its own `next dev`
--                        process/container instead (see frontend/README or
--                        WRITER.md), talking to this server cross-origin.
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
--   PUT /branch/{name}/$raw/{path} — raw-edit-mode save: like the plain PUT
--                                 above, but the body must be UTF-8 text and
--                                 is reconciled against the path's existing
--                                 atom chain (see
--                                 'Server.Writer.Branch.saveFile'/
--                                 'Storage.Ops.commitFile') instead of
--                                 landing as an opaque binary asset. With
--                                 "?asNew" instead: a wholesale replacement
--                                 (see 'Server.Writer.Branch.saveFileAsNew'/
--                                 'Storage.Ops.saveFileAsNew') rather than a
--                                 reconciled diff — no note/atom continuity
--                                 carried forward. "?newPath=..." alongside
--                                 it forks to a different file; absent, it
--                                 defaults to this same path.
--   /                           — the built frontend, if STATIC_DIR is set
--                                 (see 'staticApp'); otherwise a plain
--                                 placeholder response, unchanged from before
module Main (main) where

import Control.Monad (join)
import Network.Wai (Application, Request(pathInfo, requestMethod), responseLBS, responseFile, strictRequestBody)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setTimeout, setPort)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, PendingConnection(..), RequestHead(..), defaultConnectionOptions, acceptRequest, withPingThread, rejectRequest)
import Network.HTTP.Types (Header, status200, status400, status404, hContentType, methodGet, methodPut, methodOptions)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types (urlDecode)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Server.Core.File (readFileContent)
import Server.Writer.Branch (uploadFile, saveFile, saveFileAsNew)
import Server.Writer.Env (ServerEnv, loadServerEnv, envPort, envStaticDir)
import Server.Writer.Branch.Connection (runBranch)
import Server.Writer.File.Connection (runFile)
import Server.Writer.ContextView.Connection (runContextView)
import Server.Writer.Character.Connection (runCharacter)
import Server.Writer.Library.Connection (runLibrary)
import Server.Writer.Lore.Connection (runLore)
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
    ["lore", name]           -> accept $ runLore      env (T.pack (BC.unpack (urlDecode False name)))
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

      -- Reserved segment ahead of the generic upload PUT below, same
      -- convention as the WS router's "$context" (see wsRouter) — raw-edit
      -- save reconciles against the atom chain ('Server.Writer.Branch.saveFile')
      -- instead of depositing an opaque binary asset like a plain upload.
      --
      -- The "?asNew" query flag is the same resource, a different write
      -- strategy: a wholesale replacement ('Server.Writer.Branch.
      -- saveFileAsNew') instead of the default reconciled diff — the raw/
      -- markdown editor's own "this isn't an edit, it's a replacement"
      -- escape hatch. "?newPath=..." alongside it forks to a different
      -- file instead of replacing this one in place; absent, it defaults
      -- to this same path.
      (m, "branch" : name : "$raw" : path@(_:_)) | m == methodPut -> do
        let filePath = T.unpack (T.intercalate "/" path)
            asNew    = any ((== "asNew") . fst) (Wai.queryString req)
            newPath  = maybe filePath (T.unpack . TE.decodeUtf8Lenient)
                             (join (lookup "newPath" (Wai.queryString req)))
        body <- strictRequestBody req
        case TE.decodeUtf8' (LBS.toStrict body) of
          Left _        -> respond $ responseLBS status400 (corsHeaders req) "raw edit content must be valid UTF-8"
          Right content -> do
            result <- runAction env $
              if asNew
                then saveFileAsNew name filePath newPath content
                else saveFile name filePath content
            case result of
              Left err -> respond $ responseLBS status400 (corsHeaders req) (LBC.pack err)
              Right () -> respond $ responseLBS status200 (corsHeaders req) ""

      (m, "branch" : name : path@(_:_)) | m == methodPut -> do
        let filePath = T.unpack (T.intercalate "/" path)
        body   <- strictRequestBody req
        result <- runAction env (uploadFile name filePath (LBS.toStrict body))
        case result of
          Left err -> respond $ responseLBS status400 (corsHeaders req) (LBC.pack err)
          Right () -> respond $ responseLBS status200 (corsHeaders req) ""

      (m, path) | m == methodGet -> case envStaticDir env of
        Nothing  -> respond $ responseLBS status200 (corsHeaders req) "storyteller"
        Just dir -> staticApp dir path req respond

      _ -> respond $ responseLBS status200 (corsHeaders req) "storyteller"

corsHeaders :: Request -> [Header]
corsHeaders req =
  [ ("Access-Control-Allow-Origin",  maybe "*" id (lookup "Origin" (Wai.requestHeaders req)))
  , ("Access-Control-Allow-Methods", "GET, PUT, OPTIONS")
  , ("Access-Control-Allow-Headers", "Content-Type")
  ]

-- | Serve a built frontend (@STATIC_DIR@, see this module's ENV doc) out of
--   @dir@. Deliberately hand-rolled rather than pulling in a package like
--   @wai-app-static@ — the actual requirement is small (serve a file by
--   extension, SPA-fallback to @index.html@ for anything else) and this
--   reuses 'mimeType', already present here for the branch-file GET route.
--
--   Falls back to @dir\/index.html@ for any path that isn't a real file —
--   not just @\/@. That covers two cases identically: a genuine 404 (bad
--   path), and a deep link into the app's own client-side routing (e.g.
--   @\/master\/somefile.md@, which is never a real file — see
--   'page.tsx's @history.pushState@ — only ever a client-rendered state).
--   Serving @index.html@ either way is exactly what @next.config.ts@'s dev-
--   only @rewrites()@ already does for the same reason; this is that
--   behavior's production equivalent once the static export drops it.
staticApp :: FilePath -> [T.Text] -> Application
staticApp dir path req respond = do
  -- '/' rejection: a bare ".." segment can't smuggle a traversal past this
  -- join, since every real segment is checked before any filesystem access.
  if any (== "..") path
    then respond $ responseLBS status404 (corsHeaders req) "not found"
    else do
      -- Empty 'path' (a bare "/") joins to just 'dir' itself, a directory —
      -- 'doesFileExist' is False for it, same as any other non-file path,
      -- so it falls through to the index.html branch below with no special
      -- case needed here.
      let filePath = foldl (</>) dir (map T.unpack path)
      exists <- doesFileExist filePath
      if exists
        then respond $ responseFile status200
               ((hContentType, mimeType filePath) : cacheHeaders path ++ corsHeaders req)
               filePath Nothing
        else respond $ responseFile status200
               ((hContentType, "text/html; charset=utf-8") : corsHeaders req)
               (dir </> "index.html") Nothing

-- | Next's static export content-hashes everything under @_next/static/@,
--   so it's safe (and worth it) to cache those aggressively; anything else
--   — the HTML shell in particular — must always be revalidated, since it's
--   what changes across a redeploy.
cacheHeaders :: [T.Text] -> [Header]
cacheHeaders ("_next" : "static" : _) = [("Cache-Control", "public, max-age=31536000, immutable")]
cacheHeaders _                        = [("Cache-Control", "no-cache")]

-- | Best-effort content type from extension — good enough for the
--   image/document/frontend-asset types this server ever serves; anything
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
  ".html" -> "text/html; charset=utf-8"
  ".js"   -> "text/javascript; charset=utf-8"
  ".css"  -> "text/css; charset=utf-8"
  ".map"  -> "application/json"
  ".woff2" -> "font/woff2"
  ".ico"  -> "image/x-icon"
  _       -> "application/octet-stream"
