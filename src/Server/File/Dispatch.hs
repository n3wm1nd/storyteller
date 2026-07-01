{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
--
-- Each command handler returns a FileEvent. After any mutation the server
-- pushes the full updated file tick list as a FileUpdate. No read command —
-- reconnect is resync.
module Server.File.Dispatch
  ( dispatch
  , connectSnapshot
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (Sem, runM)
import Runix.Logging (info)

import Server.Env (ServerEnv)
import Server.File.Protocol (FileCommand(..), FileEvent(..))
import Server.Protocol (Update(..), toWireTick)
import Server.Run (runAction, actionStack, loggingWS, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Runtime (Main)
import qualified Storyteller.Storage as Storage
import Storyteller.Storage (FileTick, fileTicks, getBranch)
import Storyteller.Edit (deleteTick, editAtom, moveTick)
import Storyteller.Types (BranchName(..), TickId(..))

import Prelude hiding (readFile, writeFile)

-- ---------------------------------------------------------------------------
-- Connect snapshot
-- ---------------------------------------------------------------------------

-- | State push on connect: FilePresent + FileUpdate, or FileAbsent.
connectSnapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String FileEvent, Maybe FileEvent)
connectSnapshot env branch path = do
  r <- runAction env (queryFileTicks branch path)
  return $ case r of
    Left err          -> (Left err, Nothing)
    Right Nothing     -> (Right (FileAbsent Nothing), Nothing)
    Right (Just ticks) ->
      let upd = fileUpdate ticks
      in (Right (FilePresent Nothing), Just (FileUpdate upd))

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
  case cmd of
    Append _mid content -> do
      r <- runM $ actionStack env $ loggingWS conn $ handleAppend branch path content
      case r of
        Left err  -> emit (FileError (T.pack err))
        Right ()  -> pushUpdate env branch path conn

    Delete _mid ->
      emit (FileError "file delete not yet implemented")

    EditAtom _mid tickIdTxt newContent -> do
      r <- runAction env (handleEditAtom branch path tickIdTxt newContent)
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpdate env branch path conn

    DeleteAtom _mid tickIdTxt -> do
      r <- runAction env (handleDeleteAtom branch path tickIdTxt)
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpdate env branch path conn

    MoveAtom _mid tickIdTxt mAfterTickId -> do
      r <- runAction env (handleMoveAtom branch path tickIdTxt mAfterTickId)
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpdate env branch path conn

-- ---------------------------------------------------------------------------
-- Handlers — pure-ish, no WS concerns
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r ()
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    info $ "appending to: " <> T.pack path
    _tids <- appendAgent @Main path content
    info $ "append done: " <> T.pack path

handleEditAtom :: SessionEffects r => T.Text -> FilePath -> T.Text -> T.Text -> Sem r ()
handleEditAtom branch path tickIdTxt newContent =
  withBranch @Main branch $
    editAtom @Main (TickId tickIdTxt) path (TE.encodeUtf8 newContent) >> return ()

handleDeleteAtom :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r ()
handleDeleteAtom branch _path tickIdTxt =
  withBranch @Main branch $
    deleteTick @Main (TickId tickIdTxt) >> return ()

handleMoveAtom :: SessionEffects r => T.Text -> FilePath -> T.Text -> Maybe T.Text -> Sem r ()
handleMoveAtom branch _path tickIdTxt mAfterTickId =
  withBranch @Main branch $
    moveTick @Main (TickId tickIdTxt) (TickId <$> mAfterTickId) >> return ()

-- ---------------------------------------------------------------------------
-- Update builders
-- ---------------------------------------------------------------------------

-- | Push the current full file tick list to the client after any mutation.
pushUpdate :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
pushUpdate env branch path conn = do
  r <- runAction env (queryFileTicks branch path)
  case r of
    Left err          -> WS.sendTextData conn (encode (FileError (T.pack err)))
    Right Nothing     -> WS.sendTextData conn (encode (FileAbsent Nothing))
    Right (Just ticks) -> WS.sendTextData conn (encode (FileUpdate (fileUpdate ticks)))

-- | Build a FileUpdate from a list of file ticks.
--   Head is the last tick in the list (newest).
fileUpdate :: [FileTick] -> Update
fileUpdate ticks = Update
  { updateTicks = map toWireTick ticks
  , updateHead  = case reverse ticks of
                    []    -> ""
                    (t:_) -> Storage.ftTickId t
  }

-- ---------------------------------------------------------------------------
-- Query helpers
-- ---------------------------------------------------------------------------

-- | Fetch file ticks. Returns Nothing if the branch doesn't exist,
--   Just [] if the file is absent, Just ticks otherwise.
queryFileTicks
  :: SessionEffects r
  => T.Text -> FilePath -> Sem r (Maybe [FileTick])
queryFileTicks branch path =
  getBranch (BranchName branch) >>= \case
    Nothing -> return Nothing
    Just _  -> withBranch @Main branch $
      fmap Just (fileTicks @Main path)
