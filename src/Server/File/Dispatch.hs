{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
--
-- Routing only: decode FileCommand → call Server.File → emit events.
-- No business logic lives here.
module Server.File.Dispatch
  ( dispatch
  , connectSnapshot
  ) where

import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (runM)

import Server.File (fileState, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom)
import Server.File.Protocol (FileCommand(..), FileEvent(..))
import Server.Env (ServerEnv)
import Server.Protocol (Update(..))
import Server.Run (runAction, actionStack, loggingWS)

import Storyteller.Types (BranchName(..), TickId(..))

-- ---------------------------------------------------------------------------
-- Connect snapshot
-- ---------------------------------------------------------------------------

connectSnapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String FileEvent, Maybe FileEvent)
connectSnapshot env branch path = do
  r <- runAction env (fileState (BranchName branch) path)
  return $ case r of
    Left err         -> (Left err, Nothing)
    Right Nothing    -> (Right (FileAbsent Nothing), Nothing)
    Right (Just upd) ->
      if null (updateTicks upd)
        then (Right (FileAbsent Nothing), Nothing)
        else (Right (FilePresent Nothing), Just (FileUpdate upd))

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit    = WS.sendTextData conn . encode
      name    = BranchName branch
      pushUpd = do
        r <- runAction env (fileState name path)
        case r of
          Left err         -> emit (FileError (T.pack err))
          Right Nothing    -> emit (FileAbsent Nothing)
          Right (Just upd) -> emit (FileUpdate upd)

  case cmd of
    Append _mid content -> do
      r <- runM $ actionStack env $ loggingWS conn $ appendToFile name path content
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpd

    Delete _mid ->
      emit (FileError "file delete not yet implemented")

    EditAtom _mid tid content -> do
      r <- runAction env (editFileAtom name path (TickId tid) content)
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpd

    DeleteAtom _mid tid -> do
      r <- runAction env (deleteFileAtom name path (TickId tid))
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpd

    MoveAtom _mid tid mAfter -> do
      r <- runAction env (moveFileAtom name path (TickId tid) (TickId <$> mAfter))
      case r of
        Left err -> emit (FileError (T.pack err))
        Right () -> pushUpd
