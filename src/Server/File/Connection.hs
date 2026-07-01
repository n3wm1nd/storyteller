{-# LANGUAGE OverloadedStrings #-}

-- | /branch/{name}/{path} connection lifecycle.
--
-- On connect: push FilePresent + FileUpdate (full tick list), or FileAbsent.
-- Loop: receive FileCommand → dispatch → server pushes resulting FileUpdate.
-- No resync command — reconnect re-triggers the full state push.
module Server.File.Connection
  ( runFile
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Env (ServerEnv)
import Server.File.Dispatch (dispatch, connectSnapshot)
import Server.File.Protocol

runFile :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runFile env branch path conn = do
  (evt, mUpd) <- connectSnapshot env branch path
  case evt of
    Left err -> WS.sendTextData conn (encode (FileError (T.pack err)))
    Right e  -> do
      WS.sendTextData conn (encode e)
      maybe (return ()) (WS.sendTextData conn . encode) mUpd
  loop
  where
    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (FileError "invalid message")) >> loop
          Just cmd -> dispatch env branch path conn cmd >> loop
