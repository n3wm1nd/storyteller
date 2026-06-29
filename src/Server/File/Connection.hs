{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | /branch/{name}/{path...} connection lifecycle.
--
-- On connect: send current file content, or file.absent if the path doesn't exist.
-- Loop: receive FileCommand → dispatch → send FileEvent(s).
-- Branch name and file path are implicit from the URL — not repeated in commands.
module Server.File.Connection
  ( runFile
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Env (ServerEnv)
import Server.File.Dispatch (dispatch, snapshot)
import Server.File.Protocol

runFile :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runFile env branch path conn = do
  snap <- snapshot env branch path
  case snap of
    Left err        -> WS.sendTextData conn (encode (FileError (T.pack err)))
    Right Nothing   -> WS.sendTextData conn (encode (FileAbsent Nothing))
    Right (Just c)  -> WS.sendTextData conn (encode (FileContent Nothing c))
  loop
  where
    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (FileError "invalid message")) >> loop
          Just cmd -> dispatch env branch path conn cmd >> loop
