{-# LANGUAGE OverloadedStrings #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: push BranchReady (file list) then a full BranchUpdate (tick chain).
-- Loop: receive BranchCommand → dispatch → server pushes resulting BranchUpdate.
-- No resync command — reconnect re-triggers the full state push.
module Server.Branch.Connection
  ( runBranch
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Branch.Dispatch (dispatch, connectSnapshot)
import Server.Branch.Protocol
import Server.Env (ServerEnv(..))

runBranch :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runBranch env branch conn = do
  snap <- connectSnapshot env branch
  case snap of
    Left err -> WS.sendTextData conn (encode (BranchError (T.pack err)))
    Right (files, mUpd) -> do
      WS.sendTextData conn (encode (BranchReady Nothing branch files))
      maybe (return ()) (WS.sendTextData conn . encode . BranchUpdate) mUpd
  loop
  where
    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (BranchError "invalid message")) >> loop
          Just cmd -> dispatch env branch conn cmd >> loop
