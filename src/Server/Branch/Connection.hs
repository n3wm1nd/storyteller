{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: send snapshot of current file contents immediately.
-- Loop: receive BranchCommand → dispatch → send BranchEvent(s).
-- Branch name comes from the URL — never repeated in commands.
module Server.Branch.Connection
  ( runBranch
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Branch.Dispatch (dispatch, snapshot)
import Server.Branch.Protocol
import Server.Env (ServerEnv)

runBranch :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runBranch env branch conn = do
  snap <- snapshot env branch
  case snap of
    Left err    -> WS.sendTextData conn (encode (BranchError (T.pack err)))
    Right files -> WS.sendTextData conn (encode (BranchReady Nothing branch files))
  loop
  where
    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (BranchError "invalid message")) >> loop
          Just cmd -> dispatch env branch conn cmd >> loop
