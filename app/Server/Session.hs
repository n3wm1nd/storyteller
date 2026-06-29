{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | WebSocket session lifecycle.
--
-- Each connection goes through:
--   1. Receive session.open  → bind branch
--   2. Send session.ready    → snapshot of current file contents
--   3. Loop: receive command → dispatch → send event(s)
module Server.Session
  ( runSession
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Dispatch (dispatch, snapshot)
import Server.Env (ServerEnv)
import Server.Protocol

runSession :: ServerEnv -> WS.Connection -> IO ()
runSession env conn = do
  msg <- WS.receiveData conn
  case decode msg of
    Just (SessionOpen branch) -> do
      snap <- snapshot env branch
      case snap of
        Left err    -> send conn (Error (T.pack err))
        Right files -> send conn (SessionReady branch files)
      loop branch
    _ -> send conn (Error "expected session.open")
  where
    loop branch = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> send conn (Error "invalid message") >> loop branch
          Just cmd -> dispatch env branch conn cmd >> loop branch

send :: WS.Connection -> Event -> IO ()
send conn = WS.sendTextData conn . encode
