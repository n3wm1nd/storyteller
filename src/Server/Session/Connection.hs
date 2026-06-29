{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | /session connection lifecycle.
--
-- On connect: ready immediately, no handshake needed.
-- Loop: receive SessionCommand → dispatch → send SessionEvent(s).
module Server.Session.Connection
  ( runSession
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS

import Server.Env (ServerEnv)
import Server.Session.Dispatch (dispatch)
import Server.Session.Protocol

runSession :: ServerEnv -> WS.Connection -> IO ()
runSession env conn = do
  WS.sendTextData conn (encode SessionReady')
  loop
  where
    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (SessionError "invalid message")) >> loop
          Just cmd -> dispatch env conn cmd >> loop
