{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /session connection lifecycle.
--
-- On connect: enter the storage-level scope exactly once for the
-- connection's whole lifetime (no branch open, no notifier thread — a
-- session has neither a branch scope nor ref-move state to watch), push
-- SessionReady, then loop receiving commands via 'embed'. A command that
-- fails is caught locally with 'Polysemy.Error.catch' and reported as a
-- SessionError without unwinding the stack or ending the connection.
module Server.Session.Connection
  ( runSession
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)
import Polysemy.Error (catch)

import Server.Env (ServerEnv)
import Server.Run (SessionEffects, actionStack)
import Server.Session.Dispatch (runCommand)
import Server.Session.Protocol

runSession :: ServerEnv -> WS.Connection -> IO ()
runSession env conn = do
  result <- runM $ actionStack env $ do
    embed $ WS.sendTextData conn (encode SessionReady')
    commandLoop conn
  either (reportError conn) return result

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (SessionError (T.pack err)))

commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => WS.Connection -> Sem r ()
commandLoop conn = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (SessionError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    handle cmd =
      catch @String
        (runCommand conn cmd)
        (\err -> embed (reportError conn err))
