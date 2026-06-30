{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: send snapshot of current file contents immediately.
-- Loop: receive BranchCommand → dispatch → send BranchEvent(s).
-- Branch name comes from the URL — never repeated in commands.
--
-- A background thread subscribes to the global notification channel and
-- forwards branch.invalidated events to the client whenever the branch's
-- tick chain changes on the server side.
module Server.Branch.Connection
  ( runBranch
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, dupTChan, readTChan)
import Control.Exception (SomeException, try)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Branch.Dispatch (dispatch, snapshot, tickSnapshot)
import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Notification (BranchNotification(..))

runBranch :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runBranch env branch conn = do
  snap <- snapshot env branch
  case snap of
    Left err    -> WS.sendTextData conn (encode (BranchError (T.pack err)))
    Right files -> do
      WS.sendTextData conn (encode (BranchReady Nothing branch files))
      ticks <- tickSnapshot env branch
      case ticks of
        Left _   -> return ()
        Right ts -> WS.sendTextData conn (encode (BranchTicks ts))

  -- Subscribe to the global notification channel and forward matching events.
  sub <- atomically (dupTChan (envNotifyChan env))
  _ <- forkIO $ notifyLoop sub

  loop
  where
    notifyLoop sub = do
      BranchNotification b mapping <- atomically (readTChan sub)
      if b == branch
        then do
          let pairs = map (\(o,n) -> (o,n)) mapping
          result <- try (WS.sendTextData conn (encode (TicksInvalidated Nothing pairs)))
                    :: IO (Either SomeException ())
          case result of
            Left  _ -> return ()   -- connection closed; stop the notify thread
            Right _ -> notifyLoop sub
        else notifyLoop sub

    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (BranchError "invalid message")) >> loop
          Just cmd -> dispatch env branch conn cmd >> loop
