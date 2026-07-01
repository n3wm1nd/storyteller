{-# LANGUAGE OverloadedStrings #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: push BranchReady (file list) then a full BranchUpdate (tick chain).
--
-- All subsequent tick state arrives through exactly one path: a background
-- listener subscribed to the server's ref-move broadcasts. Every mutation —
-- whether it came from this connection's own commands, another connection,
-- or a background agent — moves the branch's git ref, and 'Server.Run.gitNotify'
-- turns that into a broadcast this listener picks up. There is no separate
-- "push after my own command succeeded" path; the command loop below just
-- runs the mutation and lets the ref-move notification do the pushing, same
-- as it would for anyone else's write. That means only one thread ever pushes
-- tick state or tracks the last HEAD sent, so 'lastHead' is a plain
-- recursive-loop accumulator in 'notifyLoop' — no shared mutable state, no
-- possibility of two pushes racing each other.
--
-- Loop: receive BranchCommand → dispatch → run the mutation, report only
-- immediate failures/structural events. No resync command — reconnect
-- re-triggers the full state push.
module Server.Branch.Connection
  ( runBranch
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, dupTChan, readTChan)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Branch.Dispatch (dispatch, connectSnapshot, notifyUpdate)
import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Notification (BranchNotification(..))
import Server.Protocol (Update(..))
import Storyteller.Types (TickId(..))

runBranch :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runBranch env branch conn = do
  lastHead   <- pushInitial
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ notifyLoop notifyChan lastHead
  loop `finally` killThread notifier
  where
    pushInitial = do
      snap <- connectSnapshot env branch
      case snap of
        Left err -> WS.sendTextData conn (encode (BranchError (T.pack err))) >> return Nothing
        Right (files, mUpd) -> do
          WS.sendTextData conn (encode (BranchReady Nothing branch files))
          case mUpd of
            Nothing  -> return Nothing
            Just upd -> WS.sendTextData conn (encode (BranchUpdate upd)) >> return (Just (updateHead upd))

    notifyLoop chan lastHead = do
      note <- atomically (readTChan chan)
      newHead <-
        if bnBranch note == branch
          then do
            result <- notifyUpdate env branch (TickId <$> lastHead)
            case result of
              Left err         -> WS.sendTextData conn (encode (BranchError (T.pack err))) >> return lastHead
              Right Nothing    -> return lastHead
              Right (Just upd) -> WS.sendTextData conn (encode (BranchUpdate upd)) >> return (Just (updateHead upd))
          else return lastHead
      notifyLoop chan newHead

    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (BranchError "invalid message")) >> loop
          Just cmd -> dispatch env branch conn cmd >> loop
