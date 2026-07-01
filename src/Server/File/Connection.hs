{-# LANGUAGE OverloadedStrings #-}

-- | /branch/{name}/{path} connection lifecycle.
--
-- On connect: push FilePresent + FileUpdate (full tick list), or FileAbsent.
--
-- All subsequent tick state arrives through exactly one path: a background
-- listener subscribed to the server's ref-move broadcasts. Every mutation —
-- whether it came from this connection's own commands, another connection,
-- or a background agent — moves the branch's git ref, and 'Server.Run.gitNotify'
-- turns that into a broadcast this listener picks up (including the absent
-- → present transition on first write). There is no separate "push after my
-- own command succeeded" path; the command loop below just runs the mutation
-- and lets the ref-move notification do the pushing, same as it would for
-- anyone else's write. That means only one thread ever pushes tick state or
-- tracks the last HEAD sent, so 'lastHead' is a plain recursive-loop
-- accumulator in 'notifyLoop' — no shared mutable state, no possibility of
-- two pushes racing each other.
--
-- Loop: receive FileCommand → dispatch → run the mutation, report only
-- immediate failures. No resync command — reconnect re-triggers the full
-- state push.
module Server.File.Connection
  ( runFile
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, dupTChan, readTChan)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Env (ServerEnv(..))
import Server.File.Dispatch (dispatch, connectSnapshot, notifyUpdate)
import Server.File.Protocol
import Server.Notification (BranchNotification(..))
import Server.Protocol (Update(..))

runFile :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runFile env branch path conn = do
  lastHead   <- pushInitial
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ notifyLoop notifyChan lastHead
  loop `finally` killThread notifier
  where
    pushInitial = do
      (evt, mUpd) <- connectSnapshot env branch path
      case evt of
        Left err -> WS.sendTextData conn (encode (FileError (T.pack err))) >> return Nothing
        Right e  -> do
          WS.sendTextData conn (encode e)
          case mUpd of
            Just (FileUpdate upd) -> WS.sendTextData conn (encode (FileUpdate upd)) >> return (Just (updateHead upd))
            _                     -> return Nothing

    notifyLoop chan lastHead = do
      note <- atomically (readTChan chan)
      newHead <-
        if bnBranch note == branch
          then do
            result <- notifyUpdate env branch path lastHead
            case result of
              Left err -> WS.sendTextData conn (encode (FileError (T.pack err))) >> return lastHead
              Right (mEvt, mUpd) -> do
                mapM_ (WS.sendTextData conn . encode) mEvt
                case mUpd of
                  Nothing  -> return lastHead
                  Just upd -> WS.sendTextData conn (encode (FileUpdate upd)) >> return (Just (updateHead upd))
          else return lastHead
      notifyLoop chan newHead

    loop = do
      result <- try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString)
      case result of
        Left  _   -> return ()
        Right msg -> case decode msg of
          Nothing  -> WS.sendTextData conn (encode (FileError "invalid message")) >> loop
          Just cmd -> dispatch env branch path conn cmd >> loop
