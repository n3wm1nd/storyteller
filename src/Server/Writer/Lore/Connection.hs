{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | @\/lore\/{name}@ connection lifecycle.
--
-- Same two-thread shape as 'Server.Writer.Character.Connection': read-only,
-- so the "command" thread just pushes once on connect and then blocks until
-- the socket closes, and the notify thread reopens the branch scope and
-- recomputes 'Server.Writer.Lore.loreTree' on every 'RefMoved'. No
-- incremental cache to thread through, same reasoning as
-- 'Server.Writer.Lore's own module comment.
--
-- 'TicksRemapped' is not forwarded: this connection never puts a bare tick
-- id on the wire for the client to track, so there is nothing for a remap
-- to invalidate.
module Server.Writer.Lore.Connection
  ( runLore
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Concurrent.STM (TChan, atomically, dupTChan)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Run (SessionEffects)
import Server.Writer.Lore (loreTree)
import Server.Writer.Lore.Protocol (LoreEvent(..))
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Core.Util (withBranch)

runLore :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runLore env branch conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan
  runInitial env branch conn `finally` killThread notifier

runInitial :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runInitial env branch conn = do
  result <- runM $ wsAction env conn $
    withBranch @Main branch (push conn) >> waitForClose conn
  either (reportError conn) return result

runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env $
    void $ watchBranch chan branch () (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> () -> BranchNotification -> Sem r ()
onNotify branch conn () = \case
  RefMoved _ _    -> withBranch @Main branch (push conn)
  TicksRemapped _ -> return ()
  UndoMoved       -> return ()

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (LoreError (T.pack err)))

push :: (BranchOpen r, Member (Embed IO) r) => WS.Connection -> Sem r ()
push conn = do
  tree <- loreTree
  embed $ WS.sendTextData conn (encode (LoreTree tree))

-- | No commands to dispatch — just block until the client disconnects, so
--   the connection (and its notifier thread) stay alive for its lifetime.
waitForClose :: Member (Embed IO) r => WS.Connection -> Sem r ()
waitForClose conn = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _ -> return ()
        Right _ -> loop
