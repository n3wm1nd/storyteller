{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /character/{charBranch} connection lifecycle.
--
-- Same two-thread shape as 'Server.Writer.Branch.Connection'/
-- 'Server.Writer.File.Connection' (see their module comments for the full
-- rationale) except there is no command loop: this connection is read-only,
-- so the "command" thread just pushes once on connect and then blocks until
-- the socket closes. The notify thread reopens the branch scope and
-- recomputes 'Server.Writer.Character.characterState' on every 'RefMoved'
-- for this branch — cheap enough (one file read) that there is no
-- incremental/since-last-push variant to maintain, unlike branch/file
-- connections' tick diffing.
--
-- 'TicksRemapped' is not forwarded: this connection never puts a bare tick
-- id on the wire for the client to track, so there is nothing for a remap
-- to invalidate.
module Server.Writer.Character.Connection
  ( runCharacter
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newTVarIO)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Run (SessionEffects)
import Server.Writer.Character (characterState, CharacterState(..))
import Server.Writer.Character.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Core.Util (withBranch)

runCharacter :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runCharacter env branch conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan
  runInitial env branch conn `finally` killThread notifier

-- | Read-only connection — no commands, so no cancel flag ever gets set;
--   a fresh, unshared 'TVar Bool' satisfies 'wsAction's signature.
runInitial :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runInitial env branch conn = do
  cancelFlag <- newTVarIO False
  result <- runM $ wsAction env conn cancelFlag $
    withBranch @Main branch (push conn branch) >> waitForClose conn
  either (reportError conn) return result

runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  cancelFlag <- newTVarIO False
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env cancelFlag $
    void $ watchBranch chan branch () (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> () -> BranchNotification -> Sem r ()
onNotify branch conn () = \case
  RefMoved _ _    -> withBranch @Main branch (push conn branch)
  TicksRemapped _ -> return ()
  UndoMoved       -> return ()

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (CharacterError (T.pack err)))

push :: (BranchOpen r, Member (Embed IO) r) => WS.Connection -> T.Text -> Sem r ()
push conn branch = do
  st <- characterState branch
  embed $ WS.sendTextData conn (encode (CharacterUpdate (charName st) (charSheet st) (charHasAvatar st)))

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
