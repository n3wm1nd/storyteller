{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | @\/branch\/{name}\/$context\/{path}@ connection lifecycle.
--
-- A preview-only sibling of 'Server.Writer.File.Connection': it never
-- writes anything, so there is no tick chain, no ref-move mutation, no
-- presence tri-state. Same two-thread shape as
-- 'Server.Writer.Character.Connection' — a command thread and an
-- independent notify thread reacting to 'RefMoved' — but with one addition
-- Character's read-only connection doesn't need: a client-submitted
-- 'PreviewContext' carries the slots to resolve, and *that* has to be
-- remembered between requests so the notify thread has something to
-- re-resolve when the branch's files change without the client re-asking.
--
-- What's still true, matching 'Server.Writer.ContextView.Protocol's module
-- header: the resolution itself
-- ('Storyteller.Writer.Agent.ContextPreview.buildPreview') is a pure
-- function of "these slots, this branch's current files" — nothing here
-- accumulates or diffs against a prior response the way
-- 'Server.Writer.File.Connection's tick-chain push does. The one 'TVar' is
-- "what was I last asked to preview", not a derived cache.
module Server.Writer.ContextView.Connection
  ( runContextView
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, TVar, newTVarIO, atomically, dupTChan, readTVarIO, writeTVar)
import Control.Exception (SomeException, try, finally)
import Control.Monad (void)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)

import Server.Core.Util (withBranch)
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Server.Writer.Run (actionStack, wsAction)
import Server.Writer.ContextView.Protocol
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Runtime (Main)
import Storyteller.Writer.Agent.ContextPreview (buildPreview)
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)

-- | 'path' is accepted (mirroring 'Server.Writer.File.Connection's shape,
--   and the eventual per-file scoping this preview is meant to describe)
--   but unused today: 'buildPreview' resolves slots against the whole
--   branch, not one target file. Kept as a route parameter rather than
--   dropped entirely so wiring a real file-scoped default filter later is a
--   one-line change here, not a route change.
runContextView :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runContextView env branch _path conn = do
  slotsVar   <- newTVarIO []
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan slotsVar
  runCommands env branch conn slotsVar `finally` killThread notifier

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (ContextViewError (T.pack err)))

-- | The command thread: dispatch 'PreviewContext' commands until the socket
--   closes. Each command is the sole writer of 'slotsVar' and pushes its own
--   response immediately, same "reopen the branch scope per command"
--   discipline every other connection follows.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> TVar [ContextSlot] -> IO ()
runCommands env branch conn slotsVar = do
  result <- runM $ wsAction env conn $ commandLoop branch conn slotsVar
  either (reportError conn) return result

commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar [ContextSlot] -> Sem r ()
commandLoop branch conn slotsVar = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing                    -> embed (reportError conn "invalid message") >> loop
          Just (PreviewContext mid slots) -> do
            embed $ atomically $ writeTVar slotsVar slots
            pushPreview branch conn mid slots
            loop

-- | The notify thread: on every 'RefMoved' for this branch, re-resolve
--   whatever slots were last submitted (empty if the client hasn't sent a
--   first request yet, in which case there is nothing to push).
--   'TicksRemapped' carries nothing this connection tracks, since it never
--   puts a tick id on the wire.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> TVar [ContextSlot] -> IO ()
runNotifier env branch conn chan slotsVar = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $
    void $ watchBranch chan branch () (onNotify branch conn slotsVar)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar [ContextSlot] -> () -> BranchNotification -> Sem r ()
onNotify branch conn slotsVar () = \case
  RefMoved _ -> do
    slots <- embed (readTVarIO slotsVar)
    if null slots then return () else pushPreview branch conn Nothing slots
  TicksRemapped _ -> return ()

pushPreview
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> [ContextSlot] -> Sem r ()
pushPreview branch conn mid slots = do
  previews <- withBranch @Main branch (hideBinaryFiles @(BranchTag Main) @Main (buildPreview @(BranchTag Main) slots))
  embed $ WS.sendTextData conn (encode (ContextPreviewed mid previews))
