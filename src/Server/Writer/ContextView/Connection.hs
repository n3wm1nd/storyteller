{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | @\/branch\/{name}\/$context\/{path}@ connection lifecycle.
--
-- A preview-only sibling of 'Server.Writer.File.Connection': it never
-- durably writes anything (the DSL program it runs is staged only for the
-- one 'buildPreview' call, via 'Storyteller.Core.Context.setContextOverride'
-- -- see that function's own Haddock), so there is no tick chain, no
-- ref-move mutation, no presence tri-state. Same two-thread shape as
-- 'Server.Writer.Character.Connection' — a command thread and an
-- independent notify thread reacting to 'RefMoved' — but with one addition
-- Character's read-only connection doesn't need: a client-submitted
-- 'PreviewContext' carries the @(path, program)@ to resolve, and *that* has
-- to be remembered between requests so the notify thread has something to
-- re-resolve when the branch's files change without the client re-asking.
--
-- What's still true, matching 'Server.Writer.ContextView.Protocol's module
-- header: the resolution itself
-- ('Storyteller.Writer.Agent.ContextPreview.buildPreview') is a pure
-- function of "this program, this path, this branch's current content" —
-- nothing here accumulates or diffs against a prior response the way
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
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Writer.ContextView.Protocol
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Storyteller.Core.Runtime (Main)
import Storyteller.Writer.Agent.ContextCost (buildProgramCosts)
import Storyteller.Writer.Agent.ContextPreview (buildPreview)

-- | 'path' (the route parameter) is accepted but unused: each
--   'PreviewContext' command carries its own @path@, since a client may
--   want to preview against a different target file than whatever the
--   connection was opened for without reopening the socket. Kept as a
--   route parameter rather than dropped entirely so a real per-file
--   default program is a one-line change here, not a route change.
runContextView :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runContextView env branch _path conn = do
  reqVar     <- newTVarIO Nothing
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan reqVar
  runCommands env branch conn reqVar `finally` killThread notifier

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (ContextViewError (T.pack err)))

-- | The command thread: dispatch 'PreviewContext' commands until the socket
--   closes. Each command is the sole writer of 'reqVar' and pushes its own
--   response immediately, same "reopen the branch scope per command"
--   discipline every other connection follows.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> IO ()
runCommands env branch conn reqVar = do
  cancelFlag <- newTVarIO False
  result <- runM $ wsAction env conn cancelFlag $ commandLoop branch conn reqVar
  either (reportError conn) return result

commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> Sem r ()
commandLoop branch conn reqVar = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing                    -> embed (reportError conn "invalid message") >> loop
          Just (PreviewContext mid path program) -> do
            embed $ atomically $ writeTVar reqVar (Just (path, program))
            pushPreview branch conn mid path program
            loop
          Just (EstimateCost mid path program) -> do
            -- Deliberately doesn't touch 'reqVar' -- unlike a preview
            -- request, a cost estimate is a one-off "show me now" action,
            -- not something the notify thread re-runs on every branch
            -- change (see this module's own Protocol-facing Haddock).
            pushCost branch conn mid path program
            loop

-- | The notify thread: on every 'RefMoved' for this branch, re-resolve
--   whatever @(path, program)@ was last submitted (nothing to push if the
--   client hasn't sent a first request yet). 'TicksRemapped' carries
--   nothing this connection tracks, since it never puts a tick id on the
--   wire.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> TVar (Maybe (FilePath, T.Text)) -> IO ()
runNotifier env branch conn chan reqVar = do
  cancelFlag <- newTVarIO False
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env cancelFlag $
    void $ watchBranch chan branch () (onNotify branch conn reqVar)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> () -> BranchNotification -> Sem r ()
onNotify branch conn reqVar () = \case
  RefMoved _ _ -> do
    req <- embed (readTVarIO reqVar)
    maybe (return ()) (uncurry (pushPreview branch conn Nothing)) req
  TicksRemapped _ -> return ()
  UndoMoved        -> return ()

pushPreview
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> FilePath -> T.Text -> Sem r ()
pushPreview branch conn mid path program = do
  result <- withBranch @Main branch (buildPreview @Main path program)
  embed $ WS.sendTextData conn (encode (ContextPreviewed mid result))

pushCost
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> FilePath -> T.Text -> Sem r ()
pushCost branch conn mid path program = do
  costs <- withBranch @Main branch (buildProgramCosts @Main path program)
  embed $ WS.sendTextData conn (encode (ContextCosted mid costs))
