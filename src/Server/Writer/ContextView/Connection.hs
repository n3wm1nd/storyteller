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
-- 'PreviewContext'\/'EntriesFor' carries what it resolved against, and
-- *that* has to be remembered between requests so the notify thread has
-- something to re-resolve when the branch's files change without the
-- client re-asking.
--
-- What's still true, matching 'Server.Writer.ContextView.Protocol's module
-- header: the resolution itself
-- ('Storyteller.Writer.Agent.ContextPreview.buildPreview'\/'buildEntries0'\/
-- 'buildEntries1') is a pure function of "this program\/name, this path,
-- this branch's current content" — nothing here accumulates or diffs
-- against a prior response the way 'Server.Writer.File.Connection's
-- tick-chain push does. Each of the two 'TVar's is "what was I last asked
-- to preview\/list", not a derived cache -- kept separate (rather than one
-- combined "last request" slot) since a client can have an outstanding
-- preview and an outstanding entries list at once (e.g. the lore editor's
-- preview alongside its own file-toggle list), each wanting its own
-- independent re-push on every branch change.
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
import Storyteller.Writer.Agent.ContextCost (buildProgramCosts, buildAdhocProgramCosts)
import Storyteller.Writer.Agent.ContextPreview (buildPreview, buildAdhocPreview, buildEntries0, buildEntries1)

-- | 'path' (the route parameter) is accepted but unused: each
--   'PreviewContext' command carries its own @path@, since a client may
--   want to preview against a different target file than whatever the
--   connection was opened for without reopening the socket. Kept as a
--   route parameter rather than dropped entirely so a real per-file
--   default program is a one-line change here, not a route change.
runContextView :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runContextView env branch _path conn = do
  reqVar     <- newTVarIO Nothing
  entriesVar <- newTVarIO Nothing
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan reqVar entriesVar
  runCommands env branch conn reqVar entriesVar `finally` killThread notifier

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (ContextViewError (T.pack err)))

-- | The command thread: dispatch commands until the socket closes. Each of
--   'PreviewContext'\/'EntriesFor' is the sole writer of its own 'TVar' and
--   pushes its own response immediately, same "reopen the branch scope per
--   command" discipline every other connection follows.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> TVar (Maybe (T.Text, Maybe FilePath)) -> IO ()
runCommands env branch conn reqVar entriesVar = do
  cancelFlag <- newTVarIO False
  result <- runM $ wsAction env conn cancelFlag $ commandLoop branch conn reqVar entriesVar
  either (reportError conn) return result

commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> TVar (Maybe (T.Text, Maybe FilePath)) -> Sem r ()
commandLoop branch conn reqVar entriesVar = loop
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
          Just (PreviewAdhoc mid program mPath) -> do
            -- Deliberately doesn't touch 'reqVar' -- same reasoning as
            -- 'EstimateAdhocCost' just below: a bare-snippet preview has
            -- no @path@ of its own for the notify thread to re-resolve
            -- against, and (like the adhoc cost case) is a one-off
            -- "show me now" request, not a live-updating view.
            pushAdhocPreview branch conn mid program mPath
            loop
          Just (EstimateCost mid path program) -> do
            -- Deliberately doesn't touch 'reqVar' -- unlike a preview
            -- request, a cost estimate is a one-off "show me now" action,
            -- not something the notify thread re-runs on every branch
            -- change (see this module's own Protocol-facing Haddock).
            pushCost branch conn mid path program
            loop
          Just (EstimateAdhocCost mid program mPath) -> do
            pushAdhocCost branch conn mid program mPath
            loop
          Just (EntriesFor mid name mPath) -> do
            embed $ atomically $ writeTVar entriesVar (Just (name, mPath))
            pushEntries branch conn mid name mPath
            loop

-- | The notify thread: on every 'RefMoved' for this branch, re-resolve
--   whatever @(path, program)@\/@(name, path)@ was last submitted on each
--   of 'reqVar'\/'entriesVar' (nothing to push for either one the client
--   hasn't sent a first request on yet). 'TicksRemapped' carries nothing
--   this connection tracks, since it never puts a tick id on the wire.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> TVar (Maybe (FilePath, T.Text)) -> TVar (Maybe (T.Text, Maybe FilePath)) -> IO ()
runNotifier env branch conn chan reqVar entriesVar = do
  cancelFlag <- newTVarIO False
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env cancelFlag $
    void $ watchBranch chan branch () (onNotify branch conn reqVar entriesVar)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> TVar (Maybe (FilePath, T.Text)) -> TVar (Maybe (T.Text, Maybe FilePath)) -> () -> BranchNotification -> Sem r ()
onNotify branch conn reqVar entriesVar () = \case
  RefMoved _ _ -> do
    req     <- embed (readTVarIO reqVar)
    entries <- embed (readTVarIO entriesVar)
    maybe (return ()) (uncurry (pushPreview branch conn Nothing)) req
    maybe (return ()) (uncurry (pushEntries branch conn Nothing)) entries
  TicksRemapped _ -> return ()
  UndoMoved        -> return ()

pushPreview
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> FilePath -> T.Text -> Sem r ()
pushPreview branch conn mid path program = do
  result <- withBranch @Main branch (buildPreview @Main path program)
  embed $ WS.sendTextData conn (encode (ContextPreviewed mid result))

pushAdhocPreview
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> T.Text -> Maybe FilePath -> Sem r ()
pushAdhocPreview branch conn mid program mPath = do
  result <- withBranch @Main branch (buildAdhocPreview @Main program mPath)
  embed $ WS.sendTextData conn (encode (ContextPreviewed mid result))

pushCost
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> FilePath -> T.Text -> Sem r ()
pushCost branch conn mid path program = do
  costs <- withBranch @Main branch (buildProgramCosts @Main path program)
  embed $ WS.sendTextData conn (encode (ContextCosted mid costs))

pushAdhocCost
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> T.Text -> Maybe FilePath -> Sem r ()
pushAdhocCost branch conn mid program mPath = do
  costs <- withBranch @Main branch (buildAdhocProgramCosts @Main program mPath)
  embed $ WS.sendTextData conn (encode (ContextCosted mid costs))

-- | @name@'s own arity decides which of 'buildEntries0'\/'buildEntries1' to
--   call -- @mPath@ present means the client is asking about a 1-arity
--   slot like @context.other@ (framed against that file), @mPath@ absent
--   means a 0-arity one like @context.lore@. There is no dynamic arity
--   lookup here: every real caller already knows which shape the slot it's
--   asking about has (the same fixed fact 'Storyteller.Core.Context.
--   resolveContext0'\/'resolveContext1''s own split rests on), so this
--   just mirrors that choice from the wire rather than probing for it.
pushEntries
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> T.Text -> Maybe FilePath -> Sem r ()
pushEntries branch conn mid name mPath = do
  entries <- withBranch @Main branch $ case mPath of
    Just path -> buildEntries1 @Main name path
    Nothing   -> buildEntries0 @Main name
  embed $ WS.sendTextData conn (encode (ContextEntries mid entries))
