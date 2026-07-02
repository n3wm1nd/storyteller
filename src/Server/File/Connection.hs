{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /branch/{name}/{path} connection lifecycle.
--
-- On connect: enter the branch's storage/filesystem scope exactly once for
-- the connection's whole lifetime (not re-entered per command — see
-- 'Server.File.FileOpen'), push FilePresent/FileAbsent + FileUpdate, then
-- loop receiving commands via 'embed'. A command that fails is caught
-- locally with 'Polysemy.Error.catch' and reported as a FileError without
-- unwinding the stack or ending the connection.
--
-- A second, independent long-lived stack runs on its own thread, entering
-- the same branch, purely to listen for ref-move broadcasts and push
-- incremental updates — the sole path by which tick state reaches this
-- connection after the initial push (including the absent → present
-- transition on first write), whether the write came from this connection's
-- own commands, another connection, or a background agent. Because each
-- thread owns its own stack, 'lastHead' is a plain recursive-loop
-- accumulator inside that thread's loop — no shared mutable state between
-- the two stacks, no possibility of their pushes racing.
module Server.File.Connection
  ( runFile
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Concurrent.STM (TChan, atomically, dupTChan)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)
import Polysemy.Error (Error, catch)

import Server.Env (ServerEnv(..))
import Server.File (FileOpen, fileState, fileStateSince)
import Server.File.Dispatch (runCommand)
import Server.File.Protocol
import Server.Notification (BranchNotification(..), watchBranch)
import Server.Protocol (Update(..))
import Server.Run (SessionEffects, actionStack, loggingWS)
import Server.Util (withBranchSplitter)
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.Runtime (Main)

runFile :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runFile env branch path conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch path conn notifyChan
  runCommands env branch path conn `finally` killThread notifier

-- | The command-loop thread's persistent stack: enter the branch once, push
--   the initial file state, then dispatch commands until the socket closes.
runCommands :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runCommands env branch path conn = do
  result <- runM $ actionStack env $ loggingWS conn $
    withBranchSplitter @Main branch $ do
      pushInitial conn path
      commandLoop conn path
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: enter the branch once,
--   then react to ref-move and tick-remap broadcasts for the connection's
--   lifetime.
runNotifier :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch path conn chan = do
  result <- runM $ actionStack env $
    withBranchSplitter @Main branch $ void $ watchBranch chan branch Nothing (onNotify conn path)
  either (reportError conn) return result

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (FileError (T.pack err)))

-- | Dispatch a notification to the right push: a ref move means this file's
--   ticks may have changed, so refetch and diff since the last push; a tick
--   remap carries its own payload straight through — see 'FileEvent.TickRemap'.
onNotify
  :: (FileOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> FilePath -> Maybe T.Text -> BranchNotification -> Sem r (Maybe T.Text)
onNotify conn path since note = case note of
  RefMoved _ ->
    pushIncremental conn path since
  TicksRemapped mapping -> do
    embed $ WS.sendTextData conn (encode (TickRemap mapping))
    return since

-- | Push present/absent plus the initial update, mirroring the shape used
--   throughout: presence is just "does this file have any ticks yet".
pushInitial :: (FileOpen r, Member (Embed IO) r) => WS.Connection -> FilePath -> Sem r ()
pushInitial conn path = do
  upd <- fileState path
  if null (updateTicks upd)
    then embed $ WS.sendTextData conn (encode (FileAbsent Nothing))
    else do
      embed $ WS.sendTextData conn (encode (FilePresent Nothing))
      embed $ WS.sendTextData conn (encode (FileUpdate upd))

commandLoop
  :: (FileOpen r, Member Splitter r, SessionEffects r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> FilePath -> Sem r ()
commandLoop conn path = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (FileError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    handle cmd =
      catch @String
        (runCommand path cmd)
        (\err -> embed (reportError conn err))

-- 'since = Nothing' means we're still in the absent state from connect —
-- mirror 'pushInitial' exactly, so it transitions to present the moment
-- the file gets its first tick. 'since = Just tid' means we already have
-- a HEAD to diff against; skip the push entirely if this write didn't
-- touch this file's chain.
pushIncremental
  :: (FileOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> FilePath -> Maybe T.Text -> Sem r (Maybe T.Text)
pushIncremental conn path since =
  catch @String
    (do
      upd <- fileStateSince path since
      case since of
        Nothing | null (updateTicks upd) -> return since
                | otherwise -> do
                    embed $ WS.sendTextData conn (encode (FilePresent Nothing))
                    embed $ WS.sendTextData conn (encode (FileUpdate upd))
                    return (Just (updateHead upd))
        Just _  | null (updateTicks upd) -> return since
                | otherwise -> do
                    embed $ WS.sendTextData conn (encode (FileUpdate upd))
                    return (Just (updateHead upd))
    )
    (\err -> embed (reportError conn err) >> return since)
