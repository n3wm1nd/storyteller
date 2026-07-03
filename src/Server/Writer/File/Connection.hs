{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /branch/{name}/{path} connection lifecycle.
--
-- On connect: enter the branch's storage/filesystem scope once to push the
-- initial FilePresent/FileAbsent + FileUpdate, then loop receiving commands
-- via 'embed'. Each command reopens the branch scope itself, nested inside
-- 'withStorage' — see 'commandLoop' — so its ref writes are all-or-nothing
-- and land (and notify) as soon as that one command finishes, rather than
-- being buffered for the connection's whole lifetime. A command that fails
-- is caught locally with 'Polysemy.Error.catch' and reported as a FileError
-- without unwinding the stack or ending the connection.
--
-- A second, independent stack runs on its own thread for the connection's
-- lifetime, purely to listen for ref-move broadcasts and push incremental
-- updates — the sole path by which tick state reaches this connection
-- after the initial push (including the absent → present transition on
-- first write), whether the write came from this connection's own
-- commands, another connection, or a background agent. It does *not* hold
-- one long-lived branch scope the way it once did: 'StoryBranch' reads are
-- a point-in-time snapshot from whenever a scope was opened (see
-- 'Storyteller.Git.runStoryBranchGit'), so a scope opened once at connect
-- would never notice anything written afterwards. Each notification
-- reopens the scope fresh instead (see 'onNotify') — the same "sync
-- happens at open" rule 'commandLoop' follows. Because each thread owns
-- its own stack, 'lastHead' is a plain recursive-loop accumulator inside
-- that thread's loop — no shared mutable state between the two stacks, no
-- possibility of their pushes racing.
module Server.Writer.File.Connection
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

import Server.Writer.Env (ServerEnv(..))
import Server.Core.File (FileOpen, fileState, fileStateSince)
import Server.Writer.File.Dispatch (runCommand)
import Server.Writer.File.Protocol
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Server.Core.Protocol (Update(..))
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction)
import Server.Core.Util (withBranch)
import Storyteller.Agent.Splitter (Splitter, splitByParagraph)
import Storyteller.Git (withStorage)
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
  result <- runM $ wsAction env conn $
    withBranch @Main branch (pushInitial conn path)
      >> splitByParagraph (commandLoop branch conn path)
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move and
--   tick-remap broadcasts for the connection's lifetime. Doesn't hold
--   'FileOpen' itself — each 'RefMoved' reopens the branch scope fresh
--   (see 'onNotify') to get a live view, rather than relying on one
--   long-held scope to notice writes made elsewhere.
runNotifier :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch path conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $
    void $ watchBranch chan branch Nothing (onNotify branch conn path)
  either (reportError conn) return result

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (FileError (T.pack err)))

-- | Dispatch a notification to the right push: a ref move means this file's
--   ticks may have changed, so reopen the branch scope (a fresh sync point)
--   and diff since the last push; a tick remap carries its own payload
--   straight through — see 'FileEvent.TickRemap'.
onNotify
  :: (SessionEffects r, Member (Embed IO) r, Member (Error String) r)
  => T.Text -> WS.Connection -> FilePath -> Maybe T.Text -> BranchNotification -> Sem r (Maybe T.Text)
onNotify branch conn path since note = case note of
  RefMoved _ ->
    withBranch @Main branch (pushIncremental conn path since)
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

-- | Dispatch commands until the socket closes. Doesn't itself hold
--   'FileOpen' — each command reopens the branch scope fresh (see 'handle'),
--   since that's what lets its own nested 'withStorage' actually take
--   effect: an already-open 'StoryBranch' interpreter's writes are wired to
--   whichever 'StoryStorage' was ambient when *it* was opened, not to one
--   introduced later around an individual command.
commandLoop
  :: (Member Splitter r, SessionEffects r, Member (Embed IO) r, Member (Error String) r)
  => T.Text -> WS.Connection -> FilePath -> Sem r ()
commandLoop branch conn path = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (FileError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    -- Each command is its own transaction: writes it makes (including any
    -- cross-branch cascade) either all land together or none do, and
    -- either way the ref-move notification other connections rely on
    -- fires right after this command, not just at connection close.
    handle cmd =
      catch @String
        (withStorage (withBranch @Main branch (runCommand path cmd)))
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
