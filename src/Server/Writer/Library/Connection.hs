{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | @\/library\/{name}@ connection lifecycle.
--
-- Same two-thread shape as 'Server.Writer.Branch.Connection'/
-- 'Server.Writer.File.Connection' (see their module comments for the full
-- rationale): a command thread that pushes the initial tree then loops
-- dispatching commands, and an independent notify thread that reopens the
-- branch scope and re-pushes the tree on every 'RefMoved'.
--
-- Unlike 'Server.Writer.Character.Connection', a real incremental cache
-- *is* worth maintaining here: 'Server.Writer.Library.libraryTree' folds
-- both chapter headings and each leaf's binary\/tracked flag via
-- 'Storage.Core.memoFold' rather than re-deriving either from scratch on
-- every push, so the notify thread threads its own accumulator through
-- 'watchBranch' — same mechanism 'Server.Writer.Branch.Connection' already
-- uses for its file-set accumulator, just carrying a 'LibraryFoldCache'
-- checkpoint set instead of a 'Set FilePath'. The command thread's own initial push starts cold
-- (it only ever runs once); the notify thread seeds its own accumulator
-- with one more push at startup, same reasoning as
-- 'Server.Writer.Branch.Connection.runNotifier' seeding its initial file
-- set — so the first real 'RefMoved' this connection sees is already warm.
--
-- 'TicksRemapped' is not forwarded: this connection never puts a bare tick
-- id on the wire for the client to track, so there is nothing for a remap
-- to invalidate.
module Server.Writer.Library.Connection
  ( runLibrary
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
import Polysemy.Error (catch)

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Logging (logCommand)
import Server.Writer.Library (LibraryFoldCache, libraryTree)
import Server.Writer.Library.Dispatch (runCommand)
import Server.Writer.Library.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Core.Util (withBranch)
import Storyteller.Core.Git (withStorage)
import qualified Storage.Core as Core

type Cache = [(Core.ObjectHash, LibraryFoldCache)]

runLibrary :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runLibrary env branch conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan
  runCommands env branch conn `finally` killThread notifier

-- | The command-loop thread's persistent stack: enter the branch once, push
--   the initial tree (cold -- this thread never loops on its own push, so
--   there's no accumulator to seed), then dispatch commands until the
--   socket closes.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runCommands env branch conn = do
  result <- runM $ wsAction env conn $
    withBranch @Main branch (void (push conn [])) >> commandLoop conn branch
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move
--   broadcasts for the connection's lifetime, reopening the branch scope
--   fresh each time (same "sync happens at open" rule as
--   'Server.Writer.Character.Connection'). Seeds its own cache with one
--   push at startup so the first real 'RefMoved' is already warm.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env $ do
    initialCache <- withBranch @Main branch (push conn [])
    void $ watchBranch chan branch initialCache (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> Cache -> BranchNotification -> Sem r Cache
onNotify branch conn cache = \case
  RefMoved _ _    -> withBranch @Main branch (push conn cache)
  TicksRemapped _ -> return cache
  UndoMoved       -> return cache

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (LibraryError (T.pack err)))

push :: (BranchOpen r, Member (Embed IO) r) => WS.Connection -> Cache -> Sem r Cache
push conn cache = do
  (tree, chapters, nextCache) <- libraryTree cache
  embed $ WS.sendTextData conn (encode (LibraryTree tree chapters))
  return nextCache

-- | Dispatch commands until the socket closes. Each command reopens the
--   branch scope fresh, nested inside 'withStorage', so its writes are
--   all-or-nothing and land (and notify) as soon as that one command
--   finishes — same as 'Server.Writer.Branch.Connection.commandLoop'.
commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => WS.Connection -> T.Text -> Sem r ()
commandLoop conn branch = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (LibraryError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    handle cmd =
      catch @String
        (logCommand (commandKind cmd)
          (withStorage (withBranch @Main branch (runCommand cmd)))
          >>= embed . mapM_ (WS.sendTextData conn . encode))
        (\err -> embed (reportError conn err))
