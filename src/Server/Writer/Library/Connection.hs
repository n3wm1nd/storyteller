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
-- branch scope and re-pushes the whole tree on every 'RefMoved'. Unlike
-- those two, there's no incremental\/since-last-push variant to maintain —
-- same call as 'Server.Writer.Character.Connection' makes: recomputing the
-- whole tree on every notification is cheap enough for now not to bother
-- with a diff.
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
import Server.Writer.Library (libraryTree)
import Server.Writer.Library.Dispatch (runCommand)
import Server.Writer.Library.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction)
import Server.Core.Util (withBranch)
import Storyteller.Core.Git (withStorage)

runLibrary :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runLibrary env branch conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan
  runCommands env branch conn `finally` killThread notifier

-- | The command-loop thread's persistent stack: enter the branch once, push
--   the initial tree, then dispatch commands until the socket closes.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runCommands env branch conn = do
  result <- runM $ wsAction env conn $
    withBranch @Main branch (push conn) >> commandLoop conn branch
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move
--   broadcasts for the connection's lifetime, reopening the branch scope
--   fresh each time (same "sync happens at open" rule as
--   'Server.Writer.Character.Connection').
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $
    void $ watchBranch chan branch () (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> () -> BranchNotification -> Sem r ()
onNotify branch conn () = \case
  RefMoved _      -> withBranch @Main branch (push conn)
  TicksRemapped _ -> return ()

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (LibraryError (T.pack err)))

push :: (BranchOpen r, Member (Embed IO) r) => WS.Connection -> Sem r ()
push conn = do
  (tree, chapters) <- libraryTree
  embed $ WS.sendTextData conn (encode (LibraryTree tree chapters))

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
