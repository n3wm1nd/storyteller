{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: enter the branch's storage/filesystem scope once to push
-- BranchReady + full BranchUpdate, then loop receiving commands via
-- 'embed'. Each command reopens the branch scope itself, nested inside
-- 'withStorage' — see 'commandLoop' — so its ref writes are all-or-nothing
-- and land (and notify) as soon as that one command finishes, rather than
-- being buffered for the connection's whole lifetime. A command that fails
-- is caught locally with 'Polysemy.Error.catch' and reported as a
-- BranchError without unwinding the stack or ending the connection.
--
-- A second, independent stack runs on its own thread for the connection's
-- lifetime, purely to listen for ref-move broadcasts and push incremental
-- updates — the sole path by which tick state reaches this connection
-- after the initial push, whether the write came from this connection's
-- own commands, another connection, or a background agent. It does *not*
-- hold one long-lived branch scope the way it once did: 'StoryBranch'
-- reads are a point-in-time snapshot from whenever a scope was opened (see
-- 'Storyteller.Git.runStoryBranchGit'), so a scope opened once at connect
-- would never notice anything written afterwards. Each notification
-- reopens the scope fresh instead (see 'onNotify') — the same "sync
-- happens at open" rule 'commandLoop' follows. Because each thread owns
-- its own stack (a 'Sem' computation can only run on the thread that calls
-- 'runM' on it), 'lastHead' is a plain recursive-loop accumulator inside
-- that thread's loop — no shared mutable state between the two stacks, no
-- possibility of their pushes racing.
module Server.Writer.Branch.Connection
  ( runBranch
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

import Server.Core.Branch (Main, BranchOpen, branchState, branchStateSince)
import Server.Writer.Branch.Dispatch (runCommand)
import Server.Writer.Branch.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Server.Core.Protocol (Update(..))
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction)
import Server.Core.Util (withBranch)
import Storyteller.Agent.Splitter (Splitter, splitByParagraph)
import Storyteller.Git (withStorage)
import Storyteller.Types (TickId(..))

runBranch :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runBranch env branch conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env branch conn notifyChan
  runCommands env branch conn `finally` killThread notifier

-- | The command-loop thread's persistent stack: enter the branch once, push
--   the initial full state, then dispatch commands until the socket closes.
runCommands :: ServerEnv -> T.Text -> WS.Connection -> IO ()
runCommands env branch conn = do
  result <- runM $ wsAction env conn $
    withBranch @Main branch (pushInitial conn branch)
      >> splitByParagraph (commandLoop conn branch)
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move
--   broadcasts for the connection's lifetime. Doesn't hold 'BranchOpen'
--   itself — each 'RefMoved' reopens the branch scope fresh (see
--   'onNotify') to get a live view, rather than relying on one long-held
--   scope to notice writes made elsewhere. Tick remaps aren't forwarded
--   here — nothing at the branch level (unlike a file connection's rebase
--   marker/context selection) tracks a bare tickId across a push yet.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $
    void $ watchBranch chan branch Nothing (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r, Member (Error String) r)
  => T.Text -> WS.Connection -> Maybe T.Text -> BranchNotification -> Sem r (Maybe T.Text)
onNotify branch conn since note = case note of
  RefMoved _      -> withBranch @Main branch (pushIncremental conn since)
  TicksRemapped _ -> return since

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (BranchError (T.pack err)))

pushInitial
  :: (BranchOpen r, Member (Embed IO) r)
  => WS.Connection -> T.Text -> Sem r ()
pushInitial conn branch = do
  (files, upd) <- branchState
  embed $ WS.sendTextData conn (encode (BranchReady Nothing branch files))
  embed $ WS.sendTextData conn (encode (BranchUpdate upd))

-- | Dispatch commands until the socket closes. Doesn't itself hold
--   'BranchOpen' — each command reopens the branch scope fresh (see
--   'handle'), since that's what lets its own nested 'withStorage' actually
--   take effect: an already-open 'StoryBranch' interpreter's writes are
--   wired to whichever 'StoryStorage' was ambient when *it* was opened, not
--   to one introduced later around an individual command.
commandLoop
  :: (Member Splitter r, SessionEffects r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> T.Text -> Sem r ()
commandLoop conn branch = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (BranchError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    -- Each command is its own transaction — see the comment in
    -- Server.Writer.File.Connection.commandLoop.
    handle cmd =
      catch @String
        (withStorage (withBranch @Main branch (runCommand branch cmd)) >>= embed . mapM_ (WS.sendTextData conn . encode))
        (\err -> embed (reportError conn err))

pushIncremental
  :: (BranchOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> Maybe T.Text -> Sem r (Maybe T.Text)
pushIncremental conn since =
  catch @String
    (do
      (_, upd) <- branchStateSince (TickId <$> since)
      embed $ WS.sendTextData conn (encode (BranchUpdate upd))
      return (Just (updateHead upd)))
    (\err -> embed (reportError conn err) >> return since)
