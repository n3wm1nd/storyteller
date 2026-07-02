{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /branch/{name} connection lifecycle.
--
-- On connect: enter the branch's storage/filesystem scope exactly once for
-- the connection's whole lifetime (not re-entered per command — see
-- 'Server.Branch.BranchOpen'), push BranchReady + full BranchUpdate, then
-- loop receiving commands via 'embed'. A command that fails is caught
-- locally with 'Polysemy.Error.catch' and reported as a BranchError without
-- unwinding the stack or ending the connection.
--
-- A second, independent long-lived stack runs on its own thread, entering
-- the same branch, purely to listen for ref-move broadcasts and push
-- incremental updates — the sole path by which tick state reaches this
-- connection after the initial push, whether the write came from this
-- connection's own commands, another connection, or a background agent.
-- Because each thread owns its own stack (a 'Sem' computation can only run
-- on the thread that calls 'runM' on it), 'lastHead' is a plain
-- recursive-loop accumulator inside that thread's loop — no shared mutable
-- state between the two stacks, no possibility of their pushes racing.
module Server.Branch.Connection
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

import Server.Branch (Main, BranchOpen, branchState, branchStateSince)
import Server.Branch.Dispatch (runCommand)
import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Notification (BranchNotification(..), watchBranch)
import Server.Protocol (Update(..))
import Server.Run (SessionEffects, actionStack, loggingWS)
import Server.Util (withBranchSplitter)
import Storyteller.Agent.Splitter (Splitter)
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
  result <- runM $ actionStack env $ loggingWS conn $
    withBranchSplitter @Main branch $ do
      pushInitial conn branch
      commandLoop conn branch
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: enter the branch once,
--   then react to ref-move broadcasts for the connection's lifetime. Tick
--   remaps aren't forwarded here — nothing at the branch level (unlike a
--   file connection's rebase marker/context selection) tracks a bare tickId
--   across a push yet.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ actionStack env $
    withBranchSplitter @Main branch $ void $ watchBranch chan branch Nothing (onNotify conn)
  either (reportError conn) return result

onNotify
  :: (BranchOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> Maybe T.Text -> BranchNotification -> Sem r (Maybe T.Text)
onNotify conn since note = case note of
  RefMoved _      -> pushIncremental conn since
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

commandLoop
  :: (BranchOpen r, Member Splitter r, SessionEffects r, Member (Embed IO) r, Member (Error String) r)
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

    handle cmd =
      catch @String
        (runCommand branch cmd >>= embed . mapM_ (WS.sendTextData conn . encode))
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
