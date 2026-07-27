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
-- the socket closes. The notify thread re-reads
-- 'Server.Writer.Character.characterState' on every 'RefMoved' for this
-- branch — cheap enough (one file read) that there is no
-- incremental/since-last-push variant to maintain, unlike branch/file
-- connections' tick diffing.
--
-- Neither push opens a branch scope: both read at the branch's current
-- position via 'Storyteller.Core.Snapshot.runSnapshotFS' (see 'push'), so
-- "read-only" is a property of this module's types rather than a promise
-- in this comment.
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

import Server.Core.Run (SessionEffects)
import Server.Writer.Character (characterState, CharacterState(..))
import Server.Writer.Character.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import qualified Storage.Core as Core
import Server.Core.Branch (Main)
import Storyteller.Core.Branch (withBranch)
import Storyteller.Core.Git (BranchTag(..), runStoryFSRead)
import Storyteller.Core.Storage (getBranch)
import Storyteller.Core.Types (BranchName(..), branchHead, unTickId)

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
    push conn branch >> waitForClose conn
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
  RefMoved _ _    -> push conn branch
  TicksRemapped _ -> return ()
  UndoMoved       -> return ()

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (CharacterError (T.pack err)))

-- | Reads the branch at its own current position rather than opening its
--   scope. This connection is read-only by design (see 'runInitial''s own
--   note -- it has no commands at all), and 'Server.Writer.Character.characterState'
--   is two 'fileExists' and a 'readFile', so 'Server.Core.Util.withBranch'
--   was supplying a writable, chain-editing 'Storyteller.Core.Branch.BranchOp'
--   scope -- rebuilt on every ref move, for every connected client -- to
--   serve a push that never writes anything. Now the type says so: no
--   'Runix.FileSystem.FileSystemWrite' in the row a snapshot interpreter
--   supplies, and no head to advance.
push :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> T.Text -> Sem r ()
push conn branch = getBranch (BranchName branch) >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack branch)
  Just _  -> do
    st <- withBranch @Main (BranchName branch) $
            runStoryFSRead @(BranchTag Main) @Main (BranchTag (BranchName branch))
              (characterState @(BranchTag Main) branch)
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
