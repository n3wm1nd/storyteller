{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /session connection lifecycle.
--
-- On connect: enter the storage-level scope exactly once for the
-- connection's whole lifetime (no branch open — a session has no one
-- branch's tick state to snapshot), push SessionReady followed immediately
-- by the current branch, character, and undo-log lists, then loop receiving
-- commands via 'embed'. A command that fails is caught locally with
-- 'Polysemy.Error.catch' and reported as a SessionError without unwinding
-- the stack or ending the connection.
--
-- There is no list-branches/list-characters/list-undo command: a session
-- never has to ask for any of these, only listen. A second thread keeps all
-- three live: 'Server.Writer.GitWorker' already broadcasts a 'RefMoved' on
-- 'envNotifyChan' for every branch ref creation/update, from any connection
-- (including 'undo.reset''s own ref restores) — the notifier re-pushes
-- 'BranchList' on every such move, 'CharacterList' too when the moved ref is
-- a 'character/*' one, and 'UndoLog' unconditionally, since every tracked
-- write also grows the undo log (see 'Storyteller.Core.Undo'). No new
-- plumbing needed underneath any of it.
module Server.Writer.Session.Connection
  ( runSession
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, atomically, dupTChan, readTChan)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)
import Polysemy.Error (catch)

import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Writer.Env (ServerEnv(..))
import Server.Core.Logging (logCommand)
import Server.Core.Run (SessionEffects)
import Server.Writer.Notification (BranchNotification(..))
import Server.Writer.Run (actionStack)
import Server.Writer.Session.Dispatch (runCommand, characterSummaries, branchNames, undoLog)
import Server.Writer.Session.Protocol

runSession :: ServerEnv -> WS.Connection -> IO ()
runSession env conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env conn notifyChan
  runCommands env conn `finally` killThread notifier

-- | No agent commands run on a session connection (session-level commands
--   are create/delete-branch only) so there's never anything to stream;
--   drop chunks rather than push them anywhere.
runCommands :: ServerEnv -> WS.Connection -> IO ()
runCommands env conn = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $ do
    embed $ WS.sendTextData conn (encode SessionReady')
    pushBranchList conn
    pushCharacterList conn
    pushUndoLog conn
    commandLoop conn
  either (reportError conn) return result

-- | Re-push the branch (and, when relevant, character) list whenever any
--   branch ref moves — covers creation and deletion alike, since both go
--   through 'Storyteller.Core.Storage.createBranch'/'deleteBranch', which
--   (like any other ref write) reaches 'Server.Writer.GitWorker'. Only 'RefMoved' is
--   relevant here; 'TicksRemapped' is about tick-id remapping within a
--   branch's own chain, not the existence of branches, so it's ignored.
runNotifier :: ServerEnv -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $ watchBranchMoves chan (onBranchMove conn)
  either (reportError conn) return result

watchBranchMoves
  :: Member (Embed IO) r
  => TChan BranchNotification -> (T.Text -> Sem r ()) -> Sem r ()
watchBranchMoves chan onMove = loop
  where
    loop = do
      note <- embed $ atomically (readTChan chan)
      case note of
        RefMoved b -> onMove b >> loop
        _          -> loop

onBranchMove :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> T.Text -> Sem r ()
onBranchMove conn b = do
  pushBranchList conn
  if "character/" `T.isPrefixOf` b then pushCharacterList conn else return ()
  -- Every real ref move is exactly when the undo log has grown (it's
  -- appended to on every tracked write, see Storyteller.Core.Undo) — so
  -- this reuses the same broadcast every other session-level list rides on,
  -- no separate notification needed.
  pushUndoLog conn

pushBranchList :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> Sem r ()
pushBranchList conn = do
  names <- branchNames
  embed $ WS.sendTextData conn (encode (BranchList names))

pushCharacterList :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> Sem r ()
pushCharacterList conn = do
  summaries <- characterSummaries
  embed $ WS.sendTextData conn (encode (CharacterList summaries))

pushUndoLog :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> Sem r ()
pushUndoLog conn = do
  evt <- undoLog
  embed $ WS.sendTextData conn (encode evt)

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (SessionError (T.pack err)))

commandLoop
  :: (SessionEffects r, Member (Embed IO) r)
  => WS.Connection -> Sem r ()
commandLoop conn = loop
  where
    loop = do
      msg <- embed (try (WS.receiveData conn) :: IO (Either SomeException LBS.ByteString))
      case msg of
        Left  _   -> return ()
        Right raw -> case decode raw of
          Nothing  -> embed (WS.sendTextData conn (encode (SessionError "invalid message"))) >> loop
          Just cmd -> handle cmd >> loop

    handle cmd =
      catch @String
        (logCommand (commandKind cmd) (runCommand conn cmd))
        (\err -> embed (reportError conn err))
