{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
-- three live: 'Server.Writer.GitWorker' broadcasts a 'RefMoved' on
-- 'envNotifyChan' for every branch ref creation, update, or deletion
-- (including 'undo.reset''s own ref restores) and an 'UndoMoved' whenever
-- 'Storyteller.Core.Undo's own log grows — the notifier re-pushes
-- 'UndoLog' on the latter, and, on the former, 'BranchList' only when a
-- moved ref's very existence changed (created or deleted -- an ordinary
-- content edit cannot possibly change which branches exist, so there's
-- nothing for that push to ever show that wasn't already sent) and
-- 'CharacterList' whenever any 'character/*' ref moved at all, existence or
-- not (its payload carries live sheet content, which an ordinary edit can
-- genuinely change). Undo log and branch existence are deliberately two
-- independent triggers rather than one push doing both on every move: a
-- reset restores several branches at once (each its own 'RefMoved') without
-- growing the log at all, and conversely a single write's own log entry
-- lands (see 'Storyteller.Core.Undo.recordUndoSnapshot') strictly after the
-- branch write it's recording, so treating 'RefMoved' as a proxy for "the
-- log grew too" would either push stale undo-log data (read before the
-- entry landed) or push extra unchanged copies of it (on a reset) — this
-- reacts to each fact exactly when it's actually true instead.
module Server.Writer.Session.Connection
  ( runSession
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, atomically, dupTChan, readTChan, tryReadTChan)
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
import Server.Writer.Run (actionStack, loggingWS)
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
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env $ do
    embed $ WS.sendTextData conn (encode SessionReady')
    pushBranchList conn
    pushCharacterList conn
    pushUndoLog conn
    commandLoop conn
  either (reportError conn) return result

-- | Re-push the branch list only when some ref's existence actually
--   changed, the character list whenever any 'character/*' ref moved at
--   all, and the undo log on every 'UndoMoved' — see the module haddock for
--   why each reacts to a different, precise subset of what's possible.
runNotifier :: ServerEnv -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env $
    watchNotifications chan (onRefMove conn) (pushUndoLog conn)
  either (reportError conn) return result

-- | One "here's what moved" batch: several branches can move as a single
--   logical action (e.g. 'Storyteller.Core.Undo.resetToUndo' restoring
--   every tracked branch, one 'updateRef'\/'deleteRef' at a time, or a
--   cross-branch rename touching several heads at once), each its own
--   'RefMoved'; a multi-branch write similarly records one
--   'Storyteller.Core.Undo.Snapshot' -- and so one 'UndoMoved' -- per branch
--   it touches. Every notification currently sitting in the channel is
--   drained up front (not just whichever kind happened to wake this
--   thread), so a 'RefMoved' and an 'UndoMoved' arriving in either order
--   within the same burst both still get acted on -- reacting only to the
--   one that triggered the wakeup risked silently dropping whichever kind
--   didn't happen to be first. 'onRefs' folds a whole burst of 'RefMoved'
--   down to the two bits it actually needs (did any existence change, did
--   any character branch move at all); a burst of 'UndoMoved' collapses to
--   nothing but a single 'onUndo' call, since every push reads live state
--   fresh regardless of how many entries just landed.
watchNotifications
  :: Member (Embed IO) r
  => TChan BranchNotification -> (Bool -> Bool -> Sem r ()) -> Sem r () -> Sem r ()
watchNotifications chan onRefs onUndo = loop
  where
    loop = do
      first <- embed $ atomically (readTChan chan)
      rest  <- embed (drainAll chan)
      let notes            = first : rest
          moves             = [ (b, existed) | RefMoved b existed <- notes ]
          branchesChanged   = any snd moves
          characterTouched  = any (isCharacterRef . fst) moves
      if null moves then return () else onRefs branchesChanged characterTouched
      if UndoMoved `elem` notes then onUndo else return ()
      loop

    -- Best-effort: only what's immediately available without waiting, so a
    -- genuinely solitary notification isn't delayed at all. A multi-ref
    -- action submits its writes in one tight loop with no yield in
    -- between, so by the time this thread wakes for the first one, the
    -- rest have essentially always already landed.
    drainAll :: TChan BranchNotification -> IO [BranchNotification]
    drainAll ch = atomically (tryReadTChan ch) >>= \case
      Just n  -> (n :) <$> drainAll ch
      Nothing -> return []

    isCharacterRef = T.isPrefixOf "character/"

onRefMove :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> Bool -> Bool -> Sem r ()
onRefMove conn branchesChanged characterTouched = do
  if branchesChanged  then pushBranchList conn    else return ()
  if characterTouched then pushCharacterList conn else return ()

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
