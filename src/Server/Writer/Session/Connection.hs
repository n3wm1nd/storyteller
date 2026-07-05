{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | /session connection lifecycle.
--
-- On connect: enter the storage-level scope exactly once for the
-- connection's whole lifetime (no branch open — a session has no one
-- branch's tick state to snapshot), push SessionReady, then loop receiving
-- commands via 'embed'. A command that fails is caught locally with
-- 'Polysemy.Error.catch' and reported as a SessionError without unwinding
-- the stack or ending the connection.
--
-- A second thread tracks the character list live, the same shape as
-- 'Server.Writer.Branch.Connection's notifier: 'gitNotify' (see
-- 'Server.Writer.Run') already broadcasts a 'RefMoved' on
-- 'envNotifyChan' for every branch ref creation/update, from any
-- connection — a session simply filters that generic, already-existing
-- broadcast to the 'character/' prefix instead of one exact branch name,
-- so live character-list tracking needs no new plumbing underneath it.
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
import Server.Writer.Session.Dispatch (runCommand, characterSummaries)
import Server.Writer.Session.Protocol

runSession :: ServerEnv -> WS.Connection -> IO ()
runSession env conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  notifier   <- forkIO $ runNotifier env conn notifyChan
  runCommands env conn `finally` killThread notifier

-- | No agent commands run on a session connection (session-level commands
--   are list/create/delete-branch only) so there's never anything to
--   stream; drop chunks rather than push them anywhere.
runCommands :: ServerEnv -> WS.Connection -> IO ()
runCommands env conn = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $ do
    embed $ WS.sendTextData conn (encode SessionReady')
    commandLoop conn
  either (reportError conn) return result

-- | Re-push the character list whenever a 'character/*' branch ref moves —
--   covers creation and deletion alike, since both go through
--   'Storyteller.Core.Storage.createBranch'/'deleteBranch', which (like any
--   other ref write) reaches 'gitNotify'. Only 'RefMoved' is relevant here;
--   'TicksRemapped' is about tick-id remapping within a branch's own chain,
--   not the existence of branches, so it's ignored.
runNotifier :: ServerEnv -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ actionStack env $ watchCharacterBranches chan (pushCharacterList conn)
  either (reportError conn) return result

watchCharacterBranches
  :: Member (Embed IO) r
  => TChan BranchNotification -> Sem r () -> Sem r ()
watchCharacterBranches chan onMatch = loop
  where
    loop = do
      note <- embed $ atomically (readTChan chan)
      case note of
        RefMoved b | "character/" `T.isPrefixOf` b -> onMatch >> loop
        _                                           -> loop

pushCharacterList :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> Sem r ()
pushCharacterList conn = do
  summaries <- characterSummaries
  embed $ WS.sendTextData conn (encode (CharacterList Nothing summaries))

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
