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
-- 'Storyteller.Core.Git.runStoryBranchGit'), so a scope opened once at connect
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
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed, runM)
import Polysemy.Error (Error, catch)

import Server.Core.Branch (Main, BranchOpen, branchState, branchStateSince)
import Server.Core.Logging (logCommand)
import Server.Writer.Branch.Dispatch (runCommand)
import Server.Writer.Branch.Protocol
import Server.Writer.Env (ServerEnv(..))
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Server.Core.Protocol (Update(..))
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Core.Util (withBranch)
import Storyteller.Common.Splitter (splitMarkdownAware)
import Storyteller.Core.Git (withStorage)
import Storyteller.Core.Types (TickId(..))

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
      >> splitMarkdownAware (commandLoop conn branch)
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move
--   broadcasts for the connection's lifetime. Doesn't hold 'BranchOpen'
--   itself — each 'RefMoved' reopens the branch scope fresh (see
--   'onNotify') to get a live view, rather than relying on one long-held
--   scope to notice writes made elsewhere. Tick remaps aren't forwarded
--   here — nothing at the branch level (unlike a file connection's rebase
--   marker/context selection) tracks a bare tickId across a push yet.
--
--   The accumulator also carries the file set last observed, seeded here
--   from the same 'branchState' read 'pushInitial' makes (on its own
--   thread/scope — see the module comment on why these two threads don't
--   share state directly), so the first 'RefMoved' diffs against the
--   branch's actual starting file list instead of empty and doesn't
--   re-announce every already-known file as newly added.
runNotifier :: ServerEnv -> T.Text -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env branch conn chan = do
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env $ do
    initialFiles <- withBranch @Main branch (fst <$> branchState)
    void $ watchBranch chan branch (Nothing, Set.fromList initialFiles) (onNotify branch conn)
  either (reportError conn) return result

onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> (Maybe T.Text, Set FilePath) -> BranchNotification -> Sem r (Maybe T.Text, Set FilePath)
onNotify branch conn (since, knownFiles) note = case note of
  RefMoved _ _    -> withBranch @Main branch (pushIncremental conn since knownFiles)
  TicksRemapped _ -> return (since, knownFiles)
  UndoMoved       -> return (since, knownFiles)

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
  :: (SessionEffects r, Member (Embed IO) r)
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
        (logCommand (commandKind cmd)
          (withStorage (withBranch @Main branch (runCommand branch cmd)))
          >>= embed . mapM_ (WS.sendTextData conn . encode))
        (\err -> embed (reportError conn err))

-- | Push updated tick state, plus a 'FileAdded'/'FileRemoved' for every path
--   that appeared or disappeared from the working tree since the last push
--   we saw. This is what makes a brand-new (or deleted) path show up live
--   for every other connection already open on this branch, regardless of
--   which connection caused it — a chat.append/file.create/delete on a file
--   connection, an upload, or a Track/CharGen command (whose own dispatch
--   already returns its own 'FileAdded's directly; this is a second,
--   redundant-but-harmless path for those, and the *only* path for
--   everything else).
pushIncremental
  :: (BranchOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> Maybe T.Text -> Set FilePath -> Sem r (Maybe T.Text, Set FilePath)
pushIncremental conn since knownFiles =
  catch @String
    (do
      (files, upd) <- branchStateSince (TickId <$> since)
      let fileSet      = Set.fromList files
          newFiles     = Set.toList (Set.difference fileSet knownFiles)
          removedFiles = Set.toList (Set.difference knownFiles fileSet)
      mapM_ (\f -> embed $ WS.sendTextData conn (encode (FileAdded Nothing f))) newFiles
      mapM_ (\f -> embed $ WS.sendTextData conn (encode (FileRemoved Nothing f))) removedFiles
      embed $ WS.sendTextData conn (encode (BranchUpdate upd))
      return (Just (updateHead upd), fileSet))
    (\err -> embed (reportError conn err) >> return (since, knownFiles))
