{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | /branch/{id}/{path} connection lifecycle.
--
-- @{id}@ is either a plain branch name, or @name\@kind@ -- a summary
-- tier's own alternate chain (see "Storyteller.Common.Summary"'s module
-- Haddock: a real commit chain, just with no ref of its own). 'openTarget'
-- is the *only* place the two are told apart: a plain name opens the
-- branch's own real head the ordinary way, moving it via 'setRef' on
-- write; @name\@kind@ opens @kind@'s current 'Summary' tick's own altHead
-- instead, and records an advanced head as a fresh 'Summary' tick on
-- @name@ (see 'Storyteller.Writer.Agent.Summarizer.runSummarizer's own
-- Haddock -- a hand-edit and a regenerated pass are indistinguishable to
-- every reader once this returns, both are just "a new 'Summary' tick").
-- Every read ('Server.Writer.File.fileStateWithSummaries'), every command
-- ('Server.Writer.File.Dispatch.runCommand'), and every event type
-- ('Server.Writer.File.Protocol') downstream is identical either way --
-- summary content is a *file*, not a special read-only projection of one.
--
-- On connect: enter the target's storage/filesystem scope once to push the
-- initial FilePresent/FileAbsent + FileUpdate, then loop receiving commands
-- via 'embed'. Each command reopens the scope itself, nested inside
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
-- 'Storyteller.Core.Git.runStoryBranchGit'), so a scope opened once at connect
-- would never notice anything written afterwards. Each notification
-- reopens the scope fresh instead (see 'onNotify') — the same "sync
-- happens at open" rule 'commandLoop' follows. Because each thread owns
-- its own stack, 'lastHead' is a plain recursive-loop accumulator inside
-- that thread's loop — no shared mutable state between the two stacks, no
-- possibility of their pushes racing.
--
-- Notifications are always watched on the *real* branch name (everything
-- a summary tier's own edits land on is a fresh tick on that same real
-- branch too -- see 'openTarget'), so a summary connection sees every
-- other write on the branch just as readily as the real file connection
-- does, with no separate broadcast channel needed.
module Server.Writer.File.Connection
  ( runFile
  , openTarget
  , targetBranch
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, newTVarIO, writeTVar)
import Control.Exception (SomeException, try, finally)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Members, Sem, embed, runM)
import Polysemy.Error (Error, catch)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import qualified Storage.Core as Core
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary (Summary(..), bootstrapAltHead, lastSummaryOf)
import Server.Writer.Env (ServerEnv(..), registerCancel, unregisterCancel)
import Server.Core.File (FileOpen)
import Server.Core.Logging (logCommand)
import Server.Writer.File (fileStateWithSummaries)
import Server.Writer.File.Dispatch (runCommand)
import Server.Writer.File.Protocol
import Server.Writer.Notification (BranchNotification(..), watchBranch)
import Server.Core.Protocol (Update(..))
import Runix.LLM.Streaming (StreamEvent)
import Runix.StreamChunk (ignoreChunks)
import Server.Core.Run (SessionEffects)
import Server.Writer.Run (actionStack, wsAction, loggingWS)
import Server.Core.Util (withBranch)
import Storyteller.Common.Splitter (Splitter, splitMarkdownAware)
import Storyteller.Core.Git (BranchOp, BranchTag, atGeneric, runBranchAndFSFrom, runStorage, withStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..), TickId(..))

runFile :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> IO ()
runFile env target path conn = do
  notifyChan <- atomically $ dupTChan (envNotifyChan env)
  cancelFlag <- newTVarIO False
  notifier   <- forkIO $ runNotifier env target path conn notifyChan
  runCommands env target path conn cancelFlag `finally` killThread notifier

-- | Open @target@'s scope -- see this module's own Haddock for the two
--   forms @target@ can take. The real branch a write ultimately always
--   lands on either way; 'runNotifier' watches that, never @target@
--   itself (a summary tier's own @name\@kind@ string is never a thing
--   ref-move notifications are keyed by).
targetBranch :: T.Text -> T.Text
targetBranch = fst . T.breakOn "@"

openTarget
  :: forall r a
  .  Members '[StoryStorage, Error String, Git, Fail] r
  => T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : r ) a
  -> Sem r a
openTarget target action = case T.breakOn "@" target of
  (branch, "") -> withBranch @Main branch action
  (branch, kindWithAt) -> do
    let kind = T.drop 1 kindWithAt
    mLast <- withBranch @Main branch (runStorage @Main (lastSummaryOf kind))
    -- No 'Summary' tick yet for this kind on this branch -- rather than
    -- refuse the connection, seed from the same fixed, parentless,
    -- empty-tree commit 'Storyteller.Writer.Agent.Summarizer.runSummarizer'\/
    -- 'Storyteller.Writer.Agent.JournalSummarizer.journalSummarize' fall
    -- back to on *their* own first pass ('bootstrapAltHead'). This tier
    -- simply doesn't exist yet until the first write actually lands
    -- ('onNotify' below then mints its first real 'Summary' tick, the
    -- same way any later edit does) -- a manually-created summary and an
    -- LLM-generated one are indistinguishable from that point on, exactly
    -- like a hand-edit of an existing one already is.
    seed <- case mLast of
      Just (_, s) -> return (Core.ObjectHash (unTickId (summaryAltHead s)))
      Nothing     -> bootstrapAltHead
    runBranchAndFSFrom @Main
      (BranchName target)
      seed
      (\newHead -> void (withBranch @Main branch (mintSummaryTick kind mLast newHead)))
      action
  where
    -- | Record @newHead@ as @kind@'s new 'Summary' tick. Never simply
    --   appended at wherever the branch's own head happens to sit *now*
    --   -- an edit made through this tier only ever accounted for content
    --   as of whichever tick it navigated from ('mLast'), never anything
    --   the real branch picked up afterward (another connection's own
    --   raw prose, an unrelated note, ...). Appending at current head
    --   would silently claim coverage of all of that: the next
    --   'Storyteller.Common.Summary.ticksSinceLastSummary' walk would
    --   find nothing new, and 'Storyteller.Writer.Agent.SummaryAccess.
    --   unsummarizedTailSince' would drop it from every reader's view
    --   entirely, not just this one's -- a real, silent loss of content
    --   from context assembly, not merely a display quirk.
    --
    --   'Storyteller.Core.Git.atGeneric' is exactly the fix already used
    --   everywhere else in this codebase for "insert a tick at a
    --   historical position, replay everything since on top of it": wind
    --   back to @mLast@'s own tick, record the new 'Summary' there, then
    --   replay whatever landed on the branch afterward back onto that --
    --   preserving it as still-unsummarized, exactly as it should read.
    --   A first-ever tier (@mLast@ 'Nothing') has no earlier position to
    --   preserve anything relative to, so it just appends normally.
    mintSummaryTick
      :: Members '[BranchOp Main, StoryStorage, Git, Fail] r'
      => T.Text -> Maybe (TickId, Summary) -> TickId -> Sem r' ()
    mintSummaryTick kind mLast newHead = void $ case mLast of
      Nothing            -> runStorage @Main (Tick.storeAs (Summary kind newHead))
      Just (oldTickId, _) -> atGeneric @Main oldTickId (runStorage @Main (Tick.storeAs (Summary kind newHead)))

-- | The command-loop thread's persistent stack: enter the target's scope
--   once, push the initial file state, then dispatch commands until the
--   socket closes. 'cancelFlag' is this connection's one long-lived
--   cancel flag — reset before each command runs and briefly published
--   under that command's own id (see 'handle') so a \/session 'cancel'
--   can reach it.
runCommands :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> TVar Bool -> IO ()
runCommands env target path conn cancelFlag = do
  result <- runM $ wsAction env conn cancelFlag $
    openTarget target (pushInitial conn path)
      >> splitMarkdownAware (commandLoop env target conn path cancelFlag)
  either (reportError conn) return result

-- | The notify-listener thread's persistent stack: react to ref-move and
--   tick-remap broadcasts for the connection's lifetime. Doesn't hold
--   'FileOpen' itself — each 'RefMoved' reopens the target's scope fresh
--   (see 'onNotify') to get a live view, rather than relying on one
--   long-held scope to notice writes made elsewhere. Watches the *real*
--   branch (see 'targetBranch'), not @target@ itself.
runNotifier :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> TChan BranchNotification -> IO ()
runNotifier env target path conn chan = do
  -- Never runs an LLM-backed command, so there is nothing to cancel — a
  -- fresh, unshared 'TVar Bool' just satisfies 'actionStack's signature.
  cancelFlag <- newTVarIO False
  result <- runM $ ignoreChunks @StreamEvent $ loggingWS conn $ actionStack env cancelFlag $
    void $ watchBranch chan (targetBranch target) Nothing (onNotify target conn path)
  either (reportError conn) return result

reportError :: WS.Connection -> String -> IO ()
reportError conn err = WS.sendTextData conn (encode (FileError (T.pack err)))

-- | Dispatch a notification to the right push: a ref move means this file's
--   ticks (or, via 'fileStateWithSummaries', its summary tiers) may have
--   changed, so reopen the target's scope (a fresh sync point) and diff
--   since the last push; a tick remap carries its own payload straight
--   through — see 'FileEvent.TickRemap'.
onNotify
  :: (SessionEffects r, Member (Embed IO) r)
  => T.Text -> WS.Connection -> FilePath -> Maybe (T.Text, T.Text) -> BranchNotification -> Sem r (Maybe (T.Text, T.Text))
onNotify target conn path since note = case note of
  RefMoved _ _ ->
    openTarget target (pushIncremental conn path since)
  TicksRemapped mapping -> do
    embed $ WS.sendTextData conn (encode (TickRemap mapping))
    return since
  UndoMoved -> return since

-- | Push present/absent plus the initial update, mirroring the shape used
--   throughout: presence is just "does this file have any ticks yet"
--   (summary tiers riding along via 'fileStateWithSummaries' never affect
--   that -- a file with only a stale summary ghost and no real atoms left
--   is still absent, see 'pushIncremental's own presence handling).
pushInitial :: (FileOpen r, Member (Embed IO) r) => WS.Connection -> FilePath -> Sem r ()
pushInitial conn path = do
  (upd, _sig) <- fileStateWithSummaries path Nothing
  if T.null (updateHead upd)
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
  :: (Member Splitter r, SessionEffects r, Member (Embed IO) r)
  => ServerEnv -> T.Text -> WS.Connection -> FilePath -> TVar Bool -> Sem r ()
commandLoop env target conn path cancelFlag = loop
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
    --
    -- 'cancelFlag' is reset before the command starts, and — if it carries
    -- an id — briefly registered under that id (see 'Server.Writer.Env')
    -- so a 'cancel' sent on \/session while this command is streaming can
    -- reach it; unregistered again once the command finishes either way.
    handle cmd = do
      embed $ atomically $ writeTVar cancelFlag False
      embed $ mapM_ (\cid -> registerCancel env cid cancelFlag) (fcId cmd)
      catch @String
        (logCommand (commandKind cmd) (withStorage (openTarget target (runCommand path cmd)))
          >>= embed . mapM_ (WS.sendTextData conn . encode))
        (\err -> embed (reportError conn err))
      embed $ mapM_ (unregisterCancel env) (fcId cmd)

-- 'since = Nothing' means we're still in the absent state from connect —
-- mirror 'pushInitial' exactly, so it transitions to present the moment
-- the file gets its first tick. 'since = Just (tid, sig)' means we already
-- have a HEAD (plus a summary signature — see 'fileStateWithSummaries') to
-- diff against; skip the push entirely if this write touched neither this
-- file's own chain nor its summary tiers. A HEAD that moved to "" (every
-- atom rebased away, e.g. a whole-file delete) is a real transition back
-- to absent, not "nothing new since last push" — 'updateHead' (never
-- affected by summary extras, see 'fileStateWithSummaries') is what's
-- checked for that, not 'updateTicks', which a stale summary ghost could
-- leave non-empty even once every real atom is gone.
pushIncremental
  :: (FileOpen r, Member (Embed IO) r, Member (Error String) r)
  => WS.Connection -> FilePath -> Maybe (T.Text, T.Text) -> Sem r (Maybe (T.Text, T.Text))
pushIncremental conn path since =
  catch @String
    (do
      (upd, sig) <- fileStateWithSummaries path (fst <$> since)
      case since of
        Nothing | T.null (updateHead upd) -> return since
                | otherwise -> do
                    embed $ WS.sendTextData conn (encode (FilePresent Nothing))
                    embed $ WS.sendTextData conn (encode (FileUpdate upd))
                    return (Just (updateHead upd, sig))
        Just (knownHead, knownSig)
          | updateHead upd == knownHead && sig == knownSig -> return since
          | T.null (updateHead upd) -> do
              embed $ WS.sendTextData conn (encode (FileAbsent Nothing))
              return Nothing
          | otherwise -> do
              embed $ WS.sendTextData conn (encode (FileUpdate upd))
              return (Just (updateHead upd, sig))
    )
    (\err -> embed (reportError conn err) >> return since)
