{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

-- | A single git-storage worker thread for the whole server process,
-- replacing one interpreter stack (formerly one @git cat-file --batch@
-- reader, now one open libgit2 repository handle) per connection with one
-- shared thread every connection and HTTP request submits jobs to. See
-- PLAN-git-storage-worker.md for the full design and why this belongs
-- here rather than in @gitlib-effect@.
--
-- 'startGitWorker' opens the one repository handle this process will ever
-- use, forks the worker loop, and links it to its caller so a worker crash
-- (a bug, not an ordinary 'Fail') takes the whole server down instead of
-- silently wedging git access -- see the module-level design doc for why
-- that's the right failure mode here.
--
-- Uses 'Runix.Git.runGitFFI' (libgit2), not the CLI interpreter this once
-- ran through as a live A/B check: the FFI interpreter measured slower in
-- production despite winning every isolated gitlib-effect-ffi-bench
-- throughput number, traced to libgit2's default
-- @GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION@ paying a redundant SHA1
-- re-check on every object read (5-8x read-throughput loss on
-- realistically-sized blobs -- see gitlib-effect-stricthash-bench and
-- 'Runix.Git.FFI.libgit2Init's own doc for why disabling it is safe for
-- this content-addressed store). With that off, FFI wins outright.
module Server.Writer.GitWorker
  ( GitWorkerQueue
  , startGitWorker
  , runGitViaWorker
  ) where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  (TChan, TQueue, atomically, newTQueueIO, readTQueue, writeTChan, writeTQueue)
import Control.Monad (forever, void)
import Polysemy (Embed, Member, Members, Sem, embed, interpret, runM)
import Polysemy.Error (Error, catch, runError)
import Polysemy.Fail (Fail, failToError)
import Runix.Git
  ( Git(..)
  , RefName
  , createRef
  , deleteRef
  , isAncestorOfAny
  , listRefs
  , lookupPath
  , readCommit
  , readObject
  , resolveRef
  , runGitFFI
  , updateRef
  , withGitCache
  , writeCommit
  , writeObject
  )
import qualified Runix.Git.FFI as FFI

import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.Git (refBranchName)
import Storyteller.Core.Types (unBranchName)
import Storyteller.Core.Undo (undoLogRef)

-- | One request for the worker: a 'Git' operation (inert data -- its @m@
-- parameter is phantom, see 'toIOGitOp') and where to deliver the answer.
data GitJob = forall a. GitJob (Git IO a) (MVar (Either String a))

newtype GitWorkerQueue = GitWorkerQueue (TQueue GitJob)

-- | Start the worker: open the one libgit2 repository handle this
-- process will ever use, then fork the loop that owns it for the rest of
-- the process's life. Linked to the calling thread so an unexpected crash
-- propagates rather than silently stopping git access for the whole
-- server.
startGitWorker :: FilePath -> TChan BranchNotification -> IO GitWorkerQueue
startGitWorker repo notifyChan = do
  queue <- newTQueueIO
  repoHandle <- FFI.openRepository FFI.defaultFFIOptions repo
  worker <- async (gitWorkerLoop repoHandle notifyChan queue)
  link worker
  return (GitWorkerQueue queue)

-- | The whole worker lifetime is one Polysemy interpretation, not one per
-- job: 'withGitCache' and 'runGitFFI' are applied once, outside 'forever',
-- so the content-object cache they install is shared and kept warm across
-- every job from every client for as long as the process runs -- the same
-- objects/commits get re-requested constantly (tick chains overlap across
-- branches), and they're immutable by hash, so this is a strict win with
-- no staleness risk. A per-job cache (the old shape, one fresh
-- interpreter stack per 'submitGitJob' call) would reset on every single
-- request and never see a hit.
--
-- 'Fail' (what 'runGitFFI' itself raises internally, e.g. "object not
-- found") is bridged to 'Error String' via 'failToError' instead of being
-- discharged with a fresh 'Polysemy.Fail.runFail' per job -- 'runFail'
-- fully consumes the effect, which only works once per interpreter
-- lifetime; 'Polysemy.Error.catch' recovers from it locally, per
-- iteration, without discharging 'Error' from the row, so the outer
-- interpreters ('withGitCache' included) never need to be re-applied.
gitWorkerLoop :: FFI.RepoHandle -> TChan BranchNotification -> TQueue GitJob -> IO ()
gitWorkerLoop repoHandle notifyChan queue =
  void
  . runM
  . runError @String
  . failToError id
  . runGitFFI repoHandle
  . withGitCache
  $ forever handleOneJob
  where
    handleOneJob :: Members '[Git, Error String, Embed IO] r => Sem r ()
    handleOneJob = do
      GitJob op replyVar <- embed (atomically (readTQueue queue))
      result <- fmap Right (dispatchGitOp op) `catch` (return . Left)
      embed (putMVar replyVar result)
      embed (notifyOnRefMove op result)

    -- 'True'/'False' here is "did this write change the ref's *existence*"
    -- (create or delete) vs. "just moved an already-existing ref to a new
    -- target" (an ordinary content edit) -- see 'RefMoved's own haddock for
    -- why that distinction matters to the one consumer that cares.
    -- 'DeleteRef' now notifies too, not just 'CreateRef'\/'UpdateRef': a
    -- deleted branch's disappearance is exactly as much an existence change
    -- as its creation, and skipping it here meant no connection but the one
    -- that issued the delete itself ever found out.
    notifyOnRefMove :: Git IO a -> Either String a -> IO ()
    notifyOnRefMove (CreateRef ref _) (Right _) = notifyRef ref True
    notifyOnRefMove (UpdateRef ref _) (Right _) = notifyRef ref False
    notifyOnRefMove (DeleteRef ref)   (Right _) = notifyRef ref True
    notifyOnRefMove _                 _         = return ()

    -- The undo log's own ref is checked first and specifically -- it isn't
    -- a story branch ref at all, so 'refBranchName' would just say
    -- 'Nothing' and this write would go unnotified (as it silently did
    -- before 'UndoMoved' existed), leaving the session connection unable to
    -- ever independently discover a write that its own preceding branch
    -- 'RefMoved' already raced past -- 'Snapshot' always runs *after* the
    -- write it's recording, so by the time this fires, the branch's own
    -- notification (and any push it triggered) is already old news.
    notifyRef :: RefName -> Bool -> IO ()
    notifyRef ref existenceChanged
      | ref == undoLogRef = atomically $ writeTChan notifyChan UndoMoved
      | otherwise = case refBranchName ref of
          Nothing     -> return ()
          Just branch -> atomically $ writeTChan notifyChan (RefMoved (unBranchName branch) existenceChanged)

-- | Replay a 'Git' request through the 'Git' effect. Valid for any @m@ --
-- none of 'Git's constructors mention it, so it's a phantom parameter, not
-- something that needs coercing.
dispatchGitOp :: Member Git r => Git m a -> Sem r a
dispatchGitOp = \case
  ResolveRef  ref          -> resolveRef ref
  CreateRef   ref hash     -> createRef  ref hash
  UpdateRef   ref hash     -> updateRef  ref hash
  DeleteRef   ref          -> deleteRef  ref
  ListRefs    prefix       -> listRefs   prefix
  ReadCommit  hash         -> readCommit hash
  WriteCommit cd           -> writeCommit cd
  ReadObject  hash         -> readObject hash
  WriteObject obj          -> writeObject obj
  LookupPath  tree path    -> lookupPath tree path
  IsAncestorOfAny targets hash -> isAncestorOfAny targets hash

-- | Same trick in reverse: a 'Git' request captured from a client's own
-- 'Sem' stack (@m ~ Sem r0@) is inert data regardless of @m@, so it can be
-- re-tagged to the @IO@-flavoured 'GitJob' the worker expects with no
-- unsafe coercion -- just pattern match and rebuild.
toIOGitOp :: Git m a -> Git IO a
toIOGitOp = \case
  ResolveRef  ref          -> ResolveRef  ref
  CreateRef   ref hash     -> CreateRef   ref hash
  UpdateRef   ref hash     -> UpdateRef   ref hash
  DeleteRef   ref          -> DeleteRef   ref
  ListRefs    prefix       -> ListRefs    prefix
  ReadCommit  hash         -> ReadCommit  hash
  WriteCommit cd           -> WriteCommit cd
  ReadObject  hash         -> ReadObject  hash
  WriteObject obj          -> WriteObject obj
  LookupPath  tree path    -> LookupPath  tree path
  IsAncestorOfAny targets hash -> IsAncestorOfAny targets hash

submitGitJob :: GitWorkerQueue -> Git IO a -> IO (Either String a)
submitGitJob (GitWorkerQueue queue) op = do
  replyVar <- newEmptyMVar
  atomically $ writeTQueue queue (GitJob op replyVar)
  takeMVar replyVar

-- | Client-side 'Git' interpreter: satisfies the same effect/laws as
-- 'Runix.Git.runGitIOPerCall' does today, just by handing the request to
-- the shared worker instead of opening its own interpreter stack. Callers
-- ('Storyteller.Core.Git', every agent, every handler) stay unaware this
-- exists.
runGitViaWorker :: Members '[Fail, Embed IO] r => GitWorkerQueue -> Sem (Git : r) a -> Sem r a
runGitViaWorker queue = interpret $ \gitOp -> do
  reply <- embed (submitGitJob queue (toIOGitOp gitOp))
  either fail return reply
