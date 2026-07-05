{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | A single git-storage worker thread for the whole server process,
-- replacing one interpreter stack (and one @git cat-file --batch@ reader)
-- per connection with one shared thread every connection and HTTP request
-- submits jobs to. See PLAN-git-storage-worker.md for the full design and
-- why this belongs here rather than in @gitlib-effect@.
--
-- 'startGitWorker' opens the one 'Batch.BatchReader' this process will ever
-- use, forks the worker loop, and links it to its caller so a worker crash
-- (a bug, not an ordinary 'Fail') takes the whole server down instead of
-- silently wedging git access -- see the module-level design doc for why
-- that's the right failure mode here.
module Server.Writer.GitWorker
  ( GitWorkerQueue
  , startGitWorker
  , runGitViaWorker
  ) where

import Control.Concurrent.Async (async, link)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  (TChan, TQueue, atomically, newTQueueIO, readTQueue, writeTChan, writeTQueue)
import Control.Monad (forever)
import Polysemy (Embed, Member, Members, Sem, embed, interpret, runM)
import Polysemy.Fail (Fail, runFail)
import Runix.Cmd (cmdsIO, interpretCmd)
import Runix.Git
  ( Git(..)
  , RefName
  , createRef
  , deleteRef
  , listRefs
  , lookupPath
  , readCommit
  , readObject
  , resolveRef
  , runGitIO
  , updateRef
  , writeCommit
  , writeObject
  )
import qualified Runix.Git.Batch as Batch

import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.Git (refBranchName)
import Storyteller.Core.Types (unBranchName)

-- | One request for the worker: a 'Git' operation (inert data -- its @m@
-- parameter is phantom, see 'toIOGitOp') and where to deliver the answer.
data GitJob = forall a. GitJob (Git IO a) (MVar (Either String a))

newtype GitWorkerQueue = GitWorkerQueue (TQueue GitJob)

-- | Start the worker: open the one 'Batch.BatchReader' this process will
-- ever use, then fork the loop that owns it for the rest of the process's
-- life. Linked to the calling thread so an unexpected crash propagates
-- rather than silently stopping git access for the whole server.
startGitWorker :: FilePath -> TChan BranchNotification -> IO GitWorkerQueue
startGitWorker repo notifyChan = do
  queue <- newTQueueIO
  br <- Batch.openBatchReader repo
  worker <- async (gitWorkerLoop repo br notifyChan queue)
  link worker
  return (GitWorkerQueue queue)

gitWorkerLoop :: FilePath -> Batch.BatchReader -> TChan BranchNotification -> TQueue GitJob -> IO ()
gitWorkerLoop repo br notifyChan queue = forever $ do
  GitJob op replyVar <- atomically (readTQueue queue)
  result <- runM . runFail . cmdsIO . interpretCmd @"git" . runGitIO repo (\k -> k br) $
    dispatchGitOp op
  putMVar replyVar result
  notifyOnRefMove op result
  where
    notifyOnRefMove :: Git IO a -> Either String a -> IO ()
    notifyOnRefMove (CreateRef ref _) (Right _) = notifyRef ref
    notifyOnRefMove (UpdateRef ref _) (Right _) = notifyRef ref
    notifyOnRefMove _                 _         = return ()

    notifyRef :: RefName -> IO ()
    notifyRef ref = case refBranchName ref of
      Nothing     -> return ()
      Just branch -> atomically $ writeTChan notifyChan (RefMoved (unBranchName branch))

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
