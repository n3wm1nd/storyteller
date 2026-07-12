{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Integration check for 'Server.Writer.GitWorker': the one thing that
-- matters here is queue-sharing behaviour a mock 'Git' can't exercise --
-- real correctness of 'Git' itself is already covered by @gitlib-effect-test@.
--
-- Four things to pin down (see PLAN-git-storage-worker.md's step 5): a
-- second client submitting to the same 'GitWorkerQueue' sees a ref a first
-- client created (the whole point of centralizing on one worker), one
-- job's ordinary 'Fail' doesn't wedge or poison the worker loop for
-- whatever is submitted next, every ref write posts the right notification
-- -- a story branch ref gets 'RefMoved', but 'Storyteller.Core.Undo''s own
-- log ref gets 'UndoMoved' instead (not 'RefMoved', and not silence -- see
-- 'Server.Writer.Session.Connection's notifier, which relies on this to
-- know precisely when the undo log itself grew, independent of whichever
-- branch ref write the entry is recording) -- and 'RefMoved's own 'Bool'
-- correctly distinguishes a branch actually appearing/disappearing
-- ('createRef'\/'deleteRef') from an existing one's head just moving
-- ('updateRef'), since 'Server.Writer.Session.Connection' relies on that
-- too, to skip re-pushing the branch list on writes that can't have
-- changed it.
module Server.Writer.GitWorkerSpec (spec) where

import Control.Concurrent.STM (TChan, newBroadcastTChanIO, atomically, dupTChan, readTChan)
import Control.Exception (bracket)
import Polysemy (Embed, Sem, runM)
import Polysemy.Fail (Fail, runFail)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import System.Timeout (timeout)
import Test.Hspec

import Runix.Git
import Server.Writer.GitWorker (GitWorkerQueue, runGitViaWorker, startGitWorker, stopGitWorker)
import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.Git (storyRefPrefix)
import Storyteller.Core.Undo (undoLogRef)

withTempRepo :: (FilePath -> IO a) -> IO a
withTempRepo action = withSystemTempDirectory "git-worker-spec" $ \dir -> do
  callProcess "git" ["-C", dir, "init", "-q"]
  action dir

-- | The worker posts a notification asynchronously, strictly after
-- replying to the job that triggered it (see 'Server.Writer.GitWorker's
-- 'handleOneJob') -- a real consumer ('Server.Writer.Session.Connection's
-- notifier) just blocks on 'readTChan' forever, so that ordering is
-- invisible to it. A non-blocking read right after 'runViaWorker' returns
-- would race that same gap and flake; block with a generous timeout
-- instead so the wait is for the notification, not the scheduler.
expectNotify :: TChan BranchNotification -> IO (Maybe BranchNotification)
expectNotify reader = timeout 1000000 (atomically (readTChan reader))

-- | One "client": its own fresh 'Fail'/'Embed IO' stack, exactly the shape
-- 'runGitViaWorker' is used at in the real server -- but every call here
-- shares the one 'GitWorkerQueue' passed in, same as every real connection
-- shares 'Server.Writer.Env.envGitWorker'.
runViaWorker :: GitWorkerQueue -> Sem '[Git, Fail, Embed IO] a -> IO (Either String a)
runViaWorker queue = runM . runFail . runGitViaWorker queue

-- | Every 'it' below starts its own worker, unlike the real server's one
-- process-lifetime instance -- so, unlike the real server, each one must be
-- torn down explicitly. Left running, a worker's queue goes unreachable
-- the moment its 'it' block returns, and the GC eventually throws
-- 'BlockedIndefinitelyOnSTM' into the (still-'link'ed) worker thread at
-- some later, unpredictable point -- surfacing as a spurious failure in
-- whatever unrelated test happens to be running when the GC gets to it.
withWorker :: FilePath -> TChan BranchNotification -> (GitWorkerQueue -> IO a) -> IO a
withWorker repo notifyChan = bracket (startGitWorker repo notifyChan) stopGitWorker

spec :: Spec
spec = around withTempRepo $ describe "runGitViaWorker" $ do

  it "lets one client see a ref another client created through the same shared queue" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    withWorker repo notifyChan $ \queue -> do
      written <- runViaWorker queue $ do
        h      <- writeBlob "hello"
        tree   <- writeTree [BlobEntry "a.md" h]
        commit <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "first" }
        createRef (RefName "refs/heads/test") commit
        return commit
      seen <- runViaWorker queue $ resolveRef (RefName "refs/heads/test")
      seen `shouldBe` fmap Just written

  it "a failing job doesn't affect a job submitted afterwards on the same queue" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    withWorker repo notifyChan $ \queue -> do
      failing <- runViaWorker queue $ readBlob (ObjectHash "0000000000000000000000000000000000000000")
      failing `shouldSatisfy` \case
        Left _ -> True
        Right _ -> False
      ok <- runViaWorker queue $ writeBlob "still works after a failed job"
      ok `shouldSatisfy` \case
        Right _ -> True
        Left _ -> False

  it "posts RefMoved for a story branch ref, but UndoMoved (not RefMoved) for the undo log's own ref" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    withWorker repo notifyChan $ \queue -> do
      reader <- atomically (dupTChan notifyChan)

      _ <- runViaWorker queue $ do
        h <- writeBlob "hello"
        createRef (RefName (storyRefPrefix <> "main")) h
      branchNote <- expectNotify reader
      branchNote `shouldBe` Just (RefMoved "main" True)

      _ <- runViaWorker queue $ do
        h <- writeBlob "an undo-log entry commit"
        createRef undoLogRef h
      undoNote <- expectNotify reader
      undoNote `shouldBe` Just UndoMoved

  it "RefMoved's existence flag is True for createRef/deleteRef but False for an ordinary updateRef" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    withWorker repo notifyChan $ \queue -> do
      reader <- atomically (dupTChan notifyChan)
      let ref = RefName (storyRefPrefix <> "main")

      _ <- runViaWorker queue $ writeBlob "hello" >>= createRef ref
      created <- expectNotify reader
      created `shouldBe` Just (RefMoved "main" True)

      _ <- runViaWorker queue $ writeBlob "hello again" >>= updateRef ref
      moved <- expectNotify reader
      moved `shouldBe` Just (RefMoved "main" False)

      _ <- runViaWorker queue $ deleteRef ref
      deleted <- expectNotify reader
      deleted `shouldBe` Just (RefMoved "main" True)
