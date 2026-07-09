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

import Control.Concurrent.STM (newBroadcastTChanIO, atomically, dupTChan, tryReadTChan)
import Polysemy (Embed, Sem, runM)
import Polysemy.Fail (Fail, runFail)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

import Runix.Git
import Server.Writer.GitWorker (GitWorkerQueue, runGitViaWorker, startGitWorker)
import Server.Writer.Notification (BranchNotification(..))
import Storyteller.Core.Git (storyRefPrefix)
import Storyteller.Core.Undo (undoLogRef)

withTempRepo :: (FilePath -> IO a) -> IO a
withTempRepo action = withSystemTempDirectory "git-worker-spec" $ \dir -> do
  callProcess "git" ["-C", dir, "init", "-q"]
  action dir

-- | One "client": its own fresh 'Fail'/'Embed IO' stack, exactly the shape
-- 'runGitViaWorker' is used at in the real server -- but every call here
-- shares the one 'GitWorkerQueue' passed in, same as every real connection
-- shares 'Server.Writer.Env.envGitWorker'.
runViaWorker :: GitWorkerQueue -> Sem '[Git, Fail, Embed IO] a -> IO (Either String a)
runViaWorker queue = runM . runFail . runGitViaWorker queue

spec :: Spec
spec = around withTempRepo $ describe "runGitViaWorker" $ do

  it "lets one client see a ref another client created through the same shared queue" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    queue <- startGitWorker repo notifyChan
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
    queue <- startGitWorker repo notifyChan
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
    queue <- startGitWorker repo notifyChan
    reader <- atomically (dupTChan notifyChan)

    _ <- runViaWorker queue $ do
      h <- writeBlob "hello"
      createRef (RefName (storyRefPrefix <> "main")) h
    branchNote <- atomically (tryReadTChan reader)
    branchNote `shouldBe` Just (RefMoved "main" True)

    _ <- runViaWorker queue $ do
      h <- writeBlob "an undo-log entry commit"
      createRef undoLogRef h
    undoNote <- atomically (tryReadTChan reader)
    undoNote `shouldBe` Just UndoMoved

  it "RefMoved's existence flag is True for createRef/deleteRef but False for an ordinary updateRef" $ \repo -> do
    notifyChan <- newBroadcastTChanIO
    queue <- startGitWorker repo notifyChan
    reader <- atomically (dupTChan notifyChan)
    let ref = RefName (storyRefPrefix <> "main")

    _ <- runViaWorker queue $ writeBlob "hello" >>= createRef ref
    created <- atomically (tryReadTChan reader)
    created `shouldBe` Just (RefMoved "main" True)

    _ <- runViaWorker queue $ writeBlob "hello again" >>= updateRef ref
    moved <- atomically (tryReadTChan reader)
    moved `shouldBe` Just (RefMoved "main" False)

    _ <- runViaWorker queue $ deleteRef ref
    deleted <- atomically (tryReadTChan reader)
    deleted `shouldBe` Just (RefMoved "main" True)
