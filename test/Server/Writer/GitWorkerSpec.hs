{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Integration check for 'Server.Writer.GitWorker': the one thing that
-- matters here is queue-sharing behaviour a mock 'Git' can't exercise --
-- real correctness of 'Git' itself is already covered by @gitlib-effect-test@.
--
-- Two things to pin down (see PLAN-git-storage-worker.md's step 5): a
-- second client submitting to the same 'GitWorkerQueue' sees a ref a first
-- client created (the whole point of centralizing on one worker), and one
-- job's ordinary 'Fail' doesn't wedge or poison the worker loop for
-- whatever is submitted next.
module Server.Writer.GitWorkerSpec (spec) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Polysemy (Embed, Sem, runM)
import Polysemy.Fail (Fail, runFail)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec

import Runix.Git
import Server.Writer.GitWorker (GitWorkerQueue, runGitViaWorker, startGitWorker)

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
