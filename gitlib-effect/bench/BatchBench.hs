{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Throughput comparison: reading objects through a persistent
-- @git cat-file --batch@ process ('Runix.Git.Batch.readBatch') vs the
-- one-shot @git cat-file -p \<hash\>@ subprocess-per-call approach
-- 'Runix.Git.runGitIO' uses today for 'ReadCommit' (and, doubled up, for
-- 'ReadObject' -- not separately measured here, see 'Runix.Git.Batch'
-- module docs).
--
-- Not a correctness check -- see @GitBatchSpec@ for that. Never fails the
-- build; it only reports numbers.
--
--   cabal bench gitlib-effect-batch-bench
module Main (main) where

import Control.Monad (forM, forM_)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Text.Printf (printf)

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)
import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git
import Runix.Git.Batch

runInRepo :: FilePath -> Sem '[Git, Cmd "git", Cmds, Resource, Fail, Embed IO] a -> IO a
runInRepo repo action = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIO repo $ action
  either (\e -> ioError (userError ("runGitIO: " <> e))) return result

objectCount :: Int
objectCount = 500

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-batch-bench" $ \repo -> do
  callProcess "git" ["-C", repo, "init", "-q"]
  hashes <- runInRepo repo (forM [1 .. objectCount] (writeBlob . mkContent))

  putStrLn "Object read throughput: persistent `cat-file --batch` vs one-shot `git cat-file`"
  putStrLn (replicate 78 '=')

  batchRate <- withBatchReader repo $ \br -> rate objectCount $
    forM_ hashes (readBatch br . unObjectHash)

  cliRate <- rate objectCount $
    forM_ hashes $ \h ->
      readProcess "git" ["-C", repo, "cat-file", "-p", T.unpack (unObjectHash h)] ""

  printf "%d reads   batch: %10.0f ops/s   one-shot: %8.1f ops/s   speedup: %6.0fx\n"
    objectCount batchRate cliRate (batchRate / cliRate)
  where
    mkContent i = BS8.pack ("content number " <> show (i :: Int))

    rate :: Int -> IO a -> IO Double
    rate n action = do
      t0 <- getMonotonicTime
      _  <- action
      t1 <- getMonotonicTime
      pure (fromIntegral n / (t1 - t0))
