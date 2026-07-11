{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Throughput comparison: 'Runix.Git.runGitIOPerCall' (CLI + persistent
-- batch reader + direct loose-object writes) vs 'Runix.Git.runGitFFIPerCall'
-- (libgit2 FFI) for the exact operation this codebase's production
-- workload is dominated by -- 'Runix.Git.ReadCommit' (see the
-- @rewriteChain@ walk in "Storyteller.Core.Git") -- plus
-- 'Runix.Git.WriteCommit' for comparison.
--
-- Isolated from the live server: no HTTP, no algorithm-level redundancy,
-- just raw sequential read/write throughput against a real repo, so any
-- delta is attributable to the interpreter alone.
--
-- Not a correctness check -- see @GitFFISpec@ for that. Never fails the
-- build; it only reports numbers.
--
--   cabal bench gitlib-effect-ffi-bench
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)
import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git

runViaCLI :: FilePath -> Sem '[Git, Cmd "git", Cmds, Resource, Fail, Embed IO] a -> IO a
runViaCLI repo action = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo $ action
  either (\e -> ioError (userError ("runGitIOPerCall: " <> e))) return result

runViaFFI :: FilePath -> Sem '[Git, Resource, Fail, Embed IO] a -> IO a
runViaFFI repo action = do
  result <- runM . runFail . runResource . runGitFFIPerCall repo $ action
  either (\e -> ioError (userError ("runGitFFIPerCall: " <> e))) return result

commitCount :: Int
commitCount = 5000

-- | A chain of `commitCount` commits, each's parent the last -- the same
-- shape @rewriteChain@ actually walks -- returning every hash written,
-- oldest first.
writeChain :: Members '[Git, Fail] r => Sem r [ObjectHash]
writeChain = go commitCount []
  where
    go 0 acc = return (reverse acc)
    go n acc = do
      h <- writeBlob (mkContent n)
      tree <- writeTree [BlobEntry "f" h]
      c <- writeCommit CommitData
             { commitParents = take 1 acc
             , commitTree    = tree
             , commitMessage = "commit " <> T.pack (show n)
             }
      go (n - 1) (c : acc)

    mkContent n = "content number " <> BS8.pack (show n)

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-ffi-bench" $ \parent -> do
  let repoCLI = parent </> "cli-repo"
      repoFFI = parent </> "ffi-repo"

  putStrLn "CLI (runGitIOPerCall) vs FFI (runGitFFIPerCall): write/read throughput"
  putStrLn (replicate 78 '=')

  (writeRateCLI, hashesCLI) <- timed $ runViaCLI repoCLI writeChain
  (writeRateFFI, hashesFFI) <- timed $ runViaFFI repoFFI writeChain

  readRateCLI <- rate commitCount $ runViaCLI repoCLI (forM_ hashesCLI readCommit)
  readRateFFI <- rate commitCount $ runViaFFI repoFFI (forM_ hashesFFI readCommit)

  printf "%d commits   write   CLI: %10.0f ops/s   FFI: %10.0f ops/s   FFI/CLI: %6.2fx\n"
    commitCount writeRateCLI writeRateFFI (writeRateFFI / writeRateCLI)
  printf "%d commits   read    CLI: %10.0f ops/s   FFI: %10.0f ops/s   FFI/CLI: %6.2fx\n"
    commitCount readRateCLI readRateFFI (readRateFFI / readRateCLI)

  -- Refs are the only other operation production actually does (per the
  -- live call-volume log: thousands of ReadCommit but only ~4 UpdateRef
  -- per request) -- so a handful of slow ref writes could account for the
  -- whole gap even though object read/write are both fine. Pre-populates
  -- a realistic number of *other* branches first (production's repo has
  -- accumulated many over real usage -- ListRefs alone was called 331
  -- times in one request), since libgit2's loose refdb backend does an
  -- existence/collision check that scans existing refs on every write.
  forM_ [1 .. existingRefCount] $ \i ->
    runViaCLI repoCLI (createRef (branchRef i) (head hashesCLI))
  forM_ [1 .. existingRefCount] $ \i ->
    runViaFFI repoFFI (createRef (branchRef i) (head hashesFFI))

  refWriteRateCLI <- rate refUpdateCount $
    runViaCLI repoCLI (forM_ [1 .. refUpdateCount] $ \i -> updateRef (updateBranchRef i) (head hashesCLI))
  refWriteRateFFI <- rate refUpdateCount $
    runViaFFI repoFFI (forM_ [1 .. refUpdateCount] $ \i -> updateRef (updateBranchRef i) (head hashesFFI))

  printf "%d refs (%d existing)   write   CLI: %10.1f ops/s   FFI: %10.1f ops/s   FFI/CLI: %6.2fx\n"
    refUpdateCount existingRefCount refWriteRateCLI refWriteRateFFI (refWriteRateFFI / refWriteRateCLI)
  where
    existingRefCount, refUpdateCount :: Int
    existingRefCount = 400
    refUpdateCount = 50

    branchRef, updateBranchRef :: Int -> RefName
    branchRef i = RefName ("refs/heads/story/branch-" <> T.pack (show i))
    updateBranchRef i = RefName ("refs/heads/story/update-branch-" <> T.pack (show i))
    timed :: IO [ObjectHash] -> IO (Double, [ObjectHash])
    timed action = do
      t0 <- getMonotonicTime
      hashes <- action
      t1 <- getMonotonicTime
      pure (fromIntegral commitCount / (t1 - t0), hashes)

    rate :: Int -> IO a -> IO Double
    rate n action = do
      t0 <- getMonotonicTime
      _  <- action
      t1 <- getMonotonicTime
      pure (fromIntegral n / (t1 - t0))
