{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Isolates a purely single-threaded, single-request hypothesis (per
-- explicit direction: the live regression reproduces with one user and
-- one request, so it cannot be a concurrency/queueing effect --
-- WorkerBench.hs's line of investigation is the wrong path): that
-- 'Runix.Git.FFI's per-byte write/read cost scales worse than the CLI
-- interpreter's as object size grows, and that real content isn't the
-- tiny fixed-size blobs every other bench here writes.
--
-- 'Storage.Core.hs' (@newBlob <- liftG (writeBlobM (oldContent <>
-- suffix))@ in its append path) stores each version's *whole
-- accumulated content*, not a diff -- so a real deep split/merge chain
-- writes progressively larger blobs as it walks deeper into an
-- append-heavy chapter, not the same handful of bytes every bench here
-- has used so far. If FFI's write/read path (a plain
-- 'Data.ByteString.useAsCStringLen' + @git_odb_write@, libgit2's own
-- zlib settings) scales worse per byte than the CLI path's direct
-- loose-object write ('Runix.Git.Store', Haskell 'Codec.Compression.Zlib'),
-- that cost is invisible at the handful-of-bytes scale every prior bench
-- used and only shows up once blobs are realistically sized.
--
-- Sweeps blob size and reports write/read ops/s AND MB/s for both
-- backends at each size -- MB/s is the number to watch: if it stays flat
-- across backends, they scale identically and this isn't it; if FFI's
-- MB/s drops off relative to CLI's as size grows, that is a real,
-- deterministic (not load-dependent) explanation for a single-request
-- regression once real content sizes are in play.
--
-- Not a correctness check; never fails the build.
--
--   cabal bench gitlib-effect-blobsize-bench
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import GHC.Clock (getMonotonicTime)
import System.IO (BufferMode(..), hSetBuffering, stdout)
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

-- | Blob sizes to sweep, in bytes -- from every prior bench's ~20-byte
-- scale up through realistic prose-chapter sizes (a few hundred KB).
sizeSweep :: [Int]
sizeSweep = [64, 1024, 16384, 131072, 524288, 2097152]

writesPerSize :: Int
writesPerSize = 30

-- | Semi-compressible filler text (not all-zero, not random) -- closer to
-- prose than either extreme, so zlib's ratio doesn't flatter one backend
-- over the other by accident.
mkContent :: Int -> BS.ByteString
mkContent n = BS.take n (BS.concat (replicate (n `div` 64 + 1) chunk))
  where
    chunk = BS8.pack "the quick brown fox jumps over the lazy dog and keeps writing more text "

timeWrites :: Members '[Git, Fail, Embed IO] r => BS.ByteString -> Sem r (Double, [ObjectHash])
timeWrites content = do
  t0 <- embed getMonotonicTime
  hashes <- mapM (const (writeBlob content)) [1 .. writesPerSize]
  t1 <- embed getMonotonicTime
  return (t1 - t0, hashes)

timeReads :: Members '[Git, Fail, Embed IO] r => [ObjectHash] -> Sem r Double
timeReads hashes = do
  t0 <- embed getMonotonicTime
  mapM_ readBlob hashes
  t1 <- embed getMonotonicTime
  return (t1 - t0)

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-blobsize-bench" $ \parent -> do
  hSetBuffering stdout LineBuffering
  putStrLn "Does per-byte write/read cost scale differently between backends as object"
  putStrLn "size grows? (Storage.Core.hs's append path writes whole accumulated content,"
  putStrLn "not diffs -- real chains write progressively larger blobs, not fixed tiny ones.)"
  printf "%d writes + %d reads timed at each size\n" writesPerSize writesPerSize
  putStrLn (replicate 100 '=')
  printf "%10s  %12s  %12s  %10s  |  %12s  %12s  %10s\n"
    ("size" :: String)
    ("CLI write" :: String) ("CLI read" :: String) ("CLI MB/s" :: String)
    ("FFI write" :: String) ("FFI read" :: String) ("FFI MB/s" :: String)

  forM_ (zip [1 :: Int ..] sizeSweep) $ \(i, size) -> do
    let repoCLI = parent </> ("cli-repo-" <> show i)
        repoFFI = parent </> ("ffi-repo-" <> show i)
        content = mkContent size
    (writeCLI, hashesCLI) <- runViaCLI repoCLI (timeWrites content)
    (writeFFI, hashesFFI) <- runViaFFI repoFFI (timeWrites content)
    readCLI <- runViaCLI repoCLI (timeReads hashesCLI)
    readFFI <- runViaFFI repoFFI (timeReads hashesFFI)
    let mbPerOp = fromIntegral size / (1024 * 1024) :: Double
        mbsCLI = mbPerOp * fromIntegral writesPerSize / writeCLI
        mbsFFI = mbPerOp * fromIntegral writesPerSize / writeFFI
    printf "%10d  %10.1fms  %10.1fms  %8.1f  |  %10.1fms  %10.1fms  %8.1f\n"
      size (writeCLI * 1000) (readCLI * 1000) mbsCLI (writeFFI * 1000) (readFFI * 1000) mbsFFI

  putStrLn ""
  putStrLn "If FFI's MB/s column falls relative to CLI's as size grows (rather than both"
  putStrLn "staying roughly proportional throughout), FFI's per-byte handling is the"
  putStrLn "single-request, no-concurrency-needed explanation for the live regression."
