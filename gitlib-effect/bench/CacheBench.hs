{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests the hypothesis raised against 'WarmupBench'/'RefScalingBench''s
-- results (neither reproduced the reported FFI-vs-CLI regression: both
-- show FFI consistently faster in isolation): that 'Runix.Git.withGitCache'
-- -- applied identically to either backend by
-- 'Server.Writer.GitWorker.gitWorkerLoop' -- mechanically *should* behave
-- the same regardless of which interpreter sits underneath it (it's pure
-- 'Polysemy.intercept'/'Polysemy.State' bookkeeping, never touching the
-- interpreter), but might not actually be delivering its speedup for the
-- FFI backend in practice.
--
-- Runs the exact same 'withGitCache'-wrapped read loop -- a high
-- repeat-hit-rate walk over a small fixed set of hashes, the shape a
-- cache should turn into near-free 'Data.Map' lookups after the first
-- pass -- through both backends and reports the speedup 'withGitCache'
-- buys each one. If both speed up by roughly the same factor, the cache
-- is working identically for both and the live regression's cause is
-- elsewhere (see 'WarmupBench'/'RefScalingBench'). If FFI's speedup is
-- much smaller (or absent), that confirms the cache genuinely isn't
-- covering FFI reads the way it covers CLI ones.
--
-- Not a correctness check; never fails the build.
--
--   cabal bench gitlib-effect-cache-bench
module Main (main) where

import Control.Monad (forM_, replicateM)
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

distinctHashes :: Int
distinctHashes = 50

totalReads :: Int
totalReads = 20000

-- | 'distinctHashes' independent commits, all parentless -- only their
-- count and that they're independently addressable matters here.
populate :: Members '[Git, Fail] r => Sem r [ObjectHash]
populate = replicateM distinctHashes $ do
  h <- writeBlob (BS8.pack "content")
  tree <- writeTree [BlobEntry "f" h]
  writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "c" }

-- | Checks one specific way 'withGitCache' could silently stop matching
-- for one backend and not the other: the 'ObjectHash' 'Text' a caller
-- gets back from a fresh 'writeCommit' vs. the 'ObjectHash' 'Text' it
-- gets back later by resolving a ref pointed at the very same commit
-- (e.g. a different code path re-deriving the id, exactly the kind of
-- provenance a real request follows -- write once, read back later via a
-- ref rather than the original in-memory value). 'withGitCache' keys its
-- 'Data.Map.Strict.Map' on 'ObjectHash' structural equality ('Eq' 'Text'
-- underneath), so if libgit2 or the CLI ever handed back a
-- differently-cased or otherwise differently-rendered hex string for the
-- identical 20-byte id, every cache lookup keyed by the "wrong" copy
-- would silently miss forever with no error -- just a cache that never
-- seems to help. Returns 'True' if they match exactly.
checkIdStability :: Members '[Git, Fail] r => Sem r Bool
checkIdStability = do
  h <- writeBlob (BS8.pack "id-stability-check")
  tree <- writeTree [BlobEntry "f" h]
  written <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "c" }
  let ref = RefName "refs/heads/id-stability-check"
  _ <- createRef ref written
  resolved <- resolveRef ref
  return (resolved == Just written)

-- | 'totalReads' 'readCommit' calls round-robining over the same
-- 'distinctHashes' hashes -- a near-100% hit rate for any cache keyed by
-- hash, after the first 'distinctHashes' calls warm it.
readLoop :: Members '[Git, Fail] r => [ObjectHash] -> Sem r ()
readLoop hashes =
  forM_ [1 .. totalReads] $ \i -> readCommit (hashes !! (i `mod` distinctHashes))

timeIt :: IO a -> IO Double
timeIt action = do
  t0 <- getMonotonicTime
  _ <- action
  t1 <- getMonotonicTime
  return (t1 - t0)

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-cache-bench" $ \parent -> do
  hSetBuffering stdout LineBuffering
  putStrLn "Does withGitCache deliver the same speedup for both backends?"
  printf "%d distinct hashes, %d total readCommit calls (round-robin, ~100%% hit rate after warmup)\n"
    distinctHashes totalReads
  putStrLn (replicate 90 '=')

  let repoCLI = parent </> "cli-repo"
      repoFFI = parent </> "ffi-repo"

  hashesCLI <- runViaCLI repoCLI populate
  hashesFFI <- runViaFFI repoFFI populate

  stableCLI <- runViaCLI repoCLI checkIdStability
  stableFFI <- runViaFFI repoFFI checkIdStability
  printf "id stability (write hash == resolveRef(ref to it)):  CLI=%s  FFI=%s\n"
    (show stableCLI) (show stableFFI)
  putStrLn ""

  -- Uncached: readLoop through the raw Git effect, no withGitCache.
  uncachedCLI <- timeIt (runViaCLI repoCLI (readLoop hashesCLI))
  uncachedFFI <- timeIt (runViaFFI repoFFI (readLoop hashesFFI))

  -- Cached: identical readLoop, wrapped in withGitCache.
  cachedCLI <- timeIt (runViaCLI repoCLI (withGitCache (readLoop hashesCLI)))
  cachedFFI <- timeIt (runViaFFI repoFFI (withGitCache (readLoop hashesFFI)))

  printf "%10s  %14s  %14s  %10s\n" ("engine" :: String) ("uncached (ms)" :: String) ("cached (ms)" :: String) ("speedup" :: String)
  printf "%10s  %14.1f  %14.1f  %9.1fx\n" ("CLI" :: String) (uncachedCLI * 1000) (cachedCLI * 1000) (uncachedCLI / cachedCLI)
  printf "%10s  %14.1f  %14.1f  %9.1fx\n" ("FFI" :: String) (uncachedFFI * 1000) (cachedFFI * 1000) (uncachedFFI / cachedFFI)

  putStrLn ""
  putStrLn "If both speedup columns are in the same ballpark, withGitCache works identically"
  putStrLn "for both backends and isn't the source of the live FFI-vs-CLI regression. If FFI's"
  putStrLn "speedup is much smaller, withGitCache isn't actually covering FFI reads."
