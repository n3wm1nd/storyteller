{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests the specific mechanism BlobSizeBench.hs's size-scaling result
-- points to: libgit2's @GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION@, which
-- the vendored header (@vendor/libgit2/include/git2/common.h@) documents
-- as:
--
-- > Enable strict verification of object hashsums when reading objects
-- > from disk. This may impact performance due to an additional checksum
-- > calculation on each object. This defaults to enabled.
--
-- i.e. every 'Runix.Git.FFI.readObjectRaw' call (-> @git_odb_read@) pays a
-- full extra SHA1 pass over the decompressed content on top of the zlib
-- inflate itself, on by default unless 'Runix.Git.FFI.FFIOptions' says
-- otherwise. That's an O(size) cost invisible at the tiny fixed sizes
-- every earlier bench but BlobSizeBench used, worse for reads than
-- writes, and requires no concurrency at all.
--
-- Runs the exact same size-sweep read/write loop 'BlobSizeBench' does
-- under 'Runix.Git.FFI.libgit2DefaultFFIOptions' (strict checks on) vs
-- 'Runix.Git.FFI.defaultFFIOptions' (this codebase's choice, both off),
-- via 'runGitFFIPerCallWith', so the delta directly attributes however
-- much of that bench's regression this one setting accounts for.
--
-- Not a correctness check -- disabling this is a real trust tradeoff for
-- production, not just a knob to flip blindly; this bench exists to
-- measure the effect, not to recommend disabling it. Never fails the
-- build.
--
--   cabal bench gitlib-effect-stricthash-bench
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
import Runix.Git
import qualified Runix.Git.FFI as FFI

runViaFFI :: FFI.FFIOptions -> FilePath -> Sem '[Git, Resource, Fail, Embed IO] a -> IO a
runViaFFI opts repo action = do
  result <- runM . runFail . runResource . runGitFFIPerCallWith opts repo $ action
  either (\e -> ioError (userError ("runGitFFIPerCallWith: " <> e))) return result

sizeSweep :: [Int]
sizeSweep = [16384, 131072, 524288, 2097152]

writesPerSize :: Int
writesPerSize = 30

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

runSweep :: String -> FFI.FFIOptions -> FilePath -> IO ()
runSweep label opts parent = do
  putStrLn label
  printf "%10s  %12s  %12s  %10s\n"
    ("size" :: String) ("write" :: String) ("read" :: String) ("read MB/s" :: String)
  forM_ (zip [1 :: Int ..] sizeSweep) $ \(i, size) -> do
    let repo = parent </> (label <> "-repo-" <> show i)
        content = mkContent size
    (writeT, hashes) <- runViaFFI opts repo (timeWrites content)
    readT <- runViaFFI opts repo (timeReads hashes)
    let mbs = (fromIntegral size / (1024 * 1024)) * fromIntegral writesPerSize / readT :: Double
    printf "%10d  %10.1fms  %10.1fms  %8.1f\n" size (writeT * 1000) (readT * 1000) mbs
  putStrLn ""

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-stricthash-bench" $ \parent -> do
  hSetBuffering stdout LineBuffering
  putStrLn "FFI-only, libgit2's own defaults (strict checks ON) vs this codebase's choice"
  putStrLn "(defaultFFIOptions, both OFF), same size sweep as BlobSizeBench.hs. If OFF closes"
  putStrLn "most of that bench's FFI-vs-CLI gap at large sizes, the extra per-read SHA1 pass"
  putStrLn "strict hash verification adds is the root cause."
  putStrLn (replicate 90 '=')

  runSweep "strict-ON " FFI.libgit2DefaultFFIOptions parent
  runSweep "strict-OFF" FFI.defaultFFIOptions parent
