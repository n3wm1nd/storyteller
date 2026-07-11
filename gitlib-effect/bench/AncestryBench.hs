{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Regression guard for the actual root cause of a live-production
-- report that the FFI interpreter ran far slower than the CLI one
-- despite winning every raw read/write/ref-write throughput measurement:
-- 'Runix.Git.IsAncestorOfAny' used to call libgit2's
-- @git_graph_descendant_of@ once per target, each doing a full ancestry
-- walk from scratch, so N targets cost N full walks -- a real GHC
-- profile of a live cascade found this responsible for 72% of total
-- wall-clock despite the op itself being called only ~14 times per
-- request. Fixed by walking the ancestry once with @git_revwalk@ and
-- checking every target incrementally against that single pass, matching
-- the CLI interpreter's own @git rev-list@-based strategy.
--
-- Sweeps target-list size (how many branches\/entities reference the
-- branch being checked -- the real-world driver of this cost, since a
-- cascade checks ancestry against every tracker\/entity branch at once)
-- against a repo with real depth, so a future regression (accidentally
-- reintroducing a per-target graph walk) shows up as FFI/CLI growing
-- with target count instead of staying flat.
--
-- Not a correctness check -- see @GitFFISpec@ for that (its own
-- "isAncestorOfAny" differential test already covers correctness of the
-- @git_revwalk@-based implementation). Never fails the build.
--
--   cabal bench gitlib-effect-ancestry-bench
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
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

mainDepth :: Int
mainDepth = 500

targetCountSweep :: [Int]
targetCountSweep = [10, 50, 200]

checksPerSize :: Int
checksPerSize = 5

buildChain :: Members '[Git, Fail] r => String -> Int -> [ObjectHash] -> Sem r [ObjectHash]
buildChain tag depth parent0 = go depth parent0 []
  where
    go 0 _ acc = return (reverse acc)
    go n parents acc = do
      h <- writeBlob (BS8.pack (tag <> " content " <> show n))
      tree <- writeTree [BlobEntry "f" h]
      c <- writeCommit CommitData
             { commitParents = parents
             , commitTree    = tree
             , commitMessage = T.pack (tag <> " commit " <> show n)
             }
      go (n - 1) [c] (c : acc)

-- | One main chain, plus @n@ other branches forking off at scattered
-- points -- the shape a cascade actually checks ancestry against
-- (entity\/tracker branches whose history threads back through the
-- branch being edited).
buildRepo :: Members '[Git, Fail] r => Int -> Sem r (ObjectHash, [ObjectHash])
buildRepo n = do
  chain <- buildChain "main" mainDepth []
  others <- mapM (\i -> do
      let forkIdx = min (length chain - 1) ((i * length chain) `div` (n + 1))
      tail_ <- buildChain ("branch" <> show i) 3 [chain !! forkIdx]
      return (last tail_))
    [1 .. n]
  return (last chain, others)

timeChecks :: Members '[Git, Fail, Embed IO] r => [ObjectHash] -> ObjectHash -> Sem r Double
timeChecks targets headHash = do
  t0 <- embed getMonotonicTime
  forM_ [1 :: Int .. checksPerSize] $ \_ -> isAncestorOfAny targets headHash
  t1 <- embed getMonotonicTime
  return (t1 - t0)

main :: IO ()
main = withSystemTempDirectory "gitlib-effect-ancestry-bench" $ \parent -> do
  hSetBuffering stdout LineBuffering
  putStrLn "isAncestorOfAny scaling with target-list size (many-branches cascade shape)"
  printf "mainDepth=%d, %d checks timed at each target count\n" mainDepth checksPerSize
  putStrLn (replicate 90 '=')
  printf "%12s  %14s  %14s  %10s\n"
    ("targets" :: String) ("CLI ms/check" :: String) ("FFI ms/check" :: String) ("FFI/CLI" :: String)

  forM_ (zip [1 :: Int ..] targetCountSweep) $ \(i, n) -> do
    let repoCLI = parent </> ("cli-repo-" <> show i)
        repoFFI = parent </> ("ffi-repo-" <> show i)
    (headCLI, othersCLI) <- runViaCLI repoCLI (buildRepo n)
    (headFFI, othersFFI) <- runViaFFI repoFFI (buildRepo n)
    -- Worst case for the old per-target-walk implementation: none of the
    -- targets are actually ancestors, so it can't short-circuit early --
    -- exercise that here by checking against the *other* branches' own
    -- heads (unrelated to the main chain's own head) rather than the
    -- main chain's real ancestors.
    cliTime <- runViaCLI repoCLI (timeChecks othersCLI headCLI)
    ffiTime <- runViaFFI repoFFI (timeChecks othersFFI headFFI)
    let cliPerCheck = cliTime * 1000 / fromIntegral checksPerSize
        ffiPerCheck = ffiTime * 1000 / fromIntegral checksPerSize
    printf "%12d  %14.2f  %14.2f  %9.2fx\n" n cliPerCheck ffiPerCheck (ffiPerCheck / cliPerCheck)

  putStrLn ""
  putStrLn "FFI/CLI should stay roughly flat across target counts (both interpreters walk"
  putStrLn "the ancestry once, not once per target). A ratio that grows with target count"
  putStrLn "is exactly the regression this bench exists to catch."
