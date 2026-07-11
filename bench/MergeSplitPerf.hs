{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Reproduces the live "merge.atoms"/"split.atoms" report directly, using
-- the real 'Storage.Ops.mergeAtoms'/'Storage.Ops.splitTick' algorithm
-- (not a synthetic linear-chain approximation like gitlib-effect's
-- WarmupBench) against a real git backend, with real tracker branches --
-- per bench/PerfCascade.hs, cascade cost scales with both chain depth
-- (N) and tracker-branch count (K), and a merge/split near the head of a
-- tracked branch has to check\/remap every tracker's refs into it, not
-- just walk one linear chain the way gitlib-effect's benches did.
--
-- Builds one "source" branch with N atoms, tracks it into K tracker
-- branches (real commit-parent-ref cross-branch links, exactly
-- 'Storyteller.Writer.Agent.Tracker' produces in production), then runs
-- R repeated merge+split request pairs near the head -- mirroring the
-- live log's alternating merge.atoms/split.atoms sequence -- through ONE
-- already-open interpreter per backend (matching
-- Server.Writer.GitWorker's shape: state persists across requests within
-- a run), timing each request and tallying git-op counts by constructor
-- (same technique as PerfCascade.hs/RealGitPerf.hs) so the source of any
-- remaining CLI-vs-FFI gap is visible directly, not just its total.
--
-- Not a correctness check; never fails the build.
--
--   cabal bench storyteller-mergesplit-perf
--   cabal bench storyteller-mergesplit-perf --benchmark-options "80 4 4"
--     -- atomCount trackerCount requestPairs
module Main (main) where

import Control.Monad (forM_, replicateM)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)
import Polysemy.State (State, evalState, modify, get)
import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git (Git(..), runGitFFIPerCall, runGitIOPerCall)

import Storyteller.Core.Git (runBranchAndFS, runStorage, runStoryStorageGit)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent.Tracker (trackBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops

data Source
data Tracker

type OpCounts = Map String Int

countGitOps :: Members '[Git, State OpCounts] r => Sem (Git : r) a -> Sem r a
countGitOps = interpret $ \case
  ResolveRef  ref     -> send (ResolveRef ref)   <* bump "ResolveRef"
  CreateRef   ref h   -> send (CreateRef ref h)  <* bump "CreateRef"
  UpdateRef   ref h   -> send (UpdateRef ref h)  <* bump "UpdateRef"
  DeleteRef   ref     -> send (DeleteRef ref)    <* bump "DeleteRef"
  ListRefs    prefix  -> send (ListRefs prefix)  <* bump "ListRefs"
  ReadCommit  h        -> send (ReadCommit h)    <* bump "ReadCommit"
  WriteCommit cd       -> send (WriteCommit cd)  <* bump "WriteCommit"
  ReadObject  h        -> send (ReadObject h)    <* bump "ReadObject"
  WriteObject o        -> send (WriteObject o)   <* bump "WriteObject"
  LookupPath  h p      -> send (LookupPath h p)  <* bump "LookupPath"
  IsAncestorOfAny ts h -> send (IsAncestorOfAny ts h) <* bump "IsAncestorOfAny"
  where
    bump :: Member (State OpCounts) r' => String -> Sem r' ()
    bump k = modify (Map.insertWith (+) k (1 :: Int))

trackerName :: Int -> BranchName
trackerName j = BranchName (T.pack ("tracker" <> show j))

paragraph :: Int -> T.Text
paragraph n = T.pack ("Paragraph " <> show n <> ": the quick brown fox jumps over the lazy dog, "
  <> "carrying the plot forward one more small step through the chapter. ")

-- | Setup: N atoms on "source", tracked into K tracker branches -- same
-- shape as PerfCascade.hs's 'buildScenario', but through a real git
-- backend and with an identity atom filter (every trackee tick is kept
-- as-is), matching what 'Storyteller.Writer.Agent.Tracker' actually does
-- when tracking everything.
setup :: Members '[StoryStorage, Git, Fail] r => Int -> Int -> Sem r ()
setup atomCount trackerCount = do
  _ <- createBranch (BranchName "source")
  runBranchAndFS @Source (BranchName "source") $
    forM_ [1 .. atomCount] $ \i ->
      runStorage @Source (Ops.addAtom "story.md" (paragraph i))

  forM_ [1 .. trackerCount] $ \j -> do
    _ <- createBranch (trackerName j)
    runBranchAndFS @Source (BranchName "source")
      . runBranchAndFS @Tracker (trackerName j)
      $ trackBranch @Source @Tracker (\t -> return (Just t)) ("story.md", "story.md")

-- | The last two atom ids on "source", oldest-first -- always valid to
-- merge (contiguous, same file) regardless of how many merge/split
-- cycles have already run, since both operations only ever produce fresh
-- atoms in place of what they consumed.
lastTwoAtoms :: Members '[StoryStorage, Git, Fail] r => Sem r (Core.ObjectHash, Core.ObjectHash)
lastTwoAtoms = runBranchAndFS @Source (BranchName "source") $ do
  (chain, _) <- runStorage @Source (Core.follow [] (\acc h t -> ((h, t) : acc, True)))
  let atomHashes = [ h | (h, Core.Atom {}) <- chain ]
  case reverse (take 2 (reverse atomHashes)) of
    [a, b] -> return (a, b)
    _      -> fail "lastTwoAtoms: fewer than 2 atoms on source"

-- | One merge+split request pair: merge the last two atoms into one, then
-- immediately split that merged atom back into two pieces -- mirroring
-- the live log's alternating merge.atoms/split.atoms sequence. Keeps the
-- chain's atom count stable across repeated requests.
mergeThenSplit :: Members '[StoryStorage, Git, Fail] r => Sem r ()
mergeThenSplit = do
  (a, b) <- lastTwoAtoms
  (merged, _) <- runBranchAndFS @Source (BranchName "source") $
    runStorage @Source (Ops.mergeAtoms [a, b])
  _ <- runBranchAndFS @Source (BranchName "source") $
    runStorage @Source (Ops.splitTick merged ["first half. ", "second half. "])
  return ()

-- | One merge+split request, wrapped to hand back its own git-op counts
-- -- same composition PerfCascade.hs/RealGitPerf.hs use ('countGitOps'
-- wraps the Git ops 'runStoryStorageGit' produces, both applied freshly
-- per request; only the real git interpreter further out, opened once
-- for the whole request loop, is what's actually persistent).
countedRequest :: Members '[StoryStorage, Git, State OpCounts, Fail] r => Sem r OpCounts
countedRequest = mergeThenSplit >> get

-- | Runs 'requestPairs' merge+split requests through ONE already-open CLI
-- interpreter (any in-process caching persists across requests within
-- this call -- matching how Server.Writer.GitWorker keeps one
-- interpreter open for the whole process), timing each request and
-- tallying its own git-op counts separately.
timeRequestsCLI :: FilePath -> Int -> IO [(Double, OpCounts)]
timeRequestsCLI repo requestPairs = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo $
    replicateM requestPairs $ do
      t0 <- embed getMonotonicTime
      counts <- evalState (Map.empty :: OpCounts) . countGitOps . runStoryStorageGit $ countedRequest
      t1 <- embed getMonotonicTime
      return (t1 - t0, counts)
  either (\e -> ioError (userError ("CLI: " <> e))) return result

-- | Same as 'timeRequestsCLI' but through one already-open FFI interpreter.
timeRequestsFFI :: FilePath -> Int -> IO [(Double, OpCounts)]
timeRequestsFFI repo requestPairs = do
  result <- runM . runFail . runResource . runGitFFIPerCall repo $
    replicateM requestPairs $ do
      t0 <- embed getMonotonicTime
      counts <- evalState (Map.empty :: OpCounts) . countGitOps . runStoryStorageGit $ countedRequest
      t1 <- embed getMonotonicTime
      return (t1 - t0, counts)
  either (\e -> ioError (userError ("FFI: " <> e))) return result

main :: IO ()
main = withSystemTempDirectory "storyteller-mergesplit-perf" $ \parent -> do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let (atomCount, trackerCount, requestPairs) = case map read args :: [Int] of
        [a, b, c] -> (a, b, c)
        _         -> (80, 4, 4)

  putStrLn "Real mergeAtoms/splitTick, real tracker branches, one persistent interpreter"
  putStrLn "per backend across all requests -- matching the live merge.atoms/split.atoms report."
  printf "atomCount=%d trackerCount=%d requestPairs=%d\n" atomCount trackerCount requestPairs
  putStrLn (replicate 90 '=')

  let repoCLI = parent </> "cli-repo"
      repoFFI = parent </> "ffi-repo"

  putStrLn "setting up CLI repo (source + trackers)..."
  _ <- runViaCLIState repoCLI (setup atomCount trackerCount)
  putStrLn "setting up FFI repo (source + trackers)..."
  _ <- runViaFFIState repoFFI (setup atomCount trackerCount)

  putStrLn "running CLI requests..."
  cliResults <- timeRequestsCLI repoCLI requestPairs
  putStrLn "running FFI requests..."
  ffiResults <- timeRequestsFFI repoFFI requestPairs

  putStrLn ""
  printf "%8s  %10s  %10s  %9s  |  %10s  %10s  %9s\n"
    ("request" :: String) ("CLI ms" :: String) ("CLI ops" :: String) ("CLI RC" :: String)
    ("FFI ms" :: String) ("FFI ops" :: String) ("FFI RC" :: String)
  forM_ (zip3 [1 :: Int ..] cliResults ffiResults) $ \(i, (ct, ccounts), (ft, fcounts)) ->
    printf "%8d  %10.1f  %10d  %9d  |  %10.1f  %10d  %9d\n"
      i (ct * 1000) (total ccounts) (rc ccounts) (ft * 1000) (total fcounts) (rc fcounts)

  putStrLn ""
  putStrLn "Per-constructor op counts, request 1 vs steady state (last request):"
  printOpTable "CLI, request 1"    (snd (head cliResults))
  printOpTable "CLI, steady state" (snd (last cliResults))
  printOpTable "FFI, request 1"    (snd (head ffiResults))
  printOpTable "FFI, steady state" (snd (last ffiResults))
  where
    total = sum . Map.elems
    rc = Map.findWithDefault 0 "ReadCommit"

    runViaCLIState repo action = do
      r <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo . runStoryStorageGit $ action
      either (\e -> ioError (userError ("CLI setup: " <> e))) return r
    runViaFFIState repo action = do
      r <- runM . runFail . runResource . runGitFFIPerCall repo . runStoryStorageGit $ action
      either (\e -> ioError (userError ("FFI setup: " <> e))) return r

    printOpTable label counts = do
      putStrLn label
      mapM_ (\(k, v) -> printf "  %-16s %d\n" k v) (List.sortOn (negate . snd) (Map.toList counts))
      putStrLn ""
