{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Scaling probe for the 'Storyteller.Core.Storage.at'/'updateReferences'
-- cascade cost.
--
-- Builds a "character" branch with N atoms, tracks all of them into K
-- separate entity/tracker branches (each tracker atom carrying a real
-- commit-parent ref back to its source atom, exactly as
-- 'Storyteller.Writer.Agent.Tracker' produces in production -- see
-- 'copyAtom'), then does one edit via 'atWithFS' at a chosen depth along
-- Source's chain: an Append while time-travelled, the scenario reported
-- to spike the server to ~200% CPU for a few seconds with as few as ~10
-- tracked atoms.
--
-- Three independent dimensions can each drive the cost up on their own:
--   N      -- atoms in Source (and so, per tracker, roughly N tracked atoms)
--   K      -- number of tracker branches referencing Source
--   depth  -- how far back from HEAD the edit lands (0 = near root,
--             1 = near head); a deep edit remaps more ticks, so every
--             referencing branch has more to rewrite too.
-- Sweeping each independently (holding the other two fixed) is what
-- answers "what's most costly": the dimension whose sweep grows fastest
-- is the one to budget hardest against.
--
-- Reports git-effect call counts by constructor rather than wall-clock
-- time: the in-memory mock's 'Data.Map' lookups are O(log n), so timing
-- alone would blur the growth signal we're trying to see under noise (GC,
-- RTS scheduling). Call counts give the same growth-rate signal without
-- the noise. Production runs 'Storyteller.Core.Git.withGitCache' (an
-- object/commit cache by hash) in front of the real git backend, and
-- still shows high CPU -- so subprocess-per-object overhead is not the
-- whole story; the recursive traversal shape itself
-- ('cascadeReplace'/'rewriteChain' in 'Storyteller.Core.Git') costs real
-- CPU independent of whether each individual read is cached, which this
-- mock-based call-count harness measures directly.
--
-- This is a 'cabal bench' component, not a test: scaling badly is a
-- performance fact to observe, not a correctness failure, so it never
-- fails the build.
--
--   cabal bench storyteller-cascade-perf                    -- default 3 sweeps
--   cabal bench storyteller-cascade-perf --benchmark-options "64 8 0.5"
--                                                             -- single (N, K, depth) run
module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Printf (printf)

import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.State (State, evalState, get, modify, put)

import Runix.Git (Git(..))
import Runix.FileSystem (writeFile, readFile)

import Git.Mock (emptyGitState, runGitMock)
import Storyteller.Core.Git (BranchTag, runBranchAndFS, runStoryStorageGit)
import Storyteller.Core.Storage (createBranch, store, follow, atWithFS)
import Storyteller.Core.Types (BranchName(..), tickId, tickParent)
import Storyteller.Writer.Agent.Tracker (trackBranch)

import Prelude hiding (writeFile, readFile)

-- | Phantom branch tags. 'Tracker' is reused sequentially for each of the K
--   tracker branches -- they're never open at once, only nested with
--   'Source' one at a time during setup, same as a real client issuing K
--   separate track commands.
data Source
data Tracker

type OpCounts = Map String Int

-- | Pass-through 'Git' interpreter that tallies calls by constructor into a
--   shared 'State OpCounts', the same "extra layer in the row" technique as
--   'Storyteller.Core.Git.withGitCache' / 'Server.BranchSpec.countRefWrites':
--   it consumes the outer 'Git' in the row and re-sends every call to the
--   real interpreter underneath (the 'Members' constraint needs a second
--   'Git' further down the stack, supplied here by 'runGitMock').
countGitOps :: Members '[Git, State OpCounts] r => Sem (Git : r) a -> Sem r a
countGitOps = interpret $ \case
  ResolveRef  ref     -> send (ResolveRef ref)  <* bump "ResolveRef"
  CreateRef   ref h    -> send (CreateRef ref h) <* bump "CreateRef"
  UpdateRef   ref h    -> send (UpdateRef ref h) <* bump "UpdateRef"
  DeleteRef   ref      -> send (DeleteRef ref)   <* bump "DeleteRef"
  ListRefs    prefix   -> send (ListRefs prefix) <* bump "ListRefs"
  ReadCommit  h        -> send (ReadCommit h)    <* bump "ReadCommit"
  WriteCommit cd       -> send (WriteCommit cd)  <* bump "WriteCommit"
  ReadObject  h        -> send (ReadObject h)    <* bump "ReadObject"
  WriteObject o        -> send (WriteObject o)   <* bump "WriteObject"
  LookupPath  h p      -> send (LookupPath h p)  <* bump "LookupPath"
  where
    bump :: Member (State OpCounts) r' => String -> Sem r' ()
    bump k = modify (Map.insertWith (+) k (1 :: Int))

-- | Strictly append-only content for the Nth atom, so each 'store' call
--   respects the branch's append-only invariant.
atomContent :: Int -> ByteString
atomContent n = BS.concat [ BS.pack ("paragraph " <> show i <> "\n") | i <- [1 .. n] ]

trackerName :: Int -> BranchName
trackerName j = BranchName (T.pack ("tracker" <> show j))

-- | Build N atoms on Source, track all of them into K tracker branches (N
--   real commit-parent edges into Source per tracker), then edit Source at
--   the given depth (0 = near root, 1 = near head) via 'atWithFS'. Returns
--   the git op counts for that single edit only -- setup's own git ops are
--   excluded by resetting the counter right before the timed step.
runScenario :: Int -> Int -> Double -> Either String OpCounts
runScenario n k depthFrac =
  run
  . runFail
  . evalState emptyGitState
  . evalState (Map.empty :: OpCounts)
  . runGitMock
  . countGitOps
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "source")
      runBranchAndFS @Source (BranchName "source") $
        forM_ [1 .. n] $ \i -> do
          writeFile @(BranchTag Source) "story.md" (atomContent i)
          _ <- store @Source (T.pack ("atom " <> show i))
          return ()

      forM_ [1 .. k] $ \j -> do
        _ <- createBranch (trackerName j)
        runBranchAndFS @Source (BranchName "source")
          . runBranchAndFS @Tracker (trackerName j)
          $ trackBranch @Source @Tracker ("story.md", "story.md")

      mid <- runBranchAndFS @Source (BranchName "source") $ do
        sourceTicks <- follow @Source [] (\acc t -> (tickId t : acc, tickParent t))
        let len = length sourceTicks
            idx = max 1 (min (len - 1) (round (depthFrac * fromIntegral (len - 1))))
        return (sourceTicks !! idx)

      put (Map.empty :: OpCounts)
      _ <- runBranchAndFS @Source (BranchName "source") $
        atWithFS @Source mid $ do
          existing <- readFile @(BranchTag Source) "story.md"
          writeFile @(BranchTag Source) "story.md" (existing <> "\nEDIT\n")
          store @Source "edit while at"
      get

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

totalOf :: OpCounts -> Int
totalOf = sum . Map.elems

header :: IO ()
header = printf "%8s  %10s  %12s  %12s  %10s\n"
  ("param" :: String) ("totalOps" :: String) ("ReadCommit" :: String)
  ("WriteCommit" :: String) ("growth" :: String)

-- | Print one sweep, holding the other two parameters fixed, and return the
--   largest observed growth-per-step factor (used to compare dimensions).
runSweep :: String -> [(String, Int, Int, Double)] -> IO Double
runSweep title cases = do
  putStrLn ""
  putStrLn title
  header
  growths <- go Nothing cases
  return $ if null growths then 1 else maximum growths
  where
    go :: Maybe Int -> [(String, Int, Int, Double)] -> IO [Double]
    go _ [] = return []
    go prevTotal ((label, n, k, d) : rest) = case runScenario n k d of
      Left err -> do
        printf "%8s  error: %s\n" label err
        go prevTotal rest
      Right counts -> do
        let total = totalOf counts
            rc    = Map.findWithDefault 0 "ReadCommit" counts
            wc    = Map.findWithDefault 0 "WriteCommit" counts
            g     = (\p -> fromIntegral total / fromIntegral p :: Double) <$> prevTotal
        printf "%8s  %10d  %12d  %12d  %10s\n" label total rc wc
          (maybe "-" (printf "%.2fx") g :: String)
        gs <- go (Just total) rest
        return (maybe gs (: gs) g)

main :: IO ()
main = do
  args <- getArgs
  case map read args :: [Double] of
    [n, k, d] -> singleRun (round n) (round k) d
    _         -> defaultSweeps

singleRun :: Int -> Int -> Double -> IO ()
singleRun n k d = do
  header
  case runScenario n k d of
    Left err     -> putStrLn ("error: " <> err)
    Right counts -> printf "%8s  %10d  %12d  %12d  %10s\n"
      (printf "%d/%d/%.2f" n k d :: String) (totalOf counts)
      (Map.findWithDefault 0 "ReadCommit" counts :: Int)
      (Map.findWithDefault 0 "WriteCommit" counts :: Int)
      ("-" :: String)

defaultSweeps :: IO ()
defaultSweeps = do
  nGrowth <- runSweep "Sweep 1: atoms per branch (N), K=2 trackers, edit at midpoint"
    [ (show n, n, 2, 0.5) | n <- [2, 4, 8, 16, 32, 64, 128, 256] ]

  kGrowth <- runSweep "Sweep 2: tracker branch count (K), N=32 atoms, edit at midpoint"
    [ (show k, 32, k, 0.5) | k <- [1, 2, 4, 8, 16, 32] ]

  putStrLn ""
  putStrLn "Sweep 3: edit depth (0 = near root, 1 = near head), N=64, K=4"
  header
  forM_ [0.0, 0.25, 0.5, 0.75, 0.95] $ \d ->
    case runScenario 64 4 d of
      Left err     -> printf "%8.2f  error: %s\n" d err
      Right counts -> printf "%8.2f  %10d  %12d  %12d  %10s\n" d (totalOf counts)
        (Map.findWithDefault 0 "ReadCommit" counts :: Int)
        (Map.findWithDefault 0 "WriteCommit" counts :: Int)
        ("-" :: String)

  putStrLn ""
  let ranked = maximumBy (comparing snd)
        [ ("atoms per branch (N)" :: String, nGrowth)
        , ("tracker branch count (K)", kGrowth)
        ]
  printf "Steepest per-doubling growth: %s at %.2fx per doubling.\n" (fst ranked) (snd ranked)
  putStrLn "(Depth isn't doubled in sweep 3, so it's not included in this ranking --\n\
           \ read its table directly: cost should scale with (chain length - depth*chain length).)"
