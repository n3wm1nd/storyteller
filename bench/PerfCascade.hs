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
-- Builds a "character" branch with N atoms, tracks all of them into a
-- second branch (each tracker atom carrying a real commit-parent ref back
-- to its source atom, exactly as 'Storyteller.Writer.Agent.Tracker'
-- produces in production -- see 'copyAtom'), then does one edit via
-- 'atWithFS' at the branch's midpoint: an Append while time-travelled,
-- the scenario reported to spike the server to ~200% CPU for a few
-- seconds with as few as ~10 tracked atoms.
--
-- Reports git-effect call counts by constructor rather than wall-clock
-- time: the in-memory mock's 'Data.Map' lookups are O(log n), so timing
-- alone would blur the O(branches x chain length) blowup we're trying to
-- see under noise (GC, RTS scheduling). Call counts give the same
-- growth-rate signal without the noise, and don't need the real git
-- subprocess backend (and its fork-per-object overhead, likely the actual
-- dominant cost in production) to show the algorithmic shape of the
-- problem -- see 'cascadeReplace'/'rewriteChain' in
-- 'Storyteller.Core.Git'.
--
-- This is a 'cabal bench' component, not a test: scaling badly is a
-- performance fact to observe, not a correctness failure, so it never
-- fails the build. Run it and read the growth column:
--
--   cabal bench storyteller-cascade-perf --benchmark-options "1 2 4 8 16 32 64 128"
--
-- (defaults to a preset list if no args given). A growth column that
-- roughly doubles when N doubles is linear; one that roughly
-- quadruples is the O(branches x chain length) blowup this harness
-- exists to catch. Once the cascade is fixed to filter to branches that
-- actually reference the edited ticks, re-running this should show the
-- growth column flatten out -- that's the regression check, read by eye
-- for now rather than asserted, since there's no agreed acceptable
-- threshold yet.
module Main (main) where

import Control.Monad (foldM_, forM_)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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

-- | Phantom branch tags: the character branch being edited, and the
--   entity/tracker branch that has copied its atoms (the two-branch shape
--   'Storyteller.Writer.Agent.Tracker'/'Storyteller.TrackerSpec' already use).
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

-- | Build N atoms on Source, track all of them into Tracker (N real
--   commit-parent edges back into Source), then edit Source at its
--   midpoint via 'atWithFS'. Returns the git op counts for that single
--   edit only -- setup's own git ops are excluded by resetting the
--   counter right before the timed step.
runScenario :: Int -> Either String OpCounts
runScenario n =
  run
  . runFail
  . evalState emptyGitState
  . evalState (Map.empty :: OpCounts)
  . runGitMock
  . countGitOps
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "source")
      _ <- createBranch (BranchName "tracker")
      runBranchAndFS @Source (BranchName "source")
        . runBranchAndFS @Tracker (BranchName "tracker")
        $ do
            forM_ [1 .. n] $ \i -> do
              writeFile @(BranchTag Source) "story.md" (atomContent i)
              _ <- store @Source (T.pack ("atom " <> show i))
              return ()

            _ <- trackBranch @Source @Tracker ("story.md", "story.md")

            sourceTicks <- follow @Source [] (\acc t -> (tickId t : acc, tickParent t))
            let mid = sourceTicks !! (length sourceTicks `div` 2)

            put (Map.empty :: OpCounts)
            _ <- atWithFS @Source mid $ do
                   existing <- readFile @(BranchTag Source) "story.md"
                   writeFile @(BranchTag Source) "story.md" (existing <> "\nEDIT\n")
                   store @Source "edit while at"
            get

main :: IO ()
main = do
  args <- getArgs
  let ns = if null args then [1, 2, 4, 8, 16, 32, 64, 128] else map read args :: [Int]
  printf "%6s  %10s  %12s  %12s  %10s\n"
    ("N" :: String) ("totalOps" :: String) ("ReadCommit" :: String)
    ("WriteCommit" :: String) ("growth" :: String)
  foldM_ step Nothing ns
  where
    step :: Maybe Int -> Int -> IO (Maybe Int)
    step prevTotal n = case runScenario n of
      Left err -> do
        printf "%6d  error: %s\n" n err
        return prevTotal
      Right counts -> do
        let total  = sum (Map.elems counts)
            rc     = Map.findWithDefault 0 "ReadCommit" counts
            wc     = Map.findWithDefault 0 "WriteCommit" counts
            growth = maybe "-" (\p -> printf "%.2fx" (fromIntegral total / fromIntegral p :: Double)) prevTotal
        printf "%6d  %10d  %12d  %12d  %10s\n" n total rc wc (growth :: String)
        return (Just total)
