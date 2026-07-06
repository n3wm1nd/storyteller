{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Diagnostic: reproduce the reported "split/merge still ~10s at ~300
-- ticks deep, no improvement from the storage-monad migration" against a
-- real git backend (not Git.Mock), to find out whether the remaining cost
-- is git-subprocess-call count (unaffected by removing interpretH) rather
-- than Polysemy dispatch overhead.
--
-- Usage: cabal bench real-git-perf --benchmark-options "<repoPath> <depth>"
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Printf (printf)

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (runResource)
import Polysemy.State (State, evalState, modify, get)
import Runix.Cmd (Cmds, cmdsIO, interpretCmd)
import Runix.Git (Git(..), runGitIOPerCall)

import Storyteller.Core.Git (runStorage, runStorageEdit, runBranchAndFS, runStoryStorageGit)
import Storyteller.Core.Storage (StoryStorage, getBranch)
import qualified Storyteller.Core.StorageMonad as SM
import Storyteller.Core.Types

type OpCounts = Map String Int

-- | Pass-through 'Git' interpreter that tallies calls by constructor —
--   same technique as 'bench/PerfCascade.hs's 'countGitOps', but wrapping
--   the *real* git backend this time instead of 'Git.Mock', so the counts
--   reflect actual subprocess-call volume.
countGitOps :: Members '[Git, State OpCounts] r => Sem (Git : r) a -> Sem r a
countGitOps = interpret $ \case
  ResolveRef  ref     -> send (ResolveRef ref)   <* bump "ResolveRef"
  CreateRef   ref h   -> send (CreateRef ref h)  <* bump "CreateRef"
  UpdateRef   ref h   -> send (UpdateRef ref h)  <* bump "UpdateRef"
  DeleteRef   ref     -> send (DeleteRef ref)    <* bump "DeleteRef"
  ListRefs    prefix  -> send (ListRefs prefix)  <* bump "ListRefs"
  ReadCommit  h       -> send (ReadCommit h)     <* bump "ReadCommit"
  WriteCommit cd      -> send (WriteCommit cd)   <* bump "WriteCommit"
  ReadObject  h       -> send (ReadObject h)     <* bump "ReadObject"
  WriteObject o       -> send (WriteObject o)    <* bump "WriteObject"
  LookupPath  h p     -> send (LookupPath h p)   <* bump "LookupPath"
  IsAncestorOfAny ts h -> send (IsAncestorOfAny ts h) <* bump "IsAncestorOfAny"
  where
    bump :: Member (State OpCounts) r' => String -> Sem r' ()
    bump k = modify (Map.insertWith (+) k (1 :: Int))

data Main_

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    (repoPath : depthStr : _) -> run repoPath (read depthStr)
    [repoPath]                -> run repoPath 300
    []                        -> run "/tmp/gitperf/story" 300
  where
    run repoPath depth = do
      putStrLn ("repo: " <> repoPath <> ", target depth: " <> show depth)
      result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repoPath
        . evalState (Map.empty :: OpCounts)
        . countGitOps
        . runStoryStorageGit
        $ diagnostic depth
      case result of
        Left err -> putStrLn ("error: " <> err)
        Right () -> return ()

-- | Find the tick at (approximately) the given depth from HEAD on
--   refs/heads/story/master, then time a single 'editAtom' there (a
--   representative "edit deep in history" op: one rewind, one write at
--   the target, then a full tail-replay of everything after it back to
--   HEAD -- same shape of work 'splitTick'/'mergeAtoms' do).
diagnostic
  :: Members '[StoryStorage, Git, State OpCounts, Fail, Embed IO] r
  => Int -> Sem r ()
diagnostic depth = do
  mB <- getBranch (BranchName "master")
  case mB of
    Nothing -> embed (putStrLn "branch story/master not found")
    Just _  -> runBranchAndFS @Main_ (BranchName "master") $ do
      t0chain <- embed getMonotonicTime
      chain <- runStorage @Main_ (SM.followChain [] (\acc t -> (t : acc, tickParent t)))
      t1chain <- embed getMonotonicTime
      embed $ printf "chain length: %d (walked in %.3fs)\n" (length chain) (t1chain - t0chain)

      -- chain is oldest-first here (root:...:head), so depth-from-HEAD n
      -- is index (length-1-n).
      let idx = max 0 (min (length chain - 1) (length chain - 1 - depth))
          target = chain !! idx
          tid = tickId target
      embed $ printf "target tick at chain index %d (depth-from-head ~%d), kind-ish msg: %s\n"
        idx (length chain - 1 - idx) (T.unpack (T.take 40 (tickMessage (tickData target))))

      -- Only proceed if this tick actually carries a "file" field (i.e. is
      -- editable via editAtom) -- else just report and stop.
      case lookup "file" (tickFields (tickData target)) of
        Nothing -> embed $ putStrLn "target tick has no 'file' field (not a plain atom) -- picking nearest one with one"
        Just file -> do
          t0 <- embed getMonotonicTime
          (_newTid, mapping) <- runStorageEdit @Main_ (SM.editAtom tid (T.unpack file) "DIAGNOSTIC EDIT\n")
          t1 <- embed getMonotonicTime
          counts <- get @OpCounts
          embed $ printf "editAtom at depth ~%d: %.3fs wall, %d ticks remapped\n"
            depth (t1 - t0) (length mapping)
          embed $ putStrLn "git op counts:"
          embed $ mapM_ (\(k, v) -> printf "  %-14s %d\n" k v) (Map.toList counts)
          embed $ printf "  %-14s %d\n" ("TOTAL" :: String) (sum (Map.elems counts))
