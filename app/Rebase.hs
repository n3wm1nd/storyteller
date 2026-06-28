{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-rebase: split working-tree diffs and merge them into the chain.
--
-- ENV:
--   STORY_REPO    path to the git repository
--   STORY_BRANCH  branch to operate on
--
-- No arguments required. Reads the current working tree, finds all content
-- that has not yet been committed, and uses 'At' to insert each block at its
-- correct position in the chain.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.Logging (Logging)

import Storyteller.Git (BranchTag)
import Storyteller.Runtime (Main, runBranchIO)
import Storyteller.Storage (StoryBranch, StoryStorage)
import Storyteller.Types (BranchName(..), TickId(..))
import Storyteller.Agent.SplitDiffMerge (splitDiffMerge)
import Storyteller.CLI.Env (StoryEnv(..), loadEnv)

main :: IO ()
main = do
  env <- loadEnv

  result <- runBranchIO @Main
    (envRepo env)
    (envEndpoint env)
    (BranchName (envBranch env))
    rebaseAction

  case result of
    Left err      -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right mapping ->
      TIO.putStrLn $ "Rebase complete. " <> T.pack (show (length mapping)) <> " tick(s) remapped."

rebaseAction
  :: Members '[ FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , StoryBranch Main
              , StoryStorage
              , Logging, Fail ] r
  => Sem r [(TickId, TickId)]
rebaseAction = splitDiffMerge @(BranchTag Main) @Main
