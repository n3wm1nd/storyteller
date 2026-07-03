{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Error (runError)
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.Logging (Logging)

import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Runtime (Main, runInfrastructure, runStoryStorageGit)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Core.Edit (commitWorkingTree)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv)

main :: IO ()
main = do
  env <- loadEnv
  let branch = BranchName (envBranch env)

  result <-
    runM . runError
    . runInfrastructure (envRepo env) (envEndpoint env)
    . runStoryStorageGit
    . runBranchAndFS @Main branch
    $ do
        getBranch branch >>= \case
          Nothing -> void $ createBranch branch
          Just _  -> return ()
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
rebaseAction = commitWorkingTree @(BranchTag Main) @Main
