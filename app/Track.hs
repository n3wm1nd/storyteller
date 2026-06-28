{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-track: copy new atoms from a trackee branch into a tracker branch.
--
-- ENV:
--   STORY_REPO    path to the git repository
--   STORY_BRANCH  tracker branch name  (the entity branch receiving copies)
--
-- ARGS:
--   <source-branch>   trackee branch name (e.g. "main")
--   <file> [file...]  one or more file paths to track
module Main (main) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Error (runError)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.Git (Git)
import Runix.Logging (Logging)

import Storyteller.Runtime
  ( runInfrastructure, runBranchAndFS, runStoryStorageGit, BranchTag(..) )
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, getBranch)
import Storyteller.Types (BranchName(..), TickId)
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.CLI.Env (StoryEnv(..), loadEnv)

-- | Phantom tags for the two branches open simultaneously.
data Source
data Tracker

main :: IO ()
main = do
  env  <- loadEnv
  args <- getArgs
  (sourceBranch, files) <- case args of
    (src : fs@(_:_)) -> return (BranchName (T.pack src), fs)
    _ -> hPutStrLn stderr "Usage: story-track <source-branch> <file> [file...]" >> exitFailure

  let trackerBranch = BranchName (envBranch env)

  result <- runTrackIO (envRepo env) (envEndpoint env) sourceBranch trackerBranch files

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right tids -> TIO.putStrLn $ "Tracked " <> T.pack (show (length tids)) <> " new tick(s)"

runTrackIO
  :: FilePath
  -> String
  -> BranchName   -- ^ trackee (source)
  -> BranchName   -- ^ tracker (destination)
  -> [FilePath]
  -> IO (Either String [TickId])
runTrackIO repoPath endpoint sourceBranch trackerBranch files =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runStoryStorageGit
  . runBranchAndFS @Source sourceBranch
  . runBranchAndFS @Tracker trackerBranch
  $ do
      getBranch trackerBranch >>= \case
        Nothing -> void $ createBranch trackerBranch
        Just _  -> return ()
      trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker) files
