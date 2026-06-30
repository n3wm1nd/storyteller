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
--   <source-branch>          trackee branch name (e.g. "story")
--   <from:to> [from:to...]   file pairs: source path in trackee, dest path in tracker
module Main (main) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Error (runError)
import Storyteller.Runtime
  ( runInfrastructure, runBranchAndFS, runStoryStorageGit, BranchTag(..) )
import Storyteller.Storage (createBranch, getBranch)
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
    (src : pairs@(_:_)) -> return (BranchName (T.pack src), map parseFilePair pairs)
    _ -> hPutStrLn stderr "Usage: story-track <source-branch> <from:to> [from:to...]" >> exitFailure

  let trackerBranch = BranchName (envBranch env)

  result <- runTrackIO (envRepo env) (envEndpoint env) sourceBranch trackerBranch files

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right tids -> TIO.putStrLn $ "Tracked " <> T.pack (show (length tids)) <> " new tick(s)"

parseFilePair :: String -> (FilePath, FilePath)
parseFilePair s = case break (== ':') s of
  (from, ':':to) -> (from, to)
  _              -> (s, s)

runTrackIO
  :: FilePath
  -> String
  -> BranchName             -- ^ trackee (source)
  -> BranchName             -- ^ tracker (destination)
  -> [(FilePath, FilePath)]
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
      fmap concat $ mapM (trackBranch @Source @Tracker) files
