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
--   <source-branch>  trackee branch name (e.g. "story")
--   <to-file>        destination path in the tracker branch
--   [only-file]      restrict to this one trackee file; omitted tracks
--                     every file on the trackee branch into <to-file>
module Main (main) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Error (runError)
import Storyteller.Core.Runtime
  ( runInfrastructure, runBranchAndFS, runStoryStorageGit )
import Storyteller.Core.Storage (createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..), TickId)
import Storyteller.Writer.Agent.Tracker (trackBranch)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv)

-- | Phantom tags for the two branches open simultaneously.
data Source
data Tracker

main :: IO ()
main = do
  env  <- loadEnv
  args <- getArgs
  (sourceBranch, toFile, onlyFile) <- case args of
    [src, to]        -> return (BranchName (T.pack src), to, Nothing)
    [src, to, only]  -> return (BranchName (T.pack src), to, Just only)
    _ -> hPutStrLn stderr "Usage: story-track <source-branch> <to-file> [only-file]" >> exitFailure

  let trackerBranch = BranchName (envBranch env)

  result <- runTrackIO (envRepo env) (envEndpoint env) sourceBranch trackerBranch onlyFile toFile

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right tids -> TIO.putStrLn $ "Tracked " <> T.pack (show (length tids)) <> " new tick(s)"

runTrackIO
  :: FilePath
  -> String
  -> BranchName             -- ^ trackee (source)
  -> BranchName             -- ^ tracker (destination)
  -> Maybe FilePath          -- ^ restrict to one trackee file; 'Nothing' = every file
  -> FilePath                -- ^ destination file on the tracker branch
  -> IO (Either String [TickId])
runTrackIO repoPath endpoint sourceBranch trackerBranch onlyFile toFile =
  runM . runError
  . runInfrastructure repoPath endpoint
  . runStoryStorageGit
  . runBranchAndFS @Source sourceBranch
  . runBranchAndFS @Tracker trackerBranch
  $ do
      getBranch trackerBranch >>= \case
        Nothing -> void $ createBranch trackerBranch
        Just _  -> return ()
      -- No character/presence context at this level (a bare CLI over two
      -- named branches) -- keep every candidate tick, same as before this
      -- CLI's 'trackBranch' gained its filter parameter.
      trackBranch @Source @Tracker onlyFile (\tick -> pure (Just tick)) toFile
