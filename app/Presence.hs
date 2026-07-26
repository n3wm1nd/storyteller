{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-presence: retroactively tag which characters are present in one
-- chapter, or every chapter, by asking the model to read the prose and
-- decide -- see "Storyteller.Writer.Agent.PresenceTrack". Meant for bulk-
-- ingesting an existing work written before presence tracking existed: run
-- once over the whole branch and every chapter gets real
-- 'Storyteller.Writer.Presence.recordPresence' ticks, the same kind the
-- Writer UI's own enter\/leave button writes by hand.
--
-- ENV:
--   STORY_REPO           path to the git repository
--   STORY_BRANCH         story branch name
--   LLAMACPP_ENDPOINT    (optional, default http://localhost:8080/v1)
--
-- ARGS:
--   <file>   path to one chapter to review, e.g. chapters/ch1.md
--   --all    review every recognized chapter on the branch, oldest first,
--            instead of one file
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead)
import Runix.Logging (Logging)

import Storyteller.Core.ContentEffects (BranchResolve, Cast, runCast)
import Storyteller.Core.Runtime (Main, runStoryGit, BranchTag, BranchOp)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent.PresenceTrack (PresenceDecision(..), trackPresenceFor, trackPresenceForAll)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

main :: IO ()
main = do
  env  <- loadEnv
  args <- getArgs
  target <- case args of
    ["--all"] -> return Nothing
    [f]       -> return (Just f)
    _         -> hPutStrLn stderr "Usage: story-presence (<file> | --all)" >> exitFailure

  result <- runStoryGit
    (envRepo env)
    (envEndpoint env)
    (BranchName (envBranch env))
    modelConfigs
    (interpretPromptStorageFS (runCast (presenceAction target)))

  case result of
    Left err       -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right reported -> mapM_ (uncurry report) reported

report :: FilePath -> [PresenceDecision] -> IO ()
report path decisions = TIO.putStrLn $
  T.pack path <> ": " <> T.intercalate ", "
    [ unBranchName (pdCharacter d) <> " " <> T.pack (show (pdEvent d)) | d <- decisions ]

presenceAction
  :: (LLMs r, Members '[ PromptStorage
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , BranchOp Main
              , StoryStorage
              , BranchResolve
              , Cast
              , Logging, Fail] r)
  => Maybe FilePath -> Sem r [(FilePath, [PresenceDecision])]
presenceAction (Just path) = (\ds -> [(path, ds)]) <$> trackPresenceFor @Main path
presenceAction Nothing     = trackPresenceForAll @Main
