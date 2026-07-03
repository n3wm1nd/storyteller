{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-write: append a new story section.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   STORY_BRANCH        story branch name
--   ACTIVE_CHARS        comma-separated character branch names (optional)
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--
-- ARGS:
--   <file>   path of the file to append to, relative to the branch root
--
-- STDIN: the instruction / prompt for what to write next.
module Main (main) where

import Control.Monad (forM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , fileExists, readFile )
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Prelude hiding (readFile, writeFile)

import Storyteller.Core.Runtime ( Main, StoryModel, runStoryGit
                           , BranchTag(..), Git, runBranchAndFS )
import Storyteller.Core.Storage (StoryBranch, StoryStorage)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Agent (Instruction(..), Prose(..), CharContextBlock(..), WordCount(..))
import Storyteller.Agent.Continuation (continueFileAgent)
import Storyteller.Agent.CharContext (charSummaryAgent)
import Storyteller.Agent.Append (appendAgent)
import Storyteller.Agent.Splitter (Splitter, splitByParagraph)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

-- | Phantom tag for character branches opened temporarily within the action.
data Char_

main :: IO ()
main = do
  env         <- loadEnv
  args        <- getArgs
  outFile     <- case args of
    [f] -> return f
    _   -> hPutStrLn stderr "Usage: story-write <file>" >> exitFailure
  instruction <- fmap T.strip TIO.getContents

  result <- runStoryGit
    (envRepo env)
    (envEndpoint env)
    (BranchName (envBranch env))
    modelConfigs
    (splitByParagraph $ writeAction outFile (Instruction instruction) (envActiveChars env))

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

writeAction
  :: Members '[ LLM StoryModel
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , StoryBranch Main
              , StoryStorage
              , Splitter
              , Git
              , Logging, Fail] r
  => FilePath -> Instruction -> [T.Text] -> Sem r T.Text
writeAction outFile instruction activeChars = do
  charContexts <- fmap concat $ forM activeChars $ \charBranch -> do
    let branchName = BranchName charBranch
    blocks <- runBranchAndFS @Char_ branchName
            $ charSummaryAgent @(BranchTag Char_)
    return $ CharContextBlock ("## Character: " <> charBranch) : blocks

  Prose generated <- continueFileAgent @(BranchTag Main) @StoryModel
                               modelConfigs (Just (WordCount 300)) charContexts [] outFile instruction
  _ <- appendAgent @Main outFile generated
  return generated
