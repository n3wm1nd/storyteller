{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile, writeFile)
import Runix.LLM (LLM)
import Runix.Logging (Logging)
import Polysemy
import Polysemy.Fail
import UniversalLLM (ModelConfig(..))

import Prelude hiding (readFile, writeFile)

import Storyteller.Git (BranchTag)
import Storyteller.Runtime
import Storyteller.Storage (StoryBranch, store)
import Storyteller.Types (BranchName(..))
import Storyteller.Agent.Continuation (continuationAgent)

configs :: [ModelConfig StoryModel]
configs =
  [ SystemPrompt "You are a creative writing assistant. Write only what is asked. Output only prose, nothing else."
  , MaxTokens 2048
  , Temperature 0.8
  ]

-- ---------------------------------------------------------------------------
-- Arg parsing
-- ---------------------------------------------------------------------------

data Mode
  = LocalMode  FilePath                      -- ^ output file, rooted at cwd
  | GitMode    FilePath BranchName FilePath  -- ^ repo path, branch, file within branch

usage :: String
usage = unlines
  [ "Usage:"
  , "  storyteller <file>"
  , "  storyteller --git <repo> <branch> <file>"
  ]

parseArgs :: [String] -> IO Mode
parseArgs ["--git", repo, branch, file] =
  return $ GitMode repo (BranchName (T.pack branch)) file
parseArgs [file] =
  return $ LocalMode file
parseArgs _ =
  hPutStrLn stderr usage >> exitFailure

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args        <- getArgs
  mode        <- parseArgs args
  endpoint    <- maybe "http://localhost:8080/v1" id <$> lookupEnv "LLAMACPP_ENDPOINT"
  instruction <- fmap T.strip TIO.getContents

  result <- case mode of
    LocalMode outFile -> do
      rootDir <- getCurrentDirectory
      runStoryIO endpoint rootDir configs
        $ localAction outFile instruction

    GitMode repoPath branch outFile ->
      runStoryGitIO endpoint repoPath branch configs
        $ gitAction outFile instruction

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

-- ---------------------------------------------------------------------------
-- Local action: read → agent → write via StoryFS
-- ---------------------------------------------------------------------------

localAction
  :: Members '[LLM StoryModel, FileSystem StoryFS, FileSystemRead StoryFS, FileSystemWrite StoryFS, Logging, Fail] r
  => FilePath -> T.Text -> Sem r T.Text
localAction outFile instruction = do
  existing <- fileExists @StoryFS outFile >>= \case
    True  -> TE.decodeUtf8 <$> readFile @StoryFS outFile
    False -> return ""
  appended <- continuationAgent @StoryFS @StoryModel configs (Just 300) existing instruction
  let sep = if T.null existing || T.isSuffixOf "\n\n" existing then ""
            else if T.isSuffixOf "\n" existing then "\n"
            else "\n\n"
  writeFile @StoryFS outFile (TE.encodeUtf8 (existing <> sep <> appended))
  return appended

-- ---------------------------------------------------------------------------
-- Git action: read → agent → write + tick via BranchTag Main
-- ---------------------------------------------------------------------------

gitAction
  :: Members '[ LLM StoryModel
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , StoryBranch Main
              , Logging, Fail] r
  => FilePath -> T.Text -> Sem r T.Text
gitAction outFile instruction = do
  existing <- fileExists @(BranchTag Main) outFile >>= \case
    True  -> TE.decodeUtf8 <$> readFile @(BranchTag Main) outFile
    False -> return ""
  appended <- continuationAgent @(BranchTag Main) @StoryModel configs (Just 300) existing instruction
  let sep = if T.null existing || T.isSuffixOf "\n\n" existing then ""
            else if T.isSuffixOf "\n" existing then "\n"
            else "\n\n"
  writeFile @(BranchTag Main) outFile (TE.encodeUtf8 (existing <> sep <> appended))
  _ <- store @Main (T.take 60 instruction)
  return appended
