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
                        , fileExists, readFile, writeFile )
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Prelude hiding (readFile, writeFile)

import Storyteller.Runtime ( Main, StoryModel, runStoryGitIO
                           , BranchTag(..), WorkingTree, State, Git
                           , runStoryFSGit, runStoryBranchGit )
import Storyteller.Storage (StoryBranch, StoryStorage, store)
import Storyteller.Types (BranchName(..))
import Storyteller.Agent.Continuation (continuationAgent)
import Storyteller.Agent.CharContext (loadCharContext)
import Storyteller.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

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

  result <- runStoryGitIO
    (envEndpoint env)
    (envRepo env)
    (BranchName (envBranch env))
    modelConfigs
    (writeAction outFile instruction (envActiveChars env))

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
              , Git
              , State WorkingTree
              , Logging, Fail] r
  => FilePath -> T.Text -> [T.Text] -> Sem r T.Text
writeAction outFile instruction activeChars = do
  charContexts <- fmap concat $ forM activeChars $ \charBranch -> do
    let branchName = BranchName charBranch
    blocks <- runStoryFSGit @Char_ branchName
            $ runStoryBranchGit @Char_ branchName
            $ loadCharContext @(BranchTag Char_)
    return $ ("## Character: " <> charBranch) : blocks

  existing <- fileExists @(BranchTag Main) outFile >>= \case
    True  -> TE.decodeUtf8 <$> readFile @(BranchTag Main) outFile
    False -> return ""
  appended <- continuationAgent @(BranchTag Main) @StoryModel
                modelConfigs (Just 300) charContexts existing instruction
  let sep = if T.null existing || T.isSuffixOf "\n\n" existing then ""
            else if T.isSuffixOf "\n" existing then "\n"
            else "\n\n"
  writeFile @(BranchTag Main) outFile (TE.encodeUtf8 (existing <> sep <> appended))
  _ <- store @Main (T.take 60 instruction)
  return appended
