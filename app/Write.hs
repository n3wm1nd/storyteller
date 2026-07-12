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
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.Logging (Logging)

import Storyteller.Core.Runtime ( Main, runStoryGit
                           , BranchTag(..), Git, BranchOp, runBranchAndFS, runStorage )
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharLabel(..), CharSummary(..))
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.CharContext (charSummaryAgent)
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Common.Splitter (Splitter, splitAtoms, splitMarkdownAware)
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
    (interpretPromptStorageFS $ splitMarkdownAware $ writeAction outFile (Instruction instruction) (envActiveChars env))

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

writeAction
  :: (LLMs r, Members '[ PromptStorage
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , BranchOp Main
              , StoryStorage
              , Splitter
              , Git
              , Logging, Fail] r)
  => FilePath -> Instruction -> [T.Text] -> Sem r T.Text
writeAction outFile instruction activeChars = do
  charBlocks <- forM activeChars $ \charBranch -> do
    let branchName = BranchName charBranch
    blocks <- runBranchAndFS @Char_ branchName
            $ charSummaryAgent @(BranchTag Char_) (const True)
    return (CharLabel charBranch, CharSummary { csSheet = [], csContext = blocks, csJournal = [] })

  (_existing, fileCtx) <- gatherFileContext @(BranchTag Main) [] outFile
  currentTicks <- runStorage @Main (Tick.fileTicksOf outFile)
  Prose generated <- writeAgent [] [] charBlocks fileCtx [] currentTicks instruction
  _ <- mapM (\c -> runStorage @Main (Ops.append outFile c)) =<< splitAtoms generated
  return generated
