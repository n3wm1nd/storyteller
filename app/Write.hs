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
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--
-- ARGS:
--   <file>   path of the file to append to, relative to the branch root
--
-- STDIN: the instruction / prompt for what to write next.
--
-- No active-characters flag any more -- 'writeAgent' reads presence off
-- the branch itself now (see its own Haddock), the same as every other
-- caller; there's no separate CLI-only notion of "active characters" left
-- to plumb through.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.Logging (Logging)

import Storyteller.Core.Branch (Branches, Visited)
import Storyteller.Core.Runtime (Main, runStoryGit, BranchTag(..), Git, BranchOp, runStorage)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (Instruction(..), Prose(..), PastChaptersMode(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Common.Splitter (Splitter, splitAtoms, splitMarkdownAware)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

import Storyteller.Writer.Agent.Context (Lore(..), Other(..), PinnedContext(..))
import Storyteller.Context.DSL.Rendering (RenderedContext(..))
import Storyteller.Core.ContentEffects (BranchResolve)
import Storyteller.Core.Context (ContextStorage, interpretContextStorageFS)
import qualified Storage.Ops as Ops

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
    (interpretPromptStorageFS $ interpretContextStorageFS $ splitMarkdownAware $ writeAction outFile (Instruction instruction))

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

writeAction
  :: (LLMs r, Members '[ PromptStorage
              , ContextStorage
              , BranchResolve
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , BranchOp Main
              , Branches Visited
              , StoryStorage
              , Splitter
              , Git
              , Logging, Fail] r)
  => FilePath -> Instruction -> Sem r T.Text
writeAction outFile instruction = do
  Prose generated <- writeAgent @Main outFile (Lore (Node [] [])) (Other (Node [] [])) FullChapters (PinnedContext (Node [] [])) instruction
  _ <- mapM (\c -> runStorage @Main (Ops.append outFile c)) =<< splitAtoms generated
  return generated
