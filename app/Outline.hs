{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-outline: expand a story outline into a chapter beat sheet.
--
-- Reads @outline.md@ (and every other branch file, as context) and writes a
-- beat sheet to the given output path (by convention @chapters/ch{N}.outline.md@).
-- The beat sheet is disposable scaffolding — see WRITER.md.
--
-- ENV:
--   STORY_REPO          path to the git repository
--   STORY_BRANCH        story branch name
--   LLAMACPP_ENDPOINT   (optional, default http://localhost:8080/v1)
--
-- ARGS:
--   <out>    path to write the beat sheet to (e.g. chapters/ch1.outline.md)
--
-- STDIN: optional — extra instruction narrowing which part of the outline to
--        expand (e.g. "chapter 1"). Prepended to the outline as guidance.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile)
import Runix.Logging (Logging)

import Storyteller.Core.Runtime (Main, runStoryGit, BranchTag(..), Git, BranchOp, runStorage)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage)
import qualified Storage.Ops as Ops
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (ExistingContent(..))
import Storyteller.Writer.Agent.Outline (OutlineDoc(..), ExpandGoal(..), expandAgent)
import Storyteller.Common.Splitter (Splitter, splitAtoms, splitMarkdownAware)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

import Storyteller.Context.DSL.Value (valueDefault)
import qualified Storyteller.Context.DSL.Render as Render
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.Context (ContextStorage, resolveContext1, runContextValue, interpretContextStorageFS)

import Prelude hiding (readFile)

main :: IO ()
main = do
  env     <- loadEnv
  args    <- getArgs
  outFile <- case args of
    [f] -> return f
    _   -> hPutStrLn stderr "Usage: story-outline <out>" >> exitFailure
  guidance <- fmap T.strip TIO.getContents

  result <- runStoryGit
    (envRepo env)
    (envEndpoint env)
    (BranchName (envBranch env))
    modelConfigs
    (interpretPromptStorageFS $ interpretContextStorageFS $ splitMarkdownAware $ outlineAction outFile guidance)

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

outlineAction
  :: (LLMs r, Members '[ PromptStorage
              , ContextStorage
              , FileSystem      (BranchTag Main)
              , FileSystemRead  (BranchTag Main)
              , FileSystemWrite (BranchTag Main)
              , BranchOp Main
              , StoryStorage
              , Splitter
              , Git
              , Logging, Fail] r)
  => FilePath -> T.Text -> Sem r T.Text
outlineAction outFile guidance = do
  -- @outline.md@ is the document being expanded; every other branch file goes
  -- along as surrounding context, via the same @context.writer@ definition
  -- every WS-driven prose path reads through now (see
  -- 'Server.Writer.File.flatMainMessages') -- @outline.md@'s own entry is
  -- already excluded by
  -- 'Storyteller.Context.DSL.Library.contextWriter''s own @path@ parameter.
  -- @outline.md@'s own content is separate, read directly below (it's the
  -- document being expanded, not surrounding context).
  writerVal <- resolveContext1 @Main "context.writer" CtxLibrary.contextWriter "outline.md"
  fileCtx <- map Render.messageToBlock <$> runContextValue @Main (valueDefault writerVal)
  ExistingContent outline <- fileExists @(BranchTag Main) "outline.md" >>= \case
    True  -> ExistingContent . TE.decodeUtf8 <$> readFile @(BranchTag Main) "outline.md"
    False -> return (ExistingContent "")
  let source = OutlineDoc $ case guidance of
        "" -> outline
        g  -> g <> "\n\n" <> outline
  beatSheet <- expandAgent ToBeatSheet fileCtx source
  _ <- mapM (\c -> runStorage @Main (Ops.append outFile c)) =<< splitAtoms beatSheet
  return beatSheet
