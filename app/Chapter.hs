{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-chapter: generate a chapter's prose from its beat sheet.
--
-- Reads the beat sheet, generates prose with one of the two drivers under
-- trial (whole-chapter in a single call, or an LLM-driven beat-by-beat loop),
-- and appends the result to the chapter file. Both read the same free-form
-- beat sheet and share the same prose core — the STORY_CHAPTER_MODE env var
-- selects which, so the two can be compared on identical input. See WRITER.md.
--
-- ENV:
--   STORY_REPO           path to the git repository
--   STORY_BRANCH         story branch name
--   ACTIVE_CHARS         comma-separated character branch names (optional)
--   STORY_CHAPTER_MODE   "whole" (default) or "bybeat"
--   LLAMACPP_ENDPOINT    (optional, default http://localhost:8080/v1)
--
-- ARGS:
--   <beatsheet>   path to the beat sheet (e.g. chapters/ch1.outline.md)
--   <out>         chapter file to append prose to (e.g. chapters/ch1.md)
module Main (main) where

import Control.Monad (forM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile)
import Runix.Logging (Logging)

import Storyteller.Core.Runtime (Main, runStoryGit, BranchTag(..), Git, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage, interpretPromptStorageFS)
import Storyteller.Core.Storage (StoryStorage)
import qualified Storage.Ops as Ops
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent
  (Prose(..), CharLabel(..), CharContextBlock(..), WordCount(..), ExistingContent(..))
import Storyteller.Writer.Agent.CharContext (charSummaryAgent)
import Storyteller.Writer.Agent.Outline (BeatSheet(..), chapterProse, chapterProseByBeat)
import Storyteller.Common.Splitter (Splitter, splitAtoms, splitMarkdownAware)
import Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv, modelConfigs)

import Storyteller.Context.DSL.Value (namedEntry)
import Storyteller.Context.DSL.Context (toContext, runContext)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.Context (ContextStorage, resolveContextQuery, runContextBinding1, runContextValue, interpretContextStorageFS)

import Prelude hiding (readFile)

-- | Phantom tag for character branches opened temporarily within the action.
data Char_

-- | Which prose driver to run.
data Mode = Whole | ByBeat

main :: IO ()
main = do
  env  <- loadEnv
  args <- getArgs
  (sheetPath, outFile) <- case args of
    [s, o] -> return (s, o)
    _      -> hPutStrLn stderr "Usage: story-chapter <beatsheet> <out>" >> exitFailure
  mode <- lookupEnv "STORY_CHAPTER_MODE" >>= \case
    Just "bybeat" -> return ByBeat
    _             -> return Whole

  result <- runStoryGit
    (envRepo env)
    (envEndpoint env)
    (BranchName (envBranch env))
    modelConfigs
    (interpretPromptStorageFS $ interpretContextStorageFS $ splitMarkdownAware
      $ chapterAction mode sheetPath outFile (envActiveChars env))

  case result of
    Left err   -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right text -> TIO.putStrLn text

chapterAction
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
  => Mode -> FilePath -> FilePath -> [T.Text] -> Sem r T.Text
chapterAction mode sheetPath outFile activeChars = do
  sheet <- fileExists @(BranchTag Main) sheetPath >>= \case
    True  -> BeatSheet . TE.decodeUtf8 <$> readFile @(BranchTag Main) sheetPath
    False -> fail ("beat sheet not found: " <> sheetPath)

  charBlocks <- forM activeChars $ \charBranch -> do
    blocks <- runBranchAndFS @Char_ (BranchName charBranch)
            $ charSummaryAgent @(BranchTag Char_) (const True)
    return (CharLabel charBranch, blocks)

  -- Same @context.main@ definition every WS-driven prose path reads
  -- through now -- see 'Server.Writer.File.flatMainMessages'. @outFile@'s
  -- own current content is separate (its own "existing prose to build
  -- on" argument below, never part of context.main's own buckets). Kept
  -- as real, model-agnostic Context DSL messages (not flattened
  -- 'ContextBlock' text) all the way into 'chapterProse'\/'chapterProseByBeat',
  -- which now bind them to a concrete model only inside 'proseAgent'.
  mainBinding <- resolveContextQuery "context.main" (CtxLibrary.toBinding1 CtxLibrary.contextQuery) Nothing
  mainVal     <- runContextBinding1 @Main mainBinding (T.pack outFile)
  fileCtx <- runContextValue @Main $ runContext $
       toContext (namedEntry "lore" mainVal)
    <> toContext (namedEntry "chapters" mainVal)
    <> toContext (namedEntry "other" mainVal)
  existing <- fileExists @(BranchTag Main) outFile >>= \case
    True  -> ExistingContent . TE.decodeUtf8 <$> readFile @(BranchTag Main) outFile
    False -> return (ExistingContent "")
  let charContexts = concatMap
        (\(CharLabel name, bs) -> CharContextBlock ("## Character: " <> name) : bs)
        charBlocks

  Prose generated <- case mode of
    Whole  -> chapterProse (Just (WordCount 1200))
                charContexts fileCtx existing sheet
    ByBeat -> chapterProseByBeat (Just (WordCount 300))
                charContexts fileCtx existing sheet maxBeats

  _ <- mapM (\c -> runStorage @Main (Ops.append outFile c)) =<< splitAtoms generated
  return generated
  where
    maxBeats = 40 :: Int
