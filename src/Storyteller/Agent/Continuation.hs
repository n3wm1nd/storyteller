{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The continuation agent: given existing content and an instruction,
--   reads context files from a filesystem, then asks the LLM what text
--   to append next.
--
--   Deliberately read-only and model-agnostic. The caller is responsible
--   for reading the target file before calling and appending the result after.
module Storyteller.Agent.Continuation
  ( continuationAgent
  ) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, listFiles, readFile, getCwd)
import Runix.LLM (LLM, queryLLM)
import UniversalLLM (Message(..), ModelConfig(..))

import Prelude hiding (readFile)

-- | Ask the LLM what text to append, given:
--   - @configs@:   model configuration
--   - @context@:   all files in the filesystem as labelled blocks (already read by caller)
--                  these are read from the provided filesystem
--   - @existing@:  current content of the file being continued (may be empty)
--   - @instruction@: what the user wants written next
--
--   Returns only the new text to append — never a full rewrite.
continuationAgent
  :: forall project model r
  .  Members '[FileSystem project, FileSystemRead project, LLM model, Fail] r
  => [ModelConfig model]
  -> Maybe Int   -- ^ approximate desired output length in words (hint to the LLM)
  -> Text        -- ^ current content of the file being continued
  -> Text        -- ^ user instruction
  -> Sem r Text
continuationAgent configs outputHint existing instruction = do
  cwd   <- getCwd @project
  files <- fmap List.sort $ listFiles @project cwd

  contextBlocks <- mapM (readContextFile @project) files

  let contextSection
        | null contextBlocks = ""
        | otherwise =
            "Context files:\n\n"
            <> T.intercalate "\n\n" contextBlocks
            <> "\n\n"

      existingSection
        | T.null existing = "The file is currently empty.\n\n"
        | otherwise       = "File to continue:\n\n" <> existing <> "\n\n"

      lengthHint = case outputHint of
        Nothing -> ""
        Just n  -> "Write approximately " <> T.pack (show n) <> " words.\n"

      userMsg = contextSection
             <> existingSection
             <> "Instruction: " <> instruction <> "\n\n"
             <> lengthHint
             <> "Write only the new text to append. Do not repeat or summarise existing content."

  response <- queryLLM @model configs [UserText userMsg]
  return $ mconcat [ t | AssistantText t <- response ]

readContextFile
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => FilePath
  -> Sem r Text
readContextFile path = do
  bytes <- readFile @project path
  return $ "### " <> T.pack path <> "\n\n" <> TE.decodeUtf8 bytes
