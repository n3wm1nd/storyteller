{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Character context agent.
--
-- Reads all files from a character branch's filesystem and formats them as
-- labelled blocks for inclusion in LLM prompts.
module Storyteller.Writer.Agent.CharContext
  ( charSummaryAgent
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, listAllFiles, readFile)

import Storyteller.Writer.Agent (CharContextBlock(..))

import Prelude hiding (readFile)

-- | Read all files from a character branch's filesystem and return them as
--   labelled blocks: @"### \<path\>\n\n\<content\>"@.
--
--   The @project@ type parameter is the filesystem phantom for the character
--   branch. The caller is responsible for having that branch's filesystem
--   interpreter in scope.
charSummaryAgent
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Sem r [CharContextBlock]
charSummaryAgent = do
  files <- List.sort <$> listAllFiles @project "/"
  mapM (readBlock @project) files

readBlock
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => FilePath
  -> Sem r CharContextBlock
readBlock path = do
  bytes <- readFile @project path
  return $ CharContextBlock $ "### " <> T.pack path <> "\n\n" <> TE.decodeUtf8 bytes
