{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Character context loader.
--
-- Reads all files from a character branch and formats them as labelled
-- text blocks for inclusion in the continuation agent's context.
module Storyteller.Agent.CharContext
  ( loadCharContext
  ) where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, getCwd, listFiles, readFile)

import Prelude hiding (readFile)

-- | Read all files from a character branch's filesystem and return them as
--   labelled blocks: @"### \<path\>\n\n\<content\>"@.
--
--   The @project@ type parameter is the filesystem phantom for this character
--   branch (e.g. @BranchTag CharBranch@). The caller is responsible for
--   having the branch's filesystem interpreter in scope.
loadCharContext
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Sem r [Text]
loadCharContext = do
  cwd   <- getCwd @project
  files <- List.sort <$> listFiles @project cwd
  mapM (readBlock @project) files

readBlock
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => FilePath
  -> Sem r Text
readBlock path = do
  bytes <- readFile @project path
  return $ "### " <> T.pack path <> "\n\n" <> TE.decodeUtf8 bytes
