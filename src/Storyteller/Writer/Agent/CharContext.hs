{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Character context agent.
--
-- Exploring a character branch — listing and reading its files — is
-- genuine work: there's no way to summarize a character without it, so
-- unlike most agents in this folder this one can't be reduced to something
-- FS-free. What it can still avoid is fusing that exploration with
-- rendering: 'readCharFiles' is the (unavoidably effectful) read, and
-- 'renderCharContext' is a plain, pure function over the result — the seam
-- where a future richer summarization/hiding scheme (see
-- @project_context_assembly_design@) plugs in without touching the FS-facing
-- half.
module Storyteller.Writer.Agent.CharContext
  ( charSummaryAgent
  , readCharFiles
  , renderCharContext
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, listAllFiles, readFile)

import Storyteller.Writer.Agent (CharContextBlock(..))

import Prelude hiding (readFile)

-- | Read all files from a character branch's filesystem, sorted by path.
--   The @project@ type parameter is the filesystem phantom for the character
--   branch. The caller is responsible for having that branch's filesystem
--   interpreter in scope.
readCharFiles
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Sem r [(FilePath, T.Text)]
readCharFiles = do
  files <- List.sort <$> listAllFiles @project "/"
  mapM (\path -> (,) path . TE.decodeUtf8 <$> readFile @project path) files

-- | Format read files as labelled blocks: @"### \<path\>\n\n\<content\>"@.
--   Pure — no filesystem access, so it's swappable independent of how the
--   files were obtained.
renderCharContext :: [(FilePath, T.Text)] -> [CharContextBlock]
renderCharContext = map $ \(path, content) ->
  CharContextBlock $ "### " <> T.pack path <> "\n\n" <> content

-- | 'readCharFiles' then 'renderCharContext' — the common case.
charSummaryAgent
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Sem r [CharContextBlock]
charSummaryAgent = renderCharContext <$> readCharFiles @project
