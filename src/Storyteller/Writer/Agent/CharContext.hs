{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Character context agent.
--
-- __Status: unused in production as of 2026-07-20.__ Every real caller
-- (ambient scene generation, 'Storyteller.Writer.Agent.AskCharacter',
-- 'Storyteller.Writer.Agent.Roleplay') now reads character context through
-- 'Storyteller.Context.DSL.Library.characterSummaryOf' instead — see that
-- module's Haddock for the four/five-bucket ('"sheet"'\/'"blurb"'\/
-- '"full"'\/'"journal"'\/'"journalFull"') shape this module's functions
-- collapse to. Kept anyway (2026-07-23): 'readCharFiles'\/'renderCharContext'\/
-- 'charSummaryAgent'\/'charSummaryFull' are already written against
-- @Runix.FileSystem@'s portable 'Runix.FileSystem.FileSystem'\/
-- 'Runix.FileSystem.FileSystemRead' effects, not 'Storage.Core.StoreT' --
-- @agent-integration@'s own @CharContextWriteSpec@ exercises exactly that,
-- running 'readCharFiles' against a plain, non-git
-- @Runix.FileSystem.System.filesystemIO@ interpreter, which is the one
-- concrete proof anywhere in this codebase that an agent-facing read
-- genuinely works on a non-git backend -- worth keeping for that reason
-- alone, unused in production or not. @charSummaryWithJournal@, the one
-- function here that stayed on raw 'Storage.Core.StoreT' (deliberately, to
-- batch every read into one 'Storyteller.Core.Git.runStorage' call -- see
-- its own former Haddock), was removed instead of converted: nothing calls
-- it in production either, and the tests that did have been rewired to
-- build a 'Storyteller.Writer.Agent.CharSummary' the same way
-- 'Storyteller.Writer.Agent.Write.activeCharacterContext' now does (@resolveContext1@ +
-- 'Storyteller.Context.DSL.Library.contextCharacter' +
-- 'Storyteller.Context.DSL.Library.characterSummaryOf').
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
  , charSummaryFull
  , readCharFiles
  , renderCharContext
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeFileName)

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, listAllFiles, readFile)

import Storyteller.Context.DSL.Value (Message(..))
import Storyteller.Writer.Agent (CharSummary(..))

import Prelude hiding (readFile)

-- | Read files from a character branch's filesystem matching @keep@, sorted
--   by path. The @project@ type parameter is the filesystem phantom for the
--   character branch. The caller is responsible for having that branch's
--   filesystem interpreter in scope.
--
--   @keep@ is the caller's call, not a default this module picks: a
--   generation call gathering ambient context for every active character
--   (see 'Server.Writer.File.activeCharacterContext') wants their journal
--   excluded (long, mostly a copy of what's already in the scene's own
--   history, and not written for a narrator to read), while an explicit
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent' query wants
--   everything, journal included -- there is no one "right" filter for
--   "a character's files" independent of who's asking.
readCharFiles
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => (FilePath -> Bool) -> Sem r [(FilePath, T.Text)]
readCharFiles keep = do
  files <- filter keep . List.sort <$> listAllFiles @project "/"
  mapM (\path -> (,) path . TE.decodeUtf8 <$> readFile @project path) files

-- | Format read files as labelled blocks: @"### \<path\>\n\n\<content\>"@.
--   Pure — no filesystem access, so it's swappable independent of how the
--   files were obtained.
renderCharContext :: [(FilePath, T.Text)] -> [Message]
renderCharContext = map $ \(path, content) ->
  User $ "### " <> T.pack path <> "\n\n" <> content

-- | 'readCharFiles' then 'renderCharContext' — the common case.
charSummaryAgent
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => (FilePath -> Bool) -> Sem r [Message]
charSummaryAgent keep = renderCharContext <$> readCharFiles @project keep

-- | 'readCharFiles' split into a 'CharSummary' by exact filename --
--   @sheet.md@, @journal.md@, everything else: @journal.md@ here is read
--   verbatim and in full, straight off the filesystem, same as any other
--   file 'readCharFiles' already reads -- unlike
--   'Storyteller.Context.DSL.Library.characterSummaryOf's curated
--   @"journal"@ bucket, this is for a caller that wants a character's
--   whole, uncurated context, still split by how often each part actually
--   changes -- @csJournal@ grows every turn, @csSheet@\/@csContext@
--   don't -- so it can place the volatile part in its own late message
--   rather than fuse it into an otherwise-stable one; see
--   'Storyteller.Writer.Agent.Roleplay''s own opening-message construction
--   for why that split is what actually protects a prompt-cache hit.
charSummaryFull
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => (FilePath -> Bool) -> Sem r CharSummary
charSummaryFull keep = categorize <$> readCharFiles @project keep
  where
    categorize files = CharSummary
      { csSheet   = renderCharContext (filter (named "sheet.md") files)
      , csContext = renderCharContext (filter (\f -> not (named "sheet.md" f || named "journal.md" f)) files)
      , csJournal = renderCharContext (filter (named "journal.md") files)
      }
    named n (p, _) = takeFileName p == n

