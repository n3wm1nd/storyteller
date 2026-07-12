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
  , charSummaryWithJournal
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, listAllFiles, readFile)

import qualified Storage.Core as Core
import qualified Storage.FS as FS
import qualified Storage.Tick as Tick

import Storyteller.Writer.Agent (CharContextBlock(..), CharSummary(..))

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
renderCharContext :: [(FilePath, T.Text)] -> [CharContextBlock]
renderCharContext = map $ \(path, content) ->
  CharContextBlock $ "### " <> T.pack path <> "\n\n" <> content

-- | 'readCharFiles' then 'renderCharContext' — the common case.
charSummaryAgent
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => (FilePath -> Bool) -> Sem r [CharContextBlock]
charSummaryAgent keep = renderCharContext <$> readCharFiles @project keep

-- | 'charSummaryAgent's read, split into 'CharSummary's three independently
--   placeable shapes, plus a curated slice of @journalPath@'s own recent
--   atom history (see 'Storage.Tick.recentAtomsOf') -- all composed into
--   one 'Core.StoreT' computation rather than several calls a caller would
--   otherwise dispatch back-to-back: one 'Storyteller.Core.Git.runStorage'
--   pays for every read here.
--
--   Lives at the 'Core.StoreT' level directly (unlike 'charSummaryAgent',
--   which goes through the 'FileSystem' Polysemy effects) precisely so it
--   composes this way; a caller opens the branch scope once (e.g. via
--   'Storyteller.Core.Git.runBranchOpGit') and passes this straight to
--   'Storyteller.Core.Git.runStorage'.
--
--   If a caller only ever wanted 'csSheet' (see its own Haddock on
--   'CharSummary' for when that's the right call), reach for
--   'readCharFiles'\/'charSummaryAgent' directly instead of computing all
--   three shapes here and discarding two of them -- this function's whole
--   point is composing reads a caller actually needs together, not being
--   the one path in for a single file.
charSummaryWithJournal
  :: forall m
  .  Core.StoreM m
  => FilePath             -- ^ sheet path, e.g. @"sheet.md"@ -- included verbatim if present
  -> FilePath             -- ^ journal path, e.g. @"journal.md"@
  -> (FilePath -> Bool)   -- ^ which other files to include (caller's layout policy; never sheet or journal, regardless of what it answers for either)
  -> Int                  -- ^ lookback: max journal atoms to examine (see 'Tick.recentAtomsOf')
  -> Int                  -- ^ maxOut: max journal atoms to include
  -> Int                  -- ^ padding: journal atoms kept on each side of a kept one
  -> Core.StoreT m CharSummary
charSummaryWithJournal sheetPath journalPath keep lookback maxOut padding = do
  files <- List.sort <$> FS.list
  let otherFiles = [ p | p <- files, p /= sheetPath, p /= journalPath, keep p ]
  sheetCtx   <- if sheetPath `elem` files
                  then renderCharContext . (: []) <$> readPair sheetPath
                  else return []
  contextCtx <- renderCharContext <$> mapM readPair otherFiles
  journal    <- Tick.recentAtomsOf journalPath lookback maxOut padding
  return CharSummary
    { csSheet   = sheetCtx
    , csContext = contextCtx
    , csJournal = renderJournalContext journal
    }
  where
    readPair p = (,) p . TE.decodeUtf8 <$> Core.readFile p

-- | A non-empty journal slice becomes one block, not one per atom: the
--   header names what this is (so a model doesn't mistake it for
--   objective narration) once, and the kept atoms -- which may span real
--   timeline gaps, since unremarkable ones in between were dropped -- are
--   joined by a plain divider rather than left looking like one
--   continuous entry.
renderJournalContext :: [Tick.FileTick] -> [CharContextBlock]
renderJournalContext [] = []
renderJournalContext ticks =
  [ CharContextBlock $
      "### From this character's own journal (their private viewpoint -- may be biased, outdated, or contradict the wider record)\n\n"
      <> T.intercalate "\n\n---\n\n" (map Tick.ftMessage ticks)
  ]
