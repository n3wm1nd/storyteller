{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Composition for the @\/library\/{name}@ connection: the writer-facing
-- organizational view over one branch (books\/chapters\/scenes rather than
-- raw files) — see WS-PROTOCOL.md. Writer-specific in the same way
-- 'Server.Writer.Character' is: it knows the @chapters\/ch{N}.md@ naming
-- convention from WRITER.md, which 'Server.Core.Branch' has no business
-- knowing about.
--
-- Structure and per-file classification are pure (see
-- 'Storyteller.Writer.Library'); this module only adds the one thing that
-- needs the filesystem: each chapter's own first line, read directly off
-- its file. Deliberately just the first line, not the full content — unlike
-- 'Server.Writer.Character.characterState' handing over a whole (small,
-- single) sheet file for the client to pull a heading out of, a library
-- tree may cover many chapters at once, and sending each one's entire prose
-- just to get a display heading would defeat the "stays push-cheap
-- indefinitely" scope test from WS-PROTOCOL.md.
module Server.Writer.Library
  ( libraryTree
  , chapterCreate
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Sem)
import Runix.FileSystem (listAllFiles, readFile)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Git (BranchTag, runStorage)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Writer.Library
  (LibraryNode(..), LibraryKind(..), ChapterUnit, buildLibraryTree, chapterUnits)

import Prelude hiding (readFile)

-- | The full organizational tree for this branch, plus every chapter
--   number already paired with its own chapter file/beat sheet (see
--   'Storyteller.Writer.Library.chapterUnits') — computed here, once, so
--   nothing downstream (this connection's push, and eventually the planned
--   Summarizer agent) has to re-derive "which chapter does this belong to"
--   independently.
libraryTree :: BranchOpen r => Sem r ([LibraryNode], [ChapterUnit])
libraryTree = do
  paths <- listAllFiles @(BranchTag Main) "/"
  tree  <- mapM withHeading (buildLibraryTree paths)
  return (tree, chapterUnits tree)

-- | Fill in 'lnHeading' for chapter nodes (and recurse into folders) —
--   nothing else needs a filesystem read.
withHeading :: BranchOpen r => LibraryNode -> Sem r LibraryNode
withHeading node = case lnKind node of
  Chapter _ -> do
    content <- TE.decodeUtf8 <$> readFile @(BranchTag Main) (lnPath node)
    return node { lnHeading = firstLine content }
  Folder -> do
    children <- mapM withHeading (lnChildren node)
    return node { lnChildren = children }
  _ -> return node

firstLine :: T.Text -> Maybe T.Text
firstLine t = case T.lines t of
  (l : _) | not (T.null l) -> Just l
  _                        -> Nothing

-- | Introduce @path@ as a new chapter file, seeded with its heading — the
--   one thing this connection's own creation command does that generic
--   'Server.Core.File.createFile' doesn't: write @# {name}@ as the file's
--   first line, the same "first H1 line is the display name" convention
--   'sheet.md' already uses (see WRITER.md). Fails on a path that already
--   has ticks, same as 'Server.Core.File.createFile'. Deliberately doesn't
--   validate that @path@ matches the @chapters\/ch{N}.md@ convention —
--   library detection is freeform (see 'Storyteller.Writer.Library'), so a
--   path that doesn't match is still created, just not later recognized as
--   a 'Chapter' node.
chapterCreate :: BranchOpen r => FilePath -> T.Text -> Sem r ()
chapterCreate path name = do
  (existing, _) <- runStorage @Main (Tick.fileTicksOf path)
  case existing of
    [] -> void (runStorage @Main (Ops.addAtom path ("# " <> name <> "\n")))
    _  -> fail ("chapterCreate: already exists: " <> path)
