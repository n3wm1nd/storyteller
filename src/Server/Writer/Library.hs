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
-- 'Storyteller.Writer.Library'); this module adds the two things that
-- can't be: the branch's current file list ('listAllFiles'), and each
-- chapter's own heading. The heading is folded incrementally over the tick
-- chain via 'Storage.Core.memoFold' ('ChapterContentCache') rather than
-- re-read off every chapter file on every push — a chapter's whole content
-- lives directly in its own commit message (see 'Storage.Core.readTick'),
-- so the fold needs no filesystem access at all, just commit reads, and
-- 'memoFold' means only the ticks that actually landed since the last push
-- get looked at, not every chapter, every time. See WS-PROTOCOL.md's
-- "stays push-cheap indefinitely" test — this is what actually keeps a
-- library tree covering many chapters cheap to re-push, not just narrowing
-- the payload to one line per chapter.
module Server.Writer.Library
  ( ChapterContentCache
  , libraryTree
  , chapterCreate
  ) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Polysemy (Sem)
import Runix.FileSystem (listAllFiles)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Git (BranchTag, runStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Writer.Library
  (LibraryNode(..), LibraryKind(..), ChapterUnit, buildLibraryTree, chapterUnits, classifyPath)

-- | Every chapter path's currently-known full content, keyed by path —
--   the 'memoFold' accumulator. Full content, not just the first line: a
--   plain sequential fold can't tell in advance whether a later tick will
--   turn out to be the file's first line (an edit that replaces its very
--   first atom, however rare), so it tracks the same thing
--   'Storage.Ops.atomHistory' would reconstruct from scratch — 'firstLine'
--   is only taken at the point a heading is actually needed.
type ChapterContentCache = Map FilePath T.Text

-- | The memoFold step: only atoms whose path classifies as a chapter
--   contribute; everything else (a 'NonAtom', or an atom for some other
--   kind of path) leaves the cache untouched. No 'Storage.Core.StoreT'
--   capability is actually exercised here — an atom's content lives in its
--   own commit message (see 'Storage.Core.readTick'), not a separate blob
--   'Storage.Core.readFile' would have to fetch — but the step still has
--   to be monadic in 'StoreT' to satisfy 'Core.memoFold's own signature.
foldChapterContent :: Core.StoreM m => ChapterContentCache -> Core.ObjectHash -> Core.Tick -> Core.StoreT m ChapterContentCache
foldChapterContent acc _h tick = return $ case tick of
  Core.Atom _ path content | Chapter _ <- classifyPath path ->
    Map.insertWith (flip (<>)) path content acc
  _ -> acc

-- | The full organizational tree for this branch, plus every chapter
--   number already paired with its own chapter file/beat sheet (see
--   'Storyteller.Writer.Library.chapterUnits') — computed here, once, so
--   nothing downstream (this connection's push, and eventually the planned
--   Summarizer agent) has to re-derive "which chapter does this belong to"
--   independently. Takes and returns a 'ChapterContentCache' — the memoized
--   heading fold's own checkpoint set, to be threaded straight through into
--   the next call (see 'Server.Writer.Library.Connection', which persists
--   it across repeated ref-move pushes the same way it already threads its
--   own file-set accumulator).
libraryTree
  :: BranchOpen r
  => [(Core.ObjectHash, ChapterContentCache)]
  -> Sem r ([LibraryNode], [ChapterUnit], [(Core.ObjectHash, ChapterContentCache)])
libraryTree cache = do
  paths <- listAllFiles @(BranchTag Main) "/"
  ((content, nextCache), _) <- runStorage @Main (Core.memoFold foldChapterContent Map.empty cache)
  let tree = withHeadings content (buildLibraryTree paths)
  return (tree, chapterUnits tree, nextCache)

-- | Fill in 'lnHeading' for chapter nodes (and recurse into folders) from
--   the already-folded content cache — a pure lookup, no filesystem or
--   'StoreT' access needed at this point.
withHeadings :: ChapterContentCache -> [LibraryNode] -> [LibraryNode]
withHeadings content = map go
  where
    go n = case lnKind n of
      Chapter _ -> n { lnHeading = Map.lookup (lnPath n) content >>= firstLine }
      Folder    -> n { lnChildren = withHeadings content (lnChildren n) }
      _         -> n

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
