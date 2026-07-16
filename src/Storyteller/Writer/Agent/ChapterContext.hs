{-# LANGUAGE ScopedTypeVariables #-}

-- | Story-branch context for continuing one chapter: the prose of every
-- chapter that comes before it, and its own tick history so far. Both are
-- 'Storyteller.Writer.Library' questions ("which chapter is this, and
-- what's earlier") answered against real file content, which is why this
-- lives beside 'Storyteller.Writer.Agent.WorldContext' rather than inside
-- that pure, IO-free module.
module Storyteller.Writer.Agent.ChapterContext
  ( earlierChaptersOf
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Maybe (mapMaybe)

import qualified Storage.FS as FS

import Storyteller.Writer.Library (LibraryKind(..), UnitInfo(..), classifyPath, buildLibraryTree, narrativeUnits)

-- | Every chapter that comes before @path@ in reading order, oldest-first,
--   paired with its own path and full current prose (not tick history -- a
--   chapter's working-tree content already IS just its atoms' concatenated
--   text, nothing else ever lands in a chapter file, so there's no "strip
--   the prompts back out" step needed the way there would be for a
--   tick-history read). The path travels alongside the prose because
--   'Storyteller.Writer.Agent.Write.buildChapterMessages' needs it to label
--   each chapter's own message pair.
--
--   @[]@ if @path@ doesn't classify as a 'Unit' at all ('Storyteller.
--   Writer.Library.classifyPath') -- writing into some other kind of file
--   has no "earlier chapters" concept, and that's a normal, not an error,
--   case.
earlierChaptersOf :: forall m. FS.StoreM m => FilePath -> FS.StoreT m [(FilePath, T.Text)]
earlierChaptersOf path = case classifyPath path of
  Unit -> do
    files <- FS.list
    let units = narrativeUnits (buildLibraryTree files)
        earlierPaths = takeWhile (/= path) (mapMaybe uiPath units)
    mapM (\p -> (,) p . TE.decodeUtf8 <$> FS.readFile p) earlierPaths
  _ -> return []
