{-# LANGUAGE OverloadedStrings #-}

-- | The pure "codex" organizational-tree derivation behind @\/lore\/{name}@
-- (see WS-PROTOCOL.md). A path belongs here iff
-- 'Storyteller.Writer.Library.classifyPath' calls it 'OtherFile' — anything
-- recognized as a chapter, beat sheet, or the whole-story outline already
-- has a home in the Library tab, so it's excluded here rather than shown
-- twice — and it isn't under a top-level @chat\/@ folder (conversational
-- scratch space, not curated lore). What's left is exactly the freeform
-- content a user hand-authors: notes, world-building, a style guide.
--
-- Deliberately pure and IO-free, same reasoning as
-- 'Storyteller.Writer.Library.buildLibraryTree': this only needs each
-- eligible path plus a short blurb already read for it, not any further
-- filesystem access of its own. See 'Server.Writer.Lore' for where the file
-- list and each blurb actually get read.
module Storyteller.Writer.Lore
  ( LoreNode(..)
  , isLoreEligible
  , buildLoreTree
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import System.FilePath (splitDirectories)

import Storyteller.Writer.Library (LibraryKind(..), classifyPath)

-- | One node in the codex tree. Unlike 'Storyteller.Writer.Library.LibraryNode'
--   there's no kind/heading/binary bookkeeping to carry — every leaf here is
--   already known to be eligible lore content, and 'lnBlurb' (empty for a
--   folder node) is the only per-file text this tree needs.
data LoreNode = LoreNode
  { lnPath     :: FilePath
  , lnName     :: T.Text
  , lnBlurb    :: T.Text
  , lnChildren :: [LoreNode]
  } deriving (Show, Eq)

-- | Whether @path@ belongs in the codex — see the module Haddock. Exposed on
--   its own so 'Server.Writer.Lore' can filter the branch's file list before
--   reading any content, rather than reading everything and discarding.
isLoreEligible :: FilePath -> Bool
isLoreEligible path =
  classifyPath path == OtherFile && take 1 (splitDirectories path) /= ["chat"]

-- | Build the codex forest from every eligible path paired with its
--   already-read blurb. Folders are synthesized wherever a path implies
--   one, same trie-then-forest shape as
--   'Storyteller.Writer.Library.buildLibraryTree', just carrying a blurb
--   payload instead of chapter/heading bookkeeping this module has no use
--   for. Children sort by name — no chapter-number ordering applies here.
buildLoreTree :: [(FilePath, T.Text)] -> [LoreNode]
buildLoreTree files = toNodes "" (foldr insertPath Map.empty files)

-- ---------------------------------------------------------------------------
-- Internal: a path trie, built up one path at a time, then converted to the
-- public 'LoreNode' forest in one pass.
-- ---------------------------------------------------------------------------

data Trie = Trie
  { trieLeaf     :: Maybe (FilePath, T.Text)
  , trieChildren :: Map FilePath Trie
  }

emptyTrie :: Trie
emptyTrie = Trie Nothing Map.empty

insertPath :: (FilePath, T.Text) -> Map FilePath Trie -> Map FilePath Trie
insertPath (path, blurb) = go (splitDirectories path)
  where
    go []           forest = forest
    go [seg]        forest = Map.insertWith mergeLeaf seg (Trie (Just (path, blurb)) Map.empty) forest
    go (seg : rest) forest =
      let entry = Map.findWithDefault emptyTrie seg forest
      in Map.insert seg (entry { trieChildren = go rest (trieChildren entry) }) forest

    mergeLeaf new old = old { trieLeaf = trieLeaf new }

toNodes :: FilePath -> Map FilePath Trie -> [LoreNode]
toNodes prefix forest = List.sortOn lnName [ mkNode name entry | (name, entry) <- Map.toList forest ]
  where
    mkNode name (Trie mLeaf children)
      | Map.null children, Just (path, blurb) <- mLeaf =
          LoreNode path (T.pack name) blurb []
      | otherwise =
          let fullPath = if null prefix then name else prefix <> "/" <> name
          in LoreNode fullPath (T.pack name) "" (toNodes fullPath children)
