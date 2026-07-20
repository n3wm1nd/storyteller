{-# LANGUAGE OverloadedStrings #-}

-- | The pure "codex" organizational-tree derivation behind @\/lore\/{name}@
-- (see WS-PROTOCOL.md) — one uniform "get me all the lore for this branch"
-- definition, used identically for the main story branch and for a
-- @character\/*@ branch (see 'Server.Writer.File.activeCharacterContext',
-- the real generation-time consumer for the character case). A path belongs
-- here iff 'Storyteller.Writer.Library.classifyPath' calls it 'OtherFile' —
-- anything recognized as a chapter, beat sheet, or the whole-story outline
-- already has a home in the Library tab, so it's excluded here rather than
-- shown twice — it isn't under a top-level @chat\/@ folder (conversational
-- scratch space, not curated lore), and it isn't a character's @sheet.md@ or
-- @journal.md@ at the branch root (WRITER.md's convention): a sheet is core
-- identity delivered unconditionally through a different path, never a
-- codex entry, and a journal is excluded from generation context entirely,
-- also never a codex entry. Applying that same basename exclusion
-- unconditionally to a story branch is harmless — a story branch practically
-- never has a @sheet.md@, and a @journal.md@ there would just be manual
-- notes with no special meaning, fine to leave out of the codex too. What's
-- left is exactly the freeform content a user hand-authors: notes,
-- world-building, a style guide, or (on a character branch) any extra file
-- beyond the sheet/journal.
--
-- Deliberately pure and IO-free, same reasoning as
-- 'Storyteller.Writer.Library.buildLibraryTree': this only needs each
-- eligible path plus a short blurb already read for it, not any further
-- filesystem access of its own. See 'Server.Writer.Lore' for where the file
-- list and each blurb actually get read.
module Storyteller.Writer.Lore
  ( LoreNode(..)
  , isLoreEligible
  , isNotScratchOrCharacterFile
  , buildLoreTree
  , parseAliases
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import System.FilePath (splitDirectories, takeFileName, takeDirectory)

import Storyteller.Writer.Library (LibraryKind(..), classifyPath)

-- | One node in the codex tree. Unlike 'Storyteller.Writer.Library.LibraryNode'
--   there's no kind/heading/binary bookkeeping to carry — every leaf here is
--   already known to be eligible lore content, and 'lnBlurb'\/'lnAliases'
--   (empty for a folder node) are the only per-file data this tree needs.
data LoreNode = LoreNode
  { lnPath     :: FilePath
  , lnName     :: T.Text
  , lnBlurb    :: T.Text
  , lnAliases  :: [T.Text]
  , lnChildren :: [LoreNode]
  } deriving (Show, Eq)

-- | Alternate names a lore entry is known by, for keyword-triggered context
--   inclusion (see the frontend's lore-mentions design) — parsed from a
--   plain markdown line, no bespoke syntax: a @**Aliases:** a, b, c@ line
--   (bold label, case-insensitive, comma-separated) anywhere in the file's
--   /first section/ — from its @# Title@ down to the next heading of any
--   level, or end of file if there isn't one. Scoped to the first section
--   on purpose: the same label text appearing in, say, a "## History"
--   section discussing what a character used to be called is prose, not a
--   declaration, and this convention is deliberately meant to generalize
--   later to other labelled lines (e.g. a future @**Tags:**@) sharing the
--   same "only the intro section counts" rule.
parseAliases :: T.Text -> [T.Text]
parseAliases content =
  case Maybe.mapMaybe aliasValue firstSection of
    []            -> []
    (value : _)   -> filter (not . T.null) (map T.strip (T.splitOn "," value))
  where
    ls = T.lines content
    firstSection = case break isHeading ls of
      (_, h : rest) | isHeading h -> takeWhile (not . isHeading) rest
      _                           -> ls
    isHeading l = "#" `T.isPrefixOf` T.stripStart l
    -- Bold markers are just emphasis, not content -- drop every "**" before
    -- matching the "label: value" shape, so "**Aliases:** a, b" and a plain
    -- "Aliases: a, b" line are recognized identically.
    aliasValue l =
      let (label, rest) = T.breakOn ":" (T.replace "**" "" (T.strip l))
      in if T.toLower (T.strip label) == "aliases" && not (T.null rest)
           then Just (T.drop 1 rest)
           else Nothing

-- | Whether @path@ belongs in the codex — see the module Haddock. Exposed on
--   its own so 'Server.Writer.Lore' can filter the branch's file list before
--   reading any content, rather than reading everything and discarding.
--
--   Deliberately narrower than what a model sees as story-wide reference
--   material (see 'Storyteller.Context.DSL.Library.contextMain', where an
--   outline or beat sheet belongs same as any other hand-authored file) --
--   this one's job is "what gets its own codex entry", where an outline
--   already has a home in the Library tab and showing it again here would
--   just be a duplicate.
isLoreEligible :: FilePath -> Bool
isLoreEligible path = classifyPath path == OtherFile && isNotScratchOrCharacterFile path

-- | The same two exclusions 'Storyteller.Context.DSL.Library.contextMain'
--   applies (@exclude("chat/**/*")@, and a character's own sheet\/journal
--   never showing up outside their own branch) -- chat scratch space and a
--   character's root-level sheet\/journal are never lore under either
--   notion of the word.
isNotScratchOrCharacterFile :: FilePath -> Bool
isNotScratchOrCharacterFile path =
  take 1 (splitDirectories path) /= ["chat"]
    && not (isRootFile "sheet.md" path)
    && not (isRootFile "journal.md" path)
  where
    isRootFile name p = takeFileName p == name && takeDirectory p == "."

-- | Build the codex forest from every eligible path paired with its
--   already-read blurb and parsed aliases. Folders are synthesized wherever
--   a path implies one, same trie-then-forest shape as
--   'Storyteller.Writer.Library.buildLibraryTree', just carrying a
--   blurb\/aliases payload instead of chapter/heading bookkeeping this
--   module has no use for. Children sort by name — no chapter-number
--   ordering applies here.
buildLoreTree :: [(FilePath, T.Text, [T.Text])] -> [LoreNode]
buildLoreTree files = toNodes "" (foldr insertPath Map.empty files)

-- ---------------------------------------------------------------------------
-- Internal: a path trie, built up one path at a time, then converted to the
-- public 'LoreNode' forest in one pass.
-- ---------------------------------------------------------------------------

data Trie = Trie
  { trieLeaf     :: Maybe (FilePath, T.Text, [T.Text])
  , trieChildren :: Map FilePath Trie
  }

emptyTrie :: Trie
emptyTrie = Trie Nothing Map.empty

insertPath :: (FilePath, T.Text, [T.Text]) -> Map FilePath Trie -> Map FilePath Trie
insertPath leaf@(path, _, _) = go (splitDirectories path)
  where
    go []           forest = forest
    go [seg]        forest = Map.insertWith mergeLeaf seg (Trie (Just leaf) Map.empty) forest
    go (seg : rest) forest =
      let entry = Map.findWithDefault emptyTrie seg forest
      in Map.insert seg (entry { trieChildren = go rest (trieChildren entry) }) forest

    mergeLeaf new old = old { trieLeaf = trieLeaf new }

toNodes :: FilePath -> Map FilePath Trie -> [LoreNode]
toNodes prefix forest = List.sortOn lnName [ mkNode name entry | (name, entry) <- Map.toList forest ]
  where
    mkNode name (Trie mLeaf children)
      | Map.null children, Just (path, blurb, aliases) <- mLeaf =
          LoreNode path (T.pack name) blurb aliases []
      | otherwise =
          let fullPath = if null prefix then name else prefix <> "/" <> name
          in LoreNode fullPath (T.pack name) "" [] (toNodes fullPath children)
