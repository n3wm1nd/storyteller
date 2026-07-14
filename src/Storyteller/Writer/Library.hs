{-# LANGUAGE OverloadedStrings #-}

-- | The pure book\/chapter\/scene organizational-tree derivation behind
-- @\/library\/{name}@ (see WS-PROTOCOL.md). Detection is deliberately
-- permissive: a path is recognized as prose iff /some/ segment of it — an
-- ancestor directory name, or the leaf's own filename stem — contains one of
-- a small fixed set of marker words (see WRITER.md's "Story structure" for
-- the authoritative list; keep the two in sync if it changes). No fixed
-- folder name and no fixed depth — this is what lets a book be one flat file
-- (@01 - the first book.md@) for one author and a deep book\/arc\/chapter\/
-- scene tree
-- (@books\/01 - the first book\/arc1\/chapters\/chapter 1 - the awakening\/story.md@)
-- for another, with the identical rule. Sibling ordering is natural-sort
-- (@ch2@ before @ch11@, @2 - the sequel@ before @14 - the finale@ — see
-- 'naturalKey'), not plain string order and not a parsed, stored "chapter
-- number" either: it's purely a comparator, nothing here ever attaches
-- numeric identity to a node or uses a number to pair anything.
--
-- Misclassification is deliberately low-stakes: a file that doesn't match
-- the heuristic is still a real, usable file (still read as plain context,
-- alphabetically) — it just won't show up in the Library tree or get
-- hierarchically summarized until renamed/moved into a recognized shape.
-- This is a convenience heuristic, not a schema, same spirit as WRITER.md's
-- "not a schema" framing for outlines/beat sheets.
--
-- Deliberately pure and IO-free: 'buildLibraryTree' only needs the branch's
-- file *paths*, not their content, so it composes without touching any
-- effect stack. 'Storyteller.Writer.Agent.ChapterSummarizer' (the
-- Summarizer agent) reuses 'classifyPath' directly instead of re-deriving
-- it — see 'Server.Writer.Library', the one UI caller, for where file
-- *content* (a chapter's own heading) gets folded in afterward.
module Storyteller.Writer.Library
  ( LibraryKind(..)
  , LibraryNode(..)
  , UnitInfo(..)
  , classifyPath
  , buildLibraryTree
  , narrativeUnits
  ) where

import Data.Char (isAlpha, isDigit)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (splitDirectories, takeFileName, takeDirectory, dropExtensions)

-- | What convention (if any) a single file path matches. 'Folder' is never
--   produced by 'classifyPath' itself — it's assigned structurally, to any
--   node 'buildLibraryTree' had to synthesize as an ancestor of some deeper
--   path.
data LibraryKind
  = Folder
  | Unit         -- ^ A real prose leaf — a chapter, a scene, a whole book-in-one-file.
  | UnitOutline  -- ^ A container's own beat sheet (@outline.md@), or a same-stem sibling outline (@{stem}.outline.md@).
  | OtherFile    -- ^ No marker word anywhere on this path — still a real node, just not treated as prose.
  deriving (Show, Eq)

-- | One node in the organizational tree. 'lnHeading' is always 'Nothing'
--   here — populating it needs the file's own content, which is
--   'Server.Writer.Library.libraryTree's job, not this pure module's.
--   'lnBinary' is always 'False' here for the same reason: whether a path
--   has any atom history at all needs the tick chain, which this pure,
--   IO-free module never touches (see the module Haddock) — it's filled
--   in effectfully alongside the heading, so the file tree can tell the
--   client "don't try to open a prose/atom viewer on this one."
data LibraryNode = LibraryNode
  { lnPath     :: FilePath
  , lnName     :: T.Text
  , lnKind     :: LibraryKind
  , lnBinary   :: Bool
  , lnHeading  :: Maybe T.Text
  , lnChildren :: [LibraryNode]
  } deriving (Show, Eq)

-- | The fixed marker vocabulary — see the module Haddock and WRITER.md.
--   Singular/plural pairs are listed explicitly rather than stemmed, so
--   this stays a plain lookup, not a rule someone has to reverse-engineer.
storyMarkers :: [Text]
storyMarkers =
  [ "story", "stories"
  , "book", "books"
  , "text", "texts"
  , "chapter", "chapters", "ch"
  , "scene", "scenes"
  ]

-- | Every alpha-only "word" in a path segment, lowercased — digits and
--   punctuation are both treated as separators (and simply vanish, rather
--   than becoming their own tokens), so @"ch1"@ splits to @["ch"]@ and
--   @"01 - the first book"@ splits to @["the", "first", "book"]@. Applied to
--   the leaf's own basename, extensions are stripped first
--   ('dropExtensions') so @"ch1.outline.md"@ is judged on @"ch1"@, not on a
--   name still carrying its suffix.
segmentWords :: String -> [Text]
segmentWords = T.words . T.map spaceify . T.toLower . T.pack
  where
    spaceify c = if isAlpha c then c else ' '

-- | Whether a single path segment (an ancestor directory name, or the
--   leaf's own extension-stripped basename) marks the path as prose.
isMarkerSegment :: String -> Bool
isMarkerSegment seg = any (`elem` storyMarkers) (segmentWords seg)

-- | Classify a single file path. See the module Haddock for the algorithm;
--   this is the one place it's implemented (mirrored, not shared, in
--   @frontend/src/lib/library.ts@ — see WRITER.md).
--
--   The two reserved outline shapes (@outline.md@, @{stem}.outline.md@) are
--   self-marking — the name itself is already an unambiguous declaration,
--   the same trust the rest of this codebase already extends to other
--   reserved filenames (@sheet.md@, @journal.md@, @style.md@) — so they
--   short-circuit the general marker-word scan rather than also needing an
--   ancestor/stem marker word nearby.
classifyPath :: FilePath -> LibraryKind
classifyPath path
  | base == "outline.md"                = UnitOutline
  | ".outline.md" `List.isSuffixOf` base = UnitOutline
  | eligible                             = Unit
  | otherwise                            = OtherFile
  where
    base = takeFileName path
    segs = splitDirectories (takeDirectory path) ++ [dropExtensions base]
    eligible = any isMarkerSegment segs

-- | A name broken into alternating runs of digits and non-digits, digit runs
--   parsed to 'Int' — @"ch2"@ becomes @[Right "ch", Left 2]@, @"2 - the
--   sequel.md"@ becomes @[Left 2, Right " - the sequel.md"]@. Comparing two
--   names token-by-token via their 'naturalKey' ('Ord' on @['Either' 'Int'
--   'Text']@, where a numeric token compares by value, not digit count) is
--   what makes @ch2@ sort before @ch11@ and @2 - the sequel@ sort before
--   @14 - the finale@ — plain string order gets both backwards once a
--   number reaches two digits. This never attaches meaning to the number
--   beyond that one comparison: nothing here stores it, names it a "chapter
--   number," or uses it to pair anything (see 'narrativeUnits', which pairs
--   purely by parent directory\/stem).
naturalKey :: Text -> [Either Int Text]
naturalKey t
  | T.null t     = []
  | isDigit (T.head t) =
      let (digits, rest) = T.span isDigit t
      in Left (read (T.unpack digits)) : naturalKey rest
  | otherwise =
      let (chars, rest) = T.break isDigit t
      in Right chars : naturalKey rest

-- | Build the organizational forest from a flat list of file paths (e.g.
--   'Runix.FileSystem.listAllFiles'). Folders are synthesized wherever a
--   path implies one; every leaf is classified via 'classifyPath'. Children
--   sort by 'naturalKey', not plain string order — see the module Haddock.
buildLibraryTree :: [FilePath] -> [LibraryNode]
buildLibraryTree paths = toNodes "" (foldr insertPath Map.empty paths)

-- ---------------------------------------------------------------------------
-- Internal: a path trie, built up one path at a time, then converted to the
-- public 'LibraryNode' forest in one pass.
-- ---------------------------------------------------------------------------

data Trie = Trie
  { trieFile     :: Maybe FilePath
  , trieChildren :: Map FilePath Trie
  }

emptyTrie :: Trie
emptyTrie = Trie Nothing Map.empty

insertPath :: FilePath -> Map FilePath Trie -> Map FilePath Trie
insertPath path = go (splitDirectories path)
  where
    go []           forest = forest
    go [seg]        forest = Map.insertWith mergeLeaf seg (Trie (Just path) Map.empty) forest
    go (seg : rest) forest =
      let entry = Map.findWithDefault emptyTrie seg forest
      in Map.insert seg (entry { trieChildren = go rest (trieChildren entry) }) forest

    mergeLeaf new old = old { trieFile = trieFile new }

toNodes :: FilePath -> Map FilePath Trie -> [LibraryNode]
toNodes prefix forest = List.sortOn (naturalKey . lnName) [ mkNode name entry | (name, entry) <- Map.toList forest ]
  where
    mkNode name (Trie mFile children)
      | Map.null children, Just path <- mFile =
          LibraryNode path (T.pack name) (classifyPath path) False Nothing []
      | otherwise =
          let fullPath = if null prefix then name else prefix <> "/" <> name
          in LibraryNode fullPath (T.pack name) Folder False Nothing (toNodes fullPath children)

-- ---------------------------------------------------------------------------
-- Reading-order flattening
-- ---------------------------------------------------------------------------

-- | One recognized prose unit: its own path (if the prose itself already
--   exists), and its outline's path (if a 'UnitOutline' sibling in the same
--   parent directory exists). Both cover the flat same-stem case
--   (@ch1.md@ \/ @ch1.outline.md@) and the per-unit-folder case
--   (@chapter 1\/story.md@ \/ @chapter 1\/outline.md@) with the one rule:
--   same parent directory. 'uiPath' is 'Nothing' for a beat sheet with no
--   prose written yet — still its own real unit (WRITER.md's "disposable
--   scaffolding": planning content counts even before the chapter itself
--   does), not dropped just because there's nothing to read yet.
data UnitInfo = UnitInfo
  { uiPath        :: Maybe FilePath
  , uiOutlinePath :: Maybe FilePath
  } deriving (Show, Eq)

-- | Every recognized unit in the tree, in document (reading) order — the
--   whole tree is already naturally sorted by 'buildLibraryTree', so this
--   is a plain depth-first walk, one entry per distinct parent
--   directory that holds a 'Unit' and\/or a 'UnitOutline'. This is a real
--   domain fact ("this file is a chapter, and here's its beat sheet if
--   any"), not a display-only grouping — computed once here rather than
--   independently re-derived by every caller (the Library UI,
--   'Storyteller.Writer.Agent.ChapterContext.earlierChaptersOf', the
--   Summarizer agent) that needs the same answer.
narrativeUnits :: [LibraryNode] -> [UnitInfo]
narrativeUnits tree = [ toUnit n | n <- leaves, isPrimary n ]
  where
    leaves = concatMap flatten tree
    flatten node = case lnKind node of
      Folder -> concatMap flatten (lnChildren node)
      _      -> [node]

    dirOf = takeDirectory . lnPath
    unitDirs = [ dirOf n | n <- leaves, lnKind n == Unit ]

    -- One entry per directory: the 'Unit' if it has one, else its 'UnitOutline'
    -- (which, by definition of 'unitDirs', has no 'Unit' sibling to prefer).
    isPrimary n = case lnKind n of
      Unit        -> True
      UnitOutline -> dirOf n `notElem` unitDirs
      _           -> False

    toUnit n = case lnKind n of
      Unit -> UnitInfo
        { uiPath        = Just (lnPath n)
        , uiOutlinePath = lnPath <$> List.find (\o -> lnKind o == UnitOutline && dirOf o == dirOf n) leaves
        }
      _ -> UnitInfo { uiPath = Nothing, uiOutlinePath = Just (lnPath n) }
