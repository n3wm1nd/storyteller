{-# LANGUAGE OverloadedStrings #-}

-- | The pure book\/chapter organizational-tree derivation behind
-- @\/library\/{name}@ (see WS-PROTOCOL.md). Detection is deliberately
-- freeform and by convention only: a path is recognized as a chapter or
-- outline purely from its own basename and immediate parent directory name
-- (@chapters\/ch{N}.md@, see WRITER.md), independent of how deeply that
-- sits in an otherwise arbitrary, user-chosen folder structure (a
-- @series\/epic\/book3\/act1\/chapters\/ch1.md@ nests exactly as well as a bare
-- @chapters\/ch1.md@). Nothing here prescribes or limits that surrounding
-- structure — every other folder\/file just becomes a plain tree node,
-- labeled, never filtered out or flattened away.
--
-- Deliberately pure and IO-free: 'buildLibraryTree' only needs the branch's
-- file *paths*, not their content, so it composes without touching any
-- effect stack. A future consumer with the identical "what belongs to which
-- chapter" question (the planned Summarizer agent, see DESIGN.md) can reuse
-- this directly instead of re-deriving it — see 'Server.Writer.Library',
-- the one caller today, for where file *content* (a chapter's own heading)
-- gets folded in afterward.
module Storyteller.Writer.Library
  ( LibraryKind(..)
  , LibraryNode(..)
  , ChapterUnit(..)
  , classifyPath
  , buildLibraryTree
  , chapterUnits
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (splitDirectories, takeFileName, takeDirectory)
import Text.Read (readMaybe)

-- | What convention (if any) a single file path matches. 'Folder' is never
--   produced by 'classifyPath' itself — it's assigned structurally, to any
--   node 'buildLibraryTree' had to synthesize as an ancestor of some deeper
--   path.
data LibraryKind
  = Folder
  | Chapter Int         -- ^ @chapters\/ch{N}.md@ — N is parsed for ordering.
  | ChapterOutline Int  -- ^ @chapters\/ch{N}.outline.md@ — the beat sheet for chapter N.
  | StoryOutline        -- ^ @outline.md@, anywhere.
  | OtherFile           -- ^ Everything else — still a real node, just unlabeled.
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

-- | Classify a single file path by its own basename and immediate parent
--   directory name only — see the module Haddock for why the rest of the
--   path is deliberately never consulted.
classifyPath :: FilePath -> LibraryKind
classifyPath path
  | base == "outline.md"                            = StoryOutline
  | parent == "chapters", Just n <- chapterOutline base = ChapterOutline n
  | parent == "chapters", Just n <- chapter base         = Chapter n
  | otherwise                                        = OtherFile
  where
    base   = takeFileName path
    parent = takeFileName (takeDirectory path)

    chapter name = do
      rest <- List.stripPrefix "ch" name
      num  <- stripSuffix ".md" rest
      readMaybe num

    chapterOutline name = do
      rest <- List.stripPrefix "ch" name
      num  <- stripSuffix ".outline.md" rest
      readMaybe num

    stripSuffix suf s = reverse <$> List.stripPrefix (reverse suf) (reverse s)

-- | Build the organizational forest from a flat list of file paths (e.g.
--   'Runix.FileSystem.listAllFiles'). Folders are synthesized wherever a
--   path implies one; every leaf is classified via 'classifyPath'. Children
--   are ordered chapter-number-first (so @ch2@ sorts before @ch10@, unlike
--   plain alphabetical), then by name.
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
toNodes prefix forest = sortNodes [ mkNode name entry | (name, entry) <- Map.toList forest ]
  where
    mkNode name (Trie mFile children)
      | Map.null children, Just path <- mFile =
          LibraryNode path (T.pack name) (classifyPath path) False Nothing []
      | otherwise =
          let fullPath = if null prefix then name else prefix <> "/" <> name
          in LibraryNode fullPath (T.pack name) Folder False Nothing (toNodes fullPath children)

sortNodes :: [LibraryNode] -> [LibraryNode]
sortNodes = List.sortOn sortKey
  where
    sortKey n = case lnKind n of
      Chapter i        -> (0 :: Int, i, lnName n)
      ChapterOutline i -> (0, i, lnName n)
      _                -> (1, 0, lnName n)

-- ---------------------------------------------------------------------------
-- Chapter/outline pairing
-- ---------------------------------------------------------------------------

-- | One chapter number's worth of artifacts — a chapter file, its beat
--   sheet, or both. Either one existing already means the chapter itself
--   exists as a concept (a beat sheet with no prose yet is still real
--   planning content, see WRITER.md's "disposable scaffolding"), so this
--   pairs them by number rather than requiring both.
data ChapterUnit = ChapterUnit
  { cuNumber      :: Int
  , cuChapterPath :: Maybe FilePath
  , cuHeading     :: Maybe Text -- ^ the chapter file's own 'lnHeading', if it exists and has one.
  , cuOutlinePath :: Maybe FilePath
  } deriving (Show, Eq)

-- | Pair every chapter file with its beat sheet by number, walking the
--   whole tree regardless of nesting (folder position carries no meaning
--   for this pairing — see the module Haddock). This is a real domain fact
--   ("chapter N" is one thing, whichever of its two possible files exist),
--   not a display-only grouping — computed once here rather than
--   independently re-derived by every caller (a UI, and eventually the
--   planned Summarizer agent) that needs the same answer.
chapterUnits :: [LibraryNode] -> [ChapterUnit]
chapterUnits tree = map toUnit numbers
  where
    leaves = concatMap flatten tree
    flatten node = case lnKind node of
      Folder -> concatMap flatten (lnChildren node)
      _      -> [node]

    chapters = [ (n, node) | node <- leaves, Chapter n        <- [lnKind node] ]
    outlines = [ (n, node) | node <- leaves, ChapterOutline n <- [lnKind node] ]
    numbers  = List.sort (List.nub (map fst chapters ++ map fst outlines))

    toUnit n = ChapterUnit
      { cuNumber      = n
      , cuChapterPath = lnPath <$> lookup n chapters
      , cuHeading     = lookup n chapters >>= lnHeading
      , cuOutlinePath = lnPath <$> lookup n outlines
      }
