{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for @\/library\/{name}@ connections — see WS-PROTOCOL.md.
--
-- Commands: 'ChapterCreate' only — everything else here is read-only; a
--           node's content is opened through the ordinary file connection.
-- Events:   a single tree push, sent on connect and again whenever the
--           branch changes. No presence\/absence tri-state (like
--           'Server.Writer.Character.Protocol') — an empty tree is just an
--           empty tree, not a missing scope.
module Server.Writer.Library.Protocol
  ( LibraryCommand(..)
  , LibraryEvent(..)
  , commandKind
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Storyteller.Writer.Library (LibraryNode(..), LibraryKind(..), ChapterUnit(..))

-- | Commands the client may send on a library connection.
--
--   'ChapterCreate' introduces @lcPath@ as a new chapter file, seeded with
--   @# {lcName}@ as its first line (see 'Server.Writer.Library.chapterCreate') —
--   distinct from the generic file connection's @file.create@, which has no
--   notion of a chapter heading convention to seed.
data LibraryCommand
  = ChapterCreate { lcPath :: FilePath, lcName :: T.Text }
  deriving (Show)

instance FromJSON LibraryCommand where
  parseJSON = withObject "LibraryCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    case t of
      "chapter.create" -> ChapterCreate <$> o .: "path" <*> o .: "name"
      _                -> fail ("unknown library command: " <> T.unpack t)

-- | Short label for logging — see 'Server.Writer.File.Protocol.commandKind'.
commandKind :: LibraryCommand -> T.Text
commandKind ChapterCreate {} = "chapter.create"

-- | Events the server sends on a library connection.
--
--   'LibraryTree' carries both the raw per-file organizational tree
--   ('nodes') and every chapter number already paired with its own chapter
--   file\/beat sheet ('chapters', see
--   'Storyteller.Writer.Library.chapterUnits') — computed once server-side
--   rather than left for the client to reconstruct: "which chapter does
--   this belong to" is a real domain fact (either artifact existing already
--   means the chapter exists as a concept), not a display-only grouping.
data LibraryEvent
  = LibraryTree { leNodes :: [LibraryNode], leChapters :: [ChapterUnit] }
  | LibraryError T.Text
  deriving (Show)

instance ToJSON LibraryEvent where
  toJSON = \case
    LibraryTree nodes chapters ->
      object
        [ "type"     .= ("library.tree" :: T.Text)
        , "nodes"    .= map nodeToJSON nodes
        , "chapters" .= map chapterUnitToJSON chapters
        ]
    LibraryError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

chapterUnitToJSON :: ChapterUnit -> Value
chapterUnitToJSON cu = object $
  [ "number" .= cuNumber cu ]
  <> maybe [] (\p -> ["chapterPath" .= p]) (cuChapterPath cu)
  <> maybe [] (\h -> ["heading"     .= h]) (cuHeading cu)
  <> maybe [] (\p -> ["outlinePath" .= p]) (cuOutlinePath cu)

nodeToJSON :: LibraryNode -> Value
nodeToJSON n = object $
  [ "path"     .= lnPath n
  , "name"     .= lnName n
  , "kind"     .= kindTag (lnKind n)
  , "children" .= map nodeToJSON (lnChildren n)
  ]
  <> maybe [] (\num -> ["number" .= num]) (kindNumber (lnKind n))
  <> maybe [] (\h -> ["heading" .= h]) (lnHeading n)

kindTag :: LibraryKind -> T.Text
kindTag = \case
  Folder           -> "folder"
  Chapter _        -> "chapter"
  ChapterOutline _ -> "chapter-outline"
  StoryOutline     -> "story-outline"
  OtherFile        -> "other"

kindNumber :: LibraryKind -> Maybe Int
kindNumber = \case
  Chapter n        -> Just n
  ChapterOutline n -> Just n
  _                -> Nothing
