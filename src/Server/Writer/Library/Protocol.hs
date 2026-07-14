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

import Storyteller.Writer.Library (LibraryNode(..), LibraryKind(..), UnitInfo(..))

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
--   ('nodes') and every prose unit already paired with its own beat sheet if
--   any ('chapters', see 'Storyteller.Writer.Library.narrativeUnits') —
--   computed once server-side rather than left for the client to
--   reconstruct: "this file has a beat sheet" is a real domain fact, not a
--   display-only grouping. A unit's heading isn't repeated here — it's
--   already on the matching node in 'nodes' ('lnHeading'), which the client
--   looks up by path the same way it already does for everything else.
data LibraryEvent
  = LibraryTree { leNodes :: [LibraryNode], leUnits :: [UnitInfo] }
  | LibraryError T.Text
  deriving (Show)

instance ToJSON LibraryEvent where
  toJSON = \case
    LibraryTree nodes units ->
      object
        [ "type"     .= ("library.tree" :: T.Text)
        , "nodes"    .= map nodeToJSON nodes
        , "chapters" .= map unitInfoToJSON units
        ]
    LibraryError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

unitInfoToJSON :: UnitInfo -> Value
unitInfoToJSON u = object $
  maybe [] (\p -> ["path"        .= p]) (uiPath u)
  <> maybe [] (\p -> ["outlinePath" .= p]) (uiOutlinePath u)

nodeToJSON :: LibraryNode -> Value
nodeToJSON n = object $
  [ "path"     .= lnPath n
  , "name"     .= lnName n
  , "kind"     .= kindTag (lnKind n)
  , "children" .= map nodeToJSON (lnChildren n)
  ]
  <> maybe [] (\h -> ["heading" .= h]) (lnHeading n)
  <> (if lnBinary n then ["binary" .= True] else [])

kindTag :: LibraryKind -> T.Text
kindTag = \case
  Folder      -> "folder"
  Unit        -> "unit"
  UnitOutline -> "unit-outline"
  OtherFile   -> "other"
