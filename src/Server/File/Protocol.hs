{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name}/{path} connections.
--
-- The connection is bound to a named path within a branch, not to a specific
-- blob. The file may be created, deleted, and recreated within the same
-- connection — the path is the identity.
--
-- On connect: server sends file.atoms (oldest-first atom chain) if the file
-- exists, or file.absent if not. New atoms arrive via atom.appended.
-- Commands: operations on this specific file (no path parameter needed).
module Server.File.Protocol
  ( FileCommand(..)
  , FileAtom(..)
  , FileEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Text as T

data FileCommand
  = Append     { fcId :: Maybe T.Text, fcContent :: T.Text }
  | Read       { fcId :: Maybe T.Text }
  | Delete     { fcId :: Maybe T.Text }
  -- Atom mutation commands:
  | EditAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcContent :: T.Text }
    -- ^ Replace an atom's content in-place. The atom's position in the chain is preserved.
  | DeleteAtom { fcId :: Maybe T.Text, fcTickId :: T.Text }
    -- ^ Remove an atom from the chain entirely.
  | MoveAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcAfterTickId :: Maybe T.Text }
    -- ^ Move an atom to a new position. afterTickId=Nothing moves to front.
  deriving (Show)

instance FromJSON FileCommand where
  parseJSON = withObject "FileCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "append"      -> Append     i <$> o .: "content"
      "read"        -> pure (Read i)
      "delete"      -> pure (Delete i)
      "edit.atom"   -> EditAtom   i <$> o .: "tickId" <*> o .: "content"
      "delete.atom" -> DeleteAtom i <$> o .: "tickId"
      "move.atom"   -> MoveAtom   i <$> o .: "tickId" <*> o .:? "afterTickId"
      _             -> fail ("unknown file command: " <> T.unpack t)

data FileAtom = FileAtom
  { atomTickId  :: T.Text
  , atomContent :: T.Text
  , atomMessage :: T.Text
  , atomParent  :: Maybe T.Text
  } deriving (Show)

instance ToJSON FileAtom where
  toJSON fa = object
    [ "tickId"  .= atomTickId  fa
    , "content" .= atomContent fa
    , "message" .= atomMessage fa
    , "parent"  .= atomParent  fa
    ]

data FileEvent
  = FileAtoms    { feAtoms :: [FileAtom] }
  | FileAbsent   { feId :: Maybe T.Text }
  | AtomAppended { feAtom :: FileAtom }
  -- Atom mutation responses:
  | AtomReplaced { feId :: Maybe T.Text, feOldTickId :: T.Text, feAtom :: FileAtom }
    -- ^ An atom was edited. Old tickId is replaced by a new one; atom carries the new data.
  | AtomDeleted  { feId :: Maybe T.Text, feOldTickId :: T.Text, feMapping :: [(T.Text, T.Text)] }
    -- ^ An atom was deleted. feMapping contains old→new id changes for atoms after the deleted one.
  | AtomMoved    { feId :: Maybe T.Text, feMapping :: [(T.Text, T.Text)] }
    -- ^ An atom was moved. feMapping contains the full old→new id rewrite for affected atoms.
  | FileUpdated  { feId :: Maybe T.Text, feContent :: T.Text }
  | FileDeleted  { feId :: Maybe T.Text }
  | FileError    T.Text
  deriving (Show)

instance ToJSON FileEvent where
  toJSON = \case
    FileAtoms atoms ->
      object [ "type" .= ("file.atoms" :: T.Text), "atoms" .= atoms ]
    FileAbsent mid ->
      object $ withId mid [ "type" .= ("file.absent" :: T.Text) ]
    AtomAppended atom ->
      object [ "type" .= ("atom.appended" :: T.Text), "atom" .= atom ]
    AtomReplaced mid oldId atom ->
      object $ withId mid
        [ "type"      .= ("atom.replaced" :: T.Text)
        , "oldTickId" .= oldId
        , "atom"      .= atom ]
    AtomDeleted mid oldId mapping ->
      object $ withId mid
        [ "type"      .= ("atom.deleted" :: T.Text)
        , "oldTickId" .= oldId
        , "mapping"   .= map (\(a,b) -> object ["old" .= a, "new" .= b]) mapping ]
    AtomMoved mid mapping ->
      object $ withId mid
        [ "type"    .= ("atom.moved" :: T.Text)
        , "mapping" .= map (\(a,b) -> object ["old" .= a, "new" .= b]) mapping ]
    FileUpdated mid content ->
      object $ withId mid [ "type" .= ("file.updated" :: T.Text), "content" .= content ]
    FileDeleted mid ->
      object $ withId mid [ "type" .= ("file.deleted" :: T.Text) ]
    FileError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
