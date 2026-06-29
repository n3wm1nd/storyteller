{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name}/{path} connections.
--
-- The connection is bound to a named path within a branch, not to a specific
-- blob. The file may be created, deleted, and recreated within the same
-- connection — the path is the identity.
--
-- On connect: server sends FileContent if the file exists, FileAbsent if not.
-- Commands: operations on this specific file (no path parameter needed).
-- Events:   file content, updates, deletion, errors.
module Server.File.Protocol
  ( FileCommand(..)
  , FileEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Text as T

data FileCommand
  = Append  { fcId :: Maybe T.Text, fcContent :: T.Text }
  | Read    { fcId :: Maybe T.Text }
  | Delete  { fcId :: Maybe T.Text }
  deriving (Show)

instance FromJSON FileCommand where
  parseJSON = withObject "FileCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "append" -> Append i <$> o .: "content"
      "read"   -> pure (Read i)
      "delete" -> pure (Delete i)
      _        -> fail ("unknown file command: " <> T.unpack t)

data FileEvent
  = FileContent { feId :: Maybe T.Text, feContent :: T.Text }
  | FileAbsent  { feId :: Maybe T.Text }
  | FileUpdated { feId :: Maybe T.Text, feContent :: T.Text }
  | FileDeleted { feId :: Maybe T.Text }
  | FileError   T.Text
  deriving (Show)

instance ToJSON FileEvent where
  toJSON = \case
    FileContent mid content ->
      object $ withId mid [ "type" .= ("file.content" :: T.Text), "content" .= content ]
    FileAbsent mid ->
      object $ withId mid [ "type" .= ("file.absent" :: T.Text) ]
    FileUpdated mid content ->
      object $ withId mid [ "type" .= ("file.updated" :: T.Text), "content" .= content ]
    FileDeleted mid ->
      object $ withId mid [ "type" .= ("file.deleted" :: T.Text) ]
    FileError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
