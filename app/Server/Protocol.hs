{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WebSocket message types.
--
-- Commands flow client → server, events flow server → client.
-- Both are JSON objects with a "type" discriminator field.
module Server.Protocol
  ( Command(..)
  , Event(..)
  , SessionOpen(..)
  , TrackFile(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import Data.Map.Strict (Map)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Shared
-- ---------------------------------------------------------------------------

data TrackFile = TrackFile
  { trackFrom :: FilePath
  , trackTo   :: FilePath
  } deriving (Show)

instance FromJSON TrackFile where
  parseJSON = withObject "TrackFile" $ \o ->
    TrackFile <$> o .: "from" <*> o .: "to"

instance ToJSON TrackFile where
  toJSON tf = object [ "from" .= trackFrom tf, "to" .= trackTo tf ]

-- ---------------------------------------------------------------------------
-- Commands (client → server)
-- ---------------------------------------------------------------------------

data SessionOpen = SessionOpen T.Text  -- branch name

instance FromJSON SessionOpen where
  parseJSON = withObject "SessionOpen" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    case t of
      "session.open" -> SessionOpen <$> o .: "branch"
      _              -> fail "expected session.open"

data Command
  = Append  { cmdId :: Maybe T.Text, cmdPath :: FilePath, cmdContent :: T.Text }
  | Track   { cmdId :: Maybe T.Text, cmdSource :: T.Text, cmdFiles :: [TrackFile] }
  | CharGen { cmdId :: Maybe T.Text, cmdPath :: FilePath, cmdScenario :: T.Text, cmdSeed :: Maybe Int }
  | ReadFile  { cmdId :: Maybe T.Text, cmdPath :: FilePath }
  | DeleteFile { cmdId :: Maybe T.Text, cmdPath :: FilePath }
  deriving (Show)

instance FromJSON Command where
  parseJSON = withObject "Command" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "append"      -> Append    i <$> o .: "path" <*> o .: "content"
      "track"       -> Track     i <$> o .: "source" <*> o .: "files"
      "chargen"     -> CharGen   i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      "read"        -> ReadFile  i <$> o .: "path"
      "delete.file" -> DeleteFile i <$> o .: "path"
      _             -> fail ("unknown command type: " <> T.unpack t)

-- ---------------------------------------------------------------------------
-- Events (server → client)
-- ---------------------------------------------------------------------------

data Event
  = SessionReady T.Text (Map FilePath T.Text)       -- branch, file snapshot
  | FileContent  { evId :: Maybe T.Text, evPath :: FilePath, evContent :: T.Text }
  | FileUpdated  { evId :: Maybe T.Text, evPath :: FilePath, evContent :: T.Text }
  | Error        T.Text
  deriving (Show)

instance ToJSON Event where
  toJSON = \case
    SessionReady branch files ->
      object [ "type" .= ("session.ready" :: T.Text), "branch" .= branch, "files" .= files ]
    FileContent mid path content ->
      object $ withId mid [ "type" .= ("file.content" :: T.Text), "path" .= path, "content" .= content ]
    FileUpdated mid path content ->
      object $ withId mid [ "type" .= ("file.updated" :: T.Text), "path" .= path, "content" .= content ]
    Error msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
