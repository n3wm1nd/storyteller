{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name} connections.
--
-- Commands: file and agent operations within an open branch.
-- Events:   file content, updates, errors.
-- The branch name is implicit — it comes from the connection URL.
module Server.Branch.Protocol
  ( BranchCommand(..)
  , BranchEvent(..)
  , TrackFile(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import Data.Map.Strict (Map)
import qualified Data.Text as T

data TrackFile = TrackFile
  { trackFrom :: FilePath
  , trackTo   :: FilePath
  } deriving (Show)

instance FromJSON TrackFile where
  parseJSON = withObject "TrackFile" $ \o ->
    TrackFile <$> o .: "from" <*> o .: "to"

instance ToJSON TrackFile where
  toJSON tf = object [ "from" .= trackFrom tf, "to" .= trackTo tf ]

data BranchCommand
  = Append     { bcId :: Maybe T.Text, bcPath :: FilePath, bcContent :: T.Text }
  | Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcFiles :: [TrackFile] }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  | ReadFile   { bcId :: Maybe T.Text, bcPath :: FilePath }
  | DeleteFile { bcId :: Maybe T.Text, bcPath :: FilePath }
  deriving (Show)

instance FromJSON BranchCommand where
  parseJSON = withObject "BranchCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "append"       -> Append     i <$> o .: "path" <*> o .: "content"
      "track"        -> Track      i <$> o .: "source" <*> o .: "files"
      "chargen"      -> CharGen    i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      "read"         -> ReadFile   i <$> o .: "path"
      "delete.file"  -> DeleteFile i <$> o .: "path"
      _              -> fail ("unknown branch command: " <> T.unpack t)

data BranchEvent
  = BranchReady  { beId :: Maybe T.Text, beBranch :: T.Text, beFiles :: Map FilePath T.Text }
  | FileContent  { beId :: Maybe T.Text, bePath :: FilePath, beContent :: T.Text }
  | FileUpdated  { beId :: Maybe T.Text, bePath :: FilePath, beContent :: T.Text }
  | BranchError  T.Text
  deriving (Show)

instance ToJSON BranchEvent where
  toJSON = \case
    BranchReady mid branch files ->
      object $ withId mid [ "type" .= ("branch.ready"  :: T.Text)
                           , "branch" .= branch, "files" .= files ]
    FileContent mid path content ->
      object $ withId mid [ "type" .= ("file.content"  :: T.Text)
                           , "path" .= path, "content" .= content ]
    FileUpdated mid path content ->
      object $ withId mid [ "type" .= ("file.updated"  :: T.Text)
                           , "path" .= path, "content" .= content ]
    BranchError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
