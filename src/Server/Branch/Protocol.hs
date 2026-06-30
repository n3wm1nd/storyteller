{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name} connections.
--
-- Branch connections carry branch-level state only: the file tree (paths, no
-- content). File content lives in /branch/{name}/{path} connections.
-- The branch name is implicit from the connection URL.
module Server.Branch.Protocol
  ( BranchCommand(..)
  , BranchEvent(..)
  , BranchTick(..)
  , TrackFile(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
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
  = Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcFiles :: [TrackFile] }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  deriving (Show)

instance FromJSON BranchCommand where
  parseJSON = withObject "BranchCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "track"   -> Track   i <$> o .: "source" <*> o .: "files"
      "chargen" -> CharGen i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      _         -> fail ("unknown branch command: " <> T.unpack t)

data BranchTick = BranchTick
  { btTickId  :: T.Text
  , btParent  :: Maybe T.Text
  , btMessage :: T.Text
  , btRefs    :: [T.Text]
  } deriving (Show)

instance ToJSON BranchTick where
  toJSON bt = object
    [ "tickId"  .= btTickId  bt
    , "parent"  .= btParent  bt
    , "message" .= btMessage bt
    , "refs"    .= btRefs    bt
    ]

data BranchEvent
  = BranchReady { beId :: Maybe T.Text, beBranch :: T.Text, beFiles :: [FilePath] }
  | BranchTicks { beTicks :: [BranchTick] }
  | FileAdded   { beId :: Maybe T.Text, bePath :: FilePath }
  | FileRemoved { beId :: Maybe T.Text, bePath :: FilePath }
  | BranchError T.Text
  deriving (Show)

instance ToJSON BranchEvent where
  toJSON = \case
    BranchReady mid branch files ->
      object $ withId mid [ "type" .= ("branch.ready" :: T.Text)
                           , "branch" .= branch, "files" .= files ]
    BranchTicks ticks ->
      object [ "type" .= ("branch.ticks" :: T.Text), "ticks" .= ticks ]
    FileAdded mid path ->
      object $ withId mid [ "type" .= ("file.added"   :: T.Text), "path" .= path ]
    FileRemoved mid path ->
      object $ withId mid [ "type" .= ("file.removed" :: T.Text), "path" .= path ]
    BranchError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
