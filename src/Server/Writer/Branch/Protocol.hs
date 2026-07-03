{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name} connections.
--
-- Commands: branch-level operations (file tracking, generation, annotations,
--           tick reordering, scene presence). No resync command — reconnect
--           is resync.
--           chat.prompt lives on the file connection (Server.Writer.File.Protocol) —
--           path is implicit from the URL there.
-- Events:   structural events (ready, file list changes) plus tick updates.
--           All tick state arrives as Update — the full filtered chain on
--           connect, affected ticks after each mutation.
module Server.Writer.Branch.Protocol
  ( BranchCommand(..)
  , BranchEvent(..)
  , TrackFile(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Server.Core.Protocol (Update, withId)

data TrackFile = TrackFile
  { trackFrom :: FilePath
  , trackTo   :: FilePath
  } deriving (Show)

instance FromJSON TrackFile where
  parseJSON = withObject "TrackFile" $ \o ->
    TrackFile <$> o .: "from" <*> o .: "to"

instance ToJSON TrackFile where
  toJSON tf = object [ "from" .= trackFrom tf, "to" .= trackTo tf ]

-- | Commands the client may send on a branch connection.
--   Each is an intent — the server decides what ticks result.
data BranchCommand
  = Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcFiles :: [TrackFile] }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  | AddNote    { bcId :: Maybe T.Text, bcRefTickId :: T.Text, bcNoteText :: T.Text }
  | MoveTick   { bcId :: Maybe T.Text, bcTickId :: T.Text, bcAfterTickId :: Maybe T.Text }
  | DeleteTick { bcId :: Maybe T.Text, bcTickId :: T.Text }
  | EnterScene { bcId :: Maybe T.Text, bcCharacter :: T.Text }
  | LeaveScene { bcId :: Maybe T.Text, bcCharacter :: T.Text }
  deriving (Show)

instance FromJSON BranchCommand where
  parseJSON = withObject "BranchCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "track"       -> Track      i <$> o .: "source" <*> o .: "files"
      "chargen"     -> CharGen    i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      "add.note"    -> AddNote    i <$> o .: "refTickId" <*> o .: "text"
      "move.tick"   -> MoveTick   i <$> o .: "tickId" <*> o .:? "afterTickId"
      "delete.tick" -> DeleteTick i <$> o .: "tickId"
      "enter.scene" -> EnterScene i <$> o .: "character"
      "leave.scene" -> LeaveScene i <$> o .: "character"
      _             -> fail ("unknown branch command: " <> T.unpack t)

-- | Events the server sends on a branch connection.
--
--   BranchReady:  sent once on connect with the branch name and current file list.
--   FileAdded:    a new file appeared in the branch tree.
--   BranchUpdate: tick state push — upsert all ticks, set head to updateHead.
--   AgentLog:     progress message from a running agent.
--   BranchError:  something went wrong; message is human-readable.
data BranchEvent
  = BranchReady  { beId :: Maybe T.Text, beBranch :: T.Text, beFiles :: [FilePath] }
  | FileAdded    { beId :: Maybe T.Text, bePath :: FilePath }
  | BranchUpdate Update
  | AgentLog     { beLevel :: T.Text, beMessage :: T.Text }
  | BranchError  T.Text
  deriving (Show)

instance ToJSON BranchEvent where
  toJSON = \case
    BranchReady mid branch files ->
      object $ withId mid
        [ "type"   .= ("branch.ready" :: T.Text)
        , "branch" .= branch
        , "files"  .= files ]
    FileAdded mid path ->
      object $ withId mid
        [ "type" .= ("file.added" :: T.Text)
        , "path" .= path ]
    BranchUpdate u -> toJSON u
    AgentLog level msg ->
      object [ "type"    .= ("agent.log" :: T.Text)
             , "level"   .= level
             , "message" .= msg ]
    BranchError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
