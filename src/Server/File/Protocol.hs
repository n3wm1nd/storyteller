{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name}/{path} connections.
--
-- Commands: intent-based file operations. The server executes them and
--           pushes the resulting state back as a FileUpdate. No read or
--           resync commands — reconnect triggers a full state push.
-- Events:   FilePresent/FileAbsent on connect, FileUpdate after mutations,
--           plus AgentLog and FileError.
module Server.File.Protocol
  ( FileCommand(..)
  , FileEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Server.Protocol (Update, withId)

-- | Commands the client may send on a file connection.
--   Each is an intent — the server decides what ticks result.
data FileCommand
  = Append     { fcId :: Maybe T.Text, fcContent :: T.Text }
  | Delete     { fcId :: Maybe T.Text }
  | EditAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcContent :: T.Text }
  | DeleteAtom { fcId :: Maybe T.Text, fcTickId :: T.Text }
  | MoveAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcAfterTickId :: Maybe T.Text }
  deriving (Show)

instance FromJSON FileCommand where
  parseJSON = withObject "FileCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "append"      -> Append     i <$> o .: "content"
      "delete"      -> pure (Delete i)
      "edit.atom"   -> EditAtom   i <$> o .: "tickId" <*> o .: "content"
      "delete.atom" -> DeleteAtom i <$> o .: "tickId"
      "move.atom"   -> MoveAtom   i <$> o .: "tickId" <*> o .:? "afterTickId"
      _             -> fail ("unknown file command: " <> T.unpack t)

-- | Events the server sends on a file connection.
--
--   FilePresent:  sent once on connect; the file exists, full tick state follows
--                 immediately as a FileUpdate.
--   FileAbsent:   sent once on connect; the file does not exist yet.
--   FileUpdate:   tick state push — upsert all ticks, set head to updateHead.
--   AgentLog:     progress message from a running agent.
--   FileError:    something went wrong; message is human-readable.
data FileEvent
  = FilePresent { feId :: Maybe T.Text }
  | FileAbsent  { feId :: Maybe T.Text }
  | FileUpdate  Update
  | AgentLog    { feLevel :: T.Text, feMessage :: T.Text }
  | FileError   T.Text
  deriving (Show)

instance ToJSON FileEvent where
  toJSON = \case
    FilePresent mid ->
      object $ withId mid [ "type" .= ("file.present" :: T.Text) ]
    FileAbsent mid ->
      object $ withId mid [ "type" .= ("file.absent"  :: T.Text) ]
    FileUpdate u -> toJSON u
    AgentLog level msg ->
      object [ "type"    .= ("agent.log" :: T.Text)
             , "level"   .= level
             , "message" .= msg ]
    FileError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
