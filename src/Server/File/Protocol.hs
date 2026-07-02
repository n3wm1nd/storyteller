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
  | ChatPrompt { fcId :: Maybe T.Text, fcPromptText :: T.Text }
  -- | Run 'fcCommand' rebased at 'fcTickId': the chain is temporarily wound
  --   back to that tick, the filesystem set to its snapshot, the inner
  --   command executed there, then every later tick is replayed on top of
  --   whatever the inner command produced. Lets a client re-target any
  --   command at a historical point without a dedicated code path per
  --   command — see 'Storyteller.Storage.atWithFS'.
  | At         { fcId :: Maybe T.Text, fcTickId :: T.Text, fcCommand :: FileCommand }
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
      "chat.prompt" -> ChatPrompt i <$> o .: "text"
      "at"          -> At         i <$> o .: "tickId" <*> o .: "command"
      _             -> fail ("unknown file command: " <> T.unpack t)

-- | Events the server sends on a file connection.
--
--   FilePresent:  sent once on connect; the file exists, full tick state follows
--                 immediately as a FileUpdate.
--   FileAbsent:   sent once on connect; the file does not exist yet.
--   FileUpdate:   tick state push — upsert all ticks, set head to updateHead.
--   TickRemap:    a rebase/replace/move rewrote tick ids; [(from, to)] pairs.
--                 The client checks any tickId it's tracking locally (a
--                 rebase marker, a context selection) and updates it in
--                 place — a no-op if it isn't tracking any of them.
--   AgentLog:     progress message from a running agent.
--   FileError:    something went wrong; message is human-readable.
--
--   Not modeled here: "chat.preview.start"/"chat.preview"/"chat.preview.thinking"/
--   "chat.preview.end", the ephemeral LLM streaming preview. Those are pushed
--   directly by 'Server.Run.streamChunksWS', which is installed once per
--   connection (file or branch alike) around the whole command loop rather
--   than constructed by any 'Server.File'/'Server.Branch' handler — the wire
--   shape is identical for both connection types, so there's nothing
--   File-specific for a 'FileEvent' constructor to add. See WS-PROTOCOL.md.
data FileEvent
  = FilePresent { feId :: Maybe T.Text }
  | FileAbsent  { feId :: Maybe T.Text }
  | FileUpdate  Update
  | TickRemap   [(T.Text, T.Text)]
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
    TickRemap mapping ->
      object [ "type" .= ("tick.remap" :: T.Text), "mapping" .= mapping ]
    AgentLog level msg ->
      object [ "type"    .= ("agent.log" :: T.Text)
             , "level"   .= level
             , "message" .= msg ]
    FileError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
