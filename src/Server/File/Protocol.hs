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
  , ContextItem(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Server.Protocol (Update, withId)

-- | A piece of pinned context the client attaches to a chat prompt — an
--   atom or annotation the user selected as reference material. 'ciContent'
--   is what the agent actually reads; 'ciTickId'/'ciKind' are for
--   traceability only. See SPEC-SELECTION-ANNOTATIONS.md.
data ContextItem = ContextItem
  { ciTickId  :: T.Text
  , ciKind    :: T.Text
  , ciContent :: T.Text
  } deriving (Show)

instance FromJSON ContextItem where
  parseJSON = withObject "ContextItem" $ \o ->
    ContextItem <$> o .: "tickId" <*> o .: "kind" <*> o .: "content"

-- | Commands the client may send on a file connection.
--   Each is an intent — the server decides what ticks result.
--
--   The "chat.*" variants are the input bar's routable targets:
--   'ChatAppend' is the instant, non-LLM verbatim insert; 'ChatWriter' is
--   Writer (or FlowWriter, implicitly, when 'cwFlowTid' is set — the tick
--   that was HEAD when the user started typing, so the agent can judge
--   whether atoms generated since then are still provisional); 'ChatFixer'
--   edits specific existing atoms in place; 'ChatNote' is instant and
--   non-LLM like 'ChatAppend', attaching an annotation instead of content.
data FileCommand
  = ChatAppend { fcId :: Maybe T.Text, fcContent :: T.Text }
  | Delete     { fcId :: Maybe T.Text }
  | EditAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcContent :: T.Text }
  | DeleteAtom { fcId :: Maybe T.Text, fcTickId :: T.Text }
  | MoveAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcAfterTickId :: Maybe T.Text }
  | ChatWriter { fcId :: Maybe T.Text, fcPromptText :: T.Text, fcContext :: [ContextItem], fcFlowTid :: Maybe T.Text }
  | ChatFixer  { fcId :: Maybe T.Text, fcPromptText :: T.Text, fcContext :: [ContextItem], fcTargets :: [T.Text] }
  -- | Instant, non-LLM: attach a note to each of 'fcTargets', or (when
  --   empty) to the file's current HEAD tick.
  | ChatNote   { fcId :: Maybe T.Text, fcNoteText :: T.Text, fcTargets :: [T.Text] }
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
      "chat.append" -> ChatAppend i <$> o .: "content"
      "delete"      -> pure (Delete i)
      "edit.atom"   -> EditAtom   i <$> o .: "tickId" <*> o .: "content"
      "delete.atom" -> DeleteAtom i <$> o .: "tickId"
      "move.atom"   -> MoveAtom   i <$> o .: "tickId" <*> o .:? "afterTickId"
      "chat.writer" -> do
        context <- fromMaybe [] <$> o .:? "context"
        flowTid <- o .:? "flowTid"
        ChatWriter i <$> o .: "text" <*> pure context <*> pure flowTid
      "chat.fixer"  -> do
        context <- fromMaybe [] <$> o .:? "context"
        targets <- fromMaybe [] <$> o .:? "targets"
        ChatFixer i <$> o .: "text" <*> pure context <*> pure targets
      "chat.note"   -> do
        targets <- fromMaybe [] <$> o .:? "targets"
        ChatNote i <$> o .: "text" <*> pure targets
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
