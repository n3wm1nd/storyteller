{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name} connections.
--
-- Commands: branch-level operations (file tracking, generation, annotations,
--           tick reordering). Scene presence (enter.scene/leave.scene) lives
--           on the file connection instead — see WRITER.md and
--           Server.Writer.File.Protocol; a scene is a file, not the whole
--           branch, so presence is scoped there. No resync command —
--           reconnect is resync.
--           chat.prompt lives on the file connection (Server.Writer.File.Protocol) —
--           path is implicit from the URL there.
-- Events:   structural events (ready, file list changes) plus tick updates.
--           All tick state arrives as Update — the full filtered chain on
--           connect, affected ticks after each mutation.
module Server.Writer.Branch.Protocol
  ( BranchCommand(..)
  , BranchEvent(..)
  , TrackFile(..)
  , UploadFile(..)
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

-- | One dropped file's destination path and raw text content — see
--   'Upload' below. Content is plain text (files are markdown by
--   convention, see WRITER.md); no binary upload support.
data UploadFile = UploadFile
  { uploadPath    :: FilePath
  , uploadContent :: T.Text
  } deriving (Show)

instance FromJSON UploadFile where
  parseJSON = withObject "UploadFile" $ \o ->
    UploadFile <$> o .: "path" <*> o .: "content"

instance ToJSON UploadFile where
  toJSON uf = object [ "path" .= uploadPath uf, "content" .= uploadContent uf ]

-- | Commands the client may send on a branch connection.
--   Each is an intent — the server decides what ticks result.
data BranchCommand
  = Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcFiles :: [TrackFile] }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  | AddNote    { bcId :: Maybe T.Text, bcRefTickId :: T.Text, bcNoteText :: T.Text }
  | MoveTick   { bcId :: Maybe T.Text, bcTickId :: T.Text, bcAfterTickId :: Maybe T.Text }
  | DeleteTick { bcId :: Maybe T.Text, bcTickId :: T.Text }
  -- Upload: write one or more dropped files' content directly into this
  -- branch, bypassing the chat-agent pipeline entirely — see TODO.md's
  -- Upload/download packet. Lives here rather than on the file connection
  -- because it isn't an operation on an already-open single file: it sends
  -- files in bulk, and a fresh path may not have a connection open for it
  -- at all yet.
  | Upload     { bcId :: Maybe T.Text, bcUploads :: [UploadFile] }
  -- Rebase: run @command@ as if @tickId@ were HEAD, then replay everything
  -- that came after it on top of the result — same as 'FileCommand's 'At'
  -- (see Server.Writer.File.Protocol), just for branch-level commands (e.g.
  -- a future Ticks-view rebase marker, the branch-level equivalent of the
  -- file view's drag handle — no client trigger for this exists yet, this
  -- is just the generic capability being available symmetrically).
  | At { bcId :: Maybe T.Text, bcTickId :: T.Text, bcCommand :: BranchCommand }
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
      "upload"      -> Upload     i <$> o .: "files"
      "at"          -> At         i <$> o .: "tickId" <*> o .: "command"
      _             -> fail ("unknown branch command: " <> T.unpack t)

-- | Events the server sends on a branch connection.
--
--   BranchReady:  sent once on connect with the branch name and current file list.
--   FileAdded:    a new file appeared in the branch tree.
--   BranchUpdate: tick state push — upsert all ticks, set head to updateHead.
--   AgentLog:     progress message from a running agent.
--   BranchError:  something went wrong; message is human-readable.
--
-- FIXME: file tree changes are only tracked one-directionally — FileAdded
-- covers Track/CharGen/Upload creating a path, but there's no
-- FileRemoved/FileRenamed (or any push at all) for a file disappearing or
-- being renamed/moved from the tree. Once delete/rename/move-file commands
-- exist (see TODO.md), a connected client's cached file list can silently
-- drift from the real tree with no event to correct it — 'pushIncremental'
-- (Connection.hs) only re-derives tick state on notify, never the file
-- list itself.
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
