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
  , commandKind
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Server.Core.Protocol (Update, withId)
import Server.Writer.File.Protocol (AtBranch(..))

-- | Commands the client may send on a branch connection.
--   Each is an intent — the server decides what ticks result.
--
--   Track's @bcOnlyFile@ restricts the source side to one file (the shape
--   a manual, user-triggered track wants); omitted, it pulls every file on
--   the source branch into @bcToFile@ (the shape an automatic,
--   write-triggered track wants — see
--   'Server.Writer.Branch.trackFiles'\/'Storyteller.Writer.Agent.Tracker.trackBranch').
data BranchCommand
  = Track      { bcId :: Maybe T.Text, bcSource :: T.Text, bcOnlyFile :: Maybe FilePath, bcToFile :: FilePath }
  | CharGen    { bcId :: Maybe T.Text, bcPath :: FilePath, bcScenario :: T.Text, bcSeed :: Maybe Int }
  -- Run one summarization pass for @bcKind@ over this branch -- see
  -- 'Storyteller.Writer.Agent.Summarizer.runSummarizer'. No target path or
  -- range: what's new is always "everything since @bcKind@'s last summary
  -- here, or since root" -- implicit, per 'Storyteller.Common.Summary'.
  | Summarize  { bcId :: Maybe T.Text, bcKind :: T.Text }
  | AddNote    { bcId :: Maybe T.Text, bcRefTickId :: T.Text, bcNoteText :: T.Text }
  | MoveTick   { bcId :: Maybe T.Text, bcTickId :: T.Text, bcAfterTickId :: Maybe T.Text }
  | DeleteTick { bcId :: Maybe T.Text, bcTickId :: T.Text }
  -- Rebase: run @command@ as if @tickId@ were HEAD, then replay everything
  -- that came after it on top of the result — same as 'FileCommand's 'At'
  -- (see Server.Writer.File.Protocol), just for branch-level commands (e.g.
  -- a future Ticks-view rebase marker, the branch-level equivalent of the
  -- file view's drag handle — no client trigger for this exists yet, this
  -- is just the generic capability being available symmetrically).
  | At { bcId :: Maybe T.Text, bcTickId :: T.Text, bcCommand :: BranchCommand, bcBranches :: [AtBranch] }
  deriving (Show)

instance FromJSON BranchCommand where
  parseJSON = withObject "BranchCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "track"       -> Track      i <$> o .: "source" <*> o .:? "onlyFile" <*> o .: "to"
      "chargen"     -> CharGen    i <$> o .: "path" <*> o .: "scenario" <*> o .:? "seed"
      "summarize"   -> Summarize  i <$> o .: "kind"
      "add.note"    -> AddNote    i <$> o .: "refTickId" <*> o .: "text"
      "move.tick"   -> MoveTick   i <$> o .: "tickId" <*> o .:? "afterTickId"
      "delete.tick" -> DeleteTick i <$> o .: "tickId"
      "at"          -> At         i <$> o .: "tickId" <*> o .: "command" <*> (fromMaybe [] <$> o .:? "branches")
      _             -> fail ("unknown branch command: " <> T.unpack t)

-- | Short label for logging — see 'Server.Writer.File.Protocol.commandKind'.
commandKind :: BranchCommand -> T.Text
commandKind = \case
  Track {}      -> "track"
  CharGen {}    -> "chargen"
  Summarize {}  -> "summarize"
  AddNote {}    -> "add.note"
  MoveTick {}   -> "move.tick"
  DeleteTick {} -> "delete.tick"
  At _ _ inner _ -> "at:" <> commandKind inner

-- | Events the server sends on a branch connection.
--
--   BranchReady:  sent once on connect with the branch name and current file list.
--   FileAdded:    a new file appeared in the branch tree.
--   FileRemoved:  a file dropped out of the branch tree (e.g. a whole-file
--                 delete — see Server.Core.File.deleteFile).
--   BranchUpdate: tick state push — upsert all ticks, set head to updateHead.
--   AgentLog:     progress message from a running agent.
--   BranchError:  something went wrong; message is human-readable.
--
-- File tree changes still have no rename event -- a rename is observed here
-- as a 'FileRemoved' plus a 'FileAdded', not a single move. Nothing today
-- needs the distinction; add one if a client ever needs to preserve
-- per-file UI state (e.g. an open editor) across a rename.
data BranchEvent
  = BranchReady  { beId :: Maybe T.Text, beBranch :: T.Text, beFiles :: [FilePath] }
  | FileAdded    { beId :: Maybe T.Text, bePath :: FilePath }
  | FileRemoved  { beId :: Maybe T.Text, bePath :: FilePath }
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
    FileRemoved mid path ->
      object $ withId mid
        [ "type" .= ("file.removed" :: T.Text)
        , "path" .= path ]
    BranchUpdate u -> toJSON u
    AgentLog level msg ->
      object [ "type"    .= ("agent.log" :: T.Text)
             , "level"   .= level
             , "message" .= msg ]
    BranchError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
