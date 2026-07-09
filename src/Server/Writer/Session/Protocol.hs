{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /session connections.
--
-- Commands: branch management. No tick or file operations at this level.
-- Events:   branch list, confirmations, errors.
-- No resync command — reconnect triggers a fresh state push.
module Server.Writer.Session.Protocol
  ( SessionCommand(..)
  , SessionEvent(..)
  , CharacterSummary(..)
  , WireUndoEntry(..)
  , commandKind
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import Data.Time (UTCTime)

data SessionCommand
  = CreateBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  | DeleteBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  -- | Restore every tracked ref to the state recorded by 'scEntryId' (an
  -- undo-log entry's own id, from a previously-pushed 'UndoLog') — see
  -- 'Storyteller.Core.Undo.resetToUndo'. Symmetric: 'scEntryId' can name any
  -- entry, earlier or later than the current one, so this doubles as both
  -- undo and redo.
  | UndoReset      { scId :: Maybe T.Text, scEntryId :: T.Text }
  deriving (Show)

instance FromJSON SessionCommand where
  parseJSON = withObject "SessionCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "create-branch"    -> CreateBranch i <$> o .: "branch"
      "delete-branch"    -> DeleteBranch i <$> o .: "branch"
      "undo.reset"       -> UndoReset i <$> o .: "entryId"
      _                  -> fail ("unknown session command: " <> T.unpack t)

-- | Short label for logging — see 'Server.Writer.File.Protocol.commandKind'.
commandKind :: SessionCommand -> T.Text
commandKind = \case
  CreateBranch {}   -> "create-branch"
  DeleteBranch {}   -> "delete-branch"
  UndoReset {}      -> "undo.reset"

-- | One character branch's sidebar-facing summary: the branch id (still
--   carrying the @character\/@ prefix; stripping it is a display concern,
--   not this event's job) plus its raw @sheet.md@ content, if any. Raw,
--   not processed — same "read is raw-but-complete" rule as everywhere else
--   in this protocol (see WS-PROTOCOL.md): the server doesn't extract a
--   display name from the sheet's H1 line, it just hands over the sheet;
--   the client decodes that into a display name the same way it decodes
--   any other raw content into a concept it needs.
data CharacterSummary = CharacterSummary
  { csBranch :: T.Text
  , csSheet  :: Maybe T.Text
  } deriving (Show)

instance ToJSON CharacterSummary where
  toJSON cs = object [ "branch" .= csBranch cs, "sheet" .= csSheet cs ]

-- | One 'Storyteller.Core.Undo.UndoEntry', wire-shaped: just enough for a
-- client to render and jump to it, not the full per-ref snapshot
-- ('undoRefs' never crosses the wire). No "current"/"redo" marker here —
-- deliberately: 'Storyteller.Core.Undo' only ever reports what real writes
-- happened, not where any one viewer currently is (see its own haddock).
-- A client derives "which dot is active right now" itself, from which
-- entry it last jumped to and whether this list has grown since — see
-- app/undo-timeline.tsx. 'weKind' is 'Storyteller.Core.Undo.undoKind'
-- as-is — an opaque tag ("atom", "root", "note", ...), absent for a
-- deletion or anything that didn't decode one; the client owns turning it
-- into a color, this event doesn't presume to.
data WireUndoEntry = WireUndoEntry
  { weId   :: T.Text
  , weTime :: UTCTime
  , weKind :: Maybe T.Text
  } deriving (Show)

instance ToJSON WireUndoEntry where
  toJSON e = object [ "id" .= weId e, "time" .= weTime e, "kind" .= weKind e ]

data SessionEvent
  = SessionReady'
  -- | Always unprompted: pushed once right after 'SessionReady'' with the
  -- current list, again whenever any branch ref moves anywhere (see
  -- 'Server.Writer.Session.Connection's notifier), and once more, directly,
  -- to the connection that just sent 'CreateBranch'/'DeleteBranch' (see
  -- 'Dispatch.runCommand') so it doesn't have to wait on the 'RefMoved'
  -- round trip it triggered itself — same one-list-no-confirmation-event
  -- shape either way, so a client never has to reconcile an incremental
  -- create/delete notice against this list.
  | BranchList     { seBranches :: [T.Text] }
  -- | Same "unprompted only" shape as 'BranchList', scoped to 'character/*'
  -- branches — see 'Server.Writer.Session.Connection's notifier.
  | CharacterList  { seCharacters :: [CharacterSummary] }
  -- | Same "unprompted only" shape as 'BranchList' — pushed once after
  -- 'SessionReady'' and again on every branch-ref move (see
  -- 'Server.Writer.Session.Connection's notifier), plus once more, directly,
  -- to the connection that just sent 'UndoReset' (see 'Dispatch.runCommand')
  -- so it doesn't have to wait on a 'RefMoved' round trip it triggered
  -- itself. Chronological, oldest first — the order a timeline renders in.
  | UndoLog        { seUndoEntries :: [WireUndoEntry] }
  | SessionError   T.Text
  deriving (Show)

instance ToJSON SessionEvent where
  toJSON = \case
    SessionReady' ->
      object [ "type" .= ("session.ready" :: T.Text) ]
    BranchList branches ->
      object [ "type" .= ("branch.list"    :: T.Text), "branches" .= branches ]
    CharacterList characters ->
      object [ "type" .= ("character.list" :: T.Text), "characters" .= characters ]
    UndoLog entries ->
      object [ "type" .= ("undo.log" :: T.Text), "entries" .= entries ]
    SessionError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
