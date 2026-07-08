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

import Server.Core.Protocol (withId)

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
-- client to render and jump to it, not the full per-ref snapshot ('undoRefs'
-- never crosses the wire — it's only used server-side to derive
-- 'weRevertsTo', see 'Server.Writer.Session.Dispatch.annotateReverts').
-- 'weRevertsTo' is set iff this entry's snapshot exactly repeats an earlier
-- entry's — i.e. this is where a reset landed — so a client can tell a
-- straight-line append from a jump back without ever seeing raw ref state.
data WireUndoEntry = WireUndoEntry
  { weId        :: T.Text
  , weTime      :: UTCTime
  , weRevertsTo :: Maybe T.Text
  } deriving (Show)

instance ToJSON WireUndoEntry where
  toJSON e = object [ "id" .= weId e, "time" .= weTime e, "revertsTo" .= weRevertsTo e ]

data SessionEvent
  = SessionReady'
  -- | Always unprompted: pushed once right after 'SessionReady'' with the
  -- current list, and again whenever any branch ref moves anywhere (see
  -- 'Server.Writer.Session.Connection's notifier) — a session never has to
  -- ask for this, only listen for it.
  | BranchList     { seBranches :: [T.Text] }
  | BranchCreated  { seId :: Maybe T.Text, seBranch :: T.Text }
  | BranchDeleted  { seId :: Maybe T.Text, seBranch :: T.Text }
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
    BranchCreated mid branch ->
      object $ withId mid [ "type" .= ("branch.created" :: T.Text), "branch"   .= branch ]
    BranchDeleted mid branch ->
      object $ withId mid [ "type" .= ("branch.deleted" :: T.Text), "branch"   .= branch ]
    CharacterList characters ->
      object [ "type" .= ("character.list" :: T.Text), "characters" .= characters ]
    UndoLog entries ->
      object [ "type" .= ("undo.log" :: T.Text), "entries" .= entries ]
    SessionError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
