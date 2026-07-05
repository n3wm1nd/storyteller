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
  , commandKind
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Server.Core.Protocol (withId)

data SessionCommand
  = CreateBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  | DeleteBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  deriving (Show)

instance FromJSON SessionCommand where
  parseJSON = withObject "SessionCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "create-branch"    -> CreateBranch i <$> o .: "branch"
      "delete-branch"    -> DeleteBranch i <$> o .: "branch"
      _                  -> fail ("unknown session command: " <> T.unpack t)

-- | Short label for logging — see 'Server.Writer.File.Protocol.commandKind'.
commandKind :: SessionCommand -> T.Text
commandKind = \case
  CreateBranch {}   -> "create-branch"
  DeleteBranch {}   -> "delete-branch"

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
    SessionError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
