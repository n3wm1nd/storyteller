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
  = ListBranches   { scId :: Maybe T.Text }
  | CreateBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  | DeleteBranch   { scId :: Maybe T.Text, scBranch :: T.Text }
  -- | Lightweight, name-only listing of 'character/*' branches — for
  -- selection/overview UIs that would otherwise have to connect to every
  -- character individually (see TODO.md's Characters list endpoint packet).
  -- Kept here rather than a dedicated connection since the full branch list
  -- (and its live tracking) already lives on '/session' — see
  -- 'Server.Writer.Session.Connection's notifier thread, which re-pushes
  -- 'CharacterList' whenever any 'character/*' branch ref moves.
  | ListCharacters { scId :: Maybe T.Text }
  deriving (Show)

instance FromJSON SessionCommand where
  parseJSON = withObject "SessionCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "list-branches"    -> pure (ListBranches i)
      "create-branch"    -> CreateBranch i <$> o .: "branch"
      "delete-branch"    -> DeleteBranch i <$> o .: "branch"
      "list-characters"  -> pure (ListCharacters i)
      _                  -> fail ("unknown session command: " <> T.unpack t)

-- | Short label for logging — see 'Server.Writer.File.Protocol.commandKind'.
commandKind :: SessionCommand -> T.Text
commandKind = \case
  ListBranches {}   -> "list-branches"
  CreateBranch {}   -> "create-branch"
  DeleteBranch {}   -> "delete-branch"
  ListCharacters {} -> "list-characters"

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
  | BranchList     { seId :: Maybe T.Text, seBranches :: [T.Text] }
  | BranchCreated  { seId :: Maybe T.Text, seBranch :: T.Text }
  | BranchDeleted  { seId :: Maybe T.Text, seBranch :: T.Text }
  -- | Pushed in response to 'ListCharacters', and again — unprompted,
  -- 'seId' is 'Nothing' then — whenever a 'character/*' branch is created
  -- or deleted anywhere, by any connection.
  | CharacterList  { seId :: Maybe T.Text, seCharacters :: [CharacterSummary] }
  | SessionError   T.Text
  deriving (Show)

instance ToJSON SessionEvent where
  toJSON = \case
    SessionReady' ->
      object [ "type" .= ("session.ready" :: T.Text) ]
    BranchList mid branches ->
      object $ withId mid [ "type" .= ("branch.list"    :: T.Text), "branches" .= branches ]
    BranchCreated mid branch ->
      object $ withId mid [ "type" .= ("branch.created" :: T.Text), "branch"   .= branch ]
    BranchDeleted mid branch ->
      object $ withId mid [ "type" .= ("branch.deleted" :: T.Text), "branch"   .= branch ]
    CharacterList mid characters ->
      object $ withId mid [ "type" .= ("character.list" :: T.Text), "characters" .= characters ]
    SessionError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
