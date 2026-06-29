{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /session connections.
--
-- Commands: branch management operations that don't require an open branch.
-- Events:   branch list, confirmation, errors.
module Server.Session.Protocol
  ( SessionCommand(..)
  , SessionEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser, Pair)
import qualified Data.Text as T

data SessionCommand
  = ListBranches { scId :: Maybe T.Text }
  | CreateBranch { scId :: Maybe T.Text, scBranch :: T.Text }
  | DeleteBranch { scId :: Maybe T.Text, scBranch :: T.Text }
  deriving (Show)

instance FromJSON SessionCommand where
  parseJSON = withObject "SessionCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "list-branches"  -> pure (ListBranches i)
      "create-branch"  -> CreateBranch i <$> o .: "branch"
      "delete-branch"  -> DeleteBranch i <$> o .: "branch"
      _                -> fail ("unknown session command: " <> T.unpack t)

data SessionEvent
  = SessionReady'
  | BranchList    { seId :: Maybe T.Text, seBranches :: [T.Text] }
  | BranchCreated { seId :: Maybe T.Text, seBranch :: T.Text }
  | BranchDeleted { seId :: Maybe T.Text, seBranch :: T.Text }
  | SessionError  T.Text
  deriving (Show)

instance ToJSON SessionEvent where
  toJSON = \case
    SessionReady' ->
      object [ "type" .= ("session.ready" :: T.Text) ]
    BranchList  mid branches ->
      object $ withId mid [ "type" .= ("branch.list"    :: T.Text), "branches" .= branches ]
    BranchCreated mid branch ->
      object $ withId mid [ "type" .= ("branch.created" :: T.Text), "branch" .= branch ]
    BranchDeleted mid branch ->
      object $ withId mid [ "type" .= ("branch.deleted" :: T.Text), "branch" .= branch ]
    SessionError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]

withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
