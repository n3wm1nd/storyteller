{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /character/{charBranch} connections.
--
-- Commands: none yet — read-only. Every field on 'CharacterUpdate' is
--   collected-and-augmented server-side (see 'Server.Writer.Character'),
--   never edited through this connection; sheet edits go through the file
--   connection for sheet.md.
-- Events:   a single content push, sent on connect and again whenever the
--   underlying data changes. No presence/absence tri-state the way a file
--   connection has — a character branch that doesn't exist yet simply isn't
--   connectable (see 'Server.Writer.Character.Connection').
module Server.Writer.Character.Protocol
  ( CharacterEvent(..)
  ) where

import Data.Aeson hiding (Error)
import qualified Data.Text as T

data CharacterEvent
  = CharacterUpdate { ceName :: T.Text, ceSheet :: Maybe T.Text, ceHasAvatar :: Bool }
  | CharacterError  T.Text
  deriving (Show)

instance ToJSON CharacterEvent where
  toJSON = \case
    CharacterUpdate name sheet hasAvatar ->
      object $
        [ "type" .= ("character.update" :: T.Text)
        , "name" .= name
        , "avatar" .= hasAvatar
        ] <> maybe [] (\s -> ["sheet" .= s]) sheet
    CharacterError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
