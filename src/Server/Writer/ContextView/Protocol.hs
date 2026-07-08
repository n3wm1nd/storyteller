{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for @\/branch\/{name}\/$context\/{path}@ connections.
--
-- Commands: 'PreviewContext' — a full, self-contained description of the
--           slots to preview. Every request carries everything needed to
--           answer it, the same discipline an LLM call's full history
--           follows: nothing about a submitted filter persists across
--           requests, so a client never needs to reconstruct or diff
--           against server-held state.
-- Events:   'ContextPreview' — the resolved entries per slot, pushed once
--           per command and again whenever the underlying branch changes
--           (re-resolved against the most recently submitted slots — see
--           'Server.Writer.ContextView.Connection').
module Server.Writer.ContextView.Protocol
  ( ContextMode(..)
  , ContextSlot(..)
  , ContextViewCommand(..)
  , ContextEntry(..)
  , ContextSlotPreview(..)
  , ContextViewEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Storyteller.Writer.Agent.ContextPreview
  ( ContextMode(..), ContextSlot(..)
  , ContextEntry(..), ContextSlotPreview(..) )

instance FromJSON ContextMode where
  parseJSON = withText "ContextMode" $ \case
    "ambient"   -> pure Ambient
    "on-demand" -> pure OnDemand
    other       -> fail ("unknown context mode: " <> T.unpack other)

instance ToJSON ContextMode where
  toJSON Ambient  = String "ambient"
  toJSON OnDemand = String "on-demand"

instance FromJSON ContextSlot where
  parseJSON = withObject "ContextSlot" $ \o ->
    ContextSlot
      <$> o .: "label"
      <*> o .: "mode"
      <*> (fromMaybe [] <$> o .:? "layout")

instance ToJSON ContextEntry where
  toJSON e = object $
    [ "path" .= cePath e, "bucket" .= ceBucket e ] <>
    maybe [] (\c -> ["content" .= c]) (ceContent e) <>
    maybe [] (\b -> ["blurb"   .= b]) (ceBlurb   e)

instance ToJSON ContextSlotPreview where
  toJSON sp = object
    [ "label"   .= cspLabel   sp
    , "mode"    .= cspMode    sp
    , "entries" .= cspEntries sp
    ]

-- | Commands the client may send on a context-view connection.
data ContextViewCommand
  = PreviewContext { cvId :: Maybe T.Text, cvSlots :: [ContextSlot] }
  deriving (Show)

instance FromJSON ContextViewCommand where
  parseJSON = withObject "ContextViewCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "context.preview" -> PreviewContext i <$> o .: "slots"
      _                 -> fail ("unknown context-view command: " <> T.unpack t)

-- | Events the server sends on a context-view connection.
data ContextViewEvent
  = ContextPreviewed { cveId :: Maybe T.Text, cveSlots :: [ContextSlotPreview] }
  | ContextViewError T.Text
  deriving (Show)

instance ToJSON ContextViewEvent where
  toJSON = \case
    ContextPreviewed mid slots ->
      object $
        [ "type"  .= ("context.preview" :: T.Text)
        , "slots" .= slots
        ] <> maybe [] (\i -> ["id" .= i]) mid
    ContextViewError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
