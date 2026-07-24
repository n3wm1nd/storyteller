{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for @\/branch\/{name}\/$context\/{path}@ connections.
--
-- Commands: 'PreviewContext' — a full, self-contained Context DSL program
--           to run against the branch, plus the @path@ it should resolve
--           against (the same @path:@ parameter every @context.writer@
--           call takes). Every request carries everything needed to answer
--           it, the same discipline an LLM call's full history follows:
--           nothing about a submitted program persists across requests, so
--           a client never needs to reconstruct or diff against
--           server-held state.
-- Events:   'ContextPreviewed' — the resolved tree, pushed once per command
--           and again whenever the underlying branch changes (re-resolved
--           against the most recently submitted program — see
--           'Server.Writer.ContextView.Connection').
module Server.Writer.ContextView.Protocol
  ( PreviewNode(..)
  , ContextViewCommand(..)
  , ContextViewEvent(..)
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T

import Storyteller.Writer.Agent.ContextPreview (PreviewNode(..))

instance ToJSON PreviewNode where
  toJSON n = object
    [ "content" .= pnContent n
    , "entries" .= [ object [ "name" .= name, "node" .= toJSON node ] | (name, node) <- pnEntries n ]
    ]

-- | Commands the client may send on a context-view connection.
data ContextViewCommand
  = PreviewContext { cvId :: Maybe T.Text, cvPath :: FilePath, cvProgram :: T.Text }
  deriving (Show)

instance FromJSON ContextViewCommand where
  parseJSON = withObject "ContextViewCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "context.preview" -> PreviewContext i <$> o .: "path" <*> o .: "program"
      _                 -> fail ("unknown context-view command: " <> T.unpack t)

-- | Events the server sends on a context-view connection.
data ContextViewEvent
  = ContextPreviewed { cveId :: Maybe T.Text, cveResult :: PreviewNode }
  | ContextViewError T.Text
  deriving (Show)

instance ToJSON ContextViewEvent where
  toJSON = \case
    ContextPreviewed mid result ->
      object $
        [ "type"   .= ("context.preview" :: T.Text)
        , "result" .= result
        ] <> maybe [] (\i -> ["id" .= i]) mid
    ContextViewError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
