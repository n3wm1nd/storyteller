{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for @\/lore\/{name}@ connections — see WS-PROTOCOL.md.
--
-- No commands: this connection is read-only, same as
-- 'Server.Writer.Character.Protocol' — curating which codex entries feed an
-- agent's context happens through the ordinary @writer:story@ context
-- filter (see 'Storyteller.Writer.Agent.ContextFilter'), not through this
-- connection. Events: a single tree push, sent on connect and again
-- whenever the branch changes.
module Server.Writer.Lore.Protocol
  ( LoreEvent(..)
  ) where

import Data.Aeson
import qualified Data.Text as T

import Storyteller.Writer.Lore (LoreNode(..))

data LoreEvent
  = LoreTree { leNodes :: [LoreNode] }
  | LoreError T.Text
  deriving (Show)

instance ToJSON LoreEvent where
  toJSON = \case
    LoreTree nodes -> object [ "type" .= ("lore.tree" :: T.Text), "nodes" .= map nodeToJSON nodes ]
    LoreError msg  -> object [ "type" .= ("error" :: T.Text), "message" .= msg ]

nodeToJSON :: LoreNode -> Value
nodeToJSON n = object
  [ "path"     .= lnPath n
  , "name"     .= lnName n
  , "blurb"    .= lnBlurb n
  , "aliases"  .= lnAliases n
  , "children" .= map nodeToJSON (lnChildren n)
  ]
