{-# LANGUAGE OverloadedStrings #-}

-- | Shared protocol types and wire encoding used across all connection types.
--
-- The central invariant: the server pushes state to clients, clients send
-- intent commands. Clients never request a resync — reconnecting triggers
-- the full state push automatically.
module Server.Core.Protocol
  ( WireTick(..)
  , Update(..)
  , toWireTick
  , tickToWireTick
  , withId
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)
import qualified Data.Text as T

import qualified Storage.Tick as Storage
import Storyteller.Core.Types (Tick(..), TickData(..), TickPos(..), tickId, tickParent, tickTypeOf, unTickId)

-- | A tick as sent over the wire. Flat representation — the client interprets
--   kind/fields/content to decide how to render it.
data WireTick = WireTick
  { wtTickId  :: T.Text
  , wtKind    :: T.Text
  , wtRefs    :: [T.Text]
  , wtFields  :: [(T.Text, T.Text)]
  , wtMessage :: T.Text
  , wtContent :: Maybe T.Text
  , wtParent  :: Maybe T.Text
  } deriving (Show, Eq)

instance ToJSON WireTick where
  toJSON wt = object $
    [ "tickId"  .= wtTickId  wt
    , "kind"    .= wtKind    wt
    , "refs"    .= wtRefs    wt
    , "message" .= wtMessage wt
    , "parent"  .= wtParent  wt
    ] <>
    (if null (wtFields wt) then []
     else ["fields" .= object (map (\(k,v) -> fromText k .= v) (wtFields wt))]) <>
    maybe [] (\c -> ["content" .= c]) (wtContent wt)

-- | A push from the server: zero or more ticks to upsert into the client's
--   store, plus the new HEAD tick id.
--
--   Both branch and file connections use this type. On connect it carries the
--   full filtered tick list; after a mutation it carries only the affected
--   ticks. The client upserts all received ticks and updates its head pointer.
data Update = Update
  { updateTicks :: [WireTick]
  , updateHead  :: T.Text
  } deriving (Show, Eq)

instance ToJSON Update where
  toJSON u = object
    [ "type"  .= ("update" :: T.Text)
    , "ticks" .= updateTicks u
    , "head"  .= updateHead  u
    ]

-- | Convert a storage-layer FileTick (file projection) to the wire representation.
toWireTick :: Storage.FileTick -> WireTick
toWireTick ft = WireTick
  { wtTickId  = Storage.ftTickId  ft
  , wtKind    = Storage.ftKind    ft
  , wtRefs    = Storage.ftRefs    ft
  , wtFields  = Storage.ftFields  ft
  , wtMessage = Storage.ftMessage ft
  , wtContent = Storage.ftContent ft
  , wtParent  = Storage.ftParent  ft
  }

-- | Convert a chain Tick (branch-level) directly to the wire representation.
--   Kind comes from the tick's own @"type"@ field ('tickTypeOf'); falls
--   back to "tick" for an untagged one. "type" itself is dropped from the
--   outward-facing fields so it isn't duplicated alongside 'wtKind' on the
--   wire, same convention 'Storage.Tick.fileTicksOf' follows.
tickToWireTick :: Tick -> WireTick
tickToWireTick t = WireTick
  { wtTickId  = unTickId (tickId t)
  , wtKind    = maybe "tick" id (tickTypeOf t)
  , wtRefs    = map unTickId (posRefs (tickPos t))
  , wtFields  = filter ((/= "type") . fst) (tickFields (tickData t))
  , wtMessage = tickMessage (tickData t)
  , wtContent = Nothing
  , wtParent  = unTickId <$> tickParent t
  }

-- | Prepend an "id" field when present; omit the field entirely when absent.
withId :: Maybe T.Text -> [Pair] -> [Pair]
withId Nothing  ps = ps
withId (Just i) ps = ("id" .= i) : ps
