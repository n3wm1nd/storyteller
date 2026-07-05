{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Atom: the file-append tick kind.
--
-- An 'Atom' is a tick that records a file append. It carries a file path hint
-- (stored in 'tickFields' as @"file"@) so that per-file views can filter the
-- chain without diffing every commit's tree.
module Storyteller.Core.Atom
  ( Atom(..)
  , contentFor
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Core.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

data Atom = Atom
  { atomFile    :: FilePath
  , atomMessage :: Text
  } deriving (Show, Eq)

instance TickType Atom where
  tickTypeName = "atom"

  toDraft (Atom file msg) = encodeDraft @Atom []
    [ ("file", T.pack file) ]
    msg

  fromTick t = do
    msg  <- decodePayload @Atom t
    file <- lookup "file" (tickFields (tickData t))
    Just Atom { atomFile = T.unpack file, atomMessage = msg }

-- | The bytes this tick itself contributed to @path@, if any.
--
--   An atom's own content lives verbatim in its commit message (see
--   'toDraft'), so this is the one place that recovers it — every caller
--   that used to reconstruct a tick's contribution by diffing filesystem
--   snapshots (before/after, or tick/parent) should go through this
--   instead. Empty for non-atom ticks and for atoms on a different file.
contentFor :: FilePath -> Tick -> Text
contentFor path t = case fromTick t of
  Just (Atom file msg) | file == path -> msg
  _                                   -> ""
