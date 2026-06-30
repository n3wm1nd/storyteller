{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Atom: the file-append tick kind.
--
-- An 'Atom' is a tick that records a file append. It carries a file path hint
-- (stored in 'tickFields' as @"file"@) so that per-file views can filter the
-- chain without diffing every commit's tree.
module Storyteller.Atom
  ( Atom(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

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
