{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Created: the file-creation tick kind.
--
-- A 'Created' tick records a path's introduction into the tree, with empty
-- content — the file exists (tracked, present) from this tick onward, but
-- carries nothing yet. Whatever content follows lands as ordinary 'Atom'
-- ticks after it, same as any other append. Carries the same @"file"@
-- tickField hint as 'Storyteller.Core.Atom.Atom' so per-file views
-- ('Storyteller.Core.Storage.fileTicks') pick it up.
module Storyteller.Core.Created
  ( Created(..)
  ) where

import qualified Data.Text as T

import Storyteller.Core.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

newtype Created = Created { createdFile :: FilePath }
  deriving (Show, Eq)

instance TickType Created where
  tickTypeName = "created"

  toDraft (Created file) = encodeDraft @Created [] [ ("file", T.pack file) ] ""

  fromTick t = do
    _    <- decodePayload @Created t
    file <- lookup "file" (tickFields (tickData t))
    Just (Created (T.unpack file))
