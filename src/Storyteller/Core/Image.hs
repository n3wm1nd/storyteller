{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Image: an image attachment tick.
--
-- An 'Image' is a tick that attaches an image to a file's timeline without
-- carrying the image bytes itself. The bytes live in a sibling 'Storage.Core.Binary'
-- file (typically under @\<file\>.assets\/@); this tick just carries the
-- attaching file's path (the @"file"@ hint, same role as 'Storyteller.Core.Atom.Atom's)
-- and the sibling asset's own path (the @"asset"@ field), so per-file views
-- can filter the chain the same way they already do for atoms.
module Storyteller.Core.Image
  ( Image(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Core.Types (TickType(..), TickData(..), Tick(..), encodeDraft, decodePayload)

data Image = Image
  { imageFile    :: FilePath  -- ^ the file whose timeline this tick belongs to
  , imageAsset   :: FilePath  -- ^ the sibling Binary tick's own path
  , imageCaption :: Text      -- ^ empty if none
  } deriving (Show, Eq)

instance TickType Image where
  tickTypeName = "image"

  toDraft (Image file asset caption) = encodeDraft @Image []
    [ ("file", T.pack file), ("asset", T.pack asset) ]
    caption

  fromTick t = do
    caption <- decodePayload @Image t
    let fields = tickFields (tickData t)
    file  <- lookup "file"  fields
    asset <- lookup "asset" fields
    Just Image { imageFile = T.unpack file, imageAsset = T.unpack asset, imageCaption = caption }
