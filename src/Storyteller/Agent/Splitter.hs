{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Atom splitter effect.
--
-- Defines the policy for how raw text is divided into atoms — the finest
-- granularity at which content is addressable in a branch. The policy is
-- expressed as an effect so callers are not coupled to it and interceptors
-- can override it (e.g. to split by sentence instead of paragraph, or to
-- call an LLM for semantic splitting).
module Storyteller.Agent.Splitter
  ( -- * Effect
    Splitter(..)
  , splitAtoms

    -- * Interpreters
  , splitByParagraph
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Kind (Type)
import Polysemy

data Splitter (m :: Type -> Type) a where
  SplitAtoms :: Text -> Splitter m [Text]

splitAtoms :: Member Splitter r => Text -> Sem r [Text]
splitAtoms t = send (SplitAtoms t)

-- | Split text into atoms at paragraph boundaries (blank lines).
--   Empty paragraphs and whitespace-only blocks are dropped.
splitByParagraph :: Sem (Splitter : r) a -> Sem r a
splitByParagraph = interpret $ \case
  SplitAtoms text -> return (byParagraph text)

byParagraph :: Text -> [Text]
byParagraph text = map (T.intercalate "\n") $ filter (not . null) $ go $ T.lines text
  where
    go [] = []
    go ls =
      let (block, rest) = break T.null ls
          trimmedBlock  = dropWhile T.null block
      in case trimmedBlock of
        [] -> go (dropWhile T.null rest)
        _  -> trimmedBlock : go (dropWhile T.null rest)
