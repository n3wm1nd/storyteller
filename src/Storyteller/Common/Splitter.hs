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
-- call an LLM for semantic splitting). Lives in 'Common' rather than
-- 'Storyteller.Writer.Agent' — closer to a type/effect declaration any app
-- could want than to app-specific policy, and 'Storyteller.Core.Append'
-- needs the option to be split-aware without depending on Writer.
module Storyteller.Common.Splitter
  ( -- * Effect
    Splitter(..)
  , splitAtoms

    -- * Interpreters
  , splitByParagraph

    -- * Exported for tests
  , byParagraph
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

-- | Split text into atoms at paragraph boundaries (runs of 2+ newlines).
--
-- Purely structural: the delimiter (the full newline run) is appended to the
-- preceding atom, so @concat (byParagraph t) == t@ for all @t@. No character
-- is ever added or removed — the atoms are just views into the original bytes.
byParagraph :: Text -> [Text]
byParagraph t = dropTrailingEmpty (go t)
  where
    go s =
      let (chunk, rest) = T.breakOn "\n\n" s
          (delim, after) = T.span (== '\n') rest
      in if T.null rest
           then [chunk]
           else (chunk <> delim) : go after
    -- A trailing empty atom arises when input ends with a delimiter run.
    -- That's not a real split — nothing follows.
    dropTrailingEmpty [] = []
    dropTrailingEmpty xs = if T.null (last xs) then init xs else xs
