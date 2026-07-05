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
  , splitMarkdownAware

    -- * Exported for tests
  , byParagraph
  , splitMarkdown
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Kind (Type)
import Polysemy

data Splitter (m :: Type -> Type) a where
  SplitAtoms :: Text -> Splitter m [Text]

splitAtoms :: Member Splitter r => Text -> Sem r [Text]
splitAtoms t = send (SplitAtoms t)

-- | Split text at heading boundaries and paragraph boundaries — see
--   'splitMarkdown'.
splitMarkdownAware :: Sem (Splitter : r) a -> Sem r a
splitMarkdownAware = interpret $ \case
  SplitAtoms text -> return (splitMarkdown True text)

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

-- | Split text at ATX heading boundaries (@#@ through @######@ at the start
--   of a line), and — when @atParagraph@ is set — also at paragraph
--   boundaries within each heading-delimited section.
--
--   A heading always starts a new atom, whether or not it's set off by a
--   blank line: this is the gap plain 'byParagraph' has (it only ever splits
--   on @"\n\n"@, so e.g. @"intro\n# Heading\nbody"@ comes back as a single
--   atom despite the embedded heading). With @atParagraph = False@ each
--   section — a heading plus everything up to the next one — is kept whole,
--   a coarser split than per-paragraph. With @atParagraph = True@,
--   'byParagraph' additionally runs within each section, so on text with no
--   headings at all @splitMarkdown True@ is exactly 'byParagraph'.
--
--   Purely structural, like 'byParagraph': @concat (splitMarkdown b t) == t@
--   for both values of @b@.
splitMarkdown :: Bool -> Text -> [Text]
splitMarkdown atParagraph t
  | atParagraph = concatMap byParagraph sections
  | otherwise   = sections
  where
    sections = splitStructural t

-- | Group lines into sections, starting a new section at each heading line.
splitStructural :: Text -> [Text]
splitStructural = reverse . map T.concat . foldl step [] . linesWithTerminators
  where
    step acc line
      | isHeadingLine line = [line] : acc
      | otherwise = case acc of
          []              -> [[line]]
          (cur : rest)    -> (cur ++ [line]) : rest

-- | An ATX heading line: 1-6 @#@ at the very start of the line, followed by
--   a space or end of line. Leading indentation is deliberately not
--   recognized (CommonMark allows up to 3 spaces) — out of scope for this
--   splitter, which only needs to recognize the common, unindented case.
isHeadingLine :: Text -> Bool
isHeadingLine line =
  let (hashes, rest) = T.span (== '#') line
      n               = T.length hashes
  in n >= 1 && n <= 6 && (T.null rest || T.head rest == ' ' || T.head rest == '\n')

-- | Split text into lines, each retaining its own trailing @"\n"@ — unlike
--   'Data.Text.lines', which can't distinguish a trailing newline from its
--   absence (both @"a\\nb"@ and @"a\\nb\\n"@ give @["a","b"]@). Here the last
--   element has no terminator iff the input itself had none, so
--   @concat (linesWithTerminators t) == t@ always.
linesWithTerminators :: Text -> [Text]
linesWithTerminators t
  | T.null t  = []
  | otherwise =
      let (line, rest) = T.break (== '\n') t
      in case T.uncons rest of
           Nothing        -> [line]
           Just (_, rest') -> (line <> "\n") : linesWithTerminators rest'
