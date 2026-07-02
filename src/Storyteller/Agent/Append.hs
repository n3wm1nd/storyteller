{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Append agent: split caller-provided text into paragraph atoms and commit
-- each as its own tick. The simplest write path — no LLM involved.
--
-- Richer agents compose this at the end of their pipeline.
module Storyteller.Agent.Append
  ( appendAgent
  , appendUnsplit
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)

import Storyteller.Agent.Splitter (Splitter, splitAtoms)
import Storyteller.Atom (Atom(..))
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, storeAs)
import Storyteller.Types (TickId)

import Prelude hiding (appendFile)

-- | Split @content@ into paragraph atoms, append each to @path@, and commit
-- each as its own tick. Returns the list of created tick IDs.
appendAgent
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Splitter, Fail ] r
  => FilePath -> T.Text -> Sem r [TickId]
appendAgent path content = do
  atoms <- splitAtoms content
  mapM (appendOne @branch path) atoms

-- | Append @content@ as a single atom, unsplit — one tick, verbatim.
--   For callers where the content is already a deliberate, whole unit (e.g.
--   a person typing and appending their own text) rather than something
--   that benefits from being decomposed into paragraph-sized atoms (e.g.
--   LLM-generated prose, which 'appendAgent' is for). Doesn't need
--   'Splitter' at all.
appendUnsplit
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
appendUnsplit = appendOne @branch

appendOne
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
appendOne path content = do
  let content' = ensureTrailingNewline content
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content')
  storeAs @branch (Atom path content')

-- | Ensure text ends with a newline — an appended atom is one text block on
-- disk, and a block should end its line.
ensureTrailingNewline :: T.Text -> T.Text
ensureTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"
