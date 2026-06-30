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
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)

import Storyteller.Agent.Splitter (Splitter, splitAtoms)
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, store)
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

appendOne
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
appendOne path content = do
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content)
  store @branch (T.take 60 content)
