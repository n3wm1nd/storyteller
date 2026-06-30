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

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)

import Storyteller.Agent.Splitter (Splitter, splitAtoms)
import Storyteller.Atom (AtomDiff(..), storeAtomDiff, treeRef)
import Storyteller.Storage (StoryBranch, StoryStorage, storeAs, get)
import Storyteller.Types (TickId, TickType(..), tickId)

-- | Split @content@ into paragraph atoms, append each to @path@, and commit
-- each atom as its own tick. Returns the list of created tick IDs.
appendAgent
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Git, Splitter, Fail] r
  => FilePath -> T.Text -> Sem r [TickId]
appendAgent path content = do
  atoms <- splitAtoms content
  mapM (appendOne @branch path) atoms

appendOne
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Git, Fail] r
  => FilePath -> T.Text -> Sem r TickId
appendOne path content = do
  headTick  <- get @branch
  parentTree <- treeRef (tickId headTick)
  atom      <- storeAtomDiff parentTree (AtomDiff path (TE.encodeUtf8 content))
  storeAs @branch atom
