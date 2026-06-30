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
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Agent.Splitter (Splitter, splitAtoms)
import Storyteller.Edit (storeAtom)
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, StoryStorage)
import Storyteller.Types (TickId)

-- | Split @content@ into paragraph atoms, append each to @path@, and commit
-- each atom as its own tick. Returns the list of created tick IDs.
appendAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Splitter, Fail ] r )
  => FilePath -> T.Text -> Sem r [TickId]
appendAgent path content = do
  atoms <- splitAtoms content
  mapM (\atom -> storeAtom @branch path (TE.encodeUtf8 atom)) atoms
