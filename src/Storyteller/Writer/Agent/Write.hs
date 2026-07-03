{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Write agent: wire charSummaryAgent → continueFileAgent → split → append.
--
-- All branch and filesystem interpreters must be in scope at the call site.
-- This module only composes — no interpreters are launched here.
module Storyteller.Writer.Agent.Write
  ( writeAgent
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharContextBlock(..), CharLabel(..), ContextBlock(..), WordCount(..))
import Storyteller.Writer.Agent.Continuation (continueFileAgent)
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Append (append)
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Git (BranchTag(..))
import Storyteller.Core.Runtime (StoryModel)
import Storyteller.Core.Storage (StoryBranch)
import Storyteller.Core.Types (TickId)

-- | Generate prose and commit it, given the target branch and any number of
--   character branches already open on the effect stack.
--
--   @charProjects@ is a list of @(label, charSummaryAgent action)@ — one per
--   active character branch. Each action runs in the character branch's
--   filesystem context, which the caller has already opened.
writeAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, Splitter, Logging, Fail ] r )
  => FilePath                                       -- ^ file to append to
  -> Instruction
  -> [ContextBlock]                                  -- ^ extra context (e.g. user's pinned selection)
  -> [(CharLabel, Sem r [CharContextBlock])]         -- ^ (label, summary action) per active char branch
  -> Sem r [TickId]
writeAgent path instruction extraContext charActions = do
  charContexts <- fmap concat $ mapM (\(CharLabel name, action) -> do
    blocks <- action
    return $ CharContextBlock ("## Character: " <> name) : blocks) charActions
  Prose generated <- continueFileAgent @project @StoryModel
                       modelConfigs (Just (WordCount 300)) charContexts extraContext path instruction
  mapM (append @branch path) =<< splitAtoms generated
