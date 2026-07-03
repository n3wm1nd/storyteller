{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Write agent: wire charSummaryAgent → continueFileAgent → appendAgent.
--
-- All branch and filesystem interpreters must be in scope at the call site.
-- This module only composes — no interpreters are launched here.
module Storyteller.Agent.Write
  ( writeAgent
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Storyteller.Agent (Instruction(..), Prose(..), CharContextBlock(..), CharLabel(..), ContextBlock(..), WordCount(..))
import Storyteller.Agent.Append (appendAgent)
import Storyteller.Agent.Continuation (continueFileAgent)
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.CLI.Env (modelConfigs)
import Storyteller.Git (BranchTag(..))
import Storyteller.Runtime (StoryModel)
import Storyteller.Storage (StoryBranch)
import Storyteller.Types (TickId)

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
  appendAgent @branch path generated
