{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Fixer: given an instruction and a set of existing atoms flagged as the
-- target of that instruction, edit each flagged atom in place via
-- 'reworkAtomsAt' (see @Storyteller.Writer.Agent.ReplaceTool@) — one single-turn,
-- single-atom tool call per target, so the model decides per-atom whether a
-- change is even warranted.
module Storyteller.Writer.Agent.Fix
  ( fixAgent
  ) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)

import Storyteller.Writer.Agent (Instruction(..), Prose(..), ContextBlock(..), WordCount(..))
import Storyteller.Writer.Agent.Continuation (continueFileAgent)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt)
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Append (append)
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Runtime (StoryModel)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, FileTick(..), fileTicks)
import Storyteller.Core.Types (TickId(..))

-- | @targets@ is the set of atoms the user selected as the subject of
--   @instruction@. Empty is valid — a future self-selecting Fixer (picking
--   its own target via tool calls) is the planned upgrade for that case;
--   for now it just behaves like Writer with no extra context.
fixAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Splitter, Fail ] r )
  => FilePath
  -> [TickId]                -- ^ targets: atoms flagged for fixing
  -> Instruction
  -> [ContextBlock]           -- ^ extra context (e.g. user's pinned selection)
  -> Sem r [TickId]
fixAgent path targets instruction extraContext = do
  ticks0 <- fileTicks @branch path
  let idxs = mapMaybe (\t -> elemIndex (unTickId t) (map ftTickId ticks0)) targets
  if null idxs
    then do
      Prose generated <- continueFileAgent @project @StoryModel
                           modelConfigs (Just (WordCount 300)) [] extraContext path instruction
      mapM (append @branch path) =<< splitAtoms generated
    else reworkAtomsAt @branch @project path instruction idxs
