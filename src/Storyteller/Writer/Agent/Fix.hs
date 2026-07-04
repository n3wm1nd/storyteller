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
--
-- @targets@ must be non-empty; there is no fallback to plain generation
-- here — "no targets selected, just write" is a different policy than
-- "rework these atoms," and the caller already knows which one it wants
-- before calling anything (see 'Server.Writer.File.chatFixer', which picks
-- between this and 'Storyteller.Writer.Agent.Write.writeAgent').
module Storyteller.Writer.Agent.Fix
  ( fixAgent
  ) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)

import Storyteller.Writer.Agent (Instruction)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Runtime (StoryModel)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, FileTick(..), fileTicks)
import Storyteller.Core.Types (TickId(..))

fixAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => FilePath
  -> [TickId]                -- ^ targets: atoms flagged for fixing (non-empty)
  -> Instruction
  -> Sem r [TickId]
fixAgent path targets instruction = do
  ticks0 <- fileTicks @branch path
  let idxs = mapMaybe (\t -> elemIndex (unTickId t) (map ftTickId ticks0)) targets
  reworkAtomsAt @branch @project path instruction idxs
