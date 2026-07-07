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
import Runix.LLM (LLM)

import Storyteller.Writer.Agent (Instruction)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.Runtime (StoryModel)
import Storage.Tick (FileTick(..), fileTicksOf)
import Storyteller.Core.Types (TickId(..))

fixAgent
  :: forall branch r
  .  Members '[LLM StoryModel, PromptStorage, BranchOp branch, Fail] r
  => FilePath
  -> [TickId]                -- ^ targets: atoms flagged for fixing (non-empty)
  -> Instruction
  -> Sem r [TickId]
fixAgent path targets instruction = do
  (ticks0, _) <- runStorage @branch (fileTicksOf path)
  let idxs = mapMaybe (\t -> elemIndex (unTickId t) (map ftTickId ticks0)) targets
  reworkAtomsAt @branch path instruction idxs
