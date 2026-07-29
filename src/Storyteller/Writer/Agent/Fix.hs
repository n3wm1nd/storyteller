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
-- With no targets at all, 'reworkWholeFile' takes over instead: the model
-- reviews the whole file against the instruction and edits whatever it
-- finds warranted, still under the same exact-match-once discipline —
-- "nothing selected" means the model does its own scoping, not "fall back
-- to plain generation" (that's a different policy, still
-- 'Server.Writer.File.chatWriter''s to pick, e.g. for a bare "write more"
-- with no fix instruction at all).
module Storyteller.Writer.Agent.Fix
  ( fixAgent
  ) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Polysemy
import Polysemy.Fail (Fail)

import Runix.FileSystem (FileSystemRead, FileSystemWrite)
import Runix.Logging (Logging)

import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Writer.Agent (Instruction)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt, reworkWholeFile)
import Storyteller.Core.Prompt (PromptStorage)
import Storage.Tick (FileTick(..))
import qualified Storage.Tick as Tick
import Storyteller.Core.Git (BranchOp, BranchTag, runStorage)
import Storyteller.Core.Types (TickId(..))

-- | Always the 'AgentModel' role -- see 'Storyteller.Core.LLM.Role.LLMs'.
--
--   Empty @targets@ is a real mode, not an error -- see
--   'reworkWholeFile': with nothing caller-selected, the model is free to
--   review the whole file and fix whatever it finds warranted, still
--   inside the same exact-match-once discipline every other fix here
--   uses. Non-empty @targets@ stays the precise per-atom path.
fixAgent
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, BranchOp branch, FileSystemRead (BranchTag branch), FileSystemWrite (BranchTag branch), Fail, Logging] r)
  => FilePath
  -> [TickId]                -- ^ targets: atoms flagged for fixing, or empty for a free-roam pass
  -> Instruction
  -> Sem r [TickId]
fixAgent path [] instruction = [] <$ reworkWholeFile @branch path instruction
fixAgent path targets instruction = do
  ticks0 <- runStorage @branch (Tick.fileTicksOf path)
  let idxs = mapMaybe (\t -> elemIndex (unTickId t) (map ftTickId ticks0)) targets
  reworkAtomsAt @branch path instruction idxs
