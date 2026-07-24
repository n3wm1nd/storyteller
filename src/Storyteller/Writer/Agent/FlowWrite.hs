{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | FlowWriter: like 'Storyteller.Writer.Agent.Write.writeAgent', but aware that a
-- generation may already have been in flight when the user typed this
-- instruction. 'flowWriteAgent' is given 'flowTid' — the tick that was HEAD
-- at type-time, not at execution-time — so it can tell which atoms it's
-- about to see were generated provisionally, after the user had already
-- moved on.
--
-- Before continuing, it revises that provisional span in place: each atom
-- generated since 'flowTid' gets its own single-atom 'reworkAtomsAt' call
-- (see @Storyteller.Writer.Agent.ReplaceTool@) against the new instruction, so the
-- model can patch anything the new instruction invalidates before building
-- on top of it. That rework genuinely needs filesystem/storage access (tick
-- ids shift as each replacement rebases, see 'reworkAtomsAt') and commits as
-- it goes; the new continuation itself is left as 'Prose' for the caller to
-- split and append, same as 'Storyteller.Writer.Agent.Write.writeAgent'.
module Storyteller.Writer.Agent.FlowWrite
  ( flowWriteAgent
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.Logging (Logging)

import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Context (ContextStorage)
import Storyteller.Core.ContentEffects (BranchResolve, fileTicksOf, runFileTicks)
import Storyteller.Core.Storage (StoryStorage, ticksSince)
import Storyteller.Writer.Agent (Instruction(..), Prose, PastChaptersMode)
import Storyteller.Writer.Agent.Context (Lore, PinnedContext)
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Git (BranchOp)
import Storyteller.Core.Types (TickId(..))

-- | See module header. Everything besides @path@\/@flowTid@ is the same
--   caller-supplied context 'writeAgent' itself takes -- passed straight
--   through to it once the in-flight span (if any) has been reworked.
--   'writeAgent' fetches its own fresh tick history internally now (see
--   its own Haddock), so this module only needs the ticks itself, locally,
--   to decide *whether* a rework is needed -- not to hand a snapshot
--   through to 'writeAgent', which would risk being the stale pre-rework
--   one.
--
--   The one place in production where 'ProseModel' (the new continuation)
--   and 'AgentModel' (the in-flight revision) genuinely run side by side in
--   a single call -- see 'Storyteller.Core.LLM.Role.LLMs'.
flowWriteAgent
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, ContextStorage, BranchResolve, BranchOp branch, StoryStorage, Fail, Logging] r)
  => FilePath                    -- ^ file being continued
  -> TickId                      -- ^ flowTid: HEAD when the user started typing
  -> Lore                        -- ^ see 'writeAgent's own Haddock
  -> PastChaptersMode
  -> PinnedContext                -- ^ pinned/short-term context
  -> Instruction
  -> Sem r ([TickId], Prose)
flowWriteAgent path flowTid lore chaptersMode pinned instruction = do
  inFlightIdxs <- runFileTicks @branch $ do
    allTicks <- fileTicksOf @branch path
    let inFlightCount = length (ticksSince (Just (unTickId flowTid)) allTicks)
    pure [length allTicks - inFlightCount .. length allTicks - 1]
  reworkedTids <- if null inFlightIdxs
    then return []
    else reworkAtomsAt @branch path (flowInstruction instruction) inFlightIdxs

  generated <- writeAgent @branch path lore chaptersMode pinned instruction
  return (reworkedTids, generated)

-- | The atom under review was generated while this instruction was already
--   queued, so it may not account for it — frame the instruction with that
--   context before handing it to 'reworkAtomsAt'.
flowInstruction :: Instruction -> Instruction
flowInstruction (Instruction instr) = Instruction $
  "This atom was generated while a new instruction was already being typed, \
  \so it may not account for it. Revise it if (and only if) it needs to \
  \change given this new instruction: " <> instr
