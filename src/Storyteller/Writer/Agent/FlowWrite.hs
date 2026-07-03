{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
-- on top of it.
module Storyteller.Writer.Agent.FlowWrite
  ( flowWriteAgent
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharContextBlock(..), CharLabel(..), ContextBlock(..), WordCount(..))
import Storyteller.Writer.Agent.Continuation (continueFileAgent)
import Storyteller.Writer.Agent.ReplaceTool (reworkAtomsAt)
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Append (append)
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Runtime (StoryModel)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, fileTicks, ticksSince)
import Storyteller.Core.Types (TickId(..))

-- | See module header. @charProjects@ is the same @(label, summary action)@
--   shape 'writeAgent' takes.
flowWriteAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Splitter, Logging, Fail ] r )
  => FilePath                                       -- ^ file to append to
  -> TickId                                          -- ^ flowTid: HEAD when the user started typing
  -> Instruction
  -> [ContextBlock]                                  -- ^ extra context (e.g. user's pinned selection)
  -> [(CharLabel, Sem r [CharContextBlock])]         -- ^ (label, summary action) per active char branch
  -> Sem r [TickId]
flowWriteAgent path flowTid instruction extraContext charActions = do
  charContexts <- fmap concat $ mapM (\(CharLabel name, action) -> do
    blocks <- action
    return $ CharContextBlock ("## Character: " <> name) : blocks) charActions

  allTicks <- fileTicks @branch path
  let inFlightCount = length (ticksSince (Just (unTickId flowTid)) allTicks)
      inFlightIdxs   = [length allTicks - inFlightCount .. length allTicks - 1]
  reworkedTids <- if inFlightCount == 0
    then return []
    else reworkAtomsAt @branch @project path (flowInstruction instruction) inFlightIdxs

  Prose generated <- continueFileAgent @project @StoryModel
                       modelConfigs (Just (WordCount 300)) charContexts extraContext path instruction
  contTids <- mapM (append @branch path) =<< splitAtoms generated
  return (reworkedTids <> contTids)

-- | The atom under review was generated while this instruction was already
--   queued, so it may not account for it — frame the instruction with that
--   context before handing it to 'reworkAtomsAt'.
flowInstruction :: Instruction -> Instruction
flowInstruction (Instruction instr) = Instruction $
  "This atom was generated while a new instruction was already being typed, \
  \so it may not account for it. Revise it if (and only if) it needs to \
  \change given this new instruction: " <> instr
