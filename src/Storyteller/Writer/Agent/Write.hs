{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Write agent: merge character context into 'proseAgent' input.
--
-- This is the pure core of "write the next bit of prose" — only 'LLM'/'Fail',
-- no filesystem, no branch, no splitter. Reading the target file's existing
-- content (@Storyteller.Writer.Agent.Continuation.gatherFileContext@),
-- summarising character branches (@Storyteller.Writer.Agent.CharContext@),
-- and appending the result (@Storyteller.Core.Append.append@) are all the
-- caller's job — this module only does the LLM-facing computation.
module Storyteller.Writer.Agent.Write
  ( writeAgent
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (LLM)

import Storyteller.Writer.Agent (Instruction, Prose, CharContextBlock(..), CharLabel(..), ContextBlock, ExistingContent, WordCount(..))
import Storyteller.Writer.Agent.Continuation (proseAgent)
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Runtime (StoryModel)

-- | Generate prose given already-gathered context.
--
--   @charBlocks@ is @(label, resolved summary blocks)@ per active character
--   branch — already read, not a deferred action: the caller has already
--   opened each character branch's filesystem and run
--   'Storyteller.Writer.Agent.CharContext.charSummaryAgent' (or an
--   equivalent) by the time this is called.
writeAgent
  :: forall r
  .  Members '[LLM StoryModel, Fail] r
  => ExistingContent
  -> [ContextBlock]                              -- ^ extra context (e.g. user's pinned selection)
  -> Instruction
  -> [(CharLabel, [CharContextBlock])]            -- ^ (label, resolved blocks) per active char branch
  -> Sem r Prose
writeAgent existing extraContext instruction charBlocks = do
  let charContexts = concatMap
        (\(CharLabel name, blocks) -> CharContextBlock ("## Character: " <> name) : blocks)
        charBlocks
  proseAgent @StoryModel modelConfigs (Just (WordCount 300)) charContexts extraContext existing instruction
