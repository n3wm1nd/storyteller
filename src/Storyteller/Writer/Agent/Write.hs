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
  , flattenCharBlocks
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Runix.Logging (Logging)

import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Writer.Agent (Instruction, Prose, CharContextBlock(..), CharLabel(..), ContextBlock, ExistingContent, WordCount(..))
import Storyteller.Writer.Agent.Continuation (proseAgent)
import Storyteller.Core.Prompt (PromptStorage)

-- | Generate prose given already-gathered context.
--
--   Always the 'ProseModel' role -- see 'Storyteller.Core.LLM.Role.LLMs'.
--   This is a thin wrapper adding character-context formatting on top of
--   'Storyteller.Writer.Agent.Continuation.proseAgent'.
--
--   @charBlocks@ is @(label, resolved summary blocks)@ per active character
--   branch — already read, not a deferred action: the caller has already
--   opened each character branch's filesystem and run
--   'Storyteller.Writer.Agent.CharContext.charSummaryAgent' (or an
--   equivalent) by the time this is called.
writeAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => ExistingContent
  -> [ContextBlock]                              -- ^ extra context (e.g. user's pinned selection)
  -> Instruction
  -> [(CharLabel, [CharContextBlock])]            -- ^ (label, resolved blocks) per active char branch
  -> Sem r Prose
writeAgent existing extraContext instruction charBlocks =
  proseAgent (Just (WordCount 300)) (flattenCharBlocks charBlocks) extraContext existing instruction

-- | @(label, resolved blocks)@ per active character branch, flattened into
--   the plain 'CharContextBlock' list a 'proseAgent'-shaped call actually
--   takes -- each branch's blocks preceded by a @"## Character: {name}"@
--   header block. Shared with 'Storyteller.Writer.Agent.Outline''s
--   reconciliation calls, which take the same flattened shape directly.
flattenCharBlocks :: [(CharLabel, [CharContextBlock])] -> [CharContextBlock]
flattenCharBlocks = concatMap
  (\(CharLabel name, blocks) -> CharContextBlock ("## Character: " <> name) : blocks)
