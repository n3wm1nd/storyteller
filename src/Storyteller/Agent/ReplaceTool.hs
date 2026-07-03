{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared per-atom "should this change, and if so how" tool call.
--
-- The tool the model is given is deliberately narrow: it takes the
-- replacement text plus a short reason, and the target atom (its tick id
-- and file path) is bound into the tool by the caller rather than supplied
-- by the model — so the model has no way to ever target the wrong atom.
-- This is what @Storyteller.Agent.Fix@ and @Storyteller.Agent.FlowWrite@
-- both reduce to: "here is one atom, here is an instruction, does it need
-- to change, and if so to what — and why".
--
-- The reason is stored as its own 'Storyteller.Common.Types.Fixup' tick
-- alongside the replacement, agent-authored and distinct from a user's
-- 'Note', so a later reader can trace back why an atom changed.
--
-- Under the hood this is @UniversalLLM@'s tool-calling support (see
-- @TOOLCALLS.md@ in universal-llm): 'replaceAtomTool' is a plain function
-- wrapped with 'mkToolWithMeta', and applying it is 'Storyteller.Core.Edit.editAtom'
-- — the same in-place-replace-preserving-position mechanics the working-tree
-- commit path already uses.
module Storyteller.Agent.ReplaceTool
  ( reworkAtom
  , reworkAtomsAt
  ) where

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import UniversalLLM (Message(..), ModelConfig(..))
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

import Storyteller.Agent (Instruction(..))
import Storyteller.Core.CLI.Env (modelConfigs)
import Storyteller.Core.Edit (editAtom)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Runtime (StoryModel)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, FileTick(..), fileTicks, storeAs)
import Storyteller.Core.Types (TickId(..))
import Storyteller.Common.Types (Fixup(..))

-- | The tool's result, round-tripped through JSON only to travel from the
--   tool call back to us — never inspected by the model itself (tool
--   results aren't sent to it again here; each atom gets one single-turn
--   call).
newtype ReplaceResult = ReplaceResult { rrNewTickId :: T.Text }

instance HasCodec ReplaceResult where
  codec = object "ReplaceResult" $
    ReplaceResult <$> requiredField "new_tick_id" "id of the tick after replacement" .= rrNewTickId

instance ToolParameter ReplaceResult where
  paramName = "replace_result"
  paramDescription = "result of replacing an atom's text"

-- | The model's stated reason for a replacement — the one thing about a
--   change only the model can supply; everything else ('Fixup's ref) is
--   closed over by the caller.
newtype FixDescription = FixDescription T.Text

instance HasCodec FixDescription where
  codec = dimapCodec FixDescription (\(FixDescription t) -> t) codec

instance ToolParameter FixDescription where
  paramName = "reason"
  paramDescription = "brief explanation of why this atom needed to change, kept for later tracing"

-- | The tool itself: the atom's identity is closed over, not a parameter,
--   so the only thing the model supplies is the replacement text and its
--   reason. The reason is committed as a 'Fixup' tick referencing the
--   atom's new id right alongside the replacement.
replaceAtomTool
  :: forall branch project r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => TickId -> FilePath -> T.Text -> FixDescription -> Sem r ReplaceResult
replaceAtomTool tid path newText (FixDescription reason) = do
  (newTid, _mapping) <- editAtom @branch tid path (TE.encodeUtf8 newText)
  _ <- storeAs @branch (Fixup [newTid] reason)
  return (ReplaceResult (unTickId newTid))

-- | Ask the model whether one specific atom needs to change given an
--   instruction, and if so replace it in place. Returns the new tick id if
--   a replacement happened, 'Nothing' if the model judged no change needed
--   (or didn't call the tool at all).
reworkAtom
  :: forall branch project r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => FilePath -> TickId -> T.Text -> Instruction -> Sem r (Maybe TickId)
reworkAtom path tid content (Instruction instr) = do
  let tool = mkToolWithMeta
               "replace_atom"
               "Replace this one atom's text with a corrected version. Only call this if the atom actually needs to change because of the instruction; otherwise don't call it."
               (replaceAtomTool @branch @project tid path)
               "new_text" "The full corrected replacement text for this atom, replacing it entirely"
               "reason"   "Brief explanation of why this atom needed to change, for later tracing"
      tools = [LLMTool tool]
      prompt = "Atom under review:\n\n" <> content
             <> "\n\nInstruction: " <> instr
             <> "\n\nIf this atom needs to change because of the instruction, call replace_atom \
                \with the corrected text and a brief reason why. If it is already fine as-is, just \
                \reply briefly and do not call the tool."

  response <- queryLLM @StoryModel (Tools (map llmToolToDefinition tools) : modelConfigs) [UserText prompt]
  case [tc | AssistantTool tc <- response] of
    (call : _) -> do
      result <- executeToolCallFromList tools call
      case toolResultOutput result of
        Right value -> case parseEither parseJSONViaCodec value of
          Right (ReplaceResult newTid) -> return (Just (TickId newTid))
          Left _                       -> return Nothing
        Left _ -> return Nothing
    [] -> return Nothing

-- | Apply 'reworkAtom' at each of the given (oldest-first) positions in the
--   file's tick chain. Positions, not tick ids: replacing one atom rebases
--   every atom after it onto new ids, but leaves position untouched, so the
--   chain is re-fetched before each attempt rather than trusting ids
--   captured before the loop started.
reworkAtomsAt
  :: forall branch project r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => FilePath -> Instruction -> [Int] -> Sem r [TickId]
reworkAtomsAt path instruction idxs = catMaybes <$> mapM oneAt idxs
  where
    oneAt idx = do
      ticks <- fileTicks @branch path
      case drop idx ticks of
        (FileTick { ftTickId = tid, ftContent = Just content } : _) ->
          reworkAtom @branch @project path (TickId tid) content instruction
        _ -> return Nothing
