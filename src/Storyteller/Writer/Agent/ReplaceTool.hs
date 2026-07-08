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
-- replacement text plus a short reason and hands both straight back as data
-- — it does not itself decide *whether* to apply anything, so it needs no
-- filesystem or storage effect at all, only enough to construct a value.
-- This is what @Storyteller.Writer.Agent.Fix@ and @Storyteller.Writer.Agent.FlowWrite@
-- both reduce to: "here is one atom, here is an instruction, does it need
-- to change, and if so to what — and why".
--
-- 'reworkAtom' is the pure decision core: given one atom's text and an
-- instruction, it asks the model and returns the proposed replacement (or
-- 'Nothing' if no change is warranted) — needing only 'LLM'/'Fail'.
--
-- 'reworkAtomsAt' is the machinery around that core: it walks the file's
-- tick chain, and for every atom 'reworkAtom' proposes a change for, applies
-- it via 'Storyteller.Core.Edit.editAtom' (the same in-place-replace-
-- preserving-position mechanics the working-tree commit path already uses)
-- and records the model's reason as its own 'Storyteller.Common.Types.Fixup'
-- tick, agent-authored and distinct from a user's 'Note', so a later reader
-- can trace back why an atom changed. This step can't be hoisted to the
-- caller the way a plain append can: positions stay put but tick ids shift
-- with every replacement, so the chain has to be re-read before each
-- application to target the atom currently at that position.
--
-- Under the hood this is @UniversalLLM@'s tool-calling support (see
-- @TOOLCALLS.md@ in universal-llm): the tool given to the model is a plain
-- function wrapped with 'mkToolWithMeta'.
module Storyteller.Writer.Agent.ReplaceTool
  ( ReplaceProposal(..)
  , reworkAtom
  , reworkAtomsAt
  ) where

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import UniversalLLM (Message(..), ModelConfig(..), HasTools, ProviderOf, SupportsSystemPrompt)
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

import Storyteller.Writer.Agent (Instruction(..))
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, applyTemplate)
import Storyteller.Core.Git (BranchOp, runStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storage.Tick (FileTick(..))
import Storyteller.Core.Types (TickId(..))
import Storyteller.Common.Types (Fixup(..))

-- | The model's proposed replacement: the new text plus its reason. Plain
--   data — whether and how it gets applied is entirely up to the caller.
data ReplaceProposal = ReplaceProposal
  { rpNewText :: T.Text
  , rpReason  :: T.Text
  } deriving (Show, Eq)

instance HasCodec ReplaceProposal where
  codec = object "ReplaceProposal" $
    ReplaceProposal
      <$> requiredField "new_text" "the full corrected replacement text for this atom" .= rpNewText
      <*> requiredField "reason"   "brief explanation of why this atom needed to change"  .= rpReason

instance ToolParameter ReplaceProposal where
  paramName = "replace_proposal"
  paramDescription = "the proposed replacement text and reason for an atom"

-- | The model's stated reason for a replacement — the one thing about a
--   change only the model can supply; everything else ('Fixup's ref) is
--   closed over by the caller.
newtype FixDescription = FixDescription T.Text

instance HasCodec FixDescription where
  codec = dimapCodec FixDescription (\(FixDescription t) -> t) codec

instance ToolParameter FixDescription where
  paramName = "reason"
  paramDescription = "brief explanation of why this atom needed to change, kept for later tracing"

-- | The tool itself: just packages the model's two arguments into a
--   'ReplaceProposal'. No filesystem/storage effect — the tool doesn't
--   decide whether or how anything gets applied, it only reports what the
--   model proposed.
proposeReplacement :: forall r. T.Text -> FixDescription -> Sem r ReplaceProposal
proposeReplacement newText (FixDescription reason) = pure (ReplaceProposal newText reason)

-- | Ask the model whether one atom needs to change given an instruction.
--   The pure decision core: only 'LLM'/'Fail', no filesystem or storage
--   access — applying a proposal is 'reworkAtomsAt's job.
--
--   Generic over @fixerModel@ -- the fixer role is independent of whatever
--   plays the prose-generation role (see
--   'Storyteller.Writer.Agent.Write.writeAgent'); a caller like
--   'Storyteller.Writer.Agent.FlowWrite.flowWriteAgent' can genuinely use
--   different models for the two. The server call site instantiates
--   @fixerModel@ at 'Storyteller.Core.LLM.Role.FixerModel' -- a choice made
--   at the call site, not baked in here.
reworkAtom
  :: forall fixerModel r
  .  ( HasTools fixerModel
     , SupportsSystemPrompt (ProviderOf fixerModel)
     , Members '[LLM fixerModel, PromptStorage, Fail] r )
  => [ModelConfig fixerModel] -> T.Text -> Instruction -> Sem r (Maybe ReplaceProposal)
reworkAtom configs content (Instruction instr) = do
  Prompt systemPrompt <- getPrompt "agent.fixer.system" defaultFixerSystemPrompt
  Prompt template     <- getPrompt "agent.fixer.template" defaultFixerTemplate

  let tool = mkToolWithMeta
               "replace_atom"
               "Replace this one atom's text with a corrected version. Only call this if the atom actually needs to change because of the instruction; otherwise don't call it."
               (proposeReplacement @r)
               "new_text" "The full corrected replacement text for this atom, replacing it entirely"
               "reason"   "Brief explanation of why this atom needed to change, for later tracing"
      tools = [LLMTool tool]
      Prompt prompt = applyTemplate (Prompt template)
        [ ("content", Prompt content), ("instruction", Prompt instr) ]

  response <- queryLLM @fixerModel
    (SystemPrompt systemPrompt : Tools (map llmToolToDefinition tools) : configs)
    [UserText prompt]
  case [tc | AssistantTool tc <- response] of
    (call : _) -> do
      result <- executeToolCallFromList tools call
      case toolResultOutput result of
        Right value -> case parseEither parseJSONViaCodec value of
          Right proposal -> return (Just proposal)
          Left _         -> return Nothing
        Left _ -> return Nothing
    [] -> return Nothing

-- | Fallback for @agent.fixer.system@, used until an override is committed
--   to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultFixerSystemPrompt :: Prompt
defaultFixerSystemPrompt = "You are a careful copy editor."

-- | Fallback for @agent.fixer.template@. Slots: {{content}}, {{instruction}}.
defaultFixerTemplate :: Prompt
defaultFixerTemplate =
  "Atom under review:\n\n{{content}}\n\n\
  \Instruction: {{instruction}}\n\n\
  \If this atom needs to change because of the instruction, call replace_atom \
  \with the corrected text and a brief reason why. If it is already fine as-is, just \
  \reply briefly and do not call the tool."

-- | Apply 'reworkAtom' at each of the given (oldest-first) positions in the
--   file's tick chain, committing every proposed replacement as it's made.
--   Positions, not tick ids: replacing one atom rebases every atom after it
--   onto new ids, but leaves position untouched, so the chain is re-fetched
--   before each attempt rather than trusting ids captured before the loop
--   started — this is the one place ids and content genuinely can't be
--   gathered upfront and handed to a pure core.
reworkAtomsAt
  :: forall fixerModel branch r
  .  ( HasTools fixerModel
     , SupportsSystemPrompt (ProviderOf fixerModel)
     , Members '[LLM fixerModel, PromptStorage, BranchOp branch, Fail] r )
  => [ModelConfig fixerModel] -> FilePath -> Instruction -> [Int] -> Sem r [TickId]
reworkAtomsAt configs path instruction idxs = catMaybes <$> mapM oneAt idxs
  where
    oneAt idx = do
      (ticks, _) <- runStorage @branch (Tick.fileTicksOf path)
      case drop idx ticks of
        (FileTick { ftTickId = tid, ftContent = Just content } : _) -> do
          mProposal <- reworkAtom @fixerModel configs content instruction
          case mProposal of
            Nothing -> return Nothing
            Just (ReplaceProposal newText reason) -> do
              (newHash, _) <- runStorage @branch (Ops.editAtomAt (Core.ObjectHash tid) newText)
              let newTid = TickId (Core.unObjectHash newHash)
              _ <- runStorage @branch (Tick.storeAs (Fixup [newTid] reason))
              return (Just newTid)
        _ -> return Nothing
