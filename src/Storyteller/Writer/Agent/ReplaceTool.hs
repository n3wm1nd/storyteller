{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared per-atom "should this change, and if so how" tool call.
--
-- The model gets two tools, not one: @replace_atom@ (retype the whole
-- atom) and @replace_text@ (an exact-match-once span swap, the same
-- find-and-replace-exactly-one-occurrence safety rule
-- @Runix.Tools.editFile@ already applies to a whole file, just against one
-- atom's in-memory text instead of a filesystem path). A small, localized
-- fix — one word, one clause — only needs @replace_text@: the model names
-- the exact span and its replacement instead of retyping the entire atom
-- around it, which is both cheaper and removes the chance of an
-- unrelated, un-asked-for rewrite creeping in elsewhere in the atom the way
-- a full retype always risks. @replace_atom@ stays available for a change
-- that's genuinely broader than one contiguous span. Neither tool decides
-- *whether* to apply anything itself, so neither needs a filesystem or
-- storage effect at all, only enough to construct a value (or, for
-- @replace_text@, run the substitution against the atom text closed over
-- from 'reworkAtom's own argument) — this is what
-- @Storyteller.Writer.Agent.Fix@ and @Storyteller.Writer.Agent.FlowWrite@
-- both reduce to: "here is one atom, here is an instruction, does it need
-- to change, and if so to what — and why".
--
-- 'reworkAtom' is the pure decision core: given one atom's text and an
-- instruction, it asks the model and returns the proposed replacement (or
-- 'Nothing' if no change is warranted) — needing only 'LLM'/'Fail'. A
-- @replace_text@ call whose @old_text@ doesn't match the atom exactly once
-- (missing, or ambiguous) comes back as a proposal identical to the
-- original content; 'reworkAtom' treats that the same as "no proposal" —
-- see its own Haddock — rather than writing a no-op edit and a Fixup tick
-- that explains nothing. This is a single-turn decision, not a retry loop
-- (unlike 'Storyteller.Writer.Agent.Outline.splitOutlineAgent'\/'Agent.
-- Integration.Judge.judge'): a model that gets @old_text@ wrong doesn't get
-- a second attempt in the same call, it just falls back to leaving the
-- atom untouched, the same as if it hadn't called anything.
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
--
-- 'reworkWholeFile' is the free-roam twin, for when the caller has no
-- target atoms at all: rather than a per-atom decision loop, it hands the
-- model 'Runix.Tools.editFile' directly against the real working-tree
-- file and lets it call that (an agentic loop, same shape as
-- @Storyteller.Writer.Agent.Chat.chatAgent@) as many times as it finds
-- warranted, reconciling the result back into the chain with one
-- 'Storage.Ops.commitFiles' once it's done.
module Storyteller.Writer.Agent.ReplaceTool
  ( ReplaceProposal(..)
  , reworkAtom
  , reworkAtomsAt
  , reworkWholeFile
  , replaceOnce
  ) where

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystemRead, FileSystemWrite, readFile)
import Runix.LLM (queryLLM)
import Runix.LLM.ToolInstances ()
import Runix.Logging (Logging, info, warning)
import qualified Runix.Tools as Tools
import UniversalLLM (Message(..), ModelConfig(..), getToolCallName)
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Writer.Agent (Instruction(..))
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfigWithPrompt)
import Storyteller.Core.Git (BranchOp, BranchTag, runStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storage.Tick (FileTick(..))
import Storyteller.Core.Types (TickId(..))
import Storyteller.Common.Types (Fixup(..))

import Prelude hiding (readFile)

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

-- | The @replace_text@ tool: given the atom's own current content (closed
--   over, not a model-supplied parameter -- the model names a span, it
--   doesn't retype the whole atom just to hand it back unchanged), run
--   'replaceOnce' and package whichever text comes out. A failed match
--   (see 'replaceOnce') comes back as @content@ unchanged, tagged with
--   'matchFailedMarker' prepended to the reason -- distinct from the text
--   just happening to come back unchanged for some other cause, so
--   'reworkAtom' can log "model tried and the span didn't match" instead
--   of silently treating a failed match identically to "model looked and
--   declined to change anything."
proposeTextReplacement :: forall r. T.Text -> T.Text -> T.Text -> FixDescription -> Sem r ReplaceProposal
proposeTextReplacement content oldText newText (FixDescription reason) =
  case replaceOnce oldText newText content of
    Nothing       -> pure $ ReplaceProposal content (matchFailedMarker <> reason)
    Just replaced -> pure $ ReplaceProposal replaced reason

-- | Prefix 'proposeTextReplacement' tags a failed-match reason with --
--   never model-supplied, so its presence unambiguously means "this
--   specific call's own @old_text@ didn't match," not "the model happened
--   to phrase its decline this way."
matchFailedMarker :: T.Text
matchFailedMarker = "\0old_text-no-match\0"

-- | Replace the one occurrence of @old@ in @haystack@ with @new@ -- 'Nothing'
--   if @old@ is empty, absent, or ambiguous (appears more than once).
--   Mirrors @Runix.Tools.editFile@\/@replaceAndCount@'s own
--   exactly-once-or-refuse rule, just against an atom's in-memory text
--   rather than a whole file on disk: an unambiguous single match is
--   required precisely so the model can't accidentally touch a second,
--   unintended occurrence of the same phrase elsewhere in the atom.
replaceOnce :: T.Text -> T.Text -> T.Text -> Maybe T.Text
replaceOnce old new haystack
  | T.null old            = Nothing
  | T.count old haystack /= 1 = Nothing
  | otherwise              = Just (T.replace old new haystack)

-- | Ask the model whether one atom needs to change given an instruction.
--   The pure decision core: only 'LLM'/'Fail', no filesystem or storage
--   access — applying a proposal is 'reworkAtomsAt's job.
--
--   @wholeFile@ is the file's full current text, given purely as
--   read-only reference — surrounding paragraphs the model needs to judge
--   whether the target atom's own wording still fits (a name, a detail, a
--   tense established elsewhere), the same reason 'writeAgent' hands a
--   generation call the chapter-so-far rather than just the next
--   instruction. It never becomes an editable target itself: both tools
--   stay closed over @content@ (this atom's own text) exactly as before,
--   so a model proposing a span from elsewhere in @wholeFile@ simply fails
--   'replaceOnce's exactly-once-in-@content@ match and is treated as no
--   proposal, same as any other unmatched span. This is what keeps
--   "more context" from becoming "laxer about what it's here to fix."
--
--   Always the 'AgentModel' role -- see 'Storyteller.Core.LLM.Role.LLMs'.
reworkAtom
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => T.Text -> T.Text -> Instruction -> Sem r (Maybe ReplaceProposal)
reworkAtom wholeFile content (Instruction instr) = do
  configsWithPrompt <- getConfigWithPrompt "agent.fixer" defaultFixerSystemPrompt defaultFixerConfig
  Prompt guidance   <- getPrompt "agent.fixer.instructions" defaultFixerInstructions

  let replaceAtomTool = mkToolWithMeta
               "replace_atom"
               "Replace this one atom's text with a corrected version, retyping the whole atom. Only call this if the atom actually needs to change because of the instruction, and prefer replace_text instead for a small, localized fix; use this only for a change broader than one contiguous span."
               (proposeReplacement @r)
               "new_text" "The full corrected replacement text for this atom, replacing it entirely"
               "reason"   "Brief explanation of why this atom needed to change, for later tracing"
      replaceTextTool = mkToolWithMeta
               "replace_text"
               "Replace one exact, contiguous span of this atom's text with a corrected version, leaving the rest of the atom untouched. Prefer this over replace_atom for a small, localized fix (e.g. correcting one detail). old_text must match the atom's text exactly once -- if it's missing or ambiguous, the atom is left unchanged, so make old_text specific enough to be unambiguous."
               (proposeTextReplacement @r content)
               "old_text" "The exact text to find in the atom -- must match exactly once"
               "new_text" "The text to replace it with"
               "reason"   "Brief explanation of why this atom needed to change, for later tracing"
      tools = [LLMTool replaceAtomTool, LLMTool replaceTextTool]
      prompt = "Full file, for context only -- you may only change the atom under review below, nothing else in this file:\n\n"
             <> wholeFile
             <> "\n\nAtom under review:\n\n" <> content <> "\n\nInstruction: " <> instr <> "\n\n" <> guidance

  response <- queryLLM
    (Tools (map llmToolToDefinition tools) : configsWithPrompt)
    [UserText prompt]
  case [tc | AssistantTool tc <- response] of
    (call : _) -> do
      result <- executeToolCallFromList tools call
      case toolResultOutput result of
        Right value -> case parseEither parseJSONViaCodec value of
          -- A 'replace_text' call whose 'old_text' didn't uniquely match
          -- comes back as @content@ unchanged, its reason tagged with
          -- 'matchFailedMarker' (see 'proposeTextReplacement') -- logged
          -- loudly rather than silently treated the same as "the model
          -- looked and declined": the model's own final reply still tends
          -- to describe what it *meant* to change, which otherwise reads
          -- to a caller as a successful fix that just didn't happen.
          Right proposal
            | matchFailedMarker `T.isPrefixOf` rpReason proposal -> do
                warning $ "fixAgent: replace_text: old_text didn't match this atom exactly once, left unchanged ("
                        <> T.drop (T.length matchFailedMarker) (rpReason proposal) <> ")"
                return Nothing
            | rpNewText proposal == content -> return Nothing
            | otherwise                     -> return (Just proposal)
          Left _ -> return Nothing
        Left _ -> return Nothing
    [] -> return Nothing

-- | Fallback for @agent.fixer@ (the namespace root is implicitly the system
--   prompt/config -- see 'Storyteller.Core.Prompt'), used until an override is committed
--   to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultFixerSystemPrompt :: Prompt
defaultFixerSystemPrompt = "You are a careful copy editor."

-- | Fallback for @agent.fixer.instructions@ -- the one free-text part of this
--   agent's user message a prompt override can actually change. Everything
--   else (the "Atom under review"/"Instruction" framing and where @content@/
--   @instr@ land in it) is fixed Haskell structure, not a slotted template --
--   see 'Storyteller.Core.Prompt' on why user-facing overrides never expose
--   template slots: there'd be nothing telling an editor which slot names
--   are even valid.
defaultFixerInstructions :: Prompt
defaultFixerInstructions =
  "If this atom needs to change because of the instruction, call replace_text with the \
  \exact old text and its replacement for a small, localized fix, or replace_atom with the \
  \full corrected text for a change broader than one contiguous span -- either way, include \
  \a brief reason why. If it is already fine as-is, just reply briefly and do not call \
  \either tool."

-- | Compiled-in sampling default for @agent.fixer@ -- see @$key.llmsettings.
--   yaml@ overrides via 'Storyteller.Core.Prompt.getConfig'. Deciding
--   whether one atom needs to change (and, if so, replacing it with a
--   corrected version via a tool call) is a small, precise edit, not open
--   creative writing -- hence a below-'defaultWriterConfig' temperature
--   (favor a reliable, literal correction over a creatively-varied rewrite,
--   without going so low the replacement text itself gets stilted) and a
--   modest token budget (one atom's worth of text, not a whole chapter).
defaultFixerConfig :: [ModelConfig AgentModel]
defaultFixerConfig = [MaxTokens 1024, Temperature 0.5]

-- | Apply 'reworkAtom' at each of the given (oldest-first) positions in the
--   file's tick chain, committing every proposed replacement as it's made.
--   Positions, not tick ids: replacing one atom rebases every atom after it
--   onto new ids, but leaves position untouched, so the chain is re-fetched
--   before each attempt rather than trusting ids captured before the loop
--   started — this is the one place ids and content genuinely can't be
--   gathered upfront and handed to a pure core.
--
--   Logged per atom, the same reasoning as 'Storyteller.Writer.Agent.Outline.splitOutlineAgent':
--   each target is its own separate 'queryLLM' call with a gap of ordinary
--   git work in between, so a fix touching several atoms would otherwise
--   look identical to a hang between one atom's streamed tokens ending and
--   the next atom's starting -- the "N of M" progress line is what tells
--   the two apart for a user actually watching it run.
--
--   The whole file is re-read fresh before every attempt, same as
--   @ticks@ -- an earlier target's own edit in this same loop changes
--   what "the rest of the file" currently says, so a later target's
--   reference context has to reflect that rather than whatever the file
--   looked like when the loop started.
reworkAtomsAt
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, BranchOp branch, FileSystemRead (BranchTag branch), Fail, Logging] r)
  => FilePath -> Instruction -> [Int] -> Sem r [TickId]
reworkAtomsAt path instruction idxs = do
  info $ "fixAgent: reviewing " <> T.pack (show total) <> " atom(s) in " <> T.pack path
  changed <- catMaybes <$> mapM oneAt (zip [1 :: Int ..] idxs)
  info $ "fixAgent: done, " <> T.pack (show (length changed)) <> " of " <> T.pack (show total) <> " atom(s) changed"
  return changed
  where
    total = length idxs
    oneAt (n, idx) = do
      ticks <- runStorage @branch (Tick.fileTicksOf path)
      wholeFile <- TE.decodeUtf8 <$> readFile @(BranchTag branch) path
      case drop idx ticks of
        (FileTick { ftTickId = tid, ftContent = Just content } : _) -> do
          info $ "fixAgent: atom " <> T.pack (show n) <> "/" <> T.pack (show total) <> ": querying model..."
          mProposal <- reworkAtom wholeFile content instruction
          case mProposal of
            Nothing -> do
              info $ "fixAgent: atom " <> T.pack (show n) <> "/" <> T.pack (show total) <> ": left unchanged"
              return Nothing
            Just (ReplaceProposal newText reason) -> do
              info $ "fixAgent: atom " <> T.pack (show n) <> "/" <> T.pack (show total) <> ": " <> reason
              newTid <- runStorage @branch (do
                newHash <- Ops.editAtomAt (Ops.ObjectHash tid) newText
                let tid' = TickId (Ops.unObjectHash newHash)
                _ <- Tick.storeAs (Fixup [tid'] reason)
                -- 'editAtomAt' only ever moves the chain (head) -- it
                -- never touches the ambient tree ('Ops.addAtom' is the
                -- one that does that itself, via its own explicit
                -- 'Ops.appendFile'). Without this, the *next* target's
                -- own 'wholeFile' read above (and this file as seen by
                -- anything reading through 'FileSystemRead' afterward)
                -- would still show the pre-edit text, even though the
                -- edit landed for real on the chain -- exactly the
                -- "nothing visibly changed" symptom a caller watching the
                -- file, not the tick chain, would see.
                Core.reset
                return tid')
              return (Just newTid)
        _ -> return Nothing

-- | Free-roam pass: no caller-selected targets, the model reviews the
--   whole file against @instruction@ and edits it directly with
--   'Runix.Tools.editFile' -- the same exact-match-once, read-modify-write
--   tool @chatAgent@ already exposes for read access, here given write
--   access instead since this agent's whole job is editing. Reused as-is
--   rather than a bespoke tool: an exact-span, must-match-once replacement
--   against a real file is exactly what this needed, no atom-resolution
--   step required at all -- each call lands straight on the working tree,
--   and one 'Ops.commitFiles' once the model is done reconciles whichever
--   atoms actually ended up different (unchanged atoms keep their ids),
--   same as 'Server.Writer.File.chatChapterRegen'. An agentic tool loop,
--   same shape as 'Storyteller.Writer.Agent.Chat.chatAgent' -- stops the
--   moment a turn comes back with no tool calls, which is also how a
--   model that finds nothing worth fixing signals "done" without ever
--   touching the file.
--
--   Deliberately not a variant of 'reworkAtomsAt': that one decides
--   per-atom, in isolation, whether *that* atom needs to change --
--   exactly the right shape when the caller already knows which atoms are
--   in scope. Here nothing is in scope yet; scoping is the model's own
--   job, and doing it by exact-span replacement (rather than by naming
--   atom indices, which the model has no stable way to refer to) keeps
--   the same "must match exactly once" safety rail that already governs
--   every localized fix, just widened from one atom's text to the whole
--   file's.
--
--   The pass records one 'Fixup' for the instruction as a whole, not one
--   per edit: unlike a targeted fix (always exactly one atom, one clear
--   ref), a free-roam pass may land on several atoms whose post-commit
--   ids aren't known until 'Ops.commitFiles' has already folded them back
--   into the chain, so there's no single atom this note is "about" --
--   refs empty, same as any note not anchored to a specific tick. Only
--   recorded if the file actually ended up different, so a pass that
--   found nothing worth changing leaves no trace.
reworkWholeFile
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, BranchOp branch, FileSystemRead (BranchTag branch), FileSystemWrite (BranchTag branch), Fail, Logging] r)
  => FilePath -> Instruction -> Sem r ()
reworkWholeFile path (Instruction instr) = do
  before <- readFile @(BranchTag branch) path
  go (1 :: Int)
  after <- readFile @(BranchTag branch) path
  if after == before
    then info "fixAgent: whole-file pass: nothing changed"
    else do
      _ <- runStorage @branch (Ops.commitFiles [path])
      _ <- runStorage @branch (Tick.storeAs (Fixup [] instr))
      info "fixAgent: whole-file pass: done"
  where
    go turnNo = do
      Prompt guidance   <- getPrompt "agent.fixer.instructions.wholefile" defaultFixerWholeFileInstructions
      configsWithPrompt <- getConfigWithPrompt "agent.fixer" defaultFixerSystemPrompt defaultFixerConfig
      wholeFile <- TE.decodeUtf8 <$> readFile @(BranchTag branch) path

      let tools = [LLMTool (Tools.editFile @(BranchTag branch))]
          prompt = "Full file:\n\n" <> wholeFile <> "\n\nInstruction: " <> instr <> "\n\n" <> guidance

      info $ "fixAgent: whole-file pass, turn " <> T.pack (show turnNo) <> ": querying model..."
      response <- queryLLM
        (Tools (map llmToolToDefinition tools) : configsWithPrompt)
        [UserText prompt]
      case [tc | AssistantTool tc <- response] of
        [] -> return ()
        calls -> do
          mapM_ (\tc -> info ("fixAgent: whole-file pass, turn " <> T.pack (show turnNo) <> ": calling " <> getToolCallName tc)) calls
          _ <- mapM (executeToolCallFromList tools) calls
          go (turnNo + 1)

-- | Fallback for @agent.fixer.instructions.wholefile@ -- the free-roam
--   twin of 'defaultFixerInstructions', framing the edit tool as
--   repeatable (find everything worth fixing, not just one span) since
--   there's no caller-selected target narrowing scope for this pass.
defaultFixerWholeFileInstructions :: Prompt
defaultFixerWholeFileInstructions =
  "Review the whole file against the instruction. Edit it directly, as many times as \
  \warranted, once per distinct fix -- each edit's old_string must match the file's \
  \current text exactly once, so make it specific enough to be unambiguous. Only change \
  \what the instruction is actually about; leave everything else exactly as it is. Once \
  \nothing more needs fixing, stop editing and reply briefly."
