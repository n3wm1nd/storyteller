{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Outline-driven generation.
--
-- The outline workflow is a progressive-refinement pipeline: a coarse
-- document is expanded one level finer, using everything above it as
-- context. @outline.md@ → @ch{N}.outline.md@ (a beat sheet) → @chapters/ch{N}.md@
-- (prose). Every arrow is the same operation — 'expandAgent' — differing only
-- in what it is told to produce. See WRITER.md for the file conventions.
--
-- Everything here is a **pure LLM core** in the same mould as
-- 'Storyteller.Writer.Agent.Continuation.proseAgent' and
-- 'Storyteller.Writer.Agent.Write.writeAgent': context comes in explicit,
-- text comes out. Reading the outline/beat-sheet files, splitting the result
-- into atoms, and appending are all the caller's job (see @app/Outline.hs@
-- and @app/Chapter.hs@) — no filesystem or storage effect appears here, so
-- these compose the same way the rest of the agent folder does.
--
-- The beat sheet is free-form markdown by design (WRITER.md): nothing here
-- parses it. That is why the two prose drivers differ only in whether the
-- chunking judgement lives inside one prompt ('chapterProse') or in an
-- LLM-driven loop that advances beat by beat ('chapterProseByBeat') — neither
-- depends on a delimiter we could grep for.
module Storyteller.Writer.Agent.Outline
  ( OutlineDoc(..)
  , BeatSheet(..)
  , ExpandGoal(..)
  , CurrentProse(..)
  , ChapterBeats(..)
  , expandAgent
  , splitOutlineAgent
  , chapterProse
  , chapterProseByBeat
  , reconcileChapter
  , reconcileChapterByBeat
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import Data.Aeson.Types (parseEither)
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.LLM.ToolInstances ()
import UniversalLLM (Message(..), ModelConfig(..))
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

import Storyteller.Core.LLM.Role (LLMs, ProseModel, AgentModel)
import Storyteller.Writer.Agent
  ( Instruction(..), Prose(..), CharContextBlock, ContextBlock(..)
  , ExistingContent(..), WordCount(..) )
import Storyteller.Writer.Agent.Continuation (proseAgent)
import Storyteller.Core.Prompt (Prompt(..), PromptKey, PromptStorage, getPrompt, getConfigWithPrompt, applyTemplate)

-- | A coarse planning document — the source of an expansion. Usually the
--   contents of @outline.md@ (whole story) or the slice of it covering one
--   chapter. Free-form markdown; see WRITER.md.
newtype OutlineDoc = OutlineDoc Text
  deriving (Show, Eq)

-- | A beat sheet — the expansion of one chapter's slice of the outline into
--   beats. The contents of @ch{N}.outline.md@. Free-form markdown; consumed
--   by the prose drivers below, never parsed by them.
newtype BeatSheet = BeatSheet Text
  deriving (Show, Eq)

-- | The chapter's current prose, handed to a reconciliation run as reference
--   material — what the regenerated chapter should preserve where it already
--   works and rewrite where it contradicts the beat sheet. Distinct from
--   'ExistingContent' (prose to /continue/): here the prose is the thing
--   being /revised/, not extended.
newtype CurrentProse = CurrentProse Text
  deriving (Show, Eq)

-- | What an 'expandAgent' run is producing. Selects the prompt pair so the
--   same expansion machinery can generate a per-chapter beat sheet from a
--   story outline, or (later) any other one-level refinement, without a
--   bespoke agent per level.
data ExpandGoal
  = ToBeatSheet          -- ^ story outline slice → chapter beat sheet
  deriving (Show, Eq)

-- | Expand a coarser document one level finer.
--
--   Pure LLM core: the outline text and any surrounding context are passed
--   in; the finer document comes back as plain 'Text'. Prompts are
--   overridable via 'PromptStorage' with a working default per goal, exactly
--   like 'proseAgent'.
expandAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> ExpandGoal
  -> [ContextBlock]        -- ^ surrounding context (other chapters' outlines, world files, ...)
  -> OutlineDoc            -- ^ the document being expanded
  -> Sem r Text
expandAgent configs goal contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt (systemKey goal) (defaultExpandSystem goal) configs
  Prompt template   <- getPrompt (templateKey goal) defaultExpandTemplate

  let contextSection
        | null contextBlocks = ""
        | otherwise =
            "Surrounding context:\n\n"
            <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
            <> "\n\n"

      Prompt userMsg = applyTemplate (Prompt template)
        [ ("context", Prompt contextSection)
        , ("source",  Prompt doc)
        ]

  response <- queryLLM configsWithPrompt [UserText userMsg]
  return $ mconcat [ t | AssistantText t <- response ]

-- | One chapter's beat sheet plus the file it should be written to. Produced
--   by 'splitOutlineAgent' when the model divides a whole-story outline into
--   chapters — the model chooses both the breakdown and each chapter's path
--   (by convention @chapters/ch{N}.outline.md@), since the chapter files may
--   not exist yet.
data ChapterBeats = ChapterBeats
  { cbPath  :: FilePath   -- ^ where to write this beat sheet
  , cbSheet :: BeatSheet  -- ^ the beat sheet itself
  } deriving (Show, Eq)

-- | Tool-call payload: the model's two arguments for one chapter, before we
--   wrap them as a 'ChapterBeats'. Kept as a distinct type so the tool's
--   result codec is unambiguous.
newtype BeatSheetPath = BeatSheetPath Text
instance HasCodec BeatSheetPath where
  codec = dimapCodec BeatSheetPath (\(BeatSheetPath t) -> t) codec
instance ToolParameter BeatSheetPath where
  paramName = "path"
  paramDescription = "path to write this chapter's beat sheet to, e.g. chapters/ch1.outline.md"

newtype BeatSheetBody = BeatSheetBody Text
instance HasCodec BeatSheetBody where
  codec = dimapCodec BeatSheetBody (\(BeatSheetBody t) -> t) codec
instance ToolParameter BeatSheetBody where
  paramName = "beat_sheet"
  paramDescription = "the full beat sheet for this chapter, as free-form markdown"

-- | Split a whole-story outline into per-chapter beat sheets.
--
--   Unlike 'expandAgent' (one document in, one out), the model here decides
--   the chapter breakdown itself and emits one beat sheet per chapter via
--   repeated @emit_beat_sheet@ tool calls — each call names the target file
--   and carries that chapter's beats. This is the "chapters may not exist
--   yet" case: the outline is the only input, and the model's judgement
--   determines how many chapters there are and what each contains.
--
--   Returns every emitted @(path, beat sheet)@; the caller writes them (see
--   'Server.Writer.File.chatSplitOutline'). A malformed call is dropped
--   rather than failing the whole batch.
splitOutlineAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig AgentModel]
  -> [ContextBlock]        -- ^ surrounding context (world files, notes, ...)
  -> OutlineDoc            -- ^ the whole-story outline being split
  -> Sem r [ChapterBeats]
splitOutlineAgent configs contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt "agent.outline.split.system" defaultSplitSystem configs
  Prompt template   <- getPrompt "agent.outline.split.template" defaultSplitTemplate

  let tool = mkToolWithMeta
               "emit_beat_sheet"
               "Emit the beat sheet for one chapter. Call once per chapter, in reading order."
               (emitBeatSheet @r)
               "path"       "Path to write this chapter's beat sheet to, e.g. chapters/ch1.outline.md"
               "beat_sheet" "The full beat sheet for this chapter, as free-form markdown"
      tools = [LLMTool tool]

      contextSection
        | null contextBlocks = ""
        | otherwise =
            "Surrounding context:\n\n"
            <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
            <> "\n\n"

      Prompt userMsg = applyTemplate (Prompt template)
        [ ("context", Prompt contextSection)
        , ("source",  Prompt doc)
        ]

  let allConfigs = Tools (map llmToolToDefinition tools) : configsWithPrompt
  loop tools allConfigs maxTurns [UserText userMsg]
  where
    -- The model emits its emit_beat_sheet calls across several turns — one (or
    -- a few) per turn, then it waits for the tool results before continuing.
    -- A single queryLLM therefore only ever sees the first turn's calls (this
    -- was the "only one chapter" bug). So we run the same execute-results-and-
    -- recurse loop a general agent uses (cf. runixCodeAgentLoop): each turn,
    -- execute this turn's calls, feed their results back as ToolResultMsg, and
    -- query again — until a turn makes no calls. emit_beat_sheet's return
    -- value is the ChapterBeats itself, so we harvest it from each execution
    -- rather than caring what the model does with the result message.
    loop _ _ 0 _ = return []
    loop tools allConfigs budget history = do
      response <- queryLLM allConfigs history
      let calls = [tc | AssistantTool tc <- response]
      if null calls
        then return []
        else do
          executed <- mapM (executeToolCallFromList tools) calls
          let sheets  = [ cb | r <- executed
                             , Right value <- [toolResultOutput r]
                             , Right cb    <- [parseEither parseJSONViaCodec value] ]
              history' = history <> response <> map ToolResultMsg executed
          (sheets <>) <$> loop tools allConfigs (budget - 1) history'

    -- Bound on the number of model turns, so a model that keeps calling the
    -- tool (or never stops) can't loop forever. Far above any realistic
    -- chapter count.
    maxTurns = 60 :: Int

-- | The @emit_beat_sheet@ tool body: package the model's two arguments into a
--   'ChapterBeats'. No effect — reporting only, like 'ReplaceTool's tool.
emitBeatSheet :: forall r. BeatSheetPath -> BeatSheetBody -> Sem r ChapterBeats
emitBeatSheet (BeatSheetPath path) (BeatSheetBody body) =
  pure (ChapterBeats (T.unpack path) (BeatSheet body))

instance HasCodec ChapterBeats where
  codec = object "ChapterBeats" $
    ChapterBeats
      <$> (T.unpack <$> requiredField "path" "path to write this chapter's beat sheet to") .= (T.pack . cbPath)
      <*> (BeatSheet <$> requiredField "beat_sheet" "the chapter's beat sheet, free-form markdown") .= (unBeatSheet . cbSheet)
    where unBeatSheet (BeatSheet t) = t

instance ToolParameter ChapterBeats where
  paramName = "chapter_beats"
  paramDescription = "one chapter's target path and its beat sheet"

-- | Generate a whole chapter's prose from its beat sheet in a single call.
--   The model paces the chapter itself; the beat sheet is handed over as the
--   instruction context. Thin wrapper over 'proseAgent' — the beat sheet is
--   the instruction source, everything else is ordinary prose generation.
chapterProse
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> Maybe WordCount
  -> [CharContextBlock]
  -> [ContextBlock]
  -> ExistingContent       -- ^ prose already written for this chapter (empty for a fresh chapter)
  -> BeatSheet
  -> Sem r Prose
chapterProse configs outputHint charContexts contextBlocks existing (BeatSheet sheet) =
  proseAgent configs outputHint charContexts contextBlocks existing
    (beatSheetInstruction sheet)

-- | Generate a chapter beat by beat: repeatedly ask the model for the prose
--   of the next unwritten beat given the beat sheet and the prose so far,
--   until it signals completion. The chunking judgement lives in the model,
--   not in a parser — the beat sheet is never split by us (WRITER.md).
--
--   Prose accumulates in memory and is returned as one 'Prose'; the caller
--   splits and appends exactly as for 'chapterProse', so the two drivers are
--   interchangeable at the call site. @maxBeats@ bounds the loop so a model
--   that never emits the done sentinel can't run away.
chapterProseByBeat
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> Maybe WordCount       -- ^ approximate length hint, per beat
  -> [CharContextBlock]
  -> [ContextBlock]
  -> ExistingContent       -- ^ prose already written for this chapter
  -> BeatSheet
  -> Int                   -- ^ maxBeats: hard cap on iterations
  -> Sem r Prose
chapterProseByBeat configs outputHint charContexts contextBlocks (ExistingContent existing0) (BeatSheet sheet) maxBeats =
  Prose . dropWritten <$> go existing0 maxBeats
  where
    -- Loop until the model signals done (or the budget runs out), carrying
    -- the full prose (pre-existing + generated) so each beat sees what came
    -- before it. Returns the full accumulated text; 'dropWritten' below peels
    -- off the pre-existing prefix so the caller only gets the new prose.
    go soFar 0      = return soFar
    go soFar budget = do
      Prose piece <- proseAgent configs outputHint charContexts contextBlocks
        (ExistingContent soFar)
        (nextBeatInstruction sheet)
      let trimmed = T.strip piece
      if T.null trimmed || doneSentinel `T.isInfixOf` trimmed
        then return soFar
        else go (soFar <> "\n\n" <> trimmed) (budget - 1)

    -- What we return is only the newly generated prose, not the pre-existing
    -- content we were primed with — the caller appends our result to a file
    -- that already holds @existing0@.
    dropWritten = T.drop (T.length existing0)

-- | Regenerate a chapter to fit its beat sheet, in one call.
--
--   Unlike 'chapterProse', this is a /reconciliation/, not a continuation:
--   the model is given the current prose, the beat sheet, and the user's
--   instruction, and produces a complete new chapter that conforms to the
--   outline — preserving what already works, rewriting what contradicts it.
--   'ExistingContent' is empty here on purpose: the output is a whole
--   chapter, not an extension. The caller reconciles it against the chain
--   ('commitFiles'), so unchanged prose keeps its atom ids.
reconcileChapter
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> Maybe WordCount
  -> [CharContextBlock]
  -> [ContextBlock]
  -> CurrentProse          -- ^ the chapter's current prose (reference, to be revised)
  -> Instruction           -- ^ the user's additional steer
  -> BeatSheet
  -> Sem r Prose
reconcileChapter configs outputHint charContexts contextBlocks current userInstr (BeatSheet sheet) =
  proseAgent configs outputHint charContexts contextBlocks (ExistingContent "")
    (reconcileInstruction current userInstr sheet)

-- | Regenerate a chapter to fit its beat sheet, beat by beat. Same
--   reconciliation framing as 'reconcileChapter', but the model advances one
--   beat per call (see 'chapterProseByBeat' for the loop mechanics), with the
--   current prose and the user's steer folded into each beat's instruction so
--   every beat is reconciled against the outline rather than continued.
reconcileChapterByBeat
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> Maybe WordCount
  -> [CharContextBlock]
  -> [ContextBlock]
  -> CurrentProse
  -> Instruction
  -> BeatSheet
  -> Int                   -- ^ maxBeats: hard cap on iterations
  -> Sem r Prose
reconcileChapterByBeat configs outputHint charContexts contextBlocks current userInstr (BeatSheet sheet) maxBeats =
  Prose <$> go "" maxBeats
  where
    go soFar 0      = return soFar
    go soFar budget = do
      Prose piece <- proseAgent configs outputHint charContexts contextBlocks
        (ExistingContent soFar)
        (reconcileNextBeatInstruction current userInstr sheet)
      let trimmed = T.strip piece
      if T.null trimmed || doneSentinel `T.isInfixOf` trimmed
        then return soFar
        else go (if T.null soFar then trimmed else soFar <> "\n\n" <> trimmed) (budget - 1)

-- | Sentinel the model emits (instead of prose) to signal the chapter is
--   fully realized. Kept blunt and unlikely to occur in prose.
doneSentinel :: Text
doneSentinel = "[[CHAPTER-COMPLETE]]"

-- | Frame a whole-chapter reconciliation: current prose in, conformed
--   chapter out.
reconcileInstruction :: CurrentProse -> Instruction -> Text -> Instruction
reconcileInstruction (CurrentProse current) (Instruction userInstr) sheet = Instruction $
  "Rewrite this chapter so it conforms to the beat sheet below. Preserve prose \
  \that already works; change anything that contradicts, is missing from, or \
  \no longer fits the beat sheet. Fix inconsistencies with the outline. Output \
  \the full revised chapter as prose, nothing else.\n\n"
  <> userSteer userInstr
  <> "Beat sheet:\n\n" <> sheet <> "\n\n"
  <> "Current chapter:\n\n" <> currentOrEmpty current

-- | The beat-by-beat variant of 'reconcileInstruction': the model writes the
--   next beat's conformed prose given the prose emitted so far.
reconcileNextBeatInstruction :: CurrentProse -> Instruction -> Text -> Instruction
reconcileNextBeatInstruction (CurrentProse current) (Instruction userInstr) sheet = Instruction $
  "Below is a beat sheet, the chapter's current prose, the user's steer, and \
  \the revised prose written so far (shown as the file to continue). Write the \
  \revised prose for the NEXT beat only — conformed to the beat sheet, \
  \preserving current prose where it works and fixing what contradicts the \
  \outline. Output only that beat's prose. If every beat is done, output \
  \exactly " <> doneSentinel <> " and nothing else.\n\n"
  <> userSteer userInstr
  <> "Beat sheet:\n\n" <> sheet <> "\n\n"
  <> "Current chapter:\n\n" <> currentOrEmpty current

userSteer :: Text -> Text
userSteer instr
  | T.null (T.strip instr) = ""
  | otherwise              = "Additional instructions from the user:\n\n" <> instr <> "\n\n"

currentOrEmpty :: Text -> Text
currentOrEmpty t
  | T.null (T.strip t) = "(the chapter has no prose yet — write it from the beat sheet)"
  | otherwise          = t

-- | Frame a full beat sheet as a single writing instruction.
beatSheetInstruction :: Text -> Instruction
beatSheetInstruction sheet = Instruction $
  "Write the prose for this chapter, following the beat sheet below. Realize \
  \every beat in order, at the pacing and length the beats imply. Output only \
  \prose.\n\nBeat sheet:\n\n" <> sheet

-- | Frame the beat sheet for one iteration of the beat-by-beat loop: given
--   the sheet and the prose already written (passed as 'ExistingContent'),
--   the model writes the next unwritten beat, or emits the done sentinel.
nextBeatInstruction :: Text -> Instruction
nextBeatInstruction sheet = Instruction $
  "Below is a beat sheet for a chapter, and the prose written for it so far \
  \(shown as the file to continue). Write the prose for the NEXT beat only — \
  \the first beat not yet covered by the prose so far. Output only that beat's \
  \prose, nothing else. If every beat is already covered, output exactly "
  <> doneSentinel <> " and nothing else.\n\nBeat sheet:\n\n" <> sheet

-- Prompt keys / defaults --------------------------------------------------

systemKey :: ExpandGoal -> PromptKey
systemKey ToBeatSheet = "agent.outline.beatsheet.system"

templateKey :: ExpandGoal -> PromptKey
templateKey ToBeatSheet = "agent.outline.beatsheet.template"

defaultExpandSystem :: ExpandGoal -> Prompt
defaultExpandSystem ToBeatSheet =
  "You are a story planner. Given a chapter's entry in a story outline, expand \
  \it into a beat sheet: one Markdown heading per beat, and under each, prose \
  \notes covering what happens, the logistics (who is present, what must be \
  \true), the emotional turn, and a rough target length in words. Write \
  \sensible, skimmable Markdown — not rigid fields. Output only the beat sheet."

-- | Slots: {{context}}, {{source}}.
defaultExpandTemplate :: Prompt
defaultExpandTemplate =
  "{{context}}Outline to expand:\n\n{{source}}\n\n\
  \Write the beat sheet now. Output only the beat sheet, no commentary."

defaultSplitSystem :: Prompt
defaultSplitSystem =
  "You are a story planner. Given a whole-story outline, divide it into \
  \chapters and produce one beat sheet per chapter. The chapters may not \
  \exist yet — use your judgement to decide how many there are and where each \
  \begins and ends. For each chapter, call emit_beat_sheet once, with a path \
  \(chapters/ch1.outline.md, chapters/ch2.outline.md, …, in reading order) and \
  \the beat sheet: one Markdown heading per beat, prose notes under each \
  \covering what happens, logistics, the emotional turn, and a rough length. \
  \Call the tool once per chapter and emit nothing else."

-- | Slots: {{context}}, {{source}}.
defaultSplitTemplate :: Prompt
defaultSplitTemplate =
  "{{context}}Story outline to divide into chapter beat sheets:\n\n{{source}}\n\n\
  \Call emit_beat_sheet once per chapter, in reading order."
