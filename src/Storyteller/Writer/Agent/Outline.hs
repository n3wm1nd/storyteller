{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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
  , splitOutlineFreeform
  , splitOutlineBulk
  , splitOnRule
  , chapterProse
  , chapterProseByBeat
  , reconcileChapter
  , reconcileChapterByBeat
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Autodocodec (HasCodec(..), dimapCodec, object, requiredField, parseJSONViaCodec, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.LLM.ToolInstances ()
import Runix.Logging (Logging, info, warning)
import UniversalLLM (Message(..), ModelConfig(..))
import UniversalLLM.Tools
  ( ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition
  , executeToolCallFromList, ToolResult(..)
  )

import Storyteller.Core.LLM.Role (LLMs, ProseModel, AgentModel)
import Storyteller.Core.LLM.Interceptor (withTurnBudget)
import Storyteller.Writer.Agent
  ( Instruction(..), Prose(..), CharContextBlock, ContextBlock(..)
  , ExistingContent(..), WordCount(..) )
import Storyteller.Writer.Agent.Continuation (proseAgent)
import Storyteller.Core.Prompt (Prompt(..), PromptKey, PromptStorage, getPrompt, getConfigWithPrompt)

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
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => ExpandGoal
  -> [ContextBlock]        -- ^ surrounding context (other chapters' outlines, world files, ...)
  -> OutlineDoc            -- ^ the document being expanded
  -> Sem r Text
expandAgent goal contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt (systemKey goal) (defaultExpandSystem goal) (defaultExpandConfig goal)
  Prompt closing    <- getPrompt (instructionsKey goal) (defaultExpandInstructions goal)

  let contextSection
        | null contextBlocks = ""
        | otherwise =
            "Surrounding context:\n\n"
            <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
            <> "\n\n"

      userMsg = contextSection <> "Outline to expand:\n\n" <> doc <> "\n\n" <> closing

  info "expandAgent: querying model..."
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

-- | Split a story outline into per-chapter beat sheets.
--
--   Unlike 'expandAgent' (one document in, one out), the model here decides
--   the chapter breakdown itself and emits one beat sheet per chapter via
--   repeated @emit_beat_sheet@ tool calls — each call names the target file
--   and carries that chapter's beats. Chapters needn't be marked out in the
--   outline already: the model's own judgement determines how many there
--   are, where each begins and ends, and invents concrete beats to fill in
--   what the outline only sketches, while staying faithful to its structure.
--
--   Safe to call again as a story grows, not just once up front: @doc@ can
--   be the whole story or only as much of it as exists so far, and
--   @contextBlocks@ (which already carries every other branch file, see
--   'Server.Writer.File.chatSplitOutline') lets the model see which
--   chapters already have a beat sheet and skip re-emitting those — the
--   system prompt tells it to. Returns every emitted @(path, beat sheet)@
--   for chapters that didn't have one yet; the caller writes them. A
--   malformed call is dropped rather than failing the whole batch.
splitOutlineAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [ContextBlock]        -- ^ surrounding context (world files, notes, ...)
  -> OutlineDoc            -- ^ the whole-story outline being split
  -> Sem r [ChapterBeats]
splitOutlineAgent contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt "agent.outline.split" defaultSplitSystem defaultSplitConfig
  Prompt closing    <- getPrompt "agent.outline.split.instructions" defaultSplitInstructions

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

      userMsg = contextSection <> "Story outline to divide into chapter beat sheets:\n\n" <> doc <> "\n\n" <> closing

  let allConfigs = Tools (map llmToolToDefinition tools) : configsWithPrompt
  info "splitOutlineAgent: splitting outline into chapter beat sheets"
  sheets <- loop tools allConfigs (1 :: Int) maxTurns [UserText userMsg]
  info $ "splitOutlineAgent: done, " <> T.pack (show (length sheets)) <> " beat sheet(s) emitted"
  return sheets
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
    --
    -- What actually goes back into history' as each call's result is *not*
    -- the raw execution output, though: 'emitBeatSheet' just echoes its own
    -- arguments back as a 'ChapterBeats', so the unmodified result would put
    -- the beat sheet the model just wrote into its own context a second
    -- time, as the "result" -- accumulating turn after turn, that's every
    -- prior chapter's text present twice for no new information (the model
    -- already knows what it wrote), with nothing that reads as "chapter 1
    -- done, N to go." 'confirmationFor' trims that down to just the saved
    -- path -- 'sheets' below still harvests the real 'ChapterBeats' from the
    -- untrimmed 'executed', so nothing is lost, only what the model has to
    -- re-read every subsequent turn.
    --
    -- Logged turn by turn (which path each call targeted, any failures) so a
    -- long-running split is visible while it's happening, not just as a
    -- final chapter count -- the same visibility
    -- @Agent.Integration.Harness.recordToolCalls@ gives the test suite,
    -- now also there for a real user watching server\/CLI logs.
    loop _ _ turnNo 0 _ = do
      warning $ "splitOutlineAgent: hit the " <> T.pack (show turnNo) <> "-turn budget without the model settling, stopping"
      return []
    loop tools allConfigs turnNo budget history = do
      info $ "splitOutlineAgent: turn " <> T.pack (show turnNo) <> ": querying model..."
      response <- queryLLM allConfigs history
      let calls = [tc | AssistantTool tc <- response]
      if null calls
        then return []
        else do
          executed <- mapM (executeToolCallFromList tools) calls
          let sheets  = [ cb | r <- executed
                             , Right value <- [toolResultOutput r]
                             , Right cb    <- [parseEither parseJSONViaCodec value] ]
              failed  = length executed - length sheets
          mapM_ (\(ChapterBeats path _) -> info ("splitOutlineAgent: turn " <> T.pack (show turnNo) <> ": emit_beat_sheet -> " <> T.pack path)) sheets
          if failed > 0
            then warning $ "splitOutlineAgent: turn " <> T.pack (show turnNo) <> ": "
                   <> T.pack (show failed) <> " of " <> T.pack (show (length executed)) <> " call(s) failed or didn't parse"
            else pure ()
          let history' = history <> response <> map (ToolResultMsg . confirmationFor) executed
          (sheets <>) <$> loop tools allConfigs (turnNo + 1) (budget - 1) history'

    -- Replace a successful emit_beat_sheet result with a short save
    -- confirmation instead of echoing the submitted 'ChapterBeats' back
    -- whole -- see the Haddock on 'loop' above. An unparseable or failed
    -- result is left as-is: that error message is small already, and the
    -- model needs to see exactly what went wrong to retry correctly.
    confirmationFor :: ToolResult -> ToolResult
    confirmationFor r@(ToolResult call (Right value)) =
      case parseEither parseJSONViaCodec value of
        Right (ChapterBeats path _) -> ToolResult call (Right (Aeson.object ["saved" Aeson..= T.pack path]))
        Left _                      -> r
    confirmationFor r = r

    -- Bound on the number of model turns, so a model that keeps calling the
    -- tool (or never stops) can't loop forever. Far above any realistic
    -- chapter count.
    maxTurns = 60 :: Int

-- | Alternative to 'splitOutlineAgent': same job (one beat sheet per
--   chapter), but drives it as a plain conversation instead of a tool-call
--   loop -- ask for "the next chapter," get back markdown prose, repeat
--   until a sentinel signals every chapter is covered, the same shape
--   'chapterProseByBeat' already uses for pacing prose beat by beat.
--
--   Exists because of a measured finding, not a hunch: probing gpt-oss-20b
--   directly with the same task, bypassing tool calling entirely, produced
--   a perfectly correct three-chapter breakdown on an outline where
--   'splitOutlineAgent' reliably duplicated, omitted, or garbled chapters
--   (see the agent-integration suite's @../PLAN.md@). The working
--   hypothesis: a forced-JSON tool call fragments a weaker model's
--   generation into isolated, syntax-constrained completions, and gives it
--   nothing but its own past tool calls to reconstruct "what have I already
--   covered" from. Plain conversation lets it draft continuously, and this
--   function removes the self-tracking burden a step further by having
--   *us* -- not the model -- count chapters and assign paths, rather than
--   asking the model to name its own target each time.
--
--   Doesn't replace 'splitOutlineAgent': that one's explicit per-chapter
--   targeting and "skip chapters that already have a beat sheet" logic are
--   still the right shape for filling in missing or added chapters later,
--   where the model needs to name one specific existing gap rather than
--   just continue a sequence from wherever it left off. This is for the
--   first, whole-outline split -- see @../PLAN.md@ for the experiment this
--   is meant to run (one chapter per turn, as here, vs. everything in one
--   bulk response) before either replaces anything in production.
splitOutlineFreeform
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [ContextBlock]        -- ^ surrounding context (world files, notes, ...)
  -> OutlineDoc            -- ^ the whole-story outline being split
  -> Sem r [ChapterBeats]
splitOutlineFreeform contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt "agent.outline.split.freeform" defaultFreeformSystem defaultFreeformConfig
  Prompt opening    <- getPrompt "agent.outline.split.freeform.instructions" defaultFreeformInstructions

  let contextSection
        | null contextBlocks = ""
        | otherwise =
            "Surrounding context:\n\n"
            <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
            <> "\n\n"

      openingMsg = contextSection <> "Story outline to divide into chapter beat sheets:\n\n" <> doc <> "\n\n" <> opening

  info "splitOutlineAgent (freeform): splitting outline into chapter beat sheets"
  -- Turn budget is tacked on via 'withTurnBudget', not a branch of 'go's
  -- own -- once the model has taken 'maxTurns' turns without settling,
  -- @withTurnBudget@ starts handing 'go' the sentinel itself, and 'go'
  -- stops the exact same way it would if the model had said so. 'go'
  -- itself never has to know a budget exists.
  sheets <- withTurnBudget @AgentModel maxTurns outlineCompleteSentinel
    (go configsWithPrompt [UserText openingMsg] (1 :: Int))
  info $ "splitOutlineAgent (freeform): done, " <> T.pack (show (length sheets)) <> " beat sheet(s) emitted"
  return sheets
  where
    -- One chapter per turn: each response is taken whole as that chapter's
    -- beat sheet (no parsing, no delimiter to grep for -- WRITER.md's usual
    -- rule), and *we* assign chapters/ch{N}.outline.md in order rather than
    -- asking the model to. The model's only job each turn is "write the
    -- next chapter, or say you're done" -- not "remember which of your own
    -- past tool calls covered which chapter."
    go configsWithPrompt history chapterNo = do
      info $ "splitOutlineAgent (freeform): chapter " <> T.pack (show chapterNo) <> ": querying model..."
      response <- queryLLM configsWithPrompt history
      let piece = T.strip (mconcat [ t | AssistantText t <- response ])
      if T.null piece || outlineCompleteSentinel `T.isInfixOf` piece
        then return []
        else do
          let path = "chapters/ch" <> show chapterNo <> ".outline.md"
          info $ "splitOutlineAgent (freeform): chapter " <> T.pack (show chapterNo) <> " -> " <> T.pack path
          let history' = history <> response <> [UserText nextChapterPrompt]
          (ChapterBeats path (BeatSheet piece) :)
            <$> go configsWithPrompt history' (chapterNo + 1)

    nextChapterPrompt = "Write the next chapter's beat sheet now, continuing in reading order. If every \
      \chapter in the outline has already been covered, respond with exactly "
      <> outlineCompleteSentinel <> " and nothing else."

    -- One chapter per turn (unlike splitOutlineAgent's tool loop, which can
    -- fit several calls in one turn), so this needs its own, larger budget.
    maxTurns = 40 :: Int

-- | Sentinel for 'splitOutlineFreeform' signalling every chapter in the
--   outline has been covered -- distinct from 'doneSentinel', which signals
--   a single chapter's *prose* is finished, not the whole split.
outlineCompleteSentinel :: Text
outlineCompleteSentinel = "[[OUTLINE-COMPLETE]]"

-- | Second variant of the same experiment 'splitOutlineFreeform' runs: same
--   job, same no-tool-calls premise, but the whole split in a single
--   response instead of one chapter per conversational turn. The model
--   writes every chapter's beat sheet at once, separated by a line
--   containing only @---@; we split on that deterministically and assign
--   paths in order, the same way 'splitOutlineFreeform' assigns them per
--   turn.
--
--   This is the one place in the module that actually depends on a
--   delimiter to grep for, which the module Haddock's "neither depends on
--   a delimiter" claim is about a different granularity than: that's about
--   never parsing *beats* out of a beat sheet's own free-form markdown
--   (still true, still nobody's job here). Splitting a bulk response into
--   *chapters* is a coarser, new kind of parsing this function alone
--   introduces, and it's exactly the tradeoff being tested: does asking for
--   one big structured response (parseable, but only if the model reliably
--   produces the delimiter) work better or worse than
--   'splitOutlineFreeform's one-small-request-per-chapter, no-parsing-needed
--   shape? See @FINDINGS.md@ once that comparison has actually been run.
--
--   If the model doesn't use the delimiter at all, this comes back with a
--   single "chapter" containing everything -- logged as a warning (visible,
--   not retried) rather than guessed at further; a real fix would need to
--   look at why, not just resubmit and hope.
splitOutlineBulk
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [ContextBlock]        -- ^ surrounding context (world files, notes, ...)
  -> OutlineDoc            -- ^ the whole-story outline being split
  -> Sem r [ChapterBeats]
splitOutlineBulk contextBlocks (OutlineDoc doc) = do
  configsWithPrompt <- getConfigWithPrompt "agent.outline.split.bulk" defaultBulkSystem defaultBulkConfig
  Prompt closing    <- getPrompt "agent.outline.split.bulk.instructions" defaultBulkInstructions

  let contextSection
        | null contextBlocks = ""
        | otherwise =
            "Surrounding context:\n\n"
            <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
            <> "\n\n"

      userMsg = contextSection <> "Story outline to divide into chapter beat sheets:\n\n" <> doc <> "\n\n" <> closing

  info "splitOutlineAgent (bulk): splitting outline into chapter beat sheets"
  response <- queryLLM configsWithPrompt [UserText userMsg]
  let whole  = mconcat [ t | AssistantText t <- response ]
      pieces = filter (not . T.null) (map T.strip (splitOnRule whole))
      sheets = zipWith (\n piece -> ChapterBeats ("chapters/ch" <> show n <> ".outline.md") (BeatSheet piece))
                 [1 :: Int ..] pieces

  if length pieces <= 1
    then warning "splitOutlineAgent (bulk): response didn't contain a --- delimiter, treating it as one chapter"
    else pure ()
  mapM_ (\(ChapterBeats path _) -> info ("splitOutlineAgent (bulk): " <> T.pack path)) sheets
  info $ "splitOutlineAgent (bulk): done, " <> T.pack (show (length sheets)) <> " beat sheet(s) emitted"
  return sheets

-- | Split text on any line that's a bare Markdown horizontal rule
--   (@---@, alone on its line once stripped) -- the delimiter
--   'defaultBulkSystem' asks the model to put between chapters.
splitOnRule :: Text -> [Text]
splitOnRule = map (T.strip . T.unlines) . go . T.lines
  where
    go [] = [[]]
    go (l:ls)
      | T.strip l == "---" = [] : go ls
      | otherwise          = case go ls of
                                (cur : rest) -> (l : cur) : rest
                                []           -> [[l]]

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
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Maybe WordCount
  -> [CharContextBlock]
  -> [Message ProseModel]
  -> ExistingContent       -- ^ prose already written for this chapter (empty for a fresh chapter)
  -> BeatSheet
  -> Sem r Prose
chapterProse outputHint charContexts context existing (BeatSheet sheet) =
  proseAgent outputHint charContexts context existing
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
--   Logged beat by beat, same reasoning as 'splitOutlineAgent'\/'reworkAtomsAt':
--   each beat is its own separate 'queryLLM' call, so a chapter running long
--   would otherwise look identical to a hang between one beat's streamed
--   tokens ending and the next beat's starting.
chapterProseByBeat
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Maybe WordCount       -- ^ approximate length hint, per beat
  -> [CharContextBlock]
  -> [Message ProseModel]
  -> ExistingContent       -- ^ prose already written for this chapter
  -> BeatSheet
  -> Int                   -- ^ maxBeats: hard cap on iterations
  -> Sem r Prose
chapterProseByBeat outputHint charContexts context (ExistingContent existing0) (BeatSheet sheet) maxBeats =
  Prose . dropWritten <$> go existing0 (1 :: Int) maxBeats
  where
    -- Loop until the model signals done (or the budget runs out), carrying
    -- the full prose (pre-existing + generated) so each beat sees what came
    -- before it. Returns the full accumulated text; 'dropWritten' below peels
    -- off the pre-existing prefix so the caller only gets the new prose.
    go soFar beatNo 0 = do
      warning $ "chapterProseByBeat: hit the " <> T.pack (show (beatNo - 1)) <> "-beat budget without the model signalling done, stopping"
      return soFar
    go soFar beatNo budget = do
      info $ "chapterProseByBeat: beat " <> T.pack (show beatNo) <> ": querying model..."
      Prose piece <- proseAgent outputHint charContexts context
        (ExistingContent soFar)
        (nextBeatInstruction sheet)
      let trimmed = T.strip piece
      if T.null trimmed || doneSentinel `T.isInfixOf` trimmed
        then do
          info $ "chapterProseByBeat: done, " <> T.pack (show (beatNo - 1)) <> " beat(s) written"
          return soFar
        else do
          info $ "chapterProseByBeat: beat " <> T.pack (show beatNo) <> " written"
          go (soFar <> "\n\n" <> trimmed) (beatNo + 1) (budget - 1)

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
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Maybe WordCount
  -> [CharContextBlock]
  -> [Message ProseModel]
  -> CurrentProse          -- ^ the chapter's current prose (reference, to be revised)
  -> Instruction           -- ^ the user's additional steer
  -> BeatSheet
  -> Sem r Prose
reconcileChapter outputHint charContexts context current userInstr (BeatSheet sheet) =
  proseAgent outputHint charContexts context (ExistingContent "")
    (reconcileInstruction current userInstr sheet)

-- | Regenerate a chapter to fit its beat sheet, beat by beat. Same
--   reconciliation framing as 'reconcileChapter', but the model advances one
--   beat per call (see 'chapterProseByBeat' for the loop mechanics), with the
--   current prose and the user's steer folded into each beat's instruction so
--   every beat is reconciled against the outline rather than continued.
--   Logged beat by beat -- same reasoning as 'chapterProseByBeat'.
reconcileChapterByBeat
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Maybe WordCount
  -> [CharContextBlock]
  -> [Message ProseModel]
  -> CurrentProse
  -> Instruction
  -> BeatSheet
  -> Int                   -- ^ maxBeats: hard cap on iterations
  -> Sem r Prose
reconcileChapterByBeat outputHint charContexts context current userInstr (BeatSheet sheet) maxBeats =
  Prose <$> go "" (1 :: Int) maxBeats
  where
    go soFar beatNo 0 = do
      warning $ "reconcileChapterByBeat: hit the " <> T.pack (show (beatNo - 1)) <> "-beat budget without the model signalling done, stopping"
      return soFar
    go soFar beatNo budget = do
      info $ "reconcileChapterByBeat: beat " <> T.pack (show beatNo) <> ": querying model..."
      Prose piece <- proseAgent outputHint charContexts context
        (ExistingContent soFar)
        (reconcileNextBeatInstruction current userInstr sheet)
      let trimmed = T.strip piece
      if T.null trimmed || doneSentinel `T.isInfixOf` trimmed
        then do
          info $ "reconcileChapterByBeat: done, " <> T.pack (show (beatNo - 1)) <> " beat(s) written"
          return soFar
        else do
          info $ "reconcileChapterByBeat: beat " <> T.pack (show beatNo) <> " written"
          go (if T.null soFar then trimmed else soFar <> "\n\n" <> trimmed) (beatNo + 1) (budget - 1)

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

-- | The namespace root -- see 'Storyteller.Core.Prompt' on why that's
--   implicitly the system prompt/config, not a @.system@ leaf.
systemKey :: ExpandGoal -> PromptKey
systemKey ToBeatSheet = "agent.outline.beatsheet"

-- | The one free-text part of 'expandAgent's user message a prompt override
--   can actually change -- the "Outline to expand:"/@doc@ framing is fixed
--   Haskell structure, not a slotted template (see 'Storyteller.Core.Prompt'
--   on why user-facing overrides never expose template slots).
instructionsKey :: ExpandGoal -> PromptKey
instructionsKey ToBeatSheet = "agent.outline.beatsheet.instructions"

defaultExpandSystem :: ExpandGoal -> Prompt
defaultExpandSystem ToBeatSheet =
  "You are a story planner. Given a chapter's entry in a story outline, expand \
  \it into a beat sheet: one Markdown heading per beat, and under each, prose \
  \notes covering what happens, the logistics (who is present, what must be \
  \true), the emotional turn, and a rough target length in words. Write \
  \sensible, skimmable Markdown — not rigid fields. Output only the beat sheet."

defaultExpandInstructions :: ExpandGoal -> Prompt
defaultExpandInstructions ToBeatSheet =
  "Write the beat sheet now. Output only the beat sheet, no commentary."

-- | Compiled-in sampling default for @agent.outline.beatsheet@ -- see
--   @$key.llmsettings.yaml@ overrides via 'Storyteller.Core.Prompt.getConfig'.
--   A beat sheet is skeletal planning notes for one chapter, not full prose
--   -- shorter than 'defaultWriterConfig's budget, and a touch cooler since
--   this is closer to structured planning than free composition.
defaultExpandConfig :: ExpandGoal -> [ModelConfig ProseModel]
defaultExpandConfig ToBeatSheet = [MaxTokens 1536, Temperature 0.8]

defaultSplitSystem :: Prompt
defaultSplitSystem =
  "You are a story planner. You'll be given an outline for a story — either \
  \the whole story, or as much of it as exists so far — and your job is to \
  \divide it into chapters and produce one beat sheet per chapter, so each \
  \chapter can be developed on its own. The chapters may not be marked out \
  \explicitly; use your judgement to decide how many there are and where \
  \each begins and ends, inventing concrete plot beats to fill each chapter \
  \out where the outline only sketches — be creative there, but stay \
  \faithful to the outline's own structure and intent: don't introduce \
  \events, characters, or a sequence that contradicts it. \
  \\n\n\
  \If the outline already marks out chapters — headings, numbered sections, \
  \any explicit break — treat that division as deliberate, not a rough \
  \sketch to improve on: whoever wrote it chose where one chapter ends and \
  \the next begins, for reasons of pacing, a cliffhanger, or chapter length \
  \that may not be spelled out. Keep every event in the chapter the outline \
  \already put it in. Do not move a beat into a neighboring chapter, even if \
  \a different split would read more smoothly to you. \
  \\n\n\
  \Before writing anything, check the surrounding context for chapters that \
  \already have a beat sheet (a chapters/*.outline.md file). Skip those — \
  \do not call emit_beat_sheet for a chapter that already has one. Only \
  \emit beat sheets for chapters that don't have one yet, so this is safe \
  \to run again as a story grows: it fills in what's missing rather than \
  \redoing what's already there. \
  \\n\n\
  \For each remaining chapter, call emit_beat_sheet once, with a path \
  \(chapters/ch1.outline.md, chapters/ch2.outline.md, …, in reading order) and \
  \the beat sheet: one Markdown heading per beat, prose notes under each \
  \covering what happens, logistics, the emotional turn, and a rough length. \
  \Call the tool once per chapter and emit nothing else."

-- | The one free-text part of 'splitOutlineAgent's user message a prompt
--   override can actually change -- see 'instructionsKey'.
defaultSplitInstructions :: Prompt
defaultSplitInstructions =
  "Call emit_beat_sheet once per chapter, in reading order."

-- | Compiled-in sampling default for @agent.outline.split@ -- see
--   @$key.llmsettings.yaml@ overrides via 'Storyteller.Core.Prompt.getConfig'.
--   With a slightly cooler temperature than 'defaultExpandConfig': this is a
--   judgement call about chapter boundaries, not creative composition, so
--   consistency matters a little more than variation -- but the beat sheet
--   text itself still needs some.
--
--   @MaxTokens@ deliberately above 'Storyteller.Writer.Agent.Continuation.defaultWriterConfig'\'s
--   3000, not below it: a beat sheet is skeletal notes, but it's an entire
--   chapter's worth (several headed beats, each with logistics/emotional-turn/
--   length notes) packed into a *single* tool-call argument, and it has to
--   fit inside one JSON string with room to spare -- 1536 measured too
--   tight in practice (@Agent.Integration.OutlineSplitQualitySpec@, run
--   against a real, richly-detailed user outline rather than a short
--   agent-generated one, hit @emit_beat_sheet@ calls truncated mid-string
--   well before the model finished writing the beat sheet).
defaultSplitConfig :: [ModelConfig AgentModel]
defaultSplitConfig = [MaxTokens 4096, Temperature 0.7]

-- | System prompt for 'splitOutlineFreeform' -- the same job as
--   'defaultSplitSystem', reframed for plain conversation instead of
--   repeated tool calls: no path assignment (that's 'splitOutlineFreeform'\'s
--   own job now), no "call the tool," just "write this chapter's beat sheet
--   as markdown, or say you're done."
defaultFreeformSystem :: Prompt
defaultFreeformSystem =
  "You are a story planner. You'll be given an outline for a story — either \
  \the whole story, or as much of it as exists so far — and asked to write \
  \one beat sheet per chapter, one at a time, in a running conversation: \
  \each turn you'll be asked for the next chapter's beat sheet, in reading \
  \order, and should respond with plain markdown only — one Markdown \
  \heading per beat, prose notes under each covering what happens, \
  \logistics, the emotional turn, and a rough length. No tool calls, no \
  \code fences, no commentary outside the beat sheet itself — just the beat \
  \sheet, since your whole response each turn is taken as exactly one \
  \chapter. \
  \\n\n\
  \The chapters may not be marked out explicitly; use your judgement to \
  \decide how many there are and where each begins and ends, inventing \
  \concrete plot beats to fill each chapter out where the outline only \
  \sketches — be creative there, but stay faithful to the outline's own \
  \structure and intent. If the outline already marks out chapters, treat \
  \that division as deliberate: keep every event in the chapter the outline \
  \already put it in, don't move a beat into a neighboring chapter."

-- | The one free-text part of 'splitOutlineFreeform's opening user message
--   a prompt override can actually change.
defaultFreeformInstructions :: Prompt
defaultFreeformInstructions =
  "Write the first chapter's beat sheet now."

-- | Compiled-in sampling default for @agent.outline.split.freeform@ -- same
--   reasoning as 'defaultSplitConfig'\'s @MaxTokens@ (a beat sheet needs
--   room), but nothing here is packed inside a JSON string argument, so
--   there's no extra JSON-overhead margin to budget for on top of the beat
--   sheet's own length.
defaultFreeformConfig :: [ModelConfig AgentModel]
defaultFreeformConfig = [MaxTokens 4096, Temperature 0.7]

-- | System prompt for 'splitOutlineBulk' -- 'defaultFreeformSystem'
--   reframed again, this time for one single response covering every
--   chapter: the model has to both decide the chapter breakdown *and*
--   signal it via the @---@ delimiter, in one shot, rather than each
--   chapter being its own separate, focused request.
defaultBulkSystem :: Prompt
defaultBulkSystem =
  "You are a story planner. You'll be given an outline for a story — either \
  \the whole story, or as much of it as exists so far — and asked to write \
  \one beat sheet per chapter, all in a single response. For each chapter, \
  \write a Markdown heading, then prose notes covering what happens, \
  \logistics, the emotional turn, and a rough length — plain markdown, no \
  \tool calls. Separate one chapter's beat sheet from the next with a line \
  \containing only ---, and nothing else on that line — no other horizontal \
  \rules, no code fences, no commentary outside the beat sheets themselves. \
  \\n\n\
  \The chapters may not be marked out explicitly; use your judgement to \
  \decide how many there are and where each begins and ends, inventing \
  \concrete plot beats to fill each chapter out where the outline only \
  \sketches — be creative there, but stay faithful to the outline's own \
  \structure and intent. If the outline already marks out chapters, treat \
  \that division as deliberate: keep every event in the chapter the outline \
  \already put it in, don't move a beat into a neighboring chapter."

-- | The one free-text part of 'splitOutlineBulk's user message a prompt
--   override can actually change.
defaultBulkInstructions :: Prompt
defaultBulkInstructions =
  "Write every chapter's beat sheet now, separated by --- as described."

-- | Compiled-in sampling default for @agent.outline.split.bulk@ -- same
--   reasoning as 'defaultFreeformConfig', except @MaxTokens@ has to cover
--   *every* chapter's beat sheet in one response instead of just one, so
--   it needs considerably more headroom.
defaultBulkConfig :: [ModelConfig AgentModel]
defaultBulkConfig = [MaxTokens 8192, Temperature 0.7]
