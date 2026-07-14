{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The first real per-domain summarizer: compresses prose. Plugs into
-- 'Storyteller.Writer.Agent.Summarizer.runSummarizer' as the @"prose/chapter"@
-- kind's @generate@ hook, replacing 'Server.Writer.Branch.passthroughGenerate'
-- for that one kind (see its own docstring: "swap this out, per kind, once
-- [a real summarizer] exists").
--
-- Split the way 'Storyteller.Writer.Agent.Continuation.gatherFileContext'\/
-- 'proseAgent' are: 'unitSummaryCandidates' is the pure read-side (which
-- files changed, and what), 'chapterSummaryGenerate' is the effectful glue
-- (fetch each file's prior compression, call the model, assemble the
-- result), 'chapterSummaryAgent' is the one LLM call.
module Storyteller.Writer.Agent.ChapterSummarizer
  ( unitSummaryCandidates
  , chapterSummaryGenerate
  , chapterSummaryAgent
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Common.Summary (lastSummaryOf, summaryContent)
import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick, fromTick)
import Storyteller.Writer.Library (LibraryKind(..), classifyPath)

-- | Pure grouping: every 'Atom' tick among @candidates@ whose path
--   classifies as a prose 'Unit' (see 'Storyteller.Writer.Library.
--   classifyPath'), concatenated per path -- same shape as
--   'Server.Writer.Branch.passthroughGenerate', filtered to real chapter
--   files instead of every atom-touched path on the chain. No effects, so
--   this is the piece 'test.Storyteller.ChapterSummarizerSpec' pins
--   directly, per the project's "extract pure before wiring" convention.
unitSummaryCandidates :: [Tick] -> Map FilePath Text
unitSummaryCandidates = List.foldl' step Map.empty
  where
    step acc t = case fromTick @Atom t of
      Just (Atom file _) | classifyPath file == Unit -> Map.insertWith (flip (<>)) file (contentFor file t) acc
      _                                               -> acc

-- | The @generate@ hook 'Storyteller.Writer.Agent.Summarizer.runSummarizer'
--   expects: for every touched chapter, look up @kind@'s most recent prior
--   compression of that exact path (empty if none -- either the file is new
--   or this is the kind's very first pass), and ask 'chapterSummaryAgent'
--   to fold the new tail into it. One 'chapterSummaryAgent' call per
--   touched chapter; every result lands in the one 'Map' 'runSummarizer'
--   then writes as a single alternate-chain commit, per its own
--   one-commit-per-pass invariant.
chapterSummaryGenerate
  :: forall source r
  .  (LLMs r, Members '[BranchOp source, StoryStorage, Git, PromptStorage, Fail, Logging] r)
  => Text -> [Tick] -> Sem r (Map FilePath Text)
chapterSummaryGenerate kind candidates =
  Map.fromList <$> mapM summarizeOne (Map.toList (unitSummaryCandidates candidates))
  where
    summarizeOne (path, newTail) = do
      mPrev <- runStorage @source (lastSummaryOf kind)
      previous <- case mPrev of
        Nothing     -> return ""
        Just (_, s) -> runStorage @source (fromMaybe "" <$> summaryContent s path)
      compressed <- chapterSummaryAgent previous newTail
      return (path, compressed)

-- | Compress one chapter's content: fold @newTail@ (the raw prose written
--   since @previous@ was produced) into an updated compressed summary,
--   preserving whatever a later chapter might need to reference back to.
--   @previous@ is empty on a kind's first pass for this path -- the prompt
--   treats that as "summarize this from scratch," not a special case the
--   caller has to branch on.
--
--   Uses 'ProseModel' (plain text out, no tools needed -- same role
--   'Storyteller.Writer.Agent.Continuation.proseAgent' uses), its own
--   'PromptStorage' key so an operator can retune the summarizer
--   independently of @agent.writer@, and a low-temperature default (unlike
--   'Storyteller.Writer.Agent.Continuation.defaultWriterConfig's creative
--   @Temperature 0.9@) -- summarization wants a faithful, repeatable
--   compression, not creative variation.
chapterSummaryAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ previous compressed summary, or empty on a first pass
  -> Text  -- ^ new raw prose written since @previous@ was produced
  -> Sem r Text
chapterSummaryAgent previous newTail = do
  configsWithPrompt <- getConfigWithPrompt "agent.summarizer.chapter" defaultSummarizerSystemPrompt defaultSummarizerConfig
  Prompt extraInstructions <- getPrompt "agent.summarizer.chapter.instructions" defaultSummarizerInstructions

  let userMsg = summarizerUserMessage previous newTail extraInstructions

  info "chapterSummaryAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText userMsg]
  return $ mconcat [ t | AssistantText t <- response ]

-- | Fallback for @agent.summarizer.chapter@, used until an override is
--   committed to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultSummarizerSystemPrompt :: Prompt
defaultSummarizerSystemPrompt = Prompt $ T.unlines
  [ "You compress a story chapter into a dense, faithful summary."
  , "Preserve every plot-relevant fact, decision, and detail a later chapter"
  , "might need to reference. Cut description, pacing, and prose style."
  , "Output only the summary, nothing else."
  ]

-- | Low-temperature default: summarization wants a repeatable, faithful
--   compression, not creative variation -- see 'chapterSummaryAgent's
--   Haddock.
defaultSummarizerConfig :: [ModelConfig ProseModel]
defaultSummarizerConfig = [MaxTokens 1000, Temperature 0.2]

-- | Fallback for @agent.summarizer.chapter.instructions@: standing
--   instructions appended to every summarizer prompt. Empty by default --
--   same opt-in-override convention as
--   'Storyteller.Writer.Agent.Continuation.defaultWriterInstructions'.
defaultSummarizerInstructions :: Prompt
defaultSummarizerInstructions = ""

-- | Assemble the user-facing prompt directly, same reasoning as
--   'Storyteller.Writer.Agent.Continuation.writerUserMessage': fixed
--   section order/headers, no named-placeholder template to typo.
summarizerUserMessage :: Text -> Text -> Text -> Text
summarizerUserMessage previous newTail extraInstructions =
  mconcat
    [ previousSection
    , "New content written since then:\n\n" <> newTail <> "\n\n"
    , extraInstructionsSection
    , "Write the updated summary."
    ]
  where
    previousSection
      | T.null previous = "This chapter has no summary yet.\n\n"
      | otherwise        = "Existing summary:\n\n" <> previous <> "\n\n"

    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"
