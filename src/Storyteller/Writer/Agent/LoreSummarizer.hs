{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The second per-domain summarizer: compresses a lore/codex article into
-- a short, factual description. Plugs into
-- 'Storyteller.Writer.Agent.Summarizer.runSummarizer' as the
-- @"lore/article"@ kind's @generate@ hook, same slot
-- 'Storyteller.Writer.Agent.ChapterSummarizer' fills for @"prose/chapter"@ --
-- deliberately the same three-way split ('loreSummaryCandidates' pure,
-- 'loreSummaryGenerate' effectful glue, 'loreSummaryAgent' the one LLM
-- call) and the same "always compress current content wholesale, never
-- fold a prior compression forward" shape (see
-- 'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate's own
-- Haddock for why that matters), differing only in which paths qualify
-- ('Storyteller.Writer.Lore.isLoreEligible' instead of
-- 'Storyteller.Writer.Library.classifyPath' @== Unit@) and in what kind of
-- compression is wanted (a short codex blurb, not condensed prose).
module Storyteller.Writer.Agent.LoreSummarizer
  ( loreSummaryCandidates
  , loreSummaryGenerate
  , loreSummaryAgent
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

import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick, fromTick)
import Storyteller.Writer.Agent.Summarizer (withTrailingNewline)
import Storyteller.Writer.Agent.SummaryAccess (rawContent)
import Storyteller.Writer.Lore (isLoreEligible)

-- | Pure grouping: every 'Atom' tick among @candidates@ whose path is
--   codex-eligible (see 'Storyteller.Writer.Lore.isLoreEligible'),
--   concatenated per path -- same shape as
--   'Storyteller.Writer.Agent.ChapterSummarizer.unitSummaryCandidates'.
--   Only the result's *keys* matter to 'loreSummaryGenerate' (which paths
--   are touched at all); see that function's own Haddock for why the
--   concatenated delta text itself is never fed to the model.
loreSummaryCandidates :: [Tick] -> Map FilePath Text
loreSummaryCandidates = List.foldl' step Map.empty
  where
    step acc t = case fromTick @Atom t of
      Just (Atom file _) | isLoreEligible file -> Map.insertWith (flip (<>)) file (contentFor file t) acc
      _                                          -> acc

-- | The @generate@ hook 'Storyteller.Writer.Agent.Summarizer.runSummarizer'
--   expects: for every touched lore article, read its *current, full* raw
--   content and ask 'loreSummaryAgent' to compress it wholesale -- same
--   one-call-per-file, one-commit-per-pass shape as
--   'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate',
--   and the same reason for reading current content instead of folding a
--   prior blurb forward: a summary has to be a pure function of current
--   content, not of when or how often @summarize@ was triggered -- see
--   that function's own Haddock for the full argument.
loreSummaryGenerate
  :: forall source r
  .  (LLMs r, Members '[BranchOp source, StoryStorage, Git, PromptStorage, Fail, Logging] r)
  => Text -> [Tick] -> Sem r (Map FilePath Text)
loreSummaryGenerate _kind candidates =
  Map.fromList <$> mapM summarizeOne (Map.keys (loreSummaryCandidates candidates))
  where
    summarizeOne path = do
      content <- rawContent @source path
      compressed <- loreSummaryAgent (fromMaybe "" content)
      return (path, compressed)

-- | Compress one lore article's current, full content wholesale into a
--   short, factual description -- a codex blurb, not condensed prose:
--   what 'chapterSummaryAgent' does for a chapter, aimed at a much shorter
--   target. See 'loreSummaryGenerate's own Haddock for why this always
--   re-summarizes from raw source rather than folding a prior blurb
--   forward.
--
--   Its own 'PromptStorage' key (@agent.summarizer.lore@) and its own
--   'MaxTokens' default -- but the same low-temperature, faithful-not-
--   creative default the whole summarizer family shares.
--
--   The system prompt/config here is fetched once, as fixed default text --
--   no per-call path or content is ever spliced into it -- so a provider's
--   prompt cache can hit across every article this kind touches in a pass,
--   and across passes; only 'summarizerUserMessage's own trailing user turn
--   ever carries this call's actual content (see
--   'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryAgent's own
--   Haddock for the general shape this follows).
loreSummaryAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ the article's current, full raw content
  -> Sem r Text
loreSummaryAgent content = do
  configsWithPrompt <- getConfigWithPrompt "agent.summarizer.lore" defaultSummarizerSystemPrompt defaultSummarizerConfig
  Prompt extraInstructions <- getPrompt "agent.summarizer.lore.instructions" defaultSummarizerInstructions

  let userMsg = summarizerUserMessage content extraInstructions

  info "loreSummaryAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText userMsg]
  return $ withTrailingNewline $ mconcat [ t | AssistantText t <- response ]

-- | Fallback for @agent.summarizer.lore@, used until an override is
--   committed to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultSummarizerSystemPrompt :: Prompt
defaultSummarizerSystemPrompt = Prompt $ T.unlines
  [ "You compress a lore/codex article into as short a factual"
  , "description as you can get away with. This is not a blurb for a"
  , "human browsing a codex: once the full article is too costly to send"
  , "with every generation call, this description is what future scenes"
  , "and character agents will read instead of the article itself --"
  , "they may never see the original again. Every extra sentence you"
  , "write is a sentence a future call pays to read again."
  , ""
  , "Include only concrete facts a later scene could actually need --"
  , "names, relationships, defining traits, history that still matters"
  , "now -- exactly as if the article had been deleted after you wrote"
  , "this. Drop flavor text, atmosphere, and anything that only restates"
  , "another fact in different words. A single sentence is the correct"
  , "output when that's all the article is really carrying; never pad"
  , "toward a target length. Output only the description, nothing else."
  ]

-- | The *visible* answer is blurb-sized -- an article's compressed
--   description has no business running as long as a compressed chapter
--   does -- but 'MaxTokens' itself still has to cover a reasoning model's
--   thinking budget too (@min 5000 (maxTokens \`div\` 2)@, see
--   'Storyteller.Writer.Agent.ChapterSummarizer.defaultSummarizerConfig's
--   own Haddock for the live-model finding this mirrors), so this is sized
--   the same as that config despite the short output.
defaultSummarizerConfig :: [ModelConfig ProseModel]
defaultSummarizerConfig = [MaxTokens 10000, Temperature 0.2]

-- | Fallback for @agent.summarizer.lore.instructions@: standing
--   instructions appended to every lore summarizer prompt. Empty by
--   default -- same opt-in-override convention as the chapter summarizer's.
defaultSummarizerInstructions :: Prompt
defaultSummarizerInstructions = ""

-- | Assemble the user-facing prompt directly, same shape as
--   'Storyteller.Writer.Agent.ChapterSummarizer.summarizerUserMessage'.
summarizerUserMessage :: Text -> Text -> Text
summarizerUserMessage content extraInstructions =
  mconcat
    [ "Article content:\n\n" <> content <> "\n\n"
    , extraInstructionsSection
    , "Write the description."
    ]
  where
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"
