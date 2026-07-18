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
-- files changed), 'chapterSummaryGenerate' is the effectful glue (read
-- each touched file's *current* content, call the model, assemble the
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

import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick, fromTick)
import Storyteller.Writer.Agent.Summarizer (withTrailingNewline)
import Storyteller.Writer.Agent.SummaryAccess (rawContent)
import Storyteller.Writer.Library (LibraryKind(..), classifyPath)

-- | Pure grouping: every 'Atom' tick among @candidates@ whose path
--   classifies as a prose 'Unit' (see 'Storyteller.Writer.Library.
--   classifyPath'), concatenated per path -- same shape as
--   'Server.Writer.Branch.passthroughGenerate', filtered to real chapter
--   files instead of every atom-touched path on the chain. No effects, so
--   this is the piece 'test.Storyteller.ChapterSummarizerSpec' pins
--   directly, per the project's "extract pure before wiring" convention.
--
--   'chapterSummaryGenerate' only ever reads this result's *keys* (which
--   paths changed at all, as a cheap trigger check) -- the concatenated
--   delta text itself is never fed to the model (see
--   'chapterSummaryGenerate's own Haddock for why); it's still pinned in
--   full because a wrong/missing path here is exactly as real a bug either
--   way -- this is still the one place "which paths count as touched" is
--   decided.
unitSummaryCandidates :: [Tick] -> Map FilePath Text
unitSummaryCandidates = List.foldl' step Map.empty
  where
    step acc t = case fromTick @Atom t of
      Just (Atom file _) | classifyPath file == Unit -> Map.insertWith (flip (<>)) file (contentFor file t) acc
      _                                               -> acc

-- | The @generate@ hook 'Storyteller.Writer.Agent.Summarizer.runSummarizer'
--   expects: for every touched chapter, read its *current, full* raw
--   content and ask 'chapterSummaryAgent' to compress it wholesale.
--
--   Deliberately does not fold a prior compression forward the way an
--   earlier version of this module did (@previous@ + @newTail@, asking the
--   model to fold new prose into its own last answer). That shape breaks a
--   real invariant a summary has to hold: calling @summarize@ once after a
--   whole chapter is written and calling it after every paragraph must
--   produce the same compression (modulo ordinary model randomness) --
--   summarizing is supposed to be a pure function of *current content*,
--   not of how many times or when it happened to be triggered. Folding a
--   prior AI-generated compression back in as input breaks that: each fold
--   is itself lossy, so repeated folding compounds drift a single clean
--   pass over the real source text never would (a game of telephone against
--   your own chapter). 'Storyteller.Writer.Agent.JournalSummarizer' never
--   had this problem in the first place -- a completed chunk is compressed
--   from its own raw span exactly once, ever, never re-fed through the
--   model -- which is the shape this now matches: always compress from raw
--   source, never from a previous summary.
--
--   'runSummarizer's own @ticksSinceLastSummary@ gate is what keeps this
--   cheap: a file with nothing new since the last pass of this kind never
--   reaches @generate@ at all, so re-deriving from full content on every
--   *real* trigger costs nothing extra for the common "nothing changed"
--   case -- the only case this trades away cheapness for is a very large,
--   already-summarized chapter picking up one small further edit, which
--   now re-sends the whole chapter instead of just the delta. Correctness
--   over that one optimization is the point.
chapterSummaryGenerate
  :: forall source r
  .  (LLMs r, Members '[BranchOp source, StoryStorage, Git, PromptStorage, Fail, Logging] r)
  => Text -> [Tick] -> Sem r (Map FilePath Text)
chapterSummaryGenerate _kind candidates =
  Map.fromList <$> mapM summarizeOne (Map.keys (unitSummaryCandidates candidates))
  where
    summarizeOne path = do
      content <- rawContent @source path
      compressed <- chapterSummaryAgent (fromMaybe "" content)
      return (path, compressed)

-- | Compress one chapter's current, full content wholesale -- see
--   'chapterSummaryGenerate's own Haddock for why this always
--   re-summarizes from raw source rather than folding a prior compression
--   forward.
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
  => Text  -- ^ the chapter's current, full raw content
  -> Sem r Text
chapterSummaryAgent content = do
  configsWithPrompt <- getConfigWithPrompt "agent.summarizer.chapter" defaultSummarizerSystemPrompt defaultSummarizerConfig
  Prompt extraInstructions <- getPrompt "agent.summarizer.chapter.instructions" defaultSummarizerInstructions

  let userMsg = summarizerUserMessage content extraInstructions

  info "chapterSummaryAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText userMsg]
  return $ withTrailingNewline $ mconcat [ t | AssistantText t <- response ]

-- | Fallback for @agent.summarizer.chapter@, used until an override is
--   committed to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultSummarizerSystemPrompt :: Prompt
defaultSummarizerSystemPrompt = Prompt $ T.unlines
  [ "You compress a story chapter into as short a summary as you can get"
  , "away with -- about a paragraph is the target, whatever a chapter's"
  , "own chain of real developments actually needs, never padded out to"
  , "reach it. This is not a synopsis for a human reader: once the story"
  , "grows too long to send in full, this summary is what future"
  , "generation calls will read instead of the original chapter text --"
  , "the writer, the characters, and any continuity check may only ever"
  , "see this version, never the prose it replaces. Every extra sentence"
  , "you write is a sentence a future call pays to read again."
  , ""
  , "Include only what changed the story: irreversible decisions, new"
  , "facts learned, relationships that shifted, promises made, things"
  , "broken or lost -- whatever a later chapter genuinely could not be"
  , "written without knowing. This is not a moment-to-moment recap:"
  , "skip beats that don't move anything forward, drop restating the same"
  , "development twice, and never pad toward a target length. If a"
  , "chapter's only real developments fit in one sentence, one sentence is"
  , "the correct output. Cut description, pacing, prose style, and dialogue"
  , "not load-bearing to a fact above. Output only the summary, nothing"
  , "else."
  ]

-- | Low-temperature default: summarization wants a repeatable, faithful
--   compression, not creative variation -- see 'chapterSummaryAgent's
--   Haddock.
--
--   'MaxTokens' sized the same way 'Storyteller.Writer.Agent.Roleplay''s
--   own configs were re-sized (see @test\/agent-integration\/FINDINGS.md@'s
--   "characterReflectAgent's ... MaxTokens were sized only for the visible
--   answer" entry): a reasoning-capable model's thinking budget
--   ('UniversalLLM.Providers.Anthropic.anthropicReasoning' is
--   @min 5000 (maxTokens \`div\` 2)@) comes out of this same total, so a
--   budget sized only for the compressed chapter's own length can leave
--   little or nothing for the answer once thinking is subtracted --
--   observed directly against a live model, not just theorized (a
--   summarize pass ran a long, visible @chat.preview.thinking@ stream
--   before finishing at an earlier, tighter value). 10000 rather than
--   something closer to the 5000 floor: a verbose reasoner can burn
--   through a smaller thinking allowance entirely and still have nothing
--   left for the answer, so this is sized to guarantee the full 5000-token
--   thinking cap is reachable *and* leaves an equal 5000 behind for the
--   answer, not just clear whatever floor happens to avoid an empty
--   response on a typical case.
defaultSummarizerConfig :: [ModelConfig ProseModel]
defaultSummarizerConfig = [MaxTokens 10000, Temperature 0.2]

-- | Fallback for @agent.summarizer.chapter.instructions@: standing
--   instructions appended to every summarizer prompt. Empty by default --
--   same opt-in-override convention as
--   'Storyteller.Writer.Agent.Continuation.defaultWriterInstructions'.
defaultSummarizerInstructions :: Prompt
defaultSummarizerInstructions = ""

-- | Assemble the user-facing prompt directly, same reasoning as
--   'Storyteller.Writer.Agent.Continuation.writerUserMessage': fixed
--   section order/headers, no named-placeholder template to typo.
summarizerUserMessage :: Text -> Text -> Text
summarizerUserMessage content extraInstructions =
  mconcat
    [ "Chapter content:\n\n" <> content <> "\n\n"
    , extraInstructionsSection
    , "Write the summary."
    ]
  where
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"
