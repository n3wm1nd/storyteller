{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The third per-domain summarizer, and the one genuinely hierarchical
-- one: @journal.md@ is append-only and unbounded, so instead of one
-- growing compression (what 'Storyteller.Writer.Agent.ChapterSummarizer'
-- and 'Storyteller.Writer.Agent.LoreSummarizer' both do -- always
-- recompress current content wholesale), this groups raw entries into
-- fixed-size chunks, compresses each chunk once, and -- once enough
-- chunks accumulate -- compresses /those/ into a coarser tier, the same
-- way again, recursively, for as many tiers as the content warrants (a
-- @log_N@-depth tree of summaries, not two hardcoded levels).
--
-- __None of that is in this module.__ It is
-- 'Storyteller.Writer.Agent.Summarizer.TieredSummary', and the whole of
-- what this module supplies is the reduce step: what @journal.md@ is, how
-- big a group should be, and a prompt for turning ten entries into one
-- paragraph. Where a group boundary falls, where each 'Summary' tick
-- lands, how one tier's output becomes the next tier's input, when to
-- stop recursing -- all of that is piping, and it lives in that effect's
-- interpreter, next to the storage effects it needs.
--
-- This module used to own that walk, which meant it carried @Git@ and
-- @StoryStorage@ in its own signatures for a chain it had no business
-- naming, and passed those on to everything that summarized a journal.
-- What was left once the piping moved out is what an agent actually is
-- here: a prompt, a couple of constants, and two calls.
--
-- One real caveat inherited from the machinery, not a bug: if the tick
-- chain itself is later edited (an atom deleted or inserted
-- retroactively), the idempotency guarantee ("run twice with nothing new
-- in between and nothing changes") no longer holds exactly -- but at that
-- point the existing summaries are either already wrong (the edit changed
-- something a summary depended on) or the edit was minor enough that the
-- boundary drift doesn't matter.
module Storyteller.Writer.Agent.JournalSummarizer
  ( journalKind
  , journalSummarize
  , journalCreateManual
  , journalChunkAgent
  , currentSheet
  , defaultJournalGroupSize
  ) where

import Prelude hiding (readFile)

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, listAllFiles, readFile)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Writer.Agent.Summarizer (Summarization, tieredSummary, withTrailingNewline)
import Storyteller.Writer.Library (journalPath)

-- | Every tier groups in batches of 10 -- "10 atoms" for tier 0, "10
--   tier-0 chunks" for tier 1, and so on -- the one number the user's own
--   description of this feature named twice.
defaultJournalGroupSize :: Int
defaultJournalGroupSize = 10

-- | The one plain kind every tier of this recursive family shares -- see
--   the module Haddock for why there's no per-tier suffix.
journalKind :: Text
journalKind = "journal"

-- | Compress @journal.md@ in tiers, using @compress@ to reduce each full
--   group. Everything else -- where a group boundary falls, where each
--   tick lands, how one tier's output becomes the next tier's input --
--   belongs to 'Storyteller.Writer.Agent.Summarizer.TieredSummary's
--   interpreter, not here. 'True' if this call wrote anything at tier 0.
--
--   Takes the compression step as a parameter, same "no agent's real
--   'Runix.LLM.queryLLM' call is unit tested" convention as
--   'Storyteller.Writer.Agent.Tasks.syncTasksWith'\/'suggestTasksWith'
--   (see @test.Storyteller.TasksSpec@'s own Haddock) -- production passes
--   'journalChunkAgent', a test passes a pure stub.
journalSummarize
  :: Member Summarization r
  => ([Text] -> Sem r Text)  -- ^ compress one full group, oldest first
  -> Sem r Bool
journalSummarize =
  tieredSummary journalKind journalPath defaultJournalGroupSize False

-- | The manual-creation entry point: force tier 0's current pass to close
--   out right now, compressing whatever's pending -- however many raw
--   items, possibly fewer than 'defaultJournalGroupSize' -- into one new
--   chunk with empty content, for the user to write into directly. Same
--   coverage-finding machinery as 'journalSummarize', so it lands exactly
--   where an automatic pass would have; the only difference is *when* a
--   chunk boundary closes.
--
--   Deeper tiers are never forced (see 'TieredSummary'), so a manual
--   tier-0 entry contributes one more ordinary item to tier 1's count and
--   nothing more.
journalCreateManual :: Member Summarization r => Sem r Bool
journalCreateManual =
  tieredSummary journalKind journalPath defaultJournalGroupSize True (const (pure ""))

-- | Compress one full group of raw items (raw journal entries at tier 0,
--   a lower tier's own newly-grown text at tier @n > 0@) into one dense
--   paragraph. No @previous@ argument -- each group is compressed fresh,
--   from exactly its own span, since 'journalSummarize' already guarantees
--   a group is only ever built once, from items no earlier group has seen
--   (see the module Haddock's idempotency argument).
--
--   Same fixed-system-prompt discipline as
--   'Storyteller.Writer.Agent.LoreSummarizer.loreSummaryAgent': the system
--   prompt/config carries no per-call content, so a provider's cache can
--   hit across every group compressed in one pass (a backlog completing
--   several groups in one 'journalSummarize' call is exactly the case this
--   protects), and across tiers, and across passes.
journalChunkAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text    -- ^ character sheet content, fixed for the whole pass -- see 'sheetTurns'
  -> [Text]  -- ^ one full group's items, oldest first
  -> Sem r Text
journalChunkAgent sheet items = do
  configsWithPrompt <- getConfigWithPrompt "agent.summarizer.journal" defaultSystemPrompt defaultConfig
  Prompt extraInstructions <- getPrompt "agent.summarizer.journal.instructions" defaultInstructions

  let messages = sheetTurns sheet ++ [UserText (groupUserMessage items extraInstructions)]

  info "journalChunkAgent: querying model..."
  response <- queryLLM configsWithPrompt messages
  return $ withTrailingNewline $ mconcat [ t | AssistantText t <- response ]

defaultSystemPrompt :: Prompt
defaultSystemPrompt = Prompt $ T.unlines
  [ "You compress a run of consecutive journal entries (or, if already"
  , "compressed once, a run of consecutive summaries) into 2-3 sentences"
  , "at most -- a whole group is usually around ten paragraphs of raw"
  , "entries, and almost none of that is worth a sentence of its own."
  , "This is not a recap for a human reader: the journal is this"
  , "character's own private continuity, and once it grows too long to"
  , "send in full, this is what the character's own future reasoning and"
  , "reflection will draw on instead of the entries it replaces -- they"
  , "are gone from the character's effective memory the moment you"
  , "compress them. Every extra sentence you write is a sentence a future"
  , "call pays to read again."
  , ""
  , "Don't assume the entries are written in this character's own voice or"
  , "from their own point of view -- some journals are kept in first"
  , "person, but others are third-person narration, another character's"
  , "observations, or a scene told from someone else's perspective that"
  , "this character merely appears in. Compress for what this character"
  , "would come away remembering either way, not for how the entry itself"
  , "was narrated."
  , ""
  , "Include only what actually changed this character going forward:"
  , "decisions made, relationships that shifted, facts learned, feelings"
  , "that genuinely turned. This is not a moment-to-moment recap of what"
  , "happened in each entry -- skip beats that didn't change anything,"
  , "collapse entries that all restate the same realization into it once,"
  , "and never pad toward a target length. If ten entries only really add"
  , "up to one lasting change, one short sentence is the correct output."
  , "Output only the compressed text, nothing else."
  ]

-- | Sized for a reasoning model's thinking budget, not just the paragraph
--   itself -- see 'Storyteller.Writer.Agent.ChapterSummarizer.
--   defaultSummarizerConfig's own Haddock for the live-model finding
--   ('MaxTokens' is shared with a model's thinking budget, @min 5000
--   (maxTokens \`div\` 2)@, so a budget sized only for the short visible
--   output can leave nothing for the answer once thinking is subtracted).
defaultConfig :: [ModelConfig ProseModel]
defaultConfig = [MaxTokens 10000, Temperature 0.2]

defaultInstructions :: Prompt
defaultInstructions = ""

-- | @source@'s current @sheet.md@, or @""@ if it doesn't have one yet.
--   Read once per 'journalSummarize' call (never per chunk) and passed
--   into 'journalChunkAgent' already curried, so every chunk this pass
--   compresses for this character sees the exact same sheet text -- see
--   'sheetTurns' for why that fixed-ness is what makes it cacheable at all.
currentSheet
  :: forall source r
  .  Members '[FileSystem (BranchTag source), FileSystemRead (BranchTag source), Fail] r
  => Sem r Text
currentSheet = do
  files <- listAllFiles @(BranchTag source) "/"
  if "sheet.md" `elem` files
    then TE.decodeUtf8 <$> readFile @(BranchTag source) "sheet.md"
    else return ""

-- | The character sheet as its own fixed user\/assistant turn pair, ahead
--   of the per-call entries turn -- deliberately never folded into the same
--   'UserText' as the entries, and never into 'defaultSystemPrompt' either
--   (per-character content can't live in a @PromptStorage@ default shared
--   across every character). A cacheable prefix has to end on an actual
--   message boundary: concatenated into one string, a tokenizer can
--   retokenize the last few tokens of the "fixed" part differently
--   depending on whatever text follows it, so two calls with the same
--   sheet but different entries wouldn't actually share a token-identical
--   prefix even though the characters happen to match. Ending the sheet on
--   its own turn pins that boundary regardless of what comes after.
--
--   The 'AssistantText' reply is a fixed constant, not a real model turn --
--   it exists only so the sheet's own 'UserText' isn't immediately
--   followed by the entries' 'UserText' (some providers silently collapse
--   two adjacent same-role turns into one, which would undo the whole
--   point of giving the sheet its own boundary).
--
--   Produces no turns at all when @sheet@ is empty, so a character with no
--   @sheet.md@ yet gets exactly what this agent always sent before.
sheetTurns :: Text -> [Message m]
sheetTurns sheet
  | T.null sheet = []
  | otherwise =
      [ UserText $ mconcat
          [ "This character's current sheet, for context on what matters to"
          , " them when judging what's worth keeping in later compressions:"
          , "\n\n"
          , sheet
          ]
      , AssistantText
          "Understood -- I'll keep that in mind when compressing this character's journal entries."
      ]

groupUserMessage :: [Text] -> Text -> Text
groupUserMessage items extraInstructions =
  mconcat
    [ "Entries to compress into one dense paragraph:\n\n"
    , T.intercalate "\n\n---\n\n" items
    , "\n\n"
    , extraInstructionsSection
    , "Write the compressed paragraph."
    ]
  where
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"
