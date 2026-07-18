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
-- Every tier writes to the exact same path (@journal.md@) inside its own
-- kind's alternate chain, exactly like
-- 'Storyteller.Writer.Agent.ChapterSummarizer' -- a @Summary@ tick points
-- at an alt-tree; the alt-tree's data is the latest summarized version of
-- the file, at that point; that's the entire read-side contract, and nothing
-- here adds to it -- 'Storyteller.Writer.Agent.SummaryAccess' reads any
-- tier back with zero tier-specific code. What's different from chapter\/
-- lore is only how a tier's *write* side decides what "current content"
-- covers, since @journal.md@ (or a lower tier's own alt-content) is too
-- large to ever resend in full:
--
--   * A tier's own alt-content is *cumulative* -- each pass appends a
--     freshly-compressed chunk onto whatever was already there, verbatim
--     (no LLM refold of the existing text -- seeMhis is what keeps the
--     result idempotent: run twice with nothing new in between and
--     nothing changes; delete every @Summary@ tick and rerun and you get
--     the same tree back, chunk boundaries and all, because a boundary is
--     never a function of *when* @summarize@ ran, only of how many raw
--     (or child-tier) items exist between the previous same-kind tick and
--     HEAD.
--
--   * A tier's own @Summary@ tick lands exactly where it was generated --
--     right after the last item it actually consumed -- never pinned to
--     whatever HEAD happens to be. 'Storyteller.Core.Git.foldAscend' is
--     what makes that true: it descends to the previous same-kind tick
--     (or root), then replays the tail back up one tick at a time,
--     letting this module's own step function insert a new @Summary@
--     tick mid-ascent the moment enough items have accumulated -- so any
--     leftover past that point simply stays where it is, structurally
--     later in history, and shows up as this kind's own candidates again
--     next time with no bookkeeping of any kind.
--
--   * Recursion is the same algorithm applied one tier up, not a special
--     case: tier @k@'s own "one item" is either a raw journal atom
--     (@k == 0@) or exactly the *new* text tier @k-1@'s own cumulative
--     alt-content grew by at its own most recent tick (a plain prefix
--     strip, since that growth is always an append) -- once tier @k@
--     writes anything, tier @k+1@ gets one attempt at its own threshold,
--     and so on until a tier's own pass produces nothing, which is exactly
--     "not enough material yet," the natural place for the recursion to
--     stop.
--
-- One real caveat, not a bug: if the tick chain itself is later edited
-- (an atom deleted or inserted retroactively), this idempotency guarantee
-- no longer holds exactly -- but at that point the existing summaries are
-- either already wrong (the edit changed something a summary depended on)
-- or the edit was minor enough that the boundary drift doesn't matter.
module Storyteller.Writer.Agent.JournalSummarizer
  ( journalKindFor
  , journalGrowth
  , journalSummarize
  , journalChunkAgent
  , currentSheet
  , defaultJournalGroupSize
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe)
import Control.Monad (void, when)
import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import qualified Storage.FS as FS
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary (Summary(..), lastSummaryOf, summaryContent)
import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, runStorage, foldAscend)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick, TickId, fromTick)
import Storyteller.Writer.Agent.Summarizer (extendAltChain, withTrailingNewline)
import Storyteller.Writer.Library (journalPath)

-- | Every tier groups in batches of 10 -- "10 atoms" for tier 0, "10
--   tier-0 chunks" for tier 1, and so on -- the one number the user's own
--   description of this feature named twice.
defaultJournalGroupSize :: Int
defaultJournalGroupSize = 10

-- | Tier @n@'s own 'Storyteller.Common.Summary.summaryKind' tag. Purely
--   internal bookkeeping -- nothing outside this module (and
--   'Storyteller.Writer.Agent.SummaryAccess', which only ever takes a
--   plain @kind@ list to read from) needs to construct or parse these.
journalKindFor :: Int -> Text
journalKindFor level = "journal/L" <> T.pack (show level)

-- | The per-tier fold state 'journalSummarize' threads through
--   'foldAscend': @caBuffer@ is items collected since this tier's last
--   write (raw atom text at tier 0, tier-@(n-1)@'s own newly-grown text at
--   tier @n@), @caAltHead@ is this tier's own alt-chain head as of its
--   last write (seeded from whatever this tier's most recent @Summary@
--   tick already held, if any -- this tier's own *cumulative content* is
--   never carried here as a value, only ever read back on demand via
--   'summaryContent', since every write is now a real 'Storage.Ops.addAtom'
--   append and the alt chain itself already accumulates it), @caChildSeen@
--   is only meaningful at tier @n > 0@ -- the child tier's cumulative
--   content as of the last child tick this fold has already consumed, so
--   the next one's *growth* (not its whole, ever-larger blob) is what gets
--   buffered -- and @caWrote@ records whether this pass wrote anything at
--   all, the signal 'journalSummarize' uses to decide whether the next
--   tier up gets its own attempt.
data ChunkAcc = ChunkAcc
  { caBuffer    :: [Text]
  , caAltHead   :: Maybe TickId
  , caChildSeen :: Text
  , caWrote     :: Bool
  }

-- | Run tier @level@'s summarization pass, then -- if it wrote anything --
--   give tier @level + 1@ its own attempt, recursively, until a tier's own
--   pass produces nothing (not enough material yet at that tier, which by
--   construction means nothing higher could have new material either).
--   Returns whether *this* tier wrote anything, so a caller triggering a
--   single tier directly (mostly for tests) can tell.
--
--   'Storyteller.Core.Git.foldAscend' does the actual work: descend to
--   tier @level@'s own previous @Summary@ tick (or root, on a first pass),
--   then replay the tail back up, handing every tick to this module's own
--   step function, which buffers raw items and, on crossing
--   'defaultJournalGroupSize', compresses them with one LLM call, extends
--   this tier's alternate chain by one commit (previous cumulative content
--   plus the newly compressed chunk, appended -- never an LLM refold of
--   what was already there), and writes a new @Summary@ tick right where
--   the fold currently stands -- exactly after the last item it consumed,
--   never pinned to real HEAD.
--
--   Takes the compression step as a parameter, same "no agent's real
--   'queryLLM' call is unit tested" convention as
--   'Storyteller.Writer.Agent.Tasks.syncTasksWith'\/'suggestTasksWith'
--   (see @test.Storyteller.TasksSpec@'s own Haddock) -- production passes
--   'journalChunkAgent', a test passes a pure stub, and the recursive
--   walk\/chunk-boundary\/idempotency machinery this function actually
--   owns is exercised without needing any real LLM effect at all.
journalSummarize
  :: forall source r
  .  Members '[BranchOp source, StoryStorage, Git, Fail] r
  => ([Text] -> Sem r Text)  -- ^ compress one full group, oldest first
  -> Int -> Sem r Bool
journalSummarize compress level = do
  let kind = journalKindFor level
  mSelf <- runStorage @source (lastSummaryOf kind)
  selfCumulative <- case mSelf of
    Nothing     -> return ""
    Just (_, s) -> runStorage @source (fromMaybe "" <$> summaryContent s journalPath)
  let target  = fst <$> mSelf
      initAcc = ChunkAcc
        { caBuffer    = []
        , caAltHead   = summaryAltHead . snd <$> mSelf
        , caChildSeen = selfCumulative
        , caWrote     = False
        }
  final <- foldAscend @source target initAcc (step kind)
  when (caWrote final) (void (journalSummarize @source compress (level + 1)))
  return (caWrote final)
  where
    -- | One tick, as 'foldAscend' replays it: at tier 0, only raw atoms on
    --   @journal.md@ itself count as an item; at tier @n > 0@, only
    --   @Summary@ ticks of tier @n - 1@'s own kind do, contributing
    --   whatever text that tick's own alt-content grew by since the last
    --   tier-@(n-1)@ tick this fold has already seen (see 'journalGrowth').
    --   Everything else (notes, other files' atoms, unrelated ticks
    --   interleaved on the same branch) passes through untouched.
    step :: Text -> ChunkAcc -> Tick -> Sem r ChunkAcc
    step kind acc t
      | level == 0 = case fromTick @Atom t of
          Just (Atom f _) | f == journalPath -> considerItem kind acc (contentFor journalPath t)
          _ -> return acc
      | otherwise = case fromTick @Summary t of
          Just s | summaryKind s == journalKindFor (level - 1) -> do
            childContent <- runStorage @source (fromMaybe "" <$> summaryContent s journalPath)
            considerItem kind (acc { caChildSeen = childContent }) (journalGrowth (caChildSeen acc) childContent)
          _ -> return acc

    -- | Once a full group accumulates, commit it as a real 'Storage.Ops.addAtom'
    --   append onto this tier's own alt-chain lifetime for @journalPath@ --
    --   not a whole-file blob replace -- so the alt chain gains genuine
    --   per-group Atom\/Tick history of its own, the same vocabulary a
    --   normal branch's own file history is written in. Safe against
    --   'Storyteller.Writer.Agent.Summarizer.runSummarizer's own "never
    --   split one pass across more than one alt-chain commit" invariant
    --   (see that module's Haddock): unlike a chapter/lore pass, which can
    --   touch several files in one call, a journal tier only ever writes
    --   @journalPath@, so one group is still always exactly one commit.
    considerItem :: Text -> ChunkAcc -> Text -> Sem r ChunkAcc
    considerItem kind acc item = do
      let buffer' = caBuffer acc ++ [item]
      if length buffer' < defaultJournalGroupSize
        then return acc { caBuffer = buffer' }
        else do
          compressed <- compress buffer'
          (_, newAltHead) <- extendAltChain (caAltHead acc) (Ops.addAtom journalPath compressed)
          _ <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
          return acc { caBuffer = [], caAltHead = Just newAltHead, caWrote = True }

-- | The text a tier's own cumulative alt-content grew by, given what it
--   held at the last child tick this fold has already consumed
--   (@seen@) and what it holds now (@now@) -- always exactly the
--   trailing append, since a tier's alt-content is only ever grown by
--   appending, never rewritten (see the module Haddock's idempotency
--   argument). Falls back to the whole of @now@ if @seen@ somehow isn't a
--   prefix of it -- shouldn't happen given that invariant, but a
--   nonsensical mismatch is still better read as "everything is new" than
--   silently losing content.
journalGrowth :: Text -> Text -> Text
journalGrowth seen now = fromMaybe now (T.stripPrefix seen now)

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
  .  Members '[BranchOp source, StoryStorage] r
  => Sem r Text
currentSheet = runStorage @source $ do
  files <- FS.list
  if "sheet.md" `elem` files
    then TE.decodeUtf8 <$> FS.readFile "sheet.md"
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
