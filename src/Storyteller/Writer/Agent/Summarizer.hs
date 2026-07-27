{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Generic summarizer machinery: given a @source@ branch scope,
--   'runSummarizer' finds whatever's new since the last summary of a
--   given kind, hands it to a per-domain @generate@ hook, and records the
--   result as a new 'Storyteller.Common.Summary.Summary' tick on
--   @source@. Per-domain summarizers (prose, character, lore, ...) only
--   ever need to supply @generate@ -- everything about finding the range,
--   extending the alternate chain, and recording the tick lives here,
--   once, generically.
--
--   'runSummarizer'\/'runSummarizerForPath' are, from a caller's own
--   point of view, just two ordinary calls: "what's pending for this
--   kind" and "record this as the new pass" (or the per-path pair of the
--   same two questions). All the alt-chain structure -- seeding, per-file
--   commit shape, where the new 'Summary' tick gets positioned -- lives
--   in 'extendAltChain'\/'recordSummary' below; a per-domain summarizer
--   supplying @generate@ never sees an alt-chain hash or a
--   'Storage.Core.StoreT' action -- only candidate ticks in, generated
--   text out.
--
--   The row is @Members '[BranchOp source, Branches, Fail]@ and stops
--   there. 'Branches' is what an alternate chain actually costs: it is a
--   commit chain with no ref, so extending one means opening a scope at a
--   /position/ ('Storyteller.Core.Branch.withBranchAt') rather than at a
--   name. Nothing here needs @Git@ or @StoryStorage@ -- those are
--   'Branches'\'s own interpreter's business, not this module's, and a
--   backend that represents "a summary of this kind" some other way
--   supplies its own 'Branches' interpreter with nothing above this line
--   changing.
--
--   This was briefly a @Summarization@ effect instead, when 'BranchOp'
--   could only open a scope by name and there was no honest way to
--   express "extend this chain from that hash." 'withBranchAt' closed
--   that gap, and once it did the effect was a GADT wrapping one
--   'runStorage' call per constructor, discharged at every call site by
--   callers who already held everything its interpreter needed. See
--   EFFECTS.md -- this module is the worked example of when that trade
--   goes which way.
--
--   There is no @alt@ branch parameter on the public API: an alternate
--   chain is never a real, named branch (see "Storyteller.Common.Summary"'s
--   module Haddock for why) -- it's extended by hash, anchored either at
--   the previous 'Storyteller.Common.Summary.summaryAltHead' of the same
--   kind, or at a fixed bootstrap commit on the very first pass. A
--   hierarchical summarizer (a book-tier summarizer whose @generate@
--   reads a chapter-tier alternate chain's own content as its input)
--   still calls this with @source@ set to the *same* real branch every
--   other tier uses -- only the @kind@ differs.
module Storyteller.Writer.Agent.Summarizer
  ( -- * Producing summaries
    runSummarizer
  , runSummarizerForPath
  , tieredPass

    -- * The questions a summarizer asks
  , pendingSummary
  , recordSummary
  , pendingPathSummary
  , recordPathSummary

    -- * Reading them back
  , summaryLadder
  , summaryOccurrences
  , densest
  , densestWithin
  , withinBudget

  , withTrailingNewline
  ) where

import Prelude hiding (writeFile)

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storage.Query (lifetimeAtoms)
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary
  (Occurrence, Summary(..), bootstrapAltHead, lastSummaryOf, lastSummaryTouching, ticksSinceLastSummary)
import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Branch (Branches, withBranchAt)
import Storyteller.Core.Git (BranchOp, atGeneric, foldAscend, runStorage)
import Storyteller.Core.Types (Tick(..), TickId(..), fromTick)
import Storyteller.Writer.Agent.SummaryAccess (rawContent, unsummarizedTailSince)
import qualified Storyteller.Writer.Agent.SummaryAccess as SummaryAccess

-- | Extend an alternate chain by one commit: enter the chain at @mPrev@
--   (the previous 'summaryAltHead' of this kind, if any) or at a freshly
--   minted empty root on the very first pass, run @action@ there, and hand
--   back its result plus the new head. @chain@ is a fresh phantom every
--   call, never the caller's own already-open branch scope -- unlike
--   'extendNestedAltChain', which deliberately reuses one.
--
--   No branch is opened, created or named: the returned position is only
--   ever reachable through whatever 'Summary' tick records it next, which
--   is why 'Storyteller.Core.Branch.withBranchAt' hands it back rather
--   than leaving it to be asked for.
--
--   @source@ is only ever the scope the bootstrap commit is written
--   through; a commit is content-addressed, so any open scope serves.
extendAltChain
  :: forall chain source r a
  .  Members '[BranchOp source, Branches, Fail] r
  => Maybe TickId
  -> Sem (BranchOp chain ': r) a
  -> Sem r (a, TickId)
extendAltChain mPrev action = do
  seed <- case mPrev of
    Just tid -> return tid
    Nothing  -> TickId . Core.unObjectHash <$> runStorage @source (lift bootstrapAltHead)
  withBranchAt @chain seed action

-- | Give @tid@'s own alternate chain (currently at @seed@) one further,
--   nested attempt at whatever @inner@ does with it -- the write-side half
--   of a hierarchical summarizer (see
--   "Storyteller.Writer.Agent.JournalSummarizer"): @inner@ runs against a
--   *freshly opened* 'BranchOp' @chain@ scope seeded at @seed@, reusing
--   the very same phantom tag @chain@ its caller is already inside
--   (Polysemy's 'interpret'-based dispatch means this nested interpreter
--   shadows the outer one correctly within @inner@ -- the same shadowing
--   'Storyteller.Core.Git.atGeneric'\/'Storyteller.Core.Git.foldAscend'
--   already rely on for scopes nested within a replay).
--
--   Re-mints @tid@ exactly once, at the end, and only if the head actually
--   moved. A position-anchored scope publishes nothing as it advances
--   ('Storyteller.Core.Branch.withBranchAt'), which is what makes that
--   possible and is the behaviour this needs: @inner@ is typically a
--   'Storyteller.Core.Git.foldAscend'-driven call, which descends by
--   repeatedly moving this very scope's head *backward* (via
--   'Storage.Core.drop') before replaying forward again. Reacting to each
--   of those intermediate moves would re-mint @tid@ mid-descent, against a
--   head that isn't the settled result yet, and can cascade without ever
--   converging.
--
--   Re-minting at all is not optional: an alternate chain has no ref, so
--   the only thing keeping its commits reachable is a 'Summary' tick
--   naming the tip (see "Storyteller.Common.Summary"). Without this,
--   whatever @inner@ wrote becomes unreachable the instant this returns.
extendNestedAltChain
  :: forall chain r a
  .  Members '[BranchOp chain, Branches] r
  => Text    -- ^ kind to re-mint @tid@ under -- unchanged across nesting depth
  -> TickId  -- ^ tid: the Summary tick whose own alternate chain is being extended
  -> TickId  -- ^ seed: tid's own summaryAltHead, i.e. that chain's current tip
  -> Sem (BranchOp chain : r) a
  -> Sem r a
extendNestedAltChain kind tid seed inner = do
  (result, finalHead) <- withBranchAt @chain seed inner
  when (finalHead /= seed) $
    void (atGeneric @chain tid (runStorage @chain (Tick.storeAs (Summary kind finalHead))))
  return result

-- | Every per-domain summarizer's own LLM call should route its raw
--   output through this before returning: a summary is never the last
--   thing written to its path -- 'Storyteller.Writer.Agent.SummaryAccess.
--   completeContents' always appends whatever's unsummarized since (the
--   raw tail), and 'Storyteller.Writer.Agent.JournalSummarizer' appends
--   the *next* chunk directly onto this one's own cumulative content --
--   so a model response with no trailing newline runs straight into
--   whatever comes after it with no separator at all.
withTrailingNewline :: Text -> Text
withTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise             = t <> "\n"

-- ---------------------------------------------------------------------------
-- What a summarizer asks the alt chain for
-- ---------------------------------------------------------------------------
--
-- These four are the questions 'runSummarizer'\/'runSummarizerForPath'
-- actually ask -- "what's pending" and "record this pass", at two
-- granularities -- rather than the chain primitives they happen to be
-- answered with. They are ordinary functions over
-- 'Storyteller.Core.Branch.BranchOp' plus the ability to enter a
-- position-anchored scope ('Storyteller.Core.Branch.withBranchAt'), which
-- is all the alt chain ever needed.

-- | Every real tick since @kind@'s last pass (or since root, if none yet),
--   or 'Nothing' if there's genuinely nothing new -- the candidate
--   material a fresh whole-branch pass should consider.
pendingSummary
  :: forall source r
  .  Member (BranchOp source) r
  => Text -> Sem r (Maybe [Tick])
pendingSummary kind = do
  candidates <- runStorage @source (ticksSinceLastSummary kind)
  return $ if null candidates then Nothing else Just candidates

-- | Record @content@ (one file per path) as @kind@'s new pass.
recordSummary
  :: forall source r
  .  Members '[BranchOp source, Branches, Fail] r
  => Text -> Map FilePath Text -> Sem r TickId
recordSummary kind files = do
  mPrev <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
  (_, newAltHead) <- extendAltChain @() @source mPrev $
    runStorage @() (mapM_ (uncurry setAltFileContent) (Map.toList files))
  newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
  return (TickId (Core.unObjectHash newHash))

-- | @path@'s current full content, if its existing @kind@ compression is
--   stale or missing -- 'Nothing' if it's already up to date, or @path@
--   isn't atom-tracked at all (nothing to summarize).
pendingPathSummary
  :: forall source r
  .  Members '[BranchOp source, Fail] r
  => Text -> FilePath -> Sem r (Maybe Text)
pendingPathSummary kind path = do
  mLast <- runStorage @source (lastSummaryTouching kind path)
  upToDate <- case mLast of
    Nothing     -> return False
    Just (_, s) -> T.null <$> unsummarizedTailSince @source s path
  if upToDate
    then return Nothing
    else do
      lifetime <- runStorage @source (lifetimeAtoms path)
      case lifetime of
        [] -> return Nothing  -- path isn't atom-tracked at all -- nothing to summarize
        _  -> Just . fromMaybe "" <$> rawContent @source path

-- | Record @content@ as @path@'s fresh @kind@ compression, correctly
--   positioned at @path@'s own last atom rather than wherever the
--   summarized branch's head happens to be (see 'runSummarizerForPath' for
--   why that positioning matters).
recordPathSummary
  :: forall source r
  .  Members '[BranchOp source, Branches, Fail] r
  => Text -> FilePath -> Text -> Sem r TickId
recordPathSummary kind path content = do
  lifetime <- runStorage @source (lifetimeAtoms path)
  case lifetime of
    [] -> fail ("recordPathSummary: " <> path <> " is not atom-tracked")
    _  -> do
      let (lastAtomHash, _) = last lifetime
      atGeneric @source (TickId (Core.unObjectHash lastAtomHash)) $ do
        mPrev <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
        (_, newAltHead) <- extendAltChain @() @source mPrev
                             (runStorage @() (setAltFileContent path content))
        newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
        return (TickId (Core.unObjectHash newHash))

-- | Commit @content@ as @path@'s current state in whichever alternate
--   chain 'extendAltChain' has anchored, as a real 'Storage.Ops.addAtom'
--   write -- or, if @path@ hasn't been written there before, seed it fresh
--   the same way ('Storage.Ops.saveFileAsNew' would fail otherwise, since
--   'Storage.Ops.deleteFile' assumes something to delete). Every
--   whole-file summarizer recomputes a file's compression from scratch
--   each pass (never folds a prior one forward -- see
--   'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate' for
--   why), so this replaces the file's prior alternate-chain lifetime
--   outright rather than appending onto it the way a tiered pass does for
--   its own, genuinely incremental, per-batch writes.
setAltFileContent :: Core.StoreM m => FilePath -> Text -> Core.StoreT m ()
setAltFileContent path content = do
  there <- Ops.exists path
  if there
    then Ops.saveFileAsNew path path content
    else void (Ops.addAtom path content)

-- | The per-tier fold state 'tieredPass' threads through 'foldAscend':
--   @caBuffer@ is entries collected since this tier's last write,
--   @caAltHead@ is this tier's own alt-chain head as of that write (seeded
--   from whatever this tier's most recent 'Summary' tick already held --
--   this tier's cumulative content is never carried here as a value, only
--   read back on demand, since every write is a real 'Storage.Ops.addAtom'
--   append and the alt chain already accumulates it), @caLastTick@ is the
--   most recently minted 'Summary' tick's id (which tick a nested attempt
--   has to re-point as its own chain grows deeper), and @caWrote@ records
--   whether this pass wrote anything -- the signal for whether a nested
--   attempt one tier up is worth making.
data TierAcc = TierAcc
  { caBuffer   :: [Text]
  , caAltHead  :: Maybe TickId
  , caLastTick :: Maybe TickId
  , caWrote    :: Bool
  }

-- | Compress @path@'s whole history in tiers: group its unconsumed entries
--   into batches of @size@, reduce each batch with @reduce@, and -- once a
--   tier has written a full batch -- apply the identical treatment to
--   /that/ tier's output, recursively, for as many tiers as the content
--   warrants. 'True' if anything was written at the tier this was called
--   on.
--
--   __The reduce is the entire contribution a caller makes.__ Everything
--   else -- where a batch boundary falls, where each tick lands in the
--   chain, how a tier's output becomes the next tier's input, when to stop
--   -- is piping, and lives here rather than in whichever agent wanted a
--   summary. A caller says "compress ten of these into one" and knows
--   nothing else; see "Storyteller.Writer.Agent.JournalSummarizer", which
--   is a prompt and two calls to this.
--
--   The reduce is an ordinary argument, so it can be a real
--   'Runix.LLM.queryLLM' agent in production and a pure stub in a test with
--   nothing else changing.
--
--   @force@ closes out a partial batch instead of waiting for a full one --
--   manual creation, where the point is to land an empty chunk exactly
--   where an automatic pass would have. Only ever applies to the tier this
--   is called on; nested tiers always run unforced.
--
--   Run this chain's own pass, then -- if it wrote anything -- give its own
--   alternate chain one further, nested attempt at the identical
--   algorithm, recursively, until a pass produces nothing (not enough
--   material yet, which by construction means nothing deeper could have
--   new material either). Returns whether /this/ chain's pass wrote.
--
--   'Storyteller.Core.Git.foldAscend' does the per-tier work: descend to
--   this chain's previous 'Summary' tick of @kind@ (or root, on a first
--   pass), then replay the tail back up, handing every tick to the step
--   below, which buffers entries and, on crossing @size@, reduces them,
--   extends this chain's alternate chain by one commit (an append -- never
--   a refold of what was already there), and writes a new 'Summary' tick
--   right where the fold currently stands: exactly after the last entry it
--   consumed, never pinned to real HEAD.
--
--   Depth is never named. Every tier shares one plain @kind@ -- no per-tier
--   suffix, no @level@ parameter anywhere -- because a tier /is/ just this
--   same function applied to a chain one level deeper, and "how far up" a
--   tick sits is exactly how many alternate chains you would open to reach
--   it ('Storyteller.Common.Summary.expandSummary' discovers that by
--   walking; nothing declares it).
--
tieredPass
  :: forall source r
  .  Members '[BranchOp source, Branches, Fail] r
  => Text                    -- ^ summary kind, shared by every tier
  -> FilePath                -- ^ the path being compressed, at every tier
  -> Int                     -- ^ entries per batch
  -> Bool                    -- ^ force a partial batch to close out
  -> ([Text] -> Sem r Text)  -- ^ reduce one batch, oldest first
  -> Sem r Bool
tieredPass kind path size force reduce = do
  mSelf <- runStorage @source (lastSummaryOf kind)
  let target  = fst <$> mSelf
      initAcc = TierAcc
        { caBuffer   = []
        , caAltHead  = summaryAltHead . snd <$> mSelf
        , caLastTick = fst <$> mSelf
        , caWrote    = False
        }
  walked <- foldAscend @source target initAcc step
  final <- if force && not (null (caBuffer walked))
             then commitBatch walked (caBuffer walked)
             else return walked
  when (caWrote final) $
    case (caLastTick final, caAltHead final) of
      (Just tid, Just altHead) ->
        void $ extendNestedAltChain @source kind tid altHead
          (tieredPass @source kind path size False (raise . reduce))
      _ -> return ()  -- unreachable: caWrote is only ever set alongside both fields
  return (caWrote final)
  where
    -- | One tick, as 'foldAscend' replays it: only atoms on @path@ count as
    --   an entry, at any depth -- everything else (notes, other files'
    --   atoms, an unrelated tick interleaved on the same chain) passes
    --   through untouched.
    step :: TierAcc -> Tick -> Sem r TierAcc
    step acc t = case fromTick @Atom t of
      Just (Atom f _) | f == path -> considerEntry acc (contentFor path t)
      _ -> return acc

    -- | Buffer @entry@; once a full batch accumulates, close it out. The
    --   forced path above closes out whatever's left the same way once the
    --   walk has finished, so both routes to a boundary go through the
    --   identical commit.
    considerEntry :: TierAcc -> Text -> Sem r TierAcc
    considerEntry acc entry = do
      let buffer' = caBuffer acc ++ [entry]
      if length buffer' < size
        then return acc { caBuffer = buffer' }
        else commitBatch acc buffer'

    -- | Commit @entries@' reduction as a real 'Storage.Ops.addAtom' append
    --   onto this chain's own alt-chain lifetime for @path@ -- not a
    --   whole-file blob replace -- so the alt chain gains genuine per-batch
    --   Atom\/Tick history of its own, the same vocabulary a normal
    --   branch's file history is written in. One batch is always exactly
    --   one commit, so 'runSummarizer's "never split one pass across more
    --   than one alt-chain commit" invariant holds here too.
    commitBatch :: TierAcc -> [Text] -> Sem r TierAcc
    commitBatch acc entries = do
        compressed <- reduce entries
        (_, newAltHead) <- extendAltChain @() @source (caAltHead acc)
                             (runStorage @() (Ops.addAtom path compressed))
        newTick <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
        return acc
          { caBuffer   = []
          , caAltHead  = Just newAltHead
          , caLastTick = Just (TickId (Core.unObjectHash newTick))
          , caWrote    = True
          }

-- | Run one summarization pass for @kind@: collect every tick on @source@
--   since @kind@'s last summary there (or since root, if none yet),
--   hand them to @generate@, extend the alternate chain with whatever
--   files it returns, and record a new 'Summary' tick on @source@
--   pointing at the new alternate-chain head. Returns 'Nothing' (and
--   touches nothing) if there was nothing new to summarize, or if
--   @generate@ decided there was nothing worth writing (an empty result
--   map) -- either way, a no-op summary tick would only assert "nothing
--   changed," which the absence of a new tick already says for free.
--
--   Overwriting a previous summary of the same @kind@ is exactly a
--   second call to this function: the alternate chain gains one more
--   commit on top of its previous head, and the new 'Summary' tick on
--   @source@ points at that -- no amend, no rebase, the older
--   alternate-chain commit stays reachable through the chain's own
--   history for as long as *some* 'Summary' tick still names it or a
--   descendant of it.
--
--   Each touched file lands in its own real 'Storage.Ops.addAtom' (or, for
--   a file new to this alternate chain, 'Storage.Ops.saveFileAsNew')
--   commit -- the alternate chain reads as an ordinary branch's own file
--   history would, one commit per write, not one undifferentiated blob
--   replace per pass. A pass touching several files therefore produces
--   several alternate-chain commits, all chained together under the one
--   final @altHead@ this call's own 'Summary' tick records -- answering
--   "what source-chain state was this particular file in the summary tree
--   built from" is then 'Storyteller.Common.Summary.summaryTickFor', which
--   finds the *earliest* 'Summary' tick whose own 'summaryAltHead' already
--   has that file's exact commit as an ancestor (not necessarily *this*
--   pass's own tick, if a later pass carried the file forward untouched).
runSummarizer
  :: forall source r
  .  Members '[BranchOp source, Branches, Fail] r
  => Text                                  -- ^ summary kind, e.g. @"prose/chapter"@
  -> ([Tick] -> Sem r (Map FilePath Text))  -- ^ generation hook: candidate ticks -> summary files to write
  -> Sem r (Maybe TickId)
runSummarizer kind generate = do
  mCandidates <- pendingSummary @source kind
  case mCandidates of
    Nothing         -> return Nothing
    Just candidates -> do
      files <- generate candidates
      if Map.null files
        then return Nothing
        else Just <$> recordSummary @source kind files

-- | Summarize exactly @path@ -- never any other file of @kind@, even one
--   that's also stale. There is deliberately no guarantee that calling
--   this leaves *every* stale file of @kind@ freshly summarized, only
--   that @path@ itself, if it needs one, gets one: a user regenerating
--   one chapter by hand should never be forced to also regenerate every
--   other chapter that happens to share its kind, and a batch pass run
--   later must still find those other files exactly as stale as they
--   really are.
--
--   Three cases, matching 'runSummarizer's own no-op contract:
--
--   * @path@ has never been summarized for @kind@ at all -- generate its
--     first one;
--   * @path@'s current alt-chain content still has some unsummarized
--     tail ('Storyteller.Writer.Agent.SummaryAccess.unsummarizedTailSince')
--     -- regenerate from @path@'s current full content (same "always a
--     pure function of current content, never folds a prior compression
--     forward" rule 'runSummarizer' upholds);
--   * neither -- @path@ is already fully covered, so this is a genuine
--     no-op, no new tick, exactly like calling 'runSummarizer' with
--     nothing new to summarize.
--
--   The one thing this needs beyond 'runSummarizer's own shape: the new
--   'Summary' tick is inserted (via 'Storyteller.Core.Git.atGeneric') at
--   @path@'s own most recent atom, not wherever @source@'s head happens
--   to be. Appending at current head would be wrong the same way a
--   hand-edit through an already-open summary tier would be (see
--   'Server.Writer.File.Connection.openTarget's own Haddock for that
--   exact argument): this call never looked at whatever else landed on
--   @source@ after @path@'s own last edit, so it must never advance
--   @kind@'s shared "last summary" boundary past content it never
--   actually processed -- anything interleaved after that point (another
--   file's own edits, unrelated notes, even a *later* unrelated 'Summary'
--   tick of the same @kind@) is replayed back on top exactly where it
--   was, still exactly as stale to any later reader as it always was.
runSummarizerForPath
  :: forall source r
  .  Members '[BranchOp source, Branches, Fail] r
  => Text                      -- ^ summary kind, e.g. @"prose/chapter"@
  -> FilePath
  -> (Text -> Sem r Text)      -- ^ generation hook: this path's current full content -> its summary
  -> Sem r (Maybe TickId)
runSummarizerForPath kind path generate = do
  mContent <- pendingPathSummary @source kind path
  case mContent of
    Nothing      -> return Nothing
    Just content -> do
      compressed <- generate content
      Just <$> recordPathSummary @source kind path compressed

-- ---------------------------------------------------------------------------
-- Reading summaries back
-- ---------------------------------------------------------------------------

-- | @path@'s complete content at every available level, finest first: the
--   raw file, then each covering @kind@ in the order given. Never empty --
--   the raw level always exists.
--
--   The whole ladder rather than one level plus a way to pick, because
--   which level a caller wants is a pure decision over the candidate texts
--   ('densestWithin' is @break@ over this list). Nothing about budgets or
--   predicates needs to touch storage. Every entry is already complete --
--   content plus whatever was written since that level was produced -- so
--   picking never trades completeness for density.
summaryLadder
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> FilePath -> Sem r [Text]
summaryLadder kinds path = do
  levels <- SummaryAccess.zoomLevels @source kinds path
  SummaryAccess.completeContents @source path levels

-- | Every historical occurrence of @kind@ covering @path@ -- what a server
--   push turns into one synthetic tick apiece.
summaryOccurrences
  :: forall source r
  .  Member (BranchOp source) r
  => Text -> FilePath -> Sem r [Occurrence]
summaryOccurrences = SummaryAccess.summariesTouchingFor @source

-- | @path@'s content at the densest (most compressed) level that still
--   satisfies @ok@ -- a plain acceptability predicate over the candidate
--   text, e.g. @(<= 500) . wordCount@; this module has no opinion on what
--   "acceptable" means. The two extremes are just particular predicates:
--   @const True@ is satisfied at the raw level ("give it to me
--   uncompressed"), @const False@ falls through to the coarsest available
--   ("as compressed as this file gets").
--
--   'False' in the result means no level satisfied @ok@ and the coarsest
--   was returned anyway -- so a caller can fall back to truncation rather
--   than be told a silent lie about whether its budget held.
--
--   Pure over 'summaryLadder', deliberately: the ladder is read once, and
--   picking a rung off it is ordinary list logic that no caller should
--   need a storage scope to run.
densestWithin
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> (Text -> Bool) -> FilePath -> Sem r (Text, Bool)
densestWithin kinds ok path = pick <$> summaryLadder @source kinds path
  where
    pick cs = case break ok cs of
      (_, found : _) -> (found, True)
      -- 'summaryLadder' always includes the raw level, so this is total.
      (_, [])        -> (last cs, False)

-- | @path@'s densest complete view -- 'densestWithin' with @ok = const
--   False@, so it always falls through to the coarsest available level.
--   No marker separates an appended tail from the summary proper -- plain
--   concatenation, the same convention prose atoms already use.
densest :: forall source r. Member (BranchOp source) r => [Text] -> FilePath -> Sem r Text
densest kinds path = fst <$> densestWithin @source kinds (const False) path

-- | @path@'s content at the least-compressed level that still fits
--   @budget@ under @estimate@ -- a plain cost function, e.g.
--   @T.length \`div\` 4@ or a real tokenizer.
withinBudget
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> (Text -> Int) -> Int -> FilePath -> Sem r (Text, Bool)
withinBudget kinds estimate budget = densestWithin @source kinds (\t -> estimate t <= budget)
