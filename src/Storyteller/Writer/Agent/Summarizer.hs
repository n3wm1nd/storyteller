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
--   entirely inside 'Summarization' (see its own Haddock); a per-domain
--   summarizer supplying @generate@ never sees a 'BranchOp' phantom, an
--   alt-chain hash, or a 'Storage.Core.StoreT' action -- only candidate
--   ticks in, generated text out.
--
--   __And neither does anything about git, which is the point of the
--   effect being in the signature rather than discharged inside it.__
--   These two functions used to call 'runSummarization' on their own
--   bodies. That reads as encapsulation and is the opposite: an
--   interpreter's requirements are its caller's requirements, so
--   @Git@ and @StoryStorage@ -- everything the alt chain is actually made
--   of -- appeared on both public signatures, and from there on every
--   per-domain summarizer and every server command that ran one. The
--   effect named the concept while its type still spelled out the
--   implementation.
--
--   So the row says @Member ('Summarization' source) r@ and stops there.
--   Whoever assembles the stack picks the interpreter ('runSummarization',
--   the git-backed one, wired once alongside the other backend
--   interpreters); a backend that represents "a summary of this kind"
--   some other way supplies its own and nothing above this line changes.
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
  ( Summarization(..)
  , pendingSummary
  , recordSummary
  , pendingPathSummary
  , recordPathSummary
  , tieredSummary
  , runSummarization
  , runSummarizer
  , runSummarizerForPath

    -- * Reading them back
  , SummaryQuery(..)
  , summaryLadder
  , summaryOccurrences
  , runSummaryQuery
  , densest
  , densestWithin
  , withinBudget

  , withTrailingNewline
  ) where

import Prelude hiding (writeFile)

import Data.Kind (Type)
import Control.Monad (void, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storage.Query (lifetimeAtoms)
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary (Summary(..), bootstrapAltHead, lastSummaryOf, lastSummaryTouching, ticksSinceLastSummary)
import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, atGeneric, foldAscend, runBranchOpGitFrom, runStorage)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick(..), TickId(..), fromTick)
import Storyteller.Common.Summary (Occurrence)
import Storyteller.Writer.Agent.SummaryAccess (rawContent, unsummarizedTailSince)
import qualified Storyteller.Writer.Agent.SummaryAccess as SummaryAccess

-- | Extend an alternate chain by one commit: open a fresh, unnamed
--   'BranchOp' @chain@ scope seeded at @mPrev@ (the previous
--   'summaryAltHead' of this kind, if any) or, on the very first pass,
--   'Storyteller.Common.Summary.bootstrapAltHead', run @action@ against
--   it, and hand back its result plus the new head. @chain@ is a fresh
--   phantom every call (never the caller's own already-open branch scope,
--   unlike 'extendNestedAltChain', which deliberately reuses one) --
--   mirrors 'extendNestedAltChain's own @runBranchOpGitFrom@-based
--   pattern, just without the "only re-mint if the head actually moved"
--   step that's specific to re-pointing an *existing* 'Summary' tick. No
--   real branch is ever opened, created, or named -- the returned hash is
--   only ever reachable through whatever 'Summary' tick records it next.
extendAltChain
  :: forall chain r a
  .  Members '[Git, StoryStorage, Fail] r
  => Maybe TickId
  -> Sem (BranchOp chain ': r) a
  -> Sem r (a, TickId)
extendAltChain mPrev action = do
  seed <- case mPrev of
    Just (TickId h) -> return (Core.ObjectHash h)
    Nothing         -> bootstrapAltHead
  runBranchOpGitFrom @chain seed (\_ -> return ()) $ do
    a <- action
    h <- runStorage @chain Core.headHash
    return (a, TickId (Core.unObjectHash h))

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
--   Deliberately does *not* re-mint on 'Storyteller.Core.Git.runBranchOpGitFrom's
--   own per-write @onAdvance@ -- that fires on *every* internal head
--   movement, including ones that are pure bookkeeping, not new content:
--   @inner@ is typically itself a 'Storyteller.Core.Git.foldAscend'-driven
--   call (see 'Storyteller.Writer.Agent.JournalSummarizer.journalSummarize'),
--   which descends by repeatedly moving this very scope's head *backward*
--   (via 'Storage.Core.drop') before replaying forward again -- reacting to
--   each of those intermediate moves would re-mint @tid@ mid-descent,
--   against a head that isn't even the settled result yet, and can cascade
--   without ever converging. Instead, @onAdvance@ is a no-op, and once
--   @inner@ has fully run, this reads the scope's own final head exactly
--   once: only if it actually differs from @seed@ (i.e. @inner@ really did
--   write something) does @tid@ get re-minted (via 'atGeneric', exactly
--   'Server.Writer.File.Connection.openTarget's own @mintSummaryTick@) to
--   point at it -- otherwise whatever @inner@ wrote, however faithfully,
--   would just be an unreachable git object the instant this call returns:
--   an alternate chain has no ref of its own, so the *only* thing that
--   keeps any of its commits reachable is some 'Summary' tick still naming
--   the tip (see "Storyteller.Common.Summary"'s module Haddock).
extendNestedAltChain
  :: forall chain r a
  .  Members '[BranchOp chain, Git, StoryStorage, Fail] r
  => Text                                 -- ^ kind to re-mint @tid@ under -- unchanged across nesting depth
  -> TickId                               -- ^ tid: the Summary tick whose own alternate chain is being extended
  -> Core.ObjectHash                      -- ^ seed: tid's own summaryAltHead, i.e. that chain's current tip
  -> Sem (BranchOp chain : r) a
  -> Sem r a
extendNestedAltChain kind tid seed inner = do
  (result, finalHead) <- runBranchOpGitFrom @chain seed (\_ -> return ()) $ do
    a <- inner
    h <- runStorage @chain Core.headHash
    return (a, h)
  when (finalHead /= seed) $
    void (atGeneric @chain tid (runStorage @chain (Tick.storeAs (Summary kind (TickId (Core.unObjectHash finalHead))))))
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
-- The alt-chain structure, hidden behind one internal effect
-- ---------------------------------------------------------------------------

-- | Everything 'runSummarizer'\/'runSummarizerForPath' need from the
--   alt-chain machinery, named as the two questions they actually ask --
--   "what's pending" and "record this pass" -- never as the chain
--   primitives ('extendAltChain', 'Storage.Ops.addAtom' vs.
--   'Storage.Ops.saveFileAsNew', 'Storyteller.Core.Git.atGeneric'
--   positioning) those questions happen to be answered with today.
--
--   Exported, and named in 'runSummarizer'\/'runSummarizerForPath's own
--   rows rather than discharged inside them -- see those functions'
--   Haddock for why that distinction is the whole value of the effect. A
--   different backend representing "a summary of this kind" some other way
--   entirely rewrites 'runSummarization' and nothing else.
--
--   __Unparameterized, deliberately__ -- the same call this effect's
--   neighbour 'Storyteller.Core.Branch.Branches' already makes, for the
--   same reason. A @source@ phantom here would say "and the branch I
--   summarize is this one," but that isn't a caller's to say: the branch
--   is whichever scope 'runSummarization' was wired inside, fixed once at
--   wiring time, and no interpreter for @'Summarization' A@ can exist
--   anywhere but inside @'Storyteller.Core.Branch.BranchOp' A@'s own
--   scope. Carrying it anyway meant every call site wrote the tag twice
--   -- once selecting the effect in the row, once selecting the scope
--   backing it -- with nothing checking that the two agreed.
--
--   A phantom earns its place when two are live at once and a caller has
--   to say which ("Storyteller.Writer.Agent.Tracker" really does hold two
--   'Storyteller.Core.Branch.BranchOp' scopes and address both). Nothing
--   summarizes two branches simultaneously; a nested wiring simply
--   shadows, which is the right reading of "summarize here" anyway.
data Summarization (m :: Type -> Type) a where
  -- | Every real tick since @kind@'s last pass (or since root, if none
  --   yet), or 'Nothing' if there's genuinely nothing new -- the
  --   candidate material a fresh whole-branch pass should consider.
  PendingSummary :: Text -> Summarization m (Maybe [Tick])
  -- | Record @content@ (one file per path) as @kind@'s new pass, given
  --   whatever candidates justified generating it.
  RecordSummary :: Text -> Map FilePath Text -> Summarization m TickId
  -- | @path@'s current full content, if its existing @kind@ compression
  --   is stale or missing -- 'Nothing' if it's already up to date, or
  --   @path@ isn't atom-tracked at all (nothing to summarize).
  PendingPathSummary :: Text -> FilePath -> Summarization m (Maybe Text)
  -- | Record @content@ as @path@'s fresh @kind@ compression, correctly
  --   positioned at @path@'s own last atom rather than wherever the
  --   summarized branch's head happens to be (see 'runSummarizerForPath's
  --   own Haddock for why that positioning matters).
  RecordPathSummary :: Text -> FilePath -> Text -> Summarization m TickId

  -- | Compress @path@'s whole history in tiers: group its unconsumed
  --   entries into batches of @size@, reduce each batch with the supplied
  --   function, and -- once a tier has written a full batch -- apply the
  --   identical treatment to /that/ tier's output, recursively, for as
  --   many tiers as the content warrants. 'True' if anything was written
  --   at the tier this was called on.
  --
  --   __The reduce function is the entire contribution a caller makes.__
  --   Everything else -- where a batch boundary falls, where each tick
  --   lands in the chain, how a tier's output becomes the next tier's
  --   input, when to stop -- is piping, and piping is what an
  --   interpreter is for. A caller says "compress ten of these into one"
  --   and knows nothing else; see 'tieredPass', which is that piping,
  --   and "Storyteller.Writer.Agent.JournalSummarizer", which is now
  --   little more than a prompt plus two calls to this.
  --
  --   The reduce runs in the caller's own @m@, so it can be a real
  --   'Runix.LLM.queryLLM' agent in production and a pure stub in a test
  --   with nothing else changing -- the one thing that has to cross this
  --   boundary in that direction, and the reason this constructor is
  --   higher-order while its four neighbours above are not.
  --
  --   @force@ closes out a partial batch instead of waiting for a full
  --   one -- manual creation, where the point is to land an empty chunk
  --   exactly where an automatic pass would have. Only ever applies to
  --   the tier this is called on; nested tiers always run unforced.
  TieredSummary
    :: Text                -- ^ summary kind, shared by every tier
    -> FilePath            -- ^ the path being compressed, at every tier
    -> Int                 -- ^ entries per batch
    -> Bool                -- ^ force a partial batch to close out
    -> ([Text] -> m Text)  -- ^ reduce one full batch, oldest first
    -> Summarization m Bool

makeSem ''Summarization

-- | The git-backed 'Summarization' interpreter -- moves every alt-chain
--   detail ('extendAltChain', per-file commit shape, 'atGeneric'
--   positioning, tier recursion, 'Storyteller.Common.Summary'
--   bookkeeping) out of every summarizer and into exactly one place.
--
--   'interpretH' rather than 'interpret' solely because 'TieredSummary'
--   carries a reduce function in the caller's own @m@. That costs a
--   reified continuation per dispatch, which is why
--   'Storyteller.Core.Branch.BranchOp' avoids it -- but a dispatch here is
--   one whole summarization pass, not one tick, so the reification happens
--   a handful of times per pass rather than once per commit walked.
--
--   'getInspectorT' is what lets the reduce's result be used as an
--   ordinary 'Text' by the storage writes below. A 'Nothing' from
--   'inspect' means some effect between here and the caller short-circuited
--   the reduce (an 'Polysemy.Error.Error', say) -- in which case there is
--   no compressed text to record and the pass stops without writing, which
--   is the correct reading of "compression didn't happen."
runSummarization
  :: forall source r a
  .  Members '[BranchOp source, Git, StoryStorage, Fail] r
  => Sem (Summarization ': r) a
  -> Sem r a
runSummarization = interpretH $ \case
  PendingSummary kind -> do
    candidates <- runStorage @source (ticksSinceLastSummary kind)
    pureT $ if null candidates then Nothing else Just candidates

  TieredSummary kind path size force reduce -> do
    ins     <- getInspectorT
    s0      <- getInitialStateT
    reduceT <- bindT reduce
    -- 'bindT' hands back a reduce that runs in @'Summarization' ': r@, so
    -- the walk runs there too and simply calls it -- then this interpreter
    -- is applied to the result recursively to land back in @r@. That is
    -- what keeps 'tieredPass' an ordinary function over ordinary storage
    -- effects, with no functorial state threaded through it and no idea
    -- it's being run from inside an interpreter.
    let reduceHere items = inspect ins <$> reduceT (items <$ s0)
    wrote <- raise . runSummarization @source $
               tieredPass @source kind path size force reduceHere
    pureT wrote

  RecordSummary kind files -> do
    mPrev <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
    (_, newAltHead) <- extendAltChain @() mPrev $
      runStorage @() (mapM_ (uncurry setAltFileContent) (Map.toList files))
    newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
    pureT (TickId (Core.unObjectHash newHash))

  PendingPathSummary kind path -> do
    mLast <- runStorage @source (lastSummaryTouching kind path)
    upToDate <- case mLast of
      Nothing     -> return False
      Just (_, s) -> T.null <$> unsummarizedTailSince @source s path
    pureT =<< if upToDate
      then return Nothing
      else do
        lifetime <- runStorage @source (lifetimeAtoms path)
        case lifetime of
          [] -> return Nothing  -- path isn't atom-tracked at all -- nothing to summarize
          _  -> Just . fromMaybe "" <$> rawContent @source path

  RecordPathSummary kind path content -> do
    lifetime <- runStorage @source (lifetimeAtoms path)
    pureT =<< case lifetime of
      [] -> fail ("recordPathSummary: " <> path <> " is not atom-tracked")
      _  -> do
        let (lastAtomHash, _) = last lifetime
        atGeneric @source (TickId (Core.unObjectHash lastAtomHash)) $ do
          mPrev <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
          (_, newAltHead) <- extendAltChain @() mPrev (runStorage @() (setAltFileContent path content))
          newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
          return (TickId (Core.unObjectHash newHash))
  where
    -- | Commit @content@ as @path@'s current state in whichever alternate
    --   chain @extendAltChain@ has seeded, as a real 'Storage.Ops.addAtom'
    --   write -- or, if @path@ hasn't been written there before, seed it
    --   fresh the same way ('Storage.Ops.saveFileAsNew' would fail
    --   otherwise, since 'Storage.Ops.deleteFile' assumes something to
    --   delete). Every per-domain summarizer always recomputes a file's
    --   *whole* current compression from scratch each pass (never folds a
    --   prior one forward -- see
    --   'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate's
    --   own Haddock for why), so this deliberately replaces the file's
    --   prior alternate-chain lifetime outright rather than appending onto
    --   it the way 'Storyteller.Writer.Agent.JournalSummarizer' does for
    --   its own, genuinely incremental, per-group writes.
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

-- | 'TieredSummary's implementation: the piping, with the reduce supplied.
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
--   A reduce that comes back 'Nothing' short-circuited somewhere above
--   this interpreter (see 'runSummarization'), so there is nothing to
--   record: the pass stops where it stands, leaving the buffered entries
--   exactly as pending as they were.
tieredPass
  :: forall source r
  .  Members '[BranchOp source, Git, StoryStorage, Fail] r
  => Text                            -- ^ summary kind, shared by every tier
  -> FilePath                        -- ^ the path being compressed, at every tier
  -> Int                             -- ^ entries per batch
  -> Bool                            -- ^ force a partial batch to close out
  -> ([Text] -> Sem r (Maybe Text))  -- ^ reduce one batch, oldest first
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
        void $ extendNestedAltChain @source kind tid (Core.ObjectHash (unTickId altHead))
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
    commitBatch acc entries = reduce entries >>= \case
      Nothing         -> return acc
      Just compressed -> do
        (_, newAltHead) <- extendAltChain @() (caAltHead acc)
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
  :: forall r
  .  Member Summarization r
  => Text                                  -- ^ summary kind, e.g. @"prose/chapter"@
  -> ([Tick] -> Sem r (Map FilePath Text))  -- ^ generation hook: candidate ticks -> summary files to write
  -> Sem r (Maybe TickId)
runSummarizer kind generate = do
  mCandidates <- pendingSummary kind
  case mCandidates of
    Nothing         -> return Nothing
    Just candidates -> do
      files <- generate candidates
      if Map.null files
        then return Nothing
        else Just <$> recordSummary kind files

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
  :: forall r
  .  Member Summarization r
  => Text                      -- ^ summary kind, e.g. @"prose/chapter"@
  -> FilePath
  -> (Text -> Sem r Text)      -- ^ generation hook: this path's current full content -> its summary
  -> Sem r (Maybe TickId)
runSummarizerForPath kind path generate = do
  mContent <- pendingPathSummary kind path
  case mContent of
    Nothing      -> return Nothing
    Just content -> do
      compressed <- generate content
      Just <$> recordPathSummary kind path compressed

-- ---------------------------------------------------------------------------
-- Reading summaries back
--
-- Below the write machinery purely because a Template Haskell splice
-- ('makeSem') ends a declaration group: anything defined after one is
-- invisible to everything before it, and 'runSummarization' calls
-- 'tieredPass'.
-- ---------------------------------------------------------------------------

-- | Reading a file through whatever compressions exist for it -- the other
--   half of this module's concept, and until now the half that wasn't
--   behind an effect at all: writes went through 'Summarization' while
--   every read reached past it into
--   "Storyteller.Writer.Agent.SummaryAccess"\'s chain walks. A backend
--   supplying its own 'Summarization' would have satisfied every write and
--   still had every reader asking questions only alternate chains can
--   answer.
--
--   __Its own effect, rather than more constructors on 'Summarization'.__
--   Reading is strictly the smaller ask: a backend that keeps compressions
--   around in some form it did not produce itself -- imported, computed
--   elsewhere, cached -- can answer every question here without being able
--   to honour a single write, and 'runSummaryQuery' reflects that by
--   needing a branch scope and nothing else, where 'runSummarization'
--   needs the whole alt-chain apparatus (and therefore @Git@ and
--   @StoryStorage@, since it mints chains). A read-only capability is also
--   simply worth having on its own: a caller that holds this can be seen,
--   from its type alone, to be incapable of changing any of it.
--
--   The practical payoff is at the wiring point. The context DSL
--   interprets its own effects per call, at whichever branch that call
--   runs against ('Storyteller.Core.Context.runContextValue' is called at
--   @\@Main@, @\@LoreSource@, and caller-generic branches), so a reader
--   bundled in with the writes would have pulled @Git@ into every DSL
--   signature -- or needed a branch phantom to avoid it, which is exactly
--   the redundancy 'Summarization' just shed.
--
--   'SummaryLadder' is deliberately the /whole ladder/ rather than one
--   level plus a way to pick: which level a caller wants is a pure
--   decision over the candidate texts ('densestWithin' is @break@ over
--   this list), so nothing about budgets, predicates or "how compressed is
--   compressed enough" needs to be an operation an interpreter implements.
--   Every entry is already complete -- content plus whatever was written
--   since that level was produced -- so picking never trades completeness
--   for density.
data SummaryQuery (m :: Type -> Type) a where
  -- | @path@'s complete content at every available level, finest first
  --   (the raw file, then each covering @kind@ in the order given).
  --   Never empty: the raw level always exists.
  SummaryLadder :: [Text] -> FilePath -> SummaryQuery m [Text]
  -- | Every historical occurrence of @kind@ covering @path@ -- what a
  --   server push turns into one synthetic tick apiece.
  SummaryOccurrences :: Text -> FilePath -> SummaryQuery m [Occurrence]

makeSem ''SummaryQuery

-- | The git-backed reader. Needs a branch scope and nothing more -- see
--   'SummaryQuery' on why that is the whole point of it being its own
--   effect.
runSummaryQuery
  :: forall source r a
  .  Member (BranchOp source) r
  => Sem (SummaryQuery ': r) a
  -> Sem r a
runSummaryQuery = interpret $ \case
  SummaryLadder kinds path -> do
    levels <- SummaryAccess.zoomLevels @source kinds path
    SummaryAccess.completeContents @source path levels
  SummaryOccurrences kind path -> SummaryAccess.summariesTouchingFor @source kind path

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
--   Pure over 'summaryLadder', deliberately: see 'SummaryQuery'.
densestWithin
  :: Member SummaryQuery r
  => [Text] -> (Text -> Bool) -> FilePath -> Sem r (Text, Bool)
densestWithin kinds ok path = pick <$> summaryLadder kinds path
  where
    pick cs = case break ok cs of
      (_, found : _) -> (found, True)
      -- 'summaryLadder' always includes the raw level, so this is total.
      (_, [])        -> (last cs, False)

-- | @path@'s densest complete view -- 'densestWithin' with @ok = const
--   False@, so it always falls through to the coarsest available level.
--   No marker separates an appended tail from the summary proper -- plain
--   concatenation, the same convention prose atoms already use.
densest :: Member SummaryQuery r => [Text] -> FilePath -> Sem r Text
densest kinds path = fst <$> densestWithin kinds (const False) path

-- | @path@'s content at the least-compressed level that still fits
--   @budget@ under @estimate@ -- a plain cost function, e.g.
--   @T.length \`div\` 4@ or a real tokenizer.
withinBudget
  :: Member SummaryQuery r
  => [Text] -> (Text -> Int) -> Int -> FilePath -> Sem r (Text, Bool)
withinBudget kinds estimate budget = densestWithin kinds (\t -> estimate t <= budget)
