{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
--   There is no @alt@ branch parameter: an alternate chain is never a
--   real, named branch (see "Storyteller.Common.Summary"'s module
--   Haddock for why) -- it's extended by hash, anchored either at the
--   previous 'Storyteller.Common.Summary.summaryAltHead' of the same
--   kind, or at a fixed bootstrap commit on the very first pass. A
--   hierarchical summarizer (a book-tier summarizer whose @generate@
--   reads a chapter-tier alternate chain's own content as its input)
--   still calls this with @source@ set to the *same* real branch every
--   other tier uses -- only the @kind@ differs.
module Storyteller.Writer.Agent.Summarizer
  ( runSummarizer
  , runSummarizerForPath
  , extendAltChain
  , extendNestedAltChain
  , withTrailingNewline
  ) where

import Prelude hiding (writeFile)

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
import Storyteller.Core.Git (BranchOp, atGeneric, runBranchOpGitFrom, runStorage)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick(..), TickId(..))
import Storyteller.Writer.Agent.SummaryAccess (rawContent, unsummarizedTailSince)

-- | Extend an alternate chain by one commit: seed a fresh, unnamed
--   'Core.StoreT' scope at @mPrev@ (the previous 'summaryAltHead' of this
--   kind, if any) or, on the very first pass,
--   'Storyteller.Common.Summary.bootstrapAltHead', run @action@ against
--   it, and hand back its result plus the new head. No branch is ever
--   opened, created, or named -- the returned hash is only ever reachable
--   through whatever 'Summary' tick records it next.
extendAltChain
  :: Members '[Git, StoryStorage, Fail] r
  => Maybe TickId
  -> (forall n. Core.StoreM n => Core.StoreT n a)
  -> Sem r (a, TickId)
extendAltChain mPrev action = do
  seed <- case mPrev of
    Just (TickId h) -> return (Core.ObjectHash h)
    Nothing         -> bootstrapAltHead
  (result, (newHead, _)) <- Core.runStoreT seed action
  return (result, TickId (Core.unObjectHash newHead))

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
  .  Members '[BranchOp source, Git, StoryStorage, Fail] r
  => Text                                  -- ^ summary kind, e.g. @"prose/chapter"@
  -> ([Tick] -> Sem r (Map FilePath Text))  -- ^ generation hook: candidate ticks -> summary files to write
  -> Sem r (Maybe TickId)
runSummarizer kind generate = do
  candidates <- runStorage @source (ticksSinceLastSummary kind)
  if null candidates
    then return Nothing
    else do
      files <- generate candidates
      if Map.null files
        then return Nothing
        else do
          mPrev <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
          (_, newAltHead) <- extendAltChain mPrev $
            mapM_ (\(path, content) -> replaceWithAtom path content) (Map.toList files)
          newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
          return (Just (TickId (Core.unObjectHash newHash)))

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
  .  Members '[BranchOp source, Git, StoryStorage, Fail] r
  => Text                      -- ^ summary kind, e.g. @"prose/chapter"@
  -> FilePath
  -> (Text -> Sem r Text)      -- ^ generation hook: this path's current full content -> its summary
  -> Sem r (Maybe TickId)
runSummarizerForPath kind path generate = do
  -- Freshness is judged against the newest tick that actually *covers*
  -- path ('lastSummaryTouching'), never the kind's newest tick, full stop
  -- ('lastSummaryOf') -- the two diverge exactly when this function's own
  -- insert-at-path's-last-atom positioning (below) has put an earlier
  -- file's summary behind a later file's, and judging against the wrong
  -- one would re-mint a fresh pass on every call forever.
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
        _  -> do
          let (lastAtomHash, _) = last lifetime
          atGeneric @source (TickId (Core.unObjectHash lastAtomHash)) $ do
            content    <- fromMaybe "" <$> rawContent @source path
            compressed <- generate content
            mPrev      <- runStorage @source (fmap (summaryAltHead . snd) <$> lastSummaryOf kind)
            (_, newAltHead) <- extendAltChain mPrev (replaceWithAtom path compressed)
            newHash    <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
            return (Just (TickId (Core.unObjectHash newHash)))

-- | Commit @content@ as @path@'s current state in whichever alternate
--   chain @extendAltChain@ has seeded, as a real 'Storage.Ops.addAtom'
--   write -- or, if @path@ hasn't been written there before, seed it
--   fresh the same way ('Storage.Ops.saveFileAsNew' would fail otherwise,
--   since 'Storage.Ops.deleteFile' assumes something to delete). Every
--   per-domain summarizer always recomputes a file's *whole* current
--   compression from scratch each pass (never folds a prior one forward
--   -- see 'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryGenerate's
--   own Haddock for why), so this deliberately replaces the file's prior
--   alternate-chain lifetime outright rather than appending onto it the
--   way 'Storyteller.Writer.Agent.JournalSummarizer' does for its own,
--   genuinely incremental, per-group writes.
replaceWithAtom :: Core.StoreM m => FilePath -> Text -> Core.StoreT m ()
replaceWithAtom path content = do
  there <- Ops.exists path
  if there
    then Ops.saveFileAsNew path path content
    else void (Ops.addAtom path content)
