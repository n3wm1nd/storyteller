{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
  ) where

import Prelude hiding (writeFile)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Common.Summary (Summary(..), bootstrapAltHead, lastSummaryOf, ticksSinceLastSummary)
import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (Tick(..), TickId(..))

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
--   The alternate chain's own append-only invariant does not apply here
--   -- a summary tree is never atom-tracked, since nothing about it needs
--   per-atom history the way source prose does (see 'Storage.Ops.saveFile's
--   own reconciliation, which this deliberately does not use). Instead, a
--   different invariant has to hold: *every* file this writes lands in
--   exactly *one* new alternate-chain commit per call, all together --
--   never one commit per file -- so that commit is unambiguously "the one
--   the new 'Summary' tick points at." Answering "what source-chain state
--   was this particular file in the summary tree built from" is then
--   always the same two-step walk: find which alternate-chain commit last
--   introduced or replaced that file (an ordinary content comparison
--   across the chain's own history, nothing summary-specific), then find
--   the 'Summary' tick on @source@ whose 'summaryAltHead' names that
--   commit (see 'Storyteller.Common.Summary.summaryTickFor'). That second
--   step would silently give the wrong answer -- or no answer at all -- if
--   a batch ever spread across several commits, since only the *last* one
--   would ever be recorded.
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
          (_, newAltHead) <- extendAltChain mPrev $ do
            mapM_ (\(path, content) -> Core.writeFile path (TE.encodeUtf8 content)) (Map.toList files)
            Ops.commitWorktree
          newHash <- runStorage @source (Tick.storeAs (Summary kind newAltHead))
          return (Just (TickId (Core.unObjectHash newHash)))
