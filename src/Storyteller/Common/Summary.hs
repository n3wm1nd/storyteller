{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A summary is a tick on a source branch whose ref points at the HEAD of
--   a separate "alternate chain" -- a *nameless* commit chain (never a real
--   branch, never given its own ref) whose tree holds compressed files
--   standing in for (a range of) the source branch's own content. Unlike
--   an inline summary (a tick whose message just holds compressed text, no
--   filesystem footprint), this lets a summarizer produce a whole
--   compressed filesystem.
--
--   The alternate chain has no lifecycle of its own: it exists only as
--   long as some 'Summary' tick's own 'summaryAltHead' names its tip.
--   Amend or rebase away the tick that pointed at it (with nothing newer
--   taking its place), and the alternate chain becomes an ordinary
--   unreachable git object -- reclaimed by GC exactly the way the temporal
--   ledger and snapshot branch already rely on elsewhere in this codebase,
--   with no explicit deletion anywhere. This is also why hierarchical
--   summarization (a book summarizer building on a chapter summarizer's
--   output) still attaches *every* 'Summary' tick, whatever its tier, to
--   the one real source branch: an alternate chain with no ref of its own
--   can never itself be "opened" to extend, so nothing can ever be
--   layered on top of it directly -- only ever read from it, to build the
--   next real tick.
--
--   Everything here is storage-agnostic (works over any 'StoreM'); the
--   Polysemy/'BranchOp' boundary -- generating content, extending the
--   alternate chain, wiring a WS command -- lives one layer up in
--   "Storyteller.Writer.Agent.Summarizer" and
--   "Storyteller.Writer.Agent.SummaryAccess".
module Storyteller.Common.Summary
  ( Summary(..)
  , previewPath
  , bootstrapAltHead
  , lastSummaryOf
  , ticksSince
  , ticksSinceLastSummary
  , availableSummaries
  , summaryTickFor
  , lastTouchedIn
  , summaryContent
  ) where

import Control.Monad.State.Strict (lift)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import Storage.Core
  ( CommitData(..), MonadStore(..), ObjectHash(..), StoreM, StoreObject(..), StoreT, readPathAt
  )
import qualified Storage.Tick as Tick
import Storyteller.Core.Types
  ( Tick(..), TickId(..), TickType(..), TickData(..)
  , encodeDraft, decodePayload, fromTick, tickId, tickParent
  )

-- | A summary tick: no filesystem footprint of its own on the source
--   branch (an ordinary opaque cross-branch pointer -- see 'Storage.Tick.storeAs'),
--   naming which summarizer produced it and where its output tree's HEAD
--   currently is.
--
--   Carries no explicit range -- the range a 'Summary' covers is implicit:
--   everything back to the *previous* 'Summary' tick of the same
--   'summaryKind' on this chain (or root, if none) -- see
--   'ticksSinceLastSummary'. A tick id may only ever appear in 'tickRefs',
--   never embedded in a message or field, so a range-start marker (which
--   would just be this tick's own chain position restated) isn't a ref at
--   all; it needs none, since the position is already implicit in where
--   this tick itself sits.
data Summary = Summary
  { summaryKind    :: Text    -- ^ app-chosen, storage-agnostic, e.g. @"prose/chapter"@, @"character/internal-state"@
  , summaryAltHead :: TickId  -- ^ HEAD of the alternate chain commit holding this summary's content, at the moment it was written -- the only thing keeping that chain reachable
  } deriving (Show, Eq)

instance TickType Summary where
  tickTypeName = "summary"

  toDraft (Summary kind altHead) = encodeDraft @Summary [altHead] [("kind", kind)] ""

  fromTick t = do
    _    <- decodePayload @Summary t
    kind <- lookup "kind" (tickFields (tickData t))
    case tickRefs (tickData t) of
      [altHead] -> Just Summary { summaryKind = kind, summaryAltHead = altHead }
      _         -> Nothing

-- | The conventional path, within an alternate branch's own tree, a
--   summarizer *may* write a short human-readable blurb of what to expect
--   in there -- "the commit pointing to the summary tree doesn't have to
--   be empty." Entirely optional: a summarizer for which this makes no
--   sense just never writes it, and reading it back (via 'summaryContent'
--   @s@ 'previewPath') gives 'Nothing'. An ordinary file, read the same
--   way as any other summarized content -- no separate wire support needed.
previewPath :: FilePath
previewPath = "SUMMARY.md"

-- | A fixed, parentless, empty-tree commit -- the technical bottom a
--   summarizer's very first pass (for a given kind, on a given branch)
--   extends to produce its first real alternate-chain commit. Every
--   caller everywhere computes and writes the exact same content-addressed
--   object, so nothing here needs naming, creating, or remembering: it
--   carries no content and is never itself the target of any read, only
--   ever a parent to build the first real commit on top of.
bootstrapAltHead :: MonadStore m => m ObjectHash
bootstrapAltHead = do
  emptyTree <- writeObject (TreeObject [])
  writeCommit CommitData { commitParents = [], commitTree = emptyTree, commitMessage = "" }

-- | The most recent 'Summary' tick of @kind@ on the currently open chain,
--   read alongside its own id -- 'Nothing' if this kind has never been
--   summarized here. Walks backward from HEAD, stopping at the first
--   match; the common, incremental case (one new summary since the last)
--   costs only what's actually new.
lastSummaryOf :: StoreM m => Text -> StoreT m (Maybe (TickId, Summary))
lastSummaryOf kind = Tick.findTick $ \_ t -> do
  s <- fromTick @Summary t
  if summaryKind s == kind then Just (tickId t, s) else Nothing

-- | Every tick strictly after @stopAt@ (exclusive), oldest-first, walking
--   back from HEAD all the way to root if @stopAt@ is 'Nothing' (or isn't
--   found in this chain's history at all). The one general "since this
--   point" walk everything else here is built from -- 'ticksSinceLastSummary'
--   is just this with @stopAt@ derived from 'lastSummaryOf'; a caller
--   that already knows a more specific stop point (e.g. exactly which
--   tick a particular file's compression is as fresh as -- see
--   'lastTouchedIn') uses this directly instead.
ticksSince :: StoreM m => Maybe TickId -> StoreT m [Tick]
ticksSince stopAt = collect Nothing []
  where
    collect cursor acc = do
      t <- currentTick cursor
      if Just (tickId t) == stopAt
        then return acc
        else case tickParent t of
          Nothing     -> return (t : acc)
          Just parent -> collect (Just parent) (t : acc)

    currentTick Nothing        = Tick.getTypesTick
    currentTick (Just (TickId h)) = Tick.readTypesTick (ObjectHash h)

-- | Every tick since @kind@'s last summary (exclusive of that summary
--   tick itself), oldest-first -- exactly the candidate set a summarizer
--   for @kind@ should consider next. If @kind@ has never run here, that's
--   every tick back to root. This is the one place that turns "a summary's
--   range is implicit" into concrete content; nothing outside this module
--   needs to know how the range is found.
--
--   Answers "what should @kind@'s *next* pass consider" -- not "how fresh
--   is a *specific file's* current compression," which can be an earlier
--   point than this if the most recent pass didn't touch that file (see
--   'lastTouchedIn').
ticksSinceLastSummary :: StoreM m => Text -> StoreT m [Tick]
ticksSinceLastSummary kind = do
  mLast <- lastSummaryOf kind
  ticksSince (fst <$> mLast)

-- | Every 'Summary' tick on the currently open chain, optionally filtered
--   to one @kind@ -- the whole "what's available to summarize this
--   branch/file with" catalogue, most-recent first (nearest HEAD).
--   Nothing here reads any alternate-chain content; see 'summaryContent'
--   for that, once a caller has picked one.
availableSummaries :: StoreM m => Maybe Text -> StoreT m [(TickId, Summary)]
availableSummaries mKind = reverse <$> go Nothing []
  where
    go cursor acc = do
      t <- currentTick cursor
      let acc' = case fromTick @Summary t of
            Just s | maybe True (== summaryKind s) mKind -> (tickId t, s) : acc
            _                                             -> acc
      case tickParent t of
        Nothing     -> return acc'
        Just parent -> go (Just parent) acc'

    currentTick Nothing           = Tick.getTypesTick
    currentTick (Just (TickId h)) = Tick.readTypesTick (ObjectHash h)

-- | The reverse of 'summaryAltHead': given a specific commit in *some*
--   alternate chain (typically found by walking that chain's own history
--   to whichever commit last introduced or replaced one particular file --
--   an ordinary content comparison, nothing summary-specific), this
--   answers "which point on *this* chain was that alternate-chain state
--   built from" -- the earliest 'Summary' tick whose own 'summaryAltHead'
--   already has @altCommit@ as an ancestor (or *is* @altCommit@ itself).
--
--   Reachability, not exact equality: a summarization pass covering
--   several files is under no obligation to land every one of them in a
--   single alternate-chain commit (a per-domain summarizer using
--   'Storage.Ops.addAtom'\/'Storage.Ops.saveFileAsNew' per file, the way
--   the alternate chain is meant to look like an ordinary branch's own
--   file history, produces one commit *per file*, all chained together
--   under the one @altHead@ the pass's own 'Summary' tick actually
--   records). Matching only that final commit exactly would leave every
--   earlier file in the same batch with no 'Summary' tick "reachable"
--   from its own last-touched commit at all. Reachability finds the
--   right answer either way: the *earliest* 'Summary' tick whose altHead
--   already contains @altCommit@ is exactly the pass @altCommit@'s own
--   file was actually written in, whether or not that pass's own altHead
--   happens to equal @altCommit@ exactly.
--
--   'availableSummaries' is newest-first; this searches oldest-first
--   (chronological) so the *first* match found really is the earliest
--   pass that already covered @altCommit@, not some later pass that
--   merely also carries it forward.
summaryTickFor :: StoreM m => ObjectHash -> StoreT m (Maybe (TickId, Summary))
summaryTickFor altCommit = do
  oldestFirst <- reverse <$> availableSummaries Nothing
  findM reachable oldestFirst
  where
    reachable (_, s) = isAncestorOrSelf altCommit (ObjectHash (unTickId (summaryAltHead s)))

    findM _ []       = return Nothing
    findM p (x : xs) = do
      ok <- p x
      if ok then return (Just x) else findM p xs

-- | Is @candidate@ either @target@ itself or somewhere in @target@'s own
--   history? The alternate chain is always a strictly linear commit
--   chain -- every write to it goes through 'Storage.Ops.addAtom'\/
--   'Storage.Ops.deleteFile' (directly, or via 'Storage.Ops.saveFileAsNew'),
--   none of which ever attach an extra ref\/parent (see their own
--   Haddocks) -- so a first-parent-only backward walk from @target@ is
--   exact, not an approximation that happens to work for the common case.
isAncestorOrSelf :: StoreM m => ObjectHash -> ObjectHash -> StoreT m Bool
isAncestorOrSelf candidate = lift . go
  where
    go h
      | h == candidate = return True
      | otherwise = do
          cd <- readCommit h
          case commitParents cd of
            []      -> return False
            (p : _) -> go p

-- | The forward half 'summaryTickFor' needs a hash to look up in the
--   first place: walking @s@'s own alternate chain backward from its
--   HEAD, this finds the exact commit where @path@ was last introduced or
--   changed -- 'Nothing' only if @path@ was never actually written there
--   (shouldn't happen if 'summaryContent' already found it there; kept
--   total rather than partial).
--
--   This matters because the alternate chain is cumulative: a pass that
--   only regenerates *some* files still carries every other file forward
--   from the previous commit untouched, so @s@ -- the *most recent*
--   'Summary' of a kind -- can easily hold a file's content from several
--   passes ago. Splicing a raw "everything since" tail onto that content
--   using @s@'s own tick as the stop point (the naive, wrong approach)
--   would use the wrong boundary: too late if a later pass touched the
--   file (missing whatever changed in between), or -- the more common
--   failure -- too early if it didn't, silently dropping the gap between
--   when the file's compression actually stopped and when @s@'s pass ran.
--   Composed with 'summaryTickFor' (@summaryTickFor \<=\< lastTouchedIn s@
--   path, roughly), this finds the *exact* source-chain tick a file's
--   current compression is as fresh as, which is the only boundary a
--   correct splice can use.
lastTouchedIn :: StoreM m => Summary -> FilePath -> StoreT m (Maybe ObjectHash)
lastTouchedIn s path = lift (go altHash)
  where
    altHash = ObjectHash (unTickId (summaryAltHead s))

    go h = do
      here <- readPathAt h path
      case here of
        Nothing -> return Nothing
        Just _  -> do
          cd <- readCommit h
          case commitParents cd of
            []      -> return (Just h)
            (p : _) -> do
              parentContent <- readPathAt p path
              if parentContent == here then go p else return (Just h)

-- | @path@'s content within @s@'s alternate chain, exactly as it stood
--   when @s@ was written -- a direct, chain-walk-free read (see
--   'Storage.Core.readPathAt'), valid from *any* currently-open branch
--   scope: an alternate chain's commit is an ordinary content-addressed
--   object, not something that requires its own branch to be open to
--   read (it has no branch to open at all -- see the module Haddock).
summaryContent :: StoreM m => Summary -> FilePath -> StoreT m (Maybe Text)
summaryContent s path = do
  mbs <- lift (readPathAt altHash path)
  return (TE.decodeUtf8With TE.lenientDecode <$> mbs)
  where
    altHash = ObjectHash (unTickId (summaryAltHead s))
