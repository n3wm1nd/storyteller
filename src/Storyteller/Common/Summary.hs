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
--   with no explicit deletion anywhere.
--
--   Hierarchical summarization (a coarser tier built from a finer tier's
--   own output, e.g. "Storyteller.Writer.Agent.JournalSummarizer") is
--   *not* a naming convention layered on top of this -- an alternate chain
--   is a real, ref-less chain of ticks in exactly the same sense a real
--   branch is, so a coarser tier's own 'Summary' tick can be posted
--   directly *onto* a finer tier's alternate chain, extending it, rather
--   than living back on the one real source branch under some
--   tier-distinguishing kind label. Depth is then a purely structural fact
--   -- *which chain* a given 'Summary' tick's own commit actually lives on
--   -- discovered by walking (see 'summariesTouching', called again from
--   a storage scope opened at a finer tier's own 'summaryAltHead'), never
--   declared: every tier of a recursive family shares one plain
--   'summaryKind', and a nested chain's own tip happening to decode as a
--   further 'Summary' is what makes it a next tier, full stop.
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
  , lastSummaryTouching
  , supersedingTipFrom
  , ticksSince
  , ticksSinceLastSummary
  , availableSummaries
  , summaryTickFor
  , lastTouchedIn
  , summaryContent
  , Occurrence(..)
  , summariesTouching
  , occurrenceDelta
  ) where

import Control.Monad.State.Strict (lift)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import Storage.Core
  ( CommitData(..), MonadStore(..), ObjectHash(..), StoreM, StoreObject(..), StoreT, headHash, readAt, readPathAt
  )
import qualified Storage.Tick as Tick
import Storyteller.Core.Atom (Atom(..), contentFor)
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

-- | 'lastSummaryOf', starting the walk from an arbitrary tick instead of
--   head -- 'lastSummaryTouchingFrom' needs the previous same-kind tick
--   *before some already-known tick*, which may sit well behind wherever
--   this chain's head currently is. Internal only: callers outside this
--   module always want either the head-anchored 'lastSummaryOf' or the
--   path-aware 'lastSummaryTouching'.
lastSummaryOfFrom :: StoreM m => ObjectHash -> Text -> StoreT m (Maybe (TickId, Summary))
lastSummaryOfFrom start kind = Tick.findTickFrom start $ \_ t -> do
  s <- fromTick @Summary t
  if summaryKind s == kind then Just (tickId t, s) else Nothing

-- | 'lastSummaryOfFrom', additionally requiring the match to be the tick
--   whose *own* pass actually contributed something for @path@, not
--   merely one that carries it forward unchanged -- skipping further back
--   past any same-kind tick that's really just carrying @path@ along
--   untouched, or belongs to a different file entirely. A shared kind
--   summarizing several files (e.g. @"prose\/chapter"@ across every
--   chapter) produces one interleaved series of ticks on the real branch,
--   only some of which ever genuinely wrote a given @path@; plain
--   'lastSummaryOfFrom'\/'lastSummaryOf' would happily return the most
--   recent tick of that kind regardless -- exactly wrong both as @path@'s
--   own current summary ('lastSummaryTouching') and as the per-occurrence
--   boundary 'summariesTouching' hands to clients.
--
--   "Contributed something" is two genuinely different things, checked
--   together as one signature compared against the *previous* same-kind
--   tick:
--
--   1. @path@'s own shallow content differs -- the ordinary case (a real
--      per-file rewrite, e.g. 'Storyteller.Writer.Agent.ChapterSummarizer').
--
--   2. The alternate chain's own tip, one hop into whatever it currently
--      resolves to, is a *different* nested 'Summary' tick (or gained\/lost
--      one entirely) -- this is the one 'summaryContent' alone can never
--      see: 'Storyteller.Writer.Agent.Summarizer.extendNestedAltChain's own
--      re-mint, once a deeper tier's pass writes something, mints its new
--      top-level tick *as a child of the tick already sitting at head*
--      (see 'Storyteller.Core.Git.atGeneric's own "already at target"
--      case) rather than replacing it -- so both the pre- and post-remint
--      ticks are real, permanent, same-kind ticks on the branch, and the
--      *only* difference between them is which alternate-chain tip the
--      later one's own 'summaryAltHead' now reaches, since the deeper
--      tier's own write never touches @path@'s shallow content at *this*
--      level at all.
--
--   Not 'summaryTickFor' (which resolves an alt-commit to the *earliest*
--   Summary tick that already reaches it via ancestry -- exactly wrong
--   here, since every pass of one @kind@ extends the same one continuous
--   alt-chain lineage, so an *earlier* pass's own altHead is trivially an
--   ancestor of everything any *later* pass ever writes, including a
--   genuine second pass over @path@ itself; 'summaryTickFor' would keep
--   resolving back to the very first pass ever, never the second one).
--   A same-kind tick whose alt-chain doesn't hold @path@ *at all* is
--   skipped the same way, continuing the walk older rather than giving up:
--   the kind's head-most tick covering @path@ is not guaranteed to be the
--   kind's head-most tick, full stop -- a per-file pass positions its new
--   'Summary' tick at that file's *own* last atom
--   ('Storyteller.Writer.Agent.Summarizer.runSummarizerForPath'), which can
--   sit behind a later tick of the same kind that never carried @path@
--   (its own pass forked from a point before @path@'s summary existed).
lastSummaryTouchingFrom :: StoreM m => ObjectHash -> Text -> FilePath -> StoreT m (Maybe (TickId, Summary))
lastSummaryTouchingFrom start kind path = do
  mCand <- lastSummaryOfFrom start kind
  case mCand of
    Nothing -> return Nothing
    Just cand@(tid, s) -> do
      mSig <- signatureFor s
      case mSig of
        Nothing  -> descendPast tid  -- doesn't cover path at all -- keep looking older
        Just sig -> settle sig cand
  where
    descendPast tid = do
      t <- Tick.readTypesTick (ObjectHash (unTickId tid))
      case tickParent t of
        Nothing         -> return Nothing
        Just (TickId p) -> lastSummaryTouchingFrom (ObjectHash p) kind path

    -- path's own shallow content, plus (if this kind ever nests) the id
    -- of whatever further same-kind Summary tick currently sits at this
    -- alt-chain's own tip -- see this function's own Haddock, point 2.
    signatureFor s = do
      mContent <- summaryContent s path
      case mContent of
        Nothing      -> return Nothing
        Just content -> do
          mNestedTip <- readAt (ObjectHash (unTickId (summaryAltHead s))) $ do
            tipTick <- Tick.getTypesTick
            return $ case fromTick @Summary tipTick of
              Just s' | summaryKind s' == kind -> Just (tickId tipTick)
              _                                -> Nothing
          return (Just (content, mNestedTip))

    settle sig (tid, s) = do
      candTick <- Tick.readTypesTick (ObjectHash (unTickId tid))
      case tickParent candTick of
        Nothing -> return (Just (tid, s))
        Just (TickId p) -> do
          mPrevCand <- lastSummaryOfFrom (ObjectHash p) kind
          case mPrevCand of
            Nothing -> return (Just (tid, s))
            Just prevCand@(_, prevS) -> do
              mPrevSig <- signatureFor prevS
              if mPrevSig == Just sig
                then settle sig prevCand
                else return (Just (tid, s))

-- | The most recent 'Summary' tick of @kind@ that genuinely covers
--   @path@ on the currently open chain -- the head-anchored entry point
--   every "read this file through its summary" consumer should use
--   ('Storyteller.Writer.Agent.SummaryAccess.zoomLevels',
--   'Storyteller.Writer.Agent.Summarizer.runSummarizerForPath's freshness
--   check). Not 'lastSummaryOf': the kind's head-most tick may not cover
--   @path@ at all (see 'lastSummaryTouchingFrom' -- this walk skips such
--   ticks), and treating it as @path@'s summary either loses an existing
--   summary entirely or mis-judges @path@'s freshness against a pass that
--   never processed it.
lastSummaryTouching :: StoreM m => Text -> FilePath -> StoreT m (Maybe (TickId, Summary))
lastSummaryTouching kind path = do
  h <- headHash
  lastSummaryTouchingFrom h kind path

-- | The tip of @tid@'s own *superseding run* within the chain whose tip is
--   @top@: @tid@ itself, or -- if edits have superseded it -- the youngest
--   of the immediately-adjacent same-@kind@ 'Summary' ticks stacked
--   directly on top of it. Adjacency is exactly what makes a tick a
--   superseding occurrence rather than an independent later pass (a real
--   pass always has its own span of source ticks in between -- see
--   'summariesTouching'), so this answers "this occurrence, in its current
--   (possibly hand-edited) state" -- what a client-held occurrence
--   reference ('Server.Writer.File.Connection.openTarget's hop targets)
--   should resolve to, or an edit made through such a reference would
--   vanish from its own holder's very next read. 'Nothing' if @tid@ isn't
--   a same-@kind@ 'Summary' reachable from @top@ at all (e.g. a reference
--   from before a remap replayed it away).
supersedingTipFrom :: StoreM m => ObjectHash -> Text -> TickId -> StoreT m (Maybe (TickId, Summary))
supersedingTipFrom top kind tid = go Nothing top
  where
    -- runTip: the youngest tick of the same-kind run the walk is currently
    -- inside, if any -- reset by every non-matching tick in between.
    go runTip h = do
      t <- Tick.readTypesTick h
      let self = case fromTick @Summary t of
            Just s | summaryKind s == kind -> Just (tickId t, s)
            _                              -> Nothing
          runTip' = case (self, runTip) of
            (Nothing, _)      -> Nothing
            (Just p, Nothing) -> Just p
            (Just _, Just _)  -> runTip
      if tickId t == tid
        then return runTip'
        else case tickParent t of
          Nothing         -> return Nothing
          Just (TickId p) -> go runTip' (ObjectHash p)

-- | Every tick strictly after @stopAt@ (exclusive), oldest-first, walking
--   back from HEAD all the way to root if @stopAt@ is 'Nothing' (or isn't
--   found in this chain's history at all). The one general "since this
--   point" walk everything else here is built from -- 'ticksSinceLastSummary'
--   is just this with @stopAt@ derived from 'lastSummaryOf'; a caller
--   that already knows a more specific stop point (e.g. exactly which
--   tick a particular file's compression is as fresh as -- see
--   'lastTouchedIn') uses this directly instead.
ticksSince :: StoreM m => Maybe TickId -> StoreT m [Tick]
ticksSince = ticksSinceFrom Nothing

-- | 'ticksSince', starting the walk from an arbitrary tick instead of head
--   -- 'ticksSince' is just this with @start = Nothing@. Internal only:
--   'summariesTouching'\/'occurrenceDelta' ask "what did *this
--   already-known tick* cover" rather than "what's new since @kind@'s last
--   summary right now" -- the two only coincide when the tick in question
--   happens to be the most recent one of its kind on the currently-open
--   chain.
ticksSinceFrom :: StoreM m => Maybe TickId -> Maybe TickId -> StoreT m [Tick]
ticksSinceFrom start stopAt = collect start []
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
availableSummaries mKind = do
  ticks <- ticksSince Nothing
  return
    [ (tickId t, s)
    | t <- reverse ticks
    , Just s <- [fromTick @Summary t]
    , maybe True (== summaryKind s) mKind
    ]

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

-- | This 'Occurrence's own delta: the concatenated content of every real
--   atom on @path@ added to the alt-chain strictly after
--   'occPrevAltHead' (exclusive) through this occurrence's own
--   'summaryAltHead' (inclusive) -- oldest first, same reconstruction
--   'Storyteller.Writer.Agent.SummaryAccess.unsummarizedTailSince' already
--   uses. This is a summary tick's own "data," in the same sense an
--   ordinary 'Storyteller.Core.Atom.Atom' tick's own data is just what it
--   appended, not the whole (ever-growing) file it's attached to --
--   'summaryContent' answers a different question ("what does this
--   alt-chain currently hold," useful for a plain preview of the current
--   state) and is the wrong thing to show as *this occurrence's* own
--   editable content.
occurrenceDelta :: StoreM m => Occurrence -> FilePath -> StoreT m Text
occurrenceDelta occ path =
  readAt (ObjectHash (unTickId (summaryAltHead (occSummary occ)))) $ do
    items <- ticksSinceFrom Nothing (occPrevAltHead occ)
    return (T.concat (map (contentFor path) items))

-- ---------------------------------------------------------------------------
-- Every historical occurrence, each independently anchored
-- ---------------------------------------------------------------------------

-- | One historical occurrence of a summary @kind@, as found by
--   'summariesTouching' -- everything a client needs to render it as its
--   own inline annotation *and* open its own "what's new since last time"
--   view, without picking/re-deriving any of this from the rest of the
--   list.
data Occurrence = Occurrence
  { occTickId  :: TickId
    -- ^ this occurrence's own real 'Summary' tick id.
  , occSummary :: Summary
  , occAnchor  :: TickId
    -- ^ the last real atom on @path@'s own history this occurrence
    --   covers -- where a client positions its inline annotation.
  , occLowerBound :: Maybe TickId
    -- ^ the *previous* (older) occurrence's own 'occAnchor', if any --
    --   the exclusive lower bound of what this occurrence covers on
    --   @path@'s own real history.
  , occPrevAltHead :: Maybe TickId
    -- ^ the *previous* occurrence's own 'summaryAltHead', if any -- the
    --   exclusive lower bound of what's *new in the alt-chain itself*
    --   for this occurrence (its own delta: exactly the tick(s) this
    --   pass added -- typically one atom, but a hand-edited note or
    --   swipe in between passes counts too), as distinct from
    --   'occLowerBound'/'occAnchor', which bound @path@'s own *real*
    --   history, not the alt-chain's. A summary tick's own "data," in
    --   the same sense an ordinary 'Storyteller.Core.Atom.Atom' tick's
    --   own data is just what it appended -- not the whole (ever-growing)
    --   file it's attached to -- is this delta, not the alt-chain's full
    --   accumulated content ('summaryContent' reads the latter, useful
    --   for a plain preview, but the wrong thing to edit against).
  } deriving (Show, Eq)

-- | Every 'Summary' tick of @kind@ that genuinely contributed something for
--   @path@ (the same "contributed something" signature
--   'lastSummaryTouchingFrom' already tests), oldest-first, each an
--   'Occurrence' carrying its own two independent boundaries (see that
--   type's own Haddock) -- handed back directly, not left for a client to
--   re-derive by sorting/searching this same list back apart, precisely so
--   opening a specific occurrence's own view is a direct lookup by the
--   clicked tick's own id, never a search over "all occurrences of this
--   kind": there is exactly one right answer for a specific tick, and
--   nothing to pick between.
--
--   A tick whose own span adds no real atom on @path@ *supersedes* its
--   predecessor -- the hand-edit case ('Server.Writer.File.Connection's
--   own @mintSummaryTick@\/@remintHop@ mint the new 'Summary' tick
--   directly after the pass being edited, so the span between them is
--   empty by construction), and the nested-tier re-mint
--   ('Storyteller.Writer.Agent.Summarizer.extendNestedAltChain') likewise.
--   A superseding run is *one* occurrence, not several: the edit happened
--   /to/ the pass, not after it. The merged occurrence keeps the base
--   tick's identity ('occTickId' -- stable, so a client-held reference
--   survives any number of edits; opening it resolves to the run's tip
--   anyway, see 'supersedingTipFrom') and the base's anchor\/bounds, but
--   carries the *tip's* own 'Summary' -- so its content and delta always
--   reflect the current, edited state. Only a tick with no predecessor
--   *and* no atom at all is dropped -- a path that never had a real atom
--   has nowhere to anchor anything.
--
--   Walks the currently-open chain from HEAD back to root, calling
--   'lastSummaryTouchingFrom' once per occurrence found (each call scoped
--   to start just behind the previous occurrence), so nesting (a coarser
--   tier's own tick living on a finer tier's own alternate chain -- see
--   this module's own Haddock) needs no special handling here at all: a
--   caller that wants a *nested* tier's own occurrences just calls this
--   again from within a storage scope opened at the finer tier's own
--   'summaryAltHead'.
summariesTouching :: StoreM m => Text -> FilePath -> StoreT m [Occurrence]
summariesTouching kind path = do
  mFirst <- lastSummaryTouching kind path
  reverse <$> go mFirst
  where
    go Nothing = return []
    go (Just (tid, s)) = do
      t <- Tick.readTypesTick (ObjectHash (unTickId tid))
      mPrev <- case tickParent t of
        Nothing         -> return Nothing
        Just (TickId p) -> lastSummaryTouchingFrom (ObjectHash p) kind path
      items <- ticksSinceFrom (tickParent t) (fst <$> mPrev)
      rest  <- go mPrev
      let prevAlt = summaryAltHead . snd <$> mPrev
      return $ case (lastAtomId items, rest) of
        -- the ordinary case: this pass's own span contains real atoms.
        (Just anchor, _)        -> Occurrence tid s anchor (occAnchor <$> listToMaybe rest) prevAlt : rest
        -- no new span of its own: supersedes its predecessor (see the
        -- Haddock above) -- one occurrence, the predecessor's identity
        -- and bounds read at this, newer, tip.
        (Nothing, older : more) -> older { occSummary = s } : more
        -- no predecessor and no atom ever: nothing to anchor to.
        (Nothing, [])           -> rest

    lastAtomId items = case [ tickId tk | tk <- items, isOwnAtom tk ] of
      [] -> Nothing
      xs -> Just (last xs)

    isOwnAtom tk = case fromTick @Atom tk of
      Just (Atom f _) -> f == path
      Nothing         -> False
