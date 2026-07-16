{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Reconciling ambient working-tree content back into the chain --
-- 'commitWorktree'\/'commitFiles'\/'commitFile' and the matching machinery
-- underneath them ('foldInto' and friends). Split out of "Storage.Ops"
-- (which re-exports the public entry points, so a consumer importing that
-- already has these): the mutating operations there each commit one
-- caller-described change, while everything here answers a harder
-- question -- given a whole file's new content, *which* atoms changed --
-- and owns the conservative classification rules for it.
--
-- Reconciliation is conservative: only trimming (removing some of an
-- atom's own original bytes, from its front and\/or back -- never its
-- middle) can change an atom's classification. Padding alone, with no
-- trim, is indistinguishable from an adjacent insertion and so is never
-- attributed to an untouched atom.
--
--   * Untouched (no trim recovered) -> kept as-is: same tick id untouched.
--   * Trimmed, nonzero content remaining (after folding in any
--     immediately adjacent new bytes) -> changed: a same-position
--     replacement.
--   * Trimmed to nothing -> dropped.
--   * New content that isn't absorbed by an adjacent trimmed atom ->
--     a standalone new atom, inserted after whatever currently precedes
--     it.
--   * Zero-length atoms (a 'Storyteller.Core.Create.createFile'
--     introduction, the empty atom a fully-emptied file keeps) record
--     *presence*, not content -- content matching can't recover anything
--     measurable from them, so their fate follows the file's instead:
--     kept while the file exists, dropped only once it's gone.
--
-- A deletion marker ('Storage.Core.removedTagKey') is not part of any of
-- this: reconciliation only ever sees the path's *current lifetime* --
-- everything after its most recent marker -- so a marker is never
-- classified, never dropped, and the previous lifetime it seals off is
-- never touched.
--
-- Only 'store'\/'drop'\/'at' ever run; the ambient tree itself is never
-- written to here -- this only changes what the chain, from here on,
-- compares it against (the same shape as 'reset', just applied by
-- rewriting the chain instead of reloading the tree).
module Storage.Reconcile
  ( commitWorktree
  , commitFiles
  , commitFile
  , saveFile
  ) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad (foldM)
import Control.Monad.State.Strict (lift)
import Data.Array (Array, listArray, (!))
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Storage.Core
import Storage.FS (list, exists)
import Storage.Query (hasAnyAtom, atomTrackedAmong)

-- | Overwrite @path@'s ambient content wholesale and reconcile it against
--   its atom chain via 'commitFile' -- the "raw edit" entry point (a UI
--   editor that just hands back the whole file, not individual atom
--   edits). Unlike 'Storage.Ops.addBinary' (an opaque deposit, no
--   reconciliation at all) this keeps 'commitFile's usual
--   preserve-unchanged-atoms diff, so a caller pasting back a mostly-
--   unmodified file still gets the same in-place update as any other
--   reconciliation path, not a full rewrite.
saveFile :: StoreM m => FilePath -> Text -> StoreT m ()
saveFile path content = do
  writeFile path (TE.encodeUtf8 content)
  commitFile path

-- | Every file touched by this reconciliation: everything currently in
--   the ambient tree, plus everything head already has committed --
--   necessary for a file that's been removed from the ambient tree
--   entirely (absent from 'list', but still present at head) to still
--   get visited, so 'commitFile' can see its target content is empty and
--   drop its whole history, rather than silently leaving stale atoms
--   behind because nothing ever looked at that path again.
commitWorktree :: StoreM m => StoreT m ()
commitWorktree = do
  ambient   <- list
  committed <- inWorktree list
  commitPathSet (Set.toList (Set.fromList ambient `Set.union` Set.fromList committed))

-- | Reconcile only the given files' working-tree content against their own
--   atom history, rather than every file in the branch -- same rule as
--   'commitWorktree', just scoped to a caller-chosen subset. Still needs
--   its own 'syncOpaqueContent' pass, same reason 'commitWorktree' does:
--   a binary/non-UTF8 path in the set (e.g. an uploaded portrait) is
--   deliberately left untouched by 'commitFile' itself, and wouldn't
--   otherwise ever land in a real commit at all.
commitFiles :: StoreM m => [FilePath] -> StoreT m ()
commitFiles = commitPathSet

-- | The shared tail of 'commitWorktree'\/'commitFiles': answer "which of
--   these are atom-tracked" for every path in *one* chain walk
--   ('atomTrackedAmong') instead of one 'hasAnyAtom' walk per path, then
--   reconcile each and sweep up whatever's left ('syncOpaqueContent').
commitPathSet :: StoreM m => [FilePath] -> StoreT m ()
commitPathSet paths = do
  tracked <- atomTrackedAmong paths
  mapM_ (\p -> commitFileKnown (p `Set.member` tracked) p) paths
  syncOpaqueContent

-- | Whatever the per-path loop above left untouched -- any path that was
--   never atom-tracked and isn't valid UTF-8 (a binary file dropped in by
--   hand, or a whole pre-existing repo adopted wholesale as a
--   StoryStorage branch) -- still needs to actually land in a real commit
--   to persist at all; 'commitFile' deliberately skips such paths rather
--   than trying to track them individually (see its own Haddock, and the
--   'Tick' module Haddock's 'Opaque' case: we don't know, and don't need
--   to know, how many paths that covers). By now every atom-tracked path
--   *should* already match what its own reconciliation just committed, so
--   in the ordinary case any remaining difference is exactly that
--   untracked content -- one opaque commit adopts all of it at once, no
--   per-path enumeration needed.
--
--   But "should" is exactly the word an actual reconciliation bug breaks:
--   'Opaque' means "content our own system didn't introduce" (see 'Tick's
--   own Haddock), so it must never become the accidental fallback for
--   content that *is* ours, just still unreconciled because something
--   upstream of here has a bug. Rather than trust that every atom-tracked
--   path was already handled, this explicitly re-checks: if any leftover
--   difference still belongs to an atom-tracked path, that's a defect in
--   the per-path reconciliation above, not adoptable external content, and
--   it fails loudly instead of silently laundering the mismatch into an
--   anonymous commit that would otherwise look exactly like a legitimate
--   external change.
syncOpaqueContent :: StoreM m => StoreT m ()
syncOpaqueContent = do
  current   <- getAmbientTree
  committed <- inWorktree getAmbientTree
  case divergentPaths current committed of
    []    -> return ()
    diffs -> do
      tracked <- Set.toList <$> atomTrackedAmong diffs
      case tracked of
        [] -> () <$ store (Opaque [])
        _  -> fail
          (  "syncOpaqueContent: refusing to fold already atom-tracked path(s) into an "
          <> "opaque commit -- this means their own reconciliation left them unresolved, "
          <> "which is a bug, not external content: " <> List.intercalate ", " tracked
          )

-- | Every path where two working trees disagree -- present in only one,
--   or present in both with different content.
divergentPaths :: WorkingTree -> WorkingTree -> [FilePath]
divergentPaths a b =
  [ p | p <- Set.toList (Map.keysSet a `Set.union` Map.keysSet b)
      , Map.lookup p a /= Map.lookup p b ]

-- | Reconcile one file's ambient content against its committed atom
--   history -- see the module Haddock for the classification rule. A
--   path with no atom history at all is introduced fresh, *if* its
--   ambient content is valid UTF-8 -- otherwise (a binary file dropped in
--   by hand, e.g. via the git CLI, or a whole pre-existing repo adopted
--   wholesale as a StoryStorage backing branch) it's left entirely
--   untouched: no tick, no tracking, just an ordinary file sitting in the
--   tree, same as it would be in any git repo that never heard of atoms.
--   A path that's *already* atom-tracked ('hasAnyAtom') has no such
--   escape hatch -- its own invariant (content is the fold of its atoms)
--   is load-bearing for every other operation here, so tampering with it
--   externally into something non-UTF8 surfaces as a loud failure
--   ('readWorking'), not a silent skip; that data needs a human, not a
--   graceful fallback.
commitFile :: StoreM m => FilePath -> StoreT m ()
commitFile path = do
  tracked <- hasAnyAtom path
  commitFileKnown tracked path

-- | 'commitFile' with the path's atom-tracked status already known --
--   so a batch caller ('commitPathSet') that answered it for every path
--   in one walk doesn't pay a separate walk per path here.
commitFileKnown :: StoreM m => Bool -> FilePath -> StoreT m ()
commitFileKnown tracked path = do
  if not tracked
    then do
      present <- exists path
      bytes   <- if present then readFile path else return BS.empty
      case TE.decodeUtf8' bytes of
        Left _     -> return ()
        Right text -> storeNewFile path text
    else do
      present <- exists path
      target  <- readWorking path
      foldInto path present target
      -- A target that reconciles down to nothing collapses every gap to
      -- empty too (gaps are substrings of target, so if it's empty they
      -- all are), which is exactly the condition under which 'foldInto'
      -- drops every atom and emits no standalone replacement -- the path
      -- would otherwise vanish from the tree entirely. That's correct
      -- when @path@ is genuinely gone from the ambient tree (see
      -- 'commitWorktree'), but wrong when it's still there with merely
      -- empty content -- the same case 'storeNewFile' already handles
      -- for a path with no history at all, by always leaving an empty
      -- marker atom behind so presence survives even when content
      -- doesn't.
      h        <- headHash
      stillHas <- lift (readPathAt h path)
      case stillHas of
        Nothing | present -> () <$ store (Atom [] path [] "")
        _                 -> return ()

-- | @path@'s current ambient content as text -- empty if the path
--   doesn't exist there at all. Fails loudly on invalid UTF-8 rather
--   than silently corrupting content.
readWorking :: StoreM m => FilePath -> StoreT m Text
readWorking path = do
  already <- exists path
  bytes   <- if already then readFile path else return BS.empty
  case TE.decodeUtf8' bytes of
    Right t  -> return t
    Left err -> fail ("readWorking: " <> path <> " is not valid UTF-8: " <> show err)

-- | A file with no prior atom history at all: introduced as an empty
--   atom (the path's own introduction -- its diff is recovered by the
--   same 'Atom' handling as any other, with no separate "introduced this
--   path" tick kind needed), immediately followed by its real content,
--   if any.
storeNewFile :: StoreM m => FilePath -> Text -> StoreT m ()
storeNewFile path content = do
  _ <- store (Atom [] path [] "")
  if T.null content then return () else () <$ store (Atom [] path [] content)

-- ---------------------------------------------------------------------------
-- foldInto: reconcile a tracked path to `target`, one tick at a time
-- ---------------------------------------------------------------------------
--
-- Every tick already carries its own complete tree snapshot -- that's what
-- makes it a commit -- so "the file's content as of any given tick" is
-- never something that needs folding up from atoms; it's one direct
-- 'readPathAt' lookup away, at any point in the chain, independent of how
-- long the file's own history is or how busy the graph around it is.
-- 'foldInto' leans on that fact entirely: it walks head towards root one
-- tick at a time, and at each of @path@'s own atoms compares that atom's
-- own recorded content against its *direct parent's* real, already-
-- materialized content -- never a re-derived fold -- to decide, locally,
-- whether this one atom accounts for the rest of `target` on its own. The
-- moment it does, the walk stops right there: everything below is
-- already proven correct by direct comparison, not assumed.
--
-- The one case that can't be settled by looking at a single atom in
-- isolation -- a change whose boundary doesn't line up with any one
-- atom's own edges -- falls back to gathering the remaining stretch of
-- history and running the same whole-history matching 'reconcileFile'
-- (the type this replaces) used, over just that stretch.

-- | Reconcile @path@'s already-tracked history so it folds to @target@ --
--   the single entry point 'commitFile' calls once a path is known to
--   have at least one 'Atom' tick. Checks first: if head's own committed
--   content for @path@ already *is* @target@, this is a no-op (the common
--   case every untouched file in a 'commitWorktree' sweep takes) and nothing
--   below ever runs. @present@ only matters for the zero-length-atom
--   convention (see 'settleTrailingMarkers') -- otherwise this never
--   touches the ambient\/working tree at all, @target@ having already
--   been read once by the caller.
foldInto :: StoreM m => FilePath -> Bool -> Text -> StoreT m ()
foldInto path present target = do
  h    <- headHash
  full <- fullContentAt h path
  if full == target && not (not present && T.null target)
    then return ()
    else reconcileAtom path present target h

-- | @path@'s complete content at @h@'s own committed tree, decoded --
--   empty if @path@ isn't present there at all. A direct tree lookup, not
--   a fold over atoms.
fullContentAt :: StoreM m => ObjectHash -> FilePath -> StoreT m Text
fullContentAt h path = do
  mbBytes <- lift (readPathAt h path)
  case mbBytes of
    Nothing -> return T.empty
    Just bs -> case TE.decodeUtf8' bs of
      Right t  -> return t
      Left err -> fail ("foldInto: " <> path <> " is not valid UTF-8 at a prior commit: " <> show err)

-- | The walk proper: @h@ is wherever we've gotten to so far (initially
-- head), @target@ is what's left to explain by @h@ and everything below
-- it. Skips anything that isn't one of @path@'s own atoms without
-- touching it; stops at the lifetime boundary ('isRemoval') or root the
-- same way 'Storage.Query.atomHistory' does.
reconcileAtom :: StoreM m => FilePath -> Bool -> Text -> ObjectHash -> StoreT m ()
reconcileAtom path present target h = do
  (cd, t) <- lift (readCommitTick h)
  let mbParent = case commitParents cd of (p : _) -> Just p; [] -> Nothing
  case t of
    Atom _ p tags _ | p == path, isRemoval tags -> emitRemainder path h target
    Atom _ p _ c    | p == path, T.null c -> do
      -- A zero-length atom records presence, not content (see
      -- 'classify'): it can never be judged by comparing text, only by
      -- whether the file is still supposed to exist at all. Its exact
      -- position among other ticks for @path@ is likewise not something
      -- any invariant here cares about -- the one thing 'commitFile'
      -- promises is that folding every surviving atom for @path@,
      -- in chain order, reproduces @target@ exactly, and an empty atom
      -- contributes nothing to that fold no matter where it ends up.
      if present then return () else () <$ at h drop
      maybe (return ()) (reconcileAtom path present target) mbParent
    Atom _ p _ c | p == path -> do
      parentFull <- maybe (return T.empty) (`fullContentAt` path) mbParent
      resolveLocal path present target h c parentFull mbParent
    _ -> maybe (emitRemainder path h target) (reconcileAtom path present target) mbParent

-- | Decide @h@'s own atom (content @c@, direct parent content
--   @parentFull@) against @target@. If @parentFull@ is provably an exact
--   prefix of @target@ -- a direct string comparison, not an assumption
--   -- then everything below @h@ is already correct and whatever's left
--   of @target@ (@rest@) belongs entirely to @h@: resolve it in one local
--   action and stop, no further reads or writes needed at all below this
--   point. Otherwise, if @c@ itself survives untouched at @target@'s own
--   tail, peel it off and keep walking -- the common shape for an
--   untouched atom sitting above a deeper, still-unlocated change.
--   Anything else (a change whose edges don't land cleanly on this one
--   atom) needs the general matcher; see 'fallbackFrom'.
resolveLocal
  :: StoreM m
  => FilePath -> Bool -> Text -> ObjectHash -> Text -> Text -> Maybe ObjectHash -> StoreT m ()
resolveLocal path present target h c parentFull mbParent
  | q == T.length parentFull = do
      applyLocalEdit path h mbParent c (T.drop q target)
      maybe (return ()) (settleTrailingMarkers path present) mbParent
  | c `T.isSuffixOf` target =
      maybe (return ()) (reconcileAtom path present (T.dropEnd (T.length c) target)) mbParent
  | otherwise = fallbackFrom path present target h
  where
    q = commonPrefixLen parentFull target

-- | Apply whatever @h@'s own atom needs to become @rest@ (already proven
--   to be exactly what belongs here, front to back), classifying by
--   direct comparison rather than a search: unchanged, or -- if @c@
--   survives fully intact somewhere inside @rest@ -- untouched, with
--   whatever's newly on either side of it becoming its own standalone
--   tick(s) rather than folding into @h@ (an atom that wasn't itself
--   trimmed keeps its own id; see 'gapFates''s eligibility rule for why
--   only a *partially* surviving atom ever absorbs a neighboring gap).
--   Otherwise a trim\/rewrite, or a drop. A leading addition is inserted
--   at @mbParent@ instead of @h@ -- 'at' always inserts a new tick right
--   *after* wherever it's anchored, so anchoring one position lower is
--   what lands it textually before @h@ rather than after it.
applyLocalEdit :: StoreM m => FilePath -> ObjectHash -> Maybe ObjectHash -> Text -> Text -> StoreT m ()
applyLocalEdit path h mbParent c rest
  | rest == c = return ()
  | Just (front, back) <- splitAroundExact c rest = do
      if T.null back then return () else () <$ at h (store (Atom [] path [] back))
      case mbParent of
        Just par | not (T.null front) -> () <$ at par (store (Atom [] path [] front))
        _                              -> return ()
  | T.null rest = () <$ at h drop
  | otherwise   = () <$ at h (editTick (\old -> case old of
      Atom refs p tags _ -> return (Atom refs p tags rest)
      _                  -> fail "foldInto: matched tick isn't an atom (unreachable)"))

-- | @Just (before, after)@ if @needle@ occurs intact somewhere in
--   @haystack@ -- the text on either side of its first occurrence --
--   'Nothing' if it doesn't appear at all.
splitAroundExact :: Text -> Text -> Maybe (Text, Text)
splitAroundExact needle haystack = case T.breakOn needle haystack of
  (before, matched) | needle `T.isPrefixOf` matched -> Just (before, T.drop (T.length needle) matched)
  _                                                  -> Nothing

-- | Once a local resolution has decided nothing below @h@ needs touching,
--   still walk past any trailing zero-length atom for @path@ -- a
--   'Storyteller.Core.Create.createFile' marker, or an earlier
--   reconcile's own leftover presence marker -- since it's invisible to
--   every content-based check above (it never contributes any text) and
--   is only ever judged by @present@ (see 'classify').
settleTrailingMarkers :: StoreM m => FilePath -> Bool -> ObjectHash -> StoreT m ()
settleTrailingMarkers path present h = do
  (cd, t) <- lift (readCommitTick h)
  case t of
    Atom _ p _ c | p == path, T.null c -> do
      if present then return () else () <$ at h drop
      case commitParents cd of
        []       -> return ()
        (par : _) -> settleTrailingMarkers path present par
    _ -> return ()

-- | The length of the longest common prefix of @a@ and @b@ -- a direct
--   O(min) comparison, not a search.
commonPrefixLen :: Text -> Text -> Int
commonPrefixLen a b = case T.commonPrefixes a b of
  Just (p, _, _) -> T.length p
  Nothing        -> 0

-- | Whatever's left of @target@ once @anchor@ (the lifetime boundary --
--   a deletion marker or root) is reached: a single new atom right after
--   it, or nothing if there's nothing left to place.
emitRemainder :: StoreM m => FilePath -> ObjectHash -> Text -> StoreT m ()
emitRemainder path anchor remaining
  | T.null remaining = return ()
  | otherwise         = () <$ at anchor (store (Atom [] path [] remaining))

-- | Every atom on @path@'s current lifetime from @h@ (inclusive) down to
--   the boundary, oldest first, plus that boundary's own hash -- the same
--   shape 'Storage.Query.atomHistory' projects, just starting wherever
--   'reconcileAtom' has already walked to instead of repeating that walk
--   from head.
historyFrom :: StoreM m => FilePath -> ObjectHash -> StoreT m ([(ObjectHash, Text)], ObjectHash)
historyFrom path h0 = go h0 []
  where
    go h acc = do
      (cd, t) <- lift (readCommitTick h)
      case t of
        Atom _ p tags content | p == path ->
          if isRemoval tags
            then return (acc, h)
            else continue cd h ((h, content) : acc)
        _ -> continue cd h acc
    continue cd h acc = case commitParents cd of
      []      -> return (acc, h)
      (p : _) -> go p acc

-- | The general case 'resolveLocal' can't settle from a single atom's own
--   edges: a change spanning more than one atom's own boundary. Falls
--   back to gathering the remaining stretch of history from @h@ down to
--   @path@'s lifetime boundary and running the same whole-history
--   matching the old, always-O(history) reconciler used (see
--   'matchAtoms'\/'gapFates'\/'classify'\/'finalAtomContents') -- reached
--   only once the cheap local checks can no longer prove anything on
--   their own, which the QuickCheck coverage confirms is rare, not
--   the common shape.
--
--   Each match's own id is resolved through the store's remap table right
--   before it's used, rather than re-walked by position: earlier atoms in
--   this same fold are mutated oldest-first, and 'at' only ever cascades
--   *forward* (towards head) from wherever it's applied, so a later
--   atom's captured hash can already be stale by the time its own turn
--   comes -- 'resolveId' answers what it's since become in O(1), the same
--   table 'at' itself already maintains as it replays.
fallbackFrom :: StoreM m => FilePath -> Bool -> Text -> ObjectHash -> StoreT m ()
fallbackFrom path present target h = do
  (history, anchor0) <- historyFrom path h
  let matches  = matchAtoms (map snd history) target
      n        = length matches
      gaps     = gapContents matches target
      fates    = gapFates matches gaps
      contents = finalAtomContents matches target gaps fates
      outs     = classify present matches contents
      perAtom  = zip6 (map fst history) matches (List.take n gaps) (List.take n fates) contents outs
      (tailGap, tailFate) = case (List.drop n gaps, List.drop n fates) of
        (g : _, f : _) -> (g, f)
        _              -> (T.empty, Standalone)
  anchor <- foldM (step path) anchor0 perAtom
  () <$ emitStandaloneGap path anchor tailGap tailFate
  where
    step p anchor (origHash, _m, gap, fate, content, outcome) = do
      anchor1 <- emitStandaloneGap p anchor gap fate
      origId  <- resolveId origHash
      case outcome of
        Kept    -> return origId
        Dropped -> anchor1 <$ at origId drop
        Changed -> at origId $ editTick $ \old -> case old of
          Atom refs _ tags _ -> return (Atom refs p tags content)
          _                  -> fail "foldInto: matched tick isn't an atom (unreachable)"

-- | A gap that folded onto a neighbor was already absorbed into that
--   atom's own content by 'finalAtomContents' -- nothing to do here. A
--   standalone gap becomes its own new atom, inserted right after
--   @anchor@ (whatever currently precedes it in the chain); returns
--   whatever should anchor the *next* insertion, which is this new
--   atom's own id when one was made, or @anchor@ unchanged otherwise.
emitStandaloneGap :: StoreM m => FilePath -> ObjectHash -> Text -> GapFate -> StoreT m ObjectHash
emitStandaloneGap _    anchor _       fate | fate /= Standalone = return anchor
emitStandaloneGap _    anchor content _                         | T.null content = return anchor
emitStandaloneGap path anchor content _                         =
  at anchor (store (Atom [] path [] content))

-- ---------------------------------------------------------------------------
-- Matching: recover each atom's surviving core from the target content
-- ---------------------------------------------------------------------------

data AtomMatch = AtomMatch
  { amOriginalLen :: Int
  , amCoreStart   :: Int  -- ^ offset into the target content
  , amCoreLen     :: Int  -- ^ 0 if the atom didn't survive at all
  }

data AtomOutcome = Kept | Changed | Dropped deriving Eq
data GapFate = FoldBack | FoldFront | Standalone deriving Eq

-- | Walk atoms oldest-first, anchoring each one's surviving core (a
--   contiguous, order-preserving substring of its original content) in
--   the target content via longest-common-substring search from the
--   current cursor onward. Trimming is only ever recognized at an atom's
--   front and\/or back -- a substring match is exactly that: no interior
--   deletions. Takes just each atom's own original content -- not its
--   id, which nothing here needs (ids are re-derived fresh, by position,
--   only once actually acting on one -- see 'fallbackFrom').
matchAtoms :: [Text] -> Text -> [AtomMatch]
matchAtoms origs target = go 0 origs
  where
    go _ [] = []
    go cursor (orig : rest) =
      let remaining      = T.drop cursor target
          (_, bOff, len) = longestCommonSubstring orig remaining
          coreStart      = cursor + bOff
          cursor'        = coreStart + len
      in AtomMatch (T.length orig) coreStart len : go cursor' rest

-- | Fully recovered, byte-for-byte. Only asked about atoms with nonempty
--   originals -- a zero-length original recovers nothing measurable from
--   *any* target ('longestCommonSubstring' short-circuits on empty input),
--   so 'classify' decides its fate from the file's own presence before
--   this question ever comes up.
isKept :: AtomMatch -> Bool
isKept m = amCoreLen m == amOriginalLen m

coreOf :: Text -> AtomMatch -> Text
coreOf target m = T.take (amCoreLen m) (T.drop (amCoreStart m) target)

-- | The N+1 stretches of target content between (and around) atom cores.
gapContents :: [AtomMatch] -> Text -> [Text]
gapContents matches target = go 0 matches
  where
    go cursor [] = [T.drop cursor target]
    go cursor (m : rest) =
      T.take (amCoreStart m - cursor) (T.drop cursor target)
        : go (amCoreStart m + amCoreLen m) rest

-- | Attribute each gap to a neighbor (folded) or mark it standalone: fold
--   onto the preceding atom's back if it's eligible, else the following
--   atom's front if eligible, else standalone. An atom is only eligible
--   if it's *partially* trimmed -- a nonempty core remains, but not the
--   whole original.
gapFates :: [AtomMatch] -> [Text] -> [GapFate]
gapFates matches gaps = zipWith3 fate3 gaps prevElig curElig
  where
    elig     = map (\m -> amCoreLen m > 0 && amCoreLen m < amOriginalLen m) matches
    prevElig = False : elig
    curElig  = elig ++ [False]
    fate3 gap prev cur
      | T.null gap = Standalone
      | prev       = FoldBack
      | cur        = FoldFront
      | otherwise  = Standalone

-- | Each atom's final content: its surviving core plus whatever gaps
--   folded onto its front\/back.
finalAtomContents :: [AtomMatch] -> Text -> [Text] -> [GapFate] -> [Text]
finalAtomContents matches target gaps fates =
  [ frontFold ff fg <> coreOf target m <> backFold bf bg
  | (m, ff, fg, bf, bg) <- zip5 matches frontFates frontGaps backFates backGaps ]
  where
    n          = length matches
    frontFates = List.take n fates
    frontGaps  = List.take n gaps
    backFates  = List.drop 1 fates
    backGaps   = List.drop 1 gaps
    frontFold fate gap = if fate == FoldFront then gap else T.empty
    backFold  fate gap = if fate == FoldBack  then gap else T.empty

-- | Classify each atom given its already-computed final content (core
--   plus any folded-in gap text -- see 'finalAtomContents'). A
--   zero-length original records presence, not content (see the module
--   Haddock): it is Kept -- same tick id, no churn -- as long as the file
--   itself survives, and Dropped only when the whole file is gone.
classify :: Bool -> [AtomMatch] -> [Text] -> [AtomOutcome]
classify filePresent matches contents =
  [ outcomeOf m fc | (m, fc) <- zip matches contents ]
  where
    outcomeOf m fc
      | amOriginalLen m == 0 = if filePresent then Kept else Dropped
      | isKept m             = Kept
      | T.null fc            = Dropped
      | otherwise            = Changed

-- | Longest common substring of two texts: returns the offset into each
--   (in characters) and the shared length. @(0, 0, 0)@ if either is empty
--   or there is no overlap. O(n*m) time and space -- fine for atom-sized
--   inputs; this is the one place that would need revisiting for very
--   large files.
longestCommonSubstring :: Text -> Text -> (Int, Int, Int)
longestCommonSubstring a b
  | n == 0 || m == 0 = (0, 0, 0)
  | otherwise        = (bestI - best, bestJ - best, best)
  where
    n = T.length a
    m = T.length b
    av, bv :: Array Int Char
    av = listArray (0, n - 1) (T.unpack a)
    bv = listArray (0, m - 1) (T.unpack b)
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (n, m)) [ cellAt i j | i <- [0 .. n], j <- [0 .. m] ]
    cellAt 0 _ = 0
    cellAt _ 0 = 0
    cellAt i j
      | av ! (i - 1) == bv ! (j - 1) = dp ! (i - 1, j - 1) + 1
      | otherwise                    = 0
    (best, bestI, bestJ) =
      List.foldl' pick (0, 0, 0) [ (dp ! (i, j), i, j) | i <- [1 .. n], j <- [1 .. m] ]
    pick acc@(bl, _, _) cand@(l, _, _) = if l > bl then cand else acc

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 (a:as) (b:bs) (c:cs) (d:ds) (e:es) = (a, b, c, d, e) : zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) = (a, b, c, d, e, f) : zip6 as bs cs ds es fs
zip6 _ _ _ _ _ _ = []
