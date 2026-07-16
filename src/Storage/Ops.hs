{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-facing operations built entirely from "Storage.Core"'s
-- primitives (store\/drop\/at\/readAt\/reset\/inWorktree\/readFile\/
-- writeFile) and "Storage.FS"'s ('list') -- nothing here reaches
-- around them, or touches the chain\/ambient tree any other way.
module Storage.Ops
  ( addAtom
  , addAtomWithRefs
  , deleteFile
  , append
  , findAtom
  , editAtom
  , editAtomAt
  , replaceAtom
  , setAtomHidden
  , addBinary
  , saveFile
  , commitWorktree
  , commitFile
  , commitFiles
  , atomHistory
  , hasAnyAtom
  , atomTrackedAmong
  , exists

    -- * Chain-editing operations -- position-aware moves\/merges\/splits
    -- over the whole chain, not just one file's own atom history
  , chainPositions
  , deleteTick
  , moveTick
  , mergeAtoms
  , splitTick
  , findCreationTick
  , renameFile
  , checkpointFile
  , saveFileAsNew
  ) where

import Prelude hiding (drop, readFile, writeFile, appendFile)

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
import qualified Storage.FS as FS
import Storage.FS (list, exists)

-- | Append @content@ to @path@ in the ambient tree, creating it if
--   absent: read whatever's there already (if anything), write the
--   extension.
appendFile :: StoreM m => FilePath -> Text -> StoreT m ()
appendFile path content = do
  already <- exists path
  old     <- if already then readFile path else return BS.empty
  writeFile path (old <> TE.encodeUtf8 content)

-- | Commit @content@ as a new atom at @path@, and append the same
--   content to the ambient tree -- two independent steps. The commit is
--   a real chain change ('store'); the append is a plain ambient-tree
--   write. If the ambient tree was already in sync with head, it's still
--   in sync afterward (the same bytes landed in both places); if it
--   wasn't, this doesn't touch whatever else was pending there.
addAtom :: StoreM m => FilePath -> Text -> StoreT m ObjectHash
addAtom = addAtomWithRefs []

-- | 'addAtom', additionally carrying cross-branch refs on the new atom --
--   for a caller (e.g. the tracker agent) that needs the committed tick
--   to reference other ticks, not just append content.
addAtomWithRefs :: StoreM m => [ObjectHash] -> FilePath -> Text -> StoreT m ObjectHash
addAtomWithRefs refs path content = do
  newHead <- store (Atom refs path [] content)
  appendFile path content
  return newHead

-- | Commit a whole-file deletion: an ordinary, empty 'Atom' tagged with
--   'removedTagKey' -- a forward event, not a rebase, so every earlier
--   atom on @path@ stays exactly where it was (see 'Storage.Core.store's
--   own Haddock). What actually communicates "this file is gone" to a
--   reader is @path@ dropping out of the tree, same as any other content
--   change; the tag itself only exists to give a content fold (see
--   'atomHistory') an efficient place to stop, not to be read as the
--   signal on its own.
--
--   Named to match the chain-level "delete" vocabulary
--   ('deleteTick'\/'deleteFileAtom'\/'Server.Core.File.deleteFile'), not
--   "Storage.FS"'s 'Storage.FS.remove' -- despite calling that internally
--   (to keep the ambient tree in sync, same convention 'addAtom' already
--   follows), the two are separate things: 'Storage.FS.remove' just drops
--   an entry from the ambient/scratch tree, with no chain effect of its
--   own; this commits a real, permanent tick.
deleteFile :: StoreM m => FilePath -> StoreT m ObjectHash
deleteFile path = do
  newHead <- store (Atom [] path [(removedTagKey, "true")] "")
  FS.remove path
  return newHead

-- | The nearest atom at or before @start@, walking backward through
--   parents. Fails once the chain runs out (root reached, no parent
--   left) without finding one.
findAtom :: StoreM m => ObjectHash -> StoreT m ObjectHash
findAtom start = do
  (cd, t) <- lift (readCommitTick start)
  case t of
    Atom {} -> return start
    _       -> case commitParents cd of
      []      -> fail "findAtom: no atom in history"
      (p : _) -> findAtom p

-- | Apply @f@ to the nearest atom's own content -- walking back from
--   head, skipping over anything since (notes and other bookkeeping
--   ticks aren't the concern here, only the last real content addition
--   is) -- keeping its cross-branch refs and chain position. Returns the
--   new head.
editAtom :: StoreM m => (Text -> Text) -> StoreT m ObjectHash
editAtom f = do
  h      <- headHash
  target <- findAtom h
  at target $ editTick $ \old -> case old of
    Atom refs path tags content -> return (Atom refs path tags (f content))
    _                            -> fail "editAtom: findAtom returned a non-atom (unreachable)"

-- | Replace the nearest atom's content outright -- 'editAtom' with a
--   constant function.
replaceAtom :: StoreM m => Text -> StoreT m ObjectHash
replaceAtom = editAtom . const

-- | Replace a specific atom's content in place, wherever it sits in
--   history -- the arbitrary-id generalization of 'editAtom' (which only
--   ever targets the atom nearest head). Preserves the atom's own refs
--   and chain position.
editAtomAt :: StoreM m => ObjectHash -> Text -> StoreT m ObjectHash
editAtomAt target content = at target $ editTick $ \old -> case old of
  Atom refs path tags _ -> return (Atom refs path tags content)
  _                      -> fail ("editAtomAt: not an atom: " <> T.unpack (unObjectHash target))

-- | Set or clear a specific atom's own "hide" tag in place -- same
--   arbitrary-id, preserves-refs-and-position shape as 'editAtomAt'. The
--   tag stays with the tick permanently (see "Storage.Core"'s 'atomTags'),
--   not a reference to it from elsewhere in the chain the way a note is.
setAtomHidden :: StoreM m => ObjectHash -> Bool -> StoreT m ObjectHash
setAtomHidden target hidden = at target $ editTick $ \old -> case old of
  Atom refs path tags content -> return (Atom refs path (setTag tags) content)
  _                            -> fail ("setAtomHidden: not an atom: " <> T.unpack (unObjectHash target))
  where
    setTag tags
      | hidden    = ("hide", "true") : filter ((/= "hide") . fst) tags
      | otherwise = filter ((/= "hide") . fst) tags

-- | Introduce or replace @path@ as a deliberately binary asset: write
--   @content@ into the ambient tree (replacing whatever was there, unlike
--   an atom's append) and commit a path-aware 'Binary' tick for it in one
--   atomic step -- bypassing 'commitFile's own decode-and-guess
--   reconciliation entirely. Use this when the caller already knows the
--   content isn't prose (e.g. an uploaded image); 'commitFiles'\/
--   'commitWorktree' remain the right call for content whose atom-vs-
--   binary status still needs to be discovered from the bytes themselves,
--   with the anonymous 'Storage.Core.Opaque' tick as their safe fallback.
addBinary :: StoreM m => FilePath -> BS.ByteString -> StoreT m ObjectHash
addBinary path content = do
  writeFile path content
  store (Binary [] path)

-- | Overwrite @path@'s ambient content wholesale and reconcile it against
--   its atom chain via 'commitFile' -- the "raw edit" entry point (a UI
--   editor that just hands back the whole file, not individual atom
--   edits). Unlike 'addBinary' (an opaque deposit, no reconciliation at
--   all) this keeps 'commitFile's usual preserve-unchanged-atoms diff, so
--   a caller pasting back a mostly-unmodified file still gets the same
--   in-place update as any other reconciliation path, not a full rewrite.
saveFile :: StoreM m => FilePath -> Text -> StoreT m ()
saveFile path content = do
  writeFile path (TE.encodeUtf8 content)
  commitFile path

-- | Append @content@ to @path@ as a single atom, ensuring it ends with a
--   newline first -- an appended atom is one text block on disk, and a
--   block should end its line. The normalizing counterpart to 'addAtom',
--   for a caller appending free-form prose rather than an already-shaped
--   diff.
append :: StoreM m => FilePath -> Text -> StoreT m ObjectHash
append path content = addAtom path (ensureTrailingNewline content)
  where
    ensureTrailingNewline t
      | "\n" `T.isSuffixOf` t = t
      | otherwise             = t <> "\n"

-- ---------------------------------------------------------------------------
-- commitWorktree: fold the ambient tree into the chain
-- ---------------------------------------------------------------------------
--
-- Reconciles each file's ambient content against its own committed atom
-- history, conservatively: only trimming (removing some of an atom's own
-- original bytes, from its front and\/or back -- never its middle) can
-- change an atom's classification. Padding alone, with no trim, is
-- indistinguishable from an adjacent insertion and so is never
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
-- this: 'atomHistory' hands reconciliation only the path's *current
-- lifetime* -- everything after its most recent marker -- so a marker is
-- never classified, never dropped, and the previous lifetime it seals
-- off is never touched.
--
-- Only 'store'\/'drop'\/'at' ever run; the ambient tree itself is never
-- written to here -- this only changes what the chain, from here on,
-- compares it against (the same shape as 'reset', just applied by
-- rewriting the chain instead of reloading the tree).

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
--   history -- see the section Haddock for the classification rule. A
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

-- | Whether @path@ has ever had at least one 'Atom' tick -- a short-
--   circuiting existence check, not the full 'atomHistory' walk. By the
--   "once atom, always atom" invariant, finding a single match already
--   proves @path@ is atom-governed from here on -- there's no need to see
--   the rest of its history (let alone reach root) just to answer this.
hasAnyAtom :: StoreM m => FilePath -> StoreT m Bool
hasAnyAtom path = Set.member path <$> atomTrackedAmong [path]

-- | Which of @paths@ have ever had at least one 'Atom' tick --
--   'hasAnyAtom' for a whole batch, sharing one walk: each commit is
--   read once however many paths are still open, and the walk stops the
--   moment every path has been answered (the same short-circuit
--   'hasAnyAtom' has always had, generalized). The worst case -- some
--   path never atom-tracked at all -- is one full walk *total*, where
--   asking per path paid it once per such path.
atomTrackedAmong :: StoreM m => [FilePath] -> StoreT m (Set.Set FilePath)
atomTrackedAmong paths = headHash >>= go (Set.fromList paths) Set.empty
  where
    go pending found h
      | Set.null pending = return found
      | otherwise = do
          (cd, t) <- lift (readCommitTick h)
          let (pending', found') = case t of
                Atom _ p _ _ | Set.member p pending
                  -> (Set.delete p pending, Set.insert p found)
                _ -> (pending, found)
          case commitParents cd of
            []        -> return found'
            (par : _) -> go pending' found' par

-- | This file's own *current lifetime*, oldest first: every atom strictly
--   after @path@'s most recent deletion event (see
--   'Storage.Core.removedTagKey'), or its whole history if it has none.
--   A deletion marker is a real, permanent tick -- 'Tick.fileTicksOf'
--   still walks straight past it for a client wanting the full timeline
--   -- but here it is a *boundary, not content*: what's before it belongs
--   to a previous life of this path, and the marker itself is never part
--   of the history handed back, so reconciliation can neither classify
--   nor drop it. (Dropping a marker would splice the previous lifetime's
--   content back into the tree underneath every atom after it.)
atomHistory :: StoreM m => FilePath -> StoreT m [(ObjectHash, Text)]
atomHistory path = do
  atoms <- lifetimeAtoms path
  return [ (h, content) | (h, Atom _ _ _ content) <- atoms ]

-- | Every atom in @path@'s current lifetime, oldest first, each with its
--   own id -- the walk 'atomHistory' (content only) and
--   'checkpointFile' (ids and tags too) both project from. Two
--   boundaries end it, whichever is hit first, both strictly about the
--   *current* lifetime:
--
--   * a removal marker ('Storage.Core.removedTagKey') -- the previous
--     lifetime it seals off is not this one;
--   * the lifetime's own creation atom: an atom on @path@ whose parent
--     commit doesn't have @path@ in its tree at all (the same tree-truth
--     definition 'findCreationTick' uses). Everything below it is either
--     unrelated, or an earlier lifetime that ended *without* a marker
--     (an external\/'Storage.Core.Opaque' deletion) -- and, crucially,
--     it's what stops a never-deleted file's walk at its creation
--     instead of paying for the entire chain below it.
--
--   The creation check costs one 'lookupPathInTree' (O(path depth)) per
--   on-@path@ atom -- never per commit; commits that aren't @path@'s own
--   atoms are passed over with a single read.
lifetimeAtoms :: StoreM m => FilePath -> StoreT m [(ObjectHash, Tick)]
lifetimeAtoms path = do
  h <- headHash
  (cd, t) <- lift (readCommitTick h)
  go h cd t []
  where
    go h cd t acc = case t of
      Atom _ p tags _ | p == path, isRemoval tags -> return acc
      a@(Atom _ p _ _) | p == path -> case commitParents cd of
        [] -> return ((h, a) : acc)
        (par : _) -> do
          (pcd, pt) <- lift (readCommitTick par)
          present   <- lift (lookupPathInTree (commitTree pcd) path)
          case present of
            Nothing -> return ((h, a) : acc)   -- @h@ is the creation atom
            Just _  -> go par pcd pt ((h, a) : acc)
      _ -> case commitParents cd of
        [] -> return acc
        (par : _) -> do
          (pcd, pt) <- lift (readCommitTick par)
          go par pcd pt acc

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

-- | The chain's very first commit -- the anchor for a 'moveTick' to the
--   very front, and 'lifetimeAnchor''s fallback for a path that has never
--   been deleted.
rootHash :: StoreM m => StoreT m ObjectHash
rootHash = headHash >>= go
  where
    go h = do
      cd <- lift (readCommit h)
      case commitParents cd of
        []      -> return h
        (p : _) -> go p

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
-- same way 'atomHistory' does.
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
--   shape 'atomHistory'\/'lifetimeAnchor' used to build together, just
--   starting wherever 'reconcileAtom' has already walked to instead of
--   repeating that walk from head.
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
--   their own, which the QuickCheck coverage below confirms is rare, not
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
--   id, which nothing here needs (see 'reconcileFile': ids are re-derived
--   fresh, by position, only once actually acting on one).
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
--   zero-length original records presence, not content (see the section
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

-- ---------------------------------------------------------------------------
-- Chain-editing operations
-- ---------------------------------------------------------------------------
--
-- Position-aware moves/merges/splits, over the whole chain rather than one
-- file's own atom history. Built entirely from 'at'/'drop'/'store'/'follow'
-- -- no separate draft type is needed the way "Storyteller.Core.StorageMonad"
-- needed 'TDraft': a 'Tick' already carries its own diff (an 'Atom's
-- 'atomContent' *is* the append), so popping and re-storing one directly is
-- both the extraction and the reinsertion step.

-- | Reconcile only the given files' working-tree content against their own
--   atom history, rather than every file in the branch -- same rule as
--   'commitWorktree', just scoped to a caller-chosen subset. Still needs
--   its own 'syncOpaqueContent' pass, same reason 'commitWorktree' does:
--   a binary/non-UTF8 path in @paths@ (e.g. an uploaded portrait) is
--   deliberately left untouched by 'commitFile' itself, and wouldn't
--   otherwise ever land in a real commit at all.
commitFiles :: StoreM m => [FilePath] -> StoreT m ()
commitFiles = commitPathSet

-- | Every non-root tick reachable from head, oldest first -- the position
--   vocabulary 'chainPositions'\/'moveTick'\/'mergeAtoms' all share.
contentChain :: StoreM m => StoreT m [(ObjectHash, Tick)]
contentChain = List.drop 1 <$> follow [] (\acc h t -> ((h, t) : acc, True))

-- | @oid@'s own position in @chain@ (0-indexed, oldest first). Fails if
--   @oid@ isn't a member at all -- an unexceptional caller error (an id
--   from outside this branch, or a stale one nothing has resolved), not a
--   normal outcome to branch on.
findPos :: StoreM m => ObjectHash -> [(ObjectHash, Tick)] -> StoreT m Int
findPos oid chain = case List.findIndex ((== oid) . fst) chain of
  Just i  -> return i
  Nothing -> fail ("findPos: not found: " <> T.unpack (unObjectHash oid))

-- | Resolve each id's position among content ticks (root excluded),
--   oldest-first -- for a caller that needs to process a batch of ids in
--   chain order without walking the chain once per id. One position map
--   built from the one chain walk, then a lookup per id -- not a
--   'findPos' list scan per id on top of it.
chainPositions :: StoreM m => [ObjectHash] -> StoreT m [(ObjectHash, Int)]
chainPositions oids = do
  chain <- contentChain
  let posMap = Map.fromList (zip (map fst chain) [0 ..])
  mapM (\oid -> case Map.lookup oid posMap of
          Just i  -> return (oid, i)
          Nothing -> fail ("chainPositions: not found: " <> T.unpack (unObjectHash oid)))
       oids

-- | Remove @tid@ from the chain entirely, replaying the tail on top of
--   whatever comes before it.
deleteTick :: StoreM m => ObjectHash -> StoreT m ()
deleteTick tid = () <$ at tid drop

-- | Move @tid@ to immediately after @mAfter@ (@Nothing@ = move to front).
--   Returns @tid@'s new id.
--
--   Backward (tid currently after target):
--     @at tid $ do { t <- drop; at after (store t) }@
--
--   Forward (tid currently before target):
--     @at after $ do { t <- at tid drop; store t }@
--
--   Both are a single nested 'at' -- one coherent rebase pass; every id
--   this displaces (including @tid@ itself) gets its remap logged by 'at'
--   \/'editTick' as it goes -- see "Storage.Core"'s 'resolveId'.
moveTick :: StoreM m => ObjectHash -> Maybe ObjectHash -> StoreT m ObjectHash
moveTick tid mAfter = do
  chain    <- contentChain
  tidPos   <- findPos tid chain
  afterPos <- case mAfter of
    Nothing  -> return (-1)
    Just aid -> findPos aid chain

  checkMoveOrder tid tidPos afterPos chain

  root <- rootHash
  let resolvedAfter = maybe root id mAfter

  -- 'at's own fallback only recognizes a single tick replaced in place;
  -- this is a composite move (an outer 'at' whose action nests a further
  -- 'at'\/'drop'\/'store' of its own), so @tid@'s own new id has to be
  -- logged explicitly -- see "Storage.Core"'s 'at' Haddock.
  if tidPos > afterPos
    then at tid $ do
      t     <- drop
      newId <- at resolvedAfter (store t)
      logRemap tid newId
      return newId
    else at resolvedAfter $ do
      t     <- at tid drop
      newId <- store t
      logRemap tid newId
      return newId

-- | @tid@ may only move to a position that keeps every reference it makes
--   (via its own 'tickRefs') behind it, and doesn't jump past anything
--   that references @tid@ itself -- the append-only chain can't represent
--   a forward reference, so a move that would require one is rejected
--   rather than silently reordering the reference out from under it.
checkMoveOrder :: StoreM m => ObjectHash -> Int -> Int -> [(ObjectHash, Tick)] -> StoreT m ()
checkMoveOrder tid tidPos afterPos chain = do
  movingTick <- case List.drop tidPos chain of
    ((_, t) : _) -> return t
    []           -> fail "moveTick: tick position out of range"
  let newPos = afterPos + 1
  mapM_ (checkRefBefore newPos) (tickRefs movingTick)
  let precedingSlice = filter ((/= tid) . fst) (List.take (afterPos + 1) chain)
  mapM_ checkNotRefTo precedingSlice
  where
    checkRefBefore newPos refId =
      case List.findIndex ((== refId) . fst) chain of
        Nothing -> return ()
        Just rp
          | rp < newPos -> return ()
          | otherwise   -> fail ("moveTick: cannot move tick before its own reference "
                                    <> T.unpack (unObjectHash refId))
    checkNotRefTo (h, t) =
      if tid `elem` tickRefs t
        then fail ("moveTick: cannot move tick after tick that references it: "
                     <> T.unpack (unObjectHash h))
        else return ()

-- | Merge a contiguous run of one file's atoms into a single atom, in the
--   position of the earliest one.
--
--   Requires at least two ids, all on the same file, occupying consecutive
--   chain positions. A gapped selection (another tick, atom or not,
--   sitting between two of the chosen atoms) is rejected rather than
--   silently collapsed -- collapsing across an unrelated tick would
--   reorder it relative to content it didn't originally follow.
mergeAtoms :: StoreM m => [ObjectHash] -> StoreT m ObjectHash
mergeAtoms []  = fail "mergeAtoms: need at least two atoms"
mergeAtoms [_] = fail "mergeAtoms: need at least two atoms"
mergeAtoms tids = do
  chain <- contentChain
  positioned <- mapM (\tid -> (,) tid <$> findPos tid chain) tids
  let ordered   = map fst (List.sortOn snd positioned)
      positions = List.sort (map snd positioned)

  checkContiguous positions
  path <- sameFile chain ordered

  merged <- at (last ordered) $ do
    popped <- popN (length ordered)
    let ticks   = reverse popped  -- oldest first, matching 'ordered'
        content = T.concat [ c | Atom _ _ _ c <- ticks ]
        refs    = filter (`notElem` tids) (concatMap tickRefs ticks)
    store (Atom refs path [] content)
  -- 'at's fallback only ever logs @last ordered -> merged@ (it's the one
  -- 'at' actually navigated to); every *other* merged id also needs to
  -- resolve to the same result -- several-old-ids-to-one-new-id, the
  -- generalization of 'splitTick's one-to-several case.
  mapM_ (\tid -> logRemap tid merged) (init ordered)
  return merged
  where
    checkContiguous ps
      | and (zipWith (\a b -> b == a + 1) ps (List.drop 1 ps)) = return ()
      | otherwise = fail "mergeAtoms: selected atoms must be contiguous"

    sameFile chain groupTids = do
      paths <- mapM (fileOfTick chain) groupTids
      case List.nub paths of
        [p] -> return p
        _   -> fail "mergeAtoms: selected atoms must all belong to the same file"

    fileOfTick chain tid = case lookup tid chain of
      Just (Atom _ p _ _) -> return p
      _                   -> fail ("mergeAtoms: not an atom: " <> T.unpack (unObjectHash tid))

    -- Popping n times off head (head is set to the last atom of the group
    -- by the enclosing 'at') walks backward through each member in turn,
    -- landing head on the anchor right before the first one once all n
    -- are popped. Returned newest-first.
    popN :: StoreM m => Int -> StoreT m [Tick]
    popN 0 = return []
    popN k = (:) <$> drop <*> popN (k - 1)

-- | Explode one atom into several caller-supplied pieces, replacing it in
--   place, and return each piece's own new id (oldest first). No
--   splitting policy lives here -- the caller decides the pieces and hands
--   them over already split. The first piece inherits @tid@'s incoming
--   references (the reverse of 'mergeAtoms'); the rest are fresh ticks
--   with no refs of their own.
splitTick :: StoreM m => ObjectHash -> [Text] -> StoreT m [ObjectHash]
splitTick _ pieces | length pieces < 2 =
  fail "splitTick: need at least two pieces"
splitTick tid pieces = at tid $ do
  old <- drop
  ids <- case old of
    Atom refs path tags _ -> storePieces refs path tags pieces
    _                      -> fail ("splitTick: not an atom: " <> T.unpack (unObjectHash tid))
  -- 'at' can't guess which of several new ticks a one-to-many action's
  -- @target@ itself becomes -- say so explicitly, so a ref to @tid@
  -- resolves to the piece that's meant to inherit it (see
  -- "Storage.Core"'s 'at' Haddock).
  case ids of
    (inheritor : _) -> logRemap tid inheritor
    []               -> return ()
  return ids
  where
    -- Every piece inherits the original atom's own tags (e.g. a hidden
    -- atom splits into hidden pieces) -- only the incoming refs are
    -- one-shot, going to the first piece alone.
    storePieces _    _    _    []       = return []
    storePieces refs path tags (p : ps) = do
      newId <- store (Atom refs path tags p)
      rest  <- storePieces [] path tags ps
      return (newId : rest)

-- | Where @path@'s *current* lifetime began: the atom, walking back from
--   head, whose own parent commit did *not* have @path@ in its tree --
--   the definition of "created" ('renameFile' rebases here). Tree-based,
--   not tag-based: it doesn't consult 'removedTagKey' at all, so it finds
--   the right tick even for content that was never atom-tracked with any
--   tag, as long as the tree itself changed presence at that point. A
--   path that was deleted and later reused at the same path has two (or
--   more) such ticks in its history; this always finds the one closest to
--   head, i.e. the file as it currently stands. Checks presence via
--   'lookupPathAt' -- a direct walk down @path@'s own segments, never
--   touching the blob's own bytes -- rather than 'loadWorkingTree', which
--   would materialize every other file in the parent's tree just to
--   answer a single-path question, once per atom on @path@ encountered
--   along the way.
findCreationTick :: StoreM m => FilePath -> StoreT m ObjectHash
findCreationTick path = headHash >>= go
  where
    go h = do
      (cd, t) <- lift (readCommitTick h)
      case t of
        Atom _ p _ _ | p == path -> case commitParents cd of
          []        -> return h
          (par : _) -> do
            parentHas <- lift (lookupPathAt par path)
            case parentHas of
              Just _  -> descend cd
              Nothing -> return h
        _ -> descend cd
      where
        descend cd = case commitParents cd of
          []        -> fail ("findCreationTick: no atom found for " <> path)
          (par : _) -> go par

-- | Rename @path@'s current lifetime (see 'findCreationTick') to
--   @newPath@: one rebase, via "Storage.Core"'s 'atWith', at the creation
--   tick -- @action@ ('editTick') renames that one tick; @onReplay@ then
--   applies the exact same substitution to every later tail tick as it
--   replays, so the whole lifetime picks up the new name in a single pass,
--   with no separate call (or id bookkeeping) per atom. Safe to apply
--   unconditionally to every tick whose recorded path is still @oldPath@,
--   with no tree check needed at replay time the way 'findCreationTick'
--   itself needs one: 'findCreationTick' already finds the *current*
--   (most recent) lifetime, so by construction nothing between it and
--   head can belong to some later, unrelated reuse of the same path --
--   there is no "later" lifetime left to accidentally catch.
--
--   Not just 'Atom's own @atomPath@: a tick kind that isn't a real file
--   diff (a 'Storyteller.Writer.Types.Presence', 'Storyteller.Writer.
--   Agent.Prompt', ...) still associates itself with a file by carrying a
--   plain @"file"@ header field of its own, on top of 'Storage.Core.Tick's
--   opaque 'NonAtom' -- see 'Storage.Tick.encodeTickData'\/'decodeTickData'
--   for that wire convention. Left alone, a rename would silently orphan
--   any such tick still naming @oldPath@ (its own tick would move, but a
--   presence event on it would keep pointing at a file that, from here on,
--   no longer exists): 'renamePathField' rewrites that header line the
--   same way @renameTo@'s 'Atom' case rewrites 'atomPath', generically over
--   every 'NonAtom' -- it never needs to know which 'TickType' it's
--   looking at, only that the wire convention is the same for all of them.
--
--   Also moves @oldPath@'s content in the ambient tree to @newPath@, same
--   convention 'addAtom'\/'deleteFile' already follow: a caller doing
--   further ambient 'Runix.FileSystem' operations immediately afterward
--   (in the same branch scope) sees the rename there too. Targeted, not a
--   whole 'Storage.Core.reset' -- this must not clobber some other file's
--   own pending ambient edit sitting in the same scope.
renameFile :: StoreM m => FilePath -> FilePath -> StoreT m ()
renameFile oldPath newPath = do
  creationTid <- findCreationTick oldPath
  () <$ atWith renameTo creationTid (editTick (fmap renameTo . requireAtom))
  finalContent <- inWorktree (readFile newPath)
  FS.remove oldPath
  writeFile newPath finalContent
  where
    renameTo t = case t of
      Atom refs p tags content | p == oldPath -> Atom refs newPath tags content
      NonAtom refs raw                        -> NonAtom refs (renamePathField raw)
      _                                       -> t
    requireAtom t@Atom {} = return t
    requireAtom _         = fail "renameFile: creation tick is not an atom"
    renamePathField raw =
      let (headerBlock, rest) = T.breakOn "\n\n" raw
      in if T.null rest
           then raw  -- no header/payload boundary at all -- not this wire convention, leave untouched
           else T.intercalate "\n" (map renameLine (T.lines headerBlock)) <> rest
    renameLine l = case T.breakOn ":" l of
      (k, v) | k == "file", T.drop 1 v == T.pack oldPath -> "file:" <> T.pack newPath
      _                                                   -> l

-- | Freeze @path@'s current lifetime behind a fresh deletion boundary,
--   then clone it in full onto a brand new lifetime at the same path: every
--   atom, and every other tick anywhere in the chain that refers to one of
--   them (a note, fixup, swipe, ...), each re-'store'd as an independent
--   copy with a fresh id. From here on, 'atomHistory'\/'fileTicksOf's own
--   current-lifetime view sees only the new copies -- so an 'editAtomAt'\/
--   'deleteTick' issued after this point can only ever touch them, never
--   reach back through the boundary into what came before -- while the
--   originals are left completely untouched, exactly the way 'deleteFile'
--   already leaves an old lifetime intact: nothing here is a rebase, only
--   new ticks get added on top.
--
--   Every atom is re-'store'd through the real 'Atom' constructor rather
--   than replayed as a generic drafted message -- 'Storage.Core.store'
--   gives 'Atom' its own dedicated tree-splicing case (folding its content
--   into the actual git tree), which a 'NonAtom' never gets; a uniformly
--   "drafted" replay would decode back as an atom on the next read
--   ('decodeTick' just sniffs the message shape) while silently never
--   having landed in the tree at all. Every other tick clones as a plain
--   'NonAtom', message untouched -- none of them carry a path-shaped field
--   of their own the way 'Atom'\/'renameFile' do, only 'tickRefs' pointing
--   at the atom(s) they annotate.
--
--   Each clone's own 'tickRefs' are rewritten to point at whichever of
--   *this* clone's siblings they originally referenced (a note pointing at
--   an atom being cloned alongside it must end up pointing at that atom's
--   new copy, not its now-frozen original) via a plain local mapping built
--   up as the clones land -- deliberately not the store's own 'logRemap'
--   table: that would make the *original* atom's id permanently resolve to
--   its clone everywhere, including for a caller still holding the old id
--   wanting history exactly as it was -- the same guarantee 'deleteFile'
--   itself depends on. A checkpoint clones; it never rebases, so both
--   copies must go on existing, independently, forever. A ref to anything
--   outside this one clone batch (unrelated, or belonging to some other
--   path entirely) is left exactly as it was -- the mapping only ever
--   redirects ids it actually minted.
--
--   'Binary'\/'Opaque' ticks are left out of the clone even if one somehow
--   carries a 'tickRefs' pointing at one of these atoms (no real caller
--   currently produces that shape): the original stays reachable and
--   correct either way, it just doesn't gain a counterpart pointing at the
--   fresh copy -- cloning one would need its own real tree content the
--   same way 'Atom' does, which nothing here has a use case to justify yet.
checkpointFile :: StoreM m => FilePath -> StoreT m ()
checkpointFile path = do
  atoms <- currentLifetimeAtoms path
  let atomIds = Set.fromList (map fst atoms)
  annotations <- follow [] $ \acc h t -> case t of
    _ | Set.member h atomIds                    -> (acc, True)
    _ | any (`Set.member` atomIds) (tickRefs t)  -> ((h, t) : acc, True)
    _                                            -> (acc, True)
  _      <- deleteFile path
  remap1 <- foldM cloneAtom Map.empty atoms
  _      <- foldM cloneAnnotation remap1 annotations
  return ()
  where
    remapRefs remap = map (\r -> Map.findWithDefault r r remap)

    cloneAtom remap (oldId, Atom refs _ tags content) = do
      newId <- store (Atom (remapRefs remap refs) path tags content)
      return (Map.insert oldId newId remap)
    cloneAtom remap _ = return remap -- unreachable: 'currentLifetimeAtoms' only yields Atom

    cloneAnnotation remap (oldId, t) = case t of
      NonAtom refs raw -> do
        newId <- store (NonAtom (remapRefs remap refs) raw)
        return (Map.insert oldId newId remap)
      _ -> return remap -- see the Haddock's own note on Binary\/Opaque

    -- Same current-lifetime boundary as 'atomHistory', but keeping each
    -- atom's own id and tags alongside its content -- 'atomHistory' only
    -- ever hands back content, since that's all its own callers need.
    currentLifetimeAtoms :: StoreM m => FilePath -> StoreT m [(ObjectHash, Tick)]
    currentLifetimeAtoms p = headHash >>= \h -> go h []
      where
        go h acc = do
          t <- lift (readTick h)
          case t of
            a@(Atom _ p' tags _) | p' == p ->
              if isRemoval tags then return acc else continue h ((h, a) : acc)
            _ -> continue h acc
        continue h acc = do
          cd <- lift (readCommit h)
          case commitParents cd of
            []        -> return acc
            (par : _) -> go par acc

-- | Replace @oldPath@ with a brand new, unrelated lifetime at @newPath@:
--   @oldPath@'s current lifetime is deleted (see 'deleteFile' -- a forward
--   event, its own history stays exactly as it was), and @newPath@ starts
--   fresh with @content@ as its one atom. Deliberately not a 'renameFile':
--   no id\/ref continuity at all between the two -- every note, fixup, or
--   swipe attached to @oldPath@'s old atoms stays exactly where it is,
--   pointing at a lifetime that's now frozen behind a deletion marker,
--   rather than following the content forward the way 'renameFile' (or
--   'checkpointFile') would carry it. The one entry point for "this is a
--   wholesale replacement, not an edit" -- e.g. a raw\/markdown editor's
--   own "save as new" action -- where nothing about the old version's own
--   history is expected to connect to the new one.
saveFileAsNew :: StoreM m => FilePath -> FilePath -> Text -> StoreT m ()
saveFileAsNew oldPath newPath content = do
  _ <- deleteFile oldPath
  _ <- addAtom newPath content
  return ()
