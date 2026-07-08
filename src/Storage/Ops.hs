{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-facing operations built entirely from "Storage.Core"'s
-- primitives (store\/drop\/at\/readAt\/reset\/inWorktree\/readFile\/
-- writeFile) and "Storage.FS"'s ('list') -- nothing here reaches
-- around them, or touches the chain\/ambient tree any other way.
module Storage.Ops
  ( addAtom
  , addAtomWithRefs
  , removeFile
  , append
  , findAtom
  , editAtom
  , editAtomAt
  , replaceAtom
  , setAtomHidden
  , addBinary
  , commitWorktree
  , commitFile
  , commitFiles
  , atomHistory
  , hasAnyAtom
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
  ) where

import Prelude hiding (drop, readFile, writeFile, appendFile)

import Control.Monad (foldM, filterM)
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
import Storage.FS (list)

-- | Whether @path@ currently has any content in the ambient tree.
exists :: StoreM m => FilePath -> StoreT m Bool
exists path = elem path <$> list

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
--   signal on its own. Also removes @path@ from the ambient tree, so a
--   caller doing further ambient 'Runix.FileSystem' operations
--   immediately afterward (in the same branch scope) sees it gone there
--   too -- same convention 'addAtom' already follows for its own ambient
--   sync.
removeFile :: StoreM m => FilePath -> StoreT m ObjectHash
removeFile path = do
  newHead <- store (Atom [] path [(removedTagKey, "true")] "")
  FS.remove path
  return newHead

-- | The nearest atom at or before @start@, walking backward through
--   parents. Fails once the chain runs out (root reached, no parent
--   left) without finding one.
findAtom :: StoreM m => ObjectHash -> StoreT m ObjectHash
findAtom start = do
  t <- lift (readTick start)
  case t of
    Atom {} -> return start
    _       -> do
      cd <- lift (readCommit start)
      case commitParents cd of
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
  mapM_ commitFile (List.nub (ambient ++ committed))
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
      tracked <- filterM hasAnyAtom diffs
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
  if not tracked
    then do
      present <- exists path
      bytes   <- if present then readFile path else return BS.empty
      case TE.decodeUtf8' bytes of
        Left _     -> return ()
        Right text -> storeNewFile path text
    else do
      history <- atomHistory path
      present <- exists path
      target  <- readWorking path
      reconcileFile path history target
      remaining <- atomHistory path
      -- A target that reconciles down to nothing collapses every gap to
      -- empty too (gaps are substrings of target, so if it's empty they
      -- all are), which is exactly the condition under which
      -- 'reconcileFile' drops every atom and emits no standalone
      -- replacement -- the path would otherwise vanish from the tree
      -- entirely. That's correct when @path@ is genuinely gone from the
      -- ambient tree (see 'commitWorktree'), but wrong when it's still
      -- there with merely empty content -- the same case 'storeNewFile'
      -- already handles for a path with no history at all, by always
      -- leaving an empty marker atom behind so presence survives even
      -- when content doesn't.
      case remaining of
        [] | present -> () <$ store (Atom [] path [] "")
        _            -> return ()

-- | Whether @path@ has ever had at least one 'Atom' tick -- a short-
--   circuiting existence check, not the full 'atomHistory' walk. By the
--   "once atom, always atom" invariant, finding a single match already
--   proves @path@ is atom-governed from here on -- there's no need to see
--   the rest of its history (let alone reach root) just to answer this.
hasAnyAtom :: StoreM m => FilePath -> StoreT m Bool
hasAnyAtom path = headHash >>= go
  where
    go h = do
      t <- lift (readTick h)
      case t of
        Atom _ p _ _ | p == path -> return True
        _ -> do
          cd <- lift (readCommit h)
          case commitParents cd of
            []      -> return False
            (p : _) -> go p

-- | This file's own committed history, oldest first, back to whichever is
--   closer: root, or @path@'s own most recent deletion event (see
--   'Storage.Core.removedTagKey'). A deletion marker is a real, permanent
--   tick -- 'Tick.fileTicksOf' still walks straight past it for a client
--   wanting the full timeline -- but it's also a fold boundary: content
--   from before it belongs to a previous life of this path, not to
--   whatever's there now (possibly nothing, possibly a fresh
--   'Storyteller.Core.Create.createFile' after it), so reconciliation here
--   must not fold it back in. The marker itself is included, at empty
--   content, so it costs nothing to fold over one more time; only what's
--   *before* it is excluded.
atomHistory :: StoreM m => FilePath -> StoreT m [(ObjectHash, Text)]
atomHistory path = headHash >>= \h -> go h []
  where
    go h acc = do
      t <- lift (readTick h)
      case t of
        Atom _ p tags content | p == path ->
          let acc' = (h, content) : acc
          in if isRemoval tags then return acc' else continue h acc'
        _ -> continue h acc
    continue h acc = do
      cd <- lift (readCommit h)
      case commitParents cd of
        []      -> return acc
        (p : _) -> go p acc

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

-- | The chain's very first commit -- the anchor a leading standalone gap
--   (content that precedes this file's first surviving atom) gets
--   inserted right after.
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

-- | The reconciliation proper, once this file is known to have some
--   existing atom history. Every atom's own current id is re-derived by
--   its position in a freshly re-walked 'atomHistory' right before it's
--   touched (rather than tracked through a remap table): an id computed
--   before this loop started can already be stale by the time an earlier
--   atom's own 'at' call has replayed the tail -- including atoms this
--   loop hasn't gotten to yet, whether or not their own content changed.
reconcileFile :: StoreM m => FilePath -> [(ObjectHash, Text)] -> Text -> StoreT m ()
reconcileFile path history target = do
  root <- rootHash
  let matches  = matchAtoms (map snd history) target
      n        = length matches
      gaps     = gapContents matches target
      fates    = gapFates matches gaps
      contents = finalAtomContents matches target gaps fates
      outs     = classify matches contents
      -- 'gaps'/'fates' carry one entry per atom (the gap immediately
      -- before it) plus one trailing entry for the gap after the last
      -- atom; pairing stops one short of that, leaving the trailing pair
      -- for the final 'emitStandaloneGap' call below.
      perAtom  = zip5 matches (List.take n gaps) (List.take n fates) contents outs
      (tailGap, tailFate) = case (List.drop n gaps, List.drop n fates) of
        (g : _, f : _) -> (g, f)
        _              -> (T.empty, Standalone)
  (anchor, _) <- foldM (step path) (root, 0) perAtom
  () <$ emitStandaloneGap path anchor tailGap tailFate
  where
    -- @liveIdx@ tracks this atom's own position in @path@'s *live*
    -- history at this exact point in the fold -- distinct from its fixed
    -- position among 'matches' (the loop's iteration order), which drifts
    -- out of sync with the live history the moment any earlier step
    -- drops an atom (shrinking it) or emits a standalone insertion
    -- (growing it). Using the fixed position directly here would, once
    -- that drift happens, hand a Dropped\/Changed branch the wrong atom's
    -- id entirely -- silently corrupting an untouched neighbor, or
    -- (caught only when the drift runs past the end) crashing in
    -- 'currentAtomIdAt'.
    step p (anchor, liveIdx) (_m, gap, fate, content, outcome) = do
      anchor1 <- emitStandaloneGap p anchor gap fate
      let liveIdx1 = if anchor1 /= anchor then liveIdx + 1 else liveIdx
      origId  <- currentAtomIdAt p liveIdx1
      case outcome of
        Kept    -> return (origId, liveIdx1 + 1)
        Dropped -> at origId drop >> return (anchor1, liveIdx1)
        Changed -> do
          newId <- at origId $ editTick $ \old -> case old of
            Atom refs _ tags _ -> return (Atom refs p tags content)
            _                  -> fail "commitFile: matched tick isn't an atom (unreachable)"
          return (newId, liveIdx1 + 1)

-- | The @idx@-th atom (0-indexed, oldest first) currently in @path@'s
--   history -- re-walked fresh; see 'reconcileFile' for why a
--   once-computed id can't be reused across this loop.
currentAtomIdAt :: StoreM m => FilePath -> Int -> StoreT m ObjectHash
currentAtomIdAt path idx = do
  history <- atomHistory path
  case List.drop idx history of
    ((oid, _) : _) -> return oid
    []             -> fail "currentAtomIdAt: atom vanished from history unexpectedly"

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

-- | A zero-length original atom (e.g. 'Storyteller.Core.Create.createFile's
--   own empty introduction atom, on a path nothing was ever appended to)
--   always has 'amCoreLen' == 'amOriginalLen' == 0, since
--   'longestCommonSubstring' short-circuits to @(0, 0, 0)@ whenever either
--   input is empty -- regardless of what the target actually says. Without
--   the 'amOriginalLen m > 0' guard, such an atom would read as "fully
--   recovered, unchanged" against *any* target, including one where the
--   whole file is gone, making it permanently undroppable.
isKept :: AtomMatch -> Bool
isKept m = amOriginalLen m > 0 && amCoreLen m == amOriginalLen m

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
--   plus any folded-in gap text -- see 'finalAtomContents').
classify :: [AtomMatch] -> [Text] -> [AtomOutcome]
classify matches contents =
  [ if isKept m then Kept else if T.null fc then Dropped else Changed
  | (m, fc) <- zip matches contents ]

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
commitFiles paths = mapM_ commitFile paths >> syncOpaqueContent

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
--   chain order without walking the chain once per id.
chainPositions :: StoreM m => [ObjectHash] -> StoreT m [(ObjectHash, Int)]
chainPositions oids = do
  chain <- contentChain
  mapM (\oid -> (,) oid <$> findPos oid chain) oids

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
--   head, i.e. the file as it currently stands.
findCreationTick :: StoreM m => FilePath -> StoreT m ObjectHash
findCreationTick path = headHash >>= go
  where
    go h = do
      t  <- lift (readTick h)
      cd <- lift (readCommit h)
      case t of
        Atom _ p _ _ | p == path -> case commitParents cd of
          []        -> return h
          (par : _) -> do
            parentTree <- lift (loadWorkingTree par)
            if Map.member path parentTree
              then descend cd
              else return h
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
--   Also moves @oldPath@'s content in the ambient tree to @newPath@, same
--   convention 'addAtom'\/'removeFile' already follow: a caller doing
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
      _                                       -> t
    requireAtom t@Atom {} = return t
    requireAtom _         = fail "renameFile: creation tick is not an atom"
