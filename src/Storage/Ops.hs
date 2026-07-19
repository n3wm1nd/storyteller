{-# LANGUAGE OverloadedStrings #-}

-- | The mutating storage operations built from "Storage.Core"'s
-- primitives (store\/drop\/at\/readAt\/reset\/inWorktree\/readFile\/
-- writeFile) -- nothing here reaches around them, or touches the
-- chain\/ambient tree any other way. Re-exports the read-only chain
-- queries ("Storage.Query") and worktree reconciliation
-- ("Storage.Reconcile"), plus the chain-navigation primitives themselves
-- ("Storage.Core"'s own @headHash@\/@store@\/@drop@\/@at@\/@follow@\/...),
-- so @import Storage.Ops@ is the complete storage toolkit for anything
-- that *does something* with the chain; "Storage.FS" holds the ambient
-- file operations, and "Storage.Tick" the typed-tick bridge.
--
-- "Storage.Core" itself stays reserved for the actual machinery: the
-- 'Storage.Core.MonadStore' class and object vocabulary, running\/
-- bootstrapping the 'Storage.Core.StoreT' scope itself
-- ('Storage.Core.runStoreT'\/'Storage.Core.freshScope'\/...), and the
-- remap-log internals -- things a caller reaching for an operation to run
-- has no business touching directly. A module that genuinely needs one of
-- those (the git interpreter itself, or something constructing raw
-- commits\/objects) still imports "Storage.Core" directly alongside this.
module Storage.Ops
  ( -- * Committing atoms
    addAtom
  , addAtomWithRefs
  , append
  , addBinary
  , deleteFile

    -- * Editing atoms in place
  , editAtom
  , editAtomAt
  , replaceAtom
  , setAtomHidden

    -- * Chain-editing operations -- position-aware moves\/merges\/splits
    -- over the whole chain, not just one file's own atom history
  , deleteTick
  , deleteTicks
  , deleteTicksSorted
  , moveTick
  , mergeAtoms
  , splitTick

    -- * Whole-lifetime operations
  , renameFile
  , checkpointFile
  , saveFileAsNew

    -- * Re-exported: read-only chain queries, worktree reconciliation,
    -- and the ambient existence check they share
  , module Storage.Query
  , module Storage.Reconcile
  , exists

    -- * Re-exported from "Storage.Core": chain-navigation and -editing
    -- primitives, and the vocabulary they're expressed in
  , ObjectHash(..)
  , StoreM
  , StoreT
  , Tick(..)
  , headHash
  , store
  , drop
  , readAt
  , at
  , atWith
  , editTick
  , replaceTick
  , resolveId
  , follow
  , followC
  , memoFold
  , syncTo
  ) where

import Prelude hiding (drop, readFile, writeFile, appendFile)

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Storage.Core
import qualified Storage.FS as FS
import Storage.FS (exists)
import Storage.Query
import Storage.Reconcile

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
--   'Storage.Query.atomHistory') an efficient place to stop, not to be
--   read as the signal on its own.
--
--   Named to match the chain-level "delete" vocabulary
--   ('deleteTick'\/'Server.Core.File.deleteFileTick'\/'Server.Core.File.deleteFile'), not
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
--   atomic step -- bypassing 'Storage.Reconcile.commitFile's own
--   decode-and-guess reconciliation entirely. Use this when the caller
--   already knows the content isn't prose (e.g. an uploaded image);
--   'commitFiles'\/'commitWorktree' remain the right call for content
--   whose atom-vs-binary status still needs to be discovered from the
--   bytes themselves, with the anonymous 'Storage.Core.Opaque' tick as
--   their safe fallback.
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
-- Chain-editing operations
-- ---------------------------------------------------------------------------
--
-- Position-aware moves/merges/splits, over the whole chain rather than one
-- file's own atom history. Built entirely from 'at'/'drop'/'store' and
-- "Storage.Query"'s position vocabulary ('contentChain'/'findPos') -- no
-- separate draft type is needed the way "Storyteller.Core.StorageMonad"
-- needed 'TDraft': a 'Tick' already carries its own diff (an 'Atom's
-- 'atomContent' *is* the append), so popping and re-storing one directly is
-- both the extraction and the reinsertion step.

-- | Remove @tid@ from the chain entirely, replaying the tail on top of
--   whatever comes before it.
deleteTick :: StoreM m => ObjectHash -> StoreT m ()
deleteTick tid = () <$ at tid drop

-- | Remove every tick in @targets@ -- any combination, in any order, on
--   any number of unrelated chains -- in one transaction. Sorts and
--   groups by connected component first ('Storage.Query.
--   descendantsFirstGrouped'), then applies 'deleteTicksSorted' to each
--   component: still one continuous dive per *component* (there's no way
--   around needing at least one per genuinely unrelated chain -- nothing
--   for one to descend through to reach another), but never one per
--   target the way looping plain 'deleteTick' would.
deleteTicks :: StoreM m => [ObjectHash] -> StoreT m ()
deleteTicks targets = do
  groups <- descendantsFirstGrouped targets
  mapM_ deleteTicksSorted groups

-- | Remove every tick in @targets@ in exactly one wind-back-and-replay,
--   rather than one independent 'at' round trip per target -- looping
--   'deleteTick' over a list, even in the right order, still means each
--   later call re-winds from the *new* head and re-walks (and
--   re-replays) everything the previous call just finished replaying.
--
--   @targets@ must already be in descendants-first order (nearest head
--   first) *and* all mutually related, each an ancestor of the one
--   before it: a candidate not reachable as an ancestor of wherever this
--   is currently descending fails loudly, same as a single misdirected
--   'at'\/'deleteTick' already does. 'deleteTicks' (above) is the general
--   entry point that establishes both preconditions first, via
--   'Storage.Query.descendantsFirstGrouped'; call this directly only when
--   a caller already knows its own targets satisfy them (e.g. one
--   already-computed group).
--
--   Works by nesting 'at' rather than by any new capability: 'at's own
--   descent, when its own action is itself another 'at' call, simply
--   keeps descending from wherever it already is instead of re-winding
--   from head -- so @at t1 (at t2 (... 'drop'))@ is one unbroken dive
--   from the original head down to the deepest target, and each layer's
--   own trailing 'drop' (once its own nested computation returns) removes
--   that layer's own target as the single ascent passes back through it.
deleteTicksSorted :: StoreM m => [ObjectHash] -> StoreT m ()
deleteTicksSorted []       = return ()
deleteTicksSorted (t : ts) = () <$ at t (deleteTicksSorted ts >> drop)

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

-- ---------------------------------------------------------------------------
-- Whole-lifetime operations
-- ---------------------------------------------------------------------------

-- | Rename @path@'s current lifetime (see 'Storage.Query.findCreationTick')
--   to @newPath@: one rebase, via "Storage.Core"'s 'atWith', at the creation
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
--   atom, and every other tick that refers to one of them -- directly or
--   through a chain of annotations of any depth (a note about a note about
--   an atom), the same any-depth rule 'Storage.Tick.relatedTicksOf' applies
--   when *rendering* annotations -- each re-'store'd as an independent
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
  atoms <- lifetimeAtoms path
  case atoms of
    [] -> () <$ deleteFile path
    ((oldestAtom, _) : _) -> do
      let atomIds = Set.fromList (map fst atoms)
      -- Candidate annotations live strictly between head and the
      -- lifetime's own oldest atom -- a ref only ever points backward in
      -- time, so nothing at or below that atom can reference it (the same
      -- bound 'Storage.Tick.relatedTicksOf' descends to); the walk stops
      -- right there instead of paying for the whole chain. Which
      -- candidates actually belong is then decided oldest-first against
      -- the growing set of already-included ids, so a chain of
      -- annotations joins hop by hop, however deep -- the inner note is
      -- always older than the one about it, and so always settled first.
      candidates <- follow [] $ \acc h t -> case t of
        _ | h == oldestAtom -> (acc, False)
        NonAtom {}          -> ((h, t) : acc, True)
        _                   -> (acc, True)
      let annotations = pickTransitive atomIds [] candidates
      _      <- deleteFile path
      remap1 <- foldM cloneAtom Map.empty atoms
      _      <- foldM cloneAnnotation remap1 annotations
      return ()
  where
    -- @candidates@ oldest-first (see 'follow''s prepend order), so every
    -- tick's own referents are already decided by the time it's looked at.
    pickTransitive _ acc [] = reverse acc
    pickTransitive included acc ((h, t) : rest)
      | any (`Set.member` included) (tickRefs t) =
          pickTransitive (Set.insert h included) ((h, t) : acc) rest
      | otherwise = pickTransitive included acc rest

    remapRefs remap = map (\r -> Map.findWithDefault r r remap)

    cloneAtom remap (oldId, Atom refs _ tags content) = do
      newId <- store (Atom (remapRefs remap refs) path tags content)
      return (Map.insert oldId newId remap)
    cloneAtom remap _ = return remap -- unreachable: 'lifetimeAtoms' only yields Atom

    cloneAnnotation remap (oldId, NonAtom refs raw) = do
      newId <- store (NonAtom (remapRefs remap refs) raw)
      return (Map.insert oldId newId remap)
    cloneAnnotation remap _ = return remap -- unreachable: only NonAtoms are collected

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
