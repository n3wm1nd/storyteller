{-# LANGUAGE OverloadedStrings #-}

-- | Read-only chain queries over "Storage.Core"'s primitives -- walks
--   that answer questions about the chain (which atoms belong to a path's
--   current lifetime, where a tick sits, whether a path was ever
--   atom-tracked) without ever moving head, replaying anything, or
--   touching the ambient tree. The mutating operations built on these
--   live in "Storage.Ops" (which re-exports this module, so a consumer
--   importing that already has the full toolkit); worktree reconciliation
--   is "Storage.Reconcile".
--
--   Every walk here is bounded by what its own answer needs -- a
--   short-circuit the moment the question is settled ('findAtom',
--   'hasAnyAtom'\/'atomTrackedAmong'), or a lifetime boundary
--   ('lifetimeAtoms') -- and reads each commit it passes exactly once
--   ('Storage.Core.readCommitTick').
module Storage.Query
  ( -- * Nearest / existence
    findAtom
  , hasAnyAtom
  , atomTrackedAmong
  , loadLiveWorkingTree

    -- * A path's current lifetime
  , atomHistory
  , lifetimeAtoms
  , findCreationTick

    -- * Chain positions
  , rootHash
  , contentChain
  , findPos
  , chainPositions

    -- * Batch processing order
  , descendantsFirst
  , descendantsFirstGrouped
  ) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Storage.Core

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

-- | 'Storage.Core.loadWorkingTree' with every never-atom-tracked path
--   removed -- an uploaded binary asset, or anything else that opted out
--   of atom tracking, simply doesn't exist as far as a caller of this
--   function is concerned, the same absence-not-an-error treatment a
--   missing lookup already gets elsewhere in this module. Whether a path
--   has ever been atom-tracked is a real storage fact (a chain walk), not
--   something derivable from a path's own text -- callers further up
--   (e.g. "Storyteller.Context.DSL.Compile"'s Reader-scope bootstrap) that
--   need "only real, prose-bearing content" get it from here rather than
--   re-deciding that policy themselves.
--
--   @commit@ is resolved explicitly throughout -- 'readAt' repositions
--   just the atom-tracking walk (which otherwise reads the *ambient*
--   head), matching 'Storage.Core.loadWorkingTree' itself, which already
--   takes an explicit commit rather than reading ambient state -- so this
--   is safe to call against a foreign commit without disturbing the
--   caller's own position.
loadLiveWorkingTree :: StoreM m => ObjectHash -> StoreT m [(FilePath, ObjectHash)]
loadLiveWorkingTree commit = do
  wt <- lift (loadWorkingTree commit)
  let files = [ (path, h) | (path, FSFile h) <- Map.toList wt ]
  tracked <- readAt commit (atomTrackedAmong (map fst files))
  pure [ (path, h) | (path, h) <- files, path `Set.member` tracked ]

-- | This file's own *current lifetime*, oldest first: every atom strictly
--   after @path@'s most recent deletion event (see
--   'Storage.Core.removedTagKey'), or since its creation if it has none.
--   A deletion marker is a real, permanent tick -- 'Storage.Tick.
--   fileTicksOf' still walks straight past it for a client wanting the
--   full timeline -- but here it is a *boundary, not content*: what's
--   before it belongs to a previous life of this path, and the marker
--   itself is never part of the history handed back.
atomHistory :: StoreM m => FilePath -> StoreT m [(ObjectHash, Text)]
atomHistory path = do
  atoms <- lifetimeAtoms path
  return [ (h, content) | (h, Atom _ _ _ content) <- atoms ]

-- | Every atom in @path@'s current lifetime, oldest first, each with its
--   own id -- the walk 'atomHistory' (content only) and
--   'Storage.Ops.checkpointFile' (ids and tags too) both project from.
--   Two boundaries end it, whichever is hit first, both strictly about
--   the *current* lifetime:
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

-- | Where @path@'s *current* lifetime began: the atom, walking back from
--   head, whose own parent commit did *not* have @path@ in its tree --
--   the definition of "created" ('Storage.Ops.renameFile' rebases here).
--   Tree-based, not tag-based: it doesn't consult 'removedTagKey' at all,
--   so it finds the right tick even for content that was never
--   atom-tracked with any tag, as long as the tree itself changed
--   presence at that point. A path that was deleted and later reused at
--   the same path has two (or more) such ticks in its history; this
--   always finds the one closest to head, i.e. the file as it currently
--   stands. Checks presence via 'lookupPathAt' -- a direct walk down
--   @path@'s own segments, never touching the blob's own bytes -- rather
--   than materializing every other file in the parent's tree just to
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

-- | The chain's very first commit -- the anchor for a
--   'Storage.Ops.moveTick' to the very front.
rootHash :: StoreM m => StoreT m ObjectHash
rootHash = headHash >>= go
  where
    go h = do
      cd <- lift (readCommit h)
      case commitParents cd of
        []      -> return h
        (p : _) -> go p

-- | Every non-root tick reachable from head, oldest first -- the position
--   vocabulary 'chainPositions'\/'Storage.Ops.moveTick'\/
--   'Storage.Ops.mergeAtoms' all share.
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

-- | Order @candidates@ descendant-first: no candidate ever precedes one of
--   its own descendants -- the correct processing order for a batch of
--   chain-rebasing edits (delete\/split\/hide) that each rewrite
--   everything *after* their own target. 'descendantsFirstGrouped' flat,
--   for a caller that only cares about overall order and processes one
--   candidate at a time via its own independent op -- see that function's
--   own Haddock for how this is actually computed.
descendantsFirst :: StoreM m => [ObjectHash] -> StoreT m [ObjectHash]
descendantsFirst = fmap concat . descendantsFirstGrouped

-- | 'descendantsFirst', kept split into one list per connected component
--   (mutual-ancestry cluster) instead of flattened into one -- what a
--   caller wanting to *nest* several edits into one continuous dive (see
--   'Storage.Ops.deleteTicks') needs to know exactly where that's safe to
--   do (within a component, every member really is an ancestor of the one
--   before it) and where it isn't (across two components, which share no
--   ancestry at all -- there is nothing for one to descend through to
--   reach the other).
--
--   Processing an ancestor before its own not-yet-handled descendant
--   would rebase (and so remap) that descendant's id out from under it
--   before its own turn arrives; two candidates neither of which
--   descends from the other (unrelated ticks, possibly on entirely
--   separate branches) get no ordering constraint between them at all --
--   there's nothing for one to invalidate in the other regardless of
--   order, which is exactly what makes them separate components.
--
--   Deliberately not built from 'contentChain'\/'chainPositions': those
--   need a head to walk from and fail outright on any id not currently
--   reachable from it (including, awkwardly, a candidate this exact
--   batch's own earlier processing has already remapped away). This walks
--   only each candidate's own ancestry, one parent hop at a time, so it
--   needs no head, costs nothing proportional to the whole chain's
--   length, and is well-defined for candidates spanning separate chains.
--
--   Two special cases skip the general search below entirely, since it's
--   worth naming exactly what's cheap rather than relying on the general
--   path happening to be fast:
--
--   * zero or one candidate -- there's nothing to compare against, so
--     "sorted" is true by definition. No lookup at all, not even a
--     single 'readCommit' -- notably, this makes a one-tick batch
--     (an ordinary single delete, the overwhelmingly common case) free,
--     the same as it was before this function existed.
--   * @candidates@ already came in oldest-first, which is the ordinary
--     case for a caller built from 'contentChain'-derived data (e.g. a
--     client's own selection, read off an already oldest-first tick
--     list) -- @reverse candidates@ is then already exactly the answer.
--     Checked with one combined descent (not the general search's one
--     independent walk per candidate): starting from the presumed
--     newest, keep descending through 'commitParents' until the next
--     presumed candidate is reached, and repeat: 'guess' is confirmed
--     the moment every candidate has been checked off in order, and
--     rejected (falling through to the general search below) the moment
--     a *different* candidate is reached first, or history runs out
--     before reaching the next one at all. Only ever succeeds when every
--     candidate is mutually related (the walk has to reach all of them
--     in one unbroken descent), so a successful guess is always exactly
--     one component -- @[guess]@.
--
--   Otherwise, the general search: for each candidate, walk backward
--   through 'commitParents' until hitting either another candidate (its
--   nearest candidate ancestor) or running out of history. That builds a
--   forest -- each candidate points at (at most) one ancestor-candidate
--   -- which a child-before-parent traversal from every root (a candidate
--   with no candidate ancestor) reads a valid order straight off of, one
--   traversal per root, one component apiece.
descendantsFirstGrouped :: StoreM m => [ObjectHash] -> StoreT m [[ObjectHash]]
descendantsFirstGrouped []           = return []
descendantsFirstGrouped cs@[_]       = return [cs]
descendantsFirstGrouped candidates = do
  let candSet = Set.fromList candidates
      guess   = reverse candidates
  alreadySorted <- verifyDescent candSet guess
  if alreadySorted then return [guess] else generalSort candSet candidates
  where
    verifyDescent _     []            = return True
    verifyDescent _     [_]           = return True
    verifyDescent cands (a : rest@(b : _)) = walkTo a
      where
        walkTo cur = do
          cd <- lift (readCommit cur)
          case commitParents cd of
            []      -> return False
            (p : _)
              | p == b              -> verifyDescent cands rest
              | Set.member p cands  -> return False
              | otherwise           -> walkTo p

    generalSort cands cs = do
      parents <- mapM (nearestCandidateAncestor cands) cs
      let edges      = zip cs parents
          childrenOf = Map.fromListWith (++) [ (anc, [c]) | (c, Just anc) <- edges ]
          roots      = [ c | (c, Nothing) <- edges ]
          emit c     = concatMap emit (Map.findWithDefault [] c childrenOf) ++ [c]
      return (map emit roots)

    nearestCandidateAncestor cands oid = do
      cd <- lift (readCommit oid)
      case commitParents cd of
        []      -> return Nothing
        (p : _)
          | Set.member p cands -> return (Just p)
          | otherwise          -> nearestCandidateAncestor cands p
