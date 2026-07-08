{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Git-backed interpreter for 'StoryStorage' and for 'Storyteller.Core.Branch's
--   'BranchOp' — the one place a concrete backend (real git, via
--   "Runix.Git") is wired into the otherwise backend-agnostic storage
--   layer. Everything above this module (agents, handlers, the branch/file
--   effects) only ever touches 'StoryStorage'\/'BranchOp'\/'MonadGit', never
--   "Runix.Git" directly — a different backend would need its own module
--   exactly like this one, and nothing above would need to change.
--
-- Conventions owned by this layer (invisible to everything above):
--
--   * Branch refs live at  @refs/heads/story/<name>@
--   * Tick messages are encoded per 'Storage.Tick.encodeTickData'.
--   * Each tick (commit) carries the full working-tree snapshot as its tree object.
--
-- Per-branch tick-chain operations (the old @StoryBranch@ effect, with its
-- higher-order @At@\/@WithFS@ constructors, then "Storyteller.Core.StorageMonad"'s
-- @StorageT@) have been replaced by "Storage.Core"'s @StoreT@ plus
-- 'Storyteller.Core.Branch.BranchOp'. 'StoryStorage' (branch
-- create\/delete\/list, cross-branch reference cascade) was always
-- first-order and is unchanged.
module Storyteller.Core.Git
  ( -- * Interpreters
    runStoryStorageGit
  , withStorage
  , withStorageDiscard

    -- * Ref naming
  , refBranchName
  , isStoryRef
  , storyRefPrefix

    -- * Branch tag for filesystem effects
  , BranchTag(..)

    -- * Storage monad embedding -- 'BranchOp' itself is declared,
    -- backend-agnostically, in 'Storyteller.Core.Branch'; re-exported here
    -- alongside its one git interpreter so callers don't need to import
    -- both modules just to run one against real git.
  , BranchOp(..)
  , runStorage
  , runBranchOpGit
  , runStoryFSGit
  , runBranchAndFS

    -- * Generic rebase (for inner actions that interleave other effects)
  , atGeneric

    -- * Cross-branch reference cascade (exported for its own unit tests --
    -- see 'Storyteller.GitCascadeSpec')
  , cascadeReplace
  ) where

import Prelude
import Control.Monad (when)
import Control.Monad.State.Strict (lift)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail
import Data.Maybe (fromMaybe, isJust)
import Polysemy.State (State, get, put, modify, evalState, execState, runState)

-- 'readCommit'\/'writeCommit'\/'readObject'\/'writeObject' hidden and
-- re-imported qualified as @RG.@ below -- "Storage.Core"'s own
-- 'Core.MonadStore' class uses the same four names for its own methods
-- (unlike a @git@-prefixed naming scheme), and an instance body can't
-- disambiguate a qualified name in binding position, only an unqualified
-- one -- so the only way to define both instances in the same module is
-- to make sure exactly one of the two pairs is ever in unqualified scope.
import Runix.Git hiding (readCommit, writeCommit, readObject, writeObject)
import qualified Runix.Git as RG
import Runix.FileSystem
  ( FileSystem(..), FileSystemRead(..), FileSystemWrite(..) )
import qualified System.FilePath.Glob as Glob

import Storyteller.Core.Types hiding (draft)
import Storyteller.Core.Storage
import Storyteller.Core.Branch (BranchOp(..), runStorage)
import qualified Storage.Core as Core
import qualified Storage.FS as FS
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick

-- ---------------------------------------------------------------------------
-- Naming conventions
-- ---------------------------------------------------------------------------

storyRefPrefix :: Text
storyRefPrefix = "refs/heads/story/"

storyRef :: BranchName -> RefName
storyRef (BranchName n) = RefName (storyRefPrefix <> n)

-- | Recover the branch name a ref update refers to, or 'Nothing' if the ref
--   isn't a story branch ref.
refBranchName :: RefName -> Maybe BranchName
refBranchName (RefName r) = BranchName <$> T.stripPrefix storyRefPrefix r

-- | Whether a ref is one of this layer's story branch refs. The one bit of
--   the naming convention exposed outside this module — e.g. to
--   'Storyteller.Core.Undo', which needs to know which ref writes are
--   "real" story mutations worth an undo-log entry, without itself knowing
--   anything about where story branches live.
isStoryRef :: RefName -> Bool
isStoryRef (RefName r) = storyRefPrefix `T.isPrefixOf` r

emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- StoryStorage interpreter
-- ---------------------------------------------------------------------------

-- | A ref, in storage terms: which branch, pointing at which tick (or
--   'Nothing' for a deletion). This is the one shape every 'StoryStorage'
--   ref mutation reduces to, whether it came from 'CreateBranch',
--   'DeleteBranch', 'SetRef', or a cascade triggered by 'UpdateReferences'.
type RefWrite = (BranchName, Maybe TickId)

-- | Everything one run of 'withStorageWithCallback' buffers: pending ref
--   writes (last write per branch wins — see 'overlayRefs') and every
--   tick id 'UpdateReferences' has renamed so far, oldest first. Kept as a
--   single overlay (rather than two stacked 'State' effects) so 'handle'
--   only ever has one thing to thread through 'get'/'modify'.
data Overlay = Overlay
  { ovRefs  :: [RefWrite]
  , ovRemap :: [(TickId, TickId)]
  }

-- | The one real implementation of 'StoryStorage'. Every ref mutation calls
--   the supplied @onRef@ callback (in order) and is recorded into the
--   returned overlay, last write per branch wins. Reads ('ListBranches')
--   see real git refs with any pending overlay entries from this same run
--   applied on top, so a read-after-write within one run observes its own
--   writes.
--
--   'runStoryStorageGit' instantiates this with a callback that writes
--   straight to git and discards the overlay (today's eager behaviour).
--   'withStorage' instantiates it with a no-op callback and replays the
--   overlay into the parent 'StoryStorage' only once the wrapped action has
--   fully succeeded — nothing is written anywhere if it fails.
--
--   The returned tick-id remap is every rename 'UpdateReferences' was ever
--   called with here, in order, *plus* everything 'cascadeReplace'
--   separately discovers while rewriting other branches' ancestry (a
--   descendant reparented onto a renamed commit, which isn't itself a key
--   of the mapping it was called with — see 'cascadeReplace's own doc).
--   Both pieces matter: dropping the input mapping loses the direct
--   renames (a cascade that touches no other branch never calls
--   'rewriteChain' on them at all — see its short-circuit for exactly this
--   case), and dropping cascadeReplace's own discoveries loses everything
--   it found *beyond* what it was told.
withStorageWithCallback
  :: forall r a
  .  Members '[Git, Fail] r
  => (BranchName -> Maybe TickId -> Sem r ())
  -> Sem (StoryStorage : r) a
  -> Sem r (a, [RefWrite], [(TickId, TickId)])
withStorageWithCallback onRef action = do
  (Overlay refs remap, a) <- runState (Overlay [] []) (reinterpret handle action)
  return (a, refs, remap)
  where
    handle :: StoryStorage m x -> Sem (State Overlay : r) x
    handle = \case
      CreateBranch name -> do
        let ref = storyRef name
        existing <- raise $ resolveRef ref
        case existing of
          Just _ -> raise $ fail $ "branch already exists: " <> T.unpack (unBranchName name)
          Nothing -> do
            rootHash <- raise $ RG.writeCommit CommitData
              { commitParents = []
              , commitTree    = emptyTree
              , commitMessage = Tick.encodeTickData (toDraft (Root name))
              }
            let tid = TickId (unObjectHash rootHash)
            applyRef name (Just tid)
            return Branch { branchName = name, branchHead = tid }

      DeleteBranch name -> applyRef name Nothing

      ListBranches -> do
        pairs   <- raise $ listRefs storyRefPrefix
        pending <- ovRefs <$> get
        return $ overlayRefs (map resolveToHead pairs) pending

      -- Reads each branch's *current* head — real git overlaid with this
      -- transaction's own pending writes so far (same computation as
      -- 'ListBranches') — rather than raw, possibly-stale git refs. A
      -- caller's own branch is typically already at its correct final
      -- head by this point (a rewrite publishes via 'SetRef' before
      -- calling this), so it naturally can't still match any of
      -- 'mapping's superseded ids and won't be redundantly reprocessed.
      -- Reading raw git refs instead would see that branch still at its
      -- *pre-rewrite* head under 'withStorage' (nothing lands in real git
      -- until the transaction replays), matching entries it shouldn't and
      -- rebuilding a second, wrong chain from stale ancestry alongside the
      -- correct one already built — this is what actually caused a moved
      -- tick's sibling to be duplicated in the chain.
      UpdateReferences mapping -> do
        pairs   <- raise $ listRefs storyRefPrefix
        pending <- ovRefs <$> get
        let current = [ (storyRef (branchName b), ObjectHash (unTickId (branchHead b)))
                      | b <- overlayRefs (map resolveToHead pairs) pending ]
            hashMapping = Map.fromList
              [ (ObjectHash (unTickId o), ObjectHash (unTickId n)) | (o, n) <- mapping ]
        discovered <- cascadeReplace current applyRef hashMapping
        let extra = [ (TickId (unObjectHash o), TickId (unObjectHash n))
                    | (o, n) <- Map.toList discovered, o /= n ]
        modify (\ov -> ov { ovRemap = ovRemap ov ++ mapping ++ extra })

      SetRef name mtid -> applyRef name mtid

      -- No cascade, no ref writes — 'AnnounceRemap' names a rename that
      -- already happened. Under 'runStoryStorageGit' (nothing further
      -- listening) that's simply nothing to do; under a *nested*
      -- 'withStorage' it still has to be deferred like everything else
      -- here, so it's folded into this scope's own accumulator rather than
      -- forwarded immediately.
      AnnounceRemap mapping -> modify (\ov -> ov { ovRemap = ovRemap ov ++ mapping })

    -- Appended, not prepended: 'overlayRefs' and 'withStorage's replay both
    -- rely on later entries for the same branch winning over earlier ones.
    applyRef :: BranchName -> Maybe TickId -> Sem (State Overlay : r) ()
    applyRef name mtid = do
      raise $ onRef name mtid
      modify (\ov -> ov { ovRefs = ovRefs ov ++ [(name, mtid)] })

    resolveToHead (RefName ref, hash) =
      Branch { branchName = BranchName (T.drop (T.length storyRefPrefix) ref)
             , branchHead = TickId (unObjectHash hash) }

-- | Overlay pending ref writes (newest last) onto a base branch list —
--   updates existing branches, adds newly created ones, drops deleted ones.
overlayRefs :: [Branch] -> [RefWrite] -> [Branch]
overlayRefs base pending =
  let winners      = Map.fromList pending   -- input is newest-last, so last-in-list (newest) wins
      survivors     = [ b | b <- base, maybe True isJust (Map.lookup (branchName b) winners) ]
      applied       = [ b { branchHead = fromMaybe (branchHead b) (fromMaybe Nothing (Map.lookup (branchName b) winners)) }
                      | b <- survivors ]
      existingNames = map branchName base
      newOnes       = [ Branch n tid | (n, Just tid) <- Map.toList winners, n `notElem` existingNames ]
  in applied ++ newOnes

-- | Eager 'StoryStorage' interpreter: every ref mutation lands in git
--   immediately. This is the default used everywhere except inside 'withStorage'.
runStoryStorageGit
  :: Members '[Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
runStoryStorageGit = fmap (\(a, _, _) -> a) . withStorageWithCallback applyToGit
  where
    applyToGit name (Just tid) = updateRef (storyRef name) (ObjectHash (unTickId tid))
    applyToGit name Nothing    = deleteRef (storyRef name)

-- | Transactional 'StoryStorage': ref mutations made by the wrapped action
--   are buffered in memory (never touch git) and, only once the action
--   succeeds, replayed into the parent 'StoryStorage' as a single batch of
--   'setRef' calls plus (if anything was renamed) one combined
--   'updateReferences' call. If the action fails, nothing is replayed and
--   nothing was ever written — same effect as if the action never ran.
--
--   Refs are replayed as at most one 'setRef' per branch, keeping only
--   each branch's last buffered write ('lastPerBranch') — the same last-
--   write-wins collapse 'overlayRefs' already applies for reads. Renames
--   are replayed as every one accumulated (see 'withStorageWithCallback's
--   'ovRemap' — no analogous collapse: unlike a ref position, an id rename
--   isn't superseded by a later one for the *same* id in the way two
--   writes to the same branch are, so nothing here should be dropped). A
--   single logical mutation (e.g. a chain move that rebases a whole tail,
--   which nests two 'at' calls and a multi-entry 'updateReferences'
--   cascade) buffers several intermediate writes; replaying every one of
--   them individually into the parent 'StoryStorage' would make each
--   intermediate state real and independently observable — one eager ref
--   write (and one rename notification), per intermediate step instead of
--   one for the whole transaction. Collapsing first ensures only the
--   final, coherent state is ever published — and gives a client tracking
--   an id through this rebase exactly the mapping it needs, no more.
withStorage
  :: Members '[StoryStorage, Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
withStorage action = do
  (a, refs, remap) <- withStorageWithCallback (\_ _ -> pure ()) action
  mapM_ (uncurry setRef) (lastPerBranch refs)
  -- 'announceRemap', not 'updateReferences' — every rename here was already
  -- cascaded (or already known, for direct 'UpdateReferences' calls) while
  -- buffered; re-running 'UpdateReferences' against the parent StoryStorage
  -- would cascade the very same, already-correct refs a second time. See
  -- 'AnnounceRemap's own doc.
  when (not (null remap)) $ announceRemap remap
  return a

-- | Keep only the last buffered write per branch — see 'withStorage'.
--   'Map.fromList' already retains the last value for duplicate keys, and
--   'refs' is oldest-first, so this is exactly 'overlayRefs's collapse.
lastPerBranch :: [RefWrite] -> [RefWrite]
lastPerBranch = Map.toList . Map.fromList

-- | Speculative 'StoryStorage': like 'withStorage', ref mutations never
--   touch git while the action runs — but unlike 'withStorage', the
--   accumulated overlay is discarded instead of replayed, even if the
--   action succeeds. Content objects/commits the action wrote are still
--   real git objects (cheap and harmless to leave unreferenced), but no
--   ref anywhere ever moves — a dry run with a hard guarantee, not just a
--   rolled-back transaction.
withStorageDiscard
  :: Members '[Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
withStorageDiscard = fmap (\(a, _, _) -> a) . withStorageWithCallback (\_ _ -> pure ())

-- ---------------------------------------------------------------------------
-- Cross-branch reference cascade
-- ---------------------------------------------------------------------------

-- | Rewrite all story branch commits that reference @old@ as a parent,
--   substituting @new@, then cascade until no more referencing commits remain.
--   Branch refs are updated via @applyRef@ rather than touched directly, so
--   the caller (a 'StoryStorage' interpreter) decides whether that lands in
--   git immediately or is buffered as part of a transaction.
--
--   Takes each branch's current head as an explicit @pairs@ argument rather
--   than reading raw git refs itself — a caller running inside a buffered
--   transaction (see 'withStorage') must pass heads overlaid with its own
--   pending writes so far, not raw git, or a branch already correctly
--   rewritten earlier in the same transaction would still read as
--   pointing at its stale, pre-rewrite head here (nothing lands in real
--   git until the transaction replays) and get wrongly matched and
--   rebuilt a second time from that stale ancestry.
--
--   Only commit parent links are rewritten — trees and blobs are not tick
--   references and are left untouched.
--
--   Takes the whole old→new mapping at once rather than one pair at a
--   time: 'rewriteChain' below walks each branch's ancestry exactly once,
--   checking every commit against the full mapping as it goes, instead of
--   re-walking the same ancestry once per mapping entry. A mapping with M
--   entries against B branches of chain length L used to cost O(M x B x L)
--   (each of the M entries independently re-listing every branch and
--   re-walking each one's full ancestry from head); batching drops that to
--   O(B x L) -- but a second, separate cost remained: a commit's parents
--   include cross-branch tick refs (e.g. a tracked atom's ref back to its
--   source tick), and 'rewriteChain' recurses into those uniformly along
--   with the branch's own chain parent. When a ref-parent's hash *isn't* a
--   mapping key, that recursion doesn't stop -- it walks that foreign
--   commit's own ancestry all the way down to prove nothing there needs
--   rewriting either. With N ref-parents whose targets' ancestries mostly
--   overlap (consecutive ticks on the same source branch nest inside each
--   other), that redid the same overlapping work up to N times: O(N^2)
--   again, just triggered by ref-parent misses instead of mapping size.
--   'rewriteChain' now shares one memo table (@hash -> resolved hash@)
--   across the whole call, so any hash reached by more than one path --
--   a branch's own chain, or several different ref-parents landing in the
--   same region -- is resolved once, not once per path. See
--   bench/PerfCascade.hs for the regression probe this fixes.
--
--   Every branch in @pairs@ is first asked a cheap reachability question
--   -- 'isAncestorOfAny': is any of @mapping@'s keys even an ancestor of
--   this branch's head? -- before 'rewriteChain' walks and re-reads its
--   ancestry at all. A branch whose entire history shares nothing with
--   @mapping@ (e.g. an unrelated story branch, or a character branch that
--   never tracked the edited one) is skipped outright on a "no" answer,
--   rather than discovering the same thing the hard way by walking and
--   reading every commit in it. See 'Runix.Git.IsAncestorOfAny' for how
--   both interpreters answer this in one shot (one native @git rev-list@
--   call, or one in-memory BFS for 'Git.Mock') rather than by doing the
--   walk 'rewriteChain' was trying to avoid in the first place. See
--   [[project_git_write_batching]] / PLAN-storage-monad.md's real-repo
--   bench (@bench/RealGitPerf.hs@) for the measurement this addresses.
-- | Returns every hash 'rewriteChain' actually resolved while walking the
--   given branches — its full memo, not just @mapping@ echoed back. That's
--   strictly *more* than @mapping@ alone in general (a descendant
--   reparented onto a renamed commit isn't itself one of @mapping@'s keys)
--   and generally disjoint from it (a hash that already appears as a key
--   short-circuits before ever touching the memo — see 'rewriteChain') —
--   this is deliberately the caller's job to combine with its own input
--   mapping if it wants the complete picture (see 'withStorageWithCallback'
--   /'ovRemap', which is exactly the seam this exists for: knowing what a
--   rebase renamed beyond what it was told to rename is what lets a client
--   tracking one of those descendant ids learn where it went).
cascadeReplace
  :: Members '[Git, Fail] r
  => [(RefName, ObjectHash)]      -- ^ each branch's current head
  -> (BranchName -> Maybe TickId -> Sem r ())
  -> Map ObjectHash ObjectHash    -- ^ old commit hash -> new commit hash, for every superseded tick
  -> Sem r (Map ObjectHash ObjectHash)
cascadeReplace pairs applyRef mapping =
  execState (Map.empty :: Map ObjectHash ObjectHash) $
    mapM_ (rewriteRef (\n t -> raise (applyRef n t)) mapping) pairs

rewriteRef
  :: Members '[Git, Fail, State (Map ObjectHash ObjectHash)] r
  => (BranchName -> Maybe TickId -> Sem r ())
  -> Map ObjectHash ObjectHash
  -> (RefName, ObjectHash)
  -> Sem r ()
rewriteRef applyRef mapping (ref, headHash) = do
  relevant <- isAncestorOfAny (Map.keys mapping) headHash
  when relevant $ do
    newHead <- rewriteChain mapping headHash
    when (newHead /= headHash) $
      case refBranchName ref of
        Just name -> applyRef name (Just (TickId (unObjectHash newHead)))
        Nothing   -> return ()

-- | Walk a commit chain rewriting any commit that appears as a key in
--   @mapping@, memoizing every hash it resolves along the way (see
--   'cascadeReplace's comment). Returns the (possibly new) head hash.
--   Works bottom-up by recursing to parents first, so a single rewrite
--   propagates upward automatically.
rewriteChain
  :: Members '[Git, Fail, State (Map ObjectHash ObjectHash)] r
  => Map ObjectHash ObjectHash
  -> ObjectHash
  -> Sem r ObjectHash
rewriteChain mapping hash = case Map.lookup hash mapping of
  Just new -> return new
  Nothing  -> do
    memo <- get
    case Map.lookup hash memo of
      Just resolved -> return resolved
      Nothing -> do
        cd         <- RG.readCommit hash
        newParents <- mapM (rewriteChain mapping) (commitParents cd)
        resolved   <- if newParents == commitParents cd
          then return hash
          else RG.writeCommit cd { commitParents = newParents }
        modify (Map.insert hash resolved)
        return resolved

-- ---------------------------------------------------------------------------
-- Storage.Core embedding -- the same conversion this module already does
-- for real git object access, retargeted at "Storage.Core"'s own,
-- differently-named vocabulary.
-- ---------------------------------------------------------------------------

toCoreHash :: ObjectHash -> Core.ObjectHash
toCoreHash = Core.ObjectHash . unObjectHash

fromCoreHash :: Core.ObjectHash -> ObjectHash
fromCoreHash = ObjectHash . Core.unObjectHash

toCoreCommit :: CommitData -> Core.CommitData
toCoreCommit cd = Core.CommitData
  { Core.commitParents = map toCoreHash (commitParents cd)
  , Core.commitTree    = toCoreHash (commitTree cd)
  , Core.commitMessage = commitMessage cd
  }

fromCoreCommit :: Core.CommitData -> CommitData
fromCoreCommit cd = CommitData
  { commitParents = map fromCoreHash (Core.commitParents cd)
  , commitTree    = fromCoreHash (Core.commitTree cd)
  , commitMessage = Core.commitMessage cd
  }

toCoreObject :: GitObject -> Core.StoreObject
toCoreObject (BlobObject bs) = Core.BlobObject bs
toCoreObject (TreeObject es) = Core.TreeObject (map toCoreEntry es)

fromCoreObject :: Core.StoreObject -> GitObject
fromCoreObject (Core.BlobObject bs) = BlobObject bs
fromCoreObject (Core.TreeObject es) = TreeObject (map fromCoreEntry es)

toCoreEntry :: TreeEntry -> Core.TreeEntry
toCoreEntry (BlobEntry n h) = Core.BlobEntry n (toCoreHash h)
toCoreEntry (SubTree n h)   = Core.SubTree n (toCoreHash h)

fromCoreEntry :: Core.TreeEntry -> TreeEntry
fromCoreEntry (Core.BlobEntry n h) = BlobEntry n (fromCoreHash h)
fromCoreEntry (Core.SubTree n h)   = SubTree n (fromCoreHash h)

-- | Any 'Sem' stack able to read/write git objects and fail is a
--   'Core.MonadStore' for free -- the instance every "Storage.Core"\/
--   "Storage.Ops"\/"Storage.Tick" operation runs against once dispatched
--   through 'runBranchOpGit'. Method names are qualified in this instance
--   body -- 'Storage.Core' deliberately reuses "Runix.Git"'s own
--   unqualified names ('readCommit'\/'writeCommit'\/'readObject'\/
--   'writeObject'), unlike a @git@-prefixed naming scheme above, so
--   qualification is the only way to say which one is meant here.
instance Members '[Git, Fail] r => Core.MonadStore (Sem r) where
  readCommit  h  = toCoreCommit <$> RG.readCommit (fromCoreHash h)
  writeCommit cd = toCoreHash <$> RG.writeCommit (fromCoreCommit cd)
  readObject  h  = toCoreObject <$> RG.readObject (fromCoreHash h)
  writeObject o  = toCoreHash <$> RG.writeObject (fromCoreObject o)

-- | Interpret 'BranchOp branch' (declared, backend-agnostically, in
--   'Storyteller.Core.Branch') against real git. Seeds the scope's
--   'Core.ScopeState' from 'StoryStorage' once, at entry, and carries it
--   forward across every dispatch via 'Core.runStoreTFrom' (rather than
--   reloading fresh each time) — so a pending, uncommitted ambient-tree
--   edit from one dispatch (e.g. one Runix 'FileSystem' tool call) is
--   still there for the next one on the same scope, and the remap table
--   accumulates across the whole scope's lifetime too, not just one
--   dispatch. Publishes the final head via 'setRef' whenever a dispatch
--   actually advanced it, and always broadcasts whatever the dispatch's
--   own remap table picked up via @updateReferences@ (a no-op when it's
--   empty) — re-resolving from 'StoryStorage' afterward, since that
--   cascade can rewrite *this* branch's own ref a second time (e.g. a
--   tick between a merged run and the original head carrying a ref into
--   the merged range).
runBranchOpGit
  :: forall branch r a
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName
  -> Sem (BranchOp branch : r) a
  -> Sem r a
runBranchOpGit branch action = do
  b <- getBranch branch >>= \case
    Nothing -> fail $ "branch not found: " <> T.unpack (unBranchName branch)
    Just b' -> pure b'
  seed0 <- Core.freshScope (Core.ObjectHash (unTickId (branchHead b)))
  evalState seed0 $ interpret
    (\case
        RunStorage comp -> do
          scope@(h, _, _) <- get @Core.ScopeState
          (result, scope'@(h', _, table)) <- Core.runStoreTFrom scope comp
          put @Core.ScopeState scope'
          let mapping = [ (TickId (Core.unObjectHash o), TickId (Core.unObjectHash n))
                        | (o, n) <- Map.toList table ]
          -- Publish the ref whenever this dispatch actually advanced head
          -- -- unconditionally, regardless of whether any remap happened.
          when (h' /= h) $ setRef branch (Just (TickId (Core.unObjectHash h')))
          -- Only broadcast/reload when a real cross-branch remap
          -- happened (a non-empty table) -- a plain append or edit with
          -- nothing to cascade has no reason to distrust its own,
          -- already-correct 'scope''; reloading unconditionally on every
          -- head move would discard legitimate pending ambient-tree state
          -- for files 'comp' never touched (e.g. another file's own
          -- not-yet-committed edit sitting in the same scope).
          when (not (Map.null table)) $ do
            updateReferences mapping
            -- The cascade just broadcast may have rewritten *this*
            -- branch's ref a second time -- re-resolve and reload fresh
            -- rather than trust 'h'' itself. Keeps the remap table
            -- accumulated so far -- 'Core.freshScope's own table is always
            -- empty, only its (head, tree) pair is wanted here.
            mB <- getBranch branch
            case mB of
              Just b' -> do
                let newHash = Core.ObjectHash (unTickId (branchHead b'))
                (_, wt, _) <- Core.freshScope newHash
                put @Core.ScopeState (newHash, wt, table)
              Nothing -> return ()
          return (result, mapping)
    )
    (raiseUnder @(State Core.ScopeState) action)

-- ---------------------------------------------------------------------------
-- FileSystem effects for a branch — kept as the ambient-file-access
-- interface (unchanged for callers), reinterpreted against the new engine
-- ---------------------------------------------------------------------------

-- | Kind-* wrapper carrying the branch type-level tag.
--   Used as the @project@ parameter for filesystem effects, so that
--   @FileSystem (BranchTag branch)@ is unambiguous on the effect stack.
newtype BranchTag (branch :: k) = BranchTag BranchName

-- | Every path under @root@ (recursively) -- @root@ of @"/"@\/@"."@\/@""@
--   means the whole tree. 'FS.list' itself is already every file
--   anywhere with no directories; this just adds the root-scoping 'Glob'
--   needs.
listAllUnder :: Core.StoreM m => FilePath -> Core.StoreT m [FilePath]
listAllUnder root = filter isRootOrUnder <$> FS.list
  where
    isRootOrUnder p
      | root `elem` ["/", ".", ""] = True
      | otherwise                  = p == root || List.isPrefixOf (root <> "/") p

-- | Interpret the three ambient 'FileSystem' effects for a branch against
--   'BranchOp branch' — every operation is a single 'runStorage'
--   dispatch against the same persistent 'Core.ScopeState' a branch's
--   other 'runStorage' calls already share (see 'runBranchOpGit'), so a
--   plain file read/write interleaves freely with a chain edit, or with
--   another file operation from a separate dispatch, on the same scope.
--   These effects were always first-order (no continuation in any
--   constructor), so this was never the expensive part being replaced.
runStoryFSGit
  :: forall branch r a
  .  Member (BranchOp branch) r
  => BranchName
  -> Sem ( FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
runStoryFSGit name = interpretFS . interpretFSRead . interpretFSWrite
  where
    interpretFS
      :: Members '[BranchOp branch] r'
      => Sem (FileSystem (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFS = interpret $ \case
      GetFileSystem ->
        return (BranchTag name)
      GetCwd ->
        return $ Right "/"
      ListFiles dir ->
        Right . fst <$> runStorage @branch (FS.listChildren dir)
      FileExists path ->
        Right . fst <$> runStorage @branch
          ((||) <$> Ops.exists path <*> FS.isDirectory path)
      IsDirectory path ->
        Right . fst <$> runStorage @branch (FS.isDirectory path)
      -- Working-tree paths carry a leading @/@, but glob patterns are
      -- written relative (@\"chapters/*.md\"@), so that leading slash is
      -- stripped before matching — only for the match, not for what's
      -- returned, so callers keep seeing the same path shape 'ListFiles'
      -- and friends already give them.
      Glob base pat ->
        Right . filter (Glob.match (Glob.compile pat) . dropWhile (== '/')) . fst
          <$> runStorage @branch (listAllUnder base)

    interpretFSRead
      :: Members '[BranchOp branch] r'
      => Sem (FileSystemRead (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSRead = interpret $ \case
      ReadFile path ->
        fst <$> runStorage @branch (do
          exists <- Ops.exists path
          isDir  <- FS.isDirectory path
          if isDir then return (Left (path <> ": is a directory"))
          else if not exists then return (Left (path <> ": not found"))
          else Right <$> Core.readFile path)

    interpretFSWrite
      :: Members '[BranchOp branch] r'
      => Sem (FileSystemWrite (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSWrite = interpret $ \case
      WriteFile path content ->
        Right . fst <$> runStorage @branch (FS.writeFile path content)
      CreateDirectory _recursive path ->
        Right . fst <$> runStorage @branch (FS.createDirectory path)
      Remove recursive path ->
        Right . fst <$> runStorage @branch (if recursive then FS.removeRecursive path else FS.remove path)

-- | Interpret 'BranchOp branch' and all three filesystem effects for a
--   branch together — takes a 'Branch' obtained from 'StoryStorage' — the
--   storage layer is the authority on which branches exist and are
--   accessible. Callers must go through 'getBranch' or 'createBranch'
--   before opening a branch here.
runBranchAndFS
  :: forall branch r a
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName
  -> Sem ( FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : BranchOp branch
         : r ) a
  -> Sem r a
runBranchAndFS name = runBranchOpGit @branch name . runStoryFSGit @branch name

-- ---------------------------------------------------------------------------
-- Generic rebase — for callers whose inner action interleaves other
-- effects (LLM, logging, ...), not just storage
-- ---------------------------------------------------------------------------
--
-- "Storage.Core"'s @at@ only accepts a closed-form 'Core.StoreT'
-- computation — it can't run arbitrary 'Sem' code (an LLM call, another
-- dispatch), because 'Core.StoreT' isn't a Polysemy effect stack. Every
-- other chain-editing caller only ever needs closed-form storage, which is
-- what makes the fast, dispatch-once 'runStorage' path correct for them.
--
-- The rebase-marker feature ("run this arbitrary command as if an earlier
-- tick were HEAD") is different: its inner action is a recursive dispatch
-- call that can invoke agents, LLM calls, anything. It still doesn't need
-- 'interpretH', though: descent is ordinary 'Sem' recursion, one small
-- 'runStorage' dispatch per tick popped ('runBranchOpGit' carries the
-- scope's state across those dispatches), 'inner' runs at the bottom as
-- perfectly ordinary in-between 'Sem' code, and the whole tail is then
-- replayed in a single 'Core.StoreT' computation — one dispatch, so the
-- cross-branch cascade fires once with the full old->new mapping instead
-- of once per replayed tick (see 'replayBack').

-- | Move to @tid@'s position, run an arbitrary inner action there, then
--   replay everything after it back onto whatever the action produced.
--   Descent pops one tick per 'runStorage' dispatch; the replay is a
--   single 'StoreT' dispatch (see 'replayBack') that hands the whole
--   old->new remap table to 'runBranchOpGit' at once, so the cross-branch
--   cascade fires exactly once for the entire tail.
atGeneric
  :: forall branch r a
  .  Members '[BranchOp branch, Git, Fail] r
  => TickId -> Sem r a -> Sem r a
atGeneric tid inner = goDown []
  where
    target = Core.ObjectHash (unTickId tid)

    goDown :: [(Core.ObjectHash, Core.Tick)] -> Sem r a
    goDown poppedRev = do
      (current, _) <- runStorage @branch Core.headHash
      if current == target
        then do
          a <- inner
          replayBack poppedRev
          return a
        else do
          (t, _) <- runStorage @branch $ do
            cd <- lift (Core.readCommit current)
            case Core.commitParents cd of
              [] -> fail $ "atGeneric: tick " <> T.unpack (unTickId tid) <> " not found in branch history"
              (_ : _) -> Core.drop
          goDown ((current, t) : poppedRev)

    -- One 'StoreT' dispatch for the whole tail: each 'store' builds on the
    --   previous (its head advances via 'Core.putHead' inside the 'StateT'),
    --   so 'runBranchOpGit' receives a single remap table covering every
    --   replayed tick and runs the cross-branch cascade once with it --
    --   rather than once per tick (O(N) cascades, O(N^2) remap entries).
    replayBack :: [(Core.ObjectHash, Core.Tick)] -> Sem r ()
    replayBack popped = () <$ runStorage @branch
      (mapM_ (\(oldHash, t) -> Core.store t >>= \newHash -> Core.logRemap oldHash newHash) popped)
