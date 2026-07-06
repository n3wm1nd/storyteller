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
--   * Tick messages are encoded per 'Storyteller.Core.StorageMonad.encodeTickData'.
--   * Each tick (commit) carries the full working-tree snapshot as its tree object.
--
-- Per-branch tick-chain operations (the old @StoryBranch@ effect, with its
-- higher-order @At@\/@WithFS@ constructors) have been replaced by
-- 'Storyteller.Core.StorageMonad.StorageT' plus 'Storyteller.Core.Branch.BranchOp'
-- -- see PLAN-storage-monad.md for why. 'StoryStorage' (branch
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
  , runStorageEdit
  , runStoryFSGit
  , runBranchAndFS

    -- * Generic rebase (for inner actions that interleave other effects)
  , atGeneric
  , readAtGeneric

    -- * Cross-branch reference cascade (exported for its own unit tests --
    -- see 'Storyteller.GitCascadeSpec')
  , cascadeReplace
  ) where

import Prelude
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)
import Polysemy.State (State, get, put, modify, evalState, runState)

import Runix.Git
import Runix.FileSystem
  ( FileSystem(..), FileSystemRead(..), FileSystemWrite(..) )

import Storyteller.Core.Types hiding (draft)
import Storyteller.Core.Storage
import Storyteller.Core.Branch (BranchOp(..), runStorage, runStorageEdit)
import qualified Storyteller.Core.StorageMonad as SM

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
withStorageWithCallback
  :: forall r a
  .  Members '[Git, Fail] r
  => (BranchName -> Maybe TickId -> Sem r ())
  -> Sem (StoryStorage : r) a
  -> Sem r (a, [RefWrite])
withStorageWithCallback onRef action =
  swap <$> runState [] (reinterpret handle action)
  where
    handle :: StoryStorage m x -> Sem (State [RefWrite] : r) x
    handle = \case
      CreateBranch name -> do
        let ref = storyRef name
        existing <- raise $ resolveRef ref
        case existing of
          Just _ -> raise $ fail $ "branch already exists: " <> T.unpack (unBranchName name)
          Nothing -> do
            rootHash <- raise $ writeCommit CommitData
              { commitParents = []
              , commitTree    = emptyTree
              , commitMessage = SM.encodeTickData (toDraft (Root name))
              }
            let tid = TickId (unObjectHash rootHash)
            applyRef name (Just tid)
            return Branch { branchName = name, branchHead = tid }

      DeleteBranch name -> applyRef name Nothing

      ListBranches -> do
        pairs   <- raise $ listRefs storyRefPrefix
        pending <- get
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
        pending <- get
        let current = [ (storyRef (branchName b), ObjectHash (unTickId (branchHead b)))
                      | b <- overlayRefs (map resolveToHead pairs) pending ]
            hashMapping = Map.fromList
              [ (ObjectHash (unTickId o), ObjectHash (unTickId n)) | (o, n) <- mapping ]
        cascadeReplace current applyRef hashMapping

      SetRef name mtid -> applyRef name mtid

    -- Appended, not prepended: 'overlayRefs' and 'withStorage's replay both
    -- rely on later entries for the same branch winning over earlier ones.
    applyRef :: BranchName -> Maybe TickId -> Sem (State [RefWrite] : r) ()
    applyRef name mtid = do
      raise $ onRef name mtid
      modify (++ [(name, mtid)])

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
runStoryStorageGit = fmap fst . withStorageWithCallback applyToGit
  where
    applyToGit name (Just tid) = updateRef (storyRef name) (ObjectHash (unTickId tid))
    applyToGit name Nothing    = deleteRef (storyRef name)

-- | Transactional 'StoryStorage': ref mutations made by the wrapped action
--   are buffered in memory (never touch git) and, only once the action
--   succeeds, replayed as a single batch of 'setRef' calls into the parent
--   'StoryStorage'. If the action fails, nothing is replayed and nothing
--   was ever written — same effect as if the action never ran.
--
--   Replayed as at most one 'setRef' per branch, keeping only each
--   branch's last buffered write ('lastPerBranch') — the same last-write-
--   wins collapse 'overlayRefs' already applies for reads. A single
--   logical mutation (e.g. 'Storyteller.Core.StorageMonad.moveTick', which
--   nests two 'at' calls and a multi-entry 'updateReferences' cascade)
--   buffers several intermediate writes to the same branch; replaying
--   every one of them individually into the parent 'StoryStorage' would
--   make each intermediate state real and independently observable — one
--   eager ref write, and one 'RefMoved' notification, per intermediate
--   step instead of one for the whole transaction. Collapsing first
--   ensures only the final, coherent state is ever published.
withStorage
  :: Members '[StoryStorage, Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
withStorage action = do
  (a, refs) <- withStorageWithCallback (\_ _ -> pure ()) action
  mapM_ (uncurry setRef) (lastPerBranch refs)
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
withStorageDiscard = fmap fst . withStorageWithCallback (\_ _ -> pure ())

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
cascadeReplace
  :: Members '[Git, Fail] r
  => [(RefName, ObjectHash)]      -- ^ each branch's current head
  -> (BranchName -> Maybe TickId -> Sem r ())
  -> Map ObjectHash ObjectHash    -- ^ old commit hash -> new commit hash, for every superseded tick
  -> Sem r ()
cascadeReplace pairs applyRef mapping =
  evalState (Map.empty :: Map ObjectHash ObjectHash) $
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
        cd         <- readCommit hash
        newParents <- mapM (rewriteChain mapping) (commitParents cd)
        resolved   <- if newParents == commitParents cd
          then return hash
          else writeCommit cd { commitParents = newParents }
        modify (Map.insert hash resolved)
        return resolved

-- ---------------------------------------------------------------------------
-- Storage monad embedding — see PLAN-storage-monad.md
-- ---------------------------------------------------------------------------

-- | Both sides are newtype-wrapped 'Text' with the same payload — this
--   pair (and 'toSMCommit'\/'fromSMCommit'\/'toSMObject'\/'fromSMObject'\/
--   'toSMEntry'\/'fromSMEntry' below) only exists because
--   'Storyteller.Core.StorageMonad' deliberately defines its own generic
--   vocabulary rather than importing "Runix.Git"'s identically-shaped
--   types, so nothing above 'SM.MonadGit' ever needs to know real git
--   hashes/commits/trees are what's actually flowing through — this
--   module is the one seam that converts between the two.
toSMHash :: ObjectHash -> SM.ObjectHash
toSMHash = SM.ObjectHash . unObjectHash

fromSMHash :: SM.ObjectHash -> ObjectHash
fromSMHash = ObjectHash . SM.unObjectHash

toSMCommit :: CommitData -> SM.CommitData
toSMCommit cd = SM.CommitData
  { SM.commitParents = map toSMHash (commitParents cd)
  , SM.commitTree    = toSMHash (commitTree cd)
  , SM.commitMessage = commitMessage cd
  }

fromSMCommit :: SM.CommitData -> CommitData
fromSMCommit cd = CommitData
  { commitParents = map fromSMHash (SM.commitParents cd)
  , commitTree    = fromSMHash (SM.commitTree cd)
  , commitMessage = SM.commitMessage cd
  }

toSMObject :: GitObject -> SM.GitObject
toSMObject (BlobObject bs) = SM.BlobObject bs
toSMObject (TreeObject es) = SM.TreeObject (map toSMEntry es)

fromSMObject :: SM.GitObject -> GitObject
fromSMObject (SM.BlobObject bs) = BlobObject bs
fromSMObject (SM.TreeObject es) = TreeObject (map fromSMEntry es)

toSMEntry :: TreeEntry -> SM.TreeEntry
toSMEntry (BlobEntry n h) = SM.BlobEntry n (toSMHash h)
toSMEntry (SubTree n h)   = SM.SubTree n (toSMHash h)

fromSMEntry :: SM.TreeEntry -> TreeEntry
fromSMEntry (SM.BlobEntry n h) = BlobEntry n (fromSMHash h)
fromSMEntry (SM.SubTree n h)   = SubTree n (fromSMHash h)

-- | Any 'Sem' stack able to read/write git objects and fail is a
--   'SM.MonadGit' for free — this is the one instance
--   'Storyteller.Core.StorageMonad' needs to reuse every tick\/tree
--   operation it defines against real git, with the same 'Git' effect
--   (and so the same mock/real interpreter swap) everything else here
--   uses. Converts between git's own types and 'SM's generic vocabulary
--   at exactly this one seam.
instance Members '[Git, Fail] r => SM.MonadGit (Sem r) where
  gitReadCommit  h  = toSMCommit <$> readCommit (fromSMHash h)
  gitWriteCommit cd = toSMHash <$> writeCommit (fromSMCommit cd)
  gitReadObject  h  = toSMObject <$> readObject (fromSMHash h)
  gitWriteObject o  = toSMHash <$> writeObject (fromSMObject o)

-- | Interpret 'BranchOp branch' (declared, backend-agnostically, in
--   'Storyteller.Core.Branch') against real git. Seeds the scope's
--   (head, working tree) state from 'StoryStorage' once, at entry — a
--   snapshot, same as the old @StoryBranch@ interpreter's was — and
--   publishes the final head via 'setRef' after every dispatch whose
--   'StorageT' computation actually advanced it, exactly once per
--   dispatch no matter how many ticks were rewritten inside it. A
--   dispatch that only read (no write) leaves the head where it started
--   and publishes nothing.
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
  let headHash0 = SM.ObjectHash (unTickId (branchHead b))
  wt0 <- SM.loadWorkingTree headHash0
  evalState wt0 . evalState headHash0 $ interpret
    (\case
        RunStorage comp -> do
          h  <- get @SM.ObjectHash
          wt <- get @SM.WorkingTree
          (a, (h', wt')) <- SM.runStorageT h wt comp
          put @SM.ObjectHash h'
          put @SM.WorkingTree wt'
          when (h' /= h) $ setRef branch (Just (TickId (SM.unObjectHash h')))
          return a

        RunStorageEdit comp -> do
          h  <- get @SM.ObjectHash
          wt <- get @SM.WorkingTree
          (result@(_, mapping), (h', wt')) <- SM.runStorageT h wt comp
          put @SM.ObjectHash h'
          put @SM.WorkingTree wt'
          when (h' /= h) $ setRef branch (Just (TickId (SM.unObjectHash h')))
          updateReferences mapping
          -- The cascade 'updateReferences' just ran may have rewritten
          -- *this* branch's ref a second time (see this function's own
          -- doc) — re-resolve and reload from git rather than trust the
          -- state this dispatch itself just wrote.
          mB <- getBranch branch
          case mB of
            Just b' -> do
              let newHash = SM.ObjectHash (unTickId (branchHead b'))
              newWt <- SM.loadWorkingTree newHash
              put @SM.ObjectHash newHash
              put @SM.WorkingTree newWt
            Nothing -> return ()
          return result
    )
    (raiseUnder @(State SM.ObjectHash) (raiseUnder @(State SM.WorkingTree) action))

-- ---------------------------------------------------------------------------
-- FileSystem effects for a branch — kept as the ambient-file-access
-- interface (unchanged for callers), reinterpreted against the new engine
-- ---------------------------------------------------------------------------

-- | Kind-* wrapper carrying the branch type-level tag.
--   Used as the @project@ parameter for filesystem effects, so that
--   @FileSystem (BranchTag branch)@ is unambiguous on the effect stack.
newtype BranchTag (branch :: k) = BranchTag BranchName

-- | Interpret the three ambient 'FileSystem' effects for a branch against
--   'BranchOp branch' — every operation is a single 'runStorage'
--   dispatch against the same persistent (head, working tree) state a
--   branch's other 'runStorage'\/'runStorageEdit' calls already share, so
--   a plain file read/write interleaves freely with a chain edit on the
--   same scope. These effects were always first-order (no continuation in
--   any constructor), so this was never the expensive part being replaced
--   — this interpreter just retargets them from the old shared-'State
--   WorkingTree' plumbing onto 'BranchOp'.
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
        Right <$> runStorage @branch (SM.listFilesS dir)
      FileExists path ->
        Right <$> runStorage @branch (SM.fileExistsS path)
      IsDirectory path ->
        Right <$> runStorage @branch (SM.isDirectoryS path)
      Glob _base _pat ->
        return $ Left "Branch FS: Glob not yet implemented"

    interpretFSRead
      :: Members '[BranchOp branch] r'
      => Sem (FileSystemRead (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSRead = interpret $ \case
      ReadFile path ->
        runStorage @branch $ do
          exists <- SM.fileExistsS path
          isDir  <- SM.isDirectoryS path
          if isDir then return (Left (path <> ": is a directory"))
          else if not exists then return (Left (path <> ": not found"))
          else Right <$> SM.readFileS path

    interpretFSWrite
      :: Members '[BranchOp branch] r'
      => Sem (FileSystemWrite (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSWrite = interpret $ \case
      WriteFile path content ->
        Right <$> runStorage @branch (SM.writeFileS path content)
      CreateDirectory _recursive path ->
        Right <$> runStorage @branch (SM.createDirectoryS path)
      Remove recursive path ->
        Right <$> runStorage @branch (SM.removeS recursive path)

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
-- 'Storyteller.Core.StorageMonad.at' only accepts a closed-form 'StorageT'
-- computation — it can't run arbitrary 'Sem' code (an LLM call, another
-- dispatch), because 'StorageT' isn't a Polysemy effect stack. Every
-- caller ported so far (moveTick, mergeAtoms, splitTick, ...) only ever
-- needed closed-form storage, which is what makes the fast, dispatch-once
-- 'runStorage' path correct for them.
--
-- The rebase-marker feature ("run this arbitrary command as if an earlier
-- tick were HEAD") is different: its inner action is a recursive dispatch
-- call that can invoke agents, LLM calls, anything. It still doesn't need
-- 'interpretH', though — 'BranchOp's own (head, working tree) state is
-- ordinary Polysemy 'State' internal to 'runBranchOpGit', so plain,
-- ordinary 'Sem' recursion can move it down one tick at a time, let
-- arbitrary code run at the bottom, and walk back up replaying each
-- level's diff by hand — exactly 'Storyteller.Core.StorageMonad.at's
-- algorithm, just with the "run the action" step happening via ordinary
-- effect dispatch instead of inside a single 'StorageT' computation.

-- | Move to @tid@'s position (checking out its snapshot as the ambient
--   working tree, same as @at@), run an arbitrary inner action there, then
--   replay everything after it back onto whatever the action produced.
--   Returns the inner result and the old->new id mapping for every
--   rewritten tick. Does not broadcast the mapping — pair with
--   'Storyteller.Core.Storage.updateReferences', as every caller of
--   'Storyteller.Core.StorageMonad.at'-family operations already does.
atGeneric
  :: forall branch r a
  .  Members '[BranchOp branch, Git, Fail] r
  => TickId -> Sem r a -> Sem r (a, [(TickId, TickId)])
atGeneric tid inner = do
  outerHead <- runStorage @branch SM.headTickId
  (a, mapping, finalHead) <- goDown outerHead
  runStorage @branch (SM.syncTo finalHead)
  return (a, mapping)
  where
    -- Entirely in 'SM.ObjectHash' space throughout -- every hash here
    -- comes from or feeds into 'SM.gitReadCommit'\/'SM.gitWriteCommit'\/
    -- 'SM.loadWorkingTree', never a raw git effect call, so there is
    -- nothing here for 'Runix.Git's own 'ObjectHash' to do.
    goDown :: SM.ObjectHash -> Sem r (a, [(TickId, TickId)], SM.ObjectHash)
    goDown current
      | TickId (SM.unObjectHash current) == tid = do
          a <- inner
          h <- runStorage @branch SM.headTickId
          return (a, [], h)
      | otherwise = do
          cd <- SM.gitReadCommit current
          case SM.commitParents cd of
            [] -> fail $ "At: tick " <> T.unpack (unTickId tid) <> " not found in branch history"
            (parent : _) -> do
              parentWt <- SM.loadWorkingTree parent
              commitWt <- SM.loadWorkingTree current
              runStorage @branch (SM.syncTo parent)
              (a, innerMapping, newParent) <- goDown parent
              newParentWt <- SM.loadWorkingTree newParent
              newWt       <- SM.applyDiff parentWt commitWt newParentWt
              treeHash    <- SM.flushWorkingTree newWt
              newHash     <- SM.gitWriteCommit cd
                { SM.commitParents = newParent : drop 1 (SM.commitParents cd)
                , SM.commitTree    = treeHash
                }
              let oldId = TickId (SM.unObjectHash current)
                  newId = TickId (SM.unObjectHash newHash)
              return (a, innerMapping <> [(oldId, newId)], newHash)

-- | Read-only counterpart of 'atGeneric': checks out @tid@'s snapshot, runs
--   the inner action, then restores the scope to exactly where it started
--   — no replay, no mapping, no write. Validates that @tid@ actually
--   precedes the current head before moving anything.
readAtGeneric
  :: forall branch r a
  .  Members '[BranchOp branch, Git, Fail] r
  => TickId -> Sem r a -> Sem r a
readAtGeneric tid inner = do
  outerHead <- runStorage @branch SM.headTickId
  validateAncestry outerHead
  runStorage @branch (SM.syncTo (SM.ObjectHash (unTickId tid)))
  a <- inner
  runStorage @branch (SM.syncTo outerHead)
  return a
  where
    validateAncestry current
      | TickId (SM.unObjectHash current) == tid = return ()
      | otherwise = do
          cd <- SM.gitReadCommit current
          case SM.commitParents cd of
            []      -> fail $ "At: tick " <> T.unpack (unTickId tid) <> " not found in branch history"
            (p : _) -> validateAncestry p
