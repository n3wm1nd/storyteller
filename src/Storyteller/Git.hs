{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Git-backed interpreters for StoryStorage and StoryBranch.
--
-- Conventions owned by this layer (invisible to everything above):
--
--   * Branch refs live at  @refs/heads/story/<name>@
--   * Tick messages are encoded as:
--       @refs: <hash1> <hash2> ...\n<message>@   when refs are present
--       @<message>@                               otherwise
--   * Each tick (commit) carries the full working-tree snapshot as its tree object.
--   * 'WorkingTree' is a complete in-memory filesystem: files carry their content,
--     directories are explicit empty entries.  On 'Store' it is serialised to a git
--     tree object; on branch checkout it is reconstructed from the commit's tree.
module Storyteller.Git
  ( -- * Branch tag for filesystem effects
    BranchTag(..)

    -- * Interpreters
  , runStoryStorageGit
  , withStorage
  , withStorageDiscard
  , runStoryBranchGit
  , runStoryFSGit

    -- * Ref naming
  , refBranchName

    -- * Working tree (in-memory filesystem)
  , FSNode(..)
  , WorkingTree
  , emptyWorkingTree
  , loadWorkingTree
  , loadTree

    -- * Combined branch + filesystem interpreter (storage-agnostic interface)
  , runBranchAndFS
  ) where

import Prelude
import Control.Monad (foldM, when)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (splitDirectories, joinPath)
import Polysemy
import Polysemy.Fail
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)
import Polysemy.State (State, get, put, modify, evalState, runState)
import Polysemy.Internal (raiseUnder, raiseUnder3)

import Runix.Git
import Runix.FileSystem
  ( FileSystem(..), FileSystemRead(..), FileSystemWrite(..) )

import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

import Storyteller.Types
import Storyteller.Storage hiding (get, drop, Get)
import qualified Storyteller.Storage as S

-- ---------------------------------------------------------------------------
-- In-memory filesystem
-- ---------------------------------------------------------------------------

-- | Kind-* wrapper carrying the branch type-level tag.
--   Used as the @project@ parameter for filesystem effects, so that
--   @FileSystem (BranchTag branch)@ is unambiguous on the effect stack.
newtype BranchTag (branch :: k) = BranchTag BranchName

-- | A node in the in-memory working tree.
--
--   'FSFile' carries only the git blob hash; content is written to git eagerly
--   on every 'WriteFile' (mandatory, not a side-effect) and fetched via
--   'ReadBlob' on demand.  This mirrors git's index/staging-area semantics:
--   the working tree is a pure hash-addressed index, no content in RAM.
--   'FSDir' represents an explicit, possibly-empty directory.
data FSNode
  = FSFile !ObjectHash
  | FSDir
  deriving (Show, Eq)

-- | The complete in-memory filesystem, keyed by path.
--   Directories are stored explicitly so empty dirs round-trip.
--   Path separator convention: forward slash, no trailing slash, relative.
type WorkingTree = Map FilePath FSNode

emptyWorkingTree :: WorkingTree
emptyWorkingTree = Map.empty

-- ---------------------------------------------------------------------------
-- WorkingTree ↔ git tree serialisation
-- ---------------------------------------------------------------------------

-- | Reconstruct a 'WorkingTree' from a git commit's tree object.
--   Reads the flat tree at the commit; subtrees are read recursively.
loadWorkingTree :: Members '[Git, Fail] r => ObjectHash -> Sem r WorkingTree
loadWorkingTree commitHash = do
  cd <- readCommit commitHash
  readTreeRecursive "" (commitTree cd)

-- | Reconstruct a 'WorkingTree' directly from a git tree hash (not a commit).
loadTree :: Members '[Git, Fail] r => ObjectHash -> Sem r WorkingTree
loadTree = readTreeRecursive ""

readTreeRecursive
  :: Members '[Git, Fail] r
  => FilePath    -- ^ path prefix (empty at root)
  -> ObjectHash
  -> Sem r WorkingTree
readTreeRecursive prefix treeHash = do
  entries <- readTree treeHash
  fmap (Map.unions) $ mapM (readEntry prefix) entries
  where
    readEntry pfx (BlobEntry name hash) = do
      let path = if null pfx then name else pfx <> "/" <> name
      return $ Map.singleton path (FSFile hash)
    readEntry pfx (SubTree name hash) = do
      let path = if null pfx then name else pfx <> "/" <> name
      sub <- readTreeRecursive path hash
      -- insert an explicit FSDir entry for the directory itself
      return $ Map.insert path FSDir sub

-- | Write the 'WorkingTree' to git, returning the root tree hash.
--   Builds the tree hierarchy bottom-up from the flat path map.
flushWorkingTree :: Members '[Git, Fail] r => WorkingTree -> Sem r ObjectHash
flushWorkingTree wt
  | Map.null wt = return emptyTree
  | otherwise   = buildTree "" wt

-- | Build git tree objects for a subtree rooted at @prefix@.
buildTree
  :: Members '[Git, Fail] r
  => FilePath    -- ^ directory prefix (empty for root)
  -> WorkingTree -- ^ the full tree (we select entries belonging to this dir)
  -> Sem r ObjectHash
buildTree prefix wt = do
  let children = directChildren prefix wt
  entries <- mapM (\name -> toEntry prefix name wt) children
  writeTree entries

-- | List the immediate child names under @prefix@.
directChildren :: FilePath -> WorkingTree -> [String]
directChildren prefix wt =
  let prefixParts = if null prefix then [] else splitDirectories prefix
      len         = length prefixParts
      names       = [ c
                    | p <- Map.keys wt
                    , let parts = splitDirectories p
                    , length parts > len
                    , take len parts == prefixParts
                    , c : _ <- [drop len parts]
                    ]
  in dedupe names
  where
    dedupe [] = []
    dedupe (x:xs) = x : dedupe (filter (/= x) xs)

toEntry
  :: Members '[Git, Fail] r
  => FilePath   -- ^ parent prefix
  -> FilePath   -- ^ child name (single component)
  -> WorkingTree
  -> Sem r TreeEntry
toEntry prefix name wt =
  let path = if null prefix then name else prefix <> "/" <> name
  in case Map.lookup path wt of
    Just (FSFile hash) ->
      return (BlobEntry name hash)
    _ -> do
      -- directory (explicit FSDir or implicit from deeper entries)
      hash <- buildTree path wt
      return (SubTree name hash)

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

emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- Message encoding / decoding
-- ---------------------------------------------------------------------------

-- | Encode a 'TickData' into a git commit message.
--   Format:
--     refs: <hash1> <hash2>   (omitted when empty)
--     <key>:<value>           (one line per field, omitted when empty)
--                             (blank line separating headers from body)
--     <message body>
encodeTickData :: TickData -> Text
encodeTickData td =
  let fieldLines = map (\(k, v) -> k <> ":" <> v) (tickFields td)
      body       = tickMessage td
  in if null fieldLines
       then body
       else T.intercalate "\n" fieldLines <> "\n\n" <> body

-- | Decode a git commit message back into refs, fields, and message body.
decodeTickData :: Text -> TickData
decodeTickData raw =
  let ls              = T.lines raw
      (headers, body) = splitHeaders ls
      fields          = [ (k, v)
                        | l <- headers
                        , let (k, rest) = T.breakOn ":" l
                        , not (T.null rest)
                        , let v = T.drop 1 rest ]
  in TickData { tickRefs = [], tickFields = fields, tickMessage = T.intercalate "\n" body }

-- | Split commit message lines into header lines and body lines.
--   Headers end at the first blank line; body is everything after.
--   If there is no blank line, the whole message is treated as body.
splitHeaders :: [Text] -> ([Text], [Text])
splitHeaders ls =
  case break T.null ls of
    (_, []        ) -> ([], ls)           -- no blank line → all body
    (headers, _ : body) -> (headers, body)

-- ---------------------------------------------------------------------------
-- Conversion between git and tick vocabulary
-- ---------------------------------------------------------------------------

commitToTick :: ObjectHash -> CommitData -> Tick
commitToTick hash cd =
  let td      = decodeTickData (commitMessage cd)
      parents = commitParents cd
      pos = TickPos
              { posId     = TickId (unObjectHash hash)
              , posParent = TickId . unObjectHash <$> listToMaybe parents
              , posRefs   = map (TickId . unObjectHash) (drop 1 parents)
              }
  in Tick { tickPos = pos, tickData = td { tickRefs = posRefs pos } }
  where
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

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
              , commitMessage = encodeTickData (toDraft (Root name))
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
      -- head by this point (an 'At'-based rewrite publishes via 'SetRef'
      -- before calling this), so it naturally can't still match any of
      -- 'mapping's superseded ids and won't be redundantly reprocessed.
      -- Reading raw git refs instead would see that branch still at its
      -- *pre-rewrite* head under 'withStorage' (nothing lands in real git
      -- until the transaction replays), matching entries it shouldn't and
      -- rebuilding a second, wrong chain from stale ancestry alongside the
      -- correct one 'At' already built — this is what actually caused a
      -- moved tick's sibling to be duplicated in the chain.
      UpdateReferences mapping ->
        mapM_ (\(o, n) -> do
          pairs   <- raise $ listRefs storyRefPrefix
          pending <- get
          let current = [ (storyRef (branchName b), ObjectHash (unTickId (branchHead b)))
                        | b <- overlayRefs (map resolveToHead pairs) pending ]
          cascadeReplace current applyRef (ObjectHash (unTickId o)) (ObjectHash (unTickId n))
          ) mapping

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
--   logical mutation (e.g. 'moveTick', which nests two 'At' calls and a
--   multi-entry 'updateReferences' cascade) buffers several intermediate
--   writes to the same branch; replaying every one of them individually
--   into the parent 'StoryStorage' would make each intermediate state
--   real and independently observable — one eager ref write, and one
--   'RefMoved' notification, per intermediate step instead of one for the
--   whole transaction. Collapsing first ensures only the final, coherent
--   state is ever published.
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
-- StoryBranch interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'StoryBranch branch' against git.
--   'Store' flushes the shared 'WorkingTree' into a real git tree object.
--   'Drop' and 'At' restore the working tree from the target commit's tree.
--
--   The branch's head is tracked as local 'State ObjectHash', seeded once
--   from 'StoryStorage' here at entry and never re-read afterwards — this
--   scope is a snapshot, same as 'WorkingTree' already was. Every write
--   still publishes via 'setRef' immediately (so a caller not wrapped in
--   'withStorage' keeps today's eager, git-visible-immediately behaviour),
--   but nothing in here reaches back out to 'StoryStorage' to notice
--   writes made by another, concurrently-open scope. A caller that needs a
--   fresh view of a branch after some other write reopens the scope (see
--   'runBranchAndFS'/'Server.Util.withBranch') rather than relying on this
--   interpreter to notice mid-flight — that's what makes the 'withStorage'
--   transaction boundary and this scope's snapshot semantics agree: both
--   sync exactly once, at open.
runStoryBranchGit
  :: forall branch r a
  .  Members '[Git, StoryStorage, Fail, State WorkingTree] r
  => BranchName
  -> Sem (StoryBranch branch : r) a
  -> Sem r a
runStoryBranchGit branch action = do
  headTid0 <- getBranch branch >>= \case
    Just b  -> pure (branchHead b)
    Nothing -> fail $ "branch not found: " <> T.unpack (unBranchName branch)
  let headHash0 = ObjectHash (unTickId headTid0)
  loadWorkingTree headHash0 >>= put
  evalState headHash0 $ interpretH (\case
    Store d -> do
      headHash' <- raise $ get @ObjectHash
      wt'      <- raise $ get @WorkingTree
      parentWt <- raise $ loadWorkingTree headHash'
      eCheck   <- raise $ checkAppendOnly parentWt wt'
      case eCheck of
        Left err -> pureT (Left err)
        Right () -> do
          treeHash <- raise $ flushWorkingTree wt'
          newHash  <- raise $ writeCommit CommitData
            { commitParents = headHash' : map (ObjectHash . unTickId) (tickRefs d)
            , commitTree    = treeHash
            , commitMessage = encodeTickData d
            }
          raise $ put @ObjectHash newHash
          raise $ setRef branch (Just (TickId (unObjectHash newHash)))
          pureT $ Right (TickId (unObjectHash newHash))

    Drop -> do
      headHash' <- raise $ get @ObjectHash
      cd        <- raise $ readCommit headHash'
      case commitParents cd of
        []      -> pureT ()
        (p : _) -> do
          raise $ put @ObjectHash p
          raise $ setRef branch (Just (TickId (unObjectHash p)))
          pureT ()

    S.Get -> do
      headHash' <- raise $ get @ObjectHash
      cd        <- raise $ readCommit headHash'
      pureT $ commitToTick headHash' cd

    S.Reset -> do
      headHash' <- raise $ get @ObjectHash
      wt'       <- raise $ loadWorkingTree headHash'
      raise $ put wt'
      pureT ()

    Follow seed step -> do
      headHash' <- raise $ get @ObjectHash
      result    <- raise $ walkFrom seed step headHash'
      pureT result

    Replace oldId d -> do
      oldCd    <- raise $ readCommit (ObjectHash (unTickId oldId))
      wt'      <- raise $ get @WorkingTree
      parentWt <- raise $ loadWorkingTree
                    (case commitParents oldCd of { (p:_) -> p; [] -> ObjectHash (unTickId oldId) })
      eCheck   <- raise $ checkAppendOnly parentWt wt'
      case eCheck of
        Left err -> pureT (Left err)
        Right () -> do
          treeHash <- raise $ flushWorkingTree wt'
          newHash  <- raise $ writeCommit CommitData
            { commitParents = commitParents oldCd
            , commitTree    = treeHash
            , commitMessage = encodeTickData d
            }
          let newId = TickId (unObjectHash newHash)
          -- Cascade to other branches only — the current branch is being
          -- rebuilt by At's rewind and must not be touched here. Reads
          -- current heads via 'listBranches' (StoryStorage, overlay-aware)
          -- rather than raw git, so a branch already rewritten earlier in
          -- the same transaction is matched against its up-to-date head —
          -- see 'cascadeReplace's own comment for why that matters.
          current <- raise $ map (\b -> (storyRef (branchName b), ObjectHash (unTickId (branchHead b))))
                       <$> listBranches
          raise $ cascadeReplaceOtherBranches current setRef branch (ObjectHash (unTickId oldId)) newHash
          raise $ put @ObjectHash newHash
          raise $ setRef branch (Just newId)
          pureT $ Right newId

    -- 'moveTick' nests one 'At' inside another (rewind to the tick being
    -- moved, then — still inside that walk — rewind again to where it's
    -- going). Both share this one head slot, so the inner call must
    -- save/restore whatever the outer call left there when it's done,
    -- rather than assuming its own start position — otherwise the inner
    -- 'At's cleanup would wipe out the outer walk's still-in-progress
    -- position.
    At replay tid innerAction -> do
      outerHead <- raise $ get @ObjectHash
      eResult   <- runAtH replay tid outerHead (runTSimple innerAction)
      case eResult of
        Left err            -> pureT (Left err)
        Right (fa, mapping) -> do
          -- A replaying walk leaves local state at the rebuilt head —
          -- publish it once, here, rather than once per rewritten tick.
          -- A read-only walk never advances anything: restore local state
          -- to wherever it started and publish nothing.
          if replay
            then do
              finalHead <- raise $ get @ObjectHash
              raise $ setRef branch (Just (TickId (unObjectHash finalHead)))
            else raise $ put @ObjectHash outerHead
          return $ fmap (\a -> Right (a, mapping)) fa

    WithFS innerAction -> do
      headHash' <- raise $ get @ObjectHash
      outerWt   <- raise $ get @WorkingTree
      headWt    <- raise $ loadWorkingTree headHash'
      raise $ put headWt
      fa        <- runTSimple innerAction
      raise $ put outerWt
      return fa

    S.FileTicks path -> do
      headHash' <- raise $ get @ObjectHash
      ticks     <- raise $ walkFileTicks path headHash'
      pureT ticks
    ) (raiseUnder action)

-- ---------------------------------------------------------------------------
-- FileSystem interpreter (git-backed, shares WorkingTree state)
-- ---------------------------------------------------------------------------

-- | Interpret filesystem effects for a branch against the in-memory 'WorkingTree'.
--
--   The project type for all three effects is @BranchTag branch@, making each
--   branch's filesystem unambiguous on the effect stack.  The 'BranchTag branch'
--   runtime value is what 'GetFileSystem' returns; all actual IO goes through
--   'Git' and 'State WorkingTree', which are shared with 'runStoryBranchGit'.
runStoryFSGit
  :: forall branch r a
  .  Members '[Git, Fail, State WorkingTree] r
  => BranchName
  -> Sem ( FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
runStoryFSGit name = interpretFS . interpretFSRead . interpretFSWrite
  where
    interpretFS
      :: Members '[State WorkingTree] r'
      => Sem (FileSystem (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFS = interpret $ \case
      GetFileSystem ->
        return (BranchTag name)
      GetCwd ->
        return $ Right "/"
      ListFiles dir -> do
        wt <- get @WorkingTree
        return $ Right [ p | p <- Map.keys wt, isDirectChild dir p ]
      FileExists path -> do
        wt <- get @WorkingTree
        return $ Right (Map.member path wt)
      IsDirectory path -> do
        wt <- get @WorkingTree
        return $ Right $ case Map.lookup path wt of
          Just FSDir -> True
          _          -> False
      Glob _base _pat ->
        return $ Left "Branch FS: Glob not yet implemented"

    interpretFSRead
      :: Members '[Git, Fail, State WorkingTree] r'
      => Sem (FileSystemRead (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSRead = interpret $ \case
      ReadFile path -> do
        wt <- get @WorkingTree
        case Map.lookup path wt of
          Just (FSFile hash) -> fmap Right (readBlob hash)
          Just FSDir         -> return $ Left (path <> ": is a directory")
          Nothing            -> return $ Left (path <> ": not found")

    interpretFSWrite
      :: Members '[Git, Fail, State WorkingTree] r'
      => Sem (FileSystemWrite (BranchTag branch) : r') a'
      -> Sem r' a'
    interpretFSWrite = interpret $ \case
      WriteFile path content -> do
        hash <- writeBlob content
        let parents = ancestorDirs path
        modify @WorkingTree $ \wt ->
          foldr (\d m -> Map.insertWith keepExisting d FSDir m) wt parents
        modify @WorkingTree (Map.insert path (FSFile hash))
        return $ Right ()
      CreateDirectory _recursive path -> do
        modify @WorkingTree (Map.insertWith keepExisting path FSDir)
        return $ Right ()
      Remove recursive path -> do
        if recursive
          then modify @WorkingTree $ Map.filterWithKey
                 (\k _ -> k /= path && not (isUnderDir path k))
          else modify @WorkingTree (Map.delete path)
        return $ Right ()

    -- Keep the existing entry rather than overwriting (so a file is not
    -- silently replaced by FSDir when a parent dir is ensured).
    keepExisting :: FSNode -> FSNode -> FSNode
    keepExisting _ old = old

    -- True when @child@ is directly inside @dir@:
    --   "a/b" is a direct child of "a", but "a/b/c" is not.
    isDirectChild :: FilePath -> FilePath -> Bool
    isDirectChild "/"  p = length (splitDirectories p) == 1
    isDirectChild "."  p = length (splitDirectories p) == 1
    isDirectChild ""   p = length (splitDirectories p) == 1
    isDirectChild dir  p =
      let dirParts  = splitDirectories dir
          pathParts = splitDirectories p
      in take (length dirParts) pathParts == dirParts
         && length pathParts == length dirParts + 1

    isUnderDir :: FilePath -> FilePath -> Bool
    isUnderDir dir path =
      let dirParts  = splitDirectories dir
          pathParts = splitDirectories path
      in take (length dirParts) pathParts == dirParts
         && length pathParts > length dirParts

    ancestorDirs :: FilePath -> [FilePath]
    ancestorDirs path =
      let parts = splitDirectories path
          -- all prefixes except the full path itself
      in [ joinPath (take n parts) | n <- [1 .. length parts - 1] ]

-- ---------------------------------------------------------------------------
-- Combined interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'StoryBranch branch' and all three filesystem effects for a branch.
--
-- Takes a 'Branch' obtained from 'StoryStorage' — the storage layer is the
-- authority on which branches exist and are accessible.  Callers must go
-- through 'getBranch' or 'createBranch' before opening a branch here.
--
-- Introduces and eliminates 'State WorkingTree' internally — each branch gets
-- its own isolated working tree state, invisible to callers and to other branch
-- interpreters on the stack.
runBranchAndFS
  :: forall branch r a
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName
  -> Sem ( StoryBranch branch
         : FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
runBranchAndFS name action = do
  getBranch name >>= \case
    Nothing -> fail $ "branch not found: " <> T.unpack (unBranchName name)
    Just _  -> pure ()
  evalState emptyWorkingTree
    . runStoryFSGit @branch name
    . runStoryBranchGit @branch name
    . subsume_
    $ action


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check that every file in the new working tree is a pure append of the
--   corresponding file in the parent tree. New files and directories are fine;
--   deletions and modifications (content not prefixed by the parent's content)
--   are rejected with a descriptive error.
checkAppendOnly
  :: Members '[Git, Fail] r
  => WorkingTree -> WorkingTree -> Sem r (Either String ())
checkAppendOnly parent new = go (Map.toList new)
  where
    go []             = return (Right ())
    go ((_, FSDir) : rest) = go rest
    go ((path, FSFile newHash) : rest) =
      case Map.lookup path parent of
        Nothing           -> go rest
        Just FSDir        -> go rest
        Just (FSFile oldHash) -> do
          oldContent <- readBlob oldHash
          newContent <- readBlob newHash
          if oldContent `BS.isPrefixOf` newContent
            then go rest
            else return $ Left $ "Store: non-append modification of " <> path
                   <> " (old=" <> show (BS.length oldContent)
                   <> " new=" <> show (BS.length newContent)
                   <> " oldHash=" <> T.unpack (unObjectHash oldHash)
                   <> " newHash=" <> T.unpack (unObjectHash newHash)
                   <> ")"

-- | Apply the diff between @originalParentWt@ and @commitWt@ onto @newParentWt@.
--   For each file: compute bytes added beyond the original parent, append to new parent.
applyDiff
  :: Members '[Git, Fail] r
  => WorkingTree   -- ^ original parent tree (what the commit was based on)
  -> WorkingTree   -- ^ commit tree (what the commit produced)
  -> WorkingTree   -- ^ new parent tree (what we're rebasing onto)
  -> Sem r WorkingTree
applyDiff originalParentWt commitWt newParentWt =
  foldM applyFile newParentWt (Map.toList commitWt)
  where
    applyFile wt (path, FSDir) =
      return $ Map.insertWith keepExisting path FSDir wt
    applyFile wt (path, FSFile newHash) = do
      commitContent <- readBlob newHash
      originalContent <- case Map.lookup path originalParentWt of
        Just (FSFile h) -> readBlob h
        _               -> return BS.empty
      let suffix      = BS.drop (BS.length originalContent) commitContent
      baseContent <- case Map.lookup path wt of
        Just (FSFile h) -> readBlob h
        _               -> return BS.empty
      hash <- writeBlob (baseContent <> suffix)
      return $ Map.insert path (FSFile hash) wt
    keepExisting _ old = old

-- | Apply the suffix that commit @cd@ added to its parent onto @parentWt@.
--   For each file: compute bytes added beyond the commit's own parent's version,
--   then append those bytes to the corresponding file in @parentWt@.
applyCommitSuffix
  :: Members '[Git, Fail] r
  => WorkingTree   -- ^ tree of the new parent (what we're rebasing onto)
  -> CommitData    -- ^ the commit being replayed
  -> Sem r WorkingTree
applyCommitSuffix parentWt cd = do
  commitWt       <- readTreeRecursive "" (commitTree cd)
  commitParentWt <- case commitParents cd of
    []    -> return emptyWorkingTree
    (p:_) -> loadWorkingTree p
  foldM (applyFile commitParentWt) parentWt (Map.toList commitWt)
  where
    applyFile _commitParentWt wt (path, FSDir) =
      return $ Map.insertWith keepExisting path FSDir wt
    applyFile commitParentWt wt (path, FSFile newHash) = do
      newContent <- readBlob newHash
      oldContent <- case Map.lookup path commitParentWt of
        Just (FSFile h) -> readBlob h
        _               -> return BS.empty
      let suffix   = BS.drop (BS.length oldContent) newContent
      baseContent <- case Map.lookup path wt of
        Just (FSFile h) -> readBlob h
        _               -> return BS.empty
      let combined = baseContent <> suffix
      hash <- writeBlob combined
      return $ Map.insert path (FSFile hash) wt
    keepExisting _ old = old

-- | Recursive implementation of At, inside the interpretH tactic context.
--
-- Base case: @current == tid@ — load the target working tree, run the action,
-- return the result and an empty mapping.
--
-- Recursive case: walk back to @parent@ (validating @tid@ is actually in the
-- branch's history, and moving the local head there so 'WithFS' resolves to
-- the right historical snapshot), recurse, then either (@replay = True@)
-- reapply this tick's diff onto whatever the inner action left and write a
-- new commit, or (@replay = False@) just pass the result through untouched
-- — no write, no mapping entry, local head stays at @parent@ until the
-- top-level caller (the 'At' case in 'runStoryBranchGit') restores it once
-- the whole walk unwinds. Only ever touches the private local 'State
-- ObjectHash' — no ref, git or 'StoryStorage', is written until that
-- top-level caller publishes the final result once, after the whole walk
-- completes.
runAtH
  :: forall branch f a rInitial r
  .  Members '[Git, Fail, State ObjectHash] r
  => Bool
  -> TickId
  -> ObjectHash
  -> Sem (WithTactics (StoryBranch branch) f (Sem rInitial) r) (f a)
  -> Sem (WithTactics (StoryBranch branch) f (Sem rInitial) r)
         (Either String (f a, [(TickId, TickId)]))
runAtH replay tid current action
  | TickId (unObjectHash current) == tid = do
      fa <- action
      return $ Right (fa, [])
  | otherwise = do
      mResult <- raise $ do
        cd <- readCommit current
        case commitParents cd of
          [] -> return $ Left $
            "At: tick " <> T.unpack (unTickId tid) <> " not found in branch history"
          (parent : _) -> do
            parentWt <- loadWorkingTree parent
            commitWt <- loadWorkingTree current
            put parent
            return $ Right (parent, parentWt, commitWt, cd)
      case mResult of
        Left err -> return (Left err)
        Right (parent, parentWt, commitWt, cd) -> do
          eInner <- runAtH replay tid parent action
          case eInner of
            Left err -> return (Left err)
            Right (fa, innerMapping)
              | not replay -> return $ Right (fa, innerMapping)
              | otherwise  -> do
                  newHash <- raise $ do
                    newParent   <- get
                    newParentWt <- loadWorkingTree newParent
                    newWt       <- applyDiff parentWt commitWt newParentWt
                    treeHash    <- flushWorkingTree newWt
                    newHash     <- writeCommit cd
                      { commitParents = newParent : drop 1 (commitParents cd)
                      , commitTree    = treeHash
                      }
                    put newHash
                    return newHash
                  let oldId = TickId (unObjectHash current)
                      newId = TickId (unObjectHash newHash)
                  return $ Right (fa, innerMapping <> [(oldId, newId)])

-- | Walk the branch from HEAD and collect all ticks belonging to @path@.
--
-- First builds the atom set by walking the chain with 'walkFrom', diffing blobs
-- at each step. Then calls 'walkFrom' again to expand the member set: any tick
-- whose extra-parent refs intersect the current member set is included, repeated
-- until stable (two passes suffices for current tick types).
-- Returns oldest-first.
walkFileTicks
  :: Members '[Git, Fail] r
  => FilePath
  -> ObjectHash
  -> Sem r [FileTick]
walkFileTicks path headHash = do
  raw      <- collectChain headHash []  -- oldest-first (prepend walks root to front)
  allTicks <- diffChain Nothing raw    -- oldest-first, with blob suffix
  let fileHint  = T.pack path
      atomIds   = [ ftTickId ft | ft <- allTicks, ftContent ft /= Nothing ]
      memberIds = expandRefs atomIds allTicks
      -- Also include ticks with a matching file field (e.g. prompts that don't ref atoms)
      fileHinted = [ ftTickId ft | ft <- allTicks
                                 , ftContent ft == Nothing
                                 , lookup "file" (ftFields ft) == Just fileHint ]
  return [ ft | ft <- allTicks, ftTickId ft `elem` memberIds || ftTickId ft `elem` fileHinted ]
  where
    collectChain :: Members '[Git, Fail] r => ObjectHash -> [(ObjectHash, CommitData)] -> Sem r [(ObjectHash, CommitData)]
    collectChain hash acc = do
      cd <- readCommit hash
      case commitParents cd of
        []      -> return ((hash, cd) : acc)
        (p : _) -> collectChain p ((hash, cd) : acc)

    diffChain :: Members '[Git, Fail] r => Maybe BS.ByteString -> [(ObjectHash, CommitData)] -> Sem r [FileTick]
    diffChain _ [] = return []
    diffChain prev ((hash, cd) : rest) = do
      mBH     <- lookupPath (commitTree cd) path
      curBlob <- case mBH of { Nothing -> return Nothing; Just h -> Just <$> readBlob h }
      let mSuffix = curBlob >>= \cur ->
            let old = maybe BS.empty id prev
            in if cur == old then Nothing
               else Just (TE.decodeUtf8With TEE.lenientDecode (BS.drop (BS.length old) cur))
      tl <- diffChain curBlob rest
      return (toFileTick hash cd mSuffix : tl)

    expandRefs :: [Text] -> [FileTick] -> [Text]
    expandRefs members ticks =
      let step ms = ms ++ [ ftTickId ft
                           | ft <- ticks
                           , ftTickId ft `notElem` ms
                           , any (`elem` ms) (ftRefs ft) ]
      in step (step members)

    toFileTick :: ObjectHash -> CommitData -> Maybe Text -> FileTick
    toFileTick hash cd mSuffix =
      let tick = commitToTick hash cd
          td   = tickData tick
          kind = case tickTypeOf tick of
                   Just t  -> t
                   Nothing -> if mSuffix /= Nothing then "atom" else "unknown"
          msg  = stripTypeTag kind (tickMessage td)
      in FileTick
        { ftTickId  = unObjectHash hash
        , ftKind    = kind
        , ftRefs    = map unObjectHash (drop 1 (commitParents cd))
        , ftFields  = tickFields td
        , ftMessage = msg
        , ftContent = mSuffix
        , ftParent  = case commitParents cd of { [] -> Nothing; (p:_) -> Just (unObjectHash p) }
        }

    stripTypeTag :: Text -> Text -> Text
    stripTypeTag kind msg =
      let prefix = "type:" <> kind <> "\n"
      in if prefix `T.isPrefixOf` msg then T.drop (T.length prefix) msg else msg

walkFrom
  :: Members '[Git, Fail] r
  => b
  -> (b -> Tick -> (b, Maybe TickId))
  -> ObjectHash
  -> Sem r b
walkFrom acc step hash = do
  cd <- readCommit hash
  let tick = commitToTick hash cd
      (acc', next) = step acc tick
  case next of
    Nothing          -> return acc'
    Just (TickId h)  -> walkFrom acc' step (ObjectHash h)

-- | Rewrite all story branch commits that reference @old@ as a parent,
--   substituting @new@, then cascade until no more referencing commits remain.
--   Branch refs are updated via @applyRef@ rather than touched directly, so
--   the caller (a 'StoryStorage' interpreter) decides whether that lands in
--   git immediately or is buffered as part of a transaction.
--
--   Takes each branch's current head as an explicit @pairs@ argument rather
--   than reading raw git refs itself — a caller running inside a buffered
--   transaction (see 'Storyteller.Git.withStorage') must pass heads
--   overlaid with its own pending writes so far, not raw git, or a branch
--   already correctly rewritten earlier in the same transaction would
--   still read as pointing at its stale, pre-rewrite head here (nothing
--   lands in real git until the transaction replays) and get wrongly
--   matched and rebuilt a second time from that stale ancestry.
--
--   Only commit parent links are rewritten — trees and blobs are not tick
--   references and are left untouched.
cascadeReplace
  :: Members '[Git, Fail] r
  => [(RefName, ObjectHash)]  -- ^ each branch's current head
  -> (BranchName -> Maybe TickId -> Sem r ())
  -> ObjectHash  -- ^ old commit hash (now superseded)
  -> ObjectHash  -- ^ new commit hash (the replacement)
  -> Sem r ()
cascadeReplace pairs applyRef old new =
  mapM_ (rewriteRef applyRef old new) pairs

-- | Like 'cascadeReplace' but skips the given branch — used by 'Replace'
--   inside 'At' where the current branch is being rebuilt by the rewind.
cascadeReplaceOtherBranches
  :: Members '[Git, Fail] r
  => [(RefName, ObjectHash)]  -- ^ each branch's current head
  -> (BranchName -> Maybe TickId -> Sem r ())
  -> BranchName
  -> ObjectHash
  -> ObjectHash
  -> Sem r ()
cascadeReplaceOtherBranches pairs applyRef skipBranch old new = do
  let others = filter (\(ref, _) -> ref /= storyRef skipBranch) pairs
  mapM_ (rewriteRef applyRef old new) others

rewriteRef
  :: Members '[Git, Fail] r
  => (BranchName -> Maybe TickId -> Sem r ())
  -> ObjectHash
  -> ObjectHash
  -> (RefName, ObjectHash)
  -> Sem r ()
rewriteRef applyRef old new (ref, headHash) = do
  newHead <- rewriteChain old new headHash
  when (newHead /= headHash) $
    case refBranchName ref of
      Just name -> applyRef name (Just (TickId (unObjectHash newHead)))
      Nothing   -> return ()

-- | Walk a commit chain rewriting any commit that has @old@ as a parent.
--   Returns the (possibly new) head hash. Works bottom-up by recursing to
--   parents first, so a single rewrite propagates upward automatically.
rewriteChain
  :: Members '[Git, Fail] r
  => ObjectHash
  -> ObjectHash
  -> ObjectHash
  -> Sem r ObjectHash
rewriteChain old new hash
  | hash == old = return new
  | otherwise   = do
      cd         <- readCommit hash
      newParents <- mapM (rewriteChain old new) (commitParents cd)
      if newParents == commitParents cd
        then return hash
        else writeCommit cd { commitParents = newParents }
