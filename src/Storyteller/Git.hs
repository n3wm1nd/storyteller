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
  , runStoryBranchGit
  , runStoryFSGit

    -- * Working tree (in-memory filesystem)
  , FSNode(..)
  , WorkingTree
  , emptyWorkingTree
  , loadWorkingTree

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
import Polysemy.State (State, get, put, modify, evalState)
import Polysemy.Internal (raiseUnder3)

import Runix.Git
import Runix.FileSystem
  ( FileSystem(..), FileSystemRead(..), FileSystemWrite(..) )

import Storyteller.Types
import Storyteller.Storage hiding (get, drop, Get)
import qualified Storyteller.Storage as S
import Storyteller.Types (TickDraft(..))

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

storyRef :: BranchName -> RefName
storyRef (BranchName n) = RefName ("refs/heads/story/" <> n)

emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- Message encoding / decoding
-- ---------------------------------------------------------------------------

encodeMessage :: [TickId] -> Text -> Text
encodeMessage [] msg  = msg
encodeMessage refs msg =
  "refs: " <> T.unwords (map unTickId refs) <> "\n" <> msg

decodeMessage :: Text -> ([TickId], Text)
decodeMessage raw =
  case T.lines raw of
    (l:rest) | "refs: " `T.isPrefixOf` l ->
      let refs = map TickId $ T.words (T.drop 6 l)
      in (refs, T.intercalate "\n" rest)
    _ -> ([], raw)

-- ---------------------------------------------------------------------------
-- Conversion between git and tick vocabulary
-- ---------------------------------------------------------------------------

commitToTick :: ObjectHash -> CommitData -> Tick
commitToTick hash cd =
  let (refs, msg) = decodeMessage (commitMessage cd)
  in Tick
     { tickId      = TickId (unObjectHash hash)
     , tickParent  = TickId . unObjectHash <$> listToMaybe (commitParents cd)
     , tickRefs    = refs
     , tickMessage = msg
     }
  where
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

-- ---------------------------------------------------------------------------
-- StoryStorage interpreter
-- ---------------------------------------------------------------------------

runStoryStorageGit
  :: Members '[Git, Fail] r
  => Sem (StoryStorage : r) a
  -> Sem r a
runStoryStorageGit = interpret $ \case
  CreateBranch name -> do
    let ref = storyRef name
    existing <- resolveRef ref
    case existing of
      Just _ -> fail $ "branch already exists: " <> T.unpack (unBranchName name)
      Nothing -> do
        rootHash <- writeCommit CommitData
          { commitParents = []
          , commitTree    = emptyTree
          , commitMessage = "root"
          }
        createRef ref rootHash
        return Branch { branchName = name, branchHead = TickId (unObjectHash rootHash) }

  DeleteBranch name ->
    deleteRef (storyRef name)

  ListBranches -> do
    pairs <- listRefs "refs/heads/story/"
    mapM resolveToHead pairs
    where
      resolveToHead (RefName ref, hash) =
        let name = BranchName $ T.drop (T.length "refs/heads/story/") ref
        in return Branch { branchName = name, branchHead = TickId (unObjectHash hash) }

  UpdateReferences mapping ->
    mapM_ (\(o, n) -> cascadeReplace (ObjectHash (unTickId o)) (ObjectHash (unTickId n))) mapping

-- ---------------------------------------------------------------------------
-- StoryBranch interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'StoryBranch branch' against git.
--   'Store' flushes the shared 'WorkingTree' into a real git tree object.
--   'Drop' and 'At' restore the working tree from the target commit's tree.
runStoryBranchGit
  :: forall branch r a
  .  Members '[Git, Fail, State WorkingTree] r
  => BranchName
  -> Sem (StoryBranch branch : r) a
  -> Sem r a
runStoryBranchGit branch action = do
  -- Initialise working tree from head commit if the branch already exists.
  -- If it doesn't exist yet, start with an empty tree — createBranch will
  -- be called before any Store, establishing the root commit.
  mHead <- resolveRef (storyRef branch)
  case mHead of
    Nothing -> put emptyWorkingTree
    Just h  -> loadWorkingTree h >>= put
  interpretH (\case
    Store d -> do
      headHash' <- raise $ resolveHead branch
      wt'       <- raise $ get @WorkingTree
      parentWt  <- raise $ loadWorkingTree headHash'
      eCheck    <- raise $ checkAppendOnly parentWt wt'
      case eCheck of
        Left err -> pureT (Left err)
        Right () -> do
          treeHash <- raise $ flushWorkingTree wt'
          newHash  <- raise $ writeCommit CommitData
            { commitParents = [headHash']
            , commitTree    = treeHash
            , commitMessage = encodeMessage (draftRefs d) (draftMessage d)
            }
          raise $ updateRef (storyRef branch) newHash
          pureT $ Right (TickId (unObjectHash newHash))

    Drop -> do
      headHash' <- raise $ resolveHead branch
      cd        <- raise $ readCommit headHash'
      case commitParents cd of
        []      -> pureT ()
        (p : _) -> raise (updateRef (storyRef branch) p) >> pureT ()

    S.Get -> do
      headHash' <- raise $ resolveHead branch
      cd        <- raise $ readCommit headHash'
      pureT $ commitToTick headHash' cd

    S.Reset -> do
      headHash' <- raise $ resolveHead branch
      wt'       <- raise $ loadWorkingTree headHash'
      raise $ put wt'
      pureT ()

    Follow seed step -> do
      headHash' <- raise $ resolveHead branch
      result    <- raise $ walkFrom seed step headHash'
      pureT result

    Replace oldId d -> do
      wt'      <- raise $ get @WorkingTree
      oldCd    <- raise $ readCommit (ObjectHash (unTickId oldId))
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
            , commitMessage = encodeMessage (draftRefs d) (draftMessage d)
            }
          let newId = TickId (unObjectHash newHash)
          -- Cascade to other branches only — the current branch is being
          -- rebuilt by At's rewind and must not be touched here.
          raise $ cascadeReplaceOtherBranches branch (ObjectHash (unTickId oldId)) newHash
          raise $ updateRef (storyRef branch) newHash
          pureT $ Right newId

    At tid innerAction -> do
      headHash' <- raise $ resolveHead branch
      eResult   <- runAtH branch tid headHash' (runTSimple innerAction)
      case eResult of
        Left err            -> pureT (Left err)
        Right (fa, mapping) -> return $ fmap (\a -> Right (a, mapping)) fa

    WithFS innerAction -> do
      headHash' <- raise $ resolveHead branch
      outerWt   <- raise $ get @WorkingTree
      headWt    <- raise $ loadWorkingTree headHash'
      raise $ put headWt
      fa        <- runTSimple innerAction
      raise $ put outerWt
      return fa
    ) action

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

resolveHead :: Members '[Git, Fail] r => BranchName -> Sem r ObjectHash
resolveHead name = do
  mhash <- resolveRef (storyRef name)
  case mhash of
    Just h  -> return h
    Nothing -> fail $ "branch not found: " <> T.unpack (unBranchName name)

-- | Recursive implementation of At, inside the interpretH tactic context.
--
-- Base case: @current == tid@ — load the target working tree, run the action,
-- return the result and an empty mapping.
--
-- Recursive case: capture this tick's diff, drop to parent, recurse, then
-- reapply the diff onto whatever the inner action left, write a new commit.
runAtH
  :: forall branch f a rInitial r
  .  Members '[Git, Fail, State WorkingTree] r
  => BranchName
  -> TickId
  -> ObjectHash
  -> Sem (WithTactics (StoryBranch branch) f (Sem rInitial) r) (f a)
  -> Sem (WithTactics (StoryBranch branch) f (Sem rInitial) r)
         (Either String (f a, [(TickId, TickId)]))
runAtH branch tid current action
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
            updateRef (storyRef branch) parent
            return $ Right (parent, parentWt, commitWt, cd)
      case mResult of
        Left err -> return (Left err)
        Right (parent, parentWt, commitWt, cd) -> do
          eInner <- runAtH branch tid parent action
          case eInner of
            Left err -> return (Left err)
            Right (fa, innerMapping) -> do
              newHash <- raise $ do
                newParent   <- resolveHead branch
                newParentWt <- loadWorkingTree newParent
                newWt       <- applyDiff parentWt commitWt newParentWt
                treeHash    <- flushWorkingTree newWt
                newHash     <- writeCommit cd
                  { commitParents = newParent : drop 1 (commitParents cd)
                  , commitTree    = treeHash
                  }
                updateRef (storyRef branch) newHash
                return newHash
              let oldId = TickId (unObjectHash current)
                  newId = TickId (unObjectHash newHash)
              return $ Right (fa, innerMapping <> [(oldId, newId)])

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
--   Branch refs are updated in place.
--
--   Only commit parent links are rewritten — trees and blobs are not tick
--   references and are left untouched.
cascadeReplace
  :: Members '[Git, Fail] r
  => ObjectHash  -- ^ old commit hash (now superseded)
  -> ObjectHash  -- ^ new commit hash (the replacement)
  -> Sem r ()
cascadeReplace old new = do
  pairs <- listRefs "refs/heads/story/"
  mapM_ (rewriteRef old new) pairs

-- | Like 'cascadeReplace' but skips the given branch — used by 'Replace'
--   inside 'At' where the current branch is being rebuilt by the rewind.
cascadeReplaceOtherBranches
  :: Members '[Git, Fail] r
  => BranchName
  -> ObjectHash
  -> ObjectHash
  -> Sem r ()
cascadeReplaceOtherBranches skipBranch old new = do
  pairs <- listRefs "refs/heads/story/"
  let others = filter (\(ref, _) -> ref /= storyRef skipBranch) pairs
  mapM_ (rewriteRef old new) others

rewriteRef
  :: Members '[Git, Fail] r
  => ObjectHash
  -> ObjectHash
  -> (RefName, ObjectHash)
  -> Sem r ()
rewriteRef old new (ref, headHash) = do
  newHead <- rewriteChain old new headHash
  when (newHead /= headHash) $ updateRef ref newHead

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
