{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | A reusable, composable storage monad for append-only tick chains.
--
-- This module is the direct implementation of @PLAN-storage-monad.md@: a
-- replacement for the higher-order 'At'\/'WithFS' Polysemy effects
-- (@Storyteller.Core.Storage@\/@Storyteller.Core.Git@), which pay Polysemy's
-- @interpretH@ tax on every read and write of a rebase because their GADT
-- constructors carry an arbitrary continuation (@m a@) that has to be
-- reified to be interpreted. Everything in this module is instead ordinary
-- first-class monadic code — a rebase's recursive walk is just recursive
-- function calls in 'StorageT', not a nested effect interpretation — so
-- there is nothing to reify and no per-level tax to pay.
--
-- 'StorageT' is deliberately not coupled to Polysemy or to git specifically:
-- it is a plain monad transformer over any base monad @m@ that can read and
-- write content-addressed git objects ('MonadGit'). A caller wires it into
-- Polysemy with a single first-order effect boundary (see
-- @Storyteller.Core.Git@'s @GitBranchOp@) that hands the whole computation
-- to 'runStorageT' in one dispatch, however deep the rebase it performs.
--
-- The vocabulary here is tick\/tree level (ticks, the working tree, the
-- append-only invariant), not raw git — 'MonadGit' is the one place git
-- vocabulary (commits, trees, blobs) is still visible, and it is the
-- pluggable seam: anything able to read and write content-addressed objects
-- the same way git does can supply an instance and reuse everything above
-- it unchanged.
module Storyteller.Core.StorageMonad
  ( -- * The pluggable object-store primitive
    MonadGit(..)
  , StorageM

    -- * Working tree
  , FSNode(..)
  , WorkingTree
  , emptyWorkingTree
  , loadWorkingTree
  , loadTree
  , flushWorkingTree

    -- * The monad
  , StorageT
  , runStorageT
  , evalStorageT

    -- * Tick chain operations
  , headTick
  , headTickId
  , headTree
  , getTick
  , storeTick
  , dropTick
  , resetTree
  , syncTo
  , followChain
  , replaceTick
  , at
  , atChecked
  , readAtS
  , withFS
  , fileTicksOf
  , store
  , storeAs
  , replace

    -- * File tick projection
  , FileTick(..)

    -- * Working-tree file access
  , readFileS
  , writeFileS
  , appendFileS
  , listFilesS
  , listAllFilesS
  , fileExistsS
  , isDirectoryS
  , removeS
  , createDirectoryS

    -- * Message encoding (tick vocabulary <-> object content)
  , encodeTickData
  , decodeTickData
  , commitToTick

    -- * Append-only invariant
  , checkAppendOnly
  , applyDiff

    -- * Editing operations — the mechanical consequences of the
    -- append-only invariant (see DATA-MODEL.md); these used to live in
    -- Storyteller.Core.Edit\/Storyteller.Core.Append as Polysemy-effect
    -- code. They are ordinary tick-chain algorithms, not story-specific
    -- policy, so they belong with the monad they're built from.
  , TDraft(..)
  , popTick
  , pushTick
  , deleteTick
  , editAtom
  , moveTick
  , mergeAtoms
  , splitTick
  , checkMoveOrder
  , chainPositions
  , append
  , appendAtom
  , storeAtom
  , unstoreAtom
  , rewriteAtom
  , commitWorkingTree
  , commitFiles
  ) where

import Control.Monad (foldM, filterM)
import Control.Monad.State.Strict
  (StateT(..), MonadState, MonadTrans, gets, modify, lift, runStateT)
import qualified Data.ByteString as BS
import Data.Array (Array, listArray, (!))
import Data.List (findIndex, zip5)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (splitDirectories, joinPath)

import Runix.Git
  ( ObjectHash(..), CommitData(..), TreeEntry(..), GitObject(..) )

import Storyteller.Core.Types hiding (draft)
import qualified Storyteller.Core.Atom as Atom
import Storyteller.Core.Created (Created(..))

-- ---------------------------------------------------------------------------
-- MonadGit: the pluggable object-store primitive
-- ---------------------------------------------------------------------------

-- | Everything the tick\/tree layer needs from a content-addressed object
--   store — the read\/write half of "Runix.Git", with no notion of refs or
--   branches (ref resolution and publication are the caller's job: they
--   happen before 'StorageT' is entered and after it returns, never inside
--   it — see @Storyteller.Core.Git@'s @GitBranchOp@).
--
--   Any monad able to read and write commits and objects the way git does
--   can instantiate this and reuse every tick\/tree operation below for
--   free; the one instance this codebase needs
--   (@Members '[Runix.Git.Git, Polysemy.Fail.Fail] r => MonadGit (Sem r)@)
--   lives in @Storyteller.Core.Git@, alongside git-specific concerns (ref
--   naming, the Polysemy effect boundary) that don't belong in a
--   storage-agnostic module.
class Monad m => MonadGit m where
  gitReadCommit  :: ObjectHash -> m CommitData
  gitWriteCommit :: CommitData -> m ObjectHash
  gitReadObject  :: ObjectHash -> m GitObject
  gitWriteObject :: GitObject  -> m ObjectHash

-- | Shorthand for the constraints every tick\/tree operation needs: a
--   pluggable object store, plus the ability to fail (append-only
--   violations, "tick not found in history", ...).
type StorageM m = (MonadGit m, MonadFail m)

readBlobM :: StorageM m => ObjectHash -> m BS.ByteString
readBlobM h = gitReadObject h >>= \case
  BlobObject bs -> return bs
  TreeObject _  -> fail $ "readBlob: hash is a tree: " <> T.unpack (unObjectHash h)

writeBlobM :: MonadGit m => BS.ByteString -> m ObjectHash
writeBlobM = gitWriteObject . BlobObject

readTreeM :: StorageM m => ObjectHash -> m [TreeEntry]
readTreeM h = gitReadObject h >>= \case
  TreeObject es -> return es
  BlobObject _  -> fail $ "readTree: hash is a blob: " <> T.unpack (unObjectHash h)

writeTreeM :: MonadGit m => [TreeEntry] -> m ObjectHash
writeTreeM = gitWriteObject . TreeObject

emptyTree :: ObjectHash
emptyTree = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- Working tree (in-memory filesystem) — unchanged from Storyteller.Core.Git
-- ---------------------------------------------------------------------------

-- | A node in the in-memory working tree. See @Storyteller.Core.Git@'s
--   original doc: 'FSFile' carries only the git blob hash; 'FSDir'
--   represents an explicit, possibly-empty directory.
data FSNode
  = FSFile !ObjectHash
  | FSDir
  deriving (Show, Eq)

-- | The complete in-memory filesystem, keyed by path.
type WorkingTree = Map FilePath FSNode

emptyWorkingTree :: WorkingTree
emptyWorkingTree = Map.empty

-- | Reconstruct a 'WorkingTree' from a git commit's tree object.
loadWorkingTree :: StorageM m => ObjectHash -> m WorkingTree
loadWorkingTree commitHash = do
  cd <- gitReadCommit commitHash
  readTreeRecursive "" (commitTree cd)

-- | Reconstruct a 'WorkingTree' directly from a git tree hash (not a commit).
loadTree :: StorageM m => ObjectHash -> m WorkingTree
loadTree = readTreeRecursive ""

readTreeRecursive :: StorageM m => FilePath -> ObjectHash -> m WorkingTree
readTreeRecursive prefix treeHash = do
  entries <- readTreeM treeHash
  fmap Map.unions $ mapM (readEntry prefix) entries
  where
    readEntry pfx (BlobEntry name hash') = do
      let path = if null pfx then name else pfx <> "/" <> name
      return $ Map.singleton path (FSFile hash')
    readEntry pfx (SubTree name hash') = do
      let path = if null pfx then name else pfx <> "/" <> name
      sub <- readTreeRecursive path hash'
      return $ Map.insert path FSDir sub

-- | Write the 'WorkingTree' to git, returning the root tree hash.
flushWorkingTree :: StorageM m => WorkingTree -> m ObjectHash
flushWorkingTree wt
  | Map.null wt = return emptyTree
  | otherwise   = buildTree "" wt

buildTree :: StorageM m => FilePath -> WorkingTree -> m ObjectHash
buildTree prefix wt = do
  let children = directChildren prefix wt
  entries <- mapM (\name -> toEntry prefix name wt) children
  writeTreeM entries

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

toEntry :: StorageM m => FilePath -> FilePath -> WorkingTree -> m TreeEntry
toEntry prefix name wt =
  let path = if null prefix then name else prefix <> "/" <> name
  in case Map.lookup path wt of
    Just (FSFile hash') -> return (BlobEntry name hash')
    _ -> do
      hash' <- buildTree path wt
      return (SubTree name hash')

-- ---------------------------------------------------------------------------
-- Message encoding / decoding — unchanged from Storyteller.Core.Git
-- ---------------------------------------------------------------------------

encodeTickData :: TickData -> Text
encodeTickData td =
  let fieldLines = map (\(k, v) -> k <> ":" <> v) (tickFields td)
      body       = tickMessage td
  in if null fieldLines
       then body
       else T.intercalate "\n" fieldLines <> "\n\n" <> body

decodeTickData :: Text -> TickData
decodeTickData raw =
  let (headers, remainder) = break T.null (T.lines raw)
      fields = [ (k, v)
               | l <- headers
               , let (k, rest) = T.breakOn ":" l
               , not (T.null rest)
               , let v = T.drop 1 rest ]
      msg = case remainder of
        []      -> raw
        (_ : _) -> T.drop (headerByteLen headers) raw
  in TickData { tickRefs = [], tickFields = fields, tickMessage = msg }
  where
    headerByteLen hs = sum (map ((+ 1) . T.length) hs) + 1

commitToTick :: ObjectHash -> CommitData -> Tick
commitToTick hash' cd =
  let td      = decodeTickData (commitMessage cd)
      parents = commitParents cd
      pos = TickPos
              { posId     = TickId (unObjectHash hash')
              , posParent = TickId . unObjectHash <$> listToMaybe parents
              , posRefs   = map (TickId . unObjectHash) (drop 1 parents)
              }
  in Tick { tickPos = pos, tickData = td { tickRefs = posRefs pos } }
  where
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

-- ---------------------------------------------------------------------------
-- The monad
-- ---------------------------------------------------------------------------

-- | State threaded through a branch scope: the current tick's object hash
--   (\"HEAD\") and the ambient working tree (committed content plus
--   whatever has been written but not yet 'storeTick'd).
type ScopeState = (ObjectHash, WorkingTree)

-- | The storage monad: ordinary 'StateT' over any 'MonadGit' base monad.
--   Nothing here is a Polysemy effect — 'at' below recurses via plain
--   function calls, so a rebase N ticks deep costs N ordinary monadic
--   binds, not N nested effect interpretations.
newtype StorageT m a = StorageT (StateT ScopeState m a)
  deriving (Functor, Applicative, Monad, MonadState ScopeState)

instance MonadTrans StorageT where
  lift = StorageT . lift

instance MonadFail m => MonadFail (StorageT m) where
  fail = StorageT . lift . fail

-- | Run a 'StorageT' computation seeded at the given head and working tree,
--   returning the result together with the (possibly advanced) final state.
runStorageT :: ObjectHash -> WorkingTree -> StorageT m a -> m (a, ScopeState)
runStorageT h wt (StorageT s) = runStateT s (h, wt)

-- | Like 'runStorageT', discarding the final state.
evalStorageT :: Monad m => ObjectHash -> WorkingTree -> StorageT m a -> m a
evalStorageT h wt = fmap fst . runStorageT h wt

liftG :: Monad m => m a -> StorageT m a
liftG = lift

-- ---------------------------------------------------------------------------
-- Tick chain operations
-- ---------------------------------------------------------------------------

headTickId :: Monad m => StorageT m ObjectHash
headTickId = gets fst

getAmbientTree :: Monad m => StorageT m WorkingTree
getAmbientTree = gets snd

putHead :: Monad m => ObjectHash -> StorageT m ()
putHead h = modify (\(_, wt) -> (h, wt))

putAmbientTree :: Monad m => WorkingTree -> StorageT m ()
putAmbientTree wt = modify (\(h, _) -> (h, wt))

-- | The tick id currently at head.
headTick :: Monad m => StorageT m TickId
headTick = TickId . unObjectHash <$> headTickId

-- | The working tree as it currently stands (committed content plus any
--   pending, not-yet-'storeTick'd writes).
headTree :: Monad m => StorageT m WorkingTree
headTree = getAmbientTree

-- | Read the tick at head.
getTick :: StorageM m => StorageT m Tick
getTick = do
  h  <- headTickId
  cd <- liftG (gitReadCommit h)
  return (commitToTick h cd)

-- | Save the current ambient working tree as a new tick at head. Fails if
--   any file violates the append-only invariant.
storeTick :: StorageM m => TickData -> StorageT m (Either String TickId)
storeTick td = do
  h        <- headTickId
  wt       <- getAmbientTree
  parentWt <- liftG (loadWorkingTree h)
  eCheck   <- liftG (checkAppendOnly td parentWt wt)
  case eCheck of
    Left err -> return (Left err)
    Right () -> do
      treeHash <- liftG (flushWorkingTree wt)
      newHash  <- liftG $ gitWriteCommit CommitData
        { commitParents = h : map (ObjectHash . unTickId) (tickRefs td)
        , commitTree    = treeHash
        , commitMessage = encodeTickData td
        }
      putHead newHash
      return (Right (TickId (unObjectHash newHash)))

-- | Rewind the tick pointer to the previous tick. Working tree is
--   untouched. Dropping the root tick is a no-op.
dropTick :: StorageM m => StorageT m ()
dropTick = do
  h  <- headTickId
  cd <- liftG (gitReadCommit h)
  case commitParents cd of
    []      -> return ()
    (p : _) -> putHead p

-- | Discard pending working-tree changes, restoring the head tick's state.
resetTree :: StorageM m => StorageT m ()
resetTree = do
  h  <- headTickId
  wt <- liftG (loadWorkingTree h)
  putAmbientTree wt

-- | Move the scope to a known tick hash (e.g. after some other operation
--   published a new head for this branch) and reload the working tree to
--   match. Branch-agnostic by design: resolving *which* hash to sync to is
--   the caller's job (via whatever ref-lookup mechanism it uses).
syncTo :: StorageM m => ObjectHash -> StorageT m ()
syncTo h = do
  putHead h
  wt <- liftG (loadWorkingTree h)
  putAmbientTree wt

-- | Walk the chain from head backwards.
followChain :: StorageM m => b -> (b -> Tick -> (b, Maybe TickId)) -> StorageT m b
followChain seed step = do
  h <- headTickId
  liftG (walkFrom seed step h)

walkFrom :: StorageM m => b -> (b -> Tick -> (b, Maybe TickId)) -> ObjectHash -> m b
walkFrom acc step hash' = do
  cd <- gitReadCommit hash'
  let tick = commitToTick hash' cd
      (acc', next) = step acc tick
  case next of
    Nothing         -> return acc'
    Just (TickId h) -> walkFrom acc' step (ObjectHash h)

-- | Replace the given tick with the current ambient working tree, in
--   place: the new tick takes the old one's exact position (same parents),
--   and head advances to it. Fails if the append-only invariant is
--   violated. Unlike the old @Replace@ handler, this never cascades other
--   branches' refs — that is a cross-branch, multi-branch concern that
--   belongs to whatever sits above a single branch's scope (mirroring
--   @Storyteller.Core.Storage@'s own separation between @StoryBranch@ and
--   @StoryStorage@).
replaceTick :: StorageM m => TickId -> TickData -> StorageT m (Either String TickId)
replaceTick oldId td = do
  let oldHash = ObjectHash (unTickId oldId)
  oldCd    <- liftG (gitReadCommit oldHash)
  wt       <- getAmbientTree
  parentWt <- liftG (loadWorkingTree (case commitParents oldCd of { (p:_) -> p; [] -> oldHash }))
  eCheck   <- liftG (checkAppendOnly td parentWt wt)
  case eCheck of
    Left err -> return (Left err)
    Right () -> do
      treeHash <- liftG (flushWorkingTree wt)
      newHash  <- liftG $ gitWriteCommit CommitData
        { commitParents = commitParents oldCd
        , commitTree    = treeHash
        , commitMessage = encodeTickData td
        }
      putHead newHash
      return (Right (TickId (unObjectHash newHash)))

-- | Move to @tid@'s position, run @action@ there, then either replay
--   everything after it back onto whatever @action@ produced
--   (@replay = True@), or restore the scope to exactly where it started
--   (@replay = False@). Returns the inner result and the old->new id
--   mapping for every rewritten tick (empty when @replay = False@).
--
--   This is the direct, @interpretH@-free replacement for the old
--   @runAtH@: the recursive walk is plain function recursion in
--   'StorageT', so every level costs one ordinary monadic bind, not one
--   Polysemy effect reification.
at :: StorageM m => Bool -> TickId -> StorageT m a -> StorageT m (Either String (a, [(TickId, TickId)]))
at replay tid action = do
  outerHead <- headTickId
  result    <- go outerHead
  case result of
    Left err -> return (Left err)
    Right ok
      | replay    -> return (Right ok)
      | otherwise -> putHead outerHead >> return (Right ok)
  where
    go current
      | TickId (unObjectHash current) == tid = do
          a <- action
          return (Right (a, []))
      | otherwise = do
          mResult <- liftG $ do
            cd <- gitReadCommit current
            case commitParents cd of
              [] -> return $ Left $
                "At: tick " <> T.unpack (unTickId tid) <> " not found in branch history"
              (parent : _) -> do
                parentWt <- loadWorkingTree parent
                commitWt <- loadWorkingTree current
                return $ Right (parent, parentWt, commitWt, cd)
          case mResult of
            Left err -> return (Left err)
            Right (parent, parentWt, commitWt, cd) -> do
              putHead parent
              eInner <- go parent
              case eInner of
                Left err -> return (Left err)
                Right (a, innerMapping)
                  | not replay -> return (Right (a, innerMapping))
                  | otherwise  -> do
                      -- The inner recursion already left head at whatever it
                      -- rebuilt @parent@ into (or, at the base case, at
                      -- @tid@ itself) — that's the new parent this level's
                      -- diff replays onto.
                      newParent   <- headTickId
                      newParentWt <- liftG (loadWorkingTree newParent)
                      newWt       <- liftG (applyDiff parentWt commitWt newParentWt)
                      treeHash    <- liftG (flushWorkingTree newWt)
                      newHash     <- liftG $ gitWriteCommit cd
                        { commitParents = newParent : drop 1 (commitParents cd)
                        , commitTree    = treeHash
                        }
                      putHead newHash
                      let oldId = TickId (unObjectHash current)
                          newId = TickId (unObjectHash newHash)
                      return $ Right (a, innerMapping <> [(oldId, newId)])

-- | Temporarily swap the ambient working tree to reflect head's committed
--   snapshot, run @action@, then restore whatever the ambient tree held
--   before. Compose with 'at' for historical filesystem access:
--   @at True tid (withFS action)@.
withFS :: StorageM m => StorageT m a -> StorageT m a
withFS action = do
  h       <- headTickId
  outerWt <- getAmbientTree
  headWt  <- liftG (loadWorkingTree h)
  putAmbientTree headWt
  a <- action
  putAmbientTree outerWt
  return a

-- | 'at', unwrapped: fails the whole 'StorageT' computation instead of
--   returning @Left@ — the common case at every call site below, which
--   already runs under a 'MonadFail' base and wants a rebase failure (an
--   unknown tick, mostly) to abort outright rather than be threaded by
--   hand.  There is no broadcasting distinction between this and the old
--   @at@\/@sneakyAt@ split any more: whether a caller's cross-branch
--   references get updated is entirely the concern of whichever Polysemy
--   runner it uses to enter 'StorageT' in the first place (see
--   @Storyteller.Core.Git@'s @runStorageEdit@) — 'StorageT' itself always
--   just returns the mapping and lets the caller decide.
atChecked :: StorageM m => Bool -> TickId -> StorageT m a -> StorageT m (a, [(TickId, TickId)])
atChecked replay tid action = at replay tid action >>= either fail return

-- | Read-only historical access: no replay, no mapping — see 'at's
--   @replay = False@ case.
readAtS :: StorageM m => TickId -> StorageT m a -> StorageT m a
readAtS tid action = fst <$> atChecked False tid action

-- | 'storeTick', unwrapped.
store :: StorageM m => TickData -> StorageT m TickId
store d = storeTick d >>= either fail return

-- | Store a typed tick — the draft is derived via 'toDraft'.
storeAs :: (StorageM m, TickType a) => a -> StorageT m TickId
storeAs = store . toDraft

-- | 'replaceTick', unwrapped.
replace :: StorageM m => TickId -> TickData -> StorageT m TickId
replace tid d = replaceTick tid d >>= either fail return

-- ---------------------------------------------------------------------------
-- Working-tree file access
-- ---------------------------------------------------------------------------

readFileS :: StorageM m => FilePath -> StorageT m BS.ByteString
readFileS path = do
  wt <- getAmbientTree
  case Map.lookup path wt of
    Just (FSFile h) -> liftG (readBlobM h)
    Just FSDir      -> fail (path <> ": is a directory")
    Nothing         -> fail (path <> ": not found")

writeFileS :: StorageM m => FilePath -> BS.ByteString -> StorageT m ()
writeFileS path content = do
  h <- liftG (writeBlobM content)
  let parents = ancestorDirs path
  modify $ \(hd, wt) ->
    ( hd
    , Map.insert path (FSFile h) (foldr (\d m -> Map.insertWith keepExisting d FSDir m) wt parents)
    )

-- | Append content to a file in the working tree, creating it if absent.
appendFileS :: StorageM m => FilePath -> BS.ByteString -> StorageT m ()
appendFileS path content = do
  exists <- fileExistsS path
  base   <- if exists then readFileS path else return BS.empty
  writeFileS path (base <> content)

createDirectoryS :: Monad m => FilePath -> StorageT m ()
createDirectoryS path = modify $ \(h, wt) -> (h, Map.insertWith keepExisting path FSDir wt)

removeS :: Monad m => Bool -> FilePath -> StorageT m ()
removeS recursive path = modify $ \(h, wt) ->
  ( h
  , if recursive
      then Map.filterWithKey (\k _ -> k /= path && not (isUnderDir path k)) wt
      else Map.delete path wt
  )

listFilesS :: Monad m => FilePath -> StorageT m [FilePath]
listFilesS dir = do
  wt <- getAmbientTree
  return [ p | p <- Map.keys wt, isDirectChild dir p ]

-- | Every plain file (no directories) anywhere under @root@ — @"/"@\/@"."@\/
--   @""@ mean the whole tree. Unlike 'listFilesS' (direct children only),
--   this recurses, so a file under a subdirectory is still found.
listAllFilesS :: Monad m => FilePath -> StorageT m [FilePath]
listAllFilesS root = do
  wt <- getAmbientTree
  return [ p | (p, FSFile _) <- Map.toList wt, isRootOrUnder root p ]
  where
    isRootOrUnder r p
      | r `elem` ["/", ".", ""] = True
      | otherwise               = p == r || isUnderDir r p

fileExistsS :: Monad m => FilePath -> StorageT m Bool
fileExistsS path = Map.member path <$> getAmbientTree

isDirectoryS :: Monad m => FilePath -> StorageT m Bool
isDirectoryS path = do
  wt <- getAmbientTree
  return $ case Map.lookup path wt of
    Just FSDir -> True
    _          -> False

keepExisting :: FSNode -> FSNode -> FSNode
keepExisting _ old = old

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
  in [ joinPath (take n parts) | n <- [1 .. length parts - 1] ]

-- ---------------------------------------------------------------------------
-- Append-only invariant
-- ---------------------------------------------------------------------------

-- | Check that every file in the new working tree is a pure append of the
--   corresponding file in the parent tree — see
--   @Storyteller.Core.Git@'s original doc for the full rationale (kept
--   verbatim; this is unchanged domain logic, just retargeted to
--   'MonadGit').
checkAppendOnly :: StorageM m => TickData -> WorkingTree -> WorkingTree -> m (Either String ())
checkAppendOnly draft' parent new = go (Map.toList new)
  where
    atomClaim = do
      content <- decodeTaggedMessage @Atom.Atom (tickMessage draft')
      file    <- lookup "file" (tickFields draft')
      return (T.unpack file, content)

    go []             = return (Right ())
    go ((_, FSDir) : rest) = go rest
    go ((path, FSFile newHash) : rest) =
      case Map.lookup path parent of
        Nothing -> case atomClaim of
          Just (atomPath, content) | atomPath == path -> do
            newContent <- readBlobM newHash
            if newContent == TE.encodeUtf8 content
              then go rest
              else atomMismatch path
          _ -> go rest
        Just FSDir        -> go rest
        Just (FSFile oldHash) -> do
          oldContent <- readBlobM oldHash
          newContent <- readBlobM newHash
          case atomClaim of
            Just (atomPath, content) | atomPath == path ->
              if newContent == oldContent <> TE.encodeUtf8 content
                then go rest
                else atomMismatch path
            _ ->
              if oldContent `BS.isPrefixOf` newContent
                then go rest
                else return $ Left $ "Store: non-append modification of " <> path
                       <> " (old=" <> show (BS.length oldContent)
                       <> " new=" <> show (BS.length newContent)
                       <> " oldHash=" <> T.unpack (unObjectHash oldHash)
                       <> " newHash=" <> T.unpack (unObjectHash newHash)
                       <> ")"

    atomMismatch path = return $ Left $
      "Store: atom message for " <> path <> " does not match its actual diff"

-- | Apply the diff between @originalParentWt@ and @commitWt@ onto
--   @newParentWt@ — unchanged domain logic from @Storyteller.Core.Git@,
--   retargeted to 'MonadGit'.
applyDiff :: StorageM m => WorkingTree -> WorkingTree -> WorkingTree -> m WorkingTree
applyDiff originalParentWt commitWt newParentWt =
  foldM applyFile newParentWt (Map.toList commitWt)
  where
    applyFile wt (path, FSDir) =
      return $ Map.insertWith keepExisting path FSDir wt
    applyFile wt (path, FSFile newHash) = do
      commitContent <- readBlobM newHash
      originalContent <- case Map.lookup path originalParentWt of
        Just (FSFile h) -> readBlobM h
        _               -> return BS.empty
      let suffix = BS.drop (BS.length originalContent) commitContent
      baseContent <- case Map.lookup path wt of
        Just (FSFile h) -> readBlobM h
        _               -> return BS.empty
      hash' <- writeBlobM (baseContent <> suffix)
      return $ Map.insert path (FSFile hash') wt

-- ---------------------------------------------------------------------------
-- File-tick projection
-- ---------------------------------------------------------------------------

-- | A single tick entry from the file-tick projection of a branch.
--   Oldest-first when returned by 'fileTicksOf'.
--   Atoms have 'ftContent = Just blobSuffix'; non-atom ticks have 'Nothing'.
data FileTick = FileTick
  { ftTickId  :: Text
  , ftKind    :: Text           -- "atom", "note", "prompt", etc.
  , ftRefs    :: [Text]
  , ftFields  :: [(Text, Text)]
  , ftMessage :: Text
  , ftContent :: Maybe Text     -- Just for atoms, Nothing otherwise
  , ftParent  :: Maybe Text
  } deriving (Show, Eq)

-- | Walk the branch history from head and extract all ticks relevant to
--   @path@ — unchanged domain logic from @Storyteller.Core.Git@'s
--   @walkFileTicks@, retargeted to 'MonadGit'. Returns oldest-first.
fileTicksOf :: StorageM m => FilePath -> StorageT m [FileTick]
fileTicksOf path = do
  h <- headTickId
  liftG (walkFileTicks path h)

walkFileTicks :: StorageM m => FilePath -> ObjectHash -> m [FileTick]
walkFileTicks path headHash = do
  raw <- collectChain headHash []
  let allTicks  = map (uncurry toFileTick) raw
      fileHint  = T.pack path
      atomIds   = [ ftTickId ft | ft <- allTicks, ftContent ft /= Nothing ]
      memberIds = expandRefs atomIds allTicks
      fileHinted = [ ftTickId ft | ft <- allTicks
                                 , ftContent ft == Nothing
                                 , lookup "file" (ftFields ft) == Just fileHint ]
      included  = Set.fromList (memberIds ++ fileHinted)
  return (relinkParents included Nothing allTicks)
  where
    relinkParents :: Set.Set Text -> Maybe Text -> [FileTick] -> [FileTick]
    relinkParents _ _ [] = []
    relinkParents included lastIncluded (ft : rest)
      | Set.member (ftTickId ft) included =
          ft { ftParent = lastIncluded } : relinkParents included (Just (ftTickId ft)) rest
      | otherwise = relinkParents included lastIncluded rest

    collectChain :: StorageM m => ObjectHash -> [(ObjectHash, CommitData)] -> m [(ObjectHash, CommitData)]
    collectChain hash' acc = do
      cd <- gitReadCommit hash'
      case commitParents cd of
        []      -> return ((hash', cd) : acc)
        (p : _) -> collectChain p ((hash', cd) : acc)

    expandRefs :: [Text] -> [FileTick] -> [Text]
    expandRefs members ticks =
      let step ms = ms ++ [ ftTickId ft
                           | ft <- ticks
                           , ftTickId ft `notElem` ms
                           , any (`elem` ms) (ftRefs ft) ]
      in step (step members)

    toFileTick :: ObjectHash -> CommitData -> FileTick
    toFileTick hash' cd =
      let tick    = commitToTick hash' cd
          td      = tickData tick
          mSuffix = case fromTick tick :: Maybe Atom.Atom of
                      Just (Atom.Atom f content) | f == path -> Just content
                      _                                      -> Nothing
          kind    = case tickTypeOf tick of
                      Just t  -> t
                      Nothing -> if mSuffix /= Nothing then "atom" else "unknown"
          msg     = stripTypeTag kind (tickMessage td)
      in FileTick
        { ftTickId  = unObjectHash hash'
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

-- ---------------------------------------------------------------------------
-- Editing operations
-- ---------------------------------------------------------------------------
--
-- Chain editing (delete/move/merge/split a tick) and append/rewrite (a
-- position-relative write-then-commit, and its inverses) — ported verbatim
-- from the old 'Storyteller.Core.Edit'\/'Storyteller.Core.Append' Polysemy
-- modules. These are mechanical consequences of the append-only invariant
-- (DATA-MODEL.md), not story-specific policy, so they live with the monad
-- they're built from rather than in an app-facing module.
--
-- None of these broadcast their returned old->new id mapping via
-- 'Storyteller.Core.Storage.updateReferences' any more — that was a
-- 'StoryStorage' (cross-branch, Polysemy) concern before, and stays one:
-- see 'Storyteller.Core.Git.runStorageEdit', the Polysemy runner that
-- dispatches one of these as a single 'GitBranchOp' and then broadcasts its
-- mapping.

-- | A tick extracted from the chain, ready to be re-inserted elsewhere.
--   Carries the metadata (message, refs) and the concrete file diffs the
--   tick introduced, so it can be faithfully replayed at a new position
--   without depending on the current working tree's state.
data TDraft = TDraft
  { tdRefs      :: [TickId]
  , tdFields    :: [(Text, Text)]
  , tdMessage   :: Text
  , tdFileDiffs :: Map FilePath Text  -- ^ per-file suffix this tick added
  } deriving (Show, Eq)

toTickData :: TDraft -> TickData
toTickData d = TickData { tickRefs = tdRefs d, tickFields = tdFields d, tickMessage = tdMessage d }

-- | Pop the tick currently at head, returning its draft with file diffs.
--   An atom's own contribution lives verbatim in its commit message (see
--   'Storyteller.Core.Atom.contentFor'), so no filesystem snapshot is
--   needed to recover it — this only rewinds head to the parent, leaving
--   it there for the caller.
popTick :: StorageM m => StorageT m TDraft
popTick = do
  tick <- getTick
  dropTick
  let diffs = case fromTick @Atom.Atom tick of
        Just (Atom.Atom path _) -> Map.singleton path (Atom.contentFor path tick)
        Nothing                 -> Map.empty
  return TDraft
    { tdRefs      = tickRefs (tickData tick)
    , tdFields    = tickFields (tickData tick)
    , tdMessage   = tickMessage (tickData tick)
    , tdFileDiffs = diffs
    }

-- | Re-insert a popped tick at the current head by appending its file diffs
--   and committing with the original message and refs. The current ambient
--   tree is irrelevant — the diffs are applied on top of whatever head's
--   own snapshot is (via 'withFS'), then committed.
pushTick :: StorageM m => TDraft -> StorageT m TickId
pushTick d = withFS $ do
  mapM_ (\(path, suffix) -> appendFileS path (TE.encodeUtf8 suffix)) (Map.toList (tdFileDiffs d))
  store (toTickData d)

-- | Remove a tick from the chain entirely. Returns the old->new id mapping
--   for all replayed ticks.
--
--   'at's own replay only ever advances the tracked head — like every
--   other top-level editing operation here, it has to explicitly
--   'resetTree' before returning so the ambient tree actually reflects
--   the rebased chain, not whatever it was before the rebase (the same
--   role the old Polysemy @StoryBranch@ effect's @sync@ played, minus the
--   git-ref re-resolution half, which is a 'Storyteller.Core.Git.runStorageEdit'
--   concern — see its own doc).
deleteTick :: StorageM m => TickId -> StorageT m [(TickId, TickId)]
deleteTick tid = do
  mapping <- snd <$> atChecked True tid dropTick
  resetTree
  return mapping

-- | Replace an atom's content in place, preserving its chain position.
--   Returns (newTickId, tail-mapping including the edited tick itself).
editAtom :: StorageM m => TickId -> FilePath -> Text -> StorageT m (TickId, [(TickId, TickId)])
editAtom tid path newContent = do
  (newTid, mapping) <- rewriteAtom tid path newContent
  resetTree
  return (newTid, (tid, newTid) : mapping)

-- ---------------------------------------------------------------------------
-- Chain-level move
-- ---------------------------------------------------------------------------

-- | Move @tid@ to immediately after @mAfter@ (@Nothing@ = move to front).
--
--   Backward (tid currently after target):
--     @at tid $ do d <- popTick; at after $ pushTick d@
--
--   Forward (tid currently before target):
--     @at after $ do (d,_) <- at tid $ popTick; pushTick d@
--
--   Both are a single nested 'at' — one coherent rebase pass. Returns the
--   complete old->new id mapping for every tick that changed.
moveTick :: StorageM m => TickId -> Maybe TickId -> StorageT m [(TickId, TickId)]
moveTick tid mAfter = do
  chain <- followChain [] (\acc t -> (t : acc, tickParent t))
  (root, contentOrdered) <- case chain of
    (r : rest) -> return (r, rest)
    []         -> fail "moveTick: branch has no root tick"

  tidPos   <- findPos "tick to move" tid contentOrdered
  afterPos <- case mAfter of
    Nothing               -> return (-1)
    Just aid | aid == tickId root -> return (-1)  -- explicit root is the same target as Nothing
             | otherwise           -> findPos "afterTickId" aid contentOrdered

  checkMoveOrder tid mAfter tidPos afterPos contentOrdered

  let resolvedAfter = maybe (tickId root) id mAfter

  ((newTid, innerMapping), outerMapping) <-
    if tidPos > afterPos
      then -- Backward: at tid (pop >>= \d -> at after (push d))
        atChecked True tid $ do
          d                  <- popTick
          (newTid, innerMap) <- atChecked True resolvedAfter $ pushTick d
          return (newTid, innerMap)
      else -- Forward: at after (at tid pop >>= push)
        atChecked True resolvedAfter $ do
          (d, innerMap) <- atChecked True tid $ popTick
          newTid        <- pushTick d
          return (newTid, innerMap)

  resetTree
  return (outerMapping <> innerMapping <> [(tid, newTid)])

-- ---------------------------------------------------------------------------
-- Merge
-- ---------------------------------------------------------------------------

-- | Merge a contiguous run of one file's atoms into a single atom, in the
--   position of the earliest one. All ids in the group remap to the one
--   result id — the several-old-ids-to-one-new-id generalization of
--   'editAtom''s single-id mapping.
--
--   Requires at least two ids, all on the same file, occupying consecutive
--   chain positions. A gapped selection (another tick, atom or not, sitting
--   between two of the chosen atoms) is rejected rather than silently
--   collapsed — collapsing across an unrelated tick would reorder it
--   relative to content it didn't originally follow.
mergeAtoms :: StorageM m => [TickId] -> StorageT m (TickId, [(TickId, TickId)])
mergeAtoms []  = fail "mergeAtoms: need at least two atoms"
mergeAtoms [_] = fail "mergeAtoms: need at least two atoms"
mergeAtoms tids = do
  chain <- followChain [] (\acc t -> (t : acc, tickParent t))
  contentOrdered <- case chain of
    (_ : rest) -> return rest
    []         -> fail "mergeAtoms: branch has no root tick"

  positioned <- mapM (\tid -> (tid,) <$> findPos "atom to merge" tid contentOrdered) tids
  let ordered    = map fst (List.sortOn snd positioned)
      positions  = List.sort (map snd positioned)

  checkContiguous positions
  path <- sameFile contentOrdered ordered

  let lastTid = last ordered

  (newTid, tailMapping) <- atChecked True lastTid $ do
    drafts <- popN (length ordered)
    pushTick (mergeDrafts path (reverse drafts))

  resetTree
  let fullMapping = tailMapping ++ [(tid, newTid) | tid <- ordered]
  return (newTid, fullMapping)
  where
    checkContiguous ps
      | and (zipWith (\a b -> b == a + 1) ps (List.drop 1 ps)) = return ()
      | otherwise = fail "mergeAtoms: selected atoms must be contiguous"

    sameFile ordered' groupTids = do
      paths <- mapM (fileOfTick ordered') groupTids
      case List.nub paths of
        [p] -> return p
        _   -> fail "mergeAtoms: selected atoms must all belong to the same file"

    fileOfTick ordered' tid =
      case List.find (\t -> tickId t == tid) ordered' of
        Just t | Just f <- lookup "file" (tickFields (tickData t)) -> return (T.unpack f)
        _ -> fail ("mergeAtoms: not an atom: " <> T.unpack (unTickId tid))

    -- Popping n times off head (set to the last atom of the group by the
    -- enclosing 'atChecked') walks backward through each member of the
    -- group in turn, landing head on the anchor right before the first
    -- one once all n are popped — 'moveTick''s single-pop idiom,
    -- generalized to n. Returned newest-first (last atom's draft first).
    popN :: StorageM m => Int -> StorageT m [TDraft]
    popN 0 = return []
    popN k = (:) <$> popTick <*> popN (k - 1)

    -- 'tdMessage' carries the tick's full raw, tagged message (e.g.
    -- @"type:atom\npara1"@) — concatenating those directly would splice the
    -- tag in mid-string. Each draft's own content is decoded out first, the
    -- pieces concatenated, then the result is re-tagged once as a single
    -- atom.
    mergeDrafts path ds = TDraft
      { tdRefs      = filter (`notElem` tids) (concatMap tdRefs ds)
      , tdFields    = [("file", T.pack path)]
      , tdMessage   = tickMessage (toDraft (Atom.Atom path (T.concat (map contentOf ds))))
      , tdFileDiffs = Map.unionsWith (<>) (map tdFileDiffs ds)
      }
      where
        contentOf d = case decodeTaggedMessage @Atom.Atom (tdMessage d) of
          Just c  -> c
          Nothing -> tdMessage d  -- unreachable: 'sameFile' already required these to be atoms

-- ---------------------------------------------------------------------------
-- Split
-- ---------------------------------------------------------------------------

-- | Explode one atom into several caller-supplied pieces, replacing it in
--   place. No splitting policy lives here — the caller decides the pieces
--   and hands them over already split. The first piece inherits @tid@'s
--   incoming references (DATA-MODEL's "which inherits the original ID" —
--   the reverse of 'mergeAtoms'); the rest are fresh ticks with no refs of
--   their own.
splitTick :: StorageM m => TickId -> [Text] -> StorageT m ([TickId], [(TickId, TickId)])
splitTick _ pieces | length pieces < 2 =
  fail "splitTick: need at least two pieces"
splitTick tid pieces = do
  (newIds, tailMapping) <- atChecked True tid $ do
    d <- popTick
    case lookup "file" (tdFields d) of
      Nothing -> fail ("splitTick: not an atom: " <> T.unpack (unTickId tid))
      Just f  ->
        -- 'popTick's own 'dropTick' only rewinds the tracked head position,
        -- not the ambient tree — so without this 'withFS', every 'storeAs'
        -- below would read the *unchanged*, still-at-the-original-atom's-
        -- full-content tree. 'withFS' loads the just-dropped-to parent's
        -- snapshot once so each 'appendFileS' actually grows the tree piece
        -- by piece, exactly as 'editAtom'/'pushTick' do.
        withFS $
          mapM (\p -> do
                  appendFileS (T.unpack f) (TE.encodeUtf8 p)
                  storeAs (Atom.Atom (T.unpack f) p))
               pieces
  case newIds of
    [] -> fail "splitTick: internal error: no pieces stored"
    (inheritor : _) -> do
      resetTree
      return (newIds, tailMapping ++ [(tid, inheritor)])

-- ---------------------------------------------------------------------------
-- Ordering invariant
-- ---------------------------------------------------------------------------

checkMoveOrder :: MonadFail m => TickId -> Maybe TickId -> Int -> Int -> [Tick] -> m ()
checkMoveOrder tid _mAfter tidPos afterPos ordered = do
  movingTick <- case List.drop tidPos ordered of
    (t : _) -> return t
    []      -> fail "checkMoveOrder: tick position out of range"
  let newPos = afterPos + 1

  mapM_ (checkRefBefore newPos) (tickRefs (tickData movingTick))

  let precedingSlice = filter (\t -> tickId t /= tid) (take (afterPos + 1) ordered)
  mapM_ (checkNotRefTo tid) precedingSlice
  where
    checkRefBefore newPos refId =
      case findIndex (\t -> tickId t == refId) ordered of
        Nothing -> return ()
        Just rp ->
          if rp < newPos then return ()
          else fail $ "cannot move tick before its own reference "
                    <> T.unpack (unTickId refId)

    checkNotRefTo movingId t =
      if movingId `elem` tickRefs (tickData t)
        then fail $ "cannot move tick after tick that references it: "
                  <> T.unpack (unTickId (tickId t))
        else return ()

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

findPos :: MonadFail m => String -> TickId -> [Tick] -> m Int
findPos label tid ordered =
  maybe (fail $ label <> " not found: " <> T.unpack (unTickId tid)) return
    (findIndex (\t -> tickId t == tid) ordered)

-- | Resolve each id's position among content ticks (root excluded),
--   oldest-first — for a caller that needs to process a batch of ids in
--   chain order without walking the chain once per id.
chainPositions :: StorageM m => [TickId] -> StorageT m [(TickId, Int)]
chainPositions tids = do
  chain <- followChain [] (\acc t -> (t : acc, tickParent t))
  contentOrdered <- case chain of
    (_ : rest) -> return rest
    []         -> fail "chainPositions: branch has no root tick"
  mapM (\tid -> (tid,) <$> findPos "tick" tid contentOrdered) tids

-- ---------------------------------------------------------------------------
-- Append, remove, and rewrite atoms, verbatim
-- ---------------------------------------------------------------------------
--
-- A position-relative write-then-commit ('storeAtom') and 'dropTick'
-- (position-relative removal) are the two moves everything here is built
-- from: 'unstoreAtom' is 'dropTick' at an arbitrary tick with the tail
-- replayed back on top; 'rewriteAtom' is both at once, under a single
-- rewind so the replayed tail lands on top of the edit instead of after it.

-- | Append @content@ to @path@ as a single atom, verbatim, and commit it.
append :: StorageM m => FilePath -> Text -> StorageT m TickId
append path content = appendAtom path (ensureTrailingNewline content)

-- | Commit @content@ as a new atom tick appended to @path@'s own
--   head-committed value — entirely independent of whatever the live
--   ambient tree currently holds, for @path@ or any other file. Built
--   under 'withFS', which loads a throwaway copy of head's own committed
--   snapshot to append onto and commit, then restores the ambient tree
--   exactly as it was.
storeAtom :: StorageM m => FilePath -> Text -> StorageT m TickId
storeAtom path content =
  withFS $ do
    appendFileS path (TE.encodeUtf8 content)
    storeAs (Atom.Atom path content)

-- | The dual of 'storeAtom': drop @tid@ — an atom tick anywhere in the
--   branch's history, not necessarily head — and replay everything after
--   it back on top, restoring the diff that tick's commit had folded in.
unstoreAtom :: StorageM m => TickId -> StorageT m [(TickId, TickId)]
unstoreAtom tid = snd <$> atChecked True tid dropTick

-- | Replace tick @tid@ in place with a freshly-appended atom: drop it, then
--   write @content@ onto whatever's left at that position, all under one
--   rewind so the tail replays on top of the edit rather than after it
--   lands somewhere else. Returns the new tick's id and the tail's
--   old->new mapping.
rewriteAtom :: StorageM m => TickId -> FilePath -> Text -> StorageT m (TickId, [(TickId, TickId)])
rewriteAtom tid path content = atChecked True tid $ do
  dropTick
  withFS $ do
    appendFileS path (TE.encodeUtf8 content)
    storeAs (Atom.Atom path content)

-- | Append @content@ to @path@ and commit it as a real atom tick, with no
--   newline normalization — the primitive 'append' builds on. An isolated
--   commit first (head advances, ambient tree untouched — see
--   'storeAtom'), then a plain, unconditional write onto whatever the
--   ambient tree currently holds for @path@.
appendAtom :: StorageM m => FilePath -> Text -> StorageT m TickId
appendAtom path content = do
  newTid <- storeAtom path content
  appendFileS path (TE.encodeUtf8 content)
  return newTid

-- | Ensure text ends with a newline — an appended atom is one text block
--   on disk, and a block should end its line.
ensureTrailingNewline :: Text -> Text
ensureTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"

-- ---------------------------------------------------------------------------
-- Working-tree commit
-- ---------------------------------------------------------------------------
--
-- Reconciles an arbitrary edited working tree against the committed atom
-- chain, conservatively: only trimming (removing some of an atom's own
-- original bytes, from its front and/or back — never its middle) can
-- change an atom's classification. Padding alone, with no trim, is
-- indistinguishable from an adjacent insertion and so is never attributed
-- to an untouched atom.
--
--   * Untouched (no trim recovered) -> kept as-is: same tick id untouched.
--   * Trimmed, nonzero content remaining (after folding in any immediately
--     adjacent new bytes) -> changed: a same-position replacement tick,
--     same pattern as 'editAtom'.
--   * Trimmed to nothing -> dropped: same pattern as 'deleteTick'.
--   * New content that isn't absorbed by an adjacent trimmed atom -> a
--     standalone new tick, inserted after whatever currently precedes it.
--
-- See Storyteller.CommitWorkingTreeSpec for the full contract and the
-- reasoning behind the fold rule.

-- | One committed atom's own contributed content, in the order they were
--   written — the file's history expressed at atom granularity rather
--   than as opaque length checkpoints.
type AtomHistory = [(TickId, Text)]

-- | Reconcile every file currently in the working tree against its atom
--   history.
commitWorkingTree :: StorageM m => StorageT m [(TickId, TickId)]
commitWorkingTree = listFilesS "/" >>= commitFiles

-- | Reconcile only the given files' working-tree content against their
--   atom history, rather than every file in the branch — same rule as
--   'commitWorkingTree' just scoped to a caller-chosen subset.
commitFiles :: StorageM m => [FilePath] -> StorageT m [(TickId, TickId)]
commitFiles files = do
  mapping  <- foldM commitFile Map.empty files
  newFiles <- filterM (fmap null . buildAtomHistory) files
  storeNewFiles newFiles
  return (Map.toList mapping)

-- | Commit one file's reconciliation, threading a running old->current
--   tick id remap table (needed because rebasing one atom's tail can move
--   ids for atoms processed later, in this file or — since 'at' rebases
--   the whole branch — any other file too).
commitFile :: StorageM m => Map TickId TickId -> FilePath -> StorageT m (Map TickId TickId)
commitFile table file = do
  history <- buildAtomHistory file
  if null history then return table else do
    target <- readWorking file
    root   <- rootTickId
    let matches  = matchAtoms history target
        n        = length matches
        gaps     = gapContents matches target
        fates    = gapFates matches gaps
        contents = finalAtomContents matches target gaps fates
        outs     = classify matches contents
        -- 'gaps'/'fates' carry one entry per atom (the gap immediately
        -- before it) plus one trailing entry for the gap after the last
        -- atom; zip5 below pairs each atom with its own leading gap/fate,
        -- leaving that trailing pair for the final 'emitStandaloneGap' call.
        perAtom  = zip5 matches (take n gaps) (take n fates) contents outs
        (tailGap, tailFate) = case (List.drop n gaps, List.drop n fates) of
          (g : _, f : _) -> (g, f)
          _              -> (T.empty, Standalone)
    (table1, anchor1) <- foldM (commitAtom file) (table, root) perAtom
    fst <$> emitStandaloneGap file table1 anchor1 tailGap tailFate

-- | Process the gap immediately before this atom (folding it in is
--   handled as part of the atom's own content — see
--   'finalAtomContents' — so only a standalone gap needs its own tick
--   here), then the atom itself: left untouched if kept, replaced in
--   place if changed, removed if dropped.
commitAtom
  :: StorageM m
  => FilePath
  -> (Map TickId TickId, TickId)
  -> (AtomMatch, Text, GapFate, Text, AtomOutcome)
  -> StorageT m (Map TickId TickId, TickId)
commitAtom file (table, anchor) (m, gap, fate, content, outcome) = do
  (table1, anchor1) <- emitStandaloneGap file table anchor gap fate
  let origId = resolveId table1 (amTickId m)
  case outcome of
    Kept -> return (table1, origId)
    Dropped -> do
      tailMapping <- unstoreAtom origId
      return (composeMapping table1 tailMapping, anchor1)
    Changed -> do
      (newTid, tailMapping) <- rewriteAtom origId file content
      return (composeMapping table1 (tailMapping ++ [(origId, newTid)]), newTid)

-- | A gap that folded onto a neighbor was already absorbed into that
--   atom's own content by 'finalAtomContents' — nothing to do here. A
--   standalone gap becomes its own new tick, inserted right after
--   @anchor@ (whatever currently precedes it in the chain).
emitStandaloneGap
  :: StorageM m
  => FilePath -> Map TickId TickId -> TickId -> Text -> GapFate
  -> StorageT m (Map TickId TickId, TickId)
emitStandaloneGap file table anchor content fate
  | fate /= Standalone || T.null content = return (table, anchor)
  | otherwise = do
      (newTid, tailMapping) <- atChecked True anchor $ withFS $ do
        appendFileS file (TE.encodeUtf8 content)
        storeAs (Atom.Atom file content)
      return (composeMapping table tailMapping, newTid)

resolveId :: Map TickId TickId -> TickId -> TickId
resolveId table tid = Map.findWithDefault tid tid table

-- | Fold a freshly-returned old->new mapping (from one 'at' call's tail
--   rebase) into the running table: existing entries whose current id was
--   itself just remapped follow the new mapping, and brand new entries
--   are added for ids not previously tracked.
composeMapping :: Map TickId TickId -> [(TickId, TickId)] -> Map TickId TickId
composeMapping table new =
  let newMap       = Map.fromList new
      updatedOld   = Map.map (\cur -> Map.findWithDefault cur cur newMap) table
      freshEntries = Map.filterWithKey (\k _ -> not (Map.member k table)) newMap
  in Map.union updatedOld freshEntries

-- | The branch's root tick — the anchor used for content inserted before
--   the first atom of a file.
rootTickId :: StorageM m => StorageT m TickId
rootTickId = do
  chain <- followChain [] (\acc t -> (t : acc, tickParent t))
  case chain of
    (root : _) -> return (tickId root)
    []         -> fail "commitWorkingTree: branch has no root tick"

-- | A file's history expressed at atom granularity: each committed tick's
--   own contributed bytes, oldest-first — read straight off each tick's
--   commit message (see 'Storyteller.Core.Atom.contentFor'), since an
--   atom's own content lives there verbatim. No filesystem access needed.
buildAtomHistory :: StorageM m => FilePath -> StorageT m AtomHistory
buildAtomHistory file = do
  ticks <- followChain [] $ \acc tick -> (tick : acc, tickParent tick)
  return [ (tickId t, Atom.contentFor file t)
         | t <- ticks
         , Just (Atom.Atom f _) <- [fromTick t]
         , f == file ]

-- ---------------------------------------------------------------------------
-- Matching: recover each atom's surviving core from the target content
-- ---------------------------------------------------------------------------

data AtomMatch = AtomMatch
  { amTickId      :: TickId
  , amOriginalLen :: Int
  , amCoreStart   :: Int  -- ^ offset into the target content
  , amCoreLen     :: Int  -- ^ 0 if the atom didn't survive at all
  }

data AtomOutcome = Kept | Changed | Dropped deriving Eq
data GapFate = FoldBack | FoldFront | Standalone deriving Eq

-- | Walk atoms oldest-first, anchoring each one's surviving core (a
--   contiguous, order-preserving substring of its original content) in
--   the target content via longest-common-substring search from the
--   current cursor onward. Trimming is only ever recognized at an atom's
--   front and/or back — a substring match is exactly that: no interior
--   deletions.
matchAtoms :: AtomHistory -> Text -> [AtomMatch]
matchAtoms history target = go 0 history
  where
    go _ [] = []
    go cursor ((tid, orig) : rest) =
      let remaining     = T.drop cursor target
          (_, bOff, len) = longestCommonSubstring orig remaining
          coreStart     = cursor + bOff
          cursor'       = coreStart + len
      in AtomMatch tid (T.length orig) coreStart len : go cursor' rest

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
--   if it's *partially* trimmed — a nonempty core remains, but not the
--   whole original.
gapFates :: [AtomMatch] -> [Text] -> [GapFate]
gapFates matches gaps = zipWith3 fate3 gaps prevElig curElig
  where
    elig     = map (\m -> amCoreLen m > 0 && amCoreLen m < amOriginalLen m) matches
    prevElig = False : elig
    curElig  = elig ++ [False]
    fate3 gap prev cur
      | T.null gap = Standalone
      | prev        = FoldBack
      | cur         = FoldFront
      | otherwise   = Standalone

-- | Each atom's final content: its surviving core plus whatever gaps
--   folded onto its front\/back.
finalAtomContents :: [AtomMatch] -> Text -> [Text] -> [GapFate] -> [Text]
finalAtomContents matches target gaps fates =
  [ frontFold ff fg <> coreOf target m <> backFold bf bg
  | (m, ff, fg, bf, bg) <- zip5 matches frontFates frontGaps backFates backGaps ]
  where
    n          = length matches
    frontFates = take n fates
    frontGaps  = take n gaps
    backFates  = List.drop 1 fates
    backGaps   = List.drop 1 gaps
    frontFold fate gap = if fate == FoldFront then gap else T.empty
    backFold  fate gap = if fate == FoldBack  then gap else T.empty

-- | Classify each atom given its already-computed final content (core
--   plus any folded-in gap text — see 'finalAtomContents').
classify :: [AtomMatch] -> [Text] -> [AtomOutcome]
classify matches contents =
  [ if isKept m then Kept else if T.null fc then Dropped else Changed
  | (m, fc) <- zip matches contents ]

-- | Longest common substring of two texts: returns the offset into each
--   (in characters) and the shared length. @(0, 0, 0)@ if either is empty
--   or there is no overlap. O(n*m) time and space — fine for atom-sized
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

-- | New files present in the working tree but absent from history: each
--   gets its own 'Created' tick (the path's introduction, empty content),
--   immediately followed by an atom tick carrying its target content if
--   any. Reads each file's target content before resetting (which
--   discards the pending buffer for files already reconciled via
--   'commitFile') so only the new files' bytes get replayed onto the now-
--   current head.
storeNewFiles :: StorageM m => [FilePath] -> StorageT m ()
storeNewFiles [] = return ()
storeNewFiles files = do
  contents <- mapM (\f -> (f,) <$> readWorking f) files
  resetTree
  mapM_ (uncurry storeNewFile) contents
  where
    storeNewFile f c = do
      writeFileS f BS.empty
      _ <- storeAs (Created f)
      if T.null c
        then return ()
        else () <$ appendAtom f c

-- | A file's current working-tree content, decoded as text — the one
--   place raw filesystem bytes cross into the atom\/'Text' world this
--   module otherwise stays in entirely. Fails loudly on invalid UTF-8
--   rather than silently replacing bad bytes: a file this can't represent
--   as text should stop reconciliation here, not corrupt content quietly
--   at some later, harder-to-trace point.
readWorking :: StorageM m => FilePath -> StorageT m Text
readWorking path = do
  exists <- fileExistsS path
  bytes  <- if exists then readFileS path else return BS.empty
  case TE.decodeUtf8' bytes of
    Right t  -> return t
    Left err -> fail ("readWorking: " <> path <> " is not valid UTF-8: " <> show err)
