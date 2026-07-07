{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core primitives for an append-only tick chain over any content-
-- addressed object store ('MonadStore') that can hold objects and refs
-- -- no dependency on git specifically, or on Storyteller's own
-- vocabulary (tick types, branches, Polysemy). A chain is a sequence of
-- commits, each holding a 'Tick': either an 'Atom' (an append to one
-- file) or a 'NonAtom' (an opaque message, no filesystem footprint).
--
-- A scope tracks two entirely independent pieces of state (see
-- 'ScopeState'): where it is in the chain (head), and an ambient working
-- tree that 'readFile'\/'writeFile'\/'createDirectory'\/'remove'\/'list'
-- operate on by path. Each piece has its own, disjoint set of operations
-- that touch it:
--
--   * 'store'\/'drop' -- the only two operations that change the chain.
--     'store' commits a 'Tick' onto head: an 'Atom' computes its new tree
--     directly from head's own (parent) tree (read its path's old blob,
--     extend it, splice that one entry back in, flush -- never from the
--     ambient tree), an append by construction, so there's nothing to
--     verify afterward; a 'NonAtom' reuses head's own tree hash directly,
--     never loading a 'WorkingTree' at all. 'drop' is the inverse: pop
--     the tick at head, moving head to its parent, and hand back
--     everything it was. @store =<< drop@ rebuilds the same commit in
--     place -- the identity 'at' replays on top of during a rebase.
--   * 'reset'\/'inWorktree'\/'readFile'\/'writeFile'\/'createDirectory'\/
--     'remove' -- the only operations that touch the ambient tree.
--     'reset' sets it to match head's own committed content, discarding
--     whatever was there before. 'inWorktree' runs an action against a
--     freshly-'reset' tree, then restores whatever the ambient tree held
--     before -- independent of the chain entirely, since nothing here
--     moves head. 'readFile'/'writeFile'/'createDirectory'/'remove'/'list' read or write it by path.
--
-- Moving around the chain -- 'readAt'\/'at' -- touches neither piece of
-- state directly; both are built entirely from 'store'\/'drop':
--
--   * 'readAt' -- a read-only, isolated peek: move to any commit, run an
--     action there, restore the chain to exactly where it started. Pure
--     navigation (following @commitParents@ down to the target) -- it
--     never needs 'drop'\/'store' itself, since nothing it does is meant
--     to last.
--   * 'at' -- a rebase: move to any commit, run an action there, and
--     replay every later commit back on top of whatever it produced, by
--     composing 'drop' (popping each tail tick on the way down) and
--     'store' (re-pushing it on the way back up) -- never anything of its
--     own.
module Storage.Core
  ( -- * Content-addressed object vocabulary
    ObjectHash(..)
  , CommitData(..)
  , TreeEntry(..)
  , StoreObject(..)
  , MonadStore(..)
  , StoreM

    -- * Working tree
  , FSNode(..)
  , WorkingTree
  , emptyWorkingTree

    -- * The monad
  , StoreT
  , ScopeState
  , runStoreT
  , headHash

    -- * Chain contents
  , Tick(..)
  , readTick

    -- * Core operations
  , store
  , drop
  , readAt
  , at
  , editTick
  , replaceTick
  , resolveId
  , reset
  , inWorktree

    -- * Ambient file access
  , readFile
  , writeFile
  , createDirectory
  , remove
  , list
  ) where

import Prelude hiding (drop, readFile, writeFile)
import qualified Data.List as List

import Control.Monad.State.Strict
  (StateT(..), MonadState, MonadTrans, gets, lift, runStateT, modify)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (splitDirectories, joinPath)

-- ---------------------------------------------------------------------------
-- Generic content-addressed object vocabulary
-- ---------------------------------------------------------------------------

-- | An opaque content hash. Whatever a concrete 'MonadStore' instance's
--   own hashes look like, as long as they round-trip through 'Text'.
newtype ObjectHash = ObjectHash { unObjectHash :: Text }
  deriving (Show, Eq, Ord)

-- | The data needed to describe or write one node of the chain: its
--   parent(s) -- the chain parent first, any cross-branch refs after --
--   the tree snapshot it commits, and its encoded message.
data CommitData = CommitData
  { commitParents :: [ObjectHash]
  , commitTree    :: ObjectHash
  , commitMessage :: Text
  } deriving (Show, Eq)

-- | A single entry in a tree snapshot.
data TreeEntry
  = BlobEntry { entryName :: FilePath, entryHash :: ObjectHash }
  | SubTree   { entryName :: FilePath, entryHash :: ObjectHash }
  deriving (Show, Eq)

-- | A single content-addressed object: either a raw blob, or a tree
--   (directory) of further entries.
data StoreObject
  = BlobObject BS.ByteString
  | TreeObject [TreeEntry]
  deriving (Show, Eq)

-- | Everything the chain layer needs from a content-addressed object
--   store -- read and write commits and objects, with no notion of refs
--   or branches (resolving and publishing those is entirely the caller's
--   job, before and after a 'StoreT' computation runs, never inside it).
--   Any store that can hold objects this way -- git, or anything else
--   content-addressed -- can supply an instance and reuse everything in
--   this module unchanged.
class Monad m => MonadStore m where
  readCommit  :: ObjectHash -> m CommitData
  writeCommit :: CommitData -> m ObjectHash
  readObject  :: ObjectHash -> m StoreObject
  writeObject :: StoreObject -> m ObjectHash

-- | Shorthand for the constraints every operation here needs: a pluggable
--   object store, plus the ability to fail (an unknown tick, ...).
type StoreM m = (MonadStore m, MonadFail m)

readBlobM :: StoreM m => ObjectHash -> m BS.ByteString
readBlobM h = readObject h >>= \case
  BlobObject bs -> return bs
  TreeObject _  -> fail $ "readBlob: hash is a tree: " <> T.unpack (unObjectHash h)

writeBlobM :: MonadStore m => BS.ByteString -> m ObjectHash
writeBlobM = writeObject . BlobObject

readTreeM :: StoreM m => ObjectHash -> m [TreeEntry]
readTreeM h = readObject h >>= \case
  TreeObject es -> return es
  BlobObject _  -> fail $ "readTree: hash is a blob: " <> T.unpack (unObjectHash h)

writeTreeM :: MonadStore m => [TreeEntry] -> m ObjectHash
writeTreeM = writeObject . TreeObject

-- ---------------------------------------------------------------------------
-- Working tree (in-memory filesystem) -- just a value at this level;
-- 'reset'\/'inWorktree' are what track one as ambient scope state.
-- ---------------------------------------------------------------------------

data FSNode
  = FSFile !ObjectHash
  | FSDir
  deriving (Show, Eq)

type WorkingTree = Map FilePath FSNode

emptyWorkingTree :: WorkingTree
emptyWorkingTree = Map.empty

-- | Reconstruct a 'WorkingTree' from a commit's tree object.
loadWorkingTree :: StoreM m => ObjectHash -> m WorkingTree
loadWorkingTree commitHash = do
  cd <- readCommit commitHash
  readTreeRecursive "" (commitTree cd)

readTreeRecursive :: StoreM m => FilePath -> ObjectHash -> m WorkingTree
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

-- | Write the 'WorkingTree' to the store, returning the root tree hash.
--   An empty tree is written like any other -- 'buildTree' naturally
--   produces @writeTreeM []@ for it, with no backend-specific shortcut
--   (a concrete store's own hash for "the empty tree" isn't something
--   this module can assume, unlike git's well-known one).
flushWorkingTree :: StoreM m => WorkingTree -> m ObjectHash
flushWorkingTree = buildTree ""

buildTree :: StoreM m => FilePath -> WorkingTree -> m ObjectHash
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
                    , List.take len parts == prefixParts
                    , c : _ <- [List.drop len parts]
                    ]
  in dedupe names
  where
    dedupe [] = []
    dedupe (x:xs) = x : dedupe (filter (/= x) xs)

toEntry :: StoreM m => FilePath -> FilePath -> WorkingTree -> m TreeEntry
toEntry prefix name wt =
  let path = if null prefix then name else prefix <> "/" <> name
  in case Map.lookup path wt of
    Just (FSFile hash') -> return (BlobEntry name hash')
    _ -> do
      hash' <- buildTree path wt
      return (SubTree name hash')

-- ---------------------------------------------------------------------------
-- Chain contents: every commit holds one Tick, either an Atom or a NonAtom
-- ---------------------------------------------------------------------------

-- | What a single commit in the chain holds: an 'Atom' (an append of
--   @atomContent@ to @atomPath@ -- the message *is* the content,
--   verbatim, so recovering it never needs a filesystem diff), or a
--   'NonAtom' (an opaque message this module doesn't interpret at all --
--   a higher, Storyteller-specific layer decodes what kind it actually
--   is). @'store' =<< 'drop'@ is the identity: popping a tick and storing
--   it right back reproduces the same commit.
--
--   The wire encoding follows the same convention the rest of this
--   codebase already uses for a tagged commit message -- an optional
--   @key:value@ fields block, a blank line, then @"type:<tag>\\n"@ before
--   the payload -- so a store already holding history from that
--   convention reads correctly here. Only 'Atom' needs special
--   treatment (its own tag plus a @file@ field, since its content is
--   also a tree change); anything else -- including every existing
--   Storyteller tick kind (@Root@, @Note@, @Prompt@, ...), each with its
--   own @"type:<tag>\\n"@ of its own -- passes through as a 'NonAtom'
--   verbatim, undecoded, for that higher layer to make sense of.
data Tick
  = Atom    { tickRefs :: [ObjectHash], atomPath :: FilePath, atomContent :: Text }
  | NonAtom { tickRefs :: [ObjectHash], nonAtomMessage :: Text }
  deriving (Show, Eq)

atomTag :: Text
atomTag = "type:atom\n"

encodeTick :: Tick -> Text
encodeTick (NonAtom _ msg)         = msg
encodeTick (Atom _ path content) = "file:" <> T.pack path <> "\n\n" <> atomTag <> content

-- | An atom, if @raw@ has the shape 'encodeTick' gives one: a @file@
--   field, a blank line, then the @"type:atom\\n"@ tag. Anything else
--   (no fields at all, a different tag, fields without that one) isn't
--   an atom this module recognises -- 'decodeTick' falls back to
--   'NonAtom' for all of it, verbatim.
decodeAtom :: Text -> Maybe (FilePath, Text)
decodeAtom raw = do
  let (headerBlock, rest0) = T.breakOn "\n\n" raw
  body <- if T.null rest0 then Nothing else Just (T.drop 2 rest0)
  payload <- T.stripPrefix atomTag body
  let fields = [ (k, T.drop 1 v)
               | l <- T.lines headerBlock
               , let (k, v) = T.breakOn ":" l
               , not (T.null v) ]
  path <- lookup "file" fields
  return (T.unpack path, payload)

decodeTick :: [ObjectHash] -> Text -> Tick
decodeTick refs raw = case decodeAtom raw of
  Just (path, content) -> Atom refs path content
  Nothing               -> NonAtom refs raw

-- | Read and decode the tick held by the commit at a given hash.
readTick :: StoreM m => ObjectHash -> m Tick
readTick h = do
  cd <- readCommit h
  return (decodeTick (List.drop 1 (commitParents cd)) (commitMessage cd))

-- ---------------------------------------------------------------------------
-- The monad: the current position in the chain, plus an ambient working
-- tree that's entirely independent of it -- 'at'\/'readAt' only ever move
-- through the chain and never touch this; 'reset'\/'inWorktree' and the four
-- file operations are the only ones that do.
-- ---------------------------------------------------------------------------

-- | The three pieces of state a scope needs: the commit currently at
--   head, the ambient working tree 'readFile'\/'writeFile'\/
--   'createDirectory'\/'remove' operate on, and a running old->new
--   remap table (every id any 'store' this scope has made has since
--   become, transitively closed -- see 'resolveId'\/'logRemap'). Kept as
--   a plain triple, not a record: nothing outside this module reaches
--   into any one piece without going through 'headHash'\/'reset'\/
--   'inWorktree'\/'resolveId', except a caller of 'runStoreT' who wants
--   the final remap table to propagate elsewhere once this computation
--   is done -- the one piece worth exposing 'ScopeState''s shape for.
type ScopeState = (ObjectHash, WorkingTree, Map ObjectHash ObjectHash)

newtype StoreT m a = StoreT (StateT ScopeState m a)
  deriving (Functor, Applicative, Monad, MonadState ScopeState)

instance MonadTrans StoreT where
  lift = StoreT . lift

instance MonadFail m => MonadFail (StoreT m) where
  fail = StoreT . lift . fail

-- | Run a 'StoreT' computation seeded at the given head (with the
--   ambient tree starting synced to it, as if 'reset' had just run, and
--   an empty remap table), returning the result together with the final
--   scope state.
runStoreT :: StoreM m => ObjectHash -> StoreT m a -> m (a, ScopeState)
runStoreT h (StoreT s) = do
  wt <- loadWorkingTree h
  runStateT s (h, wt, Map.empty)

liftG :: Monad m => m a -> StoreT m a
liftG = lift

headHash :: Monad m => StoreT m ObjectHash
headHash = gets (\(h, _, _) -> h)

putHead :: Monad m => ObjectHash -> StoreT m ()
putHead h = modify (\(_, wt, table) -> (h, wt, table))

getAmbientTree :: Monad m => StoreT m WorkingTree
getAmbientTree = gets (\(_, wt, _) -> wt)

putAmbientTree :: Monad m => WorkingTree -> StoreT m ()
putAmbientTree wt = modify (\(h, _, table) -> (h, wt, table))

-- | What @oid@ has become, if this scope has ever replaced it (directly,
--   or transitively through a chain of replacements) -- @oid@ itself if
--   not. Must be called right before actually using an id, never cached
--   from an earlier read: a value that was current when read can go
--   stale the moment something else in the same scope replaces it, so
--   only a lookup made at the point of use can be trusted. 'store'\/'at'\/
--   'readAt' already do this for the ids they themselves handle
--   (a tick's own 'tickRefs', and 'at'\/'readAt''s own @target@) --
--   nothing else in this module ever holds an id across a gap where it
--   could go stale, so nothing else needs to call this. A caller outside
--   this module, holding an id from before some other edit in the same
--   scope, does.
resolveId :: Monad m => ObjectHash -> StoreT m ObjectHash
resolveId oid = gets (\(_, _, table) -> Map.findWithDefault oid oid table)

-- | Record that @old@ has become @new@, folding it into the running
--   table so every earlier entry whose current value was @old@ now
--   points at @new@ instead -- keeps the table transitively closed
--   (never more than one hop to walk) as a cheap fixup on write, so
--   'resolveId' itself can stay a single lookup instead of chasing a
--   chain at every read.
logRemap :: Monad m => ObjectHash -> ObjectHash -> StoreT m ()
logRemap old new = modify (\(h, wt, table) -> (h, wt, composeMapping table [(old, new)]))

composeMapping :: Map ObjectHash ObjectHash -> [(ObjectHash, ObjectHash)] -> Map ObjectHash ObjectHash
composeMapping table new =
  let newMap       = Map.fromList new
      updatedOld   = Map.map (\cur -> Map.findWithDefault cur cur newMap) table
      freshEntries = Map.filterWithKey (\k _ -> not (Map.member k table)) newMap
  in Map.union updatedOld freshEntries

-- ---------------------------------------------------------------------------
-- Core operations
-- ---------------------------------------------------------------------------

-- | Commit @t@ onto head, advancing head to the new commit and returning
--   it. See the module Haddock for how the new tree is computed for each
--   'Tick' constructor. @t@'s own 'tickRefs' are resolved against the
--   running remap table right here -- the one point any ref actually
--   gets *used* (baked into the new commit as an extra parent) -- rather
--   than trusting whatever a caller happened to read earlier; see
--   'resolveId'.
store :: StoreM m => Tick -> StoreT m ObjectHash
store t = do
  h    <- headHash
  refs <- mapM resolveId (tickRefs t)
  treeHash <- case t of
    NonAtom {} -> commitTree <$> liftG (readCommit h)
    Atom { atomPath = path, atomContent = suffix } -> do
      parentWt   <- liftG (loadWorkingTree h)
      oldContent <- liftG $ case Map.lookup path parentWt of
        Just (FSFile bh) -> readBlobM bh
        _                -> return BS.empty
      newBlob <- liftG (writeBlobM (oldContent <> TE.encodeUtf8 suffix))
      liftG (flushWorkingTree (Map.insert path (FSFile newBlob) parentWt))
  newHash <- liftG $ writeCommit CommitData
    { commitParents = h : refs
    , commitTree    = treeHash
    , commitMessage = encodeTick t
    }
  putHead newHash
  return newHash

-- | Pop the tick at head, moving head to its parent, and hand back
--   everything it was. @store =<< drop@ rebuilds the same commit in
--   place.
drop :: StoreM m => StoreT m Tick
drop = do
  h  <- headHash
  t  <- liftG (readTick h)
  cd <- liftG (readCommit h)
  case commitParents cd of
    []      -> return ()
    (p : _) -> putHead p
  return t

-- | Move to @target@, run @action@ there, and restore the chain to
--   exactly where it started -- a read-only, isolated peek. Pure
--   navigation: descends by following @commitParents@ directly, with no
--   need for 'drop'\/'store' since nothing this does is meant to last.
--   Fails (via 'MonadFail') if @target@ isn't actually in head's history;
--   an unexceptional caller error, not a normal outcome to branch on.
--   @target@ is resolved once, right here, before the descent -- see
--   'resolveId': a caller may be holding an id from before some earlier
--   edit in this same scope replaced it.
readAt :: StoreM m => ObjectHash -> StoreT m a -> StoreT m a
readAt target0 action = do
  target    <- resolveId target0
  outerHead <- headHash
  result    <- descend target
  putHead outerHead
  return result
  where
    descend target = do
      current <- headHash
      if current == target
        then action
        else do
          cd <- liftG (readCommit current)
          case commitParents cd of
            [] -> fail ("readAt: " <> T.unpack (unObjectHash target) <> " not found in history")
            (parent : _) -> putHead parent >> descend target

-- | Move to @target@, run @action@ there, and replay every later commit
--   back on top of whatever @action@ produced -- a rebase. Built entirely
--   from 'drop' (popping each tail tick on the way down to @target@) and
--   'store' (re-pushing it on the way back up, onto whatever head the
--   recursive call underneath it left behind, logging that old id's own
--   replacement as it goes -- see 'resolveId'\/'logRemap') -- the
--   operations that actually touch the chain; this only orchestrates
--   position and order on top of them. @target@ itself is resolved once
--   before descending, same as 'readAt'; a cross-reference between two
--   tail ticks never goes stale mid-replay either, since 'store' (which
--   every re-push here, and @action@ itself if it edits, eventually
--   calls) resolves a tick's own 'tickRefs' at the point it's actually
--   used, not before.
--
--   Fails (via 'MonadFail') if @target@ isn't actually in head's history,
--   same as 'readAt'.
at :: StoreM m => ObjectHash -> StoreT m a -> StoreT m a
at target0 action = do
  target <- resolveId target0
  go target
  where
    go target = do
      current <- headHash
      if current == target
        then do
          -- @action@ is arbitrary caller code, not necessarily built
          -- from 'editTick'\/'replaceTick' (see 'Storage.CoreSpec'\'s
          -- raw drop\/store actions) -- so it can't be trusted to have
          -- logged its own replacement of @target@ itself. This is the
          -- one entry the tail-recursive case below can't produce on its
          -- own either, since it only ever sees ticks strictly after
          -- @target@. A redundant log entry here (when @action@ *did* go
          -- through 'editTick') is harmless -- 'logRemap' composes.
          a     <- action
          after <- headHash
          if after /= target then logRemap target after else return ()
          return a
        else do
          cd <- liftG (readCommit current)
          case commitParents cd of
            [] -> fail ("at: " <> T.unpack (unObjectHash target) <> " not found in history")
            (_ : _) -> do
              t   <- drop
              a   <- go target
              new <- store t
              logRemap current new
              return a

-- | Pop the tick at head, apply @f@ to it, and store the result in its
--   place, recording the old->new replacement (see 'resolveId'\/
--   'logRemap'). The one pattern every "edit the tick at head" operation
--   is built from.
editTick :: StoreM m => (Tick -> StoreT m Tick) -> StoreT m ObjectHash
editTick f = do
  old     <- headHash
  oldTick <- drop
  newTick <- f oldTick
  new     <- store newTick
  logRemap old new
  return new

-- | 'editTick' with the replacement given outright rather than derived
--   from what was popped.
replaceTick :: StoreM m => Tick -> StoreT m ObjectHash
replaceTick t = editTick (const (return t))

-- | Set the ambient working tree to match head's own committed content,
--   discarding whatever was there before. The chain itself (head, and
--   everything 'at'\/'readAt' do) is completely untouched -- this only
--   ever moves the ambient tree, never the other way around.
reset :: StoreM m => StoreT m ()
reset = do
  h  <- headHash
  wt <- liftG (loadWorkingTree h)
  putAmbientTree wt

-- | Run @action@ against an ambient tree freshly 'reset' to head's own
--   content, then restore whatever the ambient tree held before @action@
--   ran -- independent of the chain entirely (no 'at'\/'readAt', no head
--   movement): this only ever isolates the *other* piece of state, the
--   one 'reset' and the file operations touch.
inWorktree :: StoreM m => StoreT m a -> StoreT m a
inWorktree action = do
  outerWt <- getAmbientTree
  reset
  a <- action
  putAmbientTree outerWt
  return a

-- ---------------------------------------------------------------------------
-- Ambient file access -- the operations that read or write the
-- ambient tree by path; 'reset'\/'inWorktree' are the only ones that replace
-- or isolate the whole thing.
-- ---------------------------------------------------------------------------

-- | Read @path@'s current content from the ambient tree.
readFile :: StoreM m => FilePath -> StoreT m BS.ByteString
readFile path = do
  wt <- getAmbientTree
  case Map.lookup path wt of
    Just (FSFile h) -> liftG (readBlobM h)
    Just FSDir      -> fail (path <> ": is a directory")
    Nothing         -> fail (path <> ": not found")

-- | Write @content@ to @path@ in the ambient tree, creating it (and any
--   ancestor directory entries it needs) if absent.
writeFile :: StoreM m => FilePath -> BS.ByteString -> StoreT m ()
writeFile path content = do
  h <- liftG (writeBlobM content)
  modify $ \(hd, wt, table) ->
    ( hd
    , Map.insert path (FSFile h)
        (foldr (\d m -> Map.insertWith keepExisting d FSDir m) wt (ancestorDirs path))
    , table
    )

-- | Introduce @path@ as an explicit, possibly-empty directory entry in
--   the ambient tree.
createDirectory :: Monad m => FilePath -> StoreT m ()
createDirectory path = modify (\(h, wt, table) -> (h, Map.insertWith keepExisting path FSDir wt, table))

-- | Remove @path@ from the ambient tree.
remove :: Monad m => FilePath -> StoreT m ()
remove path = modify (\(h, wt, table) -> (h, Map.delete path wt, table))

-- | Every file path currently in the ambient tree (directories excluded).
list :: Monad m => StoreT m [FilePath]
list = do
  wt <- getAmbientTree
  return [ p | (p, FSFile _) <- Map.toList wt ]

keepExisting :: FSNode -> FSNode -> FSNode
keepExisting _ old = old

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  let parts = splitDirectories path
  in [ joinPath (take n parts) | n <- [1 .. length parts - 1] ]
