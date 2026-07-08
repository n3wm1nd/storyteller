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
--   * 'reset'\/'inWorktree'\/'readFile'\/'writeFile' -- the only
--     operations here that touch the ambient tree (the directory\/listing
--     operations -- 'createDirectory'\/'remove'\/'removeRecursive'\/'list'\/
--     'isDirectory'\/'listChildren' -- live in "Storage.FS", built on the
--     'getAmbientTree'\/'modifyAmbientTree' seam exported below).
--     'reset' sets it to match head's own committed content, discarding
--     whatever was there before. 'inWorktree' runs an action against a
--     freshly-'reset' tree, then restores whatever the ambient tree held
--     before -- independent of the chain entirely, since nothing here
--     moves head. 'readFile'\/'writeFile' read or write it by path.
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
  , loadWorkingTree

    -- * The monad
  , StoreT
  , ScopeState
  , freshScope
  , runStoreT
  , runStoreTFrom
  , headHash


    -- * Chain contents
  , Tick(..)
  , readTick
  , removedTagKey
  , isRemoval

    -- * Core operations
  , store
  , drop
  , readAt
  , at
  , atWith
  , editTick
  , replaceTick
  , resolveId
  , logRemap
  , follow
  , memoFold
  , syncTo
  , reset
  , inWorktree

    -- * Ambient file access
  , readFile
  , writeFile

    -- * Ambient tree state (seam for "Storage.FS")
  , getAmbientTree
  , modifyAmbientTree
  ) where

import Prelude hiding (drop, readFile, writeFile)
import qualified Data.List as List

import Control.Monad.State.Strict
  (StateT(..), MonadState, MonadTrans, gets, lift, runStateT, modify)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
--
--   'atomTags' carries whatever else was in the header block besides
--   @file@ -- e.g. @("hide", "true")@ -- permanently attached to the tick
--   itself (unlike a 'NonAtom' \"note\", which merely references another
--   tick from its own place in the chain). Safe to round-trip even
--   pathological content: content only ever starts after the *first*
--   @"\\n\\n"@, and every header line is one we generate ourselves as a
--   single @key:value@ line with no embedded newline, so that boundary
--   can never be confused with anything inside 'atomContent' itself.
--
--   'Binary' and 'Opaque' are two different answers to "content this
--   module never diffs as prose," for two different origins:
--
--   * 'Binary' is for content *we* deliberately introduce at a known
--     path (a character portrait upload, some future asset API) --
--     it knows its own 'binaryPath', the same way 'Atom' knows
--     'atomPath', it just never embeds its bytes as message text; they're
--     spliced into the tree from whatever's currently ambient at that
--     path (see "Storage.Ops"), replaced wholesale each time rather than
--     appended to.
--   * 'Opaque' is for content *we didn't* introduce at all -- a whole
--     git commit made some other way (the CLI, a repo adopted wholesale
--     as a StoryStorage branch), which could touch any number of paths we
--     have no way of enumerating and no reason to. It carries no path
--     because there isn't *a* path -- 'store'\'s own handling for it just
--     adopts whatever's in the ambient tree wholesale, without asking
--     which parts of it changed.
--
--   Neither is read back or verified by this module -- once content is
--   'Binary' or came in as 'Opaque', it's trusted, not policed: there's
--   nothing this module could sensibly do about a conflict even if it
--   noticed one.
data Tick
  = Atom    { tickRefs :: [ObjectHash], atomPath :: FilePath, atomTags :: [(Text, Text)], atomContent :: Text }
  | NonAtom { tickRefs :: [ObjectHash], nonAtomMessage :: Text }
  | Binary  { tickRefs :: [ObjectHash], binaryPath :: FilePath }
  | Opaque  { tickRefs :: [ObjectHash] }
  deriving (Show, Eq)

atomTag :: Text
atomTag = "type:atom\n"

-- | The 'atomTags' key marking an atom as a whole-file deletion event,
--   same convention as the existing @"hide"@ tag: metadata carried on an
--   otherwise ordinary, empty 'Atom', not a distinct 'Tick' constructor.
--   'store' reads it to *remove* 'atomPath' from the tree instead of
--   splicing content into it. This is what 'store' actually persists of
--   'Storage.Ops.deleteFile's own action -- it doesn't inspect any
--   ambient/ephemeral filesystem state to decide the tree, it commits the
--   one fact that already happened. That's also what makes replay correct:
--   popping this tick and re-'store'-ing it (any rebase elsewhere in the
--   branch's single linear chain can do that, since ticks interleave
--   across every file) must reproduce the same deletion every time, the
--   same way replaying an ordinary atom reproduces the same appended
--   content every time -- an edit made earlier in the timeline, replayed
--   forward past this tick, correctly deletes the file again, exactly as
--   it did in the original history. Kept alongside
--   'atomTag'\/'binaryTag'\/'opaqueTag' since, like them, it's part of the
--   wire encoding this module owns.
removedTagKey :: Text
removedTagKey = "removed"

isRemoval :: [(Text, Text)] -> Bool
isRemoval tags = lookup removedTagKey tags == Just "true"

binaryTag :: Text
binaryTag = "type:binary\n"

opaqueTag :: Text
opaqueTag = "type:opaque\n"

encodeTick :: Tick -> Text
encodeTick (NonAtom _ msg) = msg
encodeTick (Atom _ path tags content) =
  T.concat (map (\(k, v) -> k <> ":" <> v <> "\n") (("file", T.pack path) : tags)) <> "\n" <> atomTag <> content
encodeTick (Binary _ path) = "file:" <> T.pack path <> "\n\n" <> binaryTag
encodeTick (Opaque _) = opaqueTag

-- | An atom, if @raw@ has the shape 'encodeTick' gives one: a @file@
--   field, a blank line, then the @"type:atom\\n"@ tag. Anything else
--   (no fields at all, a different tag, fields without that one) isn't
--   an atom this module recognises -- 'decodeTick' falls back to
--   'NonAtom' for all of it, verbatim.
decodeAtom :: Text -> Maybe (FilePath, [(Text, Text)], Text)
decodeAtom raw = do
  let (headerBlock, rest0) = T.breakOn "\n\n" raw
  body <- if T.null rest0 then Nothing else Just (T.drop 2 rest0)
  payload <- T.stripPrefix atomTag body
  let fields = [ (k, T.drop 1 v)
               | l <- T.lines headerBlock
               , let (k, v) = T.breakOn ":" l
               , not (T.null v) ]
  path <- lookup "file" fields
  let tags = filter ((/= "file") . fst) fields
  return (T.unpack path, tags, payload)

-- | A binary marker, if @raw@ has the shape 'encodeTick' gives one: a
--   @file@ field, a blank line, the @"type:binary\\n"@ tag, and *nothing*
--   after it -- the payload must be empty, since 'Binary' never carries
--   its content in the message the way 'Atom' does. A message that merely
--   starts with that tag but goes on to say more isn't this module's own
--   'Binary' encoding (could be an app-level 'NonAtom' kind that happens
--   to reuse the word), so it's correctly left to fall through instead.
decodeBinary :: Text -> Maybe FilePath
decodeBinary raw = do
  let (headerBlock, rest0) = T.breakOn "\n\n" raw
  body <- if T.null rest0 then Nothing else Just (T.drop 2 rest0)
  payload <- T.stripPrefix binaryTag body
  if T.null payload then T.unpack <$> lookup "file" fields else Nothing
  where
    fields = [ (k, T.drop 1 v)
             | l <- T.lines (fst (T.breakOn "\n\n" raw))
             , let (k, v) = T.breakOn ":" l
             , not (T.null v) ]

decodeTick :: [ObjectHash] -> Text -> Tick
decodeTick refs raw
  | raw == opaqueTag = Opaque refs
  | otherwise = case decodeBinary raw of
      Just path -> Binary refs path
      Nothing -> case decodeAtom raw of
        Just (path, tags, content) -> Atom refs path tags content
        Nothing                     -> NonAtom refs raw

-- | Read and decode the tick held by the commit at a given hash.
readTick :: StoreM m => ObjectHash -> m Tick
readTick h = do
  cd <- readCommit h
  return (decodeTick (List.drop 1 (commitParents cd)) (commitMessage cd))

-- ---------------------------------------------------------------------------
-- The monad: the current position in the chain, plus an ambient working
-- tree that's entirely independent of it -- 'at'\/'readAt' only ever move
-- through the chain and never touch this; 'reset'\/'inWorktree' and the
-- file operations ('readFile'\/'writeFile' here; 'createDirectory'\/
-- 'remove'\/'list'\/... in "Storage.FS") are the only ones that do.
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

-- | The scope state a freshly-opened branch starts in: head at @h@, the
--   ambient tree synced to it (as if 'reset' had just run), and an empty
--   remap table.
freshScope :: StoreM m => ObjectHash -> m ScopeState
freshScope h = do
  wt <- loadWorkingTree h
  return (h, wt, Map.empty)

-- | Run a 'StoreT' computation seeded fresh at the given head -- see
--   'freshScope'.
runStoreT :: StoreM m => ObjectHash -> StoreT m a -> m (a, ScopeState)
runStoreT h action = freshScope h >>= \seed -> runStoreTFrom seed action

-- | Run a 'StoreT' computation resuming from a previously-captured
--   'ScopeState' rather than reloading fresh -- for a caller making
--   several separate dispatches against the same branch scope (e.g. one
--   per 'Storyteller.Core.Branch.BranchOp' effect operation) that needs
--   the ambient tree's own pending, uncommitted edits (and the remap
--   table) to survive between them, not just head. A caller that doesn't
--   need that continuity can just always call 'runStoreT' instead.
runStoreTFrom :: ScopeState -> StoreT m a -> m (a, ScopeState)
runStoreTFrom seed (StoreT s) = runStateT s seed

liftG :: Monad m => m a -> StoreT m a
liftG = lift

headHash :: Monad m => StoreT m ObjectHash
headHash = gets (\(h, _, _) -> h)

putHead :: Monad m => ObjectHash -> StoreT m ()
putHead h = modify (\(_, wt, table) -> (h, wt, table))

-- | The ambient working tree 'readFile'\/'writeFile' (and, via
--   "Storage.FS", 'createDirectory'\/'remove'\/'list'\/...) operate on.
--   Exported only as the read seam for "Storage.FS"; nothing else outside
--   this module needs it -- 'reset'\/'inWorktree' replace or isolate the
--   whole thing, the file operations touch it by path.
getAmbientTree :: Monad m => StoreT m WorkingTree
getAmbientTree = gets (\(_, wt, _) -> wt)

putAmbientTree :: Monad m => WorkingTree -> StoreT m ()
putAmbientTree wt = modify (\(h, _, table) -> (h, wt, table))

-- | Apply @f@ to the ambient working tree -- the write seam "Storage.FS"'s
--   own ambient file operations are built on, so they needn't reach into
--   'ScopeState' themselves. Like 'getAmbientTree', exported only for that
--   sibling module.
modifyAmbientTree :: Monad m => (WorkingTree -> WorkingTree) -> StoreT m ()
modifyAmbientTree f = modify (\(h, wt, table) -> (h, f wt, table))

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
--   chain at every read. 'editTick'\/'replaceTick'\/'at'\'s own tail-replay
--   already call this for the common case (one tick replaced by exactly
--   one other); exported for an @action@ passed to 'at' that produces
--   more than one successor and needs to say which one @target@ itself
--   becomes, since 'at'\'s own generic fallback can't guess that (see its
--   Haddock).
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
--
--   'Atom' is the one constructor whose commit actually splices the tree,
--   in one of two directions depending on 'atomTags': ordinarily it
--   inserts (its own current fold-of-atoms content, extended by this
--   tick's own suffix); when tagged with 'removedTagKey' (see
--   'isRemoval'), it deletes 'atomPath' from the tree instead. Both are
--   plain forward commits -- head only ever moves to a new tick built on
--   top of the current one, nothing upstream is read back and rewritten --
--   which is what makes a whole-file delete safe to record as an ordinary
--   tick rather than a rebase: every earlier atom for that path is
--   untouched, so a 'Storage.Core.at'/'readAt' to any tick before the
--   deletion still sees the file exactly as it was. A rebase-based delete
--   (physically dropping those earlier ticks from the chain, as this
--   module's own chain-editing primitives like 'Storage.Ops.deleteTick' do
--   for correcting a single misplaced atom) would instead erase that
--   history outright -- fine for "this atom was a mistake," wrong for "the
--   file existed and was later deleted."
store :: StoreM m => Tick -> StoreT m ObjectHash
store t = do
  h    <- headHash
  refs <- mapM resolveId (tickRefs t)
  treeHash <- case t of
    NonAtom {} -> commitTree <$> liftG (readCommit h)
    Atom { atomPath = path, atomTags = tags } | isRemoval tags -> do
      parentWt <- liftG (loadWorkingTree h)
      liftG (flushWorkingTree (Map.delete path parentWt))
    Atom { atomPath = path, atomContent = suffix } -> do
      parentWt   <- liftG (loadWorkingTree h)
      oldContent <- liftG $ case Map.lookup path parentWt of
        Just (FSFile bh) -> readBlobM bh
        _                -> return BS.empty
      newBlob <- liftG (writeBlobM (oldContent <> TE.encodeUtf8 suffix))
      liftG (flushWorkingTree (Map.insert path (FSFile newBlob) parentWt))
    Binary { binaryPath = path } -> do
      -- Unlike Atom, a Binary tick carries no content field of its own --
      -- whatever's currently in the *ambient* tree at this path (already
      -- written there by the caller, e.g. via 'writeFile') is what gets
      -- committed, verbatim, replacing whatever was there before.
      parentWt  <- liftG (loadWorkingTree h)
      ambientWt <- getAmbientTree
      case Map.lookup path ambientWt of
        Just node -> liftG (flushWorkingTree (Map.insert path node parentWt))
        Nothing   -> liftG (flushWorkingTree parentWt)
    Opaque {} ->
      -- No path to splice -- adopt the whole ambient tree as-is. Safe
      -- for every already-atom-tracked path too: by the time anything
      -- calls this (see 'Storage.Ops.commitWorktree'), every such path's
      -- ambient content already matches what its own reconciliation just
      -- committed, so re-flushing the whole tree changes nothing there.
      liftG . flushWorkingTree =<< getAmbientTree
  newHash <- liftG $ writeCommit CommitData
    { commitParents = h : refs
    , commitTree    = treeHash
    , commitMessage = encodeTick t
    }
  putHead newHash
  return newHash

-- | Every path where @commitHash@'s own tree differs from @parentHash@'s
--   -- @Just node@ for an added\/changed entry, @Nothing@ for one removed.
--   This is a tick's own, exact tree contribution, independent of
--   anything ambient -- the piece 'store's own 'Binary'\/'Opaque' cases
--   can't recover on replay (see 'atWith's own Haddock for why they need
--   it precomputed rather than re-derived from ambient state).
treeDelta :: StoreM m => ObjectHash -> ObjectHash -> StoreT m (Map FilePath (Maybe FSNode))
treeDelta commitHash parentHash = do
  cur <- liftG (loadWorkingTree commitHash)
  par <- liftG (loadWorkingTree parentHash)
  return $ Map.fromList
    [ (p, Map.lookup p cur)
    | p <- Set.toList (Map.keysSet cur `Set.union` Map.keysSet par)
    , Map.lookup p cur /= Map.lookup p par
    ]

-- | Commit @t@ with its tree built by applying @delta@ (see 'treeDelta')
--   onto the *current* head's own tree, instead of 'store's usual
--   per-constructor computation. Used only by 'atWith's replay of a
--   'Binary'\/'Opaque' tick: reapplies that tick's own original
--   contribution onto the rebuilt parent, rather than re-deriving it from
--   whatever the ambient tree happens to hold now.
storeWithDelta :: StoreM m => Tick -> Map FilePath (Maybe FSNode) -> StoreT m ObjectHash
storeWithDelta t delta = do
  h    <- headHash
  refs <- mapM resolveId (tickRefs t)
  parentWt <- liftG (loadWorkingTree h)
  let apply wt (p, Just node) = Map.insert p node wt
      apply wt (p, Nothing)   = Map.delete p wt
      newWt = foldl apply parentWt (Map.toList delta)
  treeHash <- liftG (flushWorkingTree newWt)
  newHash  <- liftG $ writeCommit CommitData
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
--   exactly where it started -- a read-only, isolated peek. Unlike 'at',
--   nothing here is ever replayed (no tail to rebuild), so there's no
--   need to walk there one parent at a time either -- just jump straight
--   to it. @target@ is resolved once first -- see 'resolveId': a caller
--   may be holding an id from before some earlier edit in this same
--   scope replaced it. Valid for *any* commit this store can read, not
--   just an ancestor of the current head -- a target that was never
--   written at all only surfaces as a failure if @action@ actually
--   dereferences something derived from it (e.g. 'readCommit'), not
--   proactively; real callers only ever pass a 'resolveId'-backed or
--   otherwise legitimate id.
readAt :: StoreM m => ObjectHash -> StoreT m a -> StoreT m a
readAt target0 action = do
  target    <- resolveId target0
  outerHead <- headHash
  putHead target
  result    <- action
  putHead outerHead
  return result

-- | Every tick reachable from head, read-only and state-free -- never
--   moves head, never calls 'drop'\/'store', just follows @commitParents@
--   by reading commits directly. @step@ folds each tick into an
--   accumulator, and decides whether to keep going -- the primary parent
--   is always the next stop (there's only ever one meaningful "backward"
--   in a linear chain; unlike 'StorageMonad''s 'Tick', this module's own
--   'Tick' carries no position of its own, so @step@ gets the hash
--   alongside it rather than having to dig it out). Stops when @step@
--   says so, or when the root (no parent) is reached, whichever first.
follow :: StoreM m => b -> (b -> ObjectHash -> Tick -> (b, Bool)) -> StoreT m b
follow seed step = headHash >>= \h -> liftG (walk seed h)
  where
    walk acc h = do
      t <- readTick h
      let (acc', continue) = step acc h t
      if not continue
        then return acc'
        else do
          cd <- readCommit h
          case commitParents cd of
            []      -> return acc'
            (p : _) -> walk acc' p

-- | Fold from head back to whichever cached ancestor is nearest (or all the
--   way to root, if none is cached), then combine forward from there back up
--   to head -- a memoized 'follow'. Given a small cache of already-known
--   (hash, accumulated-value) checkpoints (from a previous call), the walk
--   stops the instant it reaches a checkpointed hash, since content-
--   addressing guarantees everything below that hash -- and whatever was
--   already folded into it -- is provably unchanged; there is nothing left
--   to verify by walking further. The common case (head has advanced by one
--   tick since the last call) costs one step; a rebase costs one step per
--   replayed tick -- the same cost the replay itself already pays at the
--   storage layer, not an extra one this adds on top.
--
--   @combine acc h t@ folds one more tick (@h@, its data @t@) onto whatever
--   @acc@ already represents -- monadic in 'StoreT', not a plain
--   'Data.List.foldl'' step, since a real fold step often needs more than
--   @t@'s own content: following one of @t@'s cross-branch refs, jumping
--   elsewhere with 'at'\/'readAt', or reading the ambient tree all need
--   'StoreT'. It never has to think about the cache, though -- that
--   bookkeeping is entirely this function's job. @seed@ is @combine@'s value
--   below root, used only when the walk never hits a checkpoint and reaches
--   root instead.
--
--   Returns the folded value at head, plus the *next* cache to pass back in
--   on the following call -- callers never construct or merge this
--   themselves, they just thread whatever comes back through. It holds
--   @O(log k)@ checkpoints (@k@ = how many ticks were newly walked this
--   time), at exponentially doubling distance back from head (head itself,
--   one before it, three before, seven before, ...) rather than only the
--   single most recent position: that keeps a future call's walk short even
--   when the next head isn't a simple one-tick advance on this one (a
--   multi-tick append, or a jump to a different but nearby position),
--   without the cache ever growing past @O(log k)@ entries. When nothing
--   new was walked (head was already a checkpoint), the input cache is
--   handed back unchanged -- there is nothing new to checkpoint.
memoFold
  :: StoreM m
  => (b -> ObjectHash -> Tick -> StoreT m b)
  -> b
  -> [(ObjectHash, b)]
  -> StoreT m (b, [(ObjectHash, b)])
memoFold combine seed cache = do
  h            <- headHash
  (base, path) <- collect h []
  if null path
    then return (base, cache)
    else do
      scanned <- scanForward base path
      let final       = last scanned
          k           = length path
          distances   = takeWhile (<= k) (iterate (* 2) 1)
          checkpoints = [ (fst (path !! (k - d)), scanned !! (k - d + 1)) | d <- distances ]
      return (final, checkpoints)
  where
    cacheMap = Map.fromList cache

    -- Walks backward from @cur@, prepending each newly-seen tick onto
    -- @acc@ as it goes -- since each recursive call prepends *before*
    -- descending further, the final list comes back oldest-first (the
    -- tick nearest the checkpoint hit\/root first, head's own tick last),
    -- exactly the order 'scanForward' needs to replay them in.
    collect cur acc = case Map.lookup cur cacheMap of
      Just cachedVal -> return (cachedVal, acc)
      Nothing -> do
        t  <- liftG (readTick cur)
        cd <- liftG (readCommit cur)
        case commitParents cd of
          []      -> return (seed, (cur, t) : acc)
          (p : _) -> collect p ((cur, t) : acc)

    -- Monadic scanl: [base, combine base .., combine (that) .., ...],
    -- length = length path + 1, so the value at index (i+1) is what @acc@
    -- becomes after folding in @path !! i@.
    scanForward acc [] = return [acc]
    scanForward acc ((h, t) : rest) = do
      acc' <- combine acc h t
      more <- scanForward acc' rest
      return (acc : more)

-- | Move head to @target@ (resolved first, same as 'readAt') and reset
--   the ambient tree to match -- unlike 'readAt', this doesn't restore
--   anything afterward, and unlike 'at', nothing is replayed: a plain
--   jump to wherever the branch's ref now actually points (e.g. after
--   some other scope published a new head), same as if this scope had
--   just been freshly opened there.
syncTo :: StoreM m => ObjectHash -> StoreT m ()
syncTo target0 = do
  target <- resolveId target0
  putHead target
  reset

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
--
--   @'at' = 'atWith' 'id'@ -- see 'atWith' for the one caller
--   (@'Storage.Ops.renameFile'@) that needs the tail's own replay to do
--   more than reproduce each tick verbatim.
at :: StoreM m => ObjectHash -> StoreT m a -> StoreT m a
at = atWith id

-- | 'at', generalized with @onReplay@: applied to every tail tick just
--   before it's re-stored on the way back up, instead of replaying it
--   verbatim. @action@ itself (typically 'editTick') is still the only
--   thing that touches @target@ -- @onReplay@ only ever sees ticks
--   strictly after it, the same scope 'at's own Haddock already describes
--   for the tail.
--
--   The one caller that needs this: 'Storage.Ops.renameFile' renames a
--   file by editing just its creation tick's own @atomPath@ via @action@,
--   then needs every later atom on the old path -- still carrying the old
--   name in its own recorded data -- to pick up the same new name as it
--   replays, or the rename would only ever take effect for that one tick.
--   Plain 'at' has no hook for that; every other caller here still gets
--   exactly 'at's own behavior by passing 'id'.
atWith :: StoreM m => (Tick -> Tick) -> ObjectHash -> StoreT m a -> StoreT m a
atWith onReplay target0 action = do
  target <- resolveId target0
  go target
  where
    go target = do
      current <- headHash
      if current == target
        then do
          -- @action@ is arbitrary caller code, not necessarily built
          -- from 'editTick'\/'replaceTick' (see 'Storage.CoreSpec'\'s raw
          -- drop\/store actions) -- so it can't be trusted to have logged
          -- its own replacement of @target@ itself. This is the one entry
          -- the tail-recursive case below can't produce on its own
          -- either, since it only ever sees ticks strictly after @target@.
          --
          -- Only fires when @after@ looks exactly like "@target@, dropped
          -- and something re-stored in its place" -- @after@'s own parent
          -- is @target@'s *original* parent (captured before @action@
          -- runs, in case @target@ itself doesn't survive it). Plain
          -- @after \/= target@ isn't enough: a pure append (@action = store
          -- newThing@ with no 'drop' at all, e.g. 'emitStandaloneGap') also
          -- moves head away from @target@, but @target@ is still valid,
          -- untouched, sitting right below the new tick -- @after@'s
          -- parent is @target@ itself there, not @target@'s parent, so
          -- this check correctly leaves it alone. A composite action (one
          -- @target@ producing several successors, or an outer 'at' whose
          -- action nests further 'at'\/'drop'\/'store' calls of its own --
          -- 'moveTick'\/'mergeAtoms'\/'splitTick') won't match either
          -- shape; those explicitly call 'logRemap' themselves for exactly
          -- this reason, and are always checked here via a guard
          -- ('resolveId target' no longer being @target@) that skips this
          -- fallback once they have.
          targetParent <- liftG (parentOf target)
          a            <- action
          after        <- headHash
          afterParent  <- liftG (parentOf after)
          stillUnmapped <- (== target) <$> resolveId target
          if after /= target && afterParent == targetParent && stillUnmapped
            then logRemap target after
            else return ()
          return a
        else do
          cd <- liftG (readCommit current)
          case commitParents cd of
            [] -> fail ("at: " <> T.unpack (unObjectHash target) <> " not found in history")
            (par : _) -> do
              -- 'Binary'/'Opaque' read the *ambient* tree to decide their
              -- own tree contribution (see 'store's own Haddock) -- fine
              -- for a fresh commit, wrong on replay, where ambient state
              -- has nothing to do with the rebase in progress. Their exact
              -- contribution has to be captured here, from the commits
              -- themselves, before 'drop' discards this position -- then
              -- reapplied via 'storeWithDelta' once the parent's replay
              -- below has rebuilt whatever it rebuilds onto.
              t0    <- liftG (readTick current)
              delta <- case t0 of
                Binary {} -> Just <$> treeDelta current par
                Opaque {} -> Just <$> treeDelta current par
                _         -> return Nothing
              t   <- drop
              a   <- go target
              new <- case delta of
                Just d  -> storeWithDelta (onReplay t) d
                Nothing -> store (onReplay t)
              logRemap current new
              return a

-- | @h@'s own parent, if it has one -- 'Nothing' at the root. Used only
--   to compare "did this tick's parent change" without caring what the
--   parent actually is.
parentOf :: StoreM m => ObjectHash -> m (Maybe ObjectHash)
parentOf h = do
  cd <- readCommit h
  case commitParents cd of
    (p : _) -> return (Just p)
    []      -> return Nothing

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
-- ambient tree by path. Only 'readFile'\/'writeFile' (which need the
-- object store for blob I/O) live here; the directory\/listing operations
-- ('createDirectory'\/'remove'\/'removeRecursive'\/'list'\/'isDirectory'\/
-- 'listChildren') are in "Storage.FS", built on 'getAmbientTree'\/
-- 'modifyAmbientTree'. 'reset'\/'inWorktree' are the only ones that
-- replace or isolate the whole thing.
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

keepExisting :: FSNode -> FSNode -> FSNode
keepExisting _ old = old

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  let parts = splitDirectories path
  in [ joinPath (take n parts) | n <- [1 .. length parts - 1] ]
