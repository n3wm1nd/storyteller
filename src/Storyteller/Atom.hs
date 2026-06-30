{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Atom: the file-append tick kind.
--
-- An 'Atom' is a tick whose tree snapshot contains a file append. It carries
-- the tree hash (stored in 'tickFields' as @"tree"@) and a file hint
-- (@"file"@ field) for fast filtering. The tree is the source of truth;
-- the file hint avoids tree diffing in the common case.
--
-- 'AtomDiff' is the construction-time representation. Use 'storeAtomDiff'
-- to build the git tree and return an 'Atom' ready to be committed via
-- 'storeAs'. This path requires only 'Git' — no filesystem effects.
--
-- Use 'treeRef' to get the tree hash from any 'TickId', and 'headAtomDiff'
-- (combined with 'at') to read the diff at any position.
module Storyteller.Atom
  ( -- * Types
    Atom(..)
  , AtomDiff(..)

    -- * Build tree
  , storeAtomDiff

    -- * Read
  , treeRef
  , headAtomDiff
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Polysemy
import Polysemy.Fail

import Runix.Git
  ( Git, ObjectHash(..), readBlob, readCommit, writeBlob, writeTree
  , CommitData(..), TreeEntry(..)
  )

import Storyteller.Git (BranchTag, FSNode(..), loadWorkingTree, loadTree)
import Storyteller.Storage (StoryBranch, follow)
import Storyteller.Types
  ( TickData(..), Tick(..), TickType(..), TickId(..)
  , tickId, tickParent, tickTypeOf
  , encodeDraft, decodePayload
  )

import Prelude

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | The stored representation of an atom tick.
--   'atomTree' is the git tree hash (stored in 'tickFields' as @"tree"@).
--   The file path and content are not stored here — query via 'headAtomDiff'.
data Atom = Atom
  { atomFile    :: FilePath   -- ^ hint: which file this atom touched
  , atomTree    :: ObjectHash -- ^ the resulting git tree hash
  , atomMessage :: T.Text
  } deriving (Show, Eq)

-- | Construction-time representation: the file and bytes to append.
data AtomDiff = AtomDiff
  { atomDiffFile    :: FilePath
  , atomDiffContent :: BS.ByteString
  } deriving (Show, Eq)

instance TickType Atom where
  tickTypeName = "atom"

  toDraft (Atom file tree msg) = encodeDraft @Atom []
    [ ("tree", unObjectHash tree)
    , ("file", T.pack file)
    ]
    msg

  fromTick t = do
    msg  <- decodePayload @Atom t
    tree <- lookup "tree" (tickFields (tickData t))
    file <- lookup "file" (tickFields (tickData t))
    Just Atom
      { atomFile    = T.unpack file
      , atomTree    = ObjectHash tree
      , atomMessage = msg
      }

-- ---------------------------------------------------------------------------
-- Build tree
-- ---------------------------------------------------------------------------

-- | Apply 'AtomDiff' onto a parent tree, write the resulting blob and tree
--   to git, and return an 'Atom' ready to commit via 'storeAs'.
--   Requires only 'Git' — no filesystem effects or branch state.
storeAtomDiff
  :: Members '[Git, Fail] r
  => ObjectHash  -- ^ parent tree hash to apply the diff against
  -> AtomDiff
  -> Sem r Atom
storeAtomDiff parentTree (AtomDiff path content) = do
  -- Read parent content for this file (empty if not present).
  parentWt     <- loadTree parentTree
  parentContent <- case Map.lookup path parentWt of
    Just (FSFile h) -> readBlob h
    _               -> return BS.empty

  -- Write new blob (parent content + appended bytes).
  newHash <- writeBlob (parentContent <> content)

  -- Rebuild the tree with the new blob, preserving all other entries.
  let newWt = Map.insert path (FSFile newHash) parentWt
  treeHash <- buildTree newWt

  let msg = T.take 60 (TE.decodeUtf8With TE.lenientDecode content)
  return Atom { atomFile = path, atomTree = treeHash, atomMessage = msg }

-- ---------------------------------------------------------------------------
-- Read
-- ---------------------------------------------------------------------------

-- | Get the git tree hash for a tick id (which is a commit hash).
treeRef :: Members '[Git, Fail] r => TickId -> Sem r ObjectHash
treeRef tid = commitTree <$> readCommit (ObjectHash (unTickId tid))

-- | Read the 'AtomDiff' that HEAD introduced for @path@.
--   Uses the @"file"@ hint from 'tickFields' to verify the tick touches @path@.
--   Fails if HEAD is not an atom tick or does not touch @path@.
--
--   Combine with 'at' to read any historical position:
--
-- @
--   (diff, _) <- at tid $ headAtomDiff \@branch path
-- @
headAtomDiff
  :: forall branch r
  .  Members '[StoryBranch branch, Git, Fail] r
  => FilePath -> Sem r AtomDiff
headAtomDiff path = do
  headTick <- follow @branch Nothing $ \acc t ->
    case acc of { Just _ -> (acc, Nothing); Nothing -> (Just t, Nothing) }
  tick <- maybe (fail "headAtomDiff: empty branch") return headTick

  case tickTypeOf tick of
    Just kind | kind /= "atom" ->
      fail $ "headAtomDiff: HEAD is not an atom (kind: " <> T.unpack kind <> ")"
    _ -> return ()

  let commitHash = ObjectHash (unTickId (tickId tick))
  cd <- readCommit commitHash

  thisContent   <- blobAt (commitTree cd) path
  parentContent <- case commitParents cd of
    []      -> return BS.empty
    (p : _) -> readCommit p >>= \pcd -> blobAt (commitTree pcd) path

  return AtomDiff
    { atomDiffFile    = path
    , atomDiffContent = BS.drop (BS.length parentContent) thisContent
    }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Read a file's content from a tree hash. Returns empty if absent.
blobAt :: Members '[Git, Fail] r => ObjectHash -> FilePath -> Sem r BS.ByteString
blobAt treeHash path = do
  wt <- loadTree treeHash
  case Map.lookup path wt of
    Just (FSFile h) -> readBlob h
    _               -> return BS.empty

-- | Build a flat git tree from a WorkingTree map.
--   Only handles files (FSFile entries); directories are implicit.
buildTree :: Members '[Git, Fail] r => Map.Map FilePath FSNode -> Sem r ObjectHash
buildTree wt = do
  let entries = [ BlobEntry path hash | (path, FSFile hash) <- Map.toList wt ]
  writeTree entries
