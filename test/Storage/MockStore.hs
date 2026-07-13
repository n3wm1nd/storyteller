{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A minimal in-memory 'MonadStore' mock, shared by "Storage.CoreSpec"
--   and "Storage.OpsSpec" -- just enough to exercise "Storage.Core"'s
--   primitives without a real content-addressed backend.
module Storage.MockStore
  ( Mock
  , runMockGit
  , runChain
  , committedContent
  ) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import Data.Hashable (hash)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Numeric (showHex)
import qualified Data.Text as T

import Storage.Core

data MockState = MockState
  { msObjects :: Map T.Text StoreObject
  , msCommits :: Map T.Text CommitData
  , msRemaps  :: Map ObjectHash ObjectHash
  }

emptyMock :: MockState
emptyMock = MockState Map.empty Map.empty Map.empty

newtype Mock a = Mock (StateT MockState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState MockState)

instance MonadFail Mock where
  fail = Mock . lift . Left

-- | Content-addressed, like any real backend's -- identical content
--   always yields the identical id -- but via a fixed-size digest of the
--   content's own serialization, not the serialization itself. The
--   earlier version used @show@ directly as the id: harmless for the
--   small chains most specs build, but since a commit's id embeds its
--   parent's id verbatim (it's part of 'CommitData'), that made every
--   id's own length grow with chain depth, and a chain of @n@ commits
--   including O(n) 'Map' comparisons against those ids O(n) total work --
--   O(n^2) to build, independent of anything 'Storage.Ops' does. A
--   'hash' (fixed-size regardless of input) keeps every id the same
--   small size no matter how deep the chain gets, the same property a
--   real 40-hex-char git SHA has.
objectKey :: StoreObject -> T.Text
objectKey = hashText . show

commitKey :: CommitData -> T.Text
commitKey = hashText . show

hashText :: String -> T.Text
hashText s = T.pack (showHex (fromIntegral (hash s) :: Word) "")

instance MonadStore Mock where
  writeObject obj = do
    let h = ObjectHash (objectKey obj)
    modify (\s -> s { msObjects = Map.insert (unObjectHash h) obj (msObjects s) })
    return h

  readObject h = do
    m <- gets msObjects
    case Map.lookup (unObjectHash h) m of
      Just o  -> return o
      Nothing -> fail ("object not found: " <> T.unpack (unObjectHash h))

  writeCommit cd = do
    let h = ObjectHash (commitKey cd)
    modify (\s -> s { msCommits = Map.insert (unObjectHash h) cd (msCommits s) })
    return h

  readCommit h = do
    m <- gets msCommits
    case Map.lookup (unObjectHash h) m of
      Just cd -> return cd
      Nothing -> fail ("commit not found: " <> T.unpack (unObjectHash h))

  -- The store owns the remap table (see 'MonadStore'); 'composeMapping'
  -- is the required transitive closure, so a resolve is one lookup.
  resolveHash h = gets (\s -> Map.findWithDefault h h (msRemaps s))
  recordRemap old new =
    modify (\s -> s { msRemaps = composeMapping (msRemaps s) [(old, new)] })

runMockGit :: Mock a -> Either String a
runMockGit (Mock m) = fst <$> runStateT m emptyMock

-- | The chain's committed content for @path@ -- read via 'inWorktree'
--   (which 'reset's the ambient tree to head's own content first), never
--   via 'loadWorkingTree' directly (not exported: nothing outside
--   "Storage.Core" should call it). Test-side verification only, not a
--   primitive.
committedContent :: StoreM m => FilePath -> StoreT m BS.ByteString
committedContent path = inWorktree (readFile path)

-- | Seed a fresh chain (one root commit, an empty tree written the same
--   way any other empty tree would be, holding a plain 'NonAtom' so it
--   decodes like any other tick) and run @action@ from there, returning
--   its result and the final scope state.
runChain :: StoreT Mock a -> Either String (a, ScopeState)
runChain action = runMockGit $ do
  emptyTreeHash <- writeObject (TreeObject [])
  rootHash <- writeCommit CommitData
    { commitParents = []
    , commitTree    = emptyTreeHash
    , commitMessage = "type:root\n"
    }
  runStoreT rootHash action
