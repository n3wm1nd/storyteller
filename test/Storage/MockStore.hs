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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T

import Storage.Core

data MockState = MockState
  { msObjects :: Map T.Text StoreObject
  , msCommits :: Map T.Text CommitData
  , msNext    :: Int
  }

emptyMock :: MockState
emptyMock = MockState Map.empty Map.empty 0

newtype Mock a = Mock (StateT MockState (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState MockState)

instance MonadFail Mock where
  fail = Mock . lift . Left

freshHash :: String -> Mock ObjectHash
freshHash prefix = do
  n <- gets msNext
  modify (\s -> s { msNext = n + 1 })
  return (ObjectHash (T.pack (prefix <> show n)))

instance MonadStore Mock where
  writeObject obj = do
    h <- freshHash "obj"
    modify (\s -> s { msObjects = Map.insert (unObjectHash h) obj (msObjects s) })
    return h

  readObject h = do
    m <- gets msObjects
    case Map.lookup (unObjectHash h) m of
      Just o  -> return o
      Nothing -> fail ("object not found: " <> T.unpack (unObjectHash h))

  writeCommit cd = do
    h <- freshHash "commit"
    modify (\s -> s { msCommits = Map.insert (unObjectHash h) cd (msCommits s) })
    return h

  readCommit h = do
    m <- gets msCommits
    case Map.lookup (unObjectHash h) m of
      Just cd -> return cd
      Nothing -> fail ("commit not found: " <> T.unpack (unObjectHash h))

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
runChain :: StoreT Mock a -> Either String (a, (ObjectHash, WorkingTree))
runChain action = runMockGit $ do
  emptyTreeHash <- writeObject (TreeObject [])
  rootHash <- writeCommit CommitData
    { commitParents = []
    , commitTree    = emptyTreeHash
    , commitMessage = "type:root\n"
    }
  runStoreT rootHash action
