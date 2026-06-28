{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure in-memory mock interpreter for the Git effect.
--   All objects are content-addressed, exactly as in real git: writing the
--   same content twice returns the same hash.  This means a no-op replayTail
--   (e.g. the tail of a pure-read At) produces the same commit hashes and
--   leaves branch refs unchanged.
module Git.Mock
  ( GitState(..)
  , emptyGitState
  , runGitMock
  ) where

import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Hashable (hash, Hashable)
import Data.Word (Word64)
import Numeric (showHex)

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git

emptyTreeHash :: ObjectHash
emptyTreeHash = ObjectHash "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

data GitState = GitState
  { gsRefs     :: Map RefName ObjectHash
  , gsCommits  :: Map ObjectHash CommitData
  , gsObjects  :: Map ObjectHash GitObject
  } deriving (Show, Eq)

emptyGitState :: GitState
emptyGitState = GitState Map.empty Map.empty Map.empty

toHex :: Int -> String
toHex x = showHex (fromIntegral x :: Word64) ""

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

runGitMock
  :: Members '[State GitState, Fail] r
  => Sem (Git : r) a
  -> Sem r a
runGitMock = interpret $ \case
  ResolveRef name -> do
    s <- get
    return $ Map.lookup name (gsRefs s)

  CreateRef name h -> do
    s <- get
    put s { gsRefs = Map.insert name h (gsRefs s) }

  UpdateRef name h -> do
    s <- get
    put s { gsRefs = Map.insert name h (gsRefs s) }

  DeleteRef name -> do
    s <- get
    put s { gsRefs = Map.delete name (gsRefs s) }

  ListRefs prefix -> do
    s <- get
    let matching = Map.toList $ Map.filterWithKey
          (\(RefName k) _ -> prefix `T.isPrefixOf` k)
          (gsRefs s)
    return matching

  ReadCommit h -> do
    s <- get
    case Map.lookup h (gsCommits s) of
      Just cd -> return cd
      Nothing -> fail $ "ReadCommit: unknown hash " <> T.unpack (unObjectHash h)

  WriteCommit cd -> do
    let h = commitContentHash cd
    s <- get
    put s { gsCommits = Map.insert h cd (gsCommits s) }
    return h

  ReadObject h -> do
    s <- get
    case Map.lookup h (gsObjects s) of
      Just obj -> return obj
      Nothing
        | h == emptyTreeHash -> return (TreeObject [])
        | otherwise -> fail $ "ReadObject: unknown hash " <> T.unpack (unObjectHash h)

  WriteObject obj -> do
    let h = objectContentHash obj
    s <- get
    put s { gsObjects = Map.insert h obj (gsObjects s) }
    return h

  LookupPath h path -> do
    s <- get
    case Map.lookup h (gsObjects s) of
      Nothing -> fail $ "LookupPath: unknown tree hash " <> T.unpack (unObjectHash h)
      Just (TreeObject entries) -> return $ entryHash <$> find (\e -> entryName e == path) entries
      Just (BlobObject _)       -> fail $ "LookupPath: hash is a blob: " <> T.unpack (unObjectHash h)

-- ---------------------------------------------------------------------------
-- Content hashing
-- ---------------------------------------------------------------------------

-- Each object type uses a distinct prefix so hashes from different domains
-- never collide even if the underlying Hashable values happen to be equal.

objectContentHash :: GitObject -> ObjectHash
objectContentHash (BlobObject bs) =
  ObjectHash $ "blob:" <> T.pack (toHex (hash bs))
objectContentHash (TreeObject entries) =
  ObjectHash $ "tree:" <> T.pack (toHex (hash (map hashEntry entries)))
  where
    hashEntry e = hash (entryName e, unObjectHash (entryHash e))

commitContentHash :: CommitData -> ObjectHash
commitContentHash cd =
  ObjectHash $ "commit:" <> T.pack (toHex (hash key))
  where
    key = ( map unObjectHash (commitParents cd)
          , unObjectHash (commitTree cd)
          , commitMessage cd
          )
