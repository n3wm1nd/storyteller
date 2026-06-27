{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Pure in-memory mock interpreter for the Git effect.
--   Uses monotonically increasing integer hashes so commits are
--   distinguishable and deterministic in tests.
module Git.Mock
  ( GitState(..)
  , emptyGitState
  , runGitMock
  ) where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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
  , gsBlobs    :: Map ObjectHash ByteString
  , gsTrees    :: Map ObjectHash [TreeEntry]
  , gsNextId   :: Int
  } deriving (Show, Eq)

emptyGitState :: GitState
emptyGitState = GitState Map.empty Map.empty Map.empty Map.empty 0

freshHash :: Member (State GitState) r => Sem r ObjectHash
freshHash = do
  s <- get
  let n = gsNextId s
  put s { gsNextId = n + 1 }
  return $ ObjectHash $ T.pack (show n)

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

  CreateRef name hash -> do
    s <- get
    put s { gsRefs = Map.insert name hash (gsRefs s) }

  UpdateRef name hash -> do
    s <- get
    put s { gsRefs = Map.insert name hash (gsRefs s) }

  DeleteRef name -> do
    s <- get
    put s { gsRefs = Map.delete name (gsRefs s) }

  ListRefs prefix -> do
    s <- get
    let matching = Map.toList $ Map.filterWithKey
          (\(RefName k) _ -> prefix `T.isPrefixOf` k)
          (gsRefs s)
    return matching

  ReadCommit hash -> do
    s <- get
    case Map.lookup hash (gsCommits s) of
      Just cd -> return cd
      Nothing -> fail $ "ReadCommit: unknown hash " <> T.unpack (unObjectHash hash)

  WriteCommit cd -> do
    hash <- freshHash
    s <- get
    put s { gsCommits = Map.insert hash cd (gsCommits s) }
    return hash

  ReadBlob hash -> do
    s <- get
    case Map.lookup hash (gsBlobs s) of
      Just bs -> return bs
      Nothing -> fail $ "ReadBlob: unknown hash " <> T.unpack (unObjectHash hash)

  WriteBlob content -> do
    hash <- freshHash
    s <- get
    put s { gsBlobs = Map.insert hash content (gsBlobs s) }
    return hash

  ReadTree hash -> do
    s <- get
    case Map.lookup hash (gsTrees s) of
      Just entries -> return entries
      Nothing
        | hash == emptyTreeHash -> return []
        | otherwise -> fail $ "ReadTree: unknown hash " <> T.unpack (unObjectHash hash)

  WriteTree entries -> do
    hash <- freshHash
    s <- get
    put s { gsTrees = Map.insert hash entries (gsTrees s) }
    return hash

  LookupPath hash path -> do
    s <- get
    case Map.lookup hash (gsTrees s) of
      Nothing -> fail $ "LookupPath: unknown tree hash " <> T.unpack (unObjectHash hash)
      Just entries ->
        return $ entryHash <$> find (\e -> entryName e == path) entries
