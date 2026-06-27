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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

data GitState = GitState
  { gsRefs     :: Map RefName ObjectHash
  , gsCommits  :: Map ObjectHash CommitData
  , gsNextId   :: Int
  } deriving (Show, Eq)

emptyGitState :: GitState
emptyGitState = GitState Map.empty Map.empty 0

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

  ReadBlob hash ->
    fail $ "ReadBlob: not implemented in mock (hash=" <> T.unpack (unObjectHash hash) <> ")"

  ReadTree hash ->
    fail $ "ReadTree: not implemented in mock (hash=" <> T.unpack (unObjectHash hash) <> ")"

  LookupPath hash path ->
    fail $ "LookupPath: not implemented in mock (hash=" <> T.unpack (unObjectHash hash) <> ", path=" <> path <> ")"
