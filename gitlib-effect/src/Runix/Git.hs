{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Git effect and interpreter.
--
-- Pure git vocabulary: object hashes, refs, commits, trees, blobs.
-- No storyteller concepts leak in here.
--
-- The interpreter uses @Cmd "git"@ from runix. When a maintained libgit2
-- binding becomes available for the current GHC, the interpreter can be
-- swapped without touching the effect or anything above it.
--
-- Intended to move to runix proper once stable.
module Runix.Git
  ( -- * Types
    ObjectHash(..)
  , RefName(..)
  , CommitData(..)
  , TreeEntry(..)

    -- * Effect
  , Git(..)
  , resolveRef
  , createRef
  , updateRef
  , deleteRef
  , listRefs
  , readCommit
  , writeCommit
  , readBlob
  , readTree
  , lookupPath

    -- * Interpreter
  , runGitIO
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Polysemy
import Polysemy.Fail

import Runix.Cmd (Cmd, callIn, callStdinIn, CmdOutput(..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | An opaque git object hash (40-char hex SHA).
newtype ObjectHash = ObjectHash { unObjectHash :: Text }
  deriving (Show, Eq, Ord)

-- | A git ref name, e.g. @refs/heads/main@.
newtype RefName = RefName { unRefName :: Text }
  deriving (Show, Eq, Ord)

-- | The data needed to describe or write a commit.
data CommitData = CommitData
  { commitParents :: [ObjectHash]
  , commitTree    :: ObjectHash
  , commitMessage :: Text
  } deriving (Show, Eq)

-- | A single entry returned by 'readTree'.
data TreeEntry
  = BlobEntry { entryName :: FilePath, entryHash :: ObjectHash }
  | SubTree   { entryName :: FilePath, entryHash :: ObjectHash }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Effect
-- ---------------------------------------------------------------------------

data Git m a where
  ResolveRef  :: RefName                    -> Git m (Maybe ObjectHash)
  CreateRef   :: RefName -> ObjectHash      -> Git m ()
  UpdateRef   :: RefName -> ObjectHash      -> Git m ()
  DeleteRef   :: RefName                    -> Git m ()
  ListRefs    :: Text                       -> Git m [(RefName, ObjectHash)]
  ReadCommit  :: ObjectHash                 -> Git m CommitData
  WriteCommit :: CommitData                 -> Git m ObjectHash
  ReadBlob    :: ObjectHash                 -> Git m ByteString
  ReadTree    :: ObjectHash                 -> Git m [TreeEntry]
  LookupPath  :: ObjectHash -> FilePath     -> Git m (Maybe ObjectHash)

makeSem ''Git

-- ---------------------------------------------------------------------------
-- Interpreter
-- ---------------------------------------------------------------------------

-- | Interpret 'Git' using @Cmd "git"@ against a repository at @repoPath@.
runGitIO
  :: Members '[Cmd "git", Fail] r
  => FilePath
  -> Sem (Git : r) a
  -> Sem r a
runGitIO repo = interpret $ \case
  ResolveRef (RefName name) -> do
    out <- git repo ["rev-parse", "--verify", "--quiet", T.unpack name]
    case T.lines (stdout out) of
      (h:_) | exitCode out == 0 -> return $ Just (ObjectHash (T.strip h))
      _                         -> return Nothing

  CreateRef (RefName name) hash ->
    git_ repo ["update-ref", T.unpack name, T.unpack (unObjectHash hash)]

  UpdateRef (RefName name) hash ->
    git_ repo ["update-ref", T.unpack name, T.unpack (unObjectHash hash)]

  DeleteRef (RefName name) ->
    git_ repo ["update-ref", "-d", T.unpack name]

  ListRefs prefix -> do
    out <- git repo ["for-each-ref", "--format=%(refname) %(objectname)", T.unpack prefix]
    return $ mapMaybe parseLine (T.lines (stdout out))

  ReadCommit hash -> do
    out <- git repo ["cat-file", "-p", T.unpack (unObjectHash hash)]
    parseCommit (stdout out)

  WriteCommit cd -> do
    let parentArgs = concatMap (\p -> ["-p", T.unpack (unObjectHash p)]) (commitParents cd)
        args = ["commit-tree", T.unpack (unObjectHash (commitTree cd))] ++ parentArgs
    out <- gitStdin repo args (TE.encodeUtf8 (commitMessage cd))
    case T.lines (stdout out) of
      (h:_) | exitCode out == 0 -> return $ ObjectHash (T.strip h)
      _ -> fail $ "git commit-tree failed: " <> T.unpack (stderr out)

  ReadBlob hash -> do
    out <- git repo ["cat-file", "blob", T.unpack (unObjectHash hash)]
    return $ TE.encodeUtf8 (stdout out)

  ReadTree hash -> do
    out <- git repo ["ls-tree", T.unpack (unObjectHash hash)]
    return $ mapMaybe parseTreeLine (T.lines (stdout out))

  LookupPath hash path -> do
    out <- git repo ["ls-tree", "-r", T.unpack (unObjectHash hash), "--", path]
    case mapMaybe parseTreeLine (T.lines (stdout out)) of
      (e:_) -> return $ Just (entryHash e)
      []    -> return Nothing

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

git :: Member (Cmd "git") r => FilePath -> [String] -> Sem r CmdOutput
git repo args = callIn @"git" repo args

gitStdin :: Member (Cmd "git") r => FilePath -> [String] -> ByteString -> Sem r CmdOutput
gitStdin repo args stdin = callStdinIn @"git" repo args stdin

git_ :: Member (Cmd "git") r => FilePath -> [String] -> Sem r ()
git_ repo args = git repo args >> return ()

parseLine :: Text -> Maybe (RefName, ObjectHash)
parseLine line = case T.words line of
  [ref, hash] -> Just (RefName ref, ObjectHash hash)
  _           -> Nothing

-- ---------------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------------

parseCommit :: Member Fail r => Text -> Sem r CommitData
parseCommit raw = do
  let ls = T.lines raw
      (headers, bodyLines) = span (not . T.null) ls
      body = T.intercalate "\n" (drop 1 bodyLines)  -- skip blank separator line
      parents = [ ObjectHash (T.strip (T.drop 7 l))
                | l <- headers, "parent " `T.isPrefixOf` l ]
  tree <- case [ T.strip (T.drop 5 l) | l <- headers, "tree " `T.isPrefixOf` l ] of
    (t:_) -> return (ObjectHash t)
    []    -> fail "git cat-file commit: missing tree line"
  return CommitData
    { commitParents = parents
    , commitTree    = tree
    , commitMessage = body
    }

-- | Parse one line of @git ls-tree@ output:
-- @<mode> <type> <hash>\t<name>@
parseTreeLine :: Text -> Maybe TreeEntry
parseTreeLine line =
  case T.splitOn "\t" line of
    [meta, name] -> case T.words meta of
      [_mode, typ, hash]
        | typ == "blob"   -> Just $ BlobEntry (T.unpack name) (ObjectHash hash)
        | typ == "tree"   -> Just $ SubTree   (T.unpack name) (ObjectHash hash)
      _ -> Nothing
    _ -> Nothing
