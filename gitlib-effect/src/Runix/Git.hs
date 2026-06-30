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
  , GitObject(..)

    -- * Effect
  , Git(..)
  , resolveRef
  , createRef
  , updateRef
  , deleteRef
  , listRefs
  , readCommit
  , writeCommit
  , readObject
  , writeObject
  , lookupPath

    -- * Typed smart constructors (encode/decode around readObject/writeObject)
  , readBlob
  , writeBlob
  , readTree
  , writeTree

    -- * Interpreter
  , runGitIO

    -- * Object cache interceptor
  , withGitCache
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Polysemy
import Polysemy.Fail
import Polysemy.State (State, get, modify, evalState)

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

-- | A typed git object: either raw blob bytes or a parsed tree.
data GitObject
  = BlobObject ByteString
  | TreeObject [TreeEntry]
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
  ReadObject  :: ObjectHash                 -> Git m GitObject
  WriteObject :: GitObject                  -> Git m ObjectHash
  LookupPath  :: ObjectHash -> FilePath     -> Git m (Maybe ObjectHash)

makeSem ''Git

-- ---------------------------------------------------------------------------
-- Typed smart constructors
-- ---------------------------------------------------------------------------

readBlob :: Members '[Git, Fail] r => ObjectHash -> Sem r ByteString
readBlob h = readObject h >>= \case
  BlobObject bs -> return bs
  TreeObject _  -> fail $ "readBlob: hash is a tree: " <> T.unpack (unObjectHash h)

writeBlob :: Member Git r => ByteString -> Sem r ObjectHash
writeBlob = writeObject . BlobObject

readTree :: Members '[Git, Fail] r => ObjectHash -> Sem r [TreeEntry]
readTree h = readObject h >>= \case
  TreeObject es -> return es
  BlobObject _  -> fail $ "readTree: hash is a blob: " <> T.unpack (unObjectHash h)

writeTree :: Member Git r => [TreeEntry] -> Sem r ObjectHash
writeTree = writeObject . TreeObject

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
    let parentLines = T.unlines [ "parent " <> unObjectHash p | p <- commitParents cd ]
        raw = "tree " <> unObjectHash (commitTree cd) <> "\n"
           <> parentLines
           <> "author . <.> 0 +0000\n"
           <> "committer . <.> 0 +0000\n"
           <> "\n"
           <> commitMessage cd <> "\n"
    out <- gitStdin repo ["hash-object", "-t", "commit", "-w", "--stdin"] (TE.encodeUtf8 raw)
    case T.lines (stdout out) of
      (h:_) | exitCode out == 0 -> return $ ObjectHash (T.strip h)
      _ -> fail $ "git hash-object commit failed: " <> T.unpack (stderr out)

  ReadObject hash -> do
    out <- gitStdin repo ["cat-file", "--batch-check=%(objecttype)"] (TE.encodeUtf8 (unObjectHash hash))
    case T.strip (stdout out) of
      "blob" -> do
        raw <- git repo ["cat-file", "blob", T.unpack (unObjectHash hash)]
        return $ BlobObject (TE.encodeUtf8 (stdout raw))
      "tree" -> do
        raw <- git repo ["ls-tree", T.unpack (unObjectHash hash)]
        return $ TreeObject (mapMaybe parseTreeLine (T.lines (stdout raw)))
      typ -> fail $ "ReadObject: unsupported object type '" <> T.unpack typ
                 <> "' for " <> T.unpack (unObjectHash hash)

  WriteObject (BlobObject content) -> do
    out <- gitStdin repo ["hash-object", "-w", "--stdin"] content
    case T.lines (stdout out) of
      (h:_) | exitCode out == 0 -> return $ ObjectHash (T.strip h)
      _ -> fail $ "git hash-object failed: " <> T.unpack (stderr out)

  WriteObject (TreeObject entries) -> do
    let entryLine e = case e of
          BlobEntry name h -> "100644 blob " <> unObjectHash h <> "\t" <> T.pack name
          SubTree   name h -> "040000 tree " <> unObjectHash h <> "\t" <> T.pack name
        input = T.unlines (map entryLine entries)
    out <- gitStdin repo ["mktree"] (TE.encodeUtf8 input)
    case T.lines (stdout out) of
      (h:_) | exitCode out == 0 -> return $ ObjectHash (T.strip h)
      _ -> fail $ "git mktree failed: " <> T.unpack (stderr out)

  LookupPath hash path -> do
    out <- git repo ["ls-tree", "-r", T.unpack (unObjectHash hash), "--", path]
    case mapMaybe parseTreeLine (T.lines (stdout out)) of
      (e:_) -> return $ Just (entryHash e)
      []    -> return Nothing

-- ---------------------------------------------------------------------------
-- Object cache interceptor
-- ---------------------------------------------------------------------------

data GitCache = GitCache
  { cacheObjects :: Map ObjectHash GitObject
  , cacheCommits :: Map ObjectHash CommitData
  }

emptyGitCache :: GitCache
emptyGitCache = GitCache Map.empty Map.empty

-- | Intercept 'Git' and cache all content-addressed reads and writes.
--   Refs are never cached — they are mutable and must always hit the
--   underlying interpreter.  Objects and commits are immutable by hash,
--   so a cache hit is always correct.
withGitCache
  :: Member Git r
  => Sem r a
  -> Sem r a
withGitCache action = evalState emptyGitCache $ intercept (\case
      -- Refs are mutable — always pass through.
      ResolveRef ref        -> raise $ send (ResolveRef ref)
      CreateRef  ref hash   -> raise $ send (CreateRef  ref hash)
      UpdateRef  ref hash   -> raise $ send (UpdateRef  ref hash)
      DeleteRef  ref        -> raise $ send (DeleteRef  ref)
      ListRefs   prefix     -> raise $ send (ListRefs   prefix)

      ReadObject hash -> do
        cache <- get
        case Map.lookup hash (cacheObjects cache) of
          Just obj -> return obj
          Nothing  -> do
            obj <- raise $ send (ReadObject hash)
            modify $ \c -> c { cacheObjects = Map.insert hash obj (cacheObjects c) }
            return obj

      WriteObject obj -> do
        hash <- raise $ send (WriteObject obj)
        modify $ \c -> c { cacheObjects = Map.insert hash obj (cacheObjects c) }
        return hash

      ReadCommit hash -> do
        cache <- get
        case Map.lookup hash (cacheCommits cache) of
          Just cd -> return cd
          Nothing -> do
            cd <- raise $ send (ReadCommit hash)
            modify $ \c -> c { cacheCommits = Map.insert hash cd (cacheCommits c) }
            return cd

      WriteCommit cd -> do
        hash <- raise $ send (WriteCommit cd)
        modify $ \c -> c { cacheCommits = Map.insert hash cd (cacheCommits c) }
        return hash

      LookupPath tree path -> raise $ send (LookupPath tree path)
    ) (raise action)

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
