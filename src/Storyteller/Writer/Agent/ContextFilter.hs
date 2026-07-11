{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A read-only 'Runix.FileSystem' interceptor that hides every path with
-- no atom history at all (an uploaded binary asset, or anything else that
-- opted out of atom tracking -- see "Storage.Ops"'s 'Storage.Ops.hasAnyAtom'
-- and the design conversation on binary file support) from whatever runs
-- inside it: absent from any 'listFiles'\/'listAllFiles' result, and a
-- 'readFile' on one fails the same way any other filtered-out path does.
--
-- This is the interceptor half of the context-assembly design (see the
-- project memory on it): user-facing overrides would be their own,
-- separate stage; this one needs no configuration at all, since "has no
-- atom history" is already a fact about the branch, not a preference.
-- Applied in front of 'Storyteller.Writer.Agent.Continuation.gatherFileContext'
-- \/'Storyteller.Writer.Agent.CharContext.readCharFiles' (or anything else
-- that reads "every branch file" unconditionally) so an agent's ambient
-- context never sees binary content at all, without either of those
-- functions needing to know why some paths just don't show up.
module Storyteller.Writer.Agent.ContextFilter
  ( hideBinaryFiles
  , PickerRule(..)
  , ContextLayout
  , classifyPath
  , applyContextLayout
  ) where

import Control.Monad (filterM)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.List (sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.FilePath.Glob as Glob
import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem
  (FileSystem, FileSystemRead, PathFilter(..), filterFileSystem, filterRead, listAllFiles)
import qualified Runix.FileSystem.Path as Path

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchOp, runStorage)

-- | Wrap @action@ so every binary path in @branch@ is invisible to it.
--   Read-only narrowing, same contract as every other 'PathFilter' in
--   "Runix.FileSystem" -- it never fabricates content, only hides some of
--   what's already there. Resolves every path the exact same way
--   'filterFileSystem'\/'filterRead' themselves do ('Path.resolveRelative'
--   from cwd @"/"@, the fixed cwd every branch filesystem reports -- see
--   'Storyteller.Core.Git.runStoryFSGit'), so the snapshot taken here lines
--   up with whatever the filter is actually asked about later.
hideBinaryFiles
  :: forall project branch r a
  .  ( Members '[FileSystem project, FileSystemRead project, BranchOp branch, Fail] r )
  => Sem r a -> Sem r a
hideBinaryFiles action = do
  paths  <- listAllFiles @project "/"
  binary <- filterM (\p -> not <$> runStorage @branch (Ops.hasAnyAtom p)) paths
  let resolved = Set.fromList (map (Path.resolveRelative "/") binary)
      filt = PathFilter
        { shouldInclude = \p -> not (Set.member p resolved)
        , filterName    = "binary files are hidden"
        }
  filterRead @project filt (filterFileSystem @project filt action)

-- | One claim in a 'ContextLayout': every path matching 'prPattern' that no
--   earlier rule in the list already claimed is assigned 'prBucket'. Glob
--   syntax, same as 'Runix.FileSystem'\'s own @Glob@ filesystem op and
--   'Storyteller.Writer.Agent.ContextPreview.PathFilter' -- no bespoke
--   pattern language. See the project's context-assembly design memory
--   (2026-07-08 refinement) for the bucket-picker model this implements:
--   claim order (list position) and bucket order ('prBucket') are
--   independent axes on purpose -- a narrow pattern needs to claim before a
--   broad catch-all regardless of which bucket either targets.
--
--   'prBucket' is @Nothing@ for an explicit trash claim -- a rule that
--   matches and hides a path, distinct from no rule matching at all only in
--   that it can sit *ahead* of a broader rule in claim order (e.g.
--   @secret.md -> trash@ before a catch-all @** \/* -> bucket 1@, so the
--   catch-all doesn't also claim it). Both reach the same place: not
--   emitted by 'applyContextLayout'\/'classifyPath'.
data PickerRule = PickerRule
  { prPattern :: T.Text
  , prBucket  :: Maybe Int
  } deriving (Show, Eq)

instance FromJSON PickerRule where
  parseJSON = withObject "PickerRule" $ \o ->
    PickerRule <$> o .: "pattern" <*> o .:? "bucket"

-- | An ordered picker list, evaluated top to bottom. @[]@ means "no layout
--   configured" -- callers treat that as a request to fall back to their
--   own default ordering (see 'Storyteller.Writer.Agent.Continuation.gatherFileContext'),
--   not as "claim nothing".
type ContextLayout = [PickerRule]

-- | Partition and order a path list per a 'ContextLayout': each path is
--   claimed by the first rule (in list order) whose pattern matches it,
--   then emitted grouped by ascending bucket number, filename-sorted within
--   a bucket. A path no rule claims is dropped -- trash is not a special
--   case, just the bucket nothing emits from, reached identically whether a
--   rule explicitly targets it or no rule matches at all.
--
--   @applyContextLayout [] = id@ is deliberately not this function's
--   behaviour (an empty layout here drops everything, consistent with
--   "unclaimed = hidden") -- callers that want "no layout configured"
--   to mean "show everything" must check for @[]@ themselves before calling
--   this, since only the caller knows what its own no-layout default is.
applyContextLayout :: ContextLayout -> [FilePath] -> [FilePath]
applyContextLayout rules paths =
  map fst $ sortOn (\(path, bucket) -> (bucket, path)) $ mapMaybe claimed paths
  where
    claim = classifyPath rules
    claimed path = (,) path <$> claim path

-- | Which bucket (if any) a single path is claimed into by a
--   'ContextLayout' -- the first rule (in list order) whose pattern
--   matches, or 'Nothing' if no rule does. Exposed on its own (not just
--   folded into 'applyContextLayout') because a preview UI
--   ('Storyteller.Writer.Agent.ContextPreview') needs to show *every* path
--   annotated with its bucket, unclaimed ones included (shaded, not
--   dropped) -- the opposite of 'applyContextLayout's own job of producing
--   the final claimed-and-ordered list a generation call actually reads.
--   The extra layer of 'Maybe' this stops at matters: a rule matching
--   *first* commits its verdict (trash or a bucket) even if a later rule in
--   the list would also match and disagrees -- so @join@ (via @>>= id@)
--   collapses "first match's own bucket" rather than "first non-trash
--   match", which would silently let a broader catch-all override an
--   earlier explicit trash claim.
classifyPath :: ContextLayout -> FilePath -> Maybe Int
classifyPath rules path =
  listToMaybe [ bucket | PickerRule pat bucket <- rules, Glob.match (Glob.compile (T.unpack pat)) path ] >>= id
