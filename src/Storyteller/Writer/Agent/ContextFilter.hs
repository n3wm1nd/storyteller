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
-- Still used by UI-facing browsing reads ("Server.Writer.Lore",
-- "Server.Writer.ContextView.Connection") that walk a branch's own
-- filesystem directly. Agent context assembly no longer goes through this
-- module at all -- see "Storyteller.Context.DSL.Compile"'s own Reader-scope
-- bootstrap (backed by 'Storage.Query.loadLiveWorkingTree') for how an
-- agent's context excludes binaries now; 'hideChapters'\/'hideLore'\/
-- 'applyContextLayout', the machinery that used to narrow
-- 'Storyteller.Writer.Agent.Continuation.gatherFileContext' the same way
-- @context.main@ classifies content in DSL text today, were removed once
-- that migration left them with no caller (see CONTEXT-DSL.md).
module Storyteller.Writer.Agent.ContextFilter
  ( hideBinaryFiles
  , PickerRule(..)
  , ContextLayout
  , classifyPath
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Maybe (listToMaybe)
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
  paths   <- listAllFiles @project "/"
  tracked <- runStorage @branch (Ops.atomTrackedAmong paths)
  let binary = filter (`Set.notMember` tracked) paths
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
--   emitted by 'classifyPath'.
data PickerRule = PickerRule
  { prPattern :: T.Text
  , prBucket  :: Maybe Int
  } deriving (Show, Eq)

instance FromJSON PickerRule where
  parseJSON = withObject "PickerRule" $ \o ->
    PickerRule <$> o .: "pattern" <*> o .:? "bucket"

-- | An ordered picker list, evaluated top to bottom. @[]@ means "no layout
--   configured" -- what 'Storyteller.Writer.Agent.ContextPreview' treats as
--   a request to show every path unclaimed, rather than "claim nothing".
type ContextLayout = [PickerRule]

-- | Which bucket (if any) a single path is claimed into by a
--   'ContextLayout' -- the first rule (in list order) whose pattern
--   matches, or 'Nothing' if no rule does. A preview UI
--   ('Storyteller.Writer.Agent.ContextPreview') uses this to show *every*
--   path annotated with its bucket, unclaimed ones included (shaded, not
--   dropped). The extra layer of 'Maybe' this stops at matters: a rule
--   matching *first* commits its verdict (trash or a bucket) even if a
--   later rule in the list would also match and disagrees -- so @join@
--   (via @>>= id@) collapses "first match's own bucket" rather than "first
--   non-trash match", which would silently let a broader catch-all
--   override an earlier explicit trash claim.
classifyPath :: ContextLayout -> FilePath -> Maybe Int
classifyPath rules path =
  listToMaybe [ bucket | PickerRule pat bucket <- rules, Glob.match (Glob.compile (T.unpack pat)) path ] >>= id
