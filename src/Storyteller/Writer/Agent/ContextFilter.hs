{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  ) where

import Control.Monad (filterM)
import qualified Data.Set as Set
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
  binary <- filterM (\p -> not . fst <$> runStorage @branch (Ops.hasAnyAtom p)) paths
  let resolved = Set.fromList (map (Path.resolveRelative "/") binary)
      filt = PathFilter
        { shouldInclude = \p -> not (Set.member p resolved)
        , filterName    = "binary files are hidden"
        }
  filterRead @project filt (filterFileSystem @project filt action)
