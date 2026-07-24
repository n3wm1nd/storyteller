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
-- Still used by UI-facing browsing reads ("Server.Writer.Lore") that walk a
-- branch's own filesystem directly. Agent context assembly (including the
-- Context DSL preview, "Storyteller.Writer.Agent.ContextPreview") no longer
-- goes through this module at all -- see "Storyteller.Context.DSL.Compile"'s
-- own Reader-scope bootstrap (backed by 'Storage.Query.loadLiveWorkingTree')
-- for how binaries are excluded now; 'hideChapters'\/'hideLore'\/
-- 'applyContextLayout'\/'PickerRule'\/'classifyPath', the bucket-picker
-- machinery that used to narrow both
-- 'Storyteller.Writer.Agent.Continuation.gatherFileContext' and the old
-- glob-based context preview the same way @context.main@ classifies
-- content in DSL text today, were removed once that migration left them
-- with no caller (see CONTEXT-DSL.md).
module Storyteller.Writer.Agent.ContextFilter
  ( hideBinaryFiles
  ) where

import qualified Data.Set as Set
import Polysemy (Members, Sem, raise)
import Polysemy.Fail (Fail)
import Runix.FileSystem
  (FileSystem, FileSystemRead, PathFilter(..), filterFileSystem, filterRead, listAllFiles)
import qualified Runix.FileSystem.Path as Path

import Storyteller.Core.ContentEffects (atomTrackedAmong, runTrackedFiles)
import Storyteller.Core.Git (BranchOp)

-- | Wrap @action@ so every binary path in @branch@ is invisible to it.
--   Read-only narrowing, same contract as every other 'PathFilter' in
--   "Runix.FileSystem" -- it never fabricates content, only hides some of
--   what's already there. Resolves every path the exact same way
--   'filterFileSystem'\/'filterRead' themselves do ('Path.resolveRelative'
--   from cwd @"/"@, the fixed cwd every branch filesystem reports -- see
--   'Storyteller.Core.Git.runStoryFSGit'), so the snapshot taken here lines
--   up with whatever the filter is actually asked about later.
--
--   Which paths are tracked is asked via
--   'Storyteller.Core.ContentEffects.TrackedFiles', opened and discharged
--   locally around @action@ -- never in this function's own external
--   signature, the same "hide the effect from the caller's own row"
--   shape 'Storyteller.Writer.Agent.Summarizer.runSummarizer' settled on.
hideBinaryFiles
  :: forall project branch r a
  .  ( Members '[FileSystem project, FileSystemRead project, BranchOp branch, Fail] r )
  => Sem r a -> Sem r a
hideBinaryFiles action = runTrackedFiles @branch $ do
  paths   <- listAllFiles @project "/"
  tracked <- atomTrackedAmong @branch paths
  let binary = filter (`Set.notMember` tracked) paths
      resolved = Set.fromList (map (Path.resolveRelative "/") binary)
      filt = PathFilter
        { shouldInclude = \p -> not (Set.member p resolved)
        , filterName    = "binary files are hidden"
        }
  filterRead @project filt (filterFileSystem @project filt (raise action))
