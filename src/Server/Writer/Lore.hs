{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Composition for the @\/lore\/{name}@ connection: the codex-curation view
-- over one branch's freeform notes\/world\/style content — see
-- WS-PROTOCOL.md. Writer-specific in the same way 'Server.Writer.Library'
-- is: it reuses 'Storyteller.Writer.Library.classifyPath' to know what
-- /isn't/ codex content, which 'Server.Core.Branch' has no business
-- knowing about.
--
-- Structure is pure (see 'Storyteller.Writer.Lore'); this module adds what
-- can't be: the branch's current file list, binary-hiding (reusing
-- 'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles', the same
-- interceptor 'Storyteller.Writer.Agent.Continuation.gatherFileContext'
-- already wraps its own reads in), and each eligible file's blurb and
-- aliases (reusing 'Storyteller.Writer.Agent.ContextPreview.blurb' and
-- 'Storyteller.Writer.Lore.parseAliases' off the same content read). No
-- incremental cache like 'Server.Writer.Library.LibraryFoldCache' — a full
-- codex re-read on every ref-move is cheap enough (short files, first-
-- line-only reads) not to need one.
module Server.Writer.Lore
  ( loreTree
  ) where

import qualified Data.Text.Encoding as TE
import Polysemy (Sem)
import Runix.FileSystem (listAllFiles, readFile)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)
import Storyteller.Writer.Agent.ContextPreview (blurb)
import Storyteller.Writer.Lore (LoreNode, isLoreEligible, buildLoreTree, parseAliases)

import Prelude hiding (readFile)

-- | The full codex forest for this branch: every eligible path (see
--   'Storyteller.Writer.Lore.isLoreEligible'), paired with the first
--   non-blank line and the parsed aliases of its own content, built into a
--   tree.
loreTree :: BranchOpen r => Sem r [LoreNode]
loreTree = hideBinaryFiles @(BranchTag Main) @Main $ do
  paths <- filter isLoreEligible <$> listAllFiles @(BranchTag Main) "/"
  files <- mapM readWithBlurb paths
  return (buildLoreTree files)
  where
    readWithBlurb path = do
      content <- TE.decodeUtf8 <$> readFile @(BranchTag Main) path
      return (path, blurb content, parseAliases content)
