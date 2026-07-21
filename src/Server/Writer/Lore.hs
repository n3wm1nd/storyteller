{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- interceptor 'Server.Writer.ContextView.Connection' already wraps its own
-- reads in), and each eligible file's blurb and
-- aliases (reusing 'Storyteller.Writer.Agent.ContextPreview.blurb' and
-- 'Storyteller.Writer.Lore.parseAliases' off the same content read). No
-- incremental cache like 'Server.Writer.Library.LibraryFoldCache' — a full
-- codex re-read on every ref-move is cheap enough (short files, first-
-- line-only reads) not to need one.
--
-- Every parsed alias is additionally run through the Context DSL's
-- @context.mentionFilter@ definition (see
-- "Storyteller.Context.DSL.Library") before it ever reaches
-- 'Storyteller.Writer.Lore.LoreNode' — this is the "mention filter" the
-- composer's auto-include-on-@\@mention@ feature reads, wired straight into
-- the tree this connection already pushes rather than as a separate
-- request\/response: the default is identity (every declared alias stays
-- active), and a project narrows it by overriding @context.mentionFilter@
-- on the 'Storyteller.Core.Runtime.Contexts' branch (@aliases |
-- without(...)@\/@only(...)@), the same override mechanism every other
-- @context.*@ definition already gets.
module Server.Writer.Lore
  ( loreTree
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Sem)
import Runix.FileSystem (listAllFiles, readFile)
import Runix.Git (Git)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Context (ContextStorage, getContextDefinition, runContextValue)
import Storyteller.Core.Git (BranchTag)
import Storyteller.Context.DSL.Compile (Binding(..))
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.Value (Value(..), defaultMeta, leafValue)
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)
import Storyteller.Writer.Agent.ContextPreview (blurb)
import Storyteller.Writer.Lore (LoreNode, isLoreEligible, buildLoreTree, parseAliases)

import Prelude hiding (readFile)

-- | The full codex forest for this branch: every eligible path (see
--   'Storyteller.Writer.Lore.isLoreEligible'), paired with the first
--   non-blank line and the parsed, mention-filtered aliases of its own
--   content, built into a tree.
loreTree :: (BranchOpen r, Members '[ContextStorage, Git] r) => Sem r [LoreNode]
loreTree = hideBinaryFiles @(BranchTag Main) @Main $ do
  paths  <- filter isLoreEligible <$> listAllFiles @(BranchTag Main) "/"
  files  <- mapM readWithBlurb paths
  active <- activeMentionAliases (concatMap (\(_, _, aliases) -> aliases) files)
  return (buildLoreTree [ (path, b, filter (`Set.member` active) aliases) | (path, b, aliases) <- files ])
  where
    readWithBlurb path = do
      content <- TE.decodeUtf8 <$> readFile @(BranchTag Main) path
      return (path, blurb content, parseAliases content)

-- | Runs @context.mentionFilter@ against every alias this branch's codex
--   declares, returning the subset that stays active for
--   auto-inclusion-on-mention. Candidate entries carry no real content (a
--   plain leaf) -- today's filter vocabulary (@without@\/@only@) only ever
--   decides by name, and there's nothing here worth reading a whole file a
--   second time for just to populate a field no default or override
--   actually looks at yet.
activeMentionAliases
  :: (BranchOpen r, Members '[ContextStorage, Git] r)
  => [T.Text] -> Sem r (Set T.Text)
activeMentionAliases aliasNames = do
  let candidate = Value
        { valueDefault = pure []
        , valueEntries = [ (name, pure (leafValue [])) | name <- aliasNames ]
        , valueMeta = defaultMeta
        }
      defaultBinding = CtxLibrary.toBinding1 CtxLibrary.contextMentionFilter
  binding <- getContextDefinition "context.mentionFilter" defaultBinding
  case binding of
    Binding 1 fn -> Set.fromList . map fst . valueEntries
      <$> runContextValue @Main (fn [pure candidate] emptyValueUnused)
    Binding n _  -> fail ("context.mentionFilter: expected arity 1, got " <> show n)
  where
    -- The scope argument a 'Binding'\'s own function takes is always
    -- ignored for a top-level definition (see
    -- 'Storyteller.Core.Context.resolveContextOverride'\/'resolveContextQuery'
    -- 's own haddocks: both route through 'runDefinition', which bootstraps
    -- its own scope from whatever commit is ambient) -- this is never
    -- forced.
    emptyValueUnused = Value { valueDefault = pure [], valueEntries = [], valueMeta = defaultMeta }
