{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- can't be: the branch's current file list, binary-hiding, and each
-- eligible file's blurb and aliases (reusing
-- 'Storyteller.Writer.Lore.blurb' and 'Storyteller.Writer.Lore.parseAliases'
-- off the same content read). The first two come together, from one
-- 'Storyteller.Core.Snapshot.runTextSnapshotFS' wired at the branch's own
-- head -- so 'loreEntries', the part that actually walks files, is a plain
-- @Members '[FileSystem project, FileSystemRead project]@ function with no
-- notion of branches, git, or binaries at all. No
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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, getFileSystem, listAllFiles, readFile)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Context (ContextStorage, getContextOverrides, resolveOverrideDefinition, runContextValue)
import Storyteller.Core.Git (BranchTag(..), runStoryFSRead)
import Storyteller.Context.DSL.AST (defParams)
import Storyteller.Context.DSL.Compile (bval, emptyLibrary, runDefinition)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.Value (Value(..), defaultMeta, leafValue)
import Storyteller.Writer.Lore (LoreNode, isLoreEligible, buildLoreTree, parseAliases, blurb)

import Prelude hiding (readFile)

-- | The full codex forest for this branch: every eligible path (see
--   'Storyteller.Writer.Lore.isLoreEligible'), paired with the first
--   non-blank line and the parsed, mention-filtered aliases of its own
--   content, built into a tree.
loreTree :: (BranchOpen r, Member ContextStorage r) => Sem r [LoreNode]
loreTree = do
  -- The ambient branch filesystem is already in the row, but it shows
  -- every path; 'loreEntries' must only see readable content. So shadow it
  -- for the duration with the filtered read-only view of the same branch,
  -- taking the branch's own name from the filesystem already open on it.
  BranchTag name <- getFileSystem @(BranchTag Main)
  files  <- runStoryFSRead @(BranchTag Main) @Main (BranchTag name) (loreEntries @(BranchTag Main))
  active <- activeMentionAliases (concatMap (\(_, _, aliases) -> aliases) files)
  return (buildLoreTree [ (path, b, filter (`Set.member` active) aliases) | (path, b, aliases) <- files ])

-- | Every eligible path paired with its blurb and declared aliases --
--   'loreTree' minus the mention filter, and the half that is genuinely
--   just filesystem work.
--
--   Generic over @project@: listing and reading files is all it does, so
--   it asks for a filesystem and nothing else. Which paths are hidden as
--   binary isn't its business either -- it used to be wrapped in the
--   since-deleted @ContextFilter.hideBinaryFiles@ (an
--   interceptor needing 'Storyteller.Core.Branch.BranchOp' in the caller's
--   own row, which is what kept this whole module pinned to a git-backed
--   branch scope); the policy now lives in whichever interpreter is wired
--   ('Storyteller.Core.Snapshot.runTextSnapshotFS' above), so a consumer
--   that isn't prepared to deal with binary content simply never sees any.
loreEntries
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Sem r [(FilePath, T.Text, [T.Text])]
loreEntries = do
  paths <- filter isLoreEligible <$> listAllFiles @project "/"
  mapM readWithBlurb paths
  where
    readWithBlurb path = do
      content <- TE.decodeUtf8 <$> readFile @project path
      return (path, blurb content, parseAliases content)

-- | Runs @context.mentionFilter@ against every alias this branch's codex
--   declares, returning the subset that stays active for
--   auto-inclusion-on-mention. Candidate entries carry no real content (a
--   plain leaf) -- today's filter vocabulary (@without@\/@only@) only ever
--   decides by name, and there's nothing here worth reading a whole file a
--   second time for just to populate a field no default or override
--   actually looks at yet.
--   Runs @context.mentionFilter@ against 'emptyLibrary', not against the
--   project's full compiled table.
--
--   That is a capability decision, not a shortcut. Building the full table
--   ('buildContextLibrary') constructs every 'hostLibrary' binding, so a
--   caller owes all of their capabilities -- @charactersin@'s presence
--   data, @summarized@'s summary access, @branch@'s ability to enter
--   another branch -- regardless of which entry it goes on to run. This
--   runs exactly one definition whose default (@aliases: in aliases: for f
--   in *: as f: read f@) references no library name at all, so paying that
--   union would have meant "list the lore files on this branch" demanding
--   the right to cross into character branches. It did, until this comment
--   was written.
--
--   The visible consequence: an override of @context.mentionFilter@ that
--   /calls another library definition by name/ no longer resolves, and
--   fails loudly rather than silently. Today's filter vocabulary
--   (@without@\/@only@, and every other 'coreFilters' entry) is unaffected
--   -- filters aren't library entries. If a real override ever needs the
--   table, the fix is to hand this the library it needs from a caller that
--   can already satisfy it, not to rebuild the union here.
activeMentionAliases
  :: (BranchOpen r, Member ContextStorage r)
  => [T.Text] -> Sem r (Set T.Text)
activeMentionAliases aliasNames = do
  let candidate = Value
        { valueDefault = pure []
        , valueEntries = [ (name, pure (leafValue [])) | name <- aliasNames ]
        , valueMeta = defaultMeta
        }
  overrides <- getContextOverrides
  result <- runContextValue @Main $
    case resolveOverrideDefinition (Map.lookup "context.mentionFilter" overrides) of
      Just overrideDef
        | length (defParams overrideDef) == 1 ->
            runDefinition emptyLibrary overrideDef [bval (pure candidate)]
      _ -> CtxLibrary.contextMentionFilter (bval (pure candidate))
  pure (Set.fromList (map fst (valueEntries result)))
