{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Context preview: run a client-submitted Context DSL program against a
-- branch and hand back exactly what it resolved to, as a tree mirroring
-- the DSL's own @Value@ shape (own content, then named entries, in order).
--
-- Superseded the old bucket-picker preview (glob patterns classified into
-- numbered buckets via the since-deleted @ContextFilter.classifyPath@)
-- once agent context assembly itself moved to the Context DSL -- see
-- CONTEXT-DSL.md. Previewing a @PickerRule@ layout stopped meaning anything
-- the day @context.writer@ became the real thing every generation call
-- resolves; this module now runs the *actual* program a call would use, the
-- same way 'Server.Writer.File.chatWriter' does (stage via
-- 'setContextOverride', resolve via 'resolveContext1', render via
-- 'renderContext') -- so a preview and a real send can never disagree about
-- what a program produces.
module Storyteller.Writer.Agent.ContextPreview
  ( PreviewNode(..)
  , buildPreview
  , buildAdhocPreview
  , buildEntries0
  , buildEntries1
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Rendering (RenderedContext(..), ContextItem(..), renderContext)
import Storyteller.Context.DSL.Value (messageText, listPaths)
import Storyteller.Core.Branch (BranchOp, Branches)
import Storyteller.Core.Context
  (ContextStorage, resolveContext0, resolveContext1, resolveAdhoc, runContextValue, setContextOverride)
import Storyteller.Core.ContentEffects (BranchResolve)

-- | One node of a rendered program's result -- own text content (each
--   source 'Storyteller.Context.DSL.Value.Message' flattened to its text,
--   in order), then named child entries, mirroring
--   'Storyteller.Context.DSL.Rendering.RenderedContext' exactly. A UI walks
--   this the same way it would walk the DSL's own @as "name": ...@
--   structure.
data PreviewNode = PreviewNode
  { pnContent :: [Text]
  , pnEntries :: [(Name, PreviewNode)]
  } deriving (Show, Eq)

fromRendered :: RenderedContext ContextItem -> PreviewNode
fromRendered (Node content entries) =
  PreviewNode (map (messageText . ciMessage) content) (map (fmap fromRendered) entries)

-- | Stage @program@ as this call's own @context.writer@ override (the
--   identical mechanism 'Server.Writer.File.chatWriter' uses for a
--   client-submitted @fcContext@), resolve it, and render the result -- so
--   a preview always shows exactly what a real @chat.writer@\/
--   @correct.group@ call sending the same program would see.
buildPreview
  :: forall branch r
  .  Members '[BranchOp branch, Branches, BranchResolve, ContextStorage, Fail] r
  => FilePath -> Text -> Sem r PreviewNode
buildPreview path program = do
  setContextOverride "context.writer" program
  writerV <- resolveContext1 @branch "context.writer" (T.pack path)
  fromRendered <$> runContextValue @branch (renderContext writerV)

-- | 'buildPreview', but for a program submitted on its own rather than as
--   a whole @context.writer@ override -- the same distinction
--   'Storyteller.Writer.Agent.ContextCost.buildAdhocProgramCosts' draws
--   from 'buildProgramCosts', and via the identical resolution primitive
--   ('Storyteller.Core.Context.resolveAdhoc'): a project-default context
--   slot (@context.lore@, @context.chaptersCompressed@, ...), a saved
--   pinned snippet, or a user-defined agent's own program is compiled and
--   resolved directly against the live library table, so a name reference
--   inside it (@context.lore@ itself, say) sees this project's own
--   committed override the identical way a real send would.
--
--   @mPath@ is the editing surface's own answer to "which file would this
--   run against", passed through to a program that declares a parameter
--   and ignored by one that doesn't (see 'resolveAdhoc'). A surface with
--   no meaningful file — the standalone @.dsl@ editor — still has an
--   honest answer available: the empty glob, which resolves to nothing
--   rather than to some arbitrary file. What it must not do is send
--   nothing at all: a @path:@-headed program (every custom agent's, and
--   @context.other@'s) would then have no argument to bind and could only
--   fail, which is exactly what "preview shows nothing" looked like
--   before this parameter existed.
buildAdhocPreview
  :: forall branch r
  .  Members '[BranchOp branch, Branches, BranchResolve, ContextStorage, Fail] r
  => Text -> Maybe FilePath -> Sem r PreviewNode
buildAdhocPreview program mPath = do
  v <- resolveAdhoc @branch program (maybe [] (pure . T.pack) mPath)
  fromRendered <$> runContextValue @branch (renderContext v)

-- | The flat, full file-path list a named context slot currently resolves
--   to -- what a client-facing file-toggle list (e.g. "which lore files
--   are currently included") needs, gotten from the real,
--   possibly-overridden slot itself ('resolveContext0'\/'resolveContext1'
--   -- the exact resolution 'Storyteller.Writer.Agent.Write.writeAgent'
--   and 'Server.Writer.File.chatWriter' already use for
--   @context.lore@\/@context.other@) via
--   'Storyteller.Context.DSL.Value.listPaths', rather than a second,
--   hand-rolled glob predicate that could silently drift from what a real
--   send would actually include (the exact bug class
--   'Storyteller.Context.DSL.Library.contextOtherDef''s own Haddock
--   describes for the dotted-name-not-bare-alias discipline: if a project
--   overrides @context.lore@\/@context.other@\/@context.chapters@, this
--   list has to track that override transparently, not restate the
--   compiled-in exclusion set as a second, driftable copy).
--
--   'buildEntries0' is @context.lore@'s own shape (0-arity, no @path@);
--   'buildEntries1' is @context.other@'s (1-arity, framed against "the
--   file about to be written" the same way @context.other@ itself is).
--   Two functions, not one taking a @Maybe FilePath@, because a slot's
--   arity is a fixed fact about it (see 'resolveContext0'\/
--   'resolveContext1''s own split) -- there is no caller that could
--   sensibly pick between them for the same name.
buildEntries0
  :: forall branch r
  .  Members '[BranchOp branch, Branches, BranchResolve, ContextStorage, Fail] r
  => Name -> Sem r [Text]
buildEntries0 name = do
  v <- resolveContext0 @branch name
  runContextValue @branch (listPaths v)

buildEntries1
  :: forall branch r
  .  Members '[BranchOp branch, Branches, BranchResolve, ContextStorage, Fail] r
  => Name -> FilePath -> Sem r [Text]
buildEntries1 name path = do
  v <- resolveContext1 @branch name (T.pack path)
  runContextValue @branch (listPaths v)
