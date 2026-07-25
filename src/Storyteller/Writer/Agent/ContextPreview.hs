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
-- numbered buckets via 'Storyteller.Writer.Agent.ContextFilter.classifyPath')
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
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Rendering (RenderedContext(..), ContextItem(..), renderContext)
import Storyteller.Context.DSL.Value (messageText)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.Branch (BranchOp)
import Storyteller.Core.Context
  (ContextStorage, resolveContext1, resolveAdhoc0, runContextValue, setContextOverride)
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
  .  Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => FilePath -> Text -> Sem r PreviewNode
buildPreview path program = do
  setContextOverride "context.writer" program
  writerV <- resolveContext1 @branch "context.writer" (T.pack path)
  fromRendered <$> runContextValue @branch (renderContext writerV)

-- | 'buildPreview', but for a bare 0-arity snippet with no @path@ of its
--   own -- the same distinction 'Storyteller.Writer.Agent.ContextCost.
--   buildAdhocProgramCosts' draws from 'buildProgramCosts', and via the
--   identical resolution primitive ('Storyteller.Core.Context.
--   resolveAdhoc0'): a project-default context slot (@context.lore@,
--   @context.chaptersCompressed@, ...) or a saved pinned snippet is never
--   staged as a whole @context.writer@ override the way 'buildPreview'
--   works -- it's compiled and resolved directly against the live
--   library table, so a name reference inside it (@context.lore@ itself,
--   say) sees this project's own committed override the identical way a
--   real send would. There's no @path@ for this to frame against, so
--   nothing here excludes "the file currently being written" the way
--   'contextWriterDef' does -- exactly the same caveat 'resolveAdhoc0'
--   already documents for its own callers.
buildAdhocPreview
  :: forall branch r
  .  Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Text -> Sem r PreviewNode
buildAdhocPreview program = do
  v <- resolveAdhoc0 @branch program
  fromRendered <$> runContextValue @branch (renderContext v)
