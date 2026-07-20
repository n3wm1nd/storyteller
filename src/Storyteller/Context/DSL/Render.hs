{-# LANGUAGE OverloadedStrings #-}

-- | Turns a Context DSL 'Value' into what a real agent call actually
--   takes -- the "interpretation" @CONTEXT-DSL.md@'s own spec deliberately
--   left out of scope, now that a real caller needs one.
--
--   Two shapes, not one: 'valueMessages' preserves each entry's own
--   'Message' role (@User@\/@Assistant@), which is what lets a definition
--   like 'Storyteller.Context.DSL.Library.contextChapters' build a real
--   alternating-turn sequence (a header, then its content re-tagged
--   @Assistant@ via the widened @>@ -- see
--   'Storyteller.Context.DSL.AST.Expr''s own haddock on 'EAssistant') that
--   survives translation intact; 'valueBlocks' flattens the same walk into
--   plain 'ContextBlock's for a slot where role isn't meaningful (a leaf
--   like 'Storyteller.Context.DSL.Library.contextStyle', or a container
--   that never re-tags anything @Assistant@).
--
--   A bare 'FileRead' -- role deliberately left undecided by the DSL
--   itself (see 'Storyteller.Context.DSL.Value.Message's own haddock) --
--   is finally decided here: presented as ordinary @User@-role reference
--   material, fenced via 'renderEmbeddedFile', the same framing
--   'Storyteller.Writer.Agent.WorldContext.worldContextOf'\/
--   'Storyteller.Writer.Agent.Continuation.gatherFileContext' already give
--   arbitrary file content today.
module Storyteller.Context.DSL.Render
  ( dslMessageToLLM
  , messageToBlock
  , valueMessages
  , valueBlocks
  ) where

import qualified UniversalLLM as LLM

import Storyteller.Context.DSL.Value
import Storyteller.Writer.Agent (ContextBlock(..), renderEmbeddedFile)

-- | A DSL 'Message', finally rendered into the LLM library's own message
--   type -- polymorphic over every capability model @m@ since it only ever
--   produces 'LLM.UserText'\/'LLM.AssistantText', both unconstrained
--   constructors.
dslMessageToLLM :: Message -> LLM.Message m
dslMessageToLLM (FileRead path text) = LLM.UserText (renderEmbeddedFile path text)
dslMessageToLLM (User text)          = LLM.UserText text
dslMessageToLLM (Assistant text)     = LLM.AssistantText text

-- | Same decision, rendered into a 'ContextBlock' instead -- ignores role,
--   since a 'ContextBlock' slot has nowhere to put one.
messageToBlock :: Message -> ContextBlock
messageToBlock (FileRead path text) = ContextBlock (renderEmbeddedFile path text)
messageToBlock (User text)          = ContextBlock text
messageToBlock (Assistant text)     = ContextBlock text

-- | Every entry's own forced message list, concatenated in
--   'valueEntries' order -- role-preserving, so a container whose entries
--   each carry a real @User@\/@Assistant@ sequence (not just one message)
--   comes out as that same alternating sequence, not one message per
--   entry.
valueMessages :: Value -> Action [LLM.Message m]
valueMessages v = concat <$>
  mapM (\(_, act) -> map dslMessageToLLM <$> (valueDefault =<< act)) (valueEntries v)

-- | 'valueMessages', flattened into 'ContextBlock's instead.
valueBlocks :: Value -> Action [ContextBlock]
valueBlocks v = concat <$>
  mapM (\(_, act) -> map messageToBlock <$> (valueDefault =<< act)) (valueEntries v)
