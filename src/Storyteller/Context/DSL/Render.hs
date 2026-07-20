{-# LANGUAGE OverloadedStrings #-}

-- | Turns a Context DSL 'Value' into what a real agent call actually
--   takes -- the "interpretation" @CONTEXT-DSL.md@'s own spec deliberately
--   left out of scope, now that a real caller needs one.
--
--   Three target shapes, one shared walk ('valueAllMessages'): every
--   'Message' reachable from a 'Value', both its own forced default and
--   every entry's own default (so a leaf bucket -- e.g.
--   'Storyteller.Context.DSL.Library.contextCharacter''s own
--   @"sheet"@\/@"blurb"@\/@"journal"@ -- and a container bucket built via
--   @for@\/@as@ -- e.g. that same definition's own @"full"@, or
--   @context.main@'s @"lore"@\/@"chapters"@\/@"other"@ -- both flatten
--   correctly without a caller needing to know which shape a given
--   definition happens to produce). 'valueMessages' preserves each
--   message's own role (@User@\/@Assistant@), which is what lets a
--   definition like 'Storyteller.Context.DSL.Library.contextChapters'
--   build a real alternating-turn sequence (a header, then its content
--   re-tagged @Assistant@ via the widened @>@ -- see
--   'Storyteller.Context.DSL.AST.Expr''s own haddock on 'EAssistant') that
--   survives translation intact; 'valueBlocks'\/'valueCharBlocks' flatten
--   the same walk into plain 'ContextBlock's\/'CharContextBlock's for a
--   slot where role isn't meaningful.
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
  , messageToCharBlock
  , valueAllMessages
  , valueMessages
  , valueBlocks
  , valueCharBlocks
  ) where

import qualified UniversalLLM as LLM

import Storyteller.Context.DSL.Value
import Storyteller.Writer.Agent (CharContextBlock(..), ContextBlock(..), renderEmbeddedFile)

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

-- | Same decision again, into a 'CharContextBlock' -- same reasoning as
--   'messageToBlock', a different target type.
messageToCharBlock :: Message -> CharContextBlock
messageToCharBlock (FileRead path text) = CharContextBlock (renderEmbeddedFile path text)
messageToCharBlock (User text)          = CharContextBlock text
messageToCharBlock (Assistant text)     = CharContextBlock text

-- | Every 'Message' reachable from a 'Value' -- its own forced default,
--   then every entry's own default in 'valueEntries' order -- the one
--   traversal 'valueMessages'\/'valueBlocks'\/'valueCharBlocks' all share,
--   varying only in which per-'Message' renderer they map over the
--   result.
valueAllMessages :: Value -> Action [Message]
valueAllMessages v = do
  own      <- valueDefault v
  children <- concat <$> mapM (\(_, act) -> valueDefault =<< act) (valueEntries v)
  pure (own <> children)

valueMessages :: Value -> Action [LLM.Message m]
valueMessages v = map dslMessageToLLM <$> valueAllMessages v

-- | 'valueMessages', flattened into 'ContextBlock's instead.
valueBlocks :: Value -> Action [ContextBlock]
valueBlocks v = map messageToBlock <$> valueAllMessages v

-- | 'valueMessages', flattened into 'CharContextBlock's instead.
valueCharBlocks :: Value -> Action [CharContextBlock]
valueCharBlocks v = map messageToCharBlock <$> valueAllMessages v
