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
--   survives translation intact. There is no role-discarding variant of
--   that walk any more -- see 'valueAllMessages' for what replaced the
--   two there used to be, and why.
--
--   A bare 'FileRead' -- role deliberately left undecided by the DSL
--   itself (see 'Storyteller.Context.DSL.Value.Message's own haddock) --
--   is finally decided here: presented as ordinary @User@-role reference
--   material, fenced via 'renderEmbeddedFile' -- the same framing every
--   prose path in this application gives arbitrary file content.
module Storyteller.Context.DSL.Render
  ( dslMessageToLLM
  , valueAllMessages
  , valueMessages
  ) where

import qualified UniversalLLM as LLM

import Storyteller.Context.DSL.Value
import Storyteller.Writer.Agent (renderEmbeddedFile)

-- | A DSL 'Message', finally rendered into the LLM library's own message
--   type -- polymorphic over every capability model @m@ since it only ever
--   produces 'LLM.UserText'\/'LLM.AssistantText', both unconstrained
--   constructors.
dslMessageToLLM :: Message -> LLM.Message m
dslMessageToLLM (FileRead path text) = LLM.UserText (renderEmbeddedFile path text)
dslMessageToLLM (User text)          = LLM.UserText text
dslMessageToLLM (Assistant text)     = LLM.AssistantText text

-- | Every 'Message' reachable from a 'Value' -- its own forced default,
--   then every entry's own default in 'valueEntries' order -- the one
--   traversal every consumer shares.
--
--   This is what callers reach for when they want a 'Value''s content as
--   ordinary data. There used to be two further wrappers here --
--   @valueBlocks@\/@valueCharBlocks@, mapping each 'Message' into a
--   @Text@-shaped @ContextBlock@\/@CharContextBlock@. Both are gone: they
--   discarded the role the 'Message' was carrying (a chapter emitted as
--   'Assistant' by @> read f@ came back indistinguishable from user-role
--   reference material) and cost a prompt-cache boundary by collapsing
--   several messages into one string. Consumers take @['Message']@ and
--   flatten at the point they actually build a call, if at all.
valueAllMessages :: Value r -> Action r [Message]
valueAllMessages v = do
  own      <- valueDefault v
  children <- concat <$> mapM (\(_, act) -> valueDefault =<< act) (valueEntries v)
  pure (own <> children)

valueMessages :: Value r -> Action r [LLM.Message m]
valueMessages v = map dslMessageToLLM <$> valueAllMessages v
