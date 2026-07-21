{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rendering layer sitting between the Context DSL's own 'Value' and
--   an agent's actual LLM call -- see @CONTEXT-DSL.md@'s own "Rendering"
--   section. 'Value' stays exactly as lazy as it always was; what's new
--   here is that there are two independent ways to walk it, each
--   interpreting that laziness differently:
--
--   * 'renderContext' forces everything -- every 'valueDefault', every
--     'valueEntries' child, recursively -- into a fixed, curated
--     'Context' for direct inclusion in an agent's call.
--   * 'renderFileSystem' forces only shape (a 'Value''s own entries are
--     already a plain list, so walking them costs nothing beyond what
--     building the 'Value' itself already paid for glob expansion) --
--     never a leaf's own content. Backs both a tool-using agent's own
--     browse-then-read capability and a @$context/{path}@-style preview
--     endpoint resolving one path at a time.
--
--   Both mirror 'Value''s own shape (own content, then named children, in
--   order) rather than flattening -- flattening would turn bucket access
--   (e.g. 'Storyteller.Context.DSL.Library.contextCharacter''s
--   @"sheet"@\/@"blurb"@\/@"full"@\/@"journal"@) into string-matching a
--   flat list; keeping the tree makes it a real, checkable structural
--   lookup ('namedChild'), while still deriving 'Foldable' so anything
--   that just wants "everything, in order" gets a structure-blind
--   'Data.Foldable.toList' for free.
module Storyteller.Context.DSL.Rendering
  ( RenderedContext(..)
  , Context
  , ContextItem(..)
  , FileSystemView
  , ContextRef(..)
  , renderContext
  , renderFileSystem
  , renderText
  , renderMessages
  , namedChild
  , listDeferred
  , readRef
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

import qualified UniversalLLM as LLM

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (treeValueOfCommit)
import Storyteller.Context.DSL.Render (dslMessageToLLM)
import Storyteller.Context.DSL.Value

-- | Mirrors 'Value''s own shape exactly (own content, then named
--   children, in order), just with a leaf type @a@ chosen by whichever
--   renderer produced it -- 'ContextItem' for 'renderContext',
--   'ContextRef' for 'renderFileSystem'. Deriving 'Functor'\/'Foldable'\/
--   'Traversable' is what lets a budget-aware pass (not part of this
--   module -- see @CONTEXT-DSL.md@'s own "renderWithBudget", still
--   undesigned) shrink or replace content while the tree shape carries
--   along unchanged, and what lets 'renderText'\/'renderMessages' just be
--   ordinary folds instead of a bespoke recursive walker.
data RenderedContext a = Node
  { rcContent :: [a]
  , rcEntries :: [(Name, RenderedContext a)]
  } deriving (Functor, Foldable, Traversable)

-- | A fully-materialized entry -- 'renderContext' already forced
--   'valueDefault' to get 'ciMessage', and copied the source 'Value'
--   node's own 'Meta' across unchanged (priority\/flags\/provenance are
--   decided by @read@\/a filter, never invented at render time).
data ContextItem = ContextItem
  { ciMessage :: Message
  , ciMeta    :: Meta
  }

type Context = RenderedContext ContextItem

-- | An unforced entry -- 'renderFileSystem' never ran the 'Action' that
--   would produce real content, so all that's here is where it would come
--   from and what's already known about it structurally.
data ContextRef = ContextRef
  { crSource :: Provenance
  , crMeta   :: Meta
  }

type FileSystemView = RenderedContext ContextRef

-- | Forces every 'valueDefault' and every 'valueEntries' child,
--   recursively. One source 'Value' node can produce more than one
--   'ContextItem' if its own default expands to more than one 'Message'
--   (rare, but the type allows it) -- all of them share that node's own
--   'Meta', since there's only one 'Value' node's worth of provenance\/
--   priority\/flags to go around.
renderContext :: Value -> Action Context
renderContext v = do
  msgs     <- valueDefault v
  children <- mapM (\(name, act) -> (,) name . id <$> (renderContext =<< act)) (valueEntries v)
  pure Node
    { rcContent = map (\m -> ContextItem m (valueMeta v)) msgs
    , rcEntries = children
    }

-- | Walks the identical shape 'renderContext' does, but never runs
--   'valueDefault' -- only 'metaProvenance', already known the moment a
--   node was constructed by @read@\/'Storyteller.Context.DSL.Compile.treeValueOfCommit',
--   decides whether this node contributes a 'ContextRef' at all. A node
--   with no provenance (a purely structural container -- an @in@'s
--   narrowed scope, a @for@'s own synthetic accumulator) contributes
--   none, correctly: there's nothing there yet to browse *as a leaf*,
--   only more structure to keep walking into.
--
--   Honest limitation, not yet resolved: 'Provenance' only survives on a
--   node that's *exactly* what 'treeValueOfCommit' produced, untouched.
--   The moment content is folded through
--   'Storyteller.Context.DSL.Compile.runStmts'\/'Storyteller.Context.DSL.Compile.mkValue'
--   (any real multi-statement definition -- even something as small as
--   'Storyteller.Context.DSL.Library.loreEntry', which is just a heading
--   plus one @read@), the result is a fresh 'Value' with 'defaultMeta',
--   because a composed node built from more than one source genuinely
--   has no single provenance to assign -- there is no "the" file it came
--   from. So this is meaningful directly on a Reader scope
--   ('Storyteller.Context.DSL.Compile.currentScope',
--   'Storyteller.Context.DSL.Compile.treeValueOfBranch') or a bare
--   @read@ result, not on an arbitrary library definition's already-
--   composed output.
renderFileSystem :: Value -> Action FileSystemView
renderFileSystem v = do
  children <- mapM (\(name, act) -> (,) name <$> (renderFileSystem =<< act)) (valueEntries v)
  let own = case metaProvenance (valueMeta v) of
        Nothing   -> []
        Just prov -> [ContextRef prov (valueMeta v)]
  pure (Node own children)

-- | The true floor: no role, no model shape, nothing but concatenated
--   content in order -- meaningful even to a target with no turn/role
--   concept at all, which 'renderMessages' could never be. Ignores every
--   field of 'Meta' -- always available regardless of whether anything
--   budget-aware ever runs.
--
--   Reads only 'rcContent', the top-level node -- deliberately *not* a
--   full 'Data.Foldable.toList' walk into 'rcEntries' too. A definition's
--   own default and its named entries are not disjoint by design (see
--   the "Authoring guidance" in @CONTEXT-DSL.md@: the bare default is
--   "the safe answer for a caller that never asks for more," and a named
--   bucket may be exactly the same content re-exported under a key, e.g.
--   'Storyteller.Context.DSL.Library.contextLore''s own per-file @for@
--   loop folds each entry's content into its own top-level default *and*
--   exports it again under that file's own name) -- walking both would
--   double it. Reaching into a specific named bucket is what
--   'namedChild' is for; this always means "the default," never "the
--   whole tree, flattened."
renderText :: Context -> Text
renderText = T.intercalate "\n\n" . map (messageText . ciMessage) . rcContent

-- | The chat-shaped specialization -- built from the identical
--   'rcContent'-only traversal 'renderText' uses, so flattening this back
--   to plain text is 'renderText' by construction, not merely checked to
--   happen to match: each source 'Value' node's own default already
--   decided its own message-by-message role (@User@\/@Assistant@, via
--   @>@\/@<@), so this is an ordinary per-item map, not a re-grouping
--   pass.
renderMessages :: Context -> [LLM.Message m]
renderMessages = map (dslMessageToLLM . ciMessage) . rcContent

-- | Real, checkable structural lookup for a named bucket -- what a
--   caller reaching for a specific piece of a multi-bucket definition
--   (@"sheet"@\/@"blurb"@\/@"full"@\/@"journal"@) uses instead of
--   filtering a flattened list by a string label.
namedChild :: Name -> RenderedContext a -> Maybe (RenderedContext a)
namedChild name = lookup name . rcEntries

-- | Every 'ContextRef' reachable from a 'FileSystemView' -- the menu a
--   tool-using agent browses, free to compute since 'renderFileSystem'
--   already did all the work up front.
listDeferred :: FileSystemView -> [ContextRef]
listDeferred = toList

-- | Forces exactly the one entry a 'ContextRef' points at -- re-resolves
--   its own commit's tree ('treeValueOfCommit', the same primitive every
--   Reader-scope bootstrap already uses) rather than assuming the
--   caller's own ambient position is still the right one to read from,
--   since a 'ContextRef' may have been handed to a tool call well after
--   'renderFileSystem' itself ran.
readRef :: ContextRef -> Action ContextItem
readRef (ContextRef (Provenance path tick) meta) = do
  tree <- treeValueOfCommit tick
  case lookup (T.pack path) (valueEntries tree) of
    Nothing     -> pure (ContextItem (FileRead path "") meta)
    Just action -> do
      v    <- action
      msgs <- valueDefault v
      pure (ContextItem (headOr (FileRead path "") msgs) meta)
  where
    headOr d []      = d
    headOr _ (m : _) = m
