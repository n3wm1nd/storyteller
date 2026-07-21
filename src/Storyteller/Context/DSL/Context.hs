{-# LANGUAGE FlexibleInstances #-}

-- | 'Context': the composable currency for assembling what a call site
--   hands an agent, and 'ToBinding': the typeclass that lets plain Haskell
--   values cross into a @['dsl'| ... |]@ block's own parameter list without
--   hand-wrapping (@'Storyteller.Context.DSL.Compile.bval'@, @fn1@, ...) at
--   every call site.
--
--   == Context
--
--   A 'Value' already forces to @['Storyteller.Context.DSL.Value.Message']@
--   via 'Storyteller.Context.DSL.Render.valueAllMessages'; 'Context' is
--   just that shape given a 'Monoid' so several already-DSL-sourced
--   fragments (a @context.main@ bucket, a character's own context, a
--   client's pinned selection) compose with plain @('<>')@ instead of the
--   @concat \<$\> mapM ... [...]@ boilerplate that pattern used to need at
--   every call site. 'toContext' lifts a not-yet-forced 'Action' 'Value'
--   straight in (so @toContext (namedEntry \"lore\" mainVal) \<> ...@ needs
--   no intermediate @\<-@ bind), 'user'\/'assistant' lift a literal string
--   in the same currency, and 'runContext' is the one place that actually
--   forces it -- still returning plain DSL 'Message's, not yet bound to
--   any concrete model (see 'Storyteller.Context.DSL.Render.dslMessageToLLM'
--   for that next, separate step, still deferred to whichever agent
--   function is about to call @queryLLM@ -- see
--   'Storyteller.Writer.Agent.Continuation.proseAgent's own Haddock on why
--   binding early is wrong).
--
--   == ToBinding
--
--   Every @['dsl'| ... |]@ definition already accepts a curried
--   'Storyteller.Context.DSL.Compile.Binding' per declared parameter, but
--   until now a caller had to build one by hand -- @bval@ for a plain
--   'Action' 'Value', @fn1@\/@fn2@ for a host function. 'ToBinding'
--   overloads that construction: the quasiquoter itself (see
--   "Storyteller.Context.DSL.QQ") now applies 'toBinding' to every
--   argument it's given, so a definition parameter's real Haskell type is
--   whatever the caller actually has -- 'Text', 'Context', an 'Action'
--   'Value', or a host function -- never a 'Binding' spelled out at the
--   call site. The wire-level registry
--   ("Storyteller.Context.DSL.Library"'s @defaultLibrary@,
--   'Storyteller.Core.Context.resolveContextQuery') still deals in plain
--   'Binding's directly -- that boundary is runtime-arity-checked against
--   parsed override text, not something a Haskell type could pin anyway --
--   'ToBinding' 'Binding' (@= id@) is what keeps that path compiling
--   unchanged.
module Storyteller.Context.DSL.Context
  ( Context
  , toContext
  , ownContext
  , user
  , assistant
  , runContext
  , ToBinding(..)
  , FromArg(..)
  , toBindingFn1
  ) where

import Data.Text (Text)

import Storyteller.Context.DSL.Compile (Binding(..), bval, fn1)
import Storyteller.Context.DSL.Render (valueAllMessages)
import Storyteller.Context.DSL.Value (Action, Message(..), Value, leafValue, messagesText, valueDefault)

-- | An ordered, composable sequence of DSL 'Message's, still deferred
--   (each fragment's own 'Action' hasn't run yet) and still model-agnostic
--   (no 'UniversalLLM.Message' binding). See the module Haddock.
newtype Context = Context { unContext :: Action [Message] }

instance Semigroup Context where
  Context a <> Context b = Context ((<>) <$> a <*> b)

instance Monoid Context where
  mempty = Context (pure [])

-- | Lift a not-yet-forced 'Value' straight into 'Context', forcing it
--   (via 'Storyteller.Context.DSL.Render.valueAllMessages') only once the
--   whole composed 'Context' is itself forced by 'runContext'.
toContext :: Action Value -> Context
toContext act = Context (valueAllMessages =<< act)

-- | Lift an already-resolved 'Value''s own bare default into 'Context',
--   ignoring its named entries entirely -- unlike 'toContext', which
--   walks *into* a Value's entries too (right for a plain container like
--   @contextLore@, whose own default is empty and whose entries are the
--   real content). What a caller wants when a definition's own top-level
--   default is already the exact combined shape it needs (see
--   'Storyteller.Context.DSL.Library.contextMain''s own bare re-emit
--   statements, which build exactly this on purpose), and its named
--   entries exist for a completely different caller's own separate
--   purpose (@chatWriter@ picking @"style"@ out on its own).
ownContext :: Value -> Context
ownContext v = Context (valueDefault v)

-- | A literal string, in the same currency -- what lets genuinely static
--   framing text compose with DSL-sourced fragments via the same
--   @('<>')@, rather than needing a separate "trailing message" step.
user, assistant :: Text -> Context
user      t = Context (pure [User t])
assistant t = Context (pure [Assistant t])

-- | Force a composed 'Context' down to plain DSL 'Message's -- the one
--   place anything here actually runs. Still not bound to a concrete
--   model; see the module Haddock.
runContext :: Context -> Action [Message]
runContext = unContext

-- | Overloads what can fill one @['dsl'| ... |]@ parameter position --
--   see the module Haddock. Every instance here already had a
--   hand-written equivalent somewhere in this codebase before 'ToBinding'
--   existed; this just makes the quasiquoter apply the right one
--   automatically instead of a call site choosing by hand.
class ToBinding a where
  toBinding :: a -> Binding

-- | Already a 'Binding' -- the identity case, and what keeps every
--   pre-existing @bval x@\/@fn1 f@\/@'Storyteller.Context.DSL.Library.toBinding1' f@
--   call site (the wire-level registry boundary, which stays
--   'Binding'-typed on purpose -- see the module Haddock) compiling
--   unchanged: 'Binding' itself is just as valid an argument as anything
--   else with a 'ToBinding' instance.
instance ToBinding Binding where
  toBinding = id

-- | A literal string -- the common case for a simple parameter like a
--   character name or a target path.
instance ToBinding Text where
  toBinding t = bval (pure (leafValue [User t]))

-- | An already-composed 'Context' -- spliced in as one opaque, already-
--   ordered 'Value' with no further structure (nothing to @for@\/glob into
--   -- see 'toContext' for the direction that matters, DSL 'Value' into
--   'Context'; this is deliberately not that same isomorphism run
--   backward for anything richer).
instance ToBinding Context where
  toBinding ctx = bval (leafValue <$> unContext ctx)

-- | A plain, not-yet-forced 'Value' -- the ordinary case for composing one
--   definition out of another's own result (@lore@\/@chapters@\/@style@ in
--   'Storyteller.Context.DSL.Library.contextMain', say), where the callee
--   needs the full structure (@in lore: for f in **/*: ...@), not just
--   'Context''s flattened message sequence.
instance ToBinding (Action Value) where
  toBinding = bval

-- | A host function of one argument -- what a DSL definition calls
--   through to Haskell logic too story-specific for the DSL's own
--   primitives (see @CONTEXT-DSL.md@'s invented-calendar example).
instance ToBinding (Action Value -> Action Value) where
  toBinding = fn1

-- | The dual of 'ToBinding': how to turn one runtime call argument (an
--   already-supplied 'Action' 'Value') back into a plain Haskell value a
--   compiled-in definition's own parameter expects. Only 'Text' has a
--   real instance today -- every real 1-arity @context.*@ definition's
--   own parameter is a plain identifier\/path -- but this stays open to
--   more as 'toBindingFn1' grows real callers needing them.
class FromArg a where
  fromArg :: Action Value -> Action a

instance FromArg Text where
  fromArg act = messagesText <$> (valueDefault =<< act)

-- | Re-curries a plain, concretely-typed 1-arity Haskell function (e.g.
--   @'Storyteller.Context.DSL.Library.contextWriter' :: Text -> Action
--   Value@) back into the interpreter's own runtime-dispatched 'Binding'
--   shape -- the inverse of 'toBinding'. Needed wherever a compiled-in
--   definition, already as plainly typed as 'ToBinding' lets a *caller*
--   write it, still has to cross into something fundamentally
--   'Binding'-shaped: 'Storyteller.Core.Context.resolveContext1', which
--   needs the arity tag *before* it knows whether an override even
--   replaces this definition.
toBindingFn1 :: FromArg a => (a -> Action Value) -> Binding
toBindingFn1 f = Binding 1 go
  where
    go [a] _  = f =<< fromArg a
    go args _ = fail ("expected exactly 1 argument, got " <> show (length args))
