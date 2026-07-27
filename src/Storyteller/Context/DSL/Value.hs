{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Context DSL's one runtime type (see "Value model" in
--   @CONTEXT-DSL.md@), and the "one thing that has to be preserved
--   deliberately" from "Implementation strategy": both fields are
--   'Action's, not already-run results, so forcing a leaf is just
--   running one -- ordinary monadic composition, no bespoke recursive
--   walker needed.
--
--   'Value' carries a Polysemy effect row @r@ -- what a DSL library
--   function actually needs is named vocabulary from
--   "Storyteller.Core.ContentEffects" (@Presence@, @JournalAccess@,
--   @ConversationAccess@, ...) plus 'Runix.FileSystem' for plain file
--   reads, not a concrete storage monad, so 'Action'
--   is @ContextLibrary r -> Sem r a@, not 'Core.StoreT'-shaped the way it
--   used to be. Deliberately *not* given a closed, fixed @Members@ list
--   here -- see @project_mcp_export_effect_boundary@: a host builds its
--   own concrete @library :: ContextLibrary r@ at whatever @r@ its own
--   interpreter stack provides, and a DSL library function can only be
--   *included* in that library if it typechecks against that @r@ --
--   there is no separate "does this backend support this DSL program"
--   check anywhere, because the library's own construction already is
--   that check.
module Storyteller.Context.DSL.Value
  ( Message(..)
  , messageText
  , Binding(..)
  , bval
  , fn1
  , fn2
  , Action(..)
  , liftSem
  , Provenance(..)
  , Priority(..)
  , defaultPriority
  , ItemFlag(..)
  , Meta(..)
  , defaultMeta
  , withProvenance
  , Value(..)
  , ForcedValue(..)
  , forceValue
  , forcedValue
  , emptyValue
  , leafValue
  , messagesText
  , listPaths
  , lookupPath
  , namedEntry
  ) where

import Polysemy (Member, Sem)
import Polysemy.Fail (Fail)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T


import Storyteller.Context.DSL.AST (Name)

-- | The three, and only three, ways a message gets constructed (see
--   "Value model"). 'FileRead' deliberately carries no role -- what it
--   becomes is an interpreter decision, out of scope here.
data Message
  = FileRead FilePath Text
  | User Text
  | Assistant Text
  deriving (Eq, Show)

messageText :: Message -> Text
messageText (FileRead _ t) = t
messageText (User t)       = t
messageText (Assistant t)  = t

-- | What a local name (a @let@\/parameter\/loop variable) resolves to --
--   one constructor, since the DSL itself draws no line between "a
--   value" and "a function": a plain @x = body@ is exactly a 0-arity
--   'Binding' (rule 1, "a file with no head is a 0-ary function -- an
--   ordinary value"). The @[Action r (Value r)] -> Value r -> Action r
--   (Value r)@ shape takes the *caller's* current ambient scope as an
--   explicit argument rather than closing over whatever scope was active
--   at definition time -- see "Storyteller.Context.DSL.Compile"'s own
--   Haddock on why. Moved here (rather than living in
--   "Storyteller.Context.DSL.Compile", which imports this module) purely
--   so 'ContextLibrary' -- which needs to hold compiled 'Binding's, not
--   just parsed source -- can be defined in the same module a 'Binding'
--   is, without a cycle.
data Binding r = Binding Int ([Action r (Value r)] -> Value r -> Action r (Value r))

-- | Wraps an already-scoped 'Action' as a 0-arity 'Binding' -- the
--   ordinary "just a value" case, and by far the common one.
bval :: Action r (Value r) -> Binding r
bval action = Binding 0 (\_ _ -> action)

-- | Wraps a plain, scope-blind Haskell function as a 1-arity 'Binding' --
--   what a host passes a real function in as (the invented-calendar
--   example's own @dateMath@, or a new host-backed primitive like
--   @readconversation@).
fn1 :: Member Fail r => (Action r (Value r) -> Action r (Value r)) -> Binding r
fn1 f = Binding 1 go
  where
    go [a] _  = f a
    go args _ = Action (fail $ "fn1: expected exactly 1 argument, got " <> show (length args))

-- | 'fn1', two arguments.
fn2 :: Member Fail r => (Action r (Value r) -> Action r (Value r) -> Action r (Value r)) -> Binding r
fn2 f = Binding 2 go
  where
    go [a, b] _ = f a b
    go args _   = Action (fail $ "fn2: expected exactly 2 arguments, got " <> show (length args))

-- | A deferred computation against whatever Polysemy effect row @r@ the
--   host running this DSL provides. This is @Thunk@ made concrete:
--   constructing an 'Action' performs no effect at all (it's just a
--   function value, same as any other Haskell closure); the effect only
--   happens at 'runAction'.
--
--   Carries no library-lookup mechanism of its own -- unlike the previous
--   design (a @ContextLibrary r@ Reader parameter, consulted live at every
--   identifier reference), cross-definition name resolution now happens
--   once, at compile time, in 'Storyteller.Context.DSL.Compile.definitionBinding':
--   a compiled 'Binding' already has every 'Storyteller.Context.DSL.AST.EIdent'\/
--   'Storyteller.Context.DSL.AST.EApp' inside its own body resolved to a
--   concrete 'Binding' value, baked into the closure. This is what makes
--   an override referencing its own name resolve to whatever that name
--   meant *before* the override (the previous default, or a compile
--   failure if there wasn't one) rather than looping into itself -- see
--   'Storyteller.Core.Context.buildContextLibrary's own Haddock for the
--   fixed compile order this relies on.
newtype Action r a = Action { runAction :: Sem r a }

instance Functor (Action r) where
  fmap f (Action g) = Action (f <$> g)

instance Applicative (Action r) where
  pure = Action . pure
  Action f <*> Action g = Action (f <*> g)

instance Monad (Action r) where
  Action g >>= f = Action (g >>= runAction . f)

instance Member Fail r => MonadFail (Action r) where
  fail = Action . fail

-- | Lifts an arbitrary 'Sem' computation into 'Action' -- the only way in,
--   since 'Action's own constructor is exactly @Sem r a@. Every DSL
--   library function that reaches for a named effect
--   ('Storyteller.Core.ContentEffects.charactersPresent', 'askBranch', ...)
--   goes through this; there is no separate "storage-specific" lift the
--   way 'liftStore' used to be, because nothing here is storage-specific
--   any more -- it's just entering the underlying effect monad.
liftSem :: Sem r a -> Action r a
liftSem = Action

-- | Where a 'Value' came from -- stamped by @read@ itself (see
--   'withProvenance'), never invented by a filter. Structural: knowing it
--   never requires forcing 'valueDefault'.
--
--   The path, and nothing else. It used to also carry a
--   'Storage.Core.ObjectHash' position, so a reference could be re-read
--   later against the commit it came from. That position only existed
--   because the scope builder was handed one; now a scope is built from an
--   ordinary 'Runix.FileSystem.FileSystem', which has no position to
--   offer and shouldn't need one. Re-reading is
--   'Storyteller.Context.DSL.Rendering.readRef', which resolves a
--   reference against the scope it was produced from -- an argument the
--   caller already has, rather than a position smuggled through every
--   leaf.
--
--   That does change one thing honestly: a reference is no longer pinned
--   to the exact bytes it was made from. Nothing depends on that today
--   (the whole browse-then-read surface has no callers yet), and the
--   caller that eventually does can hold its own scope for as long as it
--   needs the references to stay meaningful.
newtype Provenance = Provenance
  { provPath :: FilePath
  } deriving (Eq, Show)

-- | Higher survives longer under budget pressure. Ordinary 'Int' wrapped
--   only so a stray positional argument can't be mistaken for one --
--   see "Newtype wrapping threshold" project convention.
newtype Priority = Priority Int deriving (Eq, Ord, Show)

defaultPriority :: Priority
defaultPriority = Priority 0

-- | What a budget-aware renderer (not this module -- see
--   'Storyteller.Context.DSL.Rendering') is allowed to do to a node under
--   pressure. Set by a filter (@pinned@, @summarizable@), never inferred.
data ItemFlag = Droppable | Summarizable | Pinned deriving (Eq, Ord, Show)

-- | Orthogonal to everything else a 'Value' carries -- most code never
--   touches this field. The one channel a rendering step (outside this
--   module) learns anything beyond content and structure through.
data Meta = Meta
  { metaProvenance :: Maybe Provenance
  , metaPriority   :: Priority
  , metaFlags      :: Set ItemFlag
  } deriving (Eq, Show)

defaultMeta :: Meta
defaultMeta = Meta Nothing defaultPriority Set.empty

-- | Stamps a 'Value' with where it came from -- what @read@'s own
--   resolution (see "Storyteller.Context.DSL.Compile") calls on every
--   entry it builds from a commit's tree, never something a filter
--   invents for itself.
withProvenance :: FilePath -> Value r -> Value r
withProvenance path v = v { valueMeta = (valueMeta v) { metaProvenance = Just (Provenance path) } }

-- | A 'Value' with every thunk already run -- plain data, in no effect row
--   at all.
--
--   This is what lets a 'Value' built inside one interpreter scope be
--   returned from it. A 'Value' is thunks in a particular row @r@, so a
--   scope built inside (say) a branch filesystem opened for one character
--   cannot outlive that interpreter: its unforced leaves would resolve
--   against whatever filesystem happened to be live when they were finally
--   forced, which is a wrong answer rather than an error. Forcing at the
--   boundary is what makes crossing scopes sound, and 'ForcedValue' is the
--   shape that survives the crossing.
--
--   Deliberately only used at scope boundaries. Forcing a whole 'Value'
--   reads every file it names, which is exactly what the DSL's laziness
--   exists to avoid on the main path -- see
--   'Storyteller.Context.DSL.Library.contextOtherDef', where @exclude@
--   consumes @context.lore@ and @context.chapters@ for their /keys/ and
--   never forces their contents. Crossing into another branch is the case
--   where that trade is worth making, because the alternative is silently
--   reading the wrong branch.
data ForcedValue = ForcedValue
  { fvDefault :: [Message]
  , fvEntries :: [(Name, ForcedValue)]
  , fvMeta    :: Meta
  } deriving (Eq, Show)

-- | Run every thunk, depth-first, in the row the 'Value' belongs to.
forceValue :: Value r -> Action r ForcedValue
forceValue v =
  ForcedValue
    <$> valueDefault v
    <*> mapM (\(name, act) -> (,) name <$> (forceValue =<< act)) (valueEntries v)
    <*> pure (valueMeta v)

-- | The already-forced result as a 'Value' again, in any row -- every
--   thunk is now a 'pure', so it carries no effects and can be handed to a
--   caller in a different scope than the one that built it.
forcedValue :: ForcedValue -> Value r
forcedValue (ForcedValue d es m) = Value
  { valueDefault = pure d
  , valueEntries = [ (name, pure (forcedValue e)) | (name, e) <- es ]
  , valueMeta    = m
  }

-- | @Value = { default :: Thunk [Message], entries :: [(Name, Value)], meta :: Meta }@.
--   An ordered association list, not a 'Data.Map.Strict.Map' -- order is
--   a real, preserved, and freely reassignable property of a 'Value'
--   (construction order by default: @as@-export declaration order,
--   'Storage.Core.WorkingTree'\'s own order for a branch's tree, ...),
--   not something a 'Map' would collapse to key order regardless of how
--   the entries were actually produced. This is what makes @sortBy@ (a
--   real filter, not a stub) and a non-lexical glob order (chapter
--   numbering, say) expressible at all -- both are just "produce this
--   list in a different order," ordinary list operations on already-pure
--   data, not a capability bolted on from outside 'Value'.
data Value r = Value
  { valueDefault :: Action r [Message]
  , valueEntries :: [(Name, Action r (Value r))]
  , valueMeta    :: Meta
  }

emptyValue :: Value r
emptyValue = Value (pure []) [] defaultMeta

-- | A leaf with no children -- "there is no separate leaf type."
leafValue :: [Message] -> Value r
leafValue msgs = Value (pure msgs) [] defaultMeta

-- | Flattens a message list to plain text, ignoring role -- what
--   filters and interpolation operate on (see "Value model": "Filters
--   and interpolation that need plain text ... work on the underlying
--   message content, ignoring role").
messagesText :: [Message] -> Text
messagesText = T.intercalate "\n" . map messageText

-- | Every full, slash-joined path reachable by walking 'valueEntries'
--   recursively -- what glob matching needs (see "Iteration and glob":
--   "pattern-matches against the entries-map keys of whatever tree is
--   currently in Reader scope"). Forces the *structure* of every
--   descendant (their own 'valueEntries', to keep walking) but never
--   their 'valueDefault' -- so listing a tree this size is cheap
--   regardless of how much content sits in it, exactly the property
--   'for'\'s own loop-variable laziness relies on downstream.
listPaths :: Value r -> Action r [Text]
listPaths v = do
  parts <- mapM childPaths (valueEntries v)
  pure (concat parts)
  where
    childPaths (name, action) = do
      child <- action
      subs  <- listPaths child
      pure $ if null subs then [name] else map (\s -> name <> "/" <> s) subs

-- | Descends into 'valueEntries' one path segment at a time ("looks it
--   up by key, recursively" -- rule 3). 'Nothing' on a missing segment
--   -- callers turn that into an empty 'Value', per the spec's own
--   "absence, not an error" rule for a @read@/glob that finds nothing.
lookupPath :: Value r -> [Name] -> Action r (Maybe (Value r))
lookupPath v []           = pure (Just v)
lookupPath v (seg : rest) = case lookup seg (valueEntries v) of
  Nothing     -> pure Nothing
  Just action -> action >>= \child -> lookupPath child rest

-- | One named top-level entry out of a container's own 'valueEntries' --
--   'emptyValue' when absent, matching @read@\'s own "absence, not an
--   error" convention (rule 3) rather than failing the whole call over a
--   definition that simply doesn't export a given bucket. What a caller
--   picking apart a multi-bucket definition's result (e.g.
--   'Storyteller.Context.DSL.Library.contextCharacter''s own
--   @"sheet"@\/@"blurb"@\/@"full"@\/@"journal"@\/@"journalFull"@) reaches
--   for, instead of a bespoke @lookup name (valueEntries v)@ at every call
--   site.
namedEntry :: Name -> Value r -> Action r (Value r)
namedEntry name v = case lookup name (valueEntries v) of
  Just act -> act
  Nothing  -> pure emptyValue

