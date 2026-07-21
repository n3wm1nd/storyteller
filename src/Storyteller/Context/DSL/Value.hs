{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | The Context DSL's one runtime type (see "Value model" in
--   @CONTEXT-DSL.md@), and the "one thing that has to be preserved
--   deliberately" from "Implementation strategy": both fields are
--   'Action's, not already-run results, so forcing a leaf is just
--   running one -- ordinary monadic composition, no bespoke recursive
--   walker needed.
--
--   'Value' itself carries no type parameter -- every deferred
--   computation an 'Action' can describe is genuinely generic over
--   *any* 'Core.StoreT' over a 'Core.StoreM' @m@, so there's no one
--   concrete monad to name. 'Action' is deliberately 'Core.StoreT'-shaped
--   (not just bare @m@): every real caller already runs the DSL from
--   inside a storage transaction that's *already positioned* at some
--   commit, so the Reader scope's own bootstrap is just reading that
--   ambient position (see 'Storyteller.Context.DSL.Compile.currentScope')
--   -- no separate lookup needed for "run in whatever I'm already in."
--   The one operation that isn't expressible via 'Core.StoreM'\/'Core.StoreT'
--   alone (resolving a branch *name* to a commit -- see
--   "Storyteller.Context.DSL.Compile"'s module haddock) is a genuinely
--   separate capability, not a storage primitive: real backends happen to
--   provide both, but nothing here assumes that, so it's its own
--   constraint, 'MonadBranch', rather than folded into 'Core.StoreM'.
module Storyteller.Context.DSL.Value
  ( Message(..)
  , messageText
  , MonadBranch(..)
  , Binding(..)
  , bval
  , fn1
  , fn2
  , ContextLibrary(..)
  , Action(..)
  , liftStore
  , askBranch
  , currentLibrary
  , lookupLibrary
  , currentHead
  , Provenance(..)
  , Priority(..)
  , defaultPriority
  , ItemFlag(..)
  , Meta(..)
  , defaultMeta
  , withProvenance
  , Value(..)
  , emptyValue
  , leafValue
  , messagesText
  , listPaths
  , lookupPath
  , namedEntry
  ) where

import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import qualified Storage.Core as Core

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Core.Types (BranchName)

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

-- | Resolves a branch name to its current commit -- see the module
--   haddock for why this can't just be another 'Core.StoreM' operation.
--   A real git backend satisfies both this and 'Core.StoreM' at once (see
--   "Storyteller.Context.DSL.Compile"'s module haddock for the deferred
--   @Sem r@ instance), but nothing here assumes that pairing.
class Monad m => MonadBranch m where
  resolveBranch :: BranchName -> m (Maybe Core.ObjectHash)

-- | What a local name (a @let@\/parameter\/loop variable) resolves to --
--   one constructor, since the DSL itself draws no line between "a
--   value" and "a function": a plain @x = body@ is exactly a 0-arity
--   'Binding' (rule 1, "a file with no head is a 0-ary function -- an
--   ordinary value"). The @[Action Value] -> Value -> Action Value@
--   shape takes the *caller's* current ambient scope as an explicit
--   argument rather than closing over whatever scope was active at
--   definition time -- see "Storyteller.Context.DSL.Compile"'s own
--   Haddock on why. Moved here (rather than living in
--   "Storyteller.Context.DSL.Compile", which imports this module) purely
--   so 'ContextLibrary' -- which needs to hold compiled 'Binding's, not
--   just parsed source -- can be defined in the same module a 'Binding'
--   is, without a cycle.
data Binding = Binding Int ([Action Value] -> Value -> Action Value)

-- | Wraps an already-scoped 'Action' as a 0-arity 'Binding' -- the
--   ordinary "just a value" case, and by far the common one.
bval :: Action Value -> Binding
bval action = Binding 0 (\_ _ -> action)

-- | Wraps a plain, scope-blind Haskell function as a 1-arity 'Binding' --
--   what a host passes a real function in as (the invented-calendar
--   example's own @dateMath@, or a new host-backed primitive like
--   @readconversation@).
fn1 :: (Action Value -> Action Value) -> Binding
fn1 f = Binding 1 go
  where
    go [a] _  = f a
    go args _ = fail $ "fn1: expected exactly 1 argument, got " <> show (length args)

-- | 'fn1', two arguments.
fn2 :: (Action Value -> Action Value -> Action Value) -> Binding
fn2 f = Binding 2 go
  where
    go [a, b] _ = f a b
    go args _   = fail $ "fn2: expected exactly 2 arguments, got " <> show (length args)

-- | The shared, once-built library table -- every named definition this
--   application knows about (compiled-in defaults, then whatever the
--   current 'Storyteller.Core.Storage.Contexts' branch overrides or adds
--   on top), fixed for the lifetime of one request/action, resolved by
--   dotted name. This is what makes cross-definition reference by plain
--   identifier possible at all (@contextWriter@'s own body calling @lore@,
--   say): 'Storyteller.Context.DSL.Compile's @EIdent@\/@EApp@ fall back
--   here once a name misses the current definition's own local 'Env'. A
--   found 'Binding' runs against the *caller's* own ambient scope (never
--   a fresh one) -- the exact same calling convention an ordinary local
--   one already has, so a library reference behaves indistinguishably
--   from a parameter or a @let@.
--
--   Holds compiled 'Binding's, not raw 'Storyteller.Context.DSL.AST.Definition's
--   -- deliberately, so a host-backed primitive (@readconversation@,
--   @embedshallow@ -- real Haskell closures, never expressible as parsed
--   DSL text) can sit in the *same* table as a pure-DSL one
--   (@lore@, @chapters@), both resolved by 'Storyteller.Context.DSL.Compile's
--   'EIdent'\/'EApp' the identical way. A host-backed entry just isn't
--   branch-overridable -- there's no text that could meaningfully replace
--   real Haskell logic, so an override attempt against one has no effect,
--   same treatment a bad arity already gets.
--
--   Deliberately *not* a 'MonadBranch'-shaped typeclass constraint on
--   'Action'\'s own @m@: unlike branch resolution, this is one plain,
--   already-built, immutable value -- fixed once per request, never
--   looked up effectfully mid-computation -- so it travels as an ordinary
--   'Action'-level Reader parameter (see 'runAction'\'s own type) rather
--   than needing a capability 'Action'\'s abstract backend has to satisfy.
newtype ContextLibrary = ContextLibrary (Map Name Binding)

-- | A deferred storage transaction, generic over any backend that can
--   satisfy 'Core.StoreM' and 'MonadBranch', plus one plain Reader
--   parameter for 'ContextLibrary' (see its own Haddock for why that one
--   isn't a third typeclass constraint the way 'MonadBranch' is). This is
--   @Thunk@ made concrete: constructing an 'Action' performs no effect at
--   all (it's just a function value, same as any other Haskell closure);
--   the effect only happens at 'runAction'.
newtype Action a = Action
  { runAction :: forall m. (Core.StoreM m, MonadBranch m) => ContextLibrary -> Core.StoreT m a }

instance Functor Action where
  fmap f (Action g) = Action (\lib -> f <$> g lib)

instance Applicative Action where
  pure a = Action (\_lib -> pure a)
  Action f <*> Action g = Action (\lib -> f lib <*> g lib)

instance Monad Action where
  Action g >>= f = Action (\lib -> g lib >>= \a -> runAction (f a) lib)

instance MonadFail Action where
  fail msg = Action (\_lib -> fail msg)

-- | Lifts an ordinary 'Core.StoreM' computation (one that doesn't need
--   branch resolution or the ambient position -- almost everything:
--   reading a blob, listing a tree) into 'Action'.
liftStore :: (forall m. Core.StoreM m => m a) -> Action a
liftStore act = Action (\_lib -> lift act)

-- | The one 'Action' that actually reaches for 'MonadBranch'.
askBranch :: BranchName -> Action (Maybe Core.ObjectHash)
askBranch name = Action (\_lib -> lift (resolveBranch name))

-- | The 'ContextLibrary' this 'Action' is running against -- whatever
--   'Storyteller.Core.Context.runContextValue' was handed at the one
--   place 'runAction' actually gets called.
currentLibrary :: Action ContextLibrary
currentLibrary = Action (\lib -> pure lib)

-- | 'currentLibrary', narrowed to one name -- what
--   'Storyteller.Context.DSL.Compile's cross-definition 'EIdent'\/'EApp'
--   fallback actually calls.
lookupLibrary :: Name -> Action (Maybe Binding)
lookupLibrary name = do
  ContextLibrary m <- currentLibrary
  pure (Map.lookup name m)

-- | The commit an 'Action' is currently ambiently positioned at -- see
--   'Core.headHash'. What 'Storyteller.Context.DSL.Compile.currentScope'
--   bootstraps the Reader scope from, with no branch lookup needed.
currentHead :: Action Core.ObjectHash
currentHead = Action (\_lib -> Core.headHash)

-- | Where a 'Value' came from -- stamped by @read@ itself (see
--   'withProvenance'), never invented by a filter. Structural: knowing it
--   never requires forcing 'valueDefault'.
data Provenance = Provenance
  { provPath :: FilePath
  , provTick :: Core.ObjectHash
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
withProvenance :: FilePath -> Core.ObjectHash -> Value -> Value
withProvenance path tick v = v { valueMeta = (valueMeta v) { metaProvenance = Just (Provenance path tick) } }

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
data Value = Value
  { valueDefault :: Action [Message]
  , valueEntries :: [(Name, Action Value)]
  , valueMeta    :: Meta
  }

emptyValue :: Value
emptyValue = Value (pure []) [] defaultMeta

-- | A leaf with no children -- "there is no separate leaf type."
leafValue :: [Message] -> Value
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
listPaths :: Value -> Action [Text]
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
lookupPath :: Value -> [Name] -> Action (Maybe Value)
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
namedEntry :: Name -> Value -> Action Value
namedEntry name v = case lookup name (valueEntries v) of
  Just act -> act
  Nothing  -> pure emptyValue

