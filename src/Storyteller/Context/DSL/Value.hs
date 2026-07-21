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
  , ContextLibrary(..)
  , Action(..)
  , liftStore
  , askBranch
  , currentLibrary
  , lookupLibrary
  , currentHead
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
import Data.Text (Text)
import qualified Data.Text as T

import qualified Storage.Core as Core

import Storyteller.Context.DSL.AST (Definition, Name)
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

-- | The shared, once-built library table -- every named definition this
--   application knows about (compiled-in defaults, then whatever the
--   current 'Storyteller.Core.Storage.Contexts' branch overrides or adds
--   on top), fixed for the lifetime of one request/action, resolved by
--   dotted name. This is what makes cross-definition reference by plain
--   identifier possible at all (@contextLore@'s own body calling
--   @loreEntry f@, say): 'Storyteller.Context.DSL.Compile's @EIdent@\/
--   @EApp@ fall back here once a name misses the current definition's own
--   local 'Env'. A found 'Definition' gets compiled and run against the
--   *caller's* own ambient scope (never a fresh one) -- the exact same
--   calling convention an ordinary local 'Storyteller.Context.DSL.Compile.Binding'
--   already has, so a library reference behaves indistinguishably from a
--   parameter or a @let@.
--
--   Deliberately *not* a 'MonadBranch'-shaped typeclass constraint on
--   'Action'\'s own @m@: unlike branch resolution, this is one plain,
--   already-built, immutable value -- fixed once per request, never
--   looked up effectfully mid-computation -- so it travels as an ordinary
--   'Action'-level Reader parameter (see 'runAction'\'s own type) rather
--   than needing a capability 'Action'\'s abstract backend has to satisfy.
newtype ContextLibrary = ContextLibrary (Map Name Definition)

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
lookupLibrary :: Name -> Action (Maybe Definition)
lookupLibrary name = do
  ContextLibrary m <- currentLibrary
  pure (Map.lookup name m)

-- | The commit an 'Action' is currently ambiently positioned at -- see
--   'Core.headHash'. What 'Storyteller.Context.DSL.Compile.currentScope'
--   bootstraps the Reader scope from, with no branch lookup needed.
currentHead :: Action Core.ObjectHash
currentHead = Action (\_lib -> Core.headHash)

-- | @Value = { default :: Thunk [Message], entries :: [(Name, Value)] }@.
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
  }

emptyValue :: Value
emptyValue = Value (pure []) []

-- | A leaf with no children -- "there is no separate leaf type."
leafValue :: [Message] -> Value
leafValue msgs = Value (pure msgs) []

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

