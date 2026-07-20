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
  , Action(..)
  , liftStore
  , askBranch
  , currentHead
  , Value(..)
  , emptyValue
  , leafValue
  , messagesText
  , listPaths
  , lookupPath
  ) where

import Control.Monad.Trans.Class (lift)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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

-- | A deferred storage transaction, generic over any backend that can
--   satisfy 'Core.StoreM' and 'MonadBranch'. This is @Thunk@ made
--   concrete: constructing an 'Action' performs no effect at all (it's
--   just a function value, same as any other Haskell closure); the
--   effect only happens at 'runAction'.
newtype Action a = Action
  { runAction :: forall m. (Core.StoreM m, MonadBranch m) => Core.StoreT m a }

instance Functor Action where
  fmap f (Action g) = Action (f <$> g)

instance Applicative Action where
  pure a = Action (pure a)
  Action f <*> Action g = Action (f <*> g)

instance Monad Action where
  Action g >>= f = Action (g >>= \a -> runAction (f a))

instance MonadFail Action where
  fail msg = Action (fail msg)

-- | Lifts an ordinary 'Core.StoreM' computation (one that doesn't need
--   branch resolution or the ambient position -- almost everything:
--   reading a blob, listing a tree) into 'Action'.
liftStore :: (forall m. Core.StoreM m => m a) -> Action a
liftStore act = Action (lift act)

-- | The one 'Action' that actually reaches for 'MonadBranch'.
askBranch :: BranchName -> Action (Maybe Core.ObjectHash)
askBranch name = Action (lift (resolveBranch name))

-- | The commit an 'Action' is currently ambiently positioned at -- see
--   'Core.headHash'. What 'Storyteller.Context.DSL.Compile.currentScope'
--   bootstraps the Reader scope from, with no branch lookup needed.
currentHead :: Action Core.ObjectHash
currentHead = Action Core.headHash

-- | @Value = { default :: Thunk [Message], entries :: Map Name Value }@.
data Value = Value
  { valueDefault :: Action [Message]
  , valueEntries :: Map Name (Action Value)
  }

emptyValue :: Value
emptyValue = Value (pure []) Map.empty

-- | A leaf with no children -- "there is no separate leaf type."
leafValue :: [Message] -> Value
leafValue msgs = Value (pure msgs) Map.empty

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
  parts <- mapM childPaths (Map.toList (valueEntries v))
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
lookupPath v (seg : rest) = case Map.lookup seg (valueEntries v) of
  Nothing     -> pure Nothing
  Just action -> action >>= \child -> lookupPath child rest
