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
--   *any* 'Core.StoreM' @m@, so there's no one concrete monad to name.
--   The one operation that isn't expressible via 'Core.StoreM' alone
--   (resolving a branch name to a commit -- see
--   "Storyteller.Context.DSL.Compile"'s module haddock) doesn't break
--   this: it's threaded through 'runAction' as an explicit parameter,
--   supplied fresh every time an 'Action' actually runs, rather than
--   closed over inside one. A deferred @as@-export whose body crosses
--   into another branch (@in (charname | branch): ...@) is exactly why
--   this matters -- if the resolver were captured at the point the
--   export was *built*, the whole point of 'Action' being safe to store
--   and run later, under whatever resolver its eventual caller supplies,
--   would break.
module Storyteller.Context.DSL.Value
  ( Message(..)
  , messageText
  , BranchResolver
  , Action(..)
  , liftStore
  , askBranch
  , Value(..)
  , emptyValue
  , leafValue
  , messagesText
  , listPaths
  , lookupPath
  ) where

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
type BranchResolver m = BranchName -> m (Maybe Core.ObjectHash)

-- | A deferred computation, generic over any backend that can satisfy
--   'Core.StoreM', plus the one capability that can't be ('BranchResolver',
--   supplied when the 'Action' finally runs, not before). This is
--   @Thunk@ made concrete: constructing an 'Action' performs no effect
--   at all (it's just a function value, same as any other Haskell
--   closure); the effect only happens at 'runAction'.
newtype Action a = Action
  { runAction :: forall m. Core.StoreM m => BranchResolver m -> m a }

instance Functor Action where
  fmap f (Action g) = Action (\rb -> f <$> g rb)

instance Applicative Action where
  pure a = Action (\_ -> pure a)
  Action f <*> Action g = Action (\rb -> f rb <*> g rb)

instance Monad Action where
  Action g >>= f = Action (\rb -> g rb >>= \a -> runAction (f a) rb)

instance MonadFail Action where
  fail msg = Action (\_ -> fail msg)

-- | Lifts an ordinary 'Core.StoreM' computation (one that doesn't need
--   branch resolution -- almost everything: reading a blob, listing a
--   tree) into 'Action'.
liftStore :: (forall m. Core.StoreM m => m a) -> Action a
liftStore act = Action (\_ -> act)

-- | The one 'Action' that actually reaches for the injected
--   'BranchResolver'.
askBranch :: BranchName -> Action (Maybe Core.ObjectHash)
askBranch name = Action (\resolveBranch -> resolveBranch name)

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
