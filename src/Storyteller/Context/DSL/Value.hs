{-# LANGUAGE OverloadedStrings #-}

-- | The Context DSL's one runtime type (see "Value model" in
--   @CONTEXT-DSL.md@), and the "one thing that has to be preserved
--   deliberately" from "Implementation strategy": both fields are
--   monadic *actions*, not already-run results, so forcing a leaf is
--   just running (binding) it -- ordinary monadic composition, no
--   bespoke recursive walker needed.
module Storyteller.Context.DSL.Value
  ( Message(..)
  , messageText
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

-- | @Value = { default :: Thunk [Message], entries :: Map Name Value }@,
--   with 'Thunk' made concrete as "whatever effects @m@ needs to run" --
--   see the module haddock.
data Value m = Value
  { valueDefault :: m [Message]
  , valueEntries :: Map Name (m (Value m))
  }

emptyValue :: Applicative m => Value m
emptyValue = Value (pure []) Map.empty

-- | A leaf with no children -- "there is no separate leaf type."
leafValue :: Applicative m => [Message] -> Value m
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
listPaths :: Monad m => Value m -> m [Text]
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
lookupPath :: Monad m => Value m -> [Name] -> m (Maybe (Value m))
lookupPath v []           = pure (Just v)
lookupPath v (seg : rest) = case Map.lookup seg (valueEntries v) of
  Nothing     -> pure Nothing
  Just action -> action >>= \child -> lookupPath child rest
