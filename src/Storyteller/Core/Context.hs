{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Context DSL definition storage -- 'Storyteller.Core.Prompt.PromptStorage'
-- for the Context DSL (see @CONTEXT-DSL.md@'s own "a branch-hosted,
-- override-with-fallback function library", the one design question that
-- module's own "Open/deferred" section left unresolved). Every call site
-- still carries a compiled-in default 'Binding' (see
-- "Storyteller.Context.DSL.Library"'s @context.*@ definitions), so the
-- system behaves identically until someone actually commits an override --
-- the same "works with no branch content at all" contract 'PromptStorage'
-- already established.
--
-- Storage is a single, dedicated 'Contexts' branch — project-scoped, not
-- tied to any content or character branch, mirroring 'Prompts' exactly. A
-- dotted key like @"context.main"@ doubles as a file path
-- (@context/main.dsl@, no leading slash -- "Storage.FS"/"Storage.Ops" do
-- exact string lookups against the tree with no path normalization at all,
-- so this has to match whatever convention every other Context DSL path in
-- this codebase already uses) in that branch, so overriding a definition is
-- just committing a @.dsl@ file there.
--
-- An override's own declared arity ('Storyteller.Context.DSL.AST.defParams')
-- has to match the default 'Binding'\'s arity exactly — there's no way to
-- change how many parameters a call site passes just by editing branch
-- content, since every real call site is ordinary typechecked Haskell (see
-- "Storyteller.Context.DSL.Library"'s own module haddock on why composition
-- between @context.*@ pieces is plain parameter passing, not free-identifier
-- resolution). A malformed or wrong-arity override silently falls back to
-- the default, the same tradeoff 'Storyteller.Core.Prompt.GetConfig' already
-- makes for unparseable YAML — loud enough to notice in the branch's own
-- history (a bad commit that visibly didn't take), not a runtime crash for
-- every query until it's fixed.
module Storyteller.Core.Context
  ( ContextStorage(..)
  , getContextDefinition
  , interpretContextStorageFS
  , interpretContextStorageMap
  , contextsBranchName
  , resolveContextOverride
  , resolveContextQuery
  , runContextValue
  , runContextBinding0
  , runContextBinding1
  ) where

import Control.Monad (void)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail (Fail)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.FS as FS
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchOp, runBranchOpGit, runStorage)
import Storyteller.Core.Runtime (Contexts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))

import Storyteller.Context.DSL.AST (Definition(..), Name)
import Storyteller.Context.DSL.Compile (Binding(..), bval, runDefinition)
import Storyteller.Context.DSL.Parser (parseDefinition, renderParseErr)
import Storyteller.Context.DSL.Value (Action, Value, Message(User), emptyValue, leafValue, runAction)

data ContextStorage (m :: Type -> Type) a where
  -- | Look up @name@'s own override, falling back to the caller-supplied
  --   default 'Binding' when none exists (or one exists but doesn't parse,
  --   or parses with the wrong arity).
  GetContextDefinition :: Name -> Binding -> ContextStorage m Binding

makeSem ''ContextStorage

-- | The one well-known branch name this module owns -- exported the same
--   way 'Storyteller.Core.Prompt.promptsBranchName' is, so
--   'Storyteller.Writer.Branches.classifyBranch' can recognize it without
--   duplicating the literal.
contextsBranchName :: BranchName
contextsBranchName = BranchName "contexts"

-- | Parses @src@ (an override's raw file content) and checks its arity
--   against @def@'s own before accepting it -- pure, so both interpreters
--   below (and any future one) share exactly this decision. 'Nothing' input
--   (no override committed) and a parse\/arity failure both fall back to
--   @def@ identically; the two are kept as separate cases only so a caller
--   with logging\/telemetry can eventually tell "no override" apart from
--   "broken override" without this function itself needing an effect to
--   report through.
--
--   The accepted override runs via 'runDefinition', not 'compileDefinition'
--   against the 'Value' scope argument a 'Binding'\'s own function is
--   handed -- matching exactly how every QQ-spliced @context.*@ definition
--   in "Storyteller.Context.DSL.Library" resolves its own scope (see
--   'Storyteller.Context.DSL.QQ'\'s own haddock: "the scope is always
--   whatever commit is ambient when the returned Action finally runs").
--   An override committed to the 'Contexts' branch still runs positioned
--   at the *caller's* branch, never the 'Contexts' branch itself.
resolveContextOverride :: Binding -> Maybe Text -> Binding
resolveContextOverride def Nothing = def
resolveContextOverride def@(Binding defaultArity _) (Just src) =
  case parseDefinition "<context override>" src of
    Left _ -> def
    Right parsedDef
      | length (defParams parsedDef) /= defaultArity -> def
      | otherwise -> Binding defaultArity $ \args _scope ->
          runDefinition parsedDef (map bval args)

-- | Real interpreter: reads overrides from the dedicated 'Contexts' branch,
--   creating it on first use. A key resolves to @/\<dots-as-slashes\>.dsl@;
--   a missing file falls back to the caller's default 'Binding'.
interpretContextStorageFS
  :: Members '[Git, StoryStorage, Fail] r
  => Sem (ContextStorage ': r) a
  -> Sem r a
interpretContextStorageFS action = do
  getBranch contextsBranchName >>= \case
    Just _  -> return ()
    Nothing -> void (createBranch contextsBranchName)
  interpret (\case
    GetContextDefinition name def -> runBranchOpGit @Contexts contextsBranchName $ do
      let path = T.unpack (T.replace "." "/" name) <> ".dsl"
      runStorage @Contexts (do
        fileExists <- Ops.exists path
        if fileExists
          then resolveContextOverride def . Just . TE.decodeUtf8 <$> FS.readFile path
          else return def)
    ) action

-- | Test/pure interpreter: resolves from a fixed map of override source
--   text, falling back to the caller's default on miss -- no filesystem or
--   branch involved, mirroring
--   'Storyteller.Core.Prompt.interpretPromptStorageMap'.
interpretContextStorageMap
  :: Map Name Text
  -> Sem (ContextStorage ': r) a
  -> Sem r a
interpretContextStorageMap overrides = interpret $ \case
  GetContextDefinition name def -> return (resolveContextOverride def (Map.lookup name overrides))

-- | Resolves a query's own inline program text with priority over
--   'getContextDefinition''s branch-override-then-default chain -- "you
--   send it with the query, but a persistent branch exists for
--   subfunctions and settings; the sent-with-the-query one is
--   authoritative" (the project chat that settled this). Unlike
--   'resolveContextOverride', a malformed or wrong-arity query fails
--   loudly rather than silently downgrading: a client just submitted this
--   text this call, so silently falling back to some other definition
--   would look like the edit did nothing, where a stored branch override
--   failing quietly (still reachable, still fixable, not something a user
--   is actively watching the result of) is the right call.
--
--   Takes @Maybe Text@, not @Text@ defaulting to @\"\"@: 'Nothing' (the
--   wire field genuinely absent) is what falls through to
--   'getContextDefinition'; @Just \"\"@ is a real, if degenerate, program
--   (parses to an empty definition -- "include nothing", a completely
--   different thing from "no override was sent") and gets parsed and run
--   like any other query text, not silently reinterpreted as "use the
--   default".
resolveContextQuery :: (Member ContextStorage r, Member Fail r) => Name -> Binding -> Maybe Text -> Sem r Binding
resolveContextQuery name def Nothing = getContextDefinition name def
resolveContextQuery _name (Binding arity _) (Just queryText) = case parseDefinition "<query context>" queryText of
  Left err -> fail (T.unpack (renderParseErr err))
  Right parsedDef
    | length (defParams parsedDef) /= arity -> fail $
        "context program: expected arity " <> show arity
          <> ", got " <> show (length (defParams parsedDef))
    | otherwise -> pure $ Binding arity $ \args _scope ->
        runDefinition parsedDef (map bval args)

-- | Runs a Context DSL 'Action' positioned at whatever commit @branch@'s
--   own 'Storyteller.Core.Git.BranchOp' scope is currently ambient at
--   (accounting for an in-progress rebase\/transaction the same way any
--   other read in that scope would) -- via 'Storage.Core.runStoreT'
--   directly against 'Sem r' itself, not 'runStorage'\/'BranchOp'
--   dispatch (see 'Storyteller.Core.Git''s own @MonadBranch (Sem r)@
--   instance haddock for why 'runStorage''s type can't accept an
--   'Action' at all: the DSL is read-only, so it never needs 'BranchOp'
--   write-buffering).
runContextValue :: forall branch r a. Members '[BranchOp branch, Git, StoryStorage, Fail] r => Action a -> Sem r a
runContextValue act = do
  h <- runStorage @branch Core.headHash
  fst <$> Core.runStoreT h (runAction act)

-- | 'runContextValue' for a 0-arity 'Binding' specifically -- every real
--   context-program call site resolves to one of these (the scope
--   argument a 'Binding'\'s own function takes is always ignored for a
--   top-level definition, per 'resolveContextOverride'\/'resolveContextQuery'
--   both routing through 'runDefinition' -- see their own haddocks), so
--   this is what every caller actually wants rather than pattern-matching
--   'Binding' by hand at each site.
runContextBinding0 :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, Fail] r => Binding -> Sem r Value
runContextBinding0 (Binding 0 fn) = runContextValue @branch (fn [] emptyValue)
runContextBinding0 (Binding n _)  = fail ("expected a 0-arity context definition, got arity " <> show n)

-- | 'runContextBinding0' for a 1-arity 'Binding' -- what a resolved
--   @context.character@-shaped definition (@charname: ...@) needs, since
--   unlike @context.main@ it always takes one real argument, never just
--   an ignored scope. @arg@ is always plain text at every real call site
--   (a bare character identifier), so this wraps it as a leaf 'Value'
--   itself rather than making every caller do that by hand.
runContextBinding1 :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, Fail] r => Binding -> Text -> Sem r Value
runContextBinding1 (Binding 1 fn) arg = runContextValue @branch (fn [pure (leafValue [User arg])] emptyValue)
runContextBinding1 (Binding n _)  _   = fail ("expected a 1-arity context definition, got arity " <> show n)
