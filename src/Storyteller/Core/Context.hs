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
{-# LANGUAGE UndecidableInstances #-}

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
  , setContextOverride
  , interpretContextStorageFS
  , interpretContextStorageMap
  , contextsBranchName
  , resolveContextOverride
  , runContextValue
  , runContextBinding0
  , runContextBinding1
  , resolveContext0
  , resolveContext1
  , ContextLibrary(..)
  , buildContextLibrary
  ) where

import Control.Monad (void)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.State (State, evalState, get, modify)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.FS as FS
import Storyteller.Core.Git (BranchOp, runBranchOpGit, runStorage)
import Storyteller.Core.Runtime (Contexts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))

import Storyteller.Context.DSL.AST (Definition(..), Name)
import Storyteller.Context.DSL.Compile (Binding(..), bval, runDefinition)
import qualified Storyteller.Context.DSL.Compile as Compile
import Storyteller.Context.DSL.Context (toBindingFn1)
import Storyteller.Context.DSL.Library (defaultLibrarySource, hostLibrary)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.Value (Action, ContextLibrary(..), Value, Message(User), emptyValue, leafValue, runAction)

data ContextStorage (m :: Type -> Type) a where
  -- | Look up @name@'s own override, falling back to the caller-supplied
  --   default 'Binding' when none exists (or one exists but doesn't parse,
  --   or parses with the wrong arity). The override map this checks is the
  --   *current* one -- whatever the branch had at interpretation start,
  --   plus anything a prior 'SetContextOverride' this same request already
  --   staged (see its own Haddock) -- so a client-submitted program and a
  --   project's committed one are genuinely indistinguishable from here.
  GetContextDefinition :: Name -> Binding -> ContextStorage m Binding
  -- | Stages @name@'s override for the rest of *this* interpretation only
  --   -- never written to the 'Contexts' branch, never visible to any
  --   other request. What a WS handler calls once, before running the
  --   command proper, when a request carries its own context program: "the
  --   client sent this for @context.writer@" becomes exactly "treat this
  --   request as if @context.writer@ had this override," through the
  --   *same* lookup 'GetContextDefinition' already does for a committed
  --   one -- no separate wire-override code path anywhere else. A parse\/
  --   arity failure is still only discovered (and still only silently
  --   falls back) the next time something actually looks @name@ up, same
  --   as a bad branch commit.
  SetContextOverride :: Name -> Text -> ContextStorage m ()
  -- | The whole library, resolved once per interpretation:
  --   'Storyteller.Context.DSL.Library.defaultLibrarySource', overridden\/
  --   extended by whatever's currently staged (branch, then any
  --   'SetContextOverride') -- fixed for the rest of the request only in
  --   the sense that nothing re-reads the branch, but reflects a
  --   'SetContextOverride' the moment it's called, same as
  --   'GetContextDefinition' does. This is what
  --   'Storyteller.Context.DSL.Value.currentLibrary' reads at
  --   'runContextValue''s one call to 'runAction', which is what lets one
  --   @[dsl| ... |]@ definition reference another by plain name
  --   (@contextWriter@'s own body calling @lore@) -- see
  --   'Storyteller.Context.DSL.Value.ContextLibrary''s own Haddock for why
  --   this rides on 'ContextStorage' (the same store 'GetContextDefinition'
  --   already reads) rather than a second, parallel channel for what's
  --   really the same resolved state.
  GetContextLibrary :: ContextStorage m ContextLibrary

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

-- | Real interpreter: reads every override on the dedicated 'Contexts'
--   branch *once* up front (creating the branch on first use), not one
--   storage round trip per 'GetContextDefinition' call -- a single
--   'ContextStorage' interpreter typically backs a whole WS command\/CLI
--   action, which may resolve several dotted names in that one action
--   (@context.main@, then @context.character@ once per active character,
--   say), and none of them needs its own separate branch read: the whole
--   branch is small, project-authored text, cheap to read in full, and
--   never changes mid-action. A key resolves to @\<dots-as-slashes\>.dsl@;
--   a missing file just never contributes an entry to the loaded map, and
--   'interpretContextStorageMap' (the same interpreter
--   'Storyteller.Core.ContextSpec' tests directly) is what actually
--   answers each lookup from there -- this function's only own job is
--   building that map.
interpretContextStorageFS
  :: Members '[Git, StoryStorage, Fail] r
  => Sem (ContextStorage ': r) a
  -> Sem r a
interpretContextStorageFS action = do
  getBranch contextsBranchName >>= \case
    Just _  -> return ()
    Nothing -> void (createBranch contextsBranchName)
  overrides <- runBranchOpGit @Contexts contextsBranchName $ runStorage @Contexts $ do
    paths <- filter (".dsl" `T.isSuffixOf`) . map T.pack <$> FS.list
    Map.fromList <$> mapM (\p -> (,) (pathToName p) . TE.decodeUtf8 <$> FS.readFile (T.unpack p)) paths
  interpretContextStorageMap overrides action
  where
    pathToName = T.replace "/" "." . fromMaybe "" . T.stripSuffix ".dsl"

-- | 'Storyteller.Context.DSL.Library.defaultLibrarySource', with
--   @overrides@ folded on top by name -- same "override, don't guess"
--   precedence 'resolveContextOverride' already gives a single named
--   query, just applied once, up front, to the whole table (see
--   'ContextLibrary''s own Haddock on why this has to be one fixed table
--   rather than a per-name decision) -- plus
--   'Storyteller.Context.DSL.Library.hostLibrary', not override-
--   addressable at all (real Haskell closures, nothing to replace them
--   with). Three cases for a pure-DSL name, matching
--   'resolveContextOverride''s own: a name already in the default library
--   only accepts an override whose arity matches the default it would
--   replace (a parse failure or an arity mismatch both just keep the
--   default -- "missing, not broken"); a name with *no* compiled-in
--   default (and not a 'hostLibrary' one) is a project genuinely adding a
--   new one, accepted at whatever arity it parses to; a name that
--   collides with a 'hostLibrary' entry is simply ignored -- the host
--   entry always wins.
buildContextLibrary :: Map Name Text -> ContextLibrary
buildContextLibrary overrides = ContextLibrary (Map.unions [compiledKnown, compiledNew, hostLibrary])
  where
    compiledKnown = Compile.definitionBinding <$> Map.mapWithKey applyOverride defaultLibrarySource
    compiledNew   = Compile.definitionBinding <$> Map.fromList (mapMaybe parseNamed (Map.toList newSource))
    newSource     = overrides `Map.difference` defaultLibrarySource `Map.difference` hostLibrary
    applyOverride name defaultDef = case Map.lookup name overrides of
      Nothing  -> defaultDef
      Just src -> case parseDefinition ("<library:" <> T.unpack name <> ">") src of
        Left _ -> defaultDef
        Right overrideDef
          | length (defParams overrideDef) /= length (defParams defaultDef) -> defaultDef
          | otherwise -> overrideDef
    parseNamed (name, src) = case parseDefinition ("<library:" <> T.unpack name <> ">") src of
      Left _    -> Nothing
      Right def -> Just (name, def)

-- | Test/pure interpreter: resolves from a fixed map of override source
--   text as the starting point, falling back to the caller's default on
--   miss -- no filesystem or branch involved, mirroring
--   'Storyteller.Core.Prompt.interpretPromptStorageMap'. Threads a
--   'Polysemy.State.State' seeded from @overrides@ underneath so
--   'SetContextOverride' has somewhere to stage into, same as
--   'interpretContextStorageFS'.
interpretContextStorageMap
  :: Map Name Text
  -> Sem (ContextStorage ': r) a
  -> Sem r a
interpretContextStorageMap overrides action =
  evalState overrides $ reinterpret
    (\case
      GetContextDefinition name def -> do
        current <- get
        return (resolveContextOverride def (Map.lookup name current))
      SetContextOverride name src -> modify (Map.insert name src)
      GetContextLibrary -> buildContextLibrary <$> get
    )
    action

-- | Runs a Context DSL 'Action' positioned at whatever commit @branch@'s
--   own 'Storyteller.Core.Git.BranchOp' scope is currently ambient at
--   (accounting for an in-progress rebase\/transaction the same way any
--   other read in that scope would) -- via 'Storage.Core.runStoreT'
--   directly against 'Sem r' itself, not 'runStorage'\/'BranchOp'
--   dispatch (see 'Storyteller.Core.Git''s own @MonadBranch (Sem r)@
--   instance haddock for why 'runStorage''s type can't accept an
--   'Action' at all: the DSL is read-only, so it never needs 'BranchOp'
--   write-buffering).
runContextValue :: forall branch r a. Members '[BranchOp branch, Git, StoryStorage, ContextStorage, Fail] r => Action a -> Sem r a
runContextValue act = do
  h   <- runStorage @branch Core.headHash
  lib <- getContextLibrary
  fst <$> Core.runStoreT h (runAction act lib)

-- | 'runContextValue' for a 0-arity 'Binding' -- what a resolved
--   @context.lore@\/@context.chapters@\/@context.style@-shaped definition
--   needs, since none of them take a real argument.
runContextBinding0 :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, ContextStorage, Fail] r => Binding -> Sem r Value
runContextBinding0 (Binding 0 fn) = runContextValue @branch (fn [] emptyValue)
runContextBinding0 (Binding n _)  = fail ("expected a 0-arity context definition, got arity " <> show n)

-- | 'runContextValue' for a 1-arity 'Binding' -- what a resolved
--   @context.character@\/@context.other@\/@context.writer@-shaped
--   definition needs, since each always takes one real argument, never
--   just an ignored scope. @arg@ is always plain text at every real call
--   site (a bare character identifier, or a target file path), so this
--   wraps it as a leaf 'Value' itself rather than making every caller do
--   that by hand.
runContextBinding1 :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, ContextStorage, Fail] r => Binding -> Text -> Sem r Value
runContextBinding1 (Binding 1 fn) arg = runContextValue @branch (fn [pure (leafValue [User arg])] emptyValue)
runContextBinding1 (Binding n _)  _   = fail ("expected a 1-arity context definition, got arity " <> show n)

-- | 'getContextDefinition' immediately followed by 'runContextBinding0' --
--   mirrors 'Storyteller.Core.Prompt.getPrompt' exactly: @name@ and its own
--   readable, compiled-in @def@ travel together, at the call site, the same
--   way @getPrompt "agent.writer" defaultWriterSystemPrompt@ does. No
--   central "every context and its default" registry -- there's no more a
--   'Storyteller.Context.DSL.Library.defaultLibrarySource'-shaped list for
--   0-\/1-arity externally-resolved definitions than 'Storyteller.Core.Prompt'
--   has one for prompts. Whether @name@'s override came from the 'Contexts'
--   branch or a same-request 'SetContextOverride' is invisible here -- both
--   already landed in the same store by the time this looks.
resolveContext0
  :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, ContextStorage, Fail] r
  => Name -> Action Value -> Sem r Value
resolveContext0 name def =
  getContextDefinition name (bval def) >>= runContextBinding0 @branch

-- | 'resolveContext0''s 1-arity counterpart -- what every real
--   @context.character@\/@context.writer@ call site wants. @def@ is the
--   plain compiled-in definition itself (@Text -> Action Value@), not a
--   'Binding' -- 'Storyteller.Context.DSL.Context.toBindingFn1' is what
--   builds the arity-tagged 'Binding' 'getContextDefinition' needs, so a
--   call site never has to.
resolveContext1
  :: forall branch r. Members '[BranchOp branch, Git, StoryStorage, ContextStorage, Fail] r
  => Name -> (Text -> Action Value) -> Text -> Sem r Value
resolveContext1 name def arg =
  getContextDefinition name (toBindingFn1 def) >>= \b -> runContextBinding1 @branch b arg
