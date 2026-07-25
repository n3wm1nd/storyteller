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
  , getContextOverrides
  , setContextOverride
  , interpretContextStorageFS
  , interpretContextStorageMap
  , contextsBranchName
  , resolveOverrideDefinition
  , ContextRow
  , runContextValue
  , resolveContext0
  , resolveContext1
  , resolveAdhoc0
  , buildContextLibrary
  ) where

import Control.Monad (void)
import Data.Kind (Type)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.State (State, evalState, get, modify)
import Runix.Git (Git)

import qualified Storage.FS as FS
import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.ContentEffects
  ( TreeAccess, Presence, JournalAccess, ConversationAccess, Summarized, BranchResolve
  , runTreeAccess, runPresence, runJournalAccess, runConversationAccess, runSummarized
  )
import Storyteller.Core.Git (runBranchOpGit)
import Storyteller.Core.Runtime (Contexts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))

import Storyteller.Context.DSL.AST (Definition(..), Name)
import Storyteller.Context.DSL.Compile (Library, bval, runDefinition, runNamed)
import qualified Storyteller.Context.DSL.Compile as Compile
import Storyteller.Context.DSL.Library (defaultLibraryOrder, defaultLibrarySource, hostLibrary)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.Value (Action, Value, Message(User), leafValue, runAction)

-- | Deliberately just data -- a name/text map plus staging, nothing
--   'Binding'\/'Action'-shaped. One 'ContextStorage' interpretation is
--   wired once per whole request (see "Server.Writer.Run"'s
--   @actionStack@), but a single request legitimately resolves context
--   against *several different branches* over its lifetime (confirmed:
--   @\@Main@, @\@LoreSource@, and a caller-generic @\@branch@ are all real
--   call sites) -- so this effect can never commit to compiling an
--   override against one branch's content-effects itself. Compiling an
--   override (parse, arity-check, 'Storyteller.Context.DSL.Compile.runDefinition')
--   is entirely 'resolveContext0'\/'resolveContext1'\/'buildContextLibrary''s
--   job now, each of which already knows its own @\@branch@ per call.
data ContextStorage (m :: Type -> Type) a where
  -- | The whole override map, resolved once per interpretation --
  --   whatever the branch had at interpretation start, plus anything a
  --   prior 'SetContextOverride' this same request already staged. Raw
  --   text, not yet parsed -- parsing (and the arity check against
  --   whatever default it might replace) happens at each call site, which
  --   is the only place that knows what arity to check against.
  GetContextOverrides :: ContextStorage m (Map Name Text)
  -- | Stages @name@'s override for the rest of *this* interpretation only
  --   -- never written to the 'Contexts' branch, never visible to any
  --   other request. What a WS handler calls once, before running the
  --   command proper, when a request carries its own context program: "the
  --   client sent this for @context.writer@" becomes exactly "treat this
  --   request as if @context.writer@ had this override," through the
  --   *same* map 'GetContextOverrides' already reads -- no separate
  --   wire-override code path anywhere else.
  SetContextOverride :: Name -> Text -> ContextStorage m ()

makeSem ''ContextStorage

-- | The four DSL-facing content effects, scoped to one @branch@, layered
--   onto whatever row @r@ a caller is already working in -- what
--   'runContextValue' interprets locally, fresh, per call (never wired
--   globally to one fixed branch -- see "Storyteller.Core.ContentEffects"'s
--   own module Haddock and @project_mcp_export_effect_boundary@ for why:
--   'runContextValue' is genuinely called at different @branch@es --
--   @\@Main@, @\@LoreSource@, a caller-generic @\@branch@ -- not one fixed
--   choice).
type ContextRow branch r =
  TreeAccess branch ': Presence branch ': JournalAccess branch ': ConversationAccess branch ': Summarized branch ': r

-- | The one well-known branch name this module owns -- exported the same
--   way 'Storyteller.Core.Prompt.promptsBranchName' is, so
--   'Storyteller.Writer.Branches.classifyBranch' can recognize it without
--   duplicating the literal.
contextsBranchName :: BranchName
contextsBranchName = BranchName "contexts"

-- | Parses @src@ -- pure, shared by every call site that needs "is this
--   valid DSL text at all." Unlike its own previous shape, this no longer
--   also checks arity against some expected value: arity mismatches (and
--   every other way an override can break something that references it)
--   are caught by 'buildContextLibrary' actually recompiling the whole
--   sequence with the override in place, not by a narrower pre-check here
--   that could only ever see the override in isolation. A parse failure
--   is the one thing genuinely decidable from @src@ alone, with nothing
--   else in scope.
resolveOverrideDefinition :: Maybe Text -> Maybe Definition
resolveOverrideDefinition Nothing = Nothing
resolveOverrideDefinition (Just src) =
  either (const Nothing) Just (parseDefinition "<context override>" src)

-- | Real interpreter: reads every override on the dedicated 'Contexts'
--   branch *once* up front (creating the branch on first use), not one
--   storage round trip per 'GetContextOverrides' call -- a single
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

-- | Splices @overrides@ into 'Storyteller.Context.DSL.Library.defaultLibraryOrder'
--   by name -- a name matching an existing slot gets *both* entries at
--   that position, default immediately followed by override, sharing one
--   'Name' (see 'Compile.Library''s own Haddock: this is exactly what two
--   entries sharing a key already means, not a special case). This is
--   what lets an override's own reference to its own name resolve to the
--   compiled default rather than failing outright -- 'Compile.buildLibrary'
--   folds left to right, so the override entry closes over a 'Library'
--   that already has the default sitting at this same name, one slot
--   behind it, the identical "compile default, then override, same key"
--   order the pre-sequence design used, just expressed as what the
--   sequence itself already contains rather than as two separate
--   'Map.insert' calls at build time.
--
--   A name matching nothing already in the default sequence, and not a
--   'Storyteller.Context.DSL.Library.hostLibrary' name either (a host
--   binding is a real Haskell closure, never override-addressable), is a
--   project genuinely adding a new one, appended after every default so
--   it can reference the whole default graph, plus any other new name
--   earlier in this same @overrides@ map. Parse failures are silently
--   skipped here, not spliced in at all -- 'buildContextLibrary' is what
--   actually decides pass/fail per name; a name whose own text doesn't
--   even parse can't be a candidate for that decision at all.
spliceOverrides
  :: forall branch r. Members '[BranchResolve, TreeAccess branch, Presence branch, JournalAccess branch, ConversationAccess branch, Summarized branch, Fail] r
  => Map Name Text -> [(Name, Definition)]
spliceOverrides overrides = concatMap applyOverride defaultLibraryOrder ++ newEntries
  where
    applyOverride (name, defaultDef) = case resolveOverrideDefinition (Map.lookup name overrides) of
      Nothing         -> [(name, defaultDef)]
      Just overrideDef -> [(name, defaultDef), (name, overrideDef)]
    hostNames  = Set.fromList (map fst (Compile.libraryEntries (hostLibrary @branch @r)))
    newSource  = overrides `Map.difference` defaultLibrarySource `Map.withoutKeys` hostNames
    newEntries = mapMaybe (\(name, src) -> (,) name <$> resolveOverrideDefinition (Just src)) (Map.toList newSource)

-- | Compiles @overrides@ against 'Storyteller.Context.DSL.Library.defaultLibraryOrder'
--   via 'spliceOverrides' \/ 'Compile.buildLibrary', and __rejects__ (not
--   silently discards, see below) any override that breaks compilation --
--   whether the override's own body doesn't resolve, or it changes some
--   name's arity out from under a *later* default definition that still
--   calls it at the old one. There is no separate arity pre-check
--   anywhere in this module any more: recompiling the whole spliced
--   sequence and seeing whether it succeeds __is__ the check, complete,
--   for the same reason 'Compile.definitionBinding' itself no longer
--   defers reference-checking into the closure it builds -- the language
--   has no @if@\/recursion, so a body's set of references is exactly its
--   text, and the full, already-parsed sequence is sitting right here to
--   walk, with nothing left to discover later that isn't already knowable
--   now.
--
--   On a rejection, the offending @name@ is dropped from @overrides@ and
--   the whole sequence is retried -- from scratch, with the remaining
--   overrides, __never__ a partially-applied library with only the
--   surviving overrides patched in ad hoc: recompiling from
--   'defaultLibraryOrder' again is what guarantees every accepted
--   override is checked against the *final* accepted set, not against
--   whatever happened to be in the table when it was first tried. This
--   always terminates, because @overrides@ strictly shrinks by one name
--   each retry and the base case -- no overrides, or none left -- is
--   'defaultLibraryOrder' by itself, which always compiles (a closed,
--   already-verified graph over 'hostLibrary', unaffected by anything any
--   project could ever commit). Returns the accepted library alongside
--   every rejected name, so a caller can surface *which* commits didn't
--   take instead of the previous silent fallback.
buildContextLibrary
  :: forall branch r. Members '[BranchResolve, TreeAccess branch, Presence branch, JournalAccess branch, ConversationAccess branch, Summarized branch, Fail] r
  => Map Name Text -> (Library r, [Name])
buildContextLibrary overrides =
  case Compile.buildLibrary (hostLibrary @branch @r) (spliceOverrides @branch @r overrides) of
    Right lib             -> (lib, [])
    Left (badName, _err)
      | Map.member badName overrides ->
          let (lib, rejected) = buildContextLibrary @branch (Map.delete badName overrides)
          in (lib, badName : rejected)
      -- A default itself can't fail to compile (see this function's own
      -- Haddock) -- if 'badName' isn't one of @overrides@'s own keys,
      -- something is wrong with 'defaultLibraryOrder' itself, not with
      -- anything a project committed, and retrying by deleting an
      -- override that was never the cause would loop forever without
      -- ever converging. Fails loudly instead.
      | otherwise -> error ("buildContextLibrary: default library itself failed to compile at "
                              <> T.unpack badName <> ": " <> _err)

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
      GetContextOverrides -> get
      SetContextOverride name src -> modify (Map.insert name src)
    )
    action

-- | Runs a Context DSL 'Action' against @branch@ -- interprets
--   'Storyteller.Core.ContentEffects.TreeAccess'\/'Presence'\/
--   'JournalAccess'\/'ConversationAccess' locally and fresh, scoped to
--   this one call's @branch@ (never wired globally to one fixed branch --
--   see 'ContextRow's own Haddock), then runs @act@. @act@ is already
--   fully compiled by the time it's handed here -- every identifier
--   inside it was resolved at its own construction (see
--   'buildContextLibrary''s own Haddock), so unlike the previous design
--   there is no library table left to build or thread through 'runAction'
--   at this point. This is what makes calling @\@Main@ here and
--   @\@LoreSource@ there, from the *same* 'ContextStorage' interpretation,
--   safe: nothing about this function commits 'ContextStorage' itself to
--   one branch.
runContextValue
  :: forall branch r a
  .  Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Action (ContextRow branch r) a -> Sem r a
runContextValue act =
    runSummarized @branch
  . runConversationAccess @branch
  . runJournalAccess @branch
  . runPresence @branch
  . runTreeAccess @branch
  $ runAction act

-- | Runs the already-compiled library entry @name@ -- mirrors
--   'Storyteller.Core.Prompt.getPrompt''s "look this key up, once" shape,
--   but with no @def@ parameter to pass any more: 'buildContextLibrary'
--   always leaves @name@ bound to *something* (the default, or an accepted
--   override -- see 'spliceOverrides''s own Haddock), so the compiled-in
--   Haskell fallback 'resolveContext0' used to take as an explicit
--   argument was never anything other than what @table@'s own @name@ slot
--   already evaluates to. Looking that slot up directly (via 'runNamed')
--   and calling it __is__ running the default when there's no accepted
--   override, with no second, parallel "or call this Haskell function
--   instead" path needed.
resolveContext0
  :: forall branch r. Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Name -> Sem r (Value (ContextRow branch r))
resolveContext0 name = do
  overrides <- getContextOverrides
  let (table, _rejected) = buildContextLibrary @branch overrides
  runContextValue @branch (runNamed @branch table name [])

-- | 'resolveContext0''s 1-arity counterpart -- what every real
--   @context.character@\/@context.writer@ call site wants.
resolveContext1
  :: forall branch r. Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Name -> Text -> Sem r (Value (ContextRow branch r))
resolveContext1 name arg = do
  overrides <- getContextOverrides
  let (table, _rejected) = buildContextLibrary @branch overrides
  runContextValue @branch (runNamed @branch table name [pure (leafValue [User arg])])

-- | Runs a raw, caller-supplied 0-arity Context DSL program against the
--   compiled library, with no name of its own and no default to fall back
--   to -- unlike 'resolveContext0'\/'resolveContext1', this __is__ the
--   content, not an override of some slot that already has a compiled-in
--   answer. What a per-call @pinnedPrograms@ entry
--   ('Server.Writer.File.chatWriter''s own wire field) resolves through: a
--   client picking a named function like @rules.magic@ to fold into this
--   turn's pinned\/authors-notes content is exactly a bare call to an
--   existing library name, the same as any @context.*@ cross-reference --
--   no different mechanism needed, just no slot identity to check an
--   override's arity against. A parse failure or non-zero declared arity
--   is a real 'Fail' here, not a silent "use the default" -- there is no
--   default for a program that was never a named slot to begin with, so
--   swallowing the error would just mean silently contributing nothing,
--   worse than telling the caller their program didn't run.
resolveAdhoc0
  :: forall branch r. Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Text -> Sem r (Value (ContextRow branch r))
resolveAdhoc0 src = do
  overrides <- getContextOverrides
  let (table, _rejected) = buildContextLibrary @branch overrides
  case resolveOverrideDefinition (Just src) of
    Nothing  -> fail ("resolveAdhoc0: not a valid 0-arity program: " <> T.unpack src)
    Just def
      | not (null (defParams def)) ->
          fail ("resolveAdhoc0: expected a 0-arity program, got " <> show (length (defParams def)) <> " parameter(s)")
      | otherwise -> runContextValue @branch (runDefinition @branch table def [])
