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

import qualified Storage.FS as FS
import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.ContentEffects
  ( TreeAccess, Presence, JournalAccess, ConversationAccess, BranchResolve
  , runTreeAccess, runPresence, runJournalAccess, runConversationAccess
  )
import Storyteller.Core.Git (runBranchOpGit)
import Storyteller.Core.Runtime (Contexts)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))

import Storyteller.Context.DSL.AST (Definition(..), Name)
import Storyteller.Context.DSL.Compile (Binding(..), bval, runDefinition)
import qualified Storyteller.Context.DSL.Compile as Compile
import Storyteller.Context.DSL.Library (defaultLibrarySource, hostLibrary)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.Value (Action, ContextLibrary(..), Value, Message(User), leafValue, runAction)

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
  TreeAccess branch ': Presence branch ': JournalAccess branch ': ConversationAccess branch ': r

-- | The one well-known branch name this module owns -- exported the same
--   way 'Storyteller.Core.Prompt.promptsBranchName' is, so
--   'Storyteller.Writer.Branches.classifyBranch' can recognize it without
--   duplicating the literal.
contextsBranchName :: BranchName
contextsBranchName = BranchName "contexts"

-- | Parses @src@ and checks its arity against @expectedArity@ before
--   accepting it -- pure, shared by every call site that needs "is there a
--   valid override for this name" (each knows its own expected arity, so
--   the check lives here once rather than duplicated per caller). 'Nothing'
--   input (no override committed) and a parse\/arity failure are both
--   "use the default," kept as one 'Maybe' rather than distinguishing them
--   -- the caller already has its own default in hand either way.
resolveOverrideDefinition :: Int -> Maybe Text -> Maybe Definition
resolveOverrideDefinition _ Nothing = Nothing
resolveOverrideDefinition expectedArity (Just src) =
  case parseDefinition "<context override>" src of
    Left _ -> Nothing
    Right parsedDef
      | length (defParams parsedDef) /= expectedArity -> Nothing
      | otherwise -> Just parsedDef

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

-- | 'Storyteller.Context.DSL.Library.defaultLibrarySource', with
--   @overrides@ folded on top by name -- same "override, don't guess"
--   precedence 'resolveOverrideDefinition' already gives a single named
--   query, just applied once, up front, to the whole table (see
--   'ContextLibrary''s own Haddock on why this has to be one fixed table
--   rather than a per-name decision) -- plus
--   'Storyteller.Context.DSL.Library.hostLibrary', not override-
--   addressable at all (real Haskell closures, nothing to replace them
--   with). Three cases for a pure-DSL name, matching
--   'resolveOverrideDefinition''s own: a name already in the default
--   library only accepts an override whose arity matches the default it
--   would replace (a parse failure or an arity mismatch both just keep
--   the default -- "missing, not broken"); a name with *no* compiled-in
--   default (and not a 'hostLibrary' one) is a project genuinely adding a
--   new one, accepted at whatever arity it parses to; a name that
--   collides with a 'hostLibrary' entry is simply ignored -- the host
--   entry always wins. Called locally by 'runContextValue', at whatever
--   @branch@\/@r@ its own local content-effects interpretation already
--   provides -- never wired as its own 'ContextStorage' operation (see
--   that effect's own Haddock for why).
buildContextLibrary
  :: forall branch r. Members '[BranchResolve, TreeAccess branch, Presence branch, JournalAccess branch, ConversationAccess branch, Fail] r
  => Map Name Text -> ContextLibrary r
buildContextLibrary overrides = ContextLibrary (Map.unions [compiledKnown, compiledNew, hostLibrary @branch @r])
  where
    compiledKnown = Compile.definitionBinding <$> Map.mapWithKey applyOverride defaultLibrarySource
    compiledNew   = Compile.definitionBinding <$> Map.fromList (mapMaybe parseNamed (Map.toList newSource))
    newSource     = overrides `Map.difference` defaultLibrarySource `Map.difference` hostLibrary @branch @r
    applyOverride name defaultDef = fromMaybe defaultDef $
      resolveOverrideDefinition (length (defParams defaultDef)) (Map.lookup name overrides)
    parseNamed (name, src) = (,) name <$> resolveOverrideDefinition (arityOf src) (Just src)
    -- | 'resolveOverrideDefinition' needs an expected arity to check
    --   *against* -- for a genuinely new name (no compiled-in default to
    --   match), whatever the source itself parses to is the expected
    --   arity, so this always accepts (barring a parse failure, which
    --   'resolveOverrideDefinition' still catches via its own 'Left' case).
    arityOf src = either (const (-1)) (length . defParams) (parseDefinition "<library>" src)

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
--   see 'ContextRow's own Haddock), builds the library at that same
--   local scope (so 'buildContextLibrary''s own @hostLibrary@\/override
--   compilation can actually call the effects it needs), then runs
--   @act@ against it. This is what makes calling @\@Main@ here and
--   @\@LoreSource@ there, from the *same* 'ContextStorage' interpretation,
--   safe: nothing about this function commits 'ContextStorage' itself to
--   one branch.
runContextValue
  :: forall branch r a
  .  Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Action (ContextRow branch r) a -> Sem r a
runContextValue act =
    runConversationAccess @branch
  . runJournalAccess @branch
  . runPresence @branch
  . runTreeAccess @branch
  $ do
      overrides <- getContextOverrides
      let lib = buildContextLibrary @branch overrides
      runAction act lib

-- | 'getContextOverrides' immediately followed by "compile the override
--   if there is one, else run the default" -- mirrors
--   'Storyteller.Core.Prompt.getPrompt' exactly: @name@ and its own
--   readable, compiled-in @def@ travel together, at the call site, the same
--   way @getPrompt "agent.writer" defaultWriterSystemPrompt@ does. No
--   central "every context and its default" registry -- there's no more a
--   'Storyteller.Context.DSL.Library.defaultLibrarySource'-shaped list for
--   0-\/1-arity externally-resolved definitions than 'Storyteller.Core.Prompt'
--   has one for prompts. Whether @name@'s override came from the 'Contexts'
--   branch or a same-request 'SetContextOverride' is invisible here -- both
--   already landed in the same store by the time this looks.
resolveContext0
  :: forall branch r. Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Name -> Action (ContextRow branch r) (Value (ContextRow branch r)) -> Sem r (Value (ContextRow branch r))
resolveContext0 name def = do
  overrides <- getContextOverrides
  runContextValue @branch $ case resolveOverrideDefinition 0 (Map.lookup name overrides) of
    Just overrideDef -> runDefinition @branch overrideDef []
    Nothing          -> def

-- | 'resolveContext0''s 1-arity counterpart -- what every real
--   @context.character@\/@context.writer@ call site wants. @def@ is the
--   plain compiled-in definition itself (@Text -> Action Value@), the
--   ordinary Haskell function, no 'Binding' wrapping needed on this side
--   any more.
resolveContext1
  :: forall branch r. Members '[BranchOp branch, BranchResolve, ContextStorage, Fail] r
  => Name -> (Text -> Action (ContextRow branch r) (Value (ContextRow branch r))) -> Text -> Sem r (Value (ContextRow branch r))
resolveContext1 name def arg = do
  overrides <- getContextOverrides
  runContextValue @branch $ case resolveOverrideDefinition 1 (Map.lookup name overrides) of
    Just overrideDef -> runDefinition @branch overrideDef [bval (pure (leafValue [User arg]))]
    Nothing          -> def arg
