{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Interpreter for the Context DSL's AST (see
--   "Storyteller.Context.DSL.AST") into 'Value' actions running directly
--   against the storage engine, per @CONTEXT-DSL.md@'s "Implementation
--   strategy" -- deliberately separate from the parser
--   ("Storyteller.Context.DSL.Parser"), and from what any of this gets
--   consumed into afterwards (a flattened string, a browsable tree, a
--   tool-mounted surface -- see "Interpretation is not part of this
--   spec").
--
--   == Fully generic -- no Polysemy anywhere in this module
--
--   This module is written exactly the way "Storage.Core"\/
--   "Storage.Ops"\/"Storage.FS" themselves are: @'Core.StoreM' m => ...
--   -> 'StoreT' m a@, with no mention of 'Polysemy.Sem',
--   'Storyteller.Core.Branch.BranchOp', or any other Polysemy effect at
--   all -- there is no import of @polysemy@ or any @Storyteller.Core.*@
--   Polysemy-effect module here. 'Value'\'s own monad is genuinely
--   'StoreT' m, not merely something that happens to satisfy a
--   'Core.MonadStore' constraint -- @read@\/@in@\/@for@\/glob matching
--   are built from "Storage.Core"\/"Storage.FS"\'s own combinators
--   ('Core.readAt'\/'Core.inWorktree'\/'FS.list'\/'Core.readFile'), the
--   same ones every hand-written agent already composes with, not a
--   parallel implementation of tree navigation.
--
--   'Core.readAt' is what makes cross-branch @in (charname | branch):@
--   possible *without* ever leaving 'StoreT': it jumps to "any commit
--   this store can read, not just an ancestor of the current head" (see
--   its own haddock in "Storage.Core"), runs an action there, and
--   restores exactly where it started -- no open per-branch scope, no
--   type-level tag, no nested dispatch, just ordinary 'StoreT'
--   composition. Paired with 'Core.inWorktree' (same peek-and-restore
--   shape, for the ambient tree instead of the chain head),
--   'treeValueOfCommit' below is the *only* place a commit ever gets
--   turned into a 'Value', used identically for the initial scope and
--   for every cross-branch @in@.
--
--   The one operation genuinely impossible to express as
--   @'Core.StoreM' m => ...@: resolving a
--   'Storyteller.Core.Types.BranchName' to a commit.
--   'Storyteller.Core.Storage.StoryStorage' (named-ref bookkeeping) is
--   deliberately a *separate* effect from 'Core.MonadStore'
--   (content-addressed objects) throughout this codebase -- not a gap
--   this module invented, a boundary it already has to respect. Rather
--   than hardcode a Polysemy-shaped answer to that one operation here
--   (which would drag Polysemy back into every signature transitively),
--   it's a plain injected function, 'BranchResolver' -- see @branch@\'s
--   implementation ('fBranch') and 'runDefinitionOnBranch'. Whoever
--   actually runs this against real git supplies a concrete resolver
--   (closing over @getBranch@ and whatever effect system they use);
--   this module never needs to know what that is. Exactly the "proxy
--   model" shape 'Storyteller.Core.Branch.BranchOp' itself uses
--   (@RunStorage :: (forall n. Core.StoreM n => Core.StoreT n a) ->
--   BranchOp branch m a@) -- Polysemy only ever touches the *dispatch*,
--   never the storage logic being dispatched.
module Storyteller.Context.DSL.Compile
  ( -- * The interpreter (Core.StoreM only)
    Binding(..)
  , Env
  , DSLFilter
  , FilterRegistry
  , coreFilters
  , errorValue
  , compileDefinition
  , runStmts
  , evalExpr
  , treeValueOfCommit

    -- * Branch resolution -- injected, not hardcoded
  , BranchResolver
  , fBranch
  , treeValueOfBranch
  , runDefinitionOnBranch
  ) where

import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob

import qualified Storage.Core as Core
import Storage.Core (StoreT)
import qualified Storage.FS as FS

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Value

import Storyteller.Core.Types (BranchName(..))

-- ---------------------------------------------------------------------------
-- Bindings and environment
-- ---------------------------------------------------------------------------

-- | What a local name (a @let@\/parameter\/loop variable) resolves to.
--   'BFun' takes the *caller's* current ambient scope as an explicit
--   argument rather than closing over whatever scope was active at
--   definition time -- this is the whole of "Reader scope is resolved
--   entirely dynamically, at the point code actually runs": a bound
--   function is a Haskell closure over 'Env' (its lexical bindings,
--   fixed at definition time, exactly as @let@ should behave) but *not*
--   over 'Value' (its dynamic scope, supplied fresh on every call).
data Binding m
  = BVal (StoreT m (Value (StoreT m)))
  | BFun Int ([StoreT m (Value (StoreT m))] -> Value (StoreT m) -> StoreT m (Value (StoreT m)))

type Env m = Map Name (Binding m)

-- ---------------------------------------------------------------------------
-- Building a Reader scope from a commit
-- ---------------------------------------------------------------------------

-- | A commit's tree, as a 'Value' -- the Reader scope a top-level
--   definition's @read@\/glob resolve against before any @in@ narrows or
--   redirects it, and what 'fBranch' produces
--   produces for an @in@ that crosses into a different branch. Used
--   identically for both: nothing here knows or cares whether @commit@
--   is "the" branch the interpreter started on or one reached via
--   @charname | branch@ mid-evaluation.
--
--   'entries' is keyed by *full path*, one level, not a hand-rolled
--   nested trie -- 'FS.list' already gives exactly this shape (a flat
--   file listing), the same way a glob result's own 'entries' is
--   already flat and keyed by full matched path (see "Value model").
--   'read'\'s resolution (see 'evalExpr'\'s @ERead@ case) tries the
--   whole literal path as one flat key first for exactly this reason,
--   before ever falling back to 'lookupPath'\'s segment-by-segment walk.
--
--   Every listing and read goes through 'Core.readAt' @commit@
--   ('Core.inWorktree' ...): jump to @commit@, sync the ambient tree
--   there, run the read, restore both -- entirely ordinary 'StoreT'
--   composition, independent of whatever the ambient scope was doing
--   before or after. Content stays deferred: each leaf's own
--   'valueDefault' is its own 'Core.readAt' call, forced only when a
--   definition actually reads it.
treeValueOfCommit :: Core.StoreM m => Core.ObjectHash -> StoreT m (Value (StoreT m))
treeValueOfCommit commit = do
  paths <- Core.readAt commit (Core.inWorktree FS.list)
  pure Value
    { valueDefault = pure []
    , valueEntries = Map.fromList
        [ ( T.pack path
          , leafValue . (: []) . FileRead path . TE.decodeUtf8
              <$> Core.readAt commit (Core.inWorktree (Core.readFile path))
          )
        | path <- paths
        ]
    }

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Compiles and immediately runs a whole 'Definition' against a
--   supplied initial scope, argument list, filter registry, and branch
--   resolver -- the entry point a host embeds (matching the spec's own
--   framing: "a compiled context ends up with exactly the type shape a
--   hand-written agent already has"). @resolveBranch@ is a plain
--   parameter, not part of @filters@ -- see the "Filters" section
--   haddock for why @branch@ isn't a filter at all.
compileDefinition
  :: Core.StoreM m
  => FilterRegistry (StoreT m)
  -> BranchResolver m
  -> Definition
  -> Value (StoreT m)               -- ^ initial ambient Reader scope
  -> [StoreT m (Value (StoreT m))]  -- ^ arguments, matched against 'defParams'
  -> StoreT m (Value (StoreT m))
compileDefinition filters resolveBranch def scope args
  | length args /= length (defParams def) = fail $
      "arity mismatch: " <> show (length (defParams def)) <> " parameter(s), "
        <> show (length args) <> " argument(s) given"
  | otherwise = do
      let env = Map.fromList (zip (defParams def) (map BVal args))
      mkValue <$> runStmts filters resolveBranch env scope (defBody def)

mkValue :: Applicative m => ([Message], Map Name (m (Value m))) -> Value m
mkValue (msgs, entries) = Value (pure msgs) entries

-- | Runs a whole 'Block', folding every statement's contribution into
--   one @([Message], entries)@ pair -- this *is* "a fresh writer
--   target" (rules 4\/5): whoever calls this (a function body, an @as@
--   body, 'compileDefinition') wraps the result into a new 'Value'.
--   @in@\/@for@ do *not* call this to get an independent 'Value' of
--   their own -- per rule 6, "the writer target is untouched" -- they
--   fold their own nested statements into the *same* accumulator this
--   call already threads (see their cases below).
runStmts
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> Block
  -> StoreT m ([Message], Map Name (StoreT m (Value (StoreT m))))
runStmts filters resolveBranch env0 scope0 = go env0 scope0 [] Map.empty
  where
    go _ _ msgs entries [] = pure (concat (reverse msgs), entries)
    go env scope msgs entries (Located _ (SExpr e) : rest) = do
      v <- evalExpr filters resolveBranch env scope e
      m <- valueDefault v
      go env scope (m : msgs) entries rest
    go env scope msgs entries (Located pos (SAs nameE body) : rest) = do
      name <- nameOf filters resolveBranch env scope nameE
      when (Map.member name entries) $
        fail $ "duplicate 'as' name " <> show name <> " at line " <> show (posLine pos)
      let entryAction = mkValue <$> runStmts filters resolveBranch env scope body
      go env scope msgs (Map.insert name entryAction entries) rest
    go env scope msgs entries (Located _ (SLet name mParams body) : rest) =
      let binding = case mParams of
            Nothing -> BVal (mkValue <$> runStmts filters resolveBranch env scope body)
            Just ps -> BFun (length ps) $ \args callerScope ->
              mkValue <$> runStmts filters resolveBranch (bindParams ps args env) callerScope body
      in go (Map.insert name binding env) scope msgs entries rest
    go env scope msgs entries (Located _ (SIn e body) : rest) = do
      newScope <- evalExpr filters resolveBranch env scope e
      (m, es) <- runStmts filters resolveBranch env newScope body
      go env scope (m : msgs) (Map.union es entries) rest
    go env scope msgs entries (Located pos (SFor var pathLit body) : rest) = do
      matches <- globMatch filters resolveBranch env scope pathLit
      (m, es) <- foldM (runOneIteration pos var body) ([], entries) matches
      go env scope (m : msgs) es rest
      where
        runOneIteration p var' body' (msgsAcc, entriesAcc) matchedPath = do
          let env' = Map.insert var' (BVal (pure (leafValue [User matchedPath]))) env
          (m1, es1) <- runStmts filters resolveBranch env' scope body'
          case Map.keys (Map.intersection es1 entriesAcc) of
            (dup : _) -> fail $ "duplicate 'as' name " <> show dup
                           <> " across for-loop iterations, near line " <> show (posLine p)
            []        -> pure ()
          pure (msgsAcc ++ m1, Map.union es1 entriesAcc)

bindParams :: [Name] -> [StoreT m (Value (StoreT m))] -> Env m -> Env m
bindParams ps args env = List.foldl' (\e (p, a) -> Map.insert p (BVal a) e) env (zip ps args)

nameOf
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> Expr -> StoreT m Name
nameOf filters resolveBranch env scope e = do
  v <- evalExpr filters resolveBranch env scope e
  messagesText <$> valueDefault v

evalExpr
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> Expr -> StoreT m (Value (StoreT m))
evalExpr filters resolveBranch env scope e = case e of
  EString Quoted parts -> leafValue . (: []) . User <$> interpText filters resolveBranch env scope parts
  EString Bare   parts -> do
    pat     <- interpText filters resolveBranch env scope parts
    matches <- globMatchPat scope pat
    entries <- Map.fromList <$> mapM (\m -> (,) m . pure <$> forceAt scope m) matches
    pure (Value (pure []) entries)
  EAssistant parts -> leafValue . (: []) . Assistant <$> interpText filters resolveBranch env scope parts
  EUser inner -> do
    v    <- evalExpr filters resolveBranch env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (User . messageText) msgs) }
  EIdent name -> case Map.lookup name env of
    Just (BVal action)  -> action
    Just (BFun arity _) -> fail $
      T.unpack name <> " needs " <> show arity <> " argument(s), used with none"
    Nothing -> fail $ "unknown identifier: " <> T.unpack name
  ERead pathLit -> do
    path <- pathLitText filters resolveBranch env scope pathLit
    resolveRead scope path >>= \case
      Nothing -> pure emptyValue
      Just v  -> pure v
  EApp headE argEs -> do
    let args = map (evalExpr filters resolveBranch env scope) argEs
    case headE of
      EIdent name -> case Map.lookup name env of
        Just (BFun arity fn) -> do
          when (length args /= arity) $ fail $
            T.unpack name <> ": expected " <> show arity <> " argument(s), got " <> show (length args)
          fn args scope
        Just (BVal _) -> fail $ T.unpack name <> " is not a function"
        Nothing       -> fail $ "unknown function: " <> T.unpack name
      _ -> fail "application head must be a plain identifier"
  -- @branch@ isn't a filter (see the "Filters" section haddock) -- its
  -- call syntax is identical, but it's dispatched here by name, straight
  -- to 'fBranch', rather than through 'applyFilter'\/'FilterRegistry'.
  EFilter inner "branch" argEs -> do
    v    <- evalExpr filters resolveBranch env scope inner
    args <- mapM (evalExpr filters resolveBranch env scope) argEs
    fBranch resolveBranch v args
  EFilter inner name argEs -> do
    v    <- evalExpr filters resolveBranch env scope inner
    args <- mapM (evalExpr filters resolveBranch env scope) argEs
    pure (applyFilter filters name v args)

-- | Resolves every @%name%@ span against 'env' (a local binding's own
--   plain text -- see 'Storyteller.Context.DSL.Value.messagesText'),
--   leaving literal spans untouched.
interpText
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> InterpText -> StoreT m Text
interpText filters resolveBranch env scope = fmap T.concat . mapM part
  where
    part (Lit t)    = pure t
    part (Interp n) = messagesText <$> (valueDefault =<< evalExpr filters resolveBranch env scope (EIdent n))

-- | @read@'s argument text -- almost 'interpText', with one addition the
--   worked examples need and the bare-token lexeme alone can't resolve:
--   a *single-segment bare* token (@read f@, @read term@ -- no @/@, no
--   @%...%@ span) is lexically identical whether the author meant "the
--   literal filename @f@" or "whatever path is bound to the loop
--   variable @f@" (see the Chekhov's-gun and living-glossary examples,
--   both of which rely on the latter). Since a real filename can't
--   simultaneously be a bound local name, preferring the variable when
--   one exists is unambiguous, and falls straight through to plain
--   literal-text lookup otherwise -- covering both without new syntax.
--   Quoted tokens are deliberately excluded from this: quoting is
--   meaningful, not stylistic (see "Value model"), and a quoted
--   @read "injury"@ must always mean the literal text @injury@, never a
--   same-named local variable.
pathLitText
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> PathLit -> StoreT m Text
pathLitText filters resolveBranch env scope (PathLit Bare [Lit name])
  | Map.member name env = messagesText <$> (valueDefault =<< evalExpr filters resolveBranch env scope (EIdent name))
pathLitText filters resolveBranch env scope (PathLit _ parts) = interpText filters resolveBranch env scope parts

-- | @read@'s own path resolution: a flat scope (a branch tree or a glob
--   result -- see 'treeValueOfCommit') stores full paths as its own
--   entries' keys, so the whole literal text is tried as one key first.
--   Falls back to 'lookupPath'\'s segment-by-segment walk for a scope
--   that's genuinely nested (an @as@-export map reached via a partial
--   path, say) -- cheap to keep as a fallback and matches rule 3's own
--   "looks it up by key, recursively" wording, even though nothing this
--   interpreter builds today actually produces multi-level nesting.
resolveRead :: Monad m => Value m -> Text -> m (Maybe (Value m))
resolveRead scope path = case Map.lookup path (valueEntries scope) of
  Just action -> Just <$> action
  Nothing     -> lookupPath scope (T.splitOn "/" path)

globMatchPat :: Monad m => Value m -> Text -> m [Text]
globMatchPat scope pat = do
  allPaths <- listPaths scope
  let compiled = Glob.compile (T.unpack pat)
  pure (List.sort (filter (Glob.match compiled . T.unpack) allPaths))

globMatch
  :: Core.StoreM m
  => FilterRegistry (StoreT m) -> BranchResolver m -> Env m -> Value (StoreT m) -> PathLit -> StoreT m [Text]
globMatch filters resolveBranch env scope (PathLit _ parts) =
  interpText filters resolveBranch env scope parts >>= globMatchPat scope

forceAt :: Monad m => Value m -> Text -> m (Value m)
forceAt scope path = maybe emptyValue id <$> resolveRead scope path

-- ---------------------------------------------------------------------------
-- Filters -- all of them pure, no exceptions. Applying a filter is a
-- synchronous, deterministic Value -> Value transform, full stop -- any
-- forcing a filter's own logic needs (reading an argument's text,
-- checking whether the piped value is empty, ...) is deferred *into the
-- returned Value's own fields*, which are already m-actions by
-- construction (the same laziness every other Value in this interpreter
-- has), not new effectfulness smuggled into the filter itself.
--
-- @branch@ genuinely can't be written this way -- 'Value'\'s own shape
-- ("Value model") requires 'valueEntries'\'s *key set* to be known
-- without running @m@ at all (a plain 'Map', never @m (Map ...)@), and
-- knowing what files exist under a branch is inescapably a storage read.
-- Rather than force that mismatch into this Map (a sum type, an
-- "effectful" filter that isn't really a filter), @branch@ simply isn't
-- one -- it's dispatched by name in 'evalExpr' directly, exactly the
-- same shape as every filter call syntactically, but not routed through
-- 'applyFilter'\/'FilterRegistry' at all. Its own implementation
-- ('fBranch') is ordinary 'StoreT' code, no different in kind from
-- @read@\'s -- 'evalExpr' already runs in 'StoreT' for every case, so
-- this isn't a new capability being introduced, just the one filter-
-- shaped call that can't be answered by rearranging values it was
-- already handed.
-- ---------------------------------------------------------------------------

-- | A filter's implementation: the piped 'Value', its call arguments,
--   the resulting 'Value' -- constructed immediately, no monad wrapper.
type DSLFilter m = Value m -> [Value m] -> Value m

type FilterRegistry m = Map Name (DSLFilter m)

-- | A 'Value' that fails when forced, never at construction -- how a
--   filter reports a problem (an arity mismatch, an unimplemented
--   filter) without itself needing to run in @m@.
errorValue :: MonadFail m => String -> Value m
errorValue msg = Value { valueDefault = fail msg, valueEntries = Map.empty }

-- | Every filter except @branch@ (see the section haddock) and the
--   content-\/LLM-backed ones, deliberately left as loud failures rather
--   than silent no-ops: they need a real LLM effect this module doesn't
--   yet take, and pretending otherwise would be worse than not having
--   them.
coreFilters :: MonadFail m => FilterRegistry m
coreFilters = Map.fromList
  [ ("orifempty",    fOrIfEmpty)
  , ("pinned",       fPassthrough)   -- v1 forces every leaf eagerly regardless (see spec's "Interpretation" section) -- 'pinned' only matters to a future budget-aware interpreter.
  , ("filewithname", fFileWithName)
  , ("charname",     fPassthrough)   -- stub: no character-display-name registry to resolve against yet: passes the identifier's own text through unchanged.
  , ("truncate",     fTruncate)
  , ("join",         fJoin)
  , ("without",      fWithout)
  , ("only",         fOnly)
  , ("latest",       fLatest)
  , ("summarize",          fNotImplemented "summarize")
  , ("draftDefinition",    fNotImplemented "draftDefinition")
  , ("extractProperNouns", fNotImplemented "extractProperNouns")
  , ("exclude",      fNotImplemented "exclude")
  , ("whereType",    fNotImplemented "whereType")
  , ("whereTag",     fNotImplemented "whereTag")
  , ("sortBy",       fNotImplemented "sortBy")
  ]

applyFilter :: MonadFail m => FilterRegistry m -> Name -> Value m -> [Value m] -> Value m
applyFilter filters name v args = case Map.lookup name filters of
  Nothing   -> errorValue ("unknown filter: " <> T.unpack name)
  Just impl -> impl v args

fNotImplemented :: MonadFail m => String -> DSLFilter m
fNotImplemented label _ _ = errorValue $
  "filter `" <> label <> "` is not yet implemented (needs a real LLM/content-analysis effect)"

fPassthrough :: DSLFilter m
fPassthrough v _ = v

-- | Picks the fallback's own default text\/entries only when @v@'s own
--   default turns out empty once forced -- deferred into the result's
--   'valueDefault' rather than decided at filter-application time.
--   'valueEntries' always stays @v@'s own: every real use passes a
--   plain string literal as the fallback (a leaf, no entries of its
--   own), so this never actually discards anything observable.
fOrIfEmpty :: MonadFail m => DSLFilter m
fOrIfEmpty v [fallback] = v
  { valueDefault = do
      msgs <- valueDefault v
      if null msgs then valueDefault fallback else pure msgs
  }
fOrIfEmpty _ args = errorValue $ "orifempty: expected exactly 1 argument, got " <> show (length args)

fFileWithName :: MonadFail m => DSLFilter m
fFileWithName v [] = leafValueM $ do
  msgs <- valueDefault v
  pure [User (T.pack (FP.takeBaseName (T.unpack (messagesText msgs))))]
fFileWithName _ args = errorValue $ "filewithname: expected no arguments, got " <> show (length args)

fTruncate :: MonadFail m => DSLFilter m
fTruncate v [nArg] = leafValueM $ do
  nMsgs <- valueDefault nArg
  let n = maybe (T.length (messagesText nMsgs)) id (readMaybeInt (messagesText nMsgs))
  msgs <- valueDefault v
  pure [User (T.take n (messagesText msgs))]
fTruncate _ args = errorValue $ "truncate: expected exactly 1 argument, got " <> show (length args)

fJoin :: MonadFail m => DSLFilter m
fJoin v [sepArg] = leafValueM $ do
  sepMsgs <- valueDefault sepArg
  let sep = messagesText sepMsgs
  entryTexts <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (Map.elems (valueEntries v))
  pure [User (T.intercalate sep entryTexts)]
fJoin _ args = errorValue $ "join: expected exactly 1 argument, got " <> show (length args)

-- | Trims @v@'s own entries down to the ones @args@' text names --
--   deferred per-entry rather than eagerly restricting the 'Map': the
--   argument names can only be known by forcing @args@, and 'Value'\'s
--   own shape ("Value model") requires 'valueEntries'\'s *key set* to be
--   known without running @m@ at all, so an excluded entry stays a key
--   (still visible to 'listPaths'\/glob matching) but becomes
--   'emptyValue' once actually forced -- observably equivalent for
--   every real use (nothing here globs *over* a @without@\/@only@
--   result), and the only way to keep filter application itself pure.
fWithout :: Monad m => DSLFilter m
fWithout v args = v { valueEntries = Map.mapWithKey trim (valueEntries v) }
  where
    trim k action = do
      excluded <- mapM (\a -> messagesText <$> valueDefault a) args
      if k `elem` excluded then pure emptyValue else action

fOnly :: Monad m => DSLFilter m
fOnly v args = v { valueEntries = Map.mapWithKey trim (valueEntries v) }
  where
    trim k action = do
      keep <- mapM (\a -> messagesText <$> valueDefault a) args
      if k `elem` keep then action else pure emptyValue

-- | Same deferred-per-entry shape as 'fWithout'\/'fOnly' -- the *set* of
--   keys ('Map.keys') is available purely, so the sort\/cutoff needs no
--   forcing; only @n@ itself does.
fLatest :: MonadFail m => DSLFilter m
fLatest v [nArg]
  | null (valueEntries v) = v -- applied to a single already-read leaf (see the invented-calendar example): no list structure to take "latest" from, pass through unchanged.
  | otherwise = v { valueEntries = Map.mapWithKey trim (valueEntries v) }
  where
    sortedKeys = List.sortBy (flip compare) (Map.keys (valueEntries v))
    trim k action = do
      nMsgs <- valueDefault nArg
      let n = maybe 1 id (readMaybeInt (messagesText nMsgs))
      if k `elem` take n sortedKeys then action else pure emptyValue
fLatest _ args = errorValue $ "latest: expected exactly 1 argument, got " <> show (length args)

leafValueM :: m [Message] -> Value m
leafValueM action = Value { valueDefault = action, valueEntries = Map.empty }

readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing

-- ---------------------------------------------------------------------------
-- Branch resolution -- injected, not hardcoded
-- ---------------------------------------------------------------------------

-- | Resolving a 'BranchName' to its current head is the one operation
--   genuinely impossible to express as plain @'Core.StoreM' m => ...@
--   (see the module haddock: 'Storyteller.Core.Storage.StoryStorage' is
--   a separate effect from 'Core.MonadStore' throughout this codebase).
--   Rather than hardcoding a Polysemy-shaped answer to that here, it's a
--   parameter -- whoever actually runs this against real git supplies a
--   real resolver (closing over whatever effect system they use; a
--   Polysemy one calling @getBranch@ is the obvious choice, but this
--   module never needs to know that). Keeps every function below,
--   including @branch@ itself, exactly as generic as the rest of this
--   module -- no @Sem@, no Polysemy import, anywhere in this file.
type BranchResolver m = BranchName -> m (Maybe Core.ObjectHash)

-- | @branch@'s own implementation -- not part of 'coreFilters' (see the
--   "Filters" section haddock: this is the one filter-shaped operation
--   that needs a real capability, not just forcing values it was
--   already handed), so it stays a plain function taking that
--   capability explicitly, dispatched by name in 'evalExpr' rather than
--   living in the pure registry. Resolves its argument's text as a
--   character branch name via the supplied 'BranchResolver' (@lift@ed
--   into 'StoreT', the one spot this module lifts a base-monad action --
--   everywhere else is plain 'StoreT' composition), then hands off to
--   'treeValueOfCommit' exactly like the initial scope was built.
fBranch :: Core.StoreM m => BranchResolver m -> Value (StoreT m) -> [Value (StoreT m)] -> StoreT m (Value (StoreT m))
fBranch resolveBranch v [] = do
  ident <- messagesText <$> valueDefault v
  lift (resolveBranch (BranchName ("character/" <> ident))) >>= \case
    Nothing     -> fail ("branch not found: character/" <> T.unpack ident)
    Just commit -> treeValueOfCommit commit
fBranch _ _ args = fail $ "branch: expected no arguments, got " <> show (length args)

-- | 'treeValueOfCommit' for a named branch -- resolves the name via the
--   supplied 'BranchResolver', then delegates; the resulting 'Value' is
--   exactly as generic as 'treeValueOfCommit'\'s own, this only adds the
--   name lookup.
treeValueOfBranch :: Core.StoreM m => BranchResolver m -> BranchName -> StoreT m (Value (StoreT m))
treeValueOfBranch resolveBranch name = lift (resolveBranch name) >>= \case
  Nothing     -> fail ("branch not found: " <> T.unpack (unBranchName name))
  Just commit -> treeValueOfCommit commit

-- | The whole pipeline as one 'StoreT' computation, one 'Core.runStoreT'
--   dispatch: resolve @branchName@, build its tree as the initial scope,
--   compile @def@ against it with 'coreFilters' and the same resolver.
--   What a host actually calls -- everything above is the reusable
--   machinery this assembles. Still fully generic: the concrete effect
--   system only shows up in whatever 'BranchResolver' the caller passes.
runDefinitionOnBranch
  :: Core.StoreM m
  => BranchResolver m
  -> BranchName
  -> Definition
  -> [StoreT m (Value (StoreT m))]
  -> m (Value (StoreT m), Core.ScopeState)
runDefinitionOnBranch resolveBranch branchName def args =
  resolveBranch branchName >>= \case
    Nothing -> fail ("branch not found: " <> T.unpack (unBranchName branchName))
    Just commit -> Core.runStoreT commit $ do
      scope <- treeValueOfCommit commit
      compileDefinition coreFilters resolveBranch def scope args
