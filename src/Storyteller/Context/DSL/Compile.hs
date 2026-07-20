{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Interpreter for the Context DSL's AST (see
--   "Storyteller.Context.DSL.AST") into 'Value' -- an 'Action', per
--   @CONTEXT-DSL.md@'s "Implementation strategy" -- deliberately
--   separate from the parser ("Storyteller.Context.DSL.Parser"), and
--   from what any of this gets consumed into afterwards (a flattened
--   string, a browsable tree, a tool-mounted surface -- see
--   "Interpretation is not part of this spec").
--
--   == A pure AST -> Action compiler
--
--   Every function here is a plain, unconstrained function from AST to
--   'Action' -- @evalExpr :: ... -> Expr -> Action Value@, no @Core.StoreM
--   m =>@ constraint, no @m@ type parameter anywhere, because
--   'Storyteller.Context.DSL.Value.Action' already carries exactly that
--   genericity itself. Compiling never touches storage; only running the
--   resulting 'Action' (via 'Storyteller.Context.DSL.Value.runAction')
--   does, and only once the caller supplies a concrete backend.
--
--   'Core.readAt'\/'Core.loadWorkingTree'\/'Core.readObject' -- the same
--   "Storage.Core" combinators every hand-written agent already composes
--   with -- are lifted into 'Action' via
--   'Storyteller.Context.DSL.Value.liftStore'; nothing here reimplements
--   tree navigation. A Reader-scope switch (@in@, cross-branch or not)
--   is just calling 'treeValueOfCommit' with a different hash -- ordinary
--   value-level dynamic scoping.
--
--   The one operation genuinely impossible to express via 'Core.StoreM'
--   alone: resolving a 'Storyteller.Core.Types.BranchName' to a commit
--   ('Storyteller.Core.Storage.StoryStorage' is a separate effect from
--   'Core.MonadStore' throughout this codebase, not a gap this module
--   invented). Rather than close over a resolver inside a deferred
--   'Action' -- which would break the moment a deferred @as@-export
--   (built once, run later) crosses a branch, since the resolver that
--   was ambient at *build* time might not be the one its eventual caller
--   wants -- 'Action' takes a 'BranchResolver' as an explicit parameter
--   to 'runAction' itself, supplied fresh every time. See @branch@\'s own
--   implementation ('fBranch') and 'runDefinitionOnBranch', the one place
--   a concrete resolver (closing over @getBranch@ and whatever effect
--   system a caller uses) actually gets supplied. This module never
--   imports @polysemy@ or any @Storyteller.Core.*@ Polysemy-effect
--   module.
module Storyteller.Context.DSL.Compile
  ( -- * The interpreter
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
  , fBranch
  , treeValueOfBranch
  , runDefinitionOnBranch
  ) where

import Control.Monad (foldM, when)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob

import qualified Storage.Core as Core

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Value

import Storyteller.Core.Types (BranchName(..))

-- ---------------------------------------------------------------------------
-- Bindings and environment
-- ---------------------------------------------------------------------------

-- | What a local name (a @let@\/parameter\/loop variable) resolves to --
--   one constructor, since the DSL itself draws no line between "a
--   value" and "a function": a plain @x = body@ is exactly a 0-arity
--   'Binding' (per rule 1, "a file with no head is a 0-ary function --
--   i.e. an ordinary value"). The @[Action Value] -> Value -> Action
--   Value@ shape takes the *caller's* current ambient scope as an
--   explicit argument rather than closing over whatever scope was
--   active at definition time -- this is the whole of "Reader scope is
--   resolved entirely dynamically, at the point code actually runs": a
--   bound function is a Haskell closure over 'Env' (its lexical
--   bindings, fixed at definition time, exactly as @let@ should behave)
--   but *not* over 'Value' (its dynamic scope, supplied fresh on every
--   call). A 0-arity binding's own closure simply never looks at the
--   scope argument it's handed -- it already captured whatever it needed
--   eagerly, at the point @x = ...@ itself ran (see 'runStmts'\'s @SLet@
--   case) -- which is what makes the capture-before-narrowing pattern
--   (@root = **/*@, used later via @in root: ...@ regardless of what's
--   ambient by then) work at all.
data Binding = Binding Int ([Action Value] -> Value -> Action Value)

type Env = Map Name Binding

-- ---------------------------------------------------------------------------
-- Building a Reader scope from a commit
-- ---------------------------------------------------------------------------

-- | A commit's tree, as a 'Value' -- the Reader scope a top-level
--   definition's @read@\/glob resolve against before any @in@ narrows or
--   redirects it, and what 'fBranch' produces for an @in@ that crosses
--   into a different branch. Used identically for both: nothing here
--   knows or cares whether @commit@ is "the" branch the interpreter
--   started on or one reached via @charname | branch@ mid-evaluation.
--
--   'entries' is keyed by *full path*, one level, not a hand-rolled
--   nested trie -- 'Core.loadWorkingTree' (one call: structure only, no
--   blob reads) already gives exactly this shape, the same way a glob
--   result's own 'entries' is already flat and keyed by full matched
--   path (see "Value model"). 'read'\'s resolution (see 'evalExpr'\'s
--   @ERead@ case) tries the whole literal path as one flat key first for
--   exactly this reason, before ever falling back to 'lookupPath'\'s
--   segment-by-segment walk. Content stays deferred: each leaf's own
--   'valueDefault' is its own 'Core.readObject' call, forced only when a
--   definition actually reads it.
treeValueOfCommit :: Core.ObjectHash -> Action Value
treeValueOfCommit commit = do
  wt <- liftStore (Core.loadWorkingTree commit)
  pure Value
    { valueDefault = pure []
    , valueEntries = Map.fromList
        [ (T.pack path, leafValue . (: []) . FileRead path . TE.decodeUtf8 <$> readBlob h)
        | (path, Core.FSFile h) <- Map.toList wt
        ]
    }
  where
    readBlob h = liftStore $ Core.readObject h >>= \case
      Core.BlobObject bs -> pure bs
      Core.TreeObject _  -> fail "internal error: file path resolved to a tree object"

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Compiles and immediately runs a whole 'Definition' against a
--   supplied initial scope, argument list, and filter registry -- the
--   entry point a host embeds (matching the spec's own framing: "a
--   compiled context ends up with exactly the type shape a hand-written
--   agent already has").
compileDefinition
  :: FilterRegistry
  -> Definition
  -> Value            -- ^ initial ambient Reader scope
  -> [Action Value]    -- ^ arguments, matched against 'defParams'
  -> Action Value
compileDefinition filters def scope args
  | length args /= length (defParams def) = fail $
      "arity mismatch: " <> show (length (defParams def)) <> " parameter(s), "
        <> show (length args) <> " argument(s) given"
  | otherwise = do
      let env = Map.fromList (zip (defParams def) (map bval args))
      mkValue <$> runStmts filters env scope (defBody def)

-- | Wraps an already-scoped 'Action' as a 0-arity 'Binding' -- the
--   caller-supplied scope argument is simply never looked at.
bval :: Action Value -> Binding
bval action = Binding 0 (\_ _ -> action)

mkValue :: ([Message], Map Name (Action Value)) -> Value
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
  :: FilterRegistry -> Env -> Value -> Block
  -> Action ([Message], Map Name (Action Value))
runStmts filters env0 scope0 = go env0 scope0 [] Map.empty
  where
    go _ _ msgs entries [] = pure (concat (reverse msgs), entries)
    go env scope msgs entries (Located _ (SExpr e) : rest) = do
      v <- evalExpr filters env scope e
      m <- valueDefault v
      go env scope (m : msgs) entries rest
    go env scope msgs entries (Located pos (SAs nameE body) : rest) = do
      name <- nameOf filters env scope nameE
      when (Map.member name entries) $
        fail $ "duplicate 'as' name " <> show name <> " at line " <> show (posLine pos)
      let entryAction = mkValue <$> runStmts filters env scope body
      go env scope msgs (Map.insert name entryAction entries) rest
    go env scope msgs entries (Located _ (SLet name mParams body) : rest) =
      let binding = case mParams of
            Nothing -> bval (mkValue <$> runStmts filters env scope body)
            Just ps -> Binding (length ps) $ \args callerScope ->
              mkValue <$> runStmts filters (bindParams ps args env) callerScope body
      in go (Map.insert name binding env) scope msgs entries rest
    go env scope msgs entries (Located _ (SIn e body) : rest) = do
      newScope <- evalExpr filters env scope e
      (m, es) <- runStmts filters env newScope body
      go env scope (m : msgs) (Map.union es entries) rest
    go env scope msgs entries (Located pos (SFor var pathLit body) : rest) = do
      matches <- globMatch filters env scope pathLit
      (m, es) <- foldM (runOneIteration pos var body) ([], entries) matches
      go env scope (m : msgs) es rest
      where
        runOneIteration p var' body' (msgsAcc, entriesAcc) matchedPath = do
          let env' = Map.insert var' (bval (pure (leafValue [User matchedPath]))) env
          (m1, es1) <- runStmts filters env' scope body'
          case Map.keys (Map.intersection es1 entriesAcc) of
            (dup : _) -> fail $ "duplicate 'as' name " <> show dup
                           <> " across for-loop iterations, near line " <> show (posLine p)
            []        -> pure ()
          pure (msgsAcc ++ m1, Map.union es1 entriesAcc)

bindParams :: [Name] -> [Action Value] -> Env -> Env
bindParams ps args env = List.foldl' (\e (p, a) -> Map.insert p (bval a) e) env (zip ps args)

nameOf :: FilterRegistry -> Env -> Value -> Expr -> Action Name
nameOf filters env scope e = do
  v <- evalExpr filters env scope e
  messagesText <$> valueDefault v

evalExpr :: FilterRegistry -> Env -> Value -> Expr -> Action Value
evalExpr filters env scope e = case e of
  EString Quoted parts -> leafValue . (: []) . User <$> interpText filters env scope parts
  EString Bare   parts -> do
    pat     <- interpText filters env scope parts
    matches <- globMatchPat scope pat
    entries <- Map.fromList <$> mapM (\m -> (,) m . pure <$> forceAt scope m) matches
    pure (Value (pure []) entries)
  EAssistant parts -> leafValue . (: []) . Assistant <$> interpText filters env scope parts
  EUser inner -> do
    v    <- evalExpr filters env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (User . messageText) msgs) }
  EIdent name -> case Map.lookup name env of
    Just (Binding 0 fn)     -> fn [] scope
    Just (Binding arity _)  -> fail $
      T.unpack name <> " needs " <> show arity <> " argument(s), used with none"
    Nothing -> fail $ "unknown identifier: " <> T.unpack name
  ERead pathLit -> do
    path <- pathLitText filters env scope pathLit
    resolveRead scope path >>= \case
      Nothing -> pure emptyValue
      Just v  -> pure v
  EApp headE argEs -> do
    let args = map (evalExpr filters env scope) argEs
    case headE of
      EIdent name -> case Map.lookup name env of
        Just (Binding arity fn) -> do
          when (length args /= arity) $ fail $
            T.unpack name <> ": expected " <> show arity <> " argument(s), got " <> show (length args)
          fn args scope
        Nothing -> fail $ "unknown function: " <> T.unpack name
      _ -> fail "application head must be a plain identifier"
  -- @branch@ isn't a filter (see the "Filters" section haddock) -- its
  -- call syntax is identical, but it's dispatched here by name, straight
  -- to 'fBranch', rather than through 'applyFilter'\/'FilterRegistry'.
  EFilter inner "branch" argEs -> do
    v    <- evalExpr filters env scope inner
    args <- mapM (evalExpr filters env scope) argEs
    fBranch v args
  EFilter inner name argEs -> do
    v    <- evalExpr filters env scope inner
    args <- mapM (evalExpr filters env scope) argEs
    pure (applyFilter filters name v args)

-- | Resolves every @%name%@ span against 'env' (a local binding's own
--   plain text -- see 'Storyteller.Context.DSL.Value.messagesText'),
--   leaving literal spans untouched.
interpText :: FilterRegistry -> Env -> Value -> InterpText -> Action Text
interpText filters env scope = fmap T.concat . mapM part
  where
    part (Lit t)    = pure t
    part (Interp n) = messagesText <$> (valueDefault =<< evalExpr filters env scope (EIdent n))

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
pathLitText :: FilterRegistry -> Env -> Value -> PathLit -> Action Text
pathLitText filters env scope (PathLit Bare [Lit name])
  | Map.member name env = messagesText <$> (valueDefault =<< evalExpr filters env scope (EIdent name))
pathLitText filters env scope (PathLit _ parts) = interpText filters env scope parts

-- | @read@'s own path resolution: a flat scope (a branch tree or a glob
--   result -- see 'treeValueOfCommit') stores full paths as its own
--   entries' keys, so the whole literal text is tried as one key first.
--   Falls back to 'lookupPath'\'s segment-by-segment walk for a scope
--   that's genuinely nested (an @as@-export map reached via a partial
--   path, say) -- cheap to keep as a fallback and matches rule 3's own
--   "looks it up by key, recursively" wording, even though nothing this
--   interpreter builds today actually produces multi-level nesting.
resolveRead :: Value -> Text -> Action (Maybe Value)
resolveRead scope path = case Map.lookup path (valueEntries scope) of
  Just action -> Just <$> action
  Nothing     -> lookupPath scope (T.splitOn "/" path)

globMatchPat :: Value -> Text -> Action [Text]
globMatchPat scope pat = do
  allPaths <- listPaths scope
  let compiled = Glob.compile (T.unpack pat)
  pure (List.sort (filter (Glob.match compiled . T.unpack) allPaths))

globMatch :: FilterRegistry -> Env -> Value -> PathLit -> Action [Text]
globMatch filters env scope (PathLit _ parts) =
  interpText filters env scope parts >>= globMatchPat scope

forceAt :: Value -> Text -> Action Value
forceAt scope path = maybe emptyValue id <$> resolveRead scope path

-- ---------------------------------------------------------------------------
-- Filters -- all of them pure, no exceptions. Applying a filter is a
-- synchronous, deterministic Value -> Value transform, full stop -- any
-- forcing a filter's own logic needs (reading an argument's text,
-- checking whether the piped value is empty, ...) is deferred *into the
-- returned Value's own fields* (already 'Action's by construction, the
-- same laziness every other Value in this interpreter has), not new
-- effectfulness smuggled into the filter itself.
--
-- @branch@ genuinely can't be written this way -- 'Value'\'s own shape
-- ("Value model") requires 'valueEntries'\'s *key set* to be known
-- without running anything at all (a plain 'Map', never an 'Action' of
-- one), and knowing what files exist under a branch is inescapably a
-- storage read. Rather than force that mismatch into this Map (a sum
-- type, an "effectful" filter that isn't really a filter), @branch@
-- simply isn't one -- it's dispatched by name in 'evalExpr' directly,
-- exactly the same shape as every filter call syntactically, but not
-- routed through 'applyFilter'\/'FilterRegistry' at all. Its own
-- implementation ('fBranch') is ordinary 'Action' code, no different in
-- kind from @read@\'s.
-- ---------------------------------------------------------------------------

-- | A filter's implementation: the piped 'Value', its call arguments,
--   the resulting 'Value' -- constructed immediately, no 'Action' wrapper.
type DSLFilter = Value -> [Value] -> Value

type FilterRegistry = Map Name DSLFilter

-- | A 'Value' that fails when forced, never at construction -- how a
--   filter reports a problem (an arity mismatch, an unimplemented
--   filter) without itself needing to run anything.
errorValue :: String -> Value
errorValue msg = Value { valueDefault = fail msg, valueEntries = Map.empty }

-- | Every filter except @branch@ (see the section haddock) and the
--   content-\/LLM-backed ones, deliberately left as loud failures rather
--   than silent no-ops: they need a real LLM effect this module doesn't
--   yet take, and pretending otherwise would be worse than not having
--   them.
coreFilters :: FilterRegistry
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

applyFilter :: FilterRegistry -> Name -> Value -> [Value] -> Value
applyFilter filters name v args = case Map.lookup name filters of
  Nothing   -> errorValue ("unknown filter: " <> T.unpack name)
  Just impl -> impl v args

fNotImplemented :: String -> DSLFilter
fNotImplemented label _ _ = errorValue $
  "filter `" <> label <> "` is not yet implemented (needs a real LLM/content-analysis effect)"

fPassthrough :: DSLFilter
fPassthrough v _ = v

-- | Picks the fallback's own default text\/entries only when @v@'s own
--   default turns out empty once forced -- deferred into the result's
--   'valueDefault' rather than decided at filter-application time.
--   'valueEntries' always stays @v@'s own: every real use passes a
--   plain string literal as the fallback (a leaf, no entries of its
--   own), so this never actually discards anything observable.
fOrIfEmpty :: DSLFilter
fOrIfEmpty v [fallback] = v
  { valueDefault = do
      msgs <- valueDefault v
      if null msgs then valueDefault fallback else pure msgs
  }
fOrIfEmpty _ args = errorValue $ "orifempty: expected exactly 1 argument, got " <> show (length args)

fFileWithName :: DSLFilter
fFileWithName v [] = leafValueA $ do
  msgs <- valueDefault v
  pure [User (T.pack (FP.takeBaseName (T.unpack (messagesText msgs))))]
fFileWithName _ args = errorValue $ "filewithname: expected no arguments, got " <> show (length args)

fTruncate :: DSLFilter
fTruncate v [nArg] = leafValueA $ do
  nMsgs <- valueDefault nArg
  let n = maybe (T.length (messagesText nMsgs)) id (readMaybeInt (messagesText nMsgs))
  msgs <- valueDefault v
  pure [User (T.take n (messagesText msgs))]
fTruncate _ args = errorValue $ "truncate: expected exactly 1 argument, got " <> show (length args)

fJoin :: DSLFilter
fJoin v [sepArg] = leafValueA $ do
  sepMsgs <- valueDefault sepArg
  let sep = messagesText sepMsgs
  entryTexts <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (Map.elems (valueEntries v))
  pure [User (T.intercalate sep entryTexts)]
fJoin _ args = errorValue $ "join: expected exactly 1 argument, got " <> show (length args)

-- | Trims @v@'s own entries down to the ones @args@' text names --
--   deferred per-entry rather than eagerly restricting the 'Map': the
--   argument names can only be known by forcing @args@, and 'Value'\'s
--   own shape ("Value model") requires 'valueEntries'\'s *key set* to be
--   known without running anything at all, so an excluded entry stays a
--   key (still visible to 'listPaths'\/glob matching) but becomes
--   'emptyValue' once actually forced -- observably equivalent for
--   every real use (nothing here globs *over* a @without@\/@only@
--   result), and the only way to keep filter application itself pure.
fWithout :: DSLFilter
fWithout v args = v { valueEntries = Map.mapWithKey trim (valueEntries v) }
  where
    trim k action = do
      excluded <- mapM (\a -> messagesText <$> valueDefault a) args
      if k `elem` excluded then pure emptyValue else action

fOnly :: DSLFilter
fOnly v args = v { valueEntries = Map.mapWithKey trim (valueEntries v) }
  where
    trim k action = do
      keep <- mapM (\a -> messagesText <$> valueDefault a) args
      if k `elem` keep then action else pure emptyValue

-- | Same deferred-per-entry shape as 'fWithout'\/'fOnly' -- the *set* of
--   keys ('Map.keys') is available purely, so the sort\/cutoff needs no
--   forcing; only @n@ itself does.
fLatest :: DSLFilter
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

leafValueA :: Action [Message] -> Value
leafValueA action = Value { valueDefault = action, valueEntries = Map.empty }

readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing

-- ---------------------------------------------------------------------------
-- Branch resolution -- injected, not hardcoded
-- ---------------------------------------------------------------------------

-- | @branch@'s own implementation -- not part of 'coreFilters' (see the
--   "Filters" section haddock: this is the one filter-shaped operation
--   that needs a real capability, not just forcing values it was
--   already handed), so it's dispatched by name in 'evalExpr' rather
--   than living in the pure registry. Resolves its argument's text as a
--   character branch name via 'askBranch', then hands off to
--   'treeValueOfCommit' exactly like the initial scope was built.
fBranch :: Value -> [Value] -> Action Value
fBranch v [] = do
  ident <- messagesText <$> valueDefault v
  askBranch (BranchName ("character/" <> ident)) >>= \case
    Nothing     -> fail ("branch not found: character/" <> T.unpack ident)
    Just commit -> treeValueOfCommit commit
fBranch _ args = fail $ "branch: expected no arguments, got " <> show (length args)

-- | 'treeValueOfCommit' for a named branch -- resolves the name via
--   'askBranch', then delegates.
treeValueOfBranch :: BranchName -> Action Value
treeValueOfBranch name = askBranch name >>= \case
  Nothing     -> fail ("branch not found: " <> T.unpack (unBranchName name))
  Just commit -> treeValueOfCommit commit

-- | The whole pipeline as one 'Action': resolve @branchName@, build its
--   tree as the initial scope, compile @def@ against it with
--   'coreFilters'. What a host actually calls -- everything above is
--   the reusable machinery this assembles. Still fully generic: the
--   concrete effect system, and the real branch resolver, only enter
--   when the returned 'Action' is finally run via
--   'Storyteller.Context.DSL.Value.runAction'.
runDefinitionOnBranch :: BranchName -> Definition -> [Action Value] -> Action Value
runDefinitionOnBranch branchName def args = do
  scope <- treeValueOfBranch branchName
  compileDefinition coreFilters def scope args
