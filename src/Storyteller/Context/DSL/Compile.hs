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
--   invented). This is exactly 'Storyteller.Context.DSL.Value.MonadBranch'
--   -- an 'Action' just carries it as a constraint, alongside
--   'Core.StoreM', rather than closing over one resolver value fixed at
--   build time (which would break the moment a deferred @as@-export,
--   built once and run later, crosses a branch under a caller its
--   builder never saw). See @branch@\'s own implementation ('fBranch')
--   and 'treeValueOfBranch', the one place resolution actually happens.
--   The vocabulary's own closedness (see "Filters" in the spec) means
--   'coreFilters' is the only 'FilterRegistry' anywhere -- filters are
--   referenced directly, never threaded as a parameter. This module
--   never imports @polysemy@ or any @Storyteller.Core.*@ Polysemy-effect
--   module.
module Storyteller.Context.DSL.Compile
  ( -- * The interpreter
    Binding(..)
  , bval
  , fn1
  , fn2
  , Env
  , DSLFilter
  , FilterRegistry
  , coreFilters
  , errorValue
  , compileDefinition
  , runStmts
  , evalExpr
  , treeValueOfCommit
  , currentScope
  , runDefinition

    -- * Branch resolution -- injected, not hardcoded
  , fBranch
  , treeValueOfBranch
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
import Storyteller.Writer.Library (naturalKey)

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
--
--   'Binding' isn't only for local @let@s -- it's also the currency
--   'compileDefinition'\/'runDefinition' take *external* arguments as
--   (see 'defParams'), which is what makes a host-implemented function
--   (the invented-calendar example's @dateMath@: "math operations that
--   only make sense for this particular call", not a general-vocabulary
--   filter) a legitimate argument, not just a leaf 'Value' -- "values are
--   just 0-arity functions, otherwise no different." 'bval'\/'fn1'\/'fn2'
--   are the two ways to build one from the outside.
data Binding = Binding Int ([Action Value] -> Value -> Action Value)

-- | Wraps an already-scoped 'Action' as a 0-arity 'Binding' -- the
--   ordinary "just a value" case, and by far the common one.
bval :: Action Value -> Binding
bval action = Binding 0 (\_ _ -> action)

-- | Wraps a plain, scope-blind Haskell function as a 1-arity 'Binding'
--   -- what a host passes a real function (@dateMath@-style: a pure
--   transform over its own argument, no reason to see the caller's
--   ambient scope) in as. The wrong-length-@args@ case can't actually
--   happen ('evalExpr'\'s @EApp@\/@EIdent@ cases already check arity
--   before ever calling into a 'Binding'\'s own function), but 'Binding'
--   itself carries no type-level guarantee of that, so this fails loudly
--   rather than via an incomplete pattern match.
fn1 :: (Action Value -> Action Value) -> Binding
fn1 f = Binding 1 go
  where
    go [a] _    = f a
    go args _   = fail $ "fn1: expected exactly 1 argument, got " <> show (length args)

-- | 'fn1', two arguments.
fn2 :: (Action Value -> Action Value -> Action Value) -> Binding
fn2 f = Binding 2 go
  where
    go [a, b] _ = f a b
    go args _   = fail $ "fn2: expected exactly 2 arguments, got " <> show (length args)

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
    , valueEntries =
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
--   supplied initial scope and argument list -- the entry point a host
--   embeds (matching the spec's own framing: "a compiled context ends
--   up with exactly the type shape a hand-written agent already has").
compileDefinition
  :: Definition
  -> Value          -- ^ initial ambient Reader scope
  -> [Binding]      -- ^ arguments, matched against 'defParams'
  -> Action Value
compileDefinition def scope args
  | length args /= length (defParams def) = fail $
      "arity mismatch: " <> show (length (defParams def)) <> " parameter(s), "
        <> show (length args) <> " argument(s) given"
  | otherwise = do
      let env = Map.fromList (zip (defParams def) args)
      mkValue <$> runStmts env scope (defBody def)

mkValue :: ([Message], [(Name, Action Value)]) -> Value
mkValue (msgs, entries) = Value (pure msgs) entries

-- | Combines a newly-produced entry list with what's already
--   accumulated -- @new@'s own values win on a key collision (matching
--   'Data.Map.Strict.union's convention, which this replaced), but the
--   combined order keeps @old@'s entries in their existing position,
--   only appending genuinely new keys at the end. Declaration order,
--   preserved by construction now that 'valueEntries' is an ordered
--   list rather than a 'Map' (see its own haddock).
unionEntries :: [(Name, a)] -> [(Name, a)] -> [(Name, a)]
unionEntries new old =
  [ (k, maybe v id (lookup k new)) | (k, v) <- old ]
  ++ filter (\(k, _) -> k `notElem` map fst old) new

-- | Runs a whole 'Block', folding every statement's contribution into
--   one @([Message], entries)@ pair -- this *is* "a fresh writer
--   target" (rules 4\/5): whoever calls this (a function body, an @as@
--   body, 'compileDefinition') wraps the result into a new 'Value'.
--   @in@\/@for@ do *not* call this to get an independent 'Value' of
--   their own -- per rule 6, "the writer target is untouched" -- they
--   fold their own nested statements into the *same* accumulator this
--   call already threads (see their cases below).
runStmts
  :: Env -> Value -> Block
  -> Action ([Message], [(Name, Action Value)])
runStmts env0 scope0 = go env0 scope0 [] []
  where
    go _ _ msgs entries [] = pure (concat (reverse msgs), entries)
    go env scope msgs entries (Located _ (SExpr e) : rest) = do
      v <- evalExpr env scope e
      m <- valueDefault v
      go env scope (m : msgs) entries rest
    go env scope msgs entries (Located pos (SAs nameE body) : rest) = do
      name <- nameOf env scope nameE
      when (any ((== name) . fst) entries) $
        fail $ "duplicate 'as' name " <> show name <> " at line " <> show (posLine pos)
      let entryAction = mkValue <$> runStmts env scope body
      go env scope msgs (entries ++ [(name, entryAction)]) rest
    go env scope msgs entries (Located _ (SLet name mParams body) : rest) =
      let binding = case mParams of
            Nothing -> bval (mkValue <$> runStmts env scope body)
            Just ps -> Binding (length ps) $ \args callerScope ->
              mkValue <$> runStmts (bindParams ps args env) callerScope body
      in go (Map.insert name binding env) scope msgs entries rest
    go env scope msgs entries (Located _ (SIn e body) : rest) = do
      newScope <- evalExpr env scope e
      (m, es) <- runStmts env newScope body
      go env scope (m : msgs) (unionEntries es entries) rest
    go env scope msgs entries (Located pos (SFor var pathLit body) : rest) = do
      matches <- globMatch env scope pathLit
      (m, es) <- foldM (runOneIteration pos var body) ([], entries) matches
      go env scope (m : msgs) es rest
      where
        runOneIteration p var' body' (msgsAcc, entriesAcc) matchedPath = do
          let env' = Map.insert var' (bval (pure (leafValue [User matchedPath]))) env
          (m1, es1) <- runStmts env' scope body'
          case filter ((`elem` map fst entriesAcc) . fst) es1 of
            ((dup, _) : _) -> fail $ "duplicate 'as' name " <> show dup
                                <> " across for-loop iterations, near line " <> show (posLine p)
            []             -> pure ()
          pure (msgsAcc ++ m1, unionEntries es1 entriesAcc)

bindParams :: [Name] -> [Action Value] -> Env -> Env
bindParams ps args env = List.foldl' (\e (p, a) -> Map.insert p (bval a) e) env (zip ps args)

nameOf :: Env -> Value -> Expr -> Action Name
nameOf env scope e = do
  v <- evalExpr env scope e
  messagesText <$> valueDefault v

evalExpr :: Env -> Value -> Expr -> Action Value
evalExpr env scope e = case e of
  EString Quoted parts -> leafValue . (: []) . User <$> interpText env scope parts
  EString Bare   parts -> do
    pat     <- interpText env scope parts
    matches <- globMatchPat scope pat
    entries <- mapM (\m -> (,) m . pure <$> forceAt scope m) matches
    pure (Value (pure []) entries)
  EAssistant inner -> do
    v    <- evalExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (Assistant . messageText) msgs) }
  EUser inner -> do
    v    <- evalExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (User . messageText) msgs) }
  EIdent name -> case Map.lookup name env of
    Just (Binding 0 fn)     -> fn [] scope
    Just (Binding arity _)  -> fail $
      T.unpack name <> " needs " <> show arity <> " argument(s), used with none"
    Nothing -> fail $ "unknown identifier: " <> T.unpack name
  ERead pathLit -> do
    path <- pathLitText env scope pathLit
    resolveRead scope path >>= \case
      Nothing -> pure emptyValue
      Just v  -> pure v
  EApp headE argEs -> do
    let args = map (evalExpr env scope) argEs
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
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    fBranch v args
  -- @without@\/@only@\/@exclude@\/@latest@ all decide *which keys
  -- survive* -- genuinely shrinking 'valueEntries', "like [a dropped key]
  -- was never there" (not just neutering a kept key's own content to
  -- 'emptyValue', the old behaviour) -- so, like @branch@, they're
  -- dispatched here rather than through the pure 'applyFilter'\/
  -- 'FilterRegistry': deciding the surviving key set needs each
  -- argument's own text forced first (see 'argCriteria'), which a pure
  -- @Value -> [Value] -> Value@ filter structurally can't do. This is
  -- what makes @exclude(lore)@ -- passing in another already-computed
  -- definition's own result, not just a literal pattern -- actually work:
  -- 'lore's own key *names* are known purely (no forcing needed at all),
  -- and once matched keys are genuinely gone, a subsequent @for@\/glob
  -- over the result can't resurrect them the way it used to.
  EFilter inner "without" argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    shrinkEntries (==) False v args
  EFilter inner "only" argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    shrinkEntries (==) True v args
  EFilter inner "exclude" argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    shrinkEntries globMatches False v args
  EFilter inner "latest" argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    case args of
      [nArg] -> do
        nMsgs <- valueDefault nArg
        let n = maybe 1 id (readMaybeInt (messagesText nMsgs))
        if null (valueEntries v)
          then pure v -- a single already-read leaf (see the invented-calendar example): no list to take "latest" from.
          else do
            let latestKeys = take n (List.sortBy (flip compare) (map fst (valueEntries v)))
            pure v { valueEntries = filter (\(k, _) -> k `elem` latestKeys) (valueEntries v) }
      _ -> fail $ "latest: expected exactly 1 argument, got " <> show (length args)
  EFilter inner name argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    pure (applyFilter name v args)

-- | What a single @without@\/@only@\/@exclude@ argument contributes to the
--   match set: another already-computed 'Value' with its own entries
--   (e.g. @lore@, passed in as @exclude(lore)@) contributes its own key
--   *names* directly -- always known purely, no forcing needed -- a plain
--   leaf (an ordinary string-literal pattern\/name) contributes its own
--   forced default text instead.
argCriteria :: Value -> Action [Text]
argCriteria a
  | null (valueEntries a) = (: []) . messagesText <$> valueDefault a
  | otherwise             = pure (map fst (valueEntries a))

globMatches :: Text -> Text -> Bool
globMatches k pat = Glob.match (Glob.compile (T.unpack pat)) (T.unpack k)

-- | Shrinks @v@'s own 'valueEntries' by whether each key satisfies
--   @matches@ against any criterion contributed by @args@ (see
--   'argCriteria'). @keep = True@ retains a key some criterion matches
--   (@only@); @keep = False@ drops it (@without@\/@exclude@).
shrinkEntries :: (Text -> Text -> Bool) -> Bool -> Value -> [Value] -> Action Value
shrinkEntries matches keep v args = do
  criteria <- concat <$> mapM argCriteria args
  let isMatched k = any (matches k) criteria
  pure v { valueEntries = filter (\(k, _) -> isMatched k == keep) (valueEntries v) }

-- | Resolves every @%name%@ span against 'env' (a local binding's own
--   plain text -- see 'Storyteller.Context.DSL.Value.messagesText'),
--   leaving literal spans untouched.
interpText :: Env -> Value -> InterpText -> Action Text
interpText env scope = fmap T.concat . mapM part
  where
    part (Lit t)    = pure t
    part (Interp n) = messagesText <$> (valueDefault =<< evalExpr env scope (EIdent n))

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
pathLitText :: Env -> Value -> PathLit -> Action Text
pathLitText env scope (PathLit Bare [Lit name])
  | Map.member name env = messagesText <$> (valueDefault =<< evalExpr env scope (EIdent name))
pathLitText env scope (PathLit _ parts) = interpText env scope parts

-- | @read@'s own path resolution: a flat scope (a branch tree or a glob
--   result -- see 'treeValueOfCommit') stores full paths as its own
--   entries' keys, so the whole literal text is tried as one key first.
--   Falls back to 'lookupPath'\'s segment-by-segment walk for a scope
--   that's genuinely nested (an @as@-export map reached via a partial
--   path, say) -- cheap to keep as a fallback and matches rule 3's own
--   "looks it up by key, recursively" wording, even though nothing this
--   interpreter builds today actually produces multi-level nesting.
resolveRead :: Value -> Text -> Action (Maybe Value)
resolveRead scope path = case lookup path (valueEntries scope) of
  Just action -> Just <$> action
  Nothing     -> lookupPath scope (T.splitOn "/" path)

-- | Matches never re-sort -- 'listPaths' already walks 'valueEntries' in
--   that 'Value's own order (see its own haddock: "order is a real,
--   preserved property"), so a scope's current order (construction order
--   for a freshly-read tree, or whatever a prior 'fSortBy' left it in)
--   survives a glob untouched. This is what makes 'sortBy' observable
--   through an ordinary @in ...: for f in ...: as f: ...@ re-export, not
--   just through 'fJoin' -- forcing a fresh lexical sort here every time
--   would silently undo any reordering a filter upstream already did.
globMatchPat :: Value -> Text -> Action [Text]
globMatchPat scope pat = do
  allPaths <- listPaths scope
  let compiled = Glob.compile (T.unpack pat)
  pure (filter (Glob.match compiled . T.unpack) allPaths)

globMatch :: Env -> Value -> PathLit -> Action [Text]
globMatch env scope (PathLit _ parts) =
  interpText env scope parts >>= globMatchPat scope

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
--
-- @without@\/@only@\/@exclude@\/@latest@ join @branch@ in that same
-- exception, for a related but distinct reason: deciding which keys
-- *survive* needs each argument's own text forced first (or, for a
-- 'Value' argument with its own entries, its key names -- always known
-- purely, see 'argCriteria'), and only 'evalExpr' -- not a pure
-- 'DSLFilter' -- runs in 'Action' at all. See their own dispatch in
-- 'evalExpr' and 'shrinkEntries'.
-- ---------------------------------------------------------------------------

-- | A filter's implementation: the piped 'Value', its call arguments,
--   the resulting 'Value' -- constructed immediately, no 'Action' wrapper.
type DSLFilter = Value -> [Value] -> Value

type FilterRegistry = Map Name DSLFilter

-- | A 'Value' that fails when forced, never at construction -- how a
--   filter reports a problem (an arity mismatch, an unimplemented
--   filter) without itself needing to run anything.
errorValue :: String -> Value
errorValue msg = Value { valueDefault = fail msg, valueEntries = [] }

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
  , ("sortBy",       fSortBy)
  , ("summarize",          fNotImplemented "summarize")
  , ("draftDefinition",    fNotImplemented "draftDefinition")
  , ("extractProperNouns", fNotImplemented "extractProperNouns")
  , ("whereType",    fNotImplemented "whereType")
  , ("whereTag",     fNotImplemented "whereTag")
  ]

applyFilter :: Name -> Value -> [Value] -> Value
applyFilter name v args = case Map.lookup name coreFilters of
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
  entryTexts <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (map snd (valueEntries v))
  pure [User (T.intercalate sep entryTexts)]
fJoin _ args = errorValue $ "join: expected exactly 1 argument, got " <> show (length args)

-- | Reorders 'valueEntries' by 'naturalKey' on each entry's own key text
--   (@\"ch2\"@ before @\"ch11\"@) -- decidable purely from the key set
--   already required to exist without forcing anything (see 'Value's own
--   haddock on why 'valueEntries' is a list, not a 'Map'), so, unlike
--   'summarize'\/'draftDefinition'\/'extractProperNouns', this needs no
--   LLM/content-analysis effect at all -- unusual for wanting an argument
--   list of exactly zero, since the ordering itself is fixed (there's only
--   one @naturalKey@), not a piped-in comparator.
fSortBy :: DSLFilter
fSortBy v [] = v { valueEntries = List.sortBy (\a b -> compare (naturalKey (fst a)) (naturalKey (fst b))) (valueEntries v) }
fSortBy _ args = errorValue $ "sortBy: expected no arguments, got " <> show (length args)

leafValueA :: Action [Message] -> Value
leafValueA action = Value { valueDefault = action, valueEntries = [] }

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
--   'askBranch', then delegates. The one case a Reader-scope switch
--   genuinely does correspond to a different commit (contrast
--   'currentScope', which needs no name or lookup at all).
treeValueOfBranch :: BranchName -> Action Value
treeValueOfBranch name = askBranch name >>= \case
  Nothing     -> fail ("branch not found: " <> T.unpack (unBranchName name))
  Just commit -> treeValueOfCommit commit

-- | The Reader scope for wherever this 'Action' is actually run --
--   'Storyteller.Context.DSL.Value.currentHead', read straight off the
--   ambient 'Core.StoreT' position, no 'Storyteller.Core.Types.BranchName'
--   or lookup needed. This is what makes 'runDefinition' able to just say
--   "run in whatever branch/session I'm already in."
currentScope :: Action Value
currentScope = currentHead >>= treeValueOfCommit

-- | The whole pipeline as one 'Action': take whatever commit is
--   currently ambient as the initial scope, compile @def@ against it.
--   What a host actually calls -- everything above is the reusable
--   machinery this assembles. Still fully generic: the concrete backend
--   only enters when the returned 'Action' is finally run via
--   'Storyteller.Context.DSL.Value.runAction'.
runDefinition :: Definition -> [Binding] -> Action Value
runDefinition def args = currentScope >>= \scope -> compileDefinition def scope args
