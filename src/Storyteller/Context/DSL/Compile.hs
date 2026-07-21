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
  , definitionBinding
  , runStmts
  , evalExpr
  , treeValueOfCommit
  , currentScope
  , runDefinition

    -- * Branch resolution -- injected, not hardcoded
  , branchBinding
  , charactersInBinding
  , treeValueOfBranch
  , journalDelta
  , readConversation
  , embedShallow
  ) where

import Control.Monad (foldM, when)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob

import qualified Storage.Core as Core
import qualified Storage.Query as Query
import qualified Storage.Tick as Tick

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Value

import Storyteller.Core.Types (BranchName(..))
import qualified Storyteller.Writer.Agent.MessageWindow as MessageWindow
import qualified Storyteller.Writer.Branches as Branches
import Storyteller.Writer.Library (naturalKey)
import qualified Storyteller.Writer.Presence as Presence
import Storyteller.Writer.Types (Character(..))

-- ---------------------------------------------------------------------------
-- Bindings and environment
-- ---------------------------------------------------------------------------

-- | 'Binding'\/'bval'\/'fn1'\/'fn2' now live in
--   "Storyteller.Context.DSL.Value" (re-exported here for every existing
--   caller) -- moved so 'Storyteller.Context.DSL.Value.ContextLibrary' can
--   hold compiled 'Binding's directly without a module cycle; see that
--   module's own Haddock on 'Binding'.
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
--   nested trie -- 'Storage.Query.loadLiveWorkingTree' (structure only, no
--   blob reads beyond what it needs for its own atom-tracking check)
--   already gives exactly this shape, the same way a glob result's own
--   'entries' is already flat and keyed by full matched path (see "Value
--   model"). 'read'\'s resolution (see 'evalExpr'\'s @ERead@ case) tries
--   the whole literal path as one flat key first for exactly this reason,
--   before ever falling back to 'lookupPath'\'s segment-by-segment walk.
--   Content stays deferred: each leaf's own 'valueDefault' is its own
--   'Core.readObject' call, forced only when a definition actually reads
--   it.
--
--   Never-atom-tracked paths (an uploaded binary asset, say) are already
--   gone by the time this sees the tree -- 'Storage.Query.loadLiveWorkingTree'
--   is the one place that policy is decided, deliberately at the storage
--   layer rather than here: a raw, non-UTF8 blob has no sensible
--   'Message' to become at all, so this module -- the DSL's own
--   interpreter -- never needs to know binary files exist in the first
--   place, rather than filtering them out after the fact.
treeValueOfCommit :: Core.ObjectHash -> Action Value
treeValueOfCommit commit = do
  files <- Action (\_lib -> Query.loadLiveWorkingTree commit)
  pure Value
    { valueDefault = pure []
    , valueEntries =
        [ (T.pack path, withProvenance path commit . leafValue . (: []) . FileRead path . TE.decodeUtf8 <$> readBlob h)
        | (path, h) <- files
        ]
    , valueMeta = defaultMeta
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
mkValue (msgs, entries) = Value (pure msgs) entries defaultMeta

-- | Compiles a parsed 'Definition' into a 'Binding' that runs against
--   whatever scope its *caller* hands it (via 'compileDefinition' directly,
--   not 'runDefinition''s own fresh 'currentScope') -- what
--   'Storyteller.Core.Context.buildContextLibrary' uses to turn each
--   pure-DSL entry (branch-committed or compiled-in) into the same
--   'Binding' shape a host-backed library entry already is, so
--   'Storyteller.Context.DSL.Value.ContextLibrary' can hold both
--   uniformly -- see that type's own Haddock.
definitionBinding :: Definition -> Binding
definitionBinding def = Binding (length (defParams def)) (\args scope -> compileDefinition def scope (map bval args))

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
    go env scope msgs entries (Located pos (SFor var srcExpr body) : rest) = do
      srcVal <- evalExpr env scope srcExpr
      let matches = map fst (valueEntries srcVal)
      (m, es) <- foldM (runOneIteration pos var body) ([], entries) matches
      go env scope (m : msgs) es rest
      where
        -- The loop variable's own default is still just the matched
        -- key's text (so @f | filewithname@\/bare @f@ stay as cheap as
        -- ever, never forcing content) -- but it now also carries a
        -- single self-keyed entry holding the real, lazy resolution at
        -- that key (the same 'forceAt' a glob match's own entries
        -- already use), so @read f@ can resolve it via the identical
        -- "force this Value's own entries" rule any other @read@
        -- argument uses, with no identifier-specific case needed at all
        -- (see 'ERead's own haddock).
        runOneIteration p var' body' (msgsAcc, entriesAcc) matchedPath = do
          let loopVar = Value (pure [User matchedPath]) [(matchedPath, forceAt scope matchedPath)] defaultMeta
              env'    = Map.insert var' (bval (pure loopVar)) env
          (m1, es1) <- runStmts env' scope body'
          case filter ((`elem` map fst entriesAcc) . fst) es1 of
            ((dup, _) : _) -> fail $ "duplicate 'as' name " <> show dup
                                <> " across for-loop iterations, near line " <> show (posLine p)
            []             -> pure ()
          pure (msgsAcc ++ m1, unionEntries es1 entriesAcc)

bindParams :: [Name] -> [Action Value] -> Env -> Env
bindParams ps args env = List.foldl' (\e (p, a) -> Map.insert p (bval a) e) env (zip ps args)

-- | Resolves an identifier against the current definition's own local
--   'Env' first (parameters, @let@s, @for@-loop variables), falling back
--   to the shared library table ('lookupLibrary') only on a local miss --
--   the same shadowing a local variable would give a same-named library
--   entry in any ordinary language. A library 'Binding' -- pure-DSL or
--   host-backed alike -- already takes the *caller's own* ambient scope
--   (never a fresh 'currentScope') when it's pure-DSL, by construction of
--   however 'Storyteller.Core.Context.buildContextLibrary' compiled it --
--   see 'ContextLibrary's own Haddock for why that's what makes a library
--   reference behave indistinguishably from a local one.
resolveIdent :: Env -> Name -> Action Binding
resolveIdent env name = case Map.lookup name env of
  Just b  -> pure b
  Nothing -> lookupLibrary name >>= \case
    Just b  -> pure b
    Nothing -> fail $ "unknown identifier: " <> T.unpack name

-- | 'resolveIdent', but reporting a miss as 'Nothing' rather than
--   failing -- what 'ERead' needs to tell "this identifier is bound
--   locally or in the library" apart from "this bare token was never
--   bound anywhere, so read's own fallback (treat its own text as a
--   literal path) applies instead." Nowhere else needs this: an
--   unresolved identifier is a hard failure everywhere else in the
--   language (see the Grammar section of @CONTEXT-DSL.md@) -- @read@ is
--   the one primitive whose argument has no other sensible meaning than
--   a path, so an unbound bare token there still means something, rather
--   than being definitely a mistake.
tryResolveIdent :: Env -> Name -> Action (Maybe Binding)
tryResolveIdent env name = case Map.lookup name env of
  Just b  -> pure (Just b)
  Nothing -> lookupLibrary name

nameOf :: Env -> Value -> Expr -> Action Name
nameOf env scope e = do
  v <- evalExpr env scope e
  messagesText <$> valueDefault v

evalExpr :: Env -> Value -> Expr -> Action Value
evalExpr env scope e = case e of
  EString Quoted parts -> leafValue . (: []) . User <$> interpText env scope parts
  EString Bare   parts -> interpText env scope parts >>= globResolve scope
  EAssistant inner -> do
    v    <- evalExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (Assistant . messageText) msgs) }
  EUser inner -> do
    v    <- evalExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (User . messageText) msgs) }
  EIdent name -> resolveIdent env name >>= \case
    Binding 0 fn    -> fn [] scope
    Binding arity _ -> fail $
      T.unpack name <> " needs " <> show arity <> " argument(s), used with none"
  -- | @read@'s argument is a general 'Expr' now, with two of its shapes
  -- given @read@'s own reading rather than the ordinary one:
  --
  --   * A string literal (quoted or bare alike -- quoting always means
  --     "definitely a path/glob, never a variable" here, deconflicting a
  --     literal filename from a same-named local) resolves via the
  --     identical glob machinery a bare expression-position glob already
  --     uses, so @read *.md@ genuinely can match more than one file.
  --   * A bare identifier that isn't bound *anywhere* (not a local, not
  --     in the library -- see 'tryResolveIdent') still means a literal
  --     path, the one place in the language an unresolved name isn't a
  --     hard failure (see the Grammar section of @CONTEXT-DSL.md@): a
  --     bound identifier (a parameter, a @let@, a @for@-loop variable --
  --     whose own 'valueEntries' now carries its real content, see
  --     'runStmts'\'s @SFor@ case -- or a library function) evaluates
  --     normally instead.
  --
  -- Anything else (an application, a filter chain) evaluates normally
  -- too. Whichever path produced it, if the resulting 'Value' has
  -- entries, @read@ forces each one in order and folds their own content
  -- into the result's own default, keeping 'valueEntries' itself intact;
  -- a 'Value' with none (an already-resolved single leaf) is returned
  -- unchanged.
  ERead argExpr -> do
    v <- case argExpr of
      EString _ parts -> interpText env scope parts >>= globResolve scope
      EIdent name -> tryResolveIdent env name >>= \case
        Just _  -> evalExpr env scope argExpr
        Nothing -> globResolve scope name
      _ -> evalExpr env scope argExpr
    if null (valueEntries v)
      then pure v
      else do
        forced   <- mapM snd (valueEntries v)
        combined <- concat <$> mapM valueDefault forced
        pure v { valueDefault = pure combined }
  EApp headE argEs -> do
    let args = map (evalExpr env scope) argEs
    case headE of
      EIdent name -> do
        Binding arity fn <- resolveIdent env name
        when (length args /= arity) $ fail $
          T.unpack name <> ": expected " <> show arity <> " argument(s), got " <> show (length args)
        fn args scope
      _ -> fail "application head must be a plain identifier"
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
  -- Every other filter name: the pure registry first (the common,
  -- zero-capability case), then the shared library, exactly like an
  -- ordinary identifier ('resolveIdent') -- @expr | name(args...)@ is
  -- just @name expr args...@ with the piped value moved to the front, so
  -- a filter needing real capability (@branch@, @charactersin@) is a
  -- 'Storyteller.Context.DSL.Value.Binding' like any other library entry,
  -- not a case hardcoded into this interpreter -- adding a new one only
  -- ever touches "Storyteller.Context.DSL.Library" now, never this
  -- module. 'EIdent'\/'EApp' already resolve a bare name the identical
  -- way; this is that same fallback, just called with the piped value
  -- prepended to whatever explicit arguments followed the pipe.
  EFilter inner name argEs -> do
    v    <- evalExpr env scope inner
    args <- mapM (evalExpr env scope) argEs
    case Map.lookup name coreFilters of
      Just impl -> impl v args
      Nothing   -> resolveIdent env name >>= \case
        Binding arity fn
          | arity /= 1 + length args -> fail $
              T.unpack name <> ": expected " <> show (arity - 1)
                <> " argument(s) after the pipe, got " <> show (length args)
          | otherwise -> fn (pure v : map pure args) scope

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

forceAt :: Value -> Text -> Action Value
forceAt scope path = maybe emptyValue id <$> resolveRead scope path

-- | Matches @pat@ against @scope@ (a glob if it has metacharacters, an
--   exact match otherwise -- 'Glob.compile' treats a plain string
--   literally) and builds a container 'Value' keyed by each match, its
--   own entry the real, lazy resolution at that path -- exactly what a
--   bare glob expression already builds (see 'EString'\'s own
--   @Bare@ case), reused here by 'ERead' for both a literal string
--   argument and an otherwise-unresolved bare identifier.
globResolve :: Value -> Text -> Action Value
globResolve scope pat = do
  matches <- globMatchPat scope pat
  entries <- mapM (\m -> (,) m . pure <$> forceAt scope m) matches
  pure (Value (pure []) entries defaultMeta)

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

-- | A filter's implementation: the piped 'Value', its call arguments, an
--   'Action' of the resulting 'Value' -- effectful at the same ceiling as
--   everything else in the DSL ('Core.StoreM'\/'MonadBranch', no LLM, no
--   mutation), not the pure @Value -> [Value] -> Value@ this used to be.
--   That restriction was a consequence of the type picked, not a
--   principle worth protecting -- see @CONTEXT-DSL.md@'s "Filters"
--   section. "Fully applied, a filter still produces a Value" remains
--   true in exactly the sense it's true everywhere else in this
--   interpreter: 'Action' 'Value' *is* "a Value" the moment something
--   forces it, and nothing changes at the surface syntax
--   (@expr | filterName(args)@ still just denotes another 'Value').
--
--   @branch@\/@without@\/@only@\/@exclude@\/@latest@ still aren't part of
--   this registry -- not because they need effects a plain 'DSLFilter'
--   now lacks (that reason is gone), but because they either need a
--   capability 'Binding' already owns (@branch@) or need to shrink
--   'valueEntries' after forcing each argument's own criteria first
--   ('shrinkEntries'), which is still simplest left as 'evalExpr's own
--   dispatch rather than folded into this registry's uniform shape.
type DSLFilter = Value -> [Value] -> Action Value

type FilterRegistry = Map Name DSLFilter

-- | A 'Value' that fails when forced, never at construction -- how a
--   filter reports a problem (an arity mismatch, an unimplemented
--   filter) without itself needing to run anything.
errorValue :: String -> Value
errorValue msg = Value { valueDefault = fail msg, valueEntries = [], valueMeta = defaultMeta }

-- | @summarize@\/@draftDefinition@\/@extractProperNouns@\/@whereType@\/
--   @whereTag@ are still left as loud failures -- not because a filter
--   can't reach storage any more (it can), but because real semantics
--   for these need a tagging convention this pass doesn't decide, or (for
--   @summarize@) an LLM effect still genuinely outside 'Action's own
--   ceiling. Pretending otherwise would be worse than not having them.
coreFilters :: FilterRegistry
coreFilters = Map.fromList
  [ ("orifempty",     fOrIfEmpty)
  , ("pinned",        fPinned)
  , ("priority",      fPriority)
  , ("summarizable",  fSummarizable)
  , ("filewithname",  fFileWithName)
  , ("charname",      fPassthrough)   -- stub: no character-display-name registry to resolve against yet: passes the identifier's own text through unchanged.
  , ("truncate",       fTruncate)
  , ("join",           fJoin)
  , ("sortBy",         fSortBy)
  , ("name",           fName)
  , ("abstract",       fAbstract)
  , ("summarize",          fNotImplemented "summarize")
  , ("draftDefinition",    fNotImplemented "draftDefinition")
  , ("extractProperNouns", fNotImplemented "extractProperNouns")
  , ("whereType",    fNotImplemented "whereType")
  , ("whereTag",     fNotImplemented "whereTag")
  ]

fNotImplemented :: String -> DSLFilter
fNotImplemented label _ _ = pure $ errorValue $
  "filter `" <> label <> "` is not yet implemented (needs a real LLM/content-analysis effect)"

fPassthrough :: DSLFilter
fPassthrough v _ = pure v

-- | Sets 'Pinned' in 'metaFlags' -- what a budget-aware renderer (outside
--   this module) treats as "never drop this."
fPinned :: DSLFilter
fPinned v [] = pure v { valueMeta = addFlag Pinned (valueMeta v) }
fPinned _ args = pure $ errorValue $ "pinned: expected no arguments, got " <> show (length args)

-- | Sets 'Summarizable' in 'metaFlags' -- what a budget-aware renderer may
--   replace with an LLM summary of the same text under pressure, rather
--   than dropping outright.
fSummarizable :: DSLFilter
fSummarizable v [] = pure v { valueMeta = addFlag Summarizable (valueMeta v) }
fSummarizable _ args = pure $ errorValue $ "summarizable: expected no arguments, got " <> show (length args)

-- | Sets 'metaPriority' -- higher survives longer under budget pressure
--   (see 'Priority's own haddock).
fPriority :: DSLFilter
fPriority v [nArg] = do
  nMsgs <- valueDefault nArg
  let n = maybe 0 id (readMaybeInt (messagesText nMsgs))
  pure v { valueMeta = (valueMeta v) { metaPriority = Priority n } }
fPriority _ args = pure $ errorValue $ "priority: expected exactly 1 argument, got " <> show (length args)

addFlag :: ItemFlag -> Meta -> Meta
addFlag flag m = m { metaFlags = Set.insert flag (metaFlags m) }

-- | Picks the fallback's own default text\/entries only when @v@'s own
--   default turns out empty once forced -- deferred into the result's
--   'valueDefault' rather than decided at filter-application time.
--   'valueEntries' always stays @v@'s own: every real use passes a
--   plain string literal as the fallback (a leaf, no entries of its
--   own), so this never actually discards anything observable.
fOrIfEmpty :: DSLFilter
fOrIfEmpty v [fallback] = pure v
  { valueDefault = do
      msgs <- valueDefault v
      if null msgs then valueDefault fallback else pure msgs
  }
fOrIfEmpty _ args = pure $ errorValue $ "orifempty: expected exactly 1 argument, got " <> show (length args)

fFileWithName :: DSLFilter
fFileWithName v [] = pure $ leafValueA $ do
  msgs <- valueDefault v
  pure [User (T.pack (FP.takeBaseName (T.unpack (messagesText msgs))))]
fFileWithName _ args = pure $ errorValue $ "filewithname: expected no arguments, got " <> show (length args)

fTruncate :: DSLFilter
fTruncate v [nArg] = pure $ leafValueA $ do
  nMsgs <- valueDefault nArg
  let n = maybe (T.length (messagesText nMsgs)) id (readMaybeInt (messagesText nMsgs))
  msgs <- valueDefault v
  pure [User (T.take n (messagesText msgs))]
fTruncate _ args = pure $ errorValue $ "truncate: expected exactly 1 argument, got " <> show (length args)

fJoin :: DSLFilter
fJoin v [sepArg] = pure $ leafValueA $ do
  sepMsgs <- valueDefault sepArg
  let sep = messagesText sepMsgs
  entryTexts <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (map snd (valueEntries v))
  pure [User (T.intercalate sep entryTexts)]
fJoin _ args = pure $ errorValue $ "join: expected exactly 1 argument, got " <> show (length args)

-- | Reorders 'valueEntries' by 'naturalKey' on each entry's own key text
--   (@\"ch2\"@ before @\"ch11\"@) -- decidable purely from the key set
--   already required to exist without forcing anything (see 'Value's own
--   haddock on why 'valueEntries' is a list, not a 'Map'), so, unlike
--   'summarize'\/'draftDefinition'\/'extractProperNouns', this needs no
--   LLM/content-analysis effect at all -- unusual for wanting an argument
--   list of exactly zero, since the ordering itself is fixed (there's only
--   one @naturalKey@), not a piped-in comparator.
fSortBy :: DSLFilter
fSortBy v [] = pure v { valueEntries = List.sortBy (\a b -> compare (naturalKey (fst a)) (naturalKey (fst b))) (valueEntries v) }
fSortBy _ args = pure $ errorValue $ "sortBy: expected no arguments, got " <> show (length args)

-- | Extracts a Markdown document's @# Title@ -- the one structural
--   convention @sheet.md@ is actually required to follow (see
--   @WRITER.md@: the first H1 is a character's display name). Unlike
--   @summarize@, this is convention over already-stored text, not
--   content analysis -- no LLM effect needed, so it's a plain filter
--   rather than something deferred to a render-time pass.
fName :: DSLFilter
fName v [] = pure $ leafValueA $ do
  msgs <- valueDefault v
  pure [User (mdHeading (messagesText msgs))]
fName _ args = pure $ errorValue $ "name: expected no arguments, got " <> show (length args)

-- | Extracts the paragraph immediately following a Markdown document's
--   leading @# Title@ -- the "acquaintance blurb" convention: whatever
--   prose a sheet opens with, up to the next blank line or heading.
--   Same non-LLM rationale as 'fName'.
fAbstract :: DSLFilter
fAbstract v [] = pure $ leafValueA $ do
  msgs <- valueDefault v
  pure [User (mdLeadParagraph (messagesText msgs))]
fAbstract _ args = pure $ errorValue $ "abstract: expected no arguments, got " <> show (length args)

mdHeading :: Text -> Text
mdHeading = go . T.lines
  where
    go []       = ""
    go (l : ls)
      | T.isPrefixOf "# " stripped = T.strip (T.drop 2 stripped)
      | otherwise                  = go ls
      where stripped = T.stripStart l

mdLeadParagraph :: Text -> Text
mdLeadParagraph txt =
  T.strip $ T.unlines $ takeWhile (not . isBoundary) $ dropWhile T.null $
    drop 1 $ dropWhile (not . isHeading) (T.lines txt)
  where
    isHeading  l = T.isPrefixOf "#" (T.stripStart l)
    isBoundary l = T.null l || isHeading l

leafValueA :: Action [Message] -> Value
leafValueA action = Value { valueDefault = action, valueEntries = [], valueMeta = defaultMeta }

readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing

-- ---------------------------------------------------------------------------
-- Branch resolution -- injected, not hardcoded
-- ---------------------------------------------------------------------------

-- | @branch@'s own implementation, as an ordinary 'Binding' -- not part
--   of 'coreFilters' (see the "Filters" section haddock: this is a
--   filter-shaped operation that needs a real capability, not just
--   forcing values it was already handed), but not hardcoded into
--   'evalExpr' either: it's registered in
--   'Storyteller.Context.DSL.Library.hostLibrary' like any other
--   host-backed name, resolved the same way whether it's called bare
--   (@branch charname@) or piped (@charname | branch@ -- see 'EFilter'\'s
--   own fallthrough case, which is exactly "the piped value becomes the
--   first argument"). Resolves its argument's text as a character branch
--   name via 'askBranch', then hands off to 'treeValueOfCommit' exactly
--   like the initial scope was built.
branchBinding :: Binding
branchBinding = fn1 go
  where
    go vArg = do
      ident <- messagesText <$> (valueDefault =<< vArg)
      askBranch (BranchName ("character/" <> ident)) >>= \case
        Nothing     -> fail ("branch not found: character/" <> T.unpack ident)
        Just commit -> treeValueOfCommit commit

-- | @charactersin@'s own implementation, as an ordinary 'Binding' -- same
--   reasoning as 'branchBinding': it needs real 'Core.StoreM' access
--   (presence-tick data isn't glob-derivable, the same reason tick
--   history needs 'readConversation' to be host-backed), but that's a
--   reason to register it in the library, not to hardcode a case into
--   this interpreter. @v@'s own forced text is the file path whose
--   presence ticks decide who's active, via
--   'Storyteller.Writer.Presence.activeCharacters' (the same pure fold
--   'Storyteller.Writer.Presence.activeCharactersFor' already wraps for
--   ordinary Haskell callers). No content sits at each entry -- the value
--   *is* the key (the identifier); a caller narrows further with @in
--   (charname | branch): ...@\/@describechar charname@, same as any other
--   character identifier this DSL already hands around.
charactersInBinding :: Binding
charactersInBinding = fn1 go
  where
    go vArg = do
      path  <- T.unpack . messagesText <$> (valueDefault =<< vArg)
      ticks <- Action (\_lib -> Tick.fileTicksOf path)
      let idents = [ Branches.branchDisplayName name | Character (BranchName name) <- Set.toList (Presence.activeCharacters ticks) ]
      pure (Value (pure []) [ (ident, pure (leafValue [User ident])) | ident <- idents ] defaultMeta)

-- | 'treeValueOfCommit' for a named branch -- resolves the name via
--   'askBranch', then delegates. The one case a Reader-scope switch
--   genuinely does correspond to a different commit (contrast
--   'currentScope', which needs no name or lookup at all).
treeValueOfBranch :: BranchName -> Action Value
treeValueOfBranch name = askBranch name >>= \case
  Nothing     -> fail ("branch not found: " <> T.unpack (unBranchName name))
  Just commit -> treeValueOfCommit commit

-- | A named character's own @journal.md@, curated by
--   'Storage.Tick.recentAtomsOf': entries that are byte-identical to
--   whatever they reference are dropped, kept ones bring @padding@
--   neighbours along -- see that function's own haddock for why that's
--   "the same content, not sent twice" rather than a length cap.
--
--   Deliberately a host-supplied 'Binding' ('fn1', not a 'coreFilters'
--   entry), for the same reason 'fBranch' is dispatched outside the
--   registry: it needs a real capability a pure 'DSLFilter' doesn't
--   have. But it goes further than 'fBranch' does -- it can't even lean
--   on an enclosing @in (charname | branch): ...@ to put it on the
--   right branch, because 'Storage.Tick.recentAtomsOf' reads the
--   *ambient* 'Core.StoreT' scope (@headHash@), and @in@\/@branch@ only
--   ever redirect the Reader-scope 'Value' that @read@\/@for@ glob
--   against -- they never reposition 'Core.StoreT' itself (see
--   'treeValueOfCommit': it takes an explicit commit hash rather than
--   reading @headHash@). So this resolves the character's branch itself
--   and hops there via 'Core.readAt', the same primitive
--   'Storyteller.Common.Summary' already uses for a historical peek that
--   must not disturb the caller's own position.
--
--   Takes @lookback@\/@maxOut@\/@padding@ baked in from the Haskell side
--   (a project's own tuning, not DSL-expressible policy -- mirrors the
--   invented-calendar example's own @dateMath@), and the character
--   identifier as its one DSL-side argument, e.g. @journal charname@
--   where @journal@ was threaded in as a parameter the same way
--   'fBranch' expects @charname | branch@'s own identifier.
journalDelta :: Int -> Int -> Int -> Binding
journalDelta lookback maxOut padding = fn1 go
  where
    go charnameArg = do
      ident <- messagesText <$> (valueDefault =<< charnameArg)
      askBranch (BranchName ("character/" <> ident)) >>= \case
        Nothing     -> fail ("branch not found: character/" <> T.unpack ident)
        Just commit -> do
          ticks <- Action (\_lib -> Core.readAt commit (Tick.recentAtomsOf "journal.md" lookback maxOut padding))
          pure (leafValue (renderJournalTicks ticks))

-- | One block per curated slice, joined by a plain divider -- entries
--   may span real timeline gaps (unremarkable ticks in between were
--   dropped), so they shouldn't read as one continuous entry, plus the
--   same framing header 'Storyteller.Writer.Agent.CharContext.
--   renderJournalContext' already uses (so a model doesn't mistake this
--   for objective narration) -- kept here rather than left to the
--   calling definition, since it's fixed framing text tied to what a
--   curated journal slice *is*, not project-overridable policy.
renderJournalTicks :: [Tick.FileTick] -> [Message]
renderJournalTicks []    = []
renderJournalTicks ticks =
  [ User $
      "### From this character's own journal (their private viewpoint -- may be biased, outdated, or contradict the wider record)\n\n"
      <> T.intercalate "\n\n---\n\n" (map Tick.ftMessage ticks)
  ]

-- | A file's own tick history, reconstructed as real, role-preserving
--   'Message's -- the DSL-level counterpart to
--   'Storyteller.Writer.Agent.Chat.historyFromFileTicks' (same source
--   data, same @"prompt"@\/@"atom"@\/hidden-tick rules), just producing
--   this module's own model-agnostic 'Message' instead of a
--   'UniversalLLM.Message' bound to one role. A host-supplied 'Binding'
--   for the same reason 'journalDelta' is one: tick history isn't
--   glob\/@read@-expressible, so there's real Haskell logic underneath,
--   but the DSL still decides *where* the result lands (@conv =
--   readconversation curchapter@, then composed with whatever else the
--   calling definition builds).
readConversation :: Binding
readConversation = fn1 go
  where
    go pathArg = do
      path  <- T.unpack . messagesText <$> (valueDefault =<< pathArg)
      ticks <- Action (\_lib -> Tick.fileTicksOf path)
      pure (leafValue (historyFromTicks ticks))

historyFromTicks :: [Tick.FileTick] -> [Message]
historyFromTicks = concatMap toMessage . filter (not . isHidden)
  where
    isHidden ft = lookup "hide" (Tick.ftFields ft) == Just "true"
    toMessage ft = case Tick.ftKind ft of
      "prompt" -> [User (Tick.ftMessage ft)]
      "atom"   -> [Assistant (maybe (Tick.ftMessage ft) id (Tick.ftContent ft))]
      _        -> []

-- | Splices @toInsert@ into @conv@ at a bounded depth from the end (2 to 4
--   turns, a project's own cache-vs-freshness tuning, baked in here the
--   same way 'journalDelta''s own curation numbers are rather than being
--   DSL-tunable) -- the DSL-level counterpart to
--   'Storyteller.Writer.Agent.MessageWindow.injectAtWindow', reusing its
--   own turn-boundary arithmetic ('Storyteller.Writer.Agent.MessageWindow.windowBoundary',
--   which is already generic over plain 'Int's, not tied to
--   'UniversalLLM.Message') against this module's own 'Message' instead.
--   See that function's own Haddock for why a *bounded* depth, not either
--   end, is what actually buys back prompt-cache hits across consecutive
--   turns.
embedShallow :: Binding
embedShallow = fn2 go
  where
    go convArg extraArg = do
      conv  <- valueDefault =<< convArg
      extra <- valueDefault =<< extraArg
      pure (leafValue (injectShallow isUserTurn 2 4 extra conv))
    isUserTurn (User _) = True
    isUserTurn _         = False

injectShallow :: (Message -> Bool) -> Int -> Int -> [Message] -> [Message] -> [Message]
injectShallow _ _ _ [] history = history
injectShallow isTurnStart lo hi toInsert history
  | boundary == 0 = toInsert ++ history
  | otherwise     = before ++ toInsert ++ after
  where
    turnIdxs = [ i | (i, m) <- zip [0 :: Int ..] history, isTurnStart m ]
    total    = length turnIdxs
    boundary = MessageWindow.windowBoundary lo hi total
    (before, after) = splitAt (turnIdxs !! boundary) history

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
