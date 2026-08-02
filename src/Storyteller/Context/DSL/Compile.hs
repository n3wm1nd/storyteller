{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
  , Library(..)
  , emptyLibrary
  , buildLibrary
  , DSLFilter
  , FilterRegistry
  , coreFilters
  , errorValue
  , compileDefinition
  , definitionBinding
  , runCompiledBlock
  , runCompiledExpr
  , ContextFS(..)
  , scopeOfFileSystem
  , currentScope
  , runDefinition
  , runNamed

    -- * Branch resolution -- injected, not hardcoded
  , branchBinding
  , charactersInBinding
  , summarizedBinding
  , summarizedOnceBinding
  , treeValueOfBranch
  , journalDelta
  , readConversation
  , embedShallow
  , hostLibrary
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

import Polysemy (Member, Members)
import Polysemy.Fail (Fail)

import qualified Storage.Core as Core

import Storyteller.Context.DSL.AST
import Storyteller.Context.DSL.Value

import Runix.FileSystem (FileSystem, FileSystemRead)
import qualified Runix.FileSystem as FS

import Storyteller.Writer.Agent.Summarizer (densest)
import Storyteller.Writer.Conversation (Turn(..), conversationTurns)
import Storyteller.Writer.Journal (JournalCuration(..), journalWindow)
import Storyteller.Writer.Presence (activeCharactersFor)
import Storyteller.Core.Branch (Branches, Visited, withBranch)
import Storyteller.Core.Git (BranchOp, BranchTag, runStoryFSRead)
import Storyteller.Core.Types (BranchName(..))
import qualified Storyteller.Writer.Agent.MessageWindow as MessageWindow
import qualified Storyteller.Writer.Branches as Branches
import Storyteller.Writer.Library (naturalKey)
import Storyteller.Writer.Types (Character(..))

-- ---------------------------------------------------------------------------
-- Bindings and environment
-- ---------------------------------------------------------------------------

-- | 'Binding'\/'bval'\/'fn1'\/'fn2' now live in
--   "Storyteller.Context.DSL.Value" (re-exported here for every existing
--   caller) -- moved so a library table (see 'Library') can hold compiled
--   'Binding's directly without a module cycle; see that module's own
--   Haddock on 'Binding'.
type Env r = Map Name (Binding r)

-- | The compile-time table an in-progress 'definitionBinding' call closes
--   over -- "everything compiled strictly before this slot," per
--   'buildLibrary''s fixed left-to-right build order. Kept as a separate
--   parameter from 'Env' throughout this module rather than merged into
--   it: 'Env' genuinely varies per call (fresh @args@\/@scope@ each time a
--   'Binding' runs), while this table is fixed once, for the lifetime of
--   whatever 'Binding' closure it was compiled into -- merging the two
--   would blur "resolved once, at compile time" with "rebuilt on every
--   call," which is exactly the distinction that makes self-reference
--   resolve to the *previous* binding rather than looping into itself.
--
--   __An ordered sequence, not a 'Map'__ -- this is the one representation
--   choice that actually matters here, not an arbitrary container swap.
--   The same 'Name' genuinely means *different* 'Binding's at different
--   points in the sequence (an override replaces a slot's definition from
--   that slot onward; everything compiled before it still saw the old
--   one), so "the library" isn't one fixed table with entries mutated in
--   place -- it's a sequence of tables, one per position, that happen to
--   share a representation. A 'Map' can only ever answer "what does this
--   name mean in the finished table," which is exactly the question that
--   doesn't have one right answer here. A list keyed by construction order
--   answers the question this module actually needs -- "what does this
--   name mean as of /this/ slot" -- as a plain positional fact: look up
--   the first match walking from here toward the list's own end, i.e.
--   toward whatever was compiled earlier. See 'buildLibrary'.
newtype Library r = Library { libraryEntries :: [(Name, Binding r)] }

emptyLibrary :: Library r
emptyLibrary = Library []

-- | Builds a 'Library' from an ordered @[(Name, Definition)]@ by compiling
--   left to right, consing each freshly-compiled @(Name, Binding r)@ onto
--   the front of what's already there. A definition at position @i@ closes
--   over exactly the 'Library' built from positions @< i@ -- everything
--   compiled earlier, nothing compiled later or at the same position --
--   which is what makes an override landing at the same key as an earlier
--   entry a genuine, order-sensitive replacement rather than an in-place
--   mutation: whichever definition physically occupies a given position in
--   the input list is what every /later/ position's own references see,
--   full stop, regardless of what a same-named entry earlier or later in
--   the list happens to be.
--
--   Two entries sharing one 'Name' is meaningful, not an error or a
--   collision to resolve -- see the type's own Haddock. Nothing here
--   deduplicates by key; a caller wanting "override replaces the default"
--   gets that by constructing its input list with the override's
--   'Definition' standing in the default's own position (see
--   'Storyteller.Core.Context.spliceOverrides'), not by this function
--   doing any lookup/replace of its own.
--
--   Stops at the first slot 'definitionBinding' rejects, reporting which
--   name it was -- there is no partial 'Library' to salvage past that
--   point (every later slot's own closure would be built against a table
--   missing the entry that failed), and no reason to build one: the whole
--   sequence is plain, already-parsed data, so re-attempting without the
--   offending override (see 'Storyteller.Core.Context.buildContextLibrary')
--   is exactly as cheap as building was the first time.
--
--   Starts from @seed@ (typically 'emptyLibrary', or
--   'Storyteller.Context.DSL.Library.hostLibrary' for a caller whose
--   sequence wants to reference @branch@\/@charactersin@\/... real
--   Haskell closures that were never themselves parsed from DSL text, so
--   have no 'Definition' of their own to fold in this way) rather than
--   always starting empty -- a host binding never itself references
--   another library name (see 'hostLibrary''s own Haddock), so seeding it
--   first is always safe regardless of what @defs@ goes on to reference.
buildLibrary :: Member Fail r => Library r -> [(Name, Definition)] -> Either (Name, String) (Library r)
buildLibrary seed = foldl' step (Right seed)
  where
    step (Left err) _ = Left err
    step (Right lib) (name, def) = case definitionBinding lib def of
      Left err      -> Left (name, err)
      Right binding -> Right (Library ((name, binding) : libraryEntries lib))

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
-- | The @project@ tag the context DSL reads through.
--
--   One concrete tag, not a phantom threaded from the caller, because DSL
--   text has no concept of /which/ filesystem it is reading -- @read
--   lore/x.md@ means "in the scope I'm evaluating against," full stop. A
--   phantom would put a type parameter on every quasiquoted definition to
--   express a distinction the language itself cannot make. (Whoever wires
--   this decides what it reads; see
--   'Storyteller.Core.Context.runContextValue'.)
--
--   Crossing branches, the one place that assumption bends, is handled
--   without bending it: @charname | branch@ enters the other branch,
--   builds a scope there, and /forces/ it before returning (see
--   'branchBinding'), so what comes back is an ordinary already-read
--   'Value' rather than a second filesystem the DSL would have to name.
data ContextFS = ContextFS

scopeOfFileSystem
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => Action r (Value r)
scopeOfFileSystem = do
  files <- liftSem (FS.glob @project "" "**/*")
  pure Value
    { valueDefault = pure []
    , valueEntries =
        [ (T.pack path, withProvenance path . leafValue . (: []) . FileRead path <$> readEntry path)
        | path <- files
        ]
    , valueMeta = defaultMeta
    }
  where
    readEntry path = TE.decodeUtf8 <$> liftSem (FS.readFile @project path)

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- The compiled IR -- 'Expr'\/'Stmt'\/'Block' with every reference site
-- resolved exactly once, at compile time
-- ---------------------------------------------------------------------------

-- | What a single name reference in a definition's body turns out to mean,
--   decided once, by 'compileExpr', never re-decided afterward. This is
--   the actual fix for "resolution should happen at compile time, not
--   look a name up again on every call" -- not merely checking that a
--   reference *would* resolve and then discarding that work (what an
--   earlier version of this module did), but keeping the answer:
--
--   * 'CGlobal' -- a reference to a 'Library' entry. The real 'Binding'
--     itself is baked directly into the compiled tree here -- there is no
--     'Name' left at this node at all once compiled, and so no lookup of
--     any kind, against 'Library' or anything else, ever happens for it
--     again, on any future call. This is the common case: most references
--     in a real definition's body are to other library definitions
--     (@context.lore@, @loreEntry@, ...), and every one of them now costs
--     nothing more at call time than reading a field already sitting in
--     the compiled closure.
--   * 'CLocalRef' -- a genuine local (a parameter, a @let@, a @for@-loop
--     variable). Unlike a library entry, a local's *identity* (which
--     binder a given reference means) is exactly as static as a global's
--     -- decided here, by the same compile pass, from the body's own
--     lexical structure alone -- but its *value* is inescapably per-call
--     (a fresh argument each time the enclosing 'Binding' is invoked, a
--     fresh iteration each time a @for@ loop runs), so this keeps the
--     'Name' and defers only the one thing that really can't be resolved
--     any earlier: looking that value up in the small, locals-only 'Env'
--     'runCompiled' threads per call (never the whole 'Library' -- see
--     'compileExpr's own Haddock for why that distinction is exactly what
--     was missing before).
data CRef r
  = CGlobal !(Binding r)
  | CLocalRef !Name

-- | 'Expr', with every 'EIdent'\/'EApp' head\/'EFilter' name and every
--   @%name%@ interpolation span replaced by its own resolved 'CRef'.
--   Otherwise a structural mirror of 'Expr' -- see 'compileExpr'.
data CExpr r
  = CString !Quoting ![CInterpPart r]
  | CAssistant !(CExpr r)
  | CUser !(CExpr r)
  | CIdent !(CRef r)
  | CApp !(CRef r) ![CExpr r]
  | CFilterWithout !(CExpr r) ![CExpr r]
  | CFilterOnly !(CExpr r) ![CExpr r]
  | CFilterExclude !(CExpr r) ![CExpr r]
  | CFilterLatest !(CExpr r) ![CExpr r]
  | CFilterCore !Name !(CExpr r) ![CExpr r]
  | CFilterUser !(CRef r) !(CExpr r) ![CExpr r]
  | CRead !(CReadArg r)

-- | 'ERead's own three argument shapes (see its Haddock in the pre-IR
--   'evalExpr', now folded into compile time instead of decided fresh on
--   every call): a string literal is always a path\/glob, resolved
--   against the ambient Reader scope, never a name lookup at all; a bare
--   identifier that resolves to a real binding (local or library) reads
--   through it normally; a bare identifier that resolves to nothing at
--   all still means a literal path, 'ERead''s one narrow exception to
--   "unresolved is a hard failure" -- decided once, here, rather than via
--   'tryResolveIdent''s runtime probe.
data CReadArg r
  = CReadPath ![CInterpPart r]
  | CReadLocalOrPath !Name !(Maybe (CRef r))
  | CReadExpr !(CExpr r)

data CInterpPart r = CLit !Text | CInterp !(CRef r)

-- | 'Stmt'\/'Block', mirrored the identical way 'CExpr' mirrors 'Expr'.
data CStmt r
  = CSExpr !(CExpr r)
  | CSAs !(CExpr r) !(CBlock r)
  | CSLet !Name !(Maybe [Name]) !(CBlock r)
  | CSIn !(CExpr r) !(CBlock r)
  | CSFor !Name !(CExpr r) !(CBlock r)

type CBlock r = [CStmt r]

-- ---------------------------------------------------------------------------
-- Compiling: Expr/Block -> CExpr/CBlock, resolving every reference exactly
-- once, against a purely static (name -> arity) shape of the local scope
-- plus the real, already-built 'Library'
-- ---------------------------------------------------------------------------

-- | Compiles one expression against @env@ (this point's own local-name
--   shape -- just enough to know which names are locals and, where
--   knowable, at what arity, never their values) and @lib@ (the real,
--   fixed 'Library'). Mirrors 'checkExpr'\'s old identifier-resolution
--   surface exactly (same @without@\/@only@\/@exclude@\/@latest@\/
--   'coreFilters' carve-outs, same 'ERead' string-literal-or-unbound-
--   bare-token exemption) -- the difference from that earlier,
--   discarded-result check is that this one *keeps* what it resolves: a
--   hit against @env@ becomes 'CLocalRef' (looked up again, cheaply, in a
--   locals-only 'Env', once per call, because only its *value* is
--   per-call); a hit against @lib@ becomes 'CGlobal' holding the real
--   'Binding' directly, looked up here and never again.
--
--   __A parameter's own arity is genuinely not knowable here, unlike
--   everything else__: a @let@\/@for@-loop variable's arity is fixed by
--   the body's own text (a plain @let@ or loop variable is always
--   0-arity; a curried @let@ declares its own parameter count), but a
--   *parameter*'s real arity is whatever 'Binding' its caller happens to
--   pass in -- e.g. a host function (@'fn1' f@) supplied for one call and
--   something else entirely for another. @env@ therefore maps a name to
--   @'Just' arity@ when it's known (library entries, @let@s, loop
--   variables) and 'Nothing' for a parameter, whose arity check can only
--   ever happen where it always correctly did: at the actual call site,
--   against the real 'Binding' 'CApp'\'s own executor fetches from
--   'Env' at runtime (see 'runCompiledExpr'\'s @CApp@ case).
compileExpr :: forall r. Library r -> Map Name (Maybe Int) -> Expr -> Either String (CExpr r)
compileExpr lib = ce
  where
    resolveRef :: Map Name (Maybe Int) -> Name -> Maybe (Maybe Int, CRef r)
    resolveRef env name = case Map.lookup name env of
      Just marity -> Just (marity, CLocalRef name)
      Nothing     -> case lookup name (libraryEntries lib) of
        Just b@(Binding arity _) -> Just (Just arity, CGlobal b)
        Nothing                  -> Nothing

    -- | Checks arity only when it's actually knowable ('Just') -- a
    -- parameter ('Nothing') always resolves, unconditionally, deferring
    -- its own arity check to the real call site.
    requireRef :: Map Name (Maybe Int) -> Int -> Name -> Either String (CRef r)
    requireRef env argc name = case resolveRef env name of
      Nothing -> Left ("unknown identifier: " <> T.unpack name)
      Just (Nothing, ref) -> Right ref
      Just (Just arity, ref)
        | arity /= argc -> Left (T.unpack name <> ": expected " <> show arity
                                    <> " argument(s), got " <> show argc)
        | otherwise     -> Right ref

    ceInterp :: Map Name (Maybe Int) -> InterpPart -> Either String (CInterpPart r)
    ceInterp _   (Lit t)    = Right (CLit t)
    ceInterp env (Interp n) = CInterp <$> requireRef env 0 n

    ce :: Map Name (Maybe Int) -> Expr -> Either String (CExpr r)
    ce env = \case
      EString q parts  -> CString q <$> mapM (ceInterp env) parts
      EAssistant inner -> CAssistant <$> ce env inner
      EUser inner      -> CUser <$> ce env inner
      EIdent name      -> CIdent <$> requireRef env 0 name
      ERead (EString Quoted parts) -> CRead . CReadPath <$> mapM (ceInterp env) parts
      ERead (EString Bare   parts) -> CRead . CReadPath <$> mapM (ceInterp env) parts
      ERead (EIdent name) -> case resolveRef env name of
        Nothing                    -> Right (CRead (CReadLocalOrPath name Nothing))
        Just (Nothing, ref)        -> Right (CRead (CReadLocalOrPath name (Just ref)))
        Just (Just 0, ref)         -> Right (CRead (CReadLocalOrPath name (Just ref)))
        Just (Just arity, _)       -> Left (T.unpack name <> " needs " <> show arity <> " argument(s), used with none")
      ERead argExpr -> CRead . CReadExpr <$> ce env argExpr
      EApp (EIdent name) argEs -> CApp <$> requireRef env (length argEs) name <*> mapM (ce env) argEs
      EApp _ _ -> Left "application head must be a plain identifier"
      EFilter inner "without" argEs -> CFilterWithout <$> ce env inner <*> mapM (ce env) argEs
      EFilter inner "only"    argEs -> CFilterOnly    <$> ce env inner <*> mapM (ce env) argEs
      EFilter inner "exclude" argEs -> CFilterExclude <$> ce env inner <*> mapM (ce env) argEs
      EFilter inner "latest"  argEs -> CFilterLatest  <$> ce env inner <*> mapM (ce env) argEs
      EFilter inner n argEs
        | Map.member n coreFiltersArity -> CFilterCore n <$> ce env inner <*> mapM (ce env) argEs
        | otherwise -> CFilterUser <$> requireRef env (1 + length argEs) n <*> ce env inner <*> mapM (ce env) argEs

-- | Compiles a whole 'Block', threading the local-name shape through
--   exactly the way 'checkBlock' used to -- 'SLet'\/'SFor' extend @env@
--   for their own body and (for @let@) everything after them, an @as@'s
--   own nested body sees the same @env@ its enclosing statement did.
--   @let@\/@for@-introduced names always carry a known arity ('Just') --
--   only 'compileBody''s own top-level parameter seeding uses 'Nothing'
--   (see 'compileExpr''s own Haddock on why).
compileBlock :: forall r. Library r -> Map Name (Maybe Int) -> Block -> Either String (CBlock r)
compileBlock lib = cb
  where
    ce = compileExpr lib

    cb :: Map Name (Maybe Int) -> Block -> Either String (CBlock r)
    cb env = \case
      [] -> Right []
      Located _ (SExpr e) : rest -> (:) . CSExpr <$> ce env e <*> cb env rest
      Located _ (SAs nameE body) : rest ->
        (\n b r -> CSAs n b : r) <$> ce env nameE <*> cb env body <*> cb env rest
      Located _ (SLet name mParams body) : rest ->
        let arity = maybe 0 length mParams
            env'  = maybe env (\ps -> foldr (\p e -> Map.insert p (Just 0) e) env ps) mParams
        in (\b r -> CSLet name mParams b : r) <$> cb env' body <*> cb (Map.insert name (Just arity) env) rest
      Located _ (SIn e body) : rest ->
        (\ce' b r -> CSIn ce' b : r) <$> ce env e <*> cb env body <*> cb env rest
      Located _ (SFor var srcExpr body) : rest ->
        (\se b r -> CSFor var se b : r) <$> ce env srcExpr <*> cb (Map.insert var (Just 0) env) body <*> cb env rest

-- | Just the name set 'coreFilters' resolves without ever consulting
--   'Library' -- what 'compileExpr' needs to tell "this filter name never
--   reaches a 'CRef'" apart from "this is a project\/default name,"
--   without needing @r@ fixed the way calling 'coreFilters' itself would.
coreFiltersArity :: Map Name ()
coreFiltersArity = Map.fromList
  [ (n, ()) | n <- ["orifempty", "pinned", "priority", "summarizable", "filewithname"
                    , "charname", "truncate", "join", "sortBy", "name", "abstract"
                    , "summarize", "draftDefinition", "extractProperNouns", "whereType", "whereTag"] ]

-- ---------------------------------------------------------------------------
-- Running the compiled IR
-- ---------------------------------------------------------------------------

-- | Resolves a 'CRef' against @env@ -- the *only* place either constructor
--   is ever inspected. 'CGlobal' is already the real 'Binding'; no lookup
--   at all, of any kind, happens for it here or anywhere else. 'CLocalRef'
--   still needs @env@ (a local's *value* is inescapably per-call -- see
--   'CRef's own Haddock), but @env@ from this point on only ever holds
--   this body's own locals, never 'Library''s dozens of unrelated entries.
resolveRef :: Member Fail r => Env r -> CRef r -> Action r (Binding r)
resolveRef _   (CGlobal b)    = pure b
resolveRef env (CLocalRef n)  = case Map.lookup n env of
  Just b  -> pure b
  Nothing -> fail $ "unknown local: " <> T.unpack n -- unreachable if compileExpr ran

-- | Compiles and immediately runs a whole 'Definition' against a supplied
--   initial scope and argument list -- the entry point a host embeds
--   (matching the spec's own framing: "a compiled context ends up with
--   exactly the type shape a hand-written agent already has"). Shares its
--   real compiling with 'definitionBinding' (via 'compileBody', see that
--   function's own Haddock for what "compiling" now means) rather than
--   routing through the 'Binding' it produces -- @args@ here is
--   @['Binding' r]@ (an arbitrary-arity function per parameter is legal,
--   even though every real caller only ever passes 0-arity leaves), which
--   'Binding''s own @fn :: [Action r (Value r)] -> ...@ argument shape
--   can't accept directly; 'compileBody''s own @env@ takes 'Binding's
--   as-is instead, the same way 'Env' always has.
compileDefinition
  :: Member Fail r
  => Library r        -- ^ compile-time table, fixed for this definition's whole body
  -> Definition
  -> Value r          -- ^ initial ambient Reader scope
  -> [Binding r]      -- ^ arguments, matched against 'defParams'
  -> Action r (Value r)
compileDefinition lib def scope args
  | length args /= length (defParams def) = fail $
      "arity mismatch: " <> show (length (defParams def)) <> " parameter(s), "
        <> show (length args) <> " argument(s) given"
  | otherwise = case compileBody lib def of
      Left err       -> fail err
      Right compiled -> mkValue <$> runCompiledBlock (Map.fromList (zip (defParams def) args)) scope compiled

mkValue :: ([Message], [(Name, Action r (Value r))]) -> Value r
mkValue (msgs, entries) = Value (pure msgs) entries defaultMeta

-- | The shared compiling step 'definitionBinding' and 'compileDefinition'
--   both use -- turns @def@'s whole body into a 'CBlock', once, resolving
--   every 'EIdent'\/'EApp'\/'EFilter' reference against @lib@ (see
--   'compileBlock'\/'compileExpr'). __This is the one and only place
--   @def@'s body is ever walked or any name in it resolved against
--   @lib@__: a library reference compiles to a real 'Binding', baked
--   directly into the returned tree; a local (parameter, @let@, @for@-loop
--   variable) compiles to a 'CLocalRef', still looked up by name, but only
--   ever against a locals-only 'Env' built fresh per call -- never
--   'Library' again, at any point after this function returns.
compileBody :: Library r -> Definition -> Either String (CBlock r)
compileBody lib def = compileBlock lib (Map.fromList (map (, Nothing) (defParams def))) (defBody def)

-- | Compiles a parsed 'Definition' into a 'Binding' that runs against
--   whatever scope its *caller* hands it (via 'runCompiledBlock' directly,
--   not 'runDefinition''s own fresh 'currentScope') -- what 'buildLibrary'
--   uses to turn each pure-DSL entry (branch-committed or compiled-in)
--   into the same 'Binding' shape a host-backed library entry already is,
--   so both sit in the same table uniformly.
--
--   @lib@ is "the table as it stood immediately before this definition's
--   own slot" in 'buildLibrary''s fixed left-to-right build order. The
--   returned 'Binding''s closure holds only 'compileBody''s
--   already-compiled 'CBlock' -- @lib@ itself, and @def@'s original,
--   unresolved 'Expr' tree, are *not* captured; there is nothing left in
--   the closure a later call could re-consult 'Library' through, because
--   there is no 'Library' reference sitting in the closure at all any
--   more.
--
--   This is possible in one pass, exhaustively, because the language has
--   no @if@ and no recursion (see \"Verification\" in @CONTEXT-DSL.md@):
--   a body's set of references is exactly its text, never data-dependent.
--   A name with no earlier binding at all, or a reference at the wrong
--   arity, is therefore a fact about @(lib, def)@ alone -- decidable
--   immediately, not something that has to wait for a runtime 'Action' to
--   stumble into it.
--
--   Every real @defParams@ name is used purely as a plain value reference
--   inside a body (@EIdent paramName@, never applied with further
--   arguments -- the language has no way to write a higher-order
--   parameter), but 'Binding''s own @fn@ argument shape
--   (@[Action r (Value r)] -> ...@) is what a caller applying this
--   'Binding' actually supplies, so each is wrapped via 'bval' into a
--   0-arity 'Binding' before insertion into 'Env' here.
definitionBinding :: Member Fail r => Library r -> Definition -> Either String (Binding r)
definitionBinding lib def = do
  compiled <- compileBody lib def
  let run args scope = mkValue <$> runCompiledBlock (bindParams (defParams def) args Map.empty) scope compiled
  Right (Binding (length (defParams def)) run)

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

-- | Runs a whole compiled 'CBlock', folding every statement's contribution
--   into one @([Message], entries)@ pair -- this *is* "a fresh writer
--   target" (rules 4\/5): whoever calls this (a function body, an @as@
--   body, 'definitionBinding''s own closure) wraps the result into a new
--   'Value'. @in@\/@for@ do *not* call this to get an independent 'Value'
--   of their own -- per rule 6, "the writer target is untouched" -- they
--   fold their own nested statements into the *same* accumulator this
--   call already threads (see their cases below).
--
--   @env@ here is deliberately *not* seeded with 'Library' the way the
--   pre-IR design's ever-rebuilt @env@ was -- every 'CGlobal' reference
--   already carries its own resolved 'Binding' directly in the compiled
--   tree (see 'CRef''s own Haddock), so there is nothing left for @env@ to
--   hold except this body's own locals: parameters, @let@s, @for@-loop
--   variables. A fresh @env@ is still built per call (a local's *value*
--   is inescapably per-call), but it only ever grows to the size of the
--   handful of locals actually in scope, never the whole 'Library'.
runCompiledBlock
  :: Member Fail r
  => Env r -> Value r -> CBlock r
  -> Action r ([Message], [(Name, Action r (Value r))])
runCompiledBlock env0 scope0 = go env0 scope0 [] []
  where
    go _ _ msgs entries [] = pure (concat (reverse msgs), entries)
    go env scope msgs entries (CSExpr e : rest) = do
      v <- runCompiledExpr env scope e
      m <- valueDefault v
      go env scope (m : msgs) entries rest
    go env scope msgs entries (CSAs nameE body : rest) = do
      name <- nameOfCompiled env scope nameE
      when (any ((== name) . fst) entries) $
        fail $ "duplicate 'as' name " <> show name
      let entryAction = mkValue <$> runCompiledBlock env scope body
      go env scope msgs (entries ++ [(name, entryAction)]) rest
    go env scope msgs entries (CSLet name mParams body : rest) =
      let binding = case mParams of
            Nothing -> bval (mkValue <$> runCompiledBlock env scope body)
            Just ps -> Binding (length ps) $ \args callerScope ->
              mkValue <$> runCompiledBlock (bindParams ps args env) callerScope body
      in go (Map.insert name binding env) scope msgs entries rest
    go env scope msgs entries (CSIn e body : rest) = do
      newScope <- runCompiledExpr env scope e
      (m, es) <- runCompiledBlock env newScope body
      go env scope (m : msgs) (unionEntries es entries) rest
    go env scope msgs entries (CSFor var srcExpr body : rest) = do
      srcVal <- runCompiledExpr env scope srcExpr
      let matches = map fst (valueEntries srcVal)
      (m, es) <- foldM (runOneIteration var body) ([], entries) matches
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
        runOneIteration var' body' (msgsAcc, entriesAcc) matchedPath = do
          let loopVar = Value (pure [User matchedPath]) [(matchedPath, forceAt scope matchedPath)] defaultMeta
              env'    = Map.insert var' (bval (pure loopVar)) env
          (m1, es1) <- runCompiledBlock env' scope body'
          case filter ((`elem` map fst entriesAcc) . fst) es1 of
            ((dup, _) : _) -> fail $ "duplicate 'as' name " <> show dup <> " across for-loop iterations"
            []             -> pure ()
          pure (msgsAcc ++ m1, unionEntries es1 entriesAcc)

bindParams :: [Name] -> [Action r (Value r)] -> Env r -> Env r
bindParams ps args env = List.foldl' (\e (p, a) -> Map.insert p (bval a) e) env (zip ps args)

nameOfCompiled :: Member Fail r => Env r -> Value r -> CExpr r -> Action r Name
nameOfCompiled env scope e = do
  v <- runCompiledExpr env scope e
  messagesText <$> valueDefault v

-- | Runs one compiled expression. Every reference site is already a
--   'CRef' -- 'resolveRef' is the only place any lookup happens at all,
--   and for a 'CGlobal' it isn't a lookup, just reading a field. This is
--   the direct executor for what 'compileExpr' produced; see that
--   function's own Haddock for what each 'CExpr' constructor means and
--   why its reference sites are already resolved.
runCompiledExpr :: Member Fail r => Env r -> Value r -> CExpr r -> Action r (Value r)
runCompiledExpr env scope e = case e of
  CString Quoted parts -> leafValue . (: []) . User <$> runCompiledInterp env scope parts
  CString Bare   parts -> runCompiledInterp env scope parts >>= globResolve scope
  CAssistant inner -> do
    v    <- runCompiledExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (Assistant . messageText) msgs) }
  CUser inner -> do
    v    <- runCompiledExpr env scope inner
    msgs <- valueDefault v
    pure v { valueDefault = pure (map (User . messageText) msgs) }
  CIdent ref -> resolveRef env ref >>= \case
    Binding 0 fn -> fn [] scope
    Binding arity _ -> fail $ "needs " <> show arity <> " argument(s), used with none"
  -- @read@'s argument was compiled into one of three shapes by
  -- 'compileExpr' (see its own Haddock and 'CReadArg's): a literal
  -- path\/glob, resolved via the identical glob machinery a bare
  -- expression-position glob already uses (so @read *.md@ genuinely can
  -- match more than one file); a bare identifier that resolved to nothing
  -- at compile time, still meaning a literal path (rule 3's one exception
  -- -- see @CONTEXT-DSL.md@'s Grammar section); or a bare identifier that
  -- did resolve, read through its own 'CRef' normally. Whichever shape,
  -- if the resulting 'Value' has entries, @read@ forces each one in order
  -- and folds their own content into the result's own default, keeping
  -- 'valueEntries' itself intact; a 'Value' with none (an already-resolved
  -- single leaf) is returned unchanged.
  CRead readArg -> do
    v <- case readArg of
      CReadPath parts -> runCompiledInterp env scope parts >>= globResolve scope
      CReadLocalOrPath name Nothing    -> globResolve scope name
      CReadLocalOrPath _    (Just ref) -> resolveRef env ref >>= \case
        Binding 0 fn    -> fn [] scope
        Binding arity _ -> fail $ "needs " <> show arity <> " argument(s), used with none"
      CReadExpr inner -> runCompiledExpr env scope inner
    if null (valueEntries v)
      then pure v
      else do
        forced   <- mapM snd (valueEntries v)
        combined <- concat <$> mapM valueDefault forced
        pure v { valueDefault = pure combined }
  CApp ref argEs -> do
    let args = map (runCompiledExpr env scope) argEs
    Binding arity fn <- resolveRef env ref
    when (length args /= arity) $ fail $
      "expected " <> show arity <> " argument(s), got " <> show (length args)
    fn args scope
  -- @without@\/@only@\/@exclude@\/@latest@ all decide *which keys
  -- survive* -- genuinely shrinking 'valueEntries', "like [a dropped key]
  -- was never there" (not just neutering a kept key's own content to
  -- 'emptyValue', the old behaviour) -- so, like every other filter that
  -- needs a resolved argument's own content, this is what makes
  -- @exclude(lore)@ -- passing in another already-computed definition's
  -- own result, not just a literal pattern -- actually work: 'lore's own
  -- key *names* are known purely (no forcing needed at all), and once
  -- matched keys are genuinely gone, a subsequent @for@\/glob over the
  -- result can't resurrect them the way it used to.
  CFilterWithout inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
    shrinkEntries (==) False v args
  CFilterOnly inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
    shrinkEntries (==) True v args
  CFilterExclude inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
    shrinkEntries globMatches False v args
  CFilterLatest inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
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
  -- @expr | filterName(args...)@ compiled to 'CFilterCore' for the pure,
  -- zero-capability registry ('coreFilters') -- @EIdent@\/@EApp@ already
  -- resolve a bare name the identical way this filter-position name did,
  -- at compile time, once; this is that same fallthrough, just called
  -- with the piped value prepended to whatever explicit arguments
  -- followed the pipe.
  CFilterCore name inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
    case Map.lookup name coreFilters of
      Just impl -> impl v args
      Nothing   -> fail ("unknown core filter: " <> T.unpack name) -- unreachable if compileExpr ran
  CFilterUser ref inner argEs -> do
    v    <- runCompiledExpr env scope inner
    args <- mapM (runCompiledExpr env scope) argEs
    resolveRef env ref >>= \case
      Binding arity fn
        | arity /= 1 + length args -> fail $
            "expected " <> show (arity - 1) <> " argument(s) after the pipe, got " <> show (length args)
        | otherwise -> fn (pure v : map pure args) scope

-- | What a single @without@\/@only@\/@exclude@ argument contributes to the
--   match set: another already-computed 'Value' with its own entries
--   (e.g. @lore@, passed in as @exclude(lore)@) contributes its own key
--   *names* directly -- always known purely, no forcing needed -- a plain
--   leaf (an ordinary string-literal pattern\/name) contributes its own
--   forced default text instead.
argCriteria :: Value r -> Action r [Text]
argCriteria a
  | null (valueEntries a) = (: []) . messagesText <$> valueDefault a
  | otherwise             = pure (map fst (valueEntries a))

globMatches :: Text -> Text -> Bool
globMatches k pat = Glob.match (Glob.compile (T.unpack pat)) (T.unpack k)

-- | Shrinks @v@'s own 'valueEntries' by whether each key satisfies
--   @matches@ against any criterion contributed by @args@ (see
--   'argCriteria'). @keep = True@ retains a key some criterion matches
--   (@only@); @keep = False@ drops it (@without@\/@exclude@).
shrinkEntries :: (Text -> Text -> Bool) -> Bool -> Value r -> [Value r] -> Action r (Value r)
shrinkEntries matches keep v args = do
  criteria <- concat <$> mapM argCriteria args
  let isMatched k = any (matches k) criteria
  pure v { valueEntries = filter (\(k, _) -> isMatched k == keep) (valueEntries v) }

-- | Resolves every @%name%@ span (already a 'CRef', see 'compileExpr'\'s
--   own @ceInterp@) against 'env', leaving literal spans untouched.
runCompiledInterp :: Member Fail r => Env r -> Value r -> [CInterpPart r] -> Action r Text
runCompiledInterp env scope = fmap T.concat . mapM part
  where
    part (CLit t)    = pure t
    part (CInterp ref) = messagesText <$> (valueDefault =<< runIdentRef ref)
    runIdentRef ref = resolveRef env ref >>= \case
      Binding 0 fn    -> fn [] scope
      Binding arity _ -> fail $ "needs " <> show arity <> " argument(s), used with none"

-- | @read@'s own path resolution: a flat scope (a branch tree or a glob
--   result -- see 'treeValueOfCommit') stores full paths as its own
--   entries' keys, so the whole literal text is tried as one key first.
--   Falls back to 'lookupPath'\'s segment-by-segment walk for a scope
--   that's genuinely nested (an @as@-export map reached via a partial
--   path, say) -- cheap to keep as a fallback and matches rule 3's own
--   "looks it up by key, recursively" wording, even though nothing this
--   interpreter builds today actually produces multi-level nesting.
resolveRead :: Value r -> Text -> Action r (Maybe (Value r))
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
globMatchPat :: Value r -> Text -> Action r [Text]
globMatchPat scope pat = do
  allPaths <- listPaths scope
  let compiled = Glob.compile (T.unpack pat)
  pure (filter (Glob.match compiled . T.unpack) allPaths)

forceAt :: Value r -> Text -> Action r (Value r)
forceAt scope path = maybe emptyValue id <$> resolveRead scope path

-- | Matches @pat@ against @scope@ (a glob if it has metacharacters, an
--   exact match otherwise -- 'Glob.compile' treats a plain string
--   literally) and builds a container 'Value' keyed by each match, its
--   own entry the real, lazy resolution at that path -- exactly what a
--   bare glob expression already builds (see 'EString'\'s own
--   @Bare@ case), reused here by 'ERead' for both a literal string
--   argument and an otherwise-unresolved bare identifier.
--
--   @valueDefault@ is the single matched path's own text when @pat@
--   resolves to exactly one file, empty otherwise -- the identical shape
--   a @for@-loop's own per-iteration binding already has (see 'SFor's own
--   @runOneIteration@: @loopVar = Value (pure [User matchedPath]) [...]
--   ...@), so a function taking a "this one file" argument (e.g.
--   'Storyteller.Context.DSL.Library.loreEntry', whose own @"## %f%"@
--   heading interpolates its parameter's default) works identically
--   whether called from inside a @for@ or directly against a single-match
--   glob -- @loreEntry lore/notes.md@, not just @loreEntry f@ inside
--   @for f in lore/**/*:@. Zero or multiple matches keep the empty
--   default: there's no single "the path" to name in either case, exactly
--   the existing ambiguity a multi-file glob (@read *.md@) already has.
globResolve :: Value r -> Text -> Action r (Value r)
globResolve scope pat = do
  matches <- globMatchPat scope pat
  entries <- mapM (\m -> (,) m . pure <$> forceAt scope m) matches
  let dflt = case matches of
        [singleMatch] -> pure [User singleMatch]
        _             -> pure []
  pure (Value dflt entries defaultMeta)

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
type DSLFilter r = Value r -> [Value r] -> Action r (Value r)

type FilterRegistry r = Map Name (DSLFilter r)

-- | A 'Value' that fails when forced, never at construction -- how a
--   filter reports a problem (an arity mismatch, an unimplemented
--   filter) without itself needing to run anything.
errorValue :: Member Fail r => String -> Value r
errorValue msg = Value { valueDefault = fail msg, valueEntries = [], valueMeta = defaultMeta }

-- | @summarize@\/@draftDefinition@\/@extractProperNouns@\/@whereType@\/
--   @whereTag@ are still left as loud failures -- not because a filter
--   can't reach storage any more (it can), but because real semantics
--   for these need a tagging convention this pass doesn't decide, or (for
--   @summarize@) an LLM effect still genuinely outside 'Action's own
--   ceiling. Pretending otherwise would be worse than not having them.
coreFilters :: Member Fail r => FilterRegistry r
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

fNotImplemented :: Member Fail r => String -> DSLFilter r
fNotImplemented label _ _ = pure $ errorValue $
  "filter `" <> label <> "` is not yet implemented (needs a real LLM/content-analysis effect)"

fPassthrough :: DSLFilter r
fPassthrough v _ = pure v

-- | Sets 'Pinned' in 'metaFlags' -- what a budget-aware renderer (outside
--   this module) treats as "never drop this."
fPinned :: Member Fail r => DSLFilter r
fPinned v [] = pure v { valueMeta = addFlag Pinned (valueMeta v) }
fPinned _ args = pure $ errorValue $ "pinned: expected no arguments, got " <> show (length args)

-- | Sets 'Summarizable' in 'metaFlags' -- what a budget-aware renderer may
--   replace with an LLM summary of the same text under pressure, rather
--   than dropping outright.
fSummarizable :: Member Fail r => DSLFilter r
fSummarizable v [] = pure v { valueMeta = addFlag Summarizable (valueMeta v) }
fSummarizable _ args = pure $ errorValue $ "summarizable: expected no arguments, got " <> show (length args)

-- | Sets 'metaPriority' -- higher survives longer under budget pressure
--   (see 'Priority's own haddock).
fPriority :: Member Fail r => DSLFilter r
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
fOrIfEmpty :: Member Fail r => DSLFilter r
fOrIfEmpty v [fallback] = pure v
  { valueDefault = do
      msgs <- valueDefault v
      if null msgs then valueDefault fallback else pure msgs
  }
fOrIfEmpty _ args = pure $ errorValue $ "orifempty: expected exactly 1 argument, got " <> show (length args)

fFileWithName :: Member Fail r => DSLFilter r
fFileWithName v [] = pure $ leafValueA $ do
  msgs <- valueDefault v
  pure [User (T.pack (FP.takeBaseName (T.unpack (messagesText msgs))))]
fFileWithName _ args = pure $ errorValue $ "filewithname: expected no arguments, got " <> show (length args)

fTruncate :: Member Fail r => DSLFilter r
fTruncate v [nArg] = pure $ leafValueA $ do
  nMsgs <- valueDefault nArg
  let n = maybe (T.length (messagesText nMsgs)) id (readMaybeInt (messagesText nMsgs))
  msgs <- valueDefault v
  pure [User (T.take n (messagesText msgs))]
fTruncate _ args = pure $ errorValue $ "truncate: expected exactly 1 argument, got " <> show (length args)

fJoin :: Member Fail r => DSLFilter r
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
fSortBy :: Member Fail r => DSLFilter r
fSortBy v [] = pure v { valueEntries = List.sortBy (\a b -> compare (naturalKey (fst a)) (naturalKey (fst b))) (valueEntries v) }
fSortBy _ args = pure $ errorValue $ "sortBy: expected no arguments, got " <> show (length args)

-- | Extracts a Markdown document's @# Title@ -- the one structural
--   convention @sheet.md@ is actually required to follow (see
--   @WRITER.md@: the first H1 is a character's display name). Unlike
--   @summarize@, this is convention over already-stored text, not
--   content analysis -- no LLM effect needed, so it's a plain filter
--   rather than something deferred to a render-time pass.
fName :: Member Fail r => DSLFilter r
fName v [] = pure $ leafValueA $ do
  msgs <- valueDefault v
  pure [User (mdHeading (messagesText msgs))]
fName _ args = pure $ errorValue $ "name: expected no arguments, got " <> show (length args)

-- | Extracts the paragraph immediately following a Markdown document's
--   leading @# Title@ -- the "acquaintance blurb" convention: whatever
--   prose a sheet opens with, up to the next blank line or heading.
--   Same non-LLM rationale as 'fName'.
fAbstract :: Member Fail r => DSLFilter r
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

leafValueA :: Action r [Message] -> Value r
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
--   name, enters that branch, and builds a scope from its filesystem
--   exactly like the initial scope was built.
--
--   The scope is forced before the branch is left, and it is not possible
--   to forget. A 'Value' is thunks in a row, so an unforced one built here
--   has the character's own filesystem effects /in its type/
--   (@Value (FileSystemRead ContextFS : FileSystem ContextFS : BranchOp
--   Visited : r)@) and simply cannot be returned from the interpreter that
--   discharges them. 'forceValue'\/'forcedValue' is how the value is made
--   to escape at all, not a discipline guarding against a silent bug --
--   dropping it is a type error, not a wrong answer. (Checked by removing
--   it: GHC rejects the result, it does not miscompile.)
--
--   What that costs is reading the character's own files eagerly, which
--   every real use of this (@context.character@'s @sheet@\/@full@\/
--   @journalFull@ buckets) goes on to read anyway. What it is /not/ doing
--   is protecting correctness by convention; the row does that.
branchBinding :: forall r. Members '[Branches, Fail] r => Binding r
branchBinding = fn1 go
  where
    go vArg = do
      ident <- messagesText <$> (valueDefault =<< vArg)
      let name = BranchName ("character/" <> ident)
      forcedValue <$> crossInto name

    crossInto name =
      Action . withBranch @Visited name . runStoryFSRead @ContextFS @Visited ContextFS . runAction $
        forceValue =<< scopeOfFileSystem @ContextFS

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
charactersInBinding :: forall branch r. Members '[BranchOp branch, Fail] r => Binding r
charactersInBinding = fn1 go
  where
    go vArg = do
      path  <- T.unpack . messagesText <$> (valueDefault =<< vArg)
      chars <- liftSem (activeCharactersFor @branch path)
      let idents = [ Branches.branchDisplayName name | Character (BranchName name) <- chars ]
      pure (Value (pure []) [ (ident, pure (leafValue [User ident])) | ident <- idents ] defaultMeta)

-- | @summarized@\/@summarizedOnce@'s shared plumbing: force @vPath@\/@vKind@,
--   split @vKind@'s text into the finest-first hierarchy
--   'Storyteller.Writer.Agent.Summarizer.densest' wants (the same way
--   'argCriteria'\/glob patterns already tokenize on whitespace, so a
--   caller wanting a coarser fallback chain just passes more than one
--   word -- @summarized(path, \"prose\/chapter prose\/book\")@), then read.
--
--   Eager, not lazy, for both: the summarized text is read and settled
--   into the resulting 'Value' the moment this runs, exactly like 'ERead'
--   resolves a plain file -- context assembly stays one deterministic
--   pass with a predictable cache boundary, rather than deferring "which
--   version" to render time.
summarizedGo :: forall branch r. Members '[BranchOp branch, Fail] r => ([Text] -> [Text]) -> Action r (Value r) -> Action r (Value r) -> Action r (Value r)
summarizedGo narrow vPath vKind = do
  path  <- T.unpack . messagesText <$> (valueDefault =<< vPath)
  kinds <- narrow . T.words . messagesText <$> (valueDefault =<< vKind)
  text  <- liftSem (densest @branch kinds path)
  pure (leafValue [FileRead path text])

-- | @summarized@'s own implementation, as an ordinary 'Binding' -- same
--   reasoning as 'branchBinding'\/'charactersInBinding': reading a file
--   through its own compressed form needs real capability
--   (a 'BranchOp' read through
--   'Storyteller.Writer.Agent.Summarizer.densest'), not just forcing
--   values already in hand, so it's a library entry rather than a
--   'coreFilters' case. Takes the summarizer kind explicitly as its
--   second argument (@path | summarized(\"prose\/chapter\")@, or bare
--   @summarized path kind@) rather than assuming one fixed kind: which
--   summarizer(s) a project actually runs is call-site knowledge, the
--   same way 'without'\/'exclude''s own match patterns are supplied by
--   the caller rather than baked in here.
--
--   "As deep as this file's compression gets" -- considers every kind in
--   the given hierarchy and settles on the coarsest that actually covers
--   @path@ (see 'Storyteller.Writer.Agent.SummaryAccess.densest'). The
--   one-level-in counterpart is 'summarizedOnceBinding'; kept as two
--   separate named filters, not one filter with a depth argument, since
--   "give me the deepest compression" and "give me exactly the next zoom
--   level" are two different questions a caller asks, not two settings
--   of the same one.
summarizedBinding :: forall branch r. Members '[BranchOp branch, Fail] r => Binding r
summarizedBinding = fn2 (summarizedGo @branch id)

-- | @summarizedOnce@'s own implementation -- 'summarizedBinding''s
--   one-level counterpart. Considers only the *finest* kind in the given
--   hierarchy (@take 1@): the file's own summary at that one tier if it
--   covers @path@, else the raw content -- never falls through to a
--   coarser tier the way 'summarizedBinding' does, even if more kinds are
--   listed. What a caller reaches for to show "the next zoom level up,"
--   as a deliberately distinct step from "how compressed can this get."
summarizedOnceBinding :: forall branch r. Members '[BranchOp branch, Fail] r => Binding r
summarizedOnceBinding = fn2 (summarizedGo @branch (take 1))

-- | 'treeValueOfCommit' for a named branch -- resolves the name via
--   'resolveBranch', then delegates. The one case a Reader-scope switch
--   genuinely does correspond to a different commit (contrast
--   'currentScope', which needs no name or lookup at all).
treeValueOfBranch :: forall r. Members '[Branches, Fail] r => BranchName -> Action r (Value r)
treeValueOfBranch name =
  fmap forcedValue . Action . withBranch @Visited name . runStoryFSRead @ContextFS @Visited ContextFS . runAction $
    forceValue =<< scopeOfFileSystem @ContextFS

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
--   against -- they never reposition 'Core.StoreT' itself. So this enters
--   the character's own branch ('withBranch') and reads the journal from
--   inside it, the same way every other cross-branch read in this codebase
--   works.
--
--   Takes @lookback@\/@maxOut@\/@padding@ baked in from the Haskell side
--   (a project's own tuning, not DSL-expressible policy -- mirrors the
--   invented-calendar example's own @dateMath@), and the character
--   identifier as its one DSL-side argument, e.g. @journal charname@
--   where @journal@ was threaded in as a parameter the same way
--   'fBranch' expects @charname | branch@'s own identifier.
journalDelta :: forall r. Members '[Branches, Fail] r => JournalCuration -> Binding r
journalDelta curation = fn1 go
  where
    go charnameArg = do
      ident <- messagesText <$> (valueDefault =<< charnameArg)
      texts <- liftSem $ withBranch @Visited (BranchName ("character/" <> ident)) $
                 journalWindow @Visited "journal.md" curation
      pure (leafValue (renderJournalTexts texts))

-- | One block per curated slice, joined by a plain divider -- entries
--   may span real timeline gaps (unremarkable ticks in between were
--   dropped), so they shouldn't read as one continuous entry, plus the
--   same framing header 'Storyteller.Writer.Agent.CharContext.
--   renderJournalContext' already uses (so a model doesn't mistake this
--   for objective narration) -- kept here rather than left to the
--   calling definition, since it's fixed framing text tied to what a
--   curated journal slice *is*, not project-overridable policy.
renderJournalTexts :: [Text] -> [Message]
renderJournalTexts []    = []
renderJournalTexts texts =
  [ User $
      "### From this character's own journal (their private viewpoint -- may be biased, outdated, or contradict the wider record)\n\n"
      <> T.intercalate "\n\n---\n\n" texts
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
readConversation :: forall branch r. Members '[BranchOp branch, Fail] r => Binding r
readConversation = fn1 go
  where
    go pathArg = do
      path  <- T.unpack . messagesText <$> (valueDefault =<< pathArg)
      turns <- liftSem (conversationTurns @branch path)
      pure (leafValue (map turnToMessage turns))

-- | 'NoteTurn' folds into 'User' rather than getting its own case: 'Message'
--   is deliberately exactly three constructors (see its own Haddock), and a
--   note is still the author's own words, just addressed to the margin
--   rather than to the model -- tagged so a reader downstream can still
--   tell it apart from an actual turn of dialogue.
turnToMessage :: Turn -> Message
turnToMessage (UserTurn t)      = User t
turnToMessage (AssistantTurn t) = Assistant t
turnToMessage (NoteTurn t)      = User ("[note] " <> t)

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
embedShallow :: Member Fail r => Binding r
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

-- | Host-backed library entries -- real Haskell closures, never
--   expressible as parsed DSL text, so they can never be branch-
--   overridden. Seeds the compile-time table
--   'Storyteller.Core.Context.buildContextLibrary' folds
--   'Storyteller.Context.DSL.Library.defaultLibraryOrder' on top of (so any
--   default\/override slot can reference a host name immediately -- safe,
--   since a host binding never itself references another library name),
--   resolved the identical way by this module's own 'EIdent'\/'EApp' -- a
--   DSL body referencing @readconversation@ can't tell it apart from a
--   bare reference to @lore@.
--
--   Also what a @['dsl'| ... |]@-spliced definition compiles against (see
--   "Storyteller.Context.DSL.QQ"): safe there for the same reason it's
--   safe as a fold seed -- a host binding's own body never references
--   another library name, so handing an isolated snippet nothing but this
--   table still lets it reach @branch@\/@charactersin@\/... while
--   correctly failing to resolve any project- or default-library name it
--   has no compile-order relationship to.
--
--   Lives here, not in "Storyteller.Context.DSL.Library" (its historical
--   home) -- moved so 'Storyteller.Context.DSL.QQ' can import it directly
--   without a module cycle ("Storyteller.Context.DSL.Library" itself
--   imports "Storyteller.Context.DSL.QQ" for 'dsl'\/'defQuote').
--   Re-exported from "Storyteller.Context.DSL.Library" for every existing
--   caller.
hostLibrary :: forall branch r. Members '[Branches, BranchOp branch, Fail] r => Library r
hostLibrary = Library
  [ ("readconversation", readConversation @branch)
  , ("embedshallow",     embedShallow)
  , ("branch",           branchBinding)
  , ("charactersin",     charactersInBinding @branch)
  , ("summarized",       summarizedBinding @branch)
  , ("summarizedOnce",   summarizedOnceBinding @branch)
  -- | The ambient character-context journal curation, pre-configured --
  --   'journalDelta''s own Haskell-level @lookback@\/@maxOut@\/@padding@
  --   tuning is genuine per-caller parametricity (see its own haddock), so
  --   it stays a host 'Binding', never expressible as parsed DSL text --
  --   but the *numbers themselves* are this application's one shared
  --   default (formerly 'Server.Writer.File.activeCharacterContext''s own
  --   constants), not something 'Storyteller.Context.DSL.Library.contextCharacterDef'
  --   should have to take as a parameter just to reference it by name.
  , ("characterJournal", journalDelta (JournalCuration 30 10 2))
  ]

-- | The Reader scope for wherever this 'Action' is actually run -- the
--   'ContextFS' filesystem, whatever a caller wired that to. No
--   'Storyteller.Core.Types.BranchName', no lookup, no position: "run in
--   whatever I'm already in" is the whole of it.
--
--   The one place in the DSL that turns a capability into a scope. From
--   here down a scope is data: 'compileDefinition' takes one and needs
--   nothing but 'Fail', every compiled 'Binding' receives one as its
--   second argument, and no filter or library definition declares a read
--   capability at all.
currentScope :: forall r. Members '[FileSystem ContextFS, FileSystemRead ContextFS, Fail] r => Action r (Value r)
currentScope = scopeOfFileSystem @ContextFS

-- | The whole pipeline as one 'Action': take whatever commit is
--   currently ambient as the initial scope, compile @def@ against it.
--   What a host actually calls -- everything above is the reusable
--   machinery this assembles. Still fully generic: the concrete backend
--   only enters when the returned 'Action' is finally run via
--   'Storyteller.Context.DSL.Value.runAction'.
--
-- | 'compileDefinition' at 'currentScope' -- the shape a
--   @['dsl'| ... |]@-spliced binding gets, and what a Haskell caller that
--   just wants "run this definition where I am" calls.
--
--   Kept, unlike the library entry points below it, because a quasiquoted
--   definition genuinely is invoked at the ambient scope by a caller that
--   has no scope of its own to pass. The capability it costs is honest and
--   confined: it lands on the handful of quoted bindings, not on every
--   definition in "Storyteller.Context.DSL.Library" -- those take their
--   scope as an argument and need nothing but 'Fail'.
runDefinition
  :: forall r
  .  Members '[FileSystem ContextFS, FileSystemRead ContextFS, Fail] r
  => Library r -> Definition -> [Binding r] -> Action r (Value r)
runDefinition lib def args = currentScope >>= \scope -> compileDefinition lib def scope args

-- | Looks @name@ up in @lib@ and runs it against @scope@ -- what a caller
--   wants once @lib@ has already been built (see
--   'Storyteller.Core.Context.buildContextLibrary'): unlike
--   'compileDefinition' there's no separate 'Definition' to compile here,
--   because @name@'s own slot in @lib@ is already a compiled 'Binding' --
--   the default, or an accepted override, whichever 'buildLibrary'
--   actually put there. A missing @name@ is a genuine, loud 'Fail' (not
--   "fall back to some caller-supplied default"): every real caller only
--   ever asks for a name it knows is a 'defaultLibraryOrder' slot, so a
--   miss here means the caller and the library have drifted, not that a
--   project simply hasn't overridden anything yet.
--
--   @scope@ is an argument, and that is the whole reason this function --
--   like every 'Binding', like 'compileDefinition' -- needs nothing but
--   'Fail'. There used to be a @runDefinition@ beside this that called
--   'currentScope' on its caller's behalf, and the cost of that
--   convenience was that roughly fifty signatures across the DSL declared
--   a content-read capability, including definitions that provably never
--   read anything. A scope is data; only 'currentScope' turns a capability
--   into one.
runNamed :: forall r. Member Fail r => Value r -> Library r -> Name -> [Action r (Value r)] -> Action r (Value r)
runNamed scope lib name args = case lookup name (libraryEntries lib) of
  Nothing -> fail ("runNamed: unknown library entry " <> T.unpack name)
  Just (Binding arity fn)
    | arity /= length args -> fail (T.unpack name <> ": expected " <> show arity
                                       <> " argument(s), got " <> show (length args))
    | otherwise             -> fn args scope
