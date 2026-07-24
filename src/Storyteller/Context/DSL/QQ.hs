{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Two quasiquoters for the Context DSL, both parsing their contents *at
--   GHC compile time* (a malformed definition is a compile error, at the
--   quote's own source location, not something that surfaces later at
--   runtime) and splicing a curried Haskell function of exactly the
--   source's own arity -- one 'Action' 'Value' parameter per declared
--   parameter (rule 1's curried parameter list), returning the compiled
--   'Action' 'Value' once fully applied. They differ only in what
--   compile-time library table the splice runs against, i.e. what other
--   named definitions the body can reference by 'EIdent'\/'EApp' (see
--   'Storyteller.Context.DSL.Compile.definitionBinding''s own Haddock for
--   why that table has to be fixed at compile time, not looked up live):
--
--   * @['dsl'| ... |]@ -- the common case, an empty table: a
--     self-contained leaf that references no other library name at all
--     (not even a 'Storyteller.Context.DSL.Compile.hostLibrary' one like
--     @branch@\/@charactersin@). Its own type signature only ever needs
--     whatever effects its own body's primitives (@read@, filters, ...)
--     require -- never widened by what some table it compiles against
--     might have needed, since there is no table.
--   * @['dslWith'| ... |]@ -- splices a function that takes the
--     compile-time table as its *leading ordinary argument*, ahead of the
--     source's own declared parameters: @tbl a1 ... an -> ...@. A call
--     site supplies a 'Storyteller.Context.DSL.Compile.Library'
--     (typically 'Storyteller.Context.DSL.Compile.hostLibrary' \@branch,
--     for a snippet that needs @branch@\/@charactersin@\/... but still
--     isn't itself part of 'Storyteller.Core.Context.buildContextLibrary's
--     fixed compile order) as an ordinary applied argument -- whatever
--     effects that table's own entries need become part of the *call
--     site's* required 'Members', not baked into the spliced definition's
--     own type the way they would be if 'dsl' always compiled against
--     'Storyteller.Context.DSL.Compile.hostLibrary'.
--
--   > injuryStatus :: Action Value
--   > injuryStatus = [dsl|
--   >   as "injury": read status/injury.md
--   > |]
--   >
--   > castingStatus :: Library r -> Action r Value -> Action r Value
--   > castingStatus = [dslWith|
--   >   charname:
--   >     in (charname | branch): read "status/casting_log.md" | orifempty "no casting today"
--   > |]
--   > -- called as: castingStatus (hostLibrary \@branch) charnameArg
--
--   The generated function is exactly
--   'Storyteller.Context.DSL.Compile.runDefinition' applied to the
--   compile-time table, the parsed 'Storyteller.Context.DSL.AST.Definition',
--   and however many arguments the lambda collects -- GHC checks the arity
--   at every call site, instead of
--   'Storyteller.Context.DSL.Compile.compileDefinition' only discovering a
--   mismatch at runtime. The scope is always whatever commit is ambient
--   when the returned 'Action' finally runs (see
--   'Storyteller.Context.DSL.Compile.currentScope') -- there's no way to
--   splice in a different scope from here, by design. Anything that needs
--   one (composing a sub-'Definition' against an explicitly chosen
--   'Storyteller.Context.DSL.Value.Value', say) calls
--   'Storyteller.Context.DSL.Parser.parseDefinition' and
--   'Storyteller.Context.DSL.Compile.compileDefinition' directly, same as
--   before quasiquotes existed.
module Storyteller.Context.DSL.QQ (dsl, dslWith, defQuote) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.TH (Exp(..), Pat(VarP), Q, Type(VarT), mkName, newName)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(lift), Loc(..), location)

import Storyteller.Context.DSL.AST (Definition(..))
import Storyteller.Context.DSL.Compile (runDefinition)
import Storyteller.Context.DSL.Context (toBinding)
import Storyteller.Context.DSL.Parser (parseDefinition, renderParseErr)

-- | Only 'quoteExp' is meaningful for a DSL that produces a 'Value'-typed
--   expression, not a pattern\/type\/declaration -- the other three
--   report a clear error instead of quietly doing nothing useful.
dsl :: QuasiQuoter
dsl = QuasiQuoter
  { quoteExp  = compileDsl NoTable
  , quotePat  = const (fail "[dsl| ... |] can only be used as an expression")
  , quoteType = const (fail "[dsl| ... |] can only be used as an expression")
  , quoteDec  = const (fail "[dsl| ... |] can only be used as an expression")
  }

-- | 'dsl', but the spliced function takes the compile-time table as its
--   own leading argument instead of always compiling against an empty one
--   -- see the module Haddock for when to reach for this instead of plain
--   'dsl'.
dslWith :: QuasiQuoter
dslWith = QuasiQuoter
  { quoteExp  = compileDsl LeadingTableArg
  , quotePat  = const (fail "[dslWith| ... |] can only be used as an expression")
  , quoteType = const (fail "[dslWith| ... |] can only be used as an expression")
  , quoteDec  = const (fail "[dslWith| ... |] can only be used as an expression")
  }

-- | Which shape 'curriedRunner' should splice -- 'dsl' always compiles
--   against @Map.empty@; 'dslWith' takes the table as a fresh leading
--   lambda parameter instead.
data TableSource = NoTable | LeadingTableArg

compileDsl :: TableSource -> String -> Q Exp
compileDsl tblSrc src = do
  loc <- location
  let label = loc_filename loc <> ":" <> show (fst (loc_start loc))
  case parseDefinition label (dropLeadingNewline (T.pack src)) of
    Left err  -> fail (T.unpack (renderParseErr err))
    Right def -> curriedRunner tblSrc def

-- | Splices to @\\a1 ... an -> 'runDefinition' \@branch Map.empty def
--   [toBinding a1, ..., toBinding an]@ ('dsl'), or @\\tbl a1 ... an ->
--   'runDefinition' \@branch tbl def [toBinding a1, ..., toBinding an]@
--   ('dslWith') -- 'defParams' contributes the trailing @a1 ... an@
--   parameters either way, with no lambda at all when there are none and
--   no table argument ('dsl', 0-arity, the common top-level case).
--   Parameter names are reused from the source's own (so a type error at
--   a call site names @charname@, not a generic @arg1@).
--
--   The spliced call always applies 'runDefinition' at a type variable
--   literally named @branch@ -- 'runDefinition''s own @branch@-phantomed
--   effects (see "Storyteller.Core.ContentEffects") mean this can't be
--   left for GHC to infer; every @['dsl'| ... |]@\/@['dslWith'| ... |]@-
--   defined binding's own type signature must therefore declare
--   @forall branch r. ... =>@ (with @ScopedTypeVariables@ in scope) using
--   that exact name, the same naming convention
--   'Storyteller.Core.Branch.runStorage'\'s own @\@branch@ call sites
--   already follow throughout this codebase.
--
--   Each declared-parameter argument goes through
--   'Storyteller.Context.DSL.Context.toBinding' rather than being used
--   bare -- this is what makes every parameter position independently
--   polymorphic (@'Storyteller.Context.DSL.Context.ToBinding' a => a ->
--   ...@) instead of forced to 'Storyteller.Context.DSL.Compile.Binding'
--   by list homogeneity with 'runDefinition''s own signature, letting a
--   call site pass a plain 'Data.Text.Text', an
--   'Storyteller.Context.DSL.Value.Action' 'Storyteller.Context.DSL.Value.Value',
--   a 'Storyteller.Context.DSL.Context.Context', or a host function
--   directly -- see "Storyteller.Context.DSL.Context"'s own Haddock. The
--   table argument ('dslWith' only) is *not* run through 'toBinding' --
--   it's a plain 'Storyteller.Context.DSL.Compile.Library', used as-is.
curriedRunner :: TableSource -> Definition -> Q Exp
curriedRunner tblSrc def = do
  argNames <- mapM (newName . T.unpack) (defParams def)
  defExpr  <- lift def
  tblName  <- case tblSrc of
    NoTable         -> pure Nothing
    LeadingTableArg -> Just <$> newName "table"
  let toBindingArg n = AppE (VarE 'toBinding) (VarE n)
      runDefinitionAtBranch = AppTypeE (VarE 'runDefinition) (VarT (mkName "branch"))
      tblExpr = case tblName of
        Nothing -> AppE (VarE 'Map.fromList) (ListE [])
        Just n  -> VarE n
      call = AppE (AppE (AppE runDefinitionAtBranch tblExpr) defExpr) (ListE (map toBindingArg argNames))
      lamParams = maybe [] ((: []) . VarP) tblName ++ map VarP argNames
  pure $ if null lamParams then call else LamE lamParams call

-- | Parses at GHC compile time exactly like 'dsl', but splices the parsed
--   'Definition' itself, plain data, rather than 'curriedRunner''s curried
--   'Storyteller.Context.DSL.Value.Action' function. What a definition
--   meant to be *both* directly callable from Haskell *and*
--   cross-referenceable by name from another definition's own body wants
--   (see "Storyteller.Core.Context".@buildContextLibrary@) -- one source,
--   quoted once here, with the ordinary Haskell-callable form built from
--   it via 'runDefinition' rather than typed out a second time as a
--   runtime-parsed string.
defQuote :: QuasiQuoter
defQuote = QuasiQuoter
  { quoteExp  = compileDefQuote
  , quotePat  = const (fail "[defQuote| ... |] can only be used as an expression")
  , quoteType = const (fail "[defQuote| ... |] can only be used as an expression")
  , quoteDec  = const (fail "[defQuote| ... |] can only be used as an expression")
  }

compileDefQuote :: String -> Q Exp
compileDefQuote src = do
  loc <- location
  let label = loc_filename loc <> ":" <> show (fst (loc_start loc))
  case parseDefinition label (dropLeadingNewline (T.pack src)) of
    Left err  -> fail (T.unpack (renderParseErr err))
    Right def -> lift def

-- | Drops exactly one leading @\\n@, the one every @[dsl|@ opened on its
--   own line (the natural way to write a multi-statement definition)
--   contributes but which isn't part of the definition itself -- without
--   this, every position in the parsed 'Definition' would be off by one
--   line from what 'parseDefinition' reports for the same text written
--   as an ordinary string.
dropLeadingNewline :: Text -> Text
dropLeadingNewline t = case T.stripPrefix "\n" t of
  Just t' -> t'
  Nothing -> t
