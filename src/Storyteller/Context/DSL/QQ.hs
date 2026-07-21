{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A quasiquoter for the Context DSL: @['dsl'| ... |]@ parses its
--   contents *at GHC compile time* (a malformed definition is a
--   compile error, at the quote's own source location, not something
--   that surfaces later at runtime) and splices a curried Haskell
--   function of exactly the source's own arity -- one 'Action' 'Value'
--   parameter per declared parameter (rule 1's curried parameter list),
--   returning the compiled 'Action' 'Value' once fully applied. A
--   0-parameter definition (the common top-level case) splices directly
--   to an 'Action' 'Value' -- nothing left to apply.
--
--   > injuryStatus :: Action Value
--   > injuryStatus = [dsl|
--   >   as "injury": read status/injury.md
--   > |]
--   >
--   > castingStatus :: Action Value -> Action Value
--   > castingStatus = [dsl|
--   >   charname:
--   >     in (charname | branch): read "status/casting_log.md" | orifempty "no casting today"
--   > |]
--
--   The generated function is exactly
--   'Storyteller.Context.DSL.Compile.runDefinition' applied to the
--   parsed 'Storyteller.Context.DSL.AST.Definition' and however many
--   arguments the lambda collects -- GHC checks the arity at every call
--   site, instead of 'Storyteller.Context.DSL.Compile.compileDefinition'
--   only discovering a mismatch at runtime. The scope is always whatever
--   commit is ambient when the returned 'Action' finally runs (see
--   'Storyteller.Context.DSL.Compile.currentScope') -- there's no way to
--   splice in a different scope from here, by design. Anything that
--   needs one (composing a sub-'Definition' against an explicitly chosen
--   'Storyteller.Context.DSL.Value.Value', say) calls
--   'Storyteller.Context.DSL.Parser.parseDefinition' and
--   'Storyteller.Context.DSL.Compile.compileDefinition' directly, same as
--   before quasiquotes existed.
module Storyteller.Context.DSL.QQ (dsl, defQuote) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.TH (Exp(..), Pat(VarP), Q, newName)
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
  { quoteExp  = compileDsl
  , quotePat  = const (fail "[dsl| ... |] can only be used as an expression")
  , quoteType = const (fail "[dsl| ... |] can only be used as an expression")
  , quoteDec  = const (fail "[dsl| ... |] can only be used as an expression")
  }

compileDsl :: String -> Q Exp
compileDsl src = do
  loc <- location
  let label = loc_filename loc <> ":" <> show (fst (loc_start loc))
  case parseDefinition label (dropLeadingNewline (T.pack src)) of
    Left err  -> fail (T.unpack (renderParseErr err))
    Right def -> curriedRunner def

-- | Splices to @\\a1 ... an -> 'runDefinition' def [toBinding a1, ...,
--   toBinding an]@, with exactly as many lambda parameters as
--   'defParams' -- the 0-arity case (most top-level definitions) needs no
--   lambda at all, just the call. Parameter names are reused from the
--   source's own (so a type error at a call site names @charname@, not a
--   generic @arg1@).
--
--   Each argument goes through 'Storyteller.Context.DSL.Context.toBinding'
--   rather than being used bare -- this is what makes every parameter
--   position independently polymorphic
--   (@'Storyteller.Context.DSL.Context.ToBinding' a => a -> ...@) instead
--   of forced to 'Storyteller.Context.DSL.Compile.Binding' by list
--   homogeneity with 'runDefinition''s own signature, letting a call site
--   pass a plain 'Data.Text.Text', an 'Storyteller.Context.DSL.Value.Action'
--   'Storyteller.Context.DSL.Value.Value', a
--   'Storyteller.Context.DSL.Context.Context', or a host function directly
--   -- see "Storyteller.Context.DSL.Context"'s own Haddock.
curriedRunner :: Definition -> Q Exp
curriedRunner def = do
  argNames <- mapM (newName . T.unpack) (defParams def)
  defExpr  <- lift def
  let toBindingArg n = AppE (VarE 'toBinding) (VarE n)
      call = AppE (AppE (VarE 'runDefinition) defExpr) (ListE (map toBindingArg argNames))
  pure $ if null argNames then call else LamE (map VarP argNames) call

-- | Parses at GHC compile time exactly like 'dsl', but splices the parsed
--   'Definition' itself, plain data, rather than 'curriedRunner''s curried
--   'Storyteller.Context.DSL.Value.Action' function. What a definition
--   meant to be *both* directly callable from Haskell *and*
--   cross-referenceable by name from another definition's own body wants
--   (see "Storyteller.Context.DSL.Value".@ContextLibrary@) -- one source,
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
