{-# LANGUAGE OverloadedStrings #-}

-- | A quasiquoter for the Context DSL: @['dsl'| ... |]@ parses its
--   contents *at GHC compile time* (a malformed definition is a
--   compile error, at the quote's own source location, not something
--   that surfaces later at runtime) and embeds the resulting
--   'Definition' directly -- no re-parsing when the program actually
--   runs. Also doubles as the multiline-string convenience a raw
--   'Data.Text.Text' literal doesn't give you (no @\\n\\@ continuations
--   needed across lines).
--
--   > injuryStatus :: Definition
--   > injuryStatus = [dsl|
--   >   as "injury": read status/injury.md
--   > |]
--
--   The produced 'Definition' is exactly what
--   'Storyteller.Context.DSL.Parser.parseDefinition' would return for
--   the same text -- this quoter contributes no new semantics, only
--   moving *when* parsing happens. Pass it straight to
--   'Storyteller.Context.DSL.Compile.compileDefinition' along with
--   whatever scope\/arguments are only known at runtime (a branch's
--   tree can't be baked in at compile time, so this only ever gets you
--   as far as a 'Definition' -- see that module's own haddock for why
--   running one still needs a real 'Storyteller.Context.DSL.Value.BranchResolver').
module Storyteller.Context.DSL.QQ (dsl) where

import Data.Text (Text)
import qualified Data.Text as T

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(lift), Loc(..), location)

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
    Right def -> lift def

-- | Drops exactly one leading @\\n@, the one every @[dsl|@ opened on its
--   own line (the natural way to write a multi-statement definition)
--   contributes but which isn't part of the definition itself -- without
--   this, every position in the embedded 'Definition' would be off by
--   one line from what 'parseDefinition' reports for the same text
--   written as an ordinary string.
dropLeadingNewline :: Text -> Text
dropLeadingNewline t = case T.stripPrefix "\n" t of
  Just t' -> t'
  Nothing -> t
