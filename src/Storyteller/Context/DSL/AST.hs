{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Abstract syntax for the Context DSL described in @CONTEXT-DSL.md@.
--   This module only describes *what a definition says*, deliberately
--   independent of the parsing library used to produce it (see
--   "Storyteller.Context.DSL.Parser") and of how it eventually gets
--   compiled/interpreted into 'Storage.Core.StoreM' actions -- neither of
--   those concerns belongs here.
--
--   The shape here follows the spec's eight primitives directly:
--
--   * Rule 1 (function definitions) is 'Definition' -- a curried
--     parameter list plus a body 'Block'.
--   * Rule 2 (bare statement -> emit) is 'SExpr' -- every expression form
--     can stand alone in statement position.
--   * Rule 3 (@read@) is 'ERead'.
--   * Rule 4/5 (@x = ...@ / @as \"name\": ...@) are 'SLet'/'SAs'.
--   * Rule 6 (@in@) is 'SIn'.
--   * Rule 7 (@> \"text\"@) is 'EAssistant'.
--   * Rule 8 (@< expr@) is 'EUser'.
--   * @for@ is deliberately not its own primitive in the AST beyond
--     'SFor' -- the spec treats it as sugar over a builtin @each@ filter,
--     but it gets a dedicated constructor here (rather than being
--     desugared during parsing) so the parser can enforce, structurally,
--     that its source is always a literal glob/path -- never an arbitrary
--     filtered expression (see the spec's "Iteration and glob" section).
--     The same restriction is why 'ERead's argument and 'SFor's source
--     both carry a 'PathLit' rather than a general 'Expr'.
module Storyteller.Context.DSL.AST
  ( Name
  , Pos(..)
  , Located(..)
  , Quoting(..)
  , InterpPart(..)
  , InterpText
  , PathLit(..)
  , Expr(..)
  , Stmt(..)
  , Block
  , Definition(..)
  ) where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift)

type Name = Text

-- | A source position, independent of whichever parser library produced
--   it -- both 1-based, matching how editors and error messages usually
--   count.
data Pos = Pos
  { posLine :: !Int
  , posCol  :: !Int
  } deriving (Eq, Ord, Show, Lift)

-- | Pairs a value with the source position it started at. Carried on
--   every statement (not every expression) because the only place this
--   is needed downstream is statement-shaped: reporting *where* a
--   duplicate @as@ name was written, or where a nested block's required
--   indentation was violated.
data Located a = Located
  { locPos  :: !Pos
  , locItem :: !a
  } deriving (Eq, Show, Lift)

-- | Whether a string/path/glob token was written quoted or bare. This is
--   meaningful, not stylistic -- see "Value model" in the spec: only the
--   bare form is ever pattern-matched as a glob against the current
--   Reader scope, and only the quoted form guarantees inert plain text.
--   The parser preserves the distinction; deciding what to *do* with a
--   'Bare' token containing no glob metacharacters (e.g. @read
--   status/injury.md@, a literal single-segment lookup written bare) is
--   left to the compiler, per the spec's own tension between the general
--   quoting rule and @read@'s "never a glob" restriction.
data Quoting = Quoted | Bare
  deriving (Eq, Show, Lift)

-- | One piece of a string/path/glob literal after splitting out
--   @%name%@ interpolation spans.
data InterpPart
  = Lit Text
  | Interp Name
  deriving (Eq, Show, Lift)

type InterpText = [InterpPart]

-- | The argument position both @read@ and @for@'s source take: always a
--   literal path or glob token (quoted or bare), interpolation allowed,
--   never a general 'Expr' -- this is what makes "read never takes a
--   glob or a predicate query" and "for's source is always a literal
--   glob expression" structural parser guarantees rather than compiler
--   checks.
data PathLit = PathLit
  { pathQuoting :: !Quoting
  , pathText    :: !InterpText
  } deriving (Eq, Show, Lift)

data Expr
  = EString !Quoting !InterpText
    -- ^ A bare statement's plain string/glob literal. Per the Value
    --   model, a quoted one becomes @User@ text; a bare one is a live
    --   glob expression against the current Reader scope. Which one
    --   applies is exactly 'EString's own 'Quoting' tag.
  | EAssistant !InterpText
    -- ^ @> "text"@ -- primitive 7, produces @Assistant@-tagged text.
  | EUser !Expr
    -- ^ @< expr@ -- primitive 8, symmetric with @>@: re-tags whatever
    --   @expr@'s own messages already are (typically
    --   'Storyteller.Context.DSL.Value.FileRead', from a @read@) as
    --   @User@. A bare, unprefixed statement already gets whatever role
    --   its own primitive naturally defaults to (@User@ for a string
    --   literal, @FileRead@ for @read@ -- see rule 2) -- @<@ exists for
    --   the case an author wants @User@ specifically, overriding a
    --   @read@'s own undecided-until-rendered default. Constructs no new
    --   message on its own -- the Value model's three
    --   message-construction rules are unchanged; this only relabels
    --   ones an inner expression already produced. Takes a general
    --   'Expr' (unlike @>@, which only ever wraps a literal string)
    --   since its job is redecorating whatever an arbitrary
    --   already-composed expression yields, most often @read@ but not
    --   limited to it.
  | EIdent !Name
    -- ^ A reference to a local binding, function parameter, or a
    --   'Contexts'-branch definition, resolved statically (never through
    --   'read') by the compiler.
  | EApp !Expr [Expr]
    -- ^ Curried application, represented as one head plus its full
    --   argument spine rather than nested single-argument applications
    --   -- an AST-level simplification the compiler can fold either way.
  | EFilter !Expr !Name [Expr]
    -- ^ @expr | filterName(args...)@. The filter vocabulary is closed
    --   and host-provided (see the spec's "Filters" section); this node
    --   only records the call, never what the filter does.
  | ERead !PathLit
    -- ^ Primitive 3.
  deriving (Eq, Show, Lift)

data Stmt
  = SExpr !Expr
    -- ^ Primitive 2: emits to the enclosing writer target.
  | SAs !Expr !Block
    -- ^ Primitive 5. The name position is itself an 'Expr' (not
    --   restricted to a quoted literal) because the spec explicitly
    --   allows a bare loop variable as a computed name (@as f: ...@ in
    --   the Chekhov's-list example) alongside interpolated string names
    --   (@as \"chapters/%ch%/full\": ...@).
  | SLet !Name !(Maybe [Name]) !Block
    -- ^ Primitive 4, plus an optional curried parameter list when the
    --   bound body is itself a function (@calendar_context = dateMath:
    --   ...@).
  | SIn !Expr !Block
    -- ^ Primitive 6.
  | SFor !Name !PathLit !Block
    -- ^ Sugar over @each@ applied to a glob (see the module haddock for
    --   why the source is a 'PathLit', not an 'Expr').
  deriving (Eq, Show, Lift)

type Block = [Located Stmt]

-- | A whole definition -- a file's contents, or the RHS of a local
--   binding whose body happens to be a function (rule 1: "a file with no
--   head is a 0-ary function"). 'defParams' is @[]@ for the 0-ary case.
data Definition = Definition
  { defParams :: [Name]
  , defBody   :: Block
  } deriving (Eq, Show, Lift)
