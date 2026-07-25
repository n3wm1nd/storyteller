{-# LANGUAGE OverloadedStrings #-}

-- | Reconstructs Context DSL source text from a parsed 'Definition' --
--   the inverse of "Storyteller.Context.DSL.Parser". Exists so a
--   compiled-in library default's own source (@[defQuote| ... |]@ in
--   "Storyteller.Context.DSL.Library") can be served to a client that
--   wants to show/seed-edit it, without keeping a second, hand-copied
--   mirror of the same text anywhere (the frontend used to; see the
--   project chat that replaced it with this module plus a small
--   endpoint).
--
--   Not a general formatter -- it never reproduces the original comments
--   or exact whitespace/parenthesization a human wrote, and always emits
--   the indented-block form of a body (never the "rest of the line"
--   inline shorthand 'Storyteller.Context.DSL.Parser.pBody' also
--   accepts). What it guarantees instead: printing then re-parsing
--   always yields an AST 'Eq' to the original (see
--   "Storyteller.Context.DSL.PrettyPrintSpec", which round-trips every
--   entry in 'Storyteller.Context.DSL.Library.defaultLibraryOrder').
--   That's the only property a "what does this actually run" viewer
--   needs -- never claims to hand back byte-identical source.
--
--   == Parenthesization
--
--   'pApp''s own arguments are 'pAtom's, not general expressions (see
--   Parser.hs), so any 'EApp'\/'EFilter'\/'EAssistant'\/'EUser' used as an
--   application argument needs wrapping parens to parse back to the same
--   tree; a bare 'EString'\/'EIdent' never does. This module always
--   parenthesizes in exactly those non-atomic argument positions --
--   conservative (sometimes a redundant paren pair a human wouldn't
--   write) but never wrong, which is the only bar 'defBody'\/round-trip
--   correctness sets.
module Storyteller.Context.DSL.PrettyPrint
  ( prettyDefinition
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Context.DSL.AST

-- | Pretty-prints a whole 'Definition' -- its curried parameter list
--   (each followed by @:@, one per line, matching 'pDefinition''s own
--   @many (identifier <* symbol ":")@ shape) then its body, indented two
--   spaces per nesting level beneath each parameter.
prettyDefinition :: Definition -> Text
prettyDefinition (Definition params body) =
  T.unlines (map (<> ":") params ++ map (prettyStmt indent) body)
  where
    indent = 2 * length params

prettyBlock :: Int -> Block -> [Text]
prettyBlock indent = concatMap (prettyStmtLines indent)

prettyStmt :: Int -> Located Stmt -> Text
prettyStmt indent = T.unlines . prettyStmtLines indent

prettyStmtLines :: Int -> Located Stmt -> [Text]
prettyStmtLines indent (Located _ stmt) = case stmt of
  SExpr e -> [pad indent <> prettyExpr e]
  SAs nameE body ->
    (pad indent <> "as " <> prettyNameExpr nameE <> ":") : prettyBlock (indent + 2) body
  SLet name mParams body ->
    let params = maybe "" (T.concat . map ((<> ":") . (" " <>))) mParams
    in (pad indent <> name <> " =" <> params) : prettyBlock (indent + 2) body
  SIn e body ->
    (pad indent <> "in " <> prettyExpr e <> ":") : prettyBlock (indent + 2) body
  SFor var srcE body ->
    (pad indent <> "for " <> var <> " in " <> prettyExpr srcE <> ":") : prettyBlock (indent + 2) body

pad :: Int -> Text
pad n = T.replicate n " "

-- | 'pNameExpr' only ever parses a quoted string or a bare identifier --
--   narrower than 'prettyExpr' needs to handle in general.
prettyNameExpr :: Expr -> Text
prettyNameExpr (EIdent n)             = n
prettyNameExpr e@(EString Quoted _)   = prettyExpr e
prettyNameExpr e                      = prettyExpr e -- defensive; not reachable from a real parse

-- | A general expression, parenthesizing exactly where 'pApp''s
--   atom-only argument grammar requires it (see this module's own
--   Haddock).
prettyExpr :: Expr -> Text
prettyExpr expr = case expr of
  EString Quoted parts -> "\"" <> prettyInterpQuoted parts <> "\""
  EString Bare   parts -> prettyBareGlob parts
  EAssistant inner -> "> " <> prettyAtomArg inner
  EUser inner       -> "< " <> prettyAtomArg inner
  EIdent name       -> name
  EApp headE argEs  -> T.unwords (prettyAtomArg headE : map prettyAtomArg argEs)
  EFilter inner name argEs ->
    prettyExpr inner <> " | " <> name <> prettyFilterArgs argEs
  ERead argE -> "read " <> prettyAtomArg argE

-- | An expression used where the grammar only accepts a 'pAtom' (an
--   application argument, @>@\/@<@'s own operand via 'pApp' -- which
--   itself bottoms out at atoms, @read@'s argument). Only the genuinely
--   atomic forms (string literal, identifier) print bare; anything
--   built from 'pExpr'\/'pApp' machinery gets wrapped in parens so
--   'pParenExpr' can hand it back whole on re-parse.
prettyAtomArg :: Expr -> Text
prettyAtomArg e@(EString _ _) = prettyExpr e
prettyAtomArg e@(EIdent _)    = prettyExpr e
prettyAtomArg e               = "(" <> prettyExpr e <> ")"

-- | @|filt(a, b)@ for two-or-more (or explicitly zero) arguments, the
--   single-bare-argument @|filt a@ shorthand for exactly one -- mirrors
--   'pFilterStep''s own @pParenArgs <|> optional pAtom@ choice. The
--   shorthand only accepts an atom, so a non-atomic single argument still
--   needs the parenthesized form to round-trip.
prettyFilterArgs :: [Expr] -> Text
prettyFilterArgs []    = ""
prettyFilterArgs [arg] = case arg of
  EString _ _ -> " " <> prettyExpr arg
  EIdent _    -> " " <> prettyExpr arg
  _           -> "(" <> prettyExpr arg <> ")"
prettyFilterArgs args  = "(" <> T.concat (intersperse ", " (map prettyExpr args)) <> ")"

-- | A quoted string's own interpolation spans re-wrapped in @%...%@;
--   literal spans printed as-is (no re-escaping needed: 'pQuotedText'
--   only recognizes @\\"@\/@\\\\@, and neither this module nor the
--   parser it targets ever produces a literal span containing an
--   unescaped @"@ from a well-formed 'InterpText' in the first place).
prettyInterpQuoted :: InterpText -> Text
prettyInterpQuoted = T.concat . map part
  where
    part (Lit t)    = t
    part (Interp n) = "%" <> n <> "%"

-- | A bare (glob/path) 'EString' can originally have come from either
--   'Storyteller.Context.DSL.Parser.pBareExpr' (a whitespace-terminated
--   token, charset-restricted) or 'Storyteller.Context.DSL.Parser.pBracketGlobExpr'
--   (@[...]@, any character except @]@\/newline) -- the AST doesn't
--   distinguish which, and only the bracket form is valid for every
--   possible path text (one containing a space, say). Always printing
--   the bracket form is therefore the only choice that's correct for
--   every 'EString' 'Bare' this module might see, not just the common
--   case; the more familiar bare-word spelling a human would write for
--   an ordinary path is a cosmetic difference this module doesn't chase
--   (see its own header on what round-trip correctness does and doesn't
--   promise).
prettyBareGlob :: InterpText -> Text
prettyBareGlob parts = "[" <> T.concat (map part parts) <> "]"
  where
    part (Lit t)    = t
    part (Interp n) = "%" <> n <> "%"
