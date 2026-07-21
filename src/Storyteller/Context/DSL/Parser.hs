{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the Context DSL (see @CONTEXT-DSL.md@). Deliberately
--   separate from "Storyteller.Context.DSL.AST" (the grammar this module
--   targets) and from any later compiler/interpreter -- this module's
--   only job is turning source text into either a 'Definition' or a
--   'ParseError' with enough structure for a frontend to render it
--   without re-implementing any parsing itself.
--
--   == Layout
--
--   The concrete syntax is indentation-sensitive (see the worked examples
--   in the spec): a @:@-introduced body is either the rest of the same
--   line (@as \"injury\": read status/injury.md@) or, if nothing follows
--   on that line, a block of one or more statements on subsequent lines,
--   indented strictly further than the statement that introduced them.
--   This is implemented by hand (see 'pIndentedBlock'\/'pBody') rather
--   than with megaparsec's 'Text.Megaparsec.Char.Lexer.indentBlock',
--   because that combinator assumes a header always produces indented
--   children; here the same @:@ can just as well be followed by a single
--   inline statement, which needed its own small state machine.
--
--   == Concrete-syntax decisions not pinned down by the spec
--
--   The spec says its concrete syntax ("string interpolation as
--   @%name%@, filter-call parens, etc.") is "validated for
--   expressiveness, not finalized for a parser." This parser had to make
--   a few calls the spec doesn't itself settle, all driven directly by
--   the worked examples:
--
--   * Filter arguments accept either @|filt(a, b)@ or a single bare
--     @|filt a@ (both appear in the worked examples: @latest(1)@ vs.
--     @orifempty \"not injured\"@) -- never more than one bare argument,
--     since that would be ambiguous with a second, separate filter.
--   * Identifiers (@let@\/@as@-computed-name\/@for@-variable\/filter\/
--     parameter names) allow interior dots (@agent.writer@-style), since
--     the spec's own dotted-key-as-path convention for named
--     definitions is expected to show up as an identifier.
--   * A bare (unquoted) token is lexed permissively -- letters, digits,
--     @_.%-*\/@ -- and classified *after* lexing: containing @\/@ or @*@
--     makes it a path\/glob 'EString'; otherwise it's an identifier
--     reference. This mirrors the spec's own framing (quoting, not
--     shape, is what's meaningful) while still letting @tracking/**.md@
--     and @charname@ share one lexeme rule instead of two competing,
--     backtracking ones.
module Storyteller.Context.DSL.Parser
  ( parseDefinition
  , ParseErr(..)
  , renderParseErr
  ) where

import Control.Monad (guard, void)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Storyteller.Context.DSL.AST

type Parser = Parsec Void Text

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | A structured parse failure, independent of megaparsec's own types --
--   everything a frontend needs to show a caret under the offending
--   column and a human-readable explanation, without linking against
--   megaparsec itself.
data ParseErr = ParseErr
  { peLine     :: !Int
  , peCol      :: !Int
  , peLineText :: !Text
    -- ^ The full source line the error occurred on, for caret rendering.
  , peMessage  :: !Text
    -- ^ A one-line, human-readable explanation (megaparsec's own
    --   rendering of "unexpected X, expecting Y").
  , peExpected :: [Text]
    -- ^ Just the "expecting" set, split out for a frontend that wants to
    --   e.g. render suggestion chips instead of parsing 'peMessage'.
  } deriving (Eq, Show)

-- | A plain-text rendering suitable for a terminal or log -- a
--   megaparsec-style two-line "message, then source line with a caret".
renderParseErr :: ParseErr -> Text
renderParseErr pe = T.unlines
  [ "line " <> T.pack (show (peLine pe)) <> ", column " <> T.pack (show (peCol pe)) <> ": " <> peMessage pe
  , peLineText pe
  , T.replicate (max 0 (peCol pe - 1)) " " <> "^"
  ]

toParseErr :: ParseErrorBundle Text Void -> ParseErr
toParseErr bundle =
  let err :| _         = bundleErrors bundle
      (line, col, lineText) = locate err (bundlePosState bundle)
      expected         = expectedItems err
  in ParseErr
       { peLine     = line
       , peCol      = col
       , peLineText = lineText
       , peMessage  = T.pack (parseErrorTextPretty err)
       , peExpected = expected
       }

locate :: ParseError Text Void -> PosState Text -> (Int, Int, Text)
locate err posState =
  let (mLineText, posState') = reachOffset (errorOffset err) posState
      pos                    = pstateSourcePos posState'
  in (unPos (sourceLine pos), unPos (sourceColumn pos), maybe "" T.pack mLineText)

expectedItems :: ParseError Text Void -> [Text]
expectedItems (TrivialError _ _ expected) =
  map renderErrorItem (Set.toList expected)
expectedItems (FancyError _ _) = []

renderErrorItem :: ErrorItem Char -> Text
renderErrorItem (Tokens ts)    = T.pack (NE.toList ts)
renderErrorItem (Label lbl)    = T.pack (NE.toList lbl)
renderErrorItem EndOfInput     = "end of input"

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Parse a whole definition (a file's contents, or a would-be file's
--   contents supplied inline) from source text. @name@ is only used to
--   label the source in error positions; it never affects parsing.
parseDefinition :: FilePath -> Text -> Either ParseErr Definition
parseDefinition name src =
  case parse (pDefinition <* scn <* eof) name src of
    Left bundle -> Left (toParseErr bundle)
    Right def   -> Right def

-- ---------------------------------------------------------------------------
-- Lexing
-- ---------------------------------------------------------------------------

-- | Intra-line whitespace/comments only -- never consumes a newline, so
--   every lexeme built on this automatically respects layout: an
--   application's argument list, or a filter chain, stops the moment the
--   line ends, without any explicit indentation check at those call
--   sites.
sc :: Parser ()
sc = L.space (void (some (char ' ' <|> char '\t'))) lineComment empty

-- | Whitespace including blank lines and comment-only lines -- used only
--   at block boundaries, where consuming past a newline is exactly the
--   point (deciding whether/where the next statement starts).
scn :: Parser ()
scn = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keywords :: [Text]
keywords = ["as", "in", "for", "read"]

keyword :: Text -> Parser ()
keyword kw = lexeme . try $ do
  _ <- string kw
  notFollowedBy identChar
  where identChar = alphaNumChar <|> char '_' <|> char '.'

-- | A strict identifier: bindings, filter names, parameters, loop
--   variables. Interior dots allowed (@agent.writer@-style), never
--   starting or ending with one.
identifier :: Parser Name
identifier = lexeme . try $ do
  first <- letterChar <|> char '_'
  rest  <- many (alphaNumChar <|> char '_' <|> char '.')
  let name = T.pack (first : rest)
  guard (not ("." `T.isSuffixOf` name))
  guard (name `notElem` keywords)
  pure name

-- | A permissive bare token: identifiers *and* bare paths/globs share
--   this one lexeme (see module haddock); callers classify by content
--   after the fact.
bareWord :: Parser Text
bareWord = lexeme . try $ do
  w <- some (alphaNumChar <|> oneOf ("_.%-*/" :: String))
  let t = T.pack w
  guard (t `notElem` keywords)
  pure t

isPathLike :: Text -> Bool
isPathLike t = T.any (`elem` ("/*" :: String)) t

-- | Split raw token text on @%name%@ interpolation spans.
parseInterp :: Text -> InterpText
parseInterp = go . T.splitOn "%"
  where
    -- T.splitOn "%" alternates literal/interpolated chunks starting with
    -- a literal one; an odd chunk count means an unterminated "%" span,
    -- which we treat leniently by folding the trailing "%" back into
    -- literal text rather than erroring here (interpolation validity is
    -- a compiler concern, not a lexer one).
    go parts = case parts of
      []  -> []
      [t] -> [Lit t | not (T.null t)]
      _   -> merge (zip (cycle [False, True]) parts)
    merge = filter isNonEmptyPart . map toPart
    toPart (isInterp, t)
      | isInterp  = Interp t
      | otherwise = Lit t
    isNonEmptyPart (Lit t)    = not (T.null t)
    isNonEmptyPart (Interp t) = not (T.null t)

pQuotedText :: Parser Text
pQuotedText = lexeme . try $ do
  _ <- char '"'
  chars <- many (escaped <|> satisfy (\c -> c /= '"' && c /= '\n'))
  _ <- char '"'
  pure (T.pack chars)
  where
    escaped = char '\\' *> (char '"' <|> char '\\')

pQuotedExpr :: Parser Expr
pQuotedExpr = EString Quoted . parseInterp <$> pQuotedText

pBareExpr :: Parser Expr
pBareExpr = classify <$> bareWord
  where
    classify t
      | isPathLike t = EString Bare (parseInterp t)
      | otherwise    = EIdent t

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

pParenExpr :: Parser Expr
pParenExpr = between (symbol "(") (symbol ")") pExpr

-- | @read@'s own argument is a general expression now (see
--   'Storyteller.Context.DSL.AST.Expr'\'s own haddock on 'ERead') --
--   'pApp', not 'pExpr', for the same reason 'pAssistantExpr' uses 'pApp'
--   for @>@'s own argument: a trailing @| filter@ has to apply to
--   @read@'s own result (@read a | b@ means "filter what @read a@
--   produced"), not get swallowed into its argument.
pReadExpr :: Parser Expr
pReadExpr = do
  keyword "read"
  ERead <$> pApp

-- | @< expr@ -- rule 8. Unlike @>@ (a statement-only literal
--   constructor, see 'pAssistantExpr'), this wraps a general expression
--   (most often @read@), so it belongs at 'pAtom' level: usable anywhere
--   an atom is, including nested inside application arguments or filter
--   chains. Wraps a whole application (@< f arg@ re-tags @f arg@'s own
--   result, not just @f@), so its own argument is 'pApp', not a bare
--   'pAtom'.
pUserExpr :: Parser Expr
pUserExpr = do
  _ <- symbol "<"
  EUser <$> pApp

-- | An atom: the tightest-binding expression form, usable as an
--   application argument or a filter's single bare argument.
pAtom :: Parser Expr
pAtom = pParenExpr <|> pReadExpr <|> pUserExpr <|> pQuotedExpr <|> pBareExpr

-- | Curried application by juxtaposition. Because 'sc' never crosses a
--   newline, this loop naturally stops at end-of-line -- no explicit
--   layout check needed here.
pApp :: Parser Expr
pApp = do
  h    <- pAtom
  args <- many (try pAtom)
  pure (if null args then h else EApp h args)

pFilterStep :: Parser (Name, [Expr])
pFilterStep = do
  name <- identifier
  args <- pParenArgs <|> (maybe [] (: []) <$> optional pAtom)
  pure (name, args)
  where
    pParenArgs = between (symbol "(") (symbol ")") (pExpr `sepBy` symbol ",")

pExpr :: Parser Expr
pExpr = do
  e0    <- pApp
  steps <- many (symbol "|" *> pFilterStep)
  pure (foldl' (\acc (n, as) -> EFilter acc n as) e0 steps)

-- | The name position of @as@: either a computed bare identifier (a loop
--   variable, per the Chekhov's-list example) or an interpolated string.
pNameExpr :: Parser Expr
pNameExpr = pQuotedExpr <|> (EIdent <$> identifier)

-- | @> expr@ -- rule 7, widened to a general expression, symmetric with
--   'pUserExpr'. Still statement-only (not folded into 'pAtom' the way
--   @<@ is) -- every real use is a bare top-level re-tag, not something
--   nested inside a filter chain or application argument.
pAssistantExpr :: Parser Expr
pAssistantExpr = do
  _ <- symbol ">"
  EAssistant <$> pApp

-- ---------------------------------------------------------------------------
-- Layout-sensitive blocks
-- ---------------------------------------------------------------------------

-- | The body of a @:@ (or, for a headless @x = ...@, the position right
--   after @=@): either the rest of the current line as a single
--   statement, or -- if nothing follows on that line -- an indented
--   block of one or more statements on the lines after, each indented
--   strictly further than @parentCol@.
pBody :: Pos -> Parser Block
pBody parentCol = inline <|> pIndentedBlock parentCol
  where
    inline = do
      sc
      notFollowedBy (void eol <|> eof <|> lineComment)
      s <- pStmtLine
      pure [s]

-- | Reads one or more statements, all at the same freshly-established
--   column, which must be indented strictly further than @parentCol@.
pIndentedBlock :: Pos -> Parser Block
pIndentedBlock parentCol = do
  scn
  col <- currentPos
  guard (posCol col > posCol parentCol)
  pStatementsAtCol (posCol col)

-- | Statements at the top of a file: like 'pIndentedBlock', but with no
--   enclosing statement to be indented further than -- whatever column
--   the first statement lands on becomes the reference for its
--   siblings. Unlike 'pIndentedBlock' (via 'pStatementsAtCol'), zero
--   statements is a valid top-level program, not a parse error: an empty
--   (or whitespace\/comment-only) definition is a genuine, meaningful
--   0-arity 'Storyteller.Context.DSL.Value.Value' -- @Value{valueDefault =
--   pure [], valueEntries = []}@, "produces nothing" -- not a mistake the
--   way an empty @as \"x\":@ body is (see the parser test for that: a
--   dangling @as@ with nothing under it is still rejected, since there's
--   no sensible empty-on-purpose reading for "attach nothing to this
--   name").
pTopBlock :: Parser Block
pTopBlock = do
  scn
  col <- currentPos
  many (try (atCol (posCol col)) *> pStmtLine)

-- | 'try' scoped to just 'atCol' (a cheap positional check: is there a
--   statement at all at this column, or have we dedented\/hit eof) --
--   deliberately not wrapped around 'pStmtLine' itself. Once a statement
--   is known to start here, any failure while actually parsing its own
--   content (a dangling @as@ with no body, say) has to propagate as a
--   real error, not be silently swallowed as "must just be the end of
--   this block" the way wrapping the whole thing in one 'try' would (this
--   is exactly what made 'pTopBlock's own "zero is fine" case, needed for
--   a genuinely empty program, also mask a real parse error in a
--   malformed one, before this was split out).
pStatementsAtCol :: Int -> Parser Block
pStatementsAtCol col = some (try (atCol col) *> pStmtLine)

-- | 'eof' is checked explicitly, not left to 'currentPos'\/'guard' alone:
--   trailing whitespace can leave @eof@'s own reported column
--   coincidentally equal to @col@, which would otherwise read as "yes,
--   there's a statement right here" and send 'pStmtLine' in against
--   nothing at all.
atCol :: Int -> Parser ()
atCol col = do
  scn
  notFollowedBy eof
  p <- currentPos
  guard (posCol p == col)

currentPos :: Parser Pos
currentPos = do
  p <- getSourcePos
  pure (Pos (unPos (sourceLine p)) (unPos (sourceColumn p)))

located :: Parser Stmt -> Parser (Located Stmt)
located p = do
  pos <- currentPos
  Located pos <$> p

pStmtLine :: Parser (Located Stmt)
pStmtLine = located $
      pAsStmt
  <|> pInStmt
  <|> pForStmt
  <|> try pLetStmt
  <|> (SExpr <$> (pAssistantExpr <|> pExpr))

pAsStmt :: Parser Stmt
pAsStmt = do
  col   <- currentPos
  keyword "as"
  nameE <- pNameExpr
  _     <- symbol ":"
  SAs nameE <$> pBody col

pInStmt :: Parser Stmt
pInStmt = do
  col <- currentPos
  keyword "in"
  e   <- pExpr
  _   <- symbol ":"
  SIn e <$> pBody col

pForStmt :: Parser Stmt
pForStmt = do
  col <- currentPos
  keyword "for"
  var <- identifier
  keyword "in"
  src <- pExpr
  _   <- symbol ":"
  SFor var src <$> pBody col

-- | @x = ...@, optionally with a curried parameter head
--   (@x = a: b: ...@). Tried after the keyword-led statements and needs
--   its own 'try' since a bare application statement can also start with
--   a plain identifier.
pLetStmt :: Parser Stmt
pLetStmt = do
  col    <- currentPos
  name   <- identifier
  _      <- symbol "="
  params <- many (try (identifier <* symbol ":"))
  body   <- pBody col
  pure (SLet name (if null params then Nothing else Just params) body)

-- ---------------------------------------------------------------------------
-- Whole definitions
-- ---------------------------------------------------------------------------

pDefinition :: Parser Definition
pDefinition = do
  scn
  params <- many (try (identifier <* symbol ":"))
  body   <- pTopBlock
  pure (Definition params body)
