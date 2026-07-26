"use client";

// CodeMirror syntax highlighting for the Context DSL (see CONTEXT-DSL.md),
// used by every CodeCostEditor instance (app/code-cost-editor.tsx).
//
// A hand-written stream tokenizer, not a lezer grammar: the DSL's *lexical*
// rules are a handful of cases (comment, string, bracket glob, bare token,
// punctuation) and are the only thing highlighting needs, while its
// interesting structure is indentation-sensitive layout — which a lezer
// grammar would have to model in full to buy anything at all here. So this
// mirrors Storyteller.Context.DSL.Parser's *lexer* exactly (`sc`/
// `lineComment`, `keywords`, `identifier`, `bareWord`, `isPathLike`,
// `pQuotedText`, `parseInterp`, `pBracketGlobExpr`) and deliberately knows
// nothing about statements. That's the boundary to keep this on the right
// side of: this file must never encode a *semantic* rule about the language
// (what a name resolves to, which filters exist) — a second, driftable copy
// of the backend's own answer. Colouring a token by its shape is safe
// precisely because shape is all the real lexer uses too.
//
// The one place the two can drift is the lexer itself, so the token rules
// below cite the Haskell function each mirrors, and dslLanguage.test.ts
// pins the cases that would silently mis-colour if one changed.

import { StreamLanguage, HighlightStyle, syntaxHighlighting, type StringStream, type StreamParser } from "@codemirror/language";
import { tags } from "@lezer/highlight";

// Parser.hs's `keywords`. A bare token is a keyword only when it *is* one
// of these entirely — `readable` is an identifier, matching the real
// lexer's `notFollowedBy identChar`.
const KEYWORDS = new Set(["as", "in", "for", "read"]);

// Parser.hs's `bareWord`: alphanumerics plus `_.%-*/`. Identifiers and bare
// paths/globs share this one lexeme and are told apart afterwards, exactly
// as the real lexer does it (see `isPathLike`) rather than by two competing
// patterns here.
const BARE_WORD = /^[A-Za-z0-9_.%*/-]+/;

// Parser.hs's `isPathLike`: containing `/` or `*` makes a bare token a
// path/glob literal rather than a name reference.
const PATH_LIKE = /[/*]/;

// `%name%` interpolation, inside a quoted string — Parser.hs's `parseInterp`.
// Unterminated spans are left as ordinary string text, the same leniency
// `parseInterp` itself applies.
const INTERP = /^%[^%\n]*%/;

// Multi-line strings are real (Parser.hs's `pQuotedText`: only `"` or
// end-of-input closes one), so "am I inside a string" has to survive across
// lines — that's the whole reason this parser carries state at all.
// `afterPipe` is the one other bit of context a token needs: the identifier
// immediately after `|` is a filter name (`pFilterStep`), not a variable
// reference.
export interface DslTokenState {
  inString: boolean;
  afterPipe: boolean;
}

export function startDslState(): DslTokenState {
  return { inString: false, afterPipe: false };
}

// Consume string content up to the closing quote, an interpolation span, or
// end of line — whichever comes first. Handles `\"`/`\\` (the only escapes
// `pQuotedText` accepts).
function stringToken(stream: StringStream, state: DslTokenState): string {
  if (stream.peek() === "%" && stream.match(INTERP)) return "dslInterp";
  let escaped = false;
  while (!stream.eol()) {
    const c = stream.next() as string;
    if (escaped) { escaped = false; continue; }
    if (c === "\\") { escaped = true; continue; }
    if (c === '"') { state.inString = false; break; }
    // Only stop for a `%` that really opens an interpolation span — a lone
    // one ("100% sure") is literal text, per parseInterp's own leniency,
    // and shouldn't even split the token.
    if (stream.peek() === "%" && startsInterp(stream)) break;
  }
  return "dslString";
}

// Does an interpolation span start exactly at the stream's position?
function startsInterp(stream: StringStream): boolean {
  return INTERP.test(stream.string.slice(stream.pos));
}

export function tokenizeDsl(stream: StringStream, state: DslTokenState): string | null {
  if (state.inString) return stringToken(stream, state);
  if (stream.eatSpace()) return null;

  // `--` to end of line (Parser.hs's `lineComment`). Must be tested before
  // the bare-token rule, whose charset includes `-`.
  if (stream.match("--")) {
    stream.skipToEnd();
    return "dslComment";
  }

  const ch = stream.peek() as string;

  if (ch === '"') {
    stream.next();
    state.inString = true;
    state.afterPipe = false;
    // An interpolation opening immediately after the quote gets its own
    // token on the next call; this one is just the delimiter.
    if (stream.peek() === "%" && startsInterp(stream)) return "dslString";
    return stringToken(stream, state);
  }

  // `[glob text]` (Parser.hs's `pBracketGlobExpr`) — unambiguously a path,
  // whatever it contains, so the brackets colour as part of it. An
  // unterminated one still consumes the rest of the line rather than
  // stalling the tokenizer.
  if (ch === "[") {
    stream.next();
    stream.match(/^[^\]\n]*\]?/);
    state.afterPipe = false;
    return "dslPath";
  }

  // `>` (assistant) and `<` (user) construct/re-tag a message's role — the
  // one piece of punctuation that changes what a line *is*, so it reads as
  // its own thing rather than as ordinary operator noise.
  if (ch === ">" || ch === "<") {
    stream.next();
    state.afterPipe = false;
    return "dslRole";
  }

  if (ch === "|" || ch === "=" || ch === ":" || ch === ";" || ch === ",") {
    stream.next();
    state.afterPipe = ch === "|";
    return "dslOperator";
  }

  if (ch === "(" || ch === ")") {
    stream.next();
    state.afterPipe = false;
    return "dslParen";
  }

  if (stream.match(BARE_WORD)) {
    const word = stream.current();
    const wasAfterPipe = state.afterPipe;
    state.afterPipe = false;
    if (KEYWORDS.has(word)) return "dslKeyword";
    if (PATH_LIKE.test(word)) return "dslPath";
    if (wasAfterPipe) return "dslFilter";
    // `x = ...` (Parser.hs's `pLetStmt`) — a name being *defined* rather
    // than referenced. Lookahead only, no state: `=` can't appear in any
    // other position where a bare token precedes it.
    if (/^\s*=/.test(stream.string.slice(stream.pos))) return "dslDef";
    return "dslVar";
  }

  // Anything else: consume one character so the tokenizer always advances.
  stream.next();
  return null;
}

export const dslStreamParser: StreamParser<DslTokenState> = {
  name: "context-dsl",
  startState: startDslState,
  token: tokenizeDsl,
  languageData: { commentTokens: { line: "--" } },
  tokenTable: {
    dslComment:  tags.lineComment,
    dslKeyword:  tags.keyword,
    dslString:   tags.string,
    dslInterp:   tags.special(tags.string),
    dslPath:     tags.literal,
    dslFilter:   tags.function(tags.variableName),
    dslDef:      tags.definition(tags.variableName),
    dslRole:     tags.controlOperator,
    dslOperator: tags.operator,
    dslParen:    tags.paren,
    dslVar:      tags.variableName,
  },
};

// Colours come from the app's own palette variables (globals.css), not from
// a CodeMirror theme preset — same reasoning as code-cost-editor.tsx's
// baseTheme: those variables already flip with light/dark, so following
// them is what keeps the editor themed at all, and it keeps the DSL's own
// colours meaning the same things they mean elsewhere in the UI (sky for
// files/paths, amber for the live/edited thing, rose for a role marker).
const dslHighlightStyle = HighlightStyle.define([
  { tag: tags.lineComment,                   color: "var(--text-faint)", fontStyle: "italic" },
  { tag: tags.keyword,                       color: "var(--violet)" },
  { tag: tags.string,                        color: "var(--emerald)" },
  { tag: tags.special(tags.string),          color: "var(--amber)" },
  { tag: tags.literal,                       color: "var(--sky)" },
  { tag: tags.function(tags.variableName),   color: "var(--teal)" },
  { tag: tags.definition(tags.variableName), color: "var(--amber)" },
  { tag: tags.controlOperator,               color: "var(--rose)" },
  { tag: tags.operator,                      color: "var(--text-dim)" },
  { tag: tags.paren,                         color: "var(--text-dim)" },
  { tag: tags.variableName,                  color: "var(--text-body)" },
]);

// The one thing an editor needs to import: language + colours together.
export const contextDslHighlighting = [
  StreamLanguage.define(dslStreamParser),
  syntaxHighlighting(dslHighlightStyle),
];
