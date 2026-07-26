import { describe, expect, test } from "bun:test";
import { StringStream } from "@codemirror/language";
import { tokenizeDsl, startDslState, type DslTokenState } from "./dslLanguage";

// Drive the tokenizer over whole lines the way CodeMirror does — one
// StringStream per line, one carried-over state — and flatten the result to
// [token, text] pairs. Whitespace (a null token) is dropped; an unstyled
// token would show up as ["", text], which is itself worth seeing in a
// failure.
function tokens(source: string): [string, string][] {
  const state: DslTokenState = startDslState();
  const out: [string, string][] = [];
  for (const line of source.split("\n")) {
    const stream = new StringStream(line, 2, 2);
    while (!stream.eol()) {
      stream.start = stream.pos;
      const tok = tokenizeDsl(stream, state);
      if (stream.pos === stream.start) throw new Error(`tokenizer did not advance at ${stream.pos} in ${JSON.stringify(line)}`);
      if (tok !== null) out.push([tok, stream.current()]);
    }
  }
  return out;
}

describe("comments", () => {
  test("-- runs to end of line", () => {
    expect(tokens("read x -- why this file")).toEqual([
      ["dslKeyword", "read"],
      ["dslVar", "x"],
      ["dslComment", "-- why this file"],
    ]);
  });

  // `-` is in bareWord's charset (Parser.hs), so the comment rule has to win
  // — otherwise a whole comment lexes as a path-ish token.
  test("a bare token may still contain a single dash", () => {
    expect(tokens("alice-battle")).toEqual([["dslVar", "alice-battle"]]);
  });
});

describe("keywords", () => {
  test("the four keywords", () => {
    expect(tokens("as in for read")).toEqual([
      ["dslKeyword", "as"],
      ["dslKeyword", "in"],
      ["dslKeyword", "for"],
      ["dslKeyword", "read"],
    ]);
  });

  // Parser.hs's `keyword` uses notFollowedBy identChar: a longer word that
  // merely starts with a keyword is an ordinary identifier.
  test("a longer word starting with a keyword is not one", () => {
    expect(tokens("readable inner forge")).toEqual([
      ["dslVar", "readable"],
      ["dslVar", "inner"],
      ["dslVar", "forge"],
    ]);
  });
});

describe("paths vs identifiers", () => {
  // Parser.hs's isPathLike: `/` or `*` in a bare token makes it a path.
  test("a bare token with a slash or star is a path", () => {
    expect(tokens("lore/magic.md tracking/**.md chapters/*")).toEqual([
      ["dslPath", "lore/magic.md"],
      ["dslPath", "tracking/**.md"],
      ["dslPath", "chapters/*"],
    ]);
  });

  test("a dotted name with neither is an identifier", () => {
    expect(tokens("context.lore")).toEqual([["dslVar", "context.lore"]]);
  });

  // pBracketGlobExpr: the brackets alone mean "definitely a path", however
  // ordinary the content looks.
  test("a bracket glob is a path even without a slash or star", () => {
    expect(tokens("[file with spaces.md]")).toEqual([["dslPath", "[file with spaces.md]"]]);
  });

  test("an unterminated bracket glob still consumes the line", () => {
    expect(tokens("[oops")).toEqual([["dslPath", "[oops"]]);
  });
});

describe("strings", () => {
  test("a quoted string, escapes included", () => {
    expect(tokens('"she said \\"no\\""')).toEqual([["dslString", '"she said \\"no\\""']]);
  });

  // pQuotedText: only `"` (or end of input) closes one, so a string spans
  // lines — the reason the tokenizer carries state at all.
  test("a string spans lines until its closing quote", () => {
    expect(tokens('"line one\nline two" read x')).toEqual([
      ["dslString", '"line one'],
      ["dslString", 'line two"'],
      ["dslKeyword", "read"],
      ["dslVar", "x"],
    ]);
  });

  test("%interpolation% inside a string is its own token", () => {
    expect(tokens('"about %name% here"')).toEqual([
      ["dslString", '"about '],
      ["dslInterp", "%name%"],
      ["dslString", ' here"'],
    ]);
  });

  test("an interpolation opening right after the quote still highlights", () => {
    expect(tokens('"%name%!"')).toEqual([
      ["dslString", '"'],
      ["dslInterp", "%name%"],
      ["dslString", '!"'],
    ]);
  });

  // parseInterp folds an unterminated `%` back into literal text rather
  // than erroring; nothing should light up as an interpolation here.
  test("an unterminated % is ordinary string text", () => {
    expect(tokens('"100% sure"')).toEqual([["dslString", '"100% sure"']]);
  });
});

describe("filters", () => {
  // pFilterStep: the identifier right after `|` names a filter.
  test("the name after a pipe is a filter, the next one is not", () => {
    expect(tokens("read x | latest(1) | orifempty y")).toEqual([
      ["dslKeyword", "read"],
      ["dslVar", "x"],
      ["dslOperator", "|"],
      ["dslFilter", "latest"],
      ["dslParen", "("],
      ["dslVar", "1"],
      ["dslParen", ")"],
      ["dslOperator", "|"],
      ["dslFilter", "orifempty"],
      ["dslVar", "y"],
    ]);
  });
});

describe("definitions and roles", () => {
  test("a name being assigned reads differently from one being referenced", () => {
    expect(tokens("blurb = read sheet.md\nother blurb")).toEqual([
      ["dslDef", "blurb"],
      ["dslOperator", "="],
      ["dslKeyword", "read"],
      ["dslVar", "sheet.md"],
      ["dslVar", "other"],
      ["dslVar", "blurb"],
    ]);
  });

  test("> and < are role markers, not plain operators", () => {
    expect(tokens('> "sure thing"\n< read notes.md')).toEqual([
      ["dslRole", ">"],
      ["dslString", '"sure thing"'],
      ["dslRole", "<"],
      ["dslKeyword", "read"],
      ["dslVar", "notes.md"],
    ]);
  });
});

describe("a whole program", () => {
  test("the shape a saved snippet actually has", () => {
    expect(tokens('as "the rules of magic":\n  read [lore/*.md] | summarized')).toEqual([
      ["dslKeyword", "as"],
      ["dslString", '"the rules of magic"'],
      ["dslOperator", ":"],
      ["dslKeyword", "read"],
      ["dslPath", "[lore/*.md]"],
      ["dslOperator", "|"],
      ["dslFilter", "summarized"],
    ]);
  });
});
