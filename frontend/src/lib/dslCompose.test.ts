import { describe, expect, test } from "bun:test";
import {
  DEFAULT_EDITS,
  composeSendProgram,
  synthesizeProgram,
  type CallContext,
  type ContextEdits,
} from "./dslCompose";

// The bug this file exists to pin down: `context.writer` is a 1-arity
// DSL definition (`path: ...`), and `Storyteller.Core.Context.
// resolveContextOverride` silently discards any staged override whose
// own declared arity doesn't match -- no error anywhere, it just falls
// back to the compiled-in default. Every program this module produces
// has to parse to exactly one parameter (`path:` as its first line) or
// it's a no-op server-side, indistinguishable from working.
function assertOneArityWrapper(program: string | null) {
  expect(program).not.toBeNull();
  const lines = (program as string).split("\n");
  expect(lines[0]).toBe("path:");
  // every following non-blank line has to be indented under that one
  // parameter -- a stray top-level statement would itself parse as a
  // second (0-arity) definition, not part of this one's body.
  for (const l of lines.slice(1)) {
    if (l.length === 0) continue;
    expect(l.startsWith(" ")).toBe(true);
  }
}

function dirtyEdits(patch: Partial<ContextEdits>): ContextEdits {
  return { ...DEFAULT_EDITS, ...patch };
}

describe("synthesizeProgram", () => {
  test("returns null when edits are untouched (nothing to send, server default runs)", () => {
    expect(synthesizeProgram(DEFAULT_EDITS)).toBeNull();
  });

  test("wraps the synthesized body in a 1-arity `path:` frame", () => {
    const program = synthesizeProgram(dirtyEdits({ baseline: { lore: true, chapters: true, style: false } }));
    assertOneArityWrapper(program);
  });

  test("excludes the current path from chapters, mirroring contextWriterDef", () => {
    const program = synthesizeProgram(dirtyEdits({ baseline: { lore: true, chapters: true, style: false } }));
    expect(program).toContain("exclude(path)");
  });

  // The bug underneath the arity bug: `as "name": expr` only ever
  // contributes a *named entry*, never the enclosing block's own
  // default -- and `renderText`/`renderMessages` (what the LLM actually
  // sees) read only that default, never named entries. Wrapping
  // lore/chapters/style in `as "bucket": ...` (as this module used to)
  // would produce a program that parses, resolves, and overrides
  // correctly, yet sends the model *nothing* -- indistinguishable from
  // working right up until someone reads the actual prompt. These three
  // must be bare statements at the program's own top level (indented
  // exactly once under `path:`), not nested under an `as` block.
  test("lore/chapters/style are bare statements, not named-only (so they actually reach the model)", () => {
    // baseline matches the untouched default (all three on) -- dirtied
    // via an unrelated field so isDirty is true without changing what
    // lore/chapters/style themselves synthesize to.
    const program = synthesizeProgram(
      dirtyEdits({ baseline: { lore: true, chapters: true, style: true }, excludedLorePaths: ["x"] }),
    ) as string;
    const lines = program.split("\n");
    expect(lines).toContain("  context.lore");
    expect(lines).toContain("  context.style");
    expect(lines).toContain("  in (context.chapters | exclude(path)):");
    // and specifically NOT wrapped in a labelling `as` block:
    expect(program).not.toContain(`as "lore":`);
    expect(program).not.toContain(`as "chapters":`);
    expect(program).not.toContain(`as "style":`);
  });

  // The same bug, for content a user explicitly opted into -- here it's
  // the opposite requirement: a manually added character/file has no
  // other mechanism showing it to the model at all, so it must be BOTH
  // bare (reaches the model) and named (individually addressable), via
  // the `x = ...; as "name": x; x` idiom.
  test("a manually added character is both bare-emitted and named, not named-only", () => {
    const program = synthesizeProgram(
      dirtyEdits({ characters: [{ id: "aria", depth: "blurb" }] }),
    ) as string;
    const lines = program.split("\n").map((l) => l.trim());
    expect(lines).toContain(`as "aria": x`);
    // a bare re-emit of the same binding, its own separate statement:
    expect(lines.filter((l) => l === "x").length).toBeGreaterThan(0);
  });

  test("a manually added extra file is both bare-emitted and named, not named-only", () => {
    const program = synthesizeProgram(
      dirtyEdits({ extraFiles: [{ path: "notes/battle-plan.md" }] }),
    ) as string;
    const lines = program.split("\n").map((l) => l.trim());
    expect(lines).toContain(`as "battle-plan": x`);
    expect(lines.filter((l) => l === "x").length).toBeGreaterThan(0);
  });

  test("with no manual cast list, falls back to the presence-driven charactersin loop", () => {
    const program = synthesizeProgram(dirtyEdits({ baseline: { lore: false, chapters: false, style: true } }));
    expect(program).toContain("for c in (charactersin path):");
    expect(program).toContain("as c: context.character c");
  });

  test("a manual cast list replaces the presence-driven loop entirely, not merges with it", () => {
    const program = synthesizeProgram(
      dirtyEdits({ characters: [{ id: "aria", depth: "blurb" }] }),
    );
    expect(program).toContain(`as "aria":`);
    expect(program).not.toContain("charactersin");
  });

  test("a 'full' depth character reads sheet/full/journal, not just the blurb default", () => {
    const program = synthesizeProgram(
      dirtyEdits({ characters: [{ id: "aria", depth: "full" }] }),
    );
    expect(program).toContain(`read "sheet"`);
    expect(program).toContain(`read "full"`);
    expect(program).toContain(`read "journal"`);
  });
});

describe("composeSendProgram", () => {
  const base: CallContext = {
    path: "chapters/ch3.md",
    edits: DEFAULT_EDITS,
    mentionCharacterIds: [],
    namedName: null,
  };

  test("returns null when nothing is edited, no mentions, no named function", () => {
    expect(composeSendProgram(base)).toBeNull();
  });

  test("a named function is called with `path`, itself wrapped in a 1-arity frame", () => {
    const program = composeSendProgram({ ...base, namedName: "myTemplate" });
    assertOneArityWrapper(program);
    expect(program).toContain("myTemplate path");
  });

  test("a mention overlay alone still produces a valid 1-arity program", () => {
    const program = composeSendProgram({ ...base, mentionCharacterIds: ["aria"] });
    assertOneArityWrapper(program);
    expect(program).toContain(`as "@aria":`);
  });

  test("edits and mention overlay share one path: frame, not two separate programs", () => {
    const program = composeSendProgram({
      ...base,
      edits: dirtyEdits({ baseline: { lore: true, chapters: false, style: false } }),
      mentionCharacterIds: ["aria"],
    });
    assertOneArityWrapper(program);
    expect((program as string).match(/^path:/gm)?.length).toBe(1);
    expect(program).toContain("context.lore");
    expect(program).toContain(`as "@aria":`);
  });
});
