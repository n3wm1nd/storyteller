import { describe, expect, test } from "bun:test";
import {
  DEFAULT_EDITS,
  synthesizeLoreOverride,
  composeWriterContextFields,
  isDirty,
  renderLoreProgram,
  parseLoreProgram,
  toggleLorePathInProgram,
  type CallContext,
  type ContextEdits,
} from "./dslCompose";

function dirtyEdits(patch: Partial<ContextEdits>): ContextEdits {
  return { ...DEFAULT_EDITS, ...patch };
}

describe("isDirty", () => {
  test("false for untouched defaults", () => {
    expect(isDirty(DEFAULT_EDITS)).toBe(false);
  });

  test("true when lore is toggled off", () => {
    expect(isDirty(dirtyEdits({ loreEnabled: false }))).toBe(true);
  });

  test("true when pastChaptersMode is compressed", () => {
    expect(isDirty(dirtyEdits({ pastChaptersMode: "compressed" }))).toBe(true);
  });

  test("true when a pinned program is added", () => {
    expect(isDirty(dirtyEdits({ pinnedProgramNames: ["rules.magic"] }))).toBe(true);
  });
});

describe("synthesizeLoreOverride", () => {
  test("null when lore is untouched (omit the wire field, server default runs)", () => {
    expect(synthesizeLoreOverride(DEFAULT_EDITS)).toBeNull();
  });

  test("a real 0-arity program when lore is disabled", () => {
    const program = synthesizeLoreOverride(dirtyEdits({ loreEnabled: false }));
    expect(program).not.toBeNull();
  });
});

describe("renderLoreProgram / parseLoreProgram round-trip", () => {
  test("empty selection round-trips", () => {
    const program = renderLoreProgram([]);
    expect(parseLoreProgram(program)).toEqual([]);
  });

  test("a chosen path list round-trips exactly", () => {
    const paths = ["lore/alice.md", "lore/battle-log.md"];
    const program = renderLoreProgram(paths);
    expect(parseLoreProgram(program)).toEqual(paths);
  });

  test("a single path with spaces, commas, and quotes round-trips (no escaping needed -- ']' is the only real boundary)", () => {
    const paths = ['lore/file with spaces, "quotes".md'];
    const program = renderLoreProgram(paths);
    expect(parseLoreProgram(program)).toEqual(paths);
  });

  test("text freely appended after the generated prefix still parses (trailing text is preserved, not rejected)", () => {
    const program = renderLoreProgram(["lore/a.md"]);
    expect(parseLoreProgram(program + "\nread \"extra.md\"\n")).toEqual(["lore/a.md"]);
  });

  test("any real DSL source with no recognizable generated header parses as zero-selection (a valid, trivial prefix match)", () => {
    // The empty-selection form IS the empty string, so it's a prefix of
    // any text -- this is what makes an untouched project default
    // (contextLoreDef's own glob-walking body, say) show "0 checked"
    // rather than disabling the checkboxes outright.
    expect(parseLoreProgram("context.lore\n")).toEqual([]);
    expect(parseLoreProgram("for f in lore/**/*:\n  read f\n")).toEqual([]);
  });

  test("text that shares the generated header but isn't the generated shape reads as zero-selection, not a conflict", () => {
    // The header alone (renderLoreProgram([]) is the empty string, not
    // the header) isn't "claimed" territory -- real default source
    // (contextLoreDef) legitimately starts with this same banner line
    // for unrelated reasons, and must not be treated as broken/disabled.
    expect(parseLoreProgram('"## Story background"\nnot the generated body at all\n')).toEqual([]);
    expect(parseLoreProgram('"## Story background"\nfor f in lore/**/*:\n  x = loreEntry f\n  as f: x\n  x\n')).toEqual([]);
  });

  test("checking every real entry (all-selected) is distinguishable from none-selected", () => {
    const none = renderLoreProgram([]);
    const all = renderLoreProgram(["lore/a.md", "lore/b.md"]);
    expect(none).not.toBe(all);
    expect(parseLoreProgram(none)).toEqual([]);
    expect(parseLoreProgram(all)).toEqual(["lore/a.md", "lore/b.md"]);
  });
});

describe("toggleLorePathInProgram", () => {
  test("adds a path to an empty selection", () => {
    const next = toggleLorePathInProgram("", "lore/a.md");
    expect(parseLoreProgram(next)).toEqual(["lore/a.md"]);
  });

  test("removes an already-checked path", () => {
    const program = renderLoreProgram(["lore/a.md", "lore/b.md"]);
    const next = toggleLorePathInProgram(program, "lore/a.md");
    expect(parseLoreProgram(next)).toEqual(["lore/b.md"]);
  });

  test("toggling against real (non-checkbox) DSL source starts a fresh selection, leaving that source as trailing text", () => {
    const defaultSource = "for f in lore/**/*:\n  read f\n";
    const next = toggleLorePathInProgram(defaultSource, "lore/a.md");
    expect(parseLoreProgram(next)).toEqual(["lore/a.md"]);
    expect(next).toContain(defaultSource);
  });

  test("toggling against the real default source, which shares the generator's own banner line, still starts a fresh selection", () => {
    const defaultSource = '"## Story background"\nfor f in lore/**/*:\n  x = loreEntry f\n  as f: x\n  x\n';
    const next = toggleLorePathInProgram(defaultSource, "lore/a.md");
    expect(parseLoreProgram(next)).toEqual(["lore/a.md"]);
    expect(next).toContain(defaultSource);
  });

  test("toggling a path a hand-added suffix already references still preserves that suffix untouched", () => {
    const program = renderLoreProgram(["lore/a.md"]) + "\nread \"extra.md\"\n";
    const next = toggleLorePathInProgram(program, "lore/b.md");
    expect(parseLoreProgram(next)).toEqual(["lore/a.md", "lore/b.md"]);
    expect(next).toContain('read "extra.md"');
  });
});

describe("composeWriterContextFields", () => {
  const base: CallContext = {
    path: "chapters/ch3.md",
    edits: DEFAULT_EDITS,
    mentionCharacterIds: [],
  };

  test("no fields at all when nothing is touched and no mentions", () => {
    const fields = composeWriterContextFields(base);
    expect(fields.lore).toBeUndefined();
    expect(fields.pastChaptersMode).toBeUndefined();
    expect(fields.pinnedPrograms).toBeUndefined();
  });

  test("pastChaptersMode is only sent when non-default", () => {
    const fields = composeWriterContextFields({ ...base, edits: dirtyEdits({ pastChaptersMode: "compressed" }) });
    expect(fields.pastChaptersMode).toBe("compressed");
  });

  test("pinnedProgramNames become pinnedPrograms verbatim", () => {
    const fields = composeWriterContextFields({ ...base, edits: dirtyEdits({ pinnedProgramNames: ["rules.magic"] }) });
    expect(fields.pinnedPrograms).toContain("rules.magic");
  });

  test("a mention overlay folds into pinnedPrograms alongside named pins", () => {
    const fields = composeWriterContextFields({
      ...base,
      edits: dirtyEdits({ pinnedProgramNames: ["rules.magic"] }),
      mentionCharacterIds: ["aria"],
    });
    expect(fields.pinnedPrograms?.length).toBe(2);
    expect(fields.pinnedPrograms).toContain("rules.magic");
    expect(fields.pinnedPrograms?.some((p) => p.includes("aria"))).toBe(true);
  });

  test("lore override present only when lore has been touched", () => {
    const fields = composeWriterContextFields({ ...base, edits: dirtyEdits({ loreEnabled: false }) });
    expect(fields.lore).not.toBeUndefined();
  });
});
