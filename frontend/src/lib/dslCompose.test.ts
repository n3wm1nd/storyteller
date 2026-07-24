import { describe, expect, test } from "bun:test";
import {
  DEFAULT_EDITS,
  synthesizeLoreOverride,
  composeWriterContextFields,
  isDirty,
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
