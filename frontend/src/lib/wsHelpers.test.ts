import { describe, expect, test } from "bun:test";
import { applyFileUpdate, applyUpdate, atRebase, isChatPreviewEvent, remapSet, remapTickId } from "./wsHelpers";
import type { FileCommand, WireTick } from "./ws";

function tick(id: string, parent: string | null, overrides: Partial<WireTick> = {}): WireTick {
  return { tickId: id, kind: "atom", refs: [], message: "", parent, ...overrides };
}

describe("applyUpdate", () => {
  test("upserts ticks without mutating the input map", () => {
    const before = { a: tick("a", null) };
    const after = applyUpdate(before, { type: "update", head: "b", ticks: [tick("b", "a")] });
    expect(before).toEqual({ a: tick("a", null) });
    expect(after).toEqual({ a: tick("a", null), b: tick("b", "a") });
  });

  test("an update entry overwrites an existing tick with the same id", () => {
    const before = { a: tick("a", null, { message: "old" }) };
    const after = applyUpdate(before, { type: "update", head: "a", ticks: [tick("a", null, { message: "new" })] });
    expect(after.a.message).toBe("new");
  });
});

describe("applyFileUpdate", () => {
  function summaryTick(id: string, kind: string): WireTick {
    return { tickId: id, kind: "summary", refs: [], message: "", parent: null, fields: { kind } };
  }

  test("drops a summary-kind tick that no longer appears in the push (deleted, or its kind rebased away)", () => {
    const before = { s1: summaryTick("s1", "prose/chapter"), a: tick("a", null) };
    // summaryTicksFor always resends the *complete* current summary set —
    // an update with no summary ticks at all means none currently exist.
    const after = applyFileUpdate(before, { type: "update", head: "a", ticks: [tick("a", null)] });
    expect(after).toEqual({ a: tick("a", null) });
  });

  test("keeps a summary-kind tick that's still present in the push", () => {
    const before = { s1: summaryTick("s1", "prose/chapter") };
    const after = applyFileUpdate(before, { type: "update", head: "a", ticks: [summaryTick("s1", "prose/chapter")] });
    expect(after).toEqual({ s1: summaryTick("s1", "prose/chapter") });
  });

  test("a real (non-summary) tick is never dropped just for being absent from one push", () => {
    const before = { a: tick("a", null) };
    const after = applyFileUpdate(before, { type: "update", head: "a", ticks: [] });
    expect(after).toEqual({ a: tick("a", null) });
  });

  test("replaces a stale summary tick with a newer one of the same kind, not both", () => {
    const before = { old: summaryTick("old", "prose/chapter") };
    const after = applyFileUpdate(before, { type: "update", head: "a", ticks: [summaryTick("new", "prose/chapter")] });
    expect(after).toEqual({ new: summaryTick("new", "prose/chapter") });
  });
});

describe("isChatPreviewEvent", () => {
  test.each([
    ["chat.preview.start", true],
    ["chat.preview", true],
    ["chat.preview.thinking", true],
    ["chat.preview.end", true],
    ["update", false],
    ["error", false],
  ] as const)("%s -> %s", (type, expected) => {
    expect(isChatPreviewEvent({ type })).toBe(expected);
  });
});

describe("remapTickId", () => {
  test("returns the mapped id when present", () => {
    expect(remapTickId(new Map([["old", "new"]]), "old")).toBe("new");
  });

  test("passes through unmapped ids unchanged", () => {
    expect(remapTickId(new Map(), "id")).toBe("id");
  });
});

describe("remapSet", () => {
  test("returns the same reference when nothing in the set is remapped", () => {
    const input = new Set(["a", "b"]);
    expect(remapSet(new Map([["c", "d"]]), input)).toBe(input);
  });

  test("returns a new set with ids remapped when something changes", () => {
    const input = new Set(["a", "b"]);
    const result = remapSet(new Map([["a", "a2"]]), input);
    expect(result).not.toBe(input);
    expect(result).toEqual(new Set(["a2", "b"]));
  });

  test("remapping two ids onto the same target still dedupes into one entry", () => {
    const result = remapSet(new Map([["a", "x"], ["b", "x"]]), new Set(["a", "b"]));
    expect(result).toEqual(new Set(["x"]));
  });
});

describe("atRebase", () => {
  const cmd: FileCommand = { type: "chat.writer", id: "cmd-1", text: "go on" };

  test("no marker set: command passes through unwrapped", () => {
    expect(atRebase(null, {}, cmd, {})).toBe(cmd);
  });

  test("marker set but missing from ticks: passes through unwrapped", () => {
    expect(atRebase("missing", {}, cmd, {})).toBe(cmd);
  });

  test("marker with no parent (root tick): passes through unwrapped", () => {
    const ticks = { m: tick("m", null) };
    expect(atRebase("m", ticks, cmd, {})).toBe(cmd);
  });

  test("wraps in 'at' using the marker's *parent* as the pivot, carrying the inner id up", () => {
    const ticks = { m: tick("m", "parent-of-m") };
    const result = atRebase("m", ticks, cmd, {});
    expect(result).toEqual({ type: "at", id: "cmd-1", tickId: "parent-of-m", command: cmd });
  });

  test("attaches only journal markers with a non-null position", () => {
    const ticks = { m: tick("m", "p") };
    const result = atRebase("m", ticks, cmd, { "character/alice": "j1", "character/bob": null });
    expect(result).toEqual({
      type: "at",
      id: "cmd-1",
      tickId: "p",
      command: cmd,
      branches: [{ branch: "character/alice", tickId: "j1" }],
    });
  });

  test("omits the 'branches' field entirely when nothing resolved", () => {
    const ticks = { m: tick("m", "p") };
    const result = atRebase("m", ticks, cmd, { "character/alice": null });
    expect(result).not.toHaveProperty("branches");
  });
});
