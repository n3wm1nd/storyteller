import { describe, expect, test } from "bun:test";
import { triggeredLorePaths } from "./loreTrigger";
import type { LoreNode } from "./ws";

function lore(path: string, aliases: string[] = [], children: LoreNode[] = []): LoreNode {
  return { path, name: path, blurb: "", aliases, children };
}

describe("triggeredLorePaths", () => {
  const tree = [
    lore("lore/alice.md", ["Al"]),
    lore("lore/folder", [], [lore("lore/folder/bob.md", ["Bobby"])]),
  ];

  test("no triggered paths configured yields nothing, regardless of text", () => {
    expect(triggeredLorePaths("Alice said hi", tree, new Set())).toEqual([]);
  });

  test("empty text yields nothing even with triggers configured", () => {
    expect(triggeredLorePaths("", tree, new Set(["lore/alice.md"]))).toEqual([]);
  });

  test("matches on the entry's own basename", () => {
    expect(triggeredLorePaths("alice walked in", tree, new Set(["lore/alice.md"]))).toEqual(["lore/alice.md"]);
  });

  test("matches on an alias too", () => {
    expect(triggeredLorePaths("Al waved", tree, new Set(["lore/alice.md"]))).toEqual(["lore/alice.md"]);
  });

  test("matches whole words only, not substrings", () => {
    expect(triggeredLorePaths("Alicia was there", tree, new Set(["lore/alice.md"]))).toEqual([]);
  });

  test("only scans entries actually present in the triggered set", () => {
    expect(triggeredLorePaths("bob and bobby were there", tree, new Set(["lore/alice.md"]))).toEqual([]);
  });

  test("walks nested folders via flattenLore", () => {
    expect(triggeredLorePaths("Bobby ran off", tree, new Set(["lore/folder/bob.md"]))).toEqual([
      "lore/folder/bob.md",
    ]);
  });

  test("a name containing regex metacharacters doesn't throw or misfire", () => {
    const special = [lore("lore/what?.md", ["what?"])];
    expect(triggeredLorePaths("what? he asked", special, new Set(["lore/what?.md"]))).toEqual(["lore/what?.md"]);
  });
});
