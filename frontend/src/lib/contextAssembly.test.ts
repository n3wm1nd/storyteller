import { describe, expect, test } from "bun:test";
import { composeContextLayout } from "./contextAssembly";

describe("composeContextLayout", () => {
  test("empty base layout means 'show everything' — forced paths add nothing", () => {
    expect(composeContextLayout([], ["lore/alice.md"])).toEqual([]);
  });

  test("no forced paths leaves a curated layout untouched", () => {
    const base = [{ pattern: "chapters/**", bucket: 2 }];
    expect(composeContextLayout(base, [])).toEqual(base);
  });

  test("forced paths are prepended at bucket 1, ahead of the curated layout", () => {
    const base = [{ pattern: "chapters/**", bucket: 2 }];
    expect(composeContextLayout(base, ["lore/alice.md"])).toEqual([
      { pattern: "lore/alice.md", bucket: 1 },
      { pattern: "chapters/**", bucket: 2 },
    ]);
  });

  test("multiple forced paths keep their given order, all ahead of the base layout", () => {
    const base = [{ pattern: "**/*", bucket: 3 }];
    const result = composeContextLayout(base, ["a.md", "b.md"]);
    expect(result).toEqual([
      { pattern: "a.md", bucket: 1 },
      { pattern: "b.md", bucket: 1 },
      { pattern: "**/*", bucket: 3 },
    ]);
  });
});
