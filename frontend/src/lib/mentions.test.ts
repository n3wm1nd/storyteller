import { describe, expect, test } from "bun:test";
import { mentionSuggestions, resolveMentions } from "./mentions";
import type { LoreNode } from "./ws";

function lore(path: string, blurb = ""): LoreNode {
  return { path, name: path, blurb, aliases: [], children: [] };
}

describe("mentionSuggestions", () => {
  const entries = [lore("lore/alice.md"), lore("lore/albert.md"), lore("lore/bob.md")];

  test("no suggestions when the caret isn't in an '@' token", () => {
    expect(mentionSuggestions("hello world", 5, entries)).toEqual([]);
  });

  test("matches by basename prefix, case-insensitively", () => {
    const result = mentionSuggestions("hey @Al", 7, entries);
    expect(result.map((s) => s.display).sort()).toEqual(["@albert", "@alice"]);
  });

  test("insertText carries the full markup, display stays plain", () => {
    const result = mentionSuggestions("@alice", 6, entries);
    expect(result).toEqual([
      {
        replaceStart: 0,
        replaceEnd: 6,
        insertText: "@[alice](lore/alice.md) ",
        display: "@alice",
        description: "",
      },
    ]);
  });

  test("replaceStart/replaceEnd only cover the '@' token itself", () => {
    const result = mentionSuggestions("say hi to @al", 13, entries);
    expect(result[0].replaceStart).toBe(10);
    expect(result[0].replaceEnd).toBe(13);
  });
});

describe("resolveMentions", () => {
  test("plain text with no mentions passes through untouched", () => {
    expect(resolveMentions("just prose")).toEqual({ cleanText: "just prose", paths: [] });
  });

  test("strips markup back to a plain @Name and collects the path", () => {
    expect(resolveMentions("ask @[Alice](lore/alice.md) about the war")).toEqual({
      cleanText: "ask @Alice about the war",
      paths: ["lore/alice.md"],
    });
  });

  test("duplicate mentions of the same path are deduped in paths, kept in text", () => {
    const result = resolveMentions("@[Alice](lore/alice.md) met @[Alice](lore/alice.md) again");
    expect(result.cleanText).toBe("@Alice met @Alice again");
    expect(result.paths).toEqual(["lore/alice.md"]);
  });

  test("multiple distinct mentions preserve first-seen order", () => {
    const result = resolveMentions("@[Bob](lore/bob.md) and @[Alice](lore/alice.md)");
    expect(result.paths).toEqual(["lore/bob.md", "lore/alice.md"]);
  });
});
