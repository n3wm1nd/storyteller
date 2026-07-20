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
    expect(resolveMentions("just prose")).toBe("just prose");
  });

  test("strips markup back to a plain @Name", () => {
    expect(resolveMentions("ask @[Alice](lore/alice.md) about the war")).toBe("ask @Alice about the war");
  });

  test("repeated mentions of the same name are each stripped in place", () => {
    expect(resolveMentions("@[Alice](lore/alice.md) met @[Alice](lore/alice.md) again"))
      .toBe("@Alice met @Alice again");
  });
});
