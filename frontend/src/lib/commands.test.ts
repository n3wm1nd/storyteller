import { describe, expect, test } from "bun:test";
import { commandSuggestions, currentToken, parseCommand, type CommandDef } from "./commands";

describe("currentToken", () => {
  test("scans back to the nearest whitespace", () => {
    expect(currentToken("hello wor", 9)).toEqual({ start: 6, word: "wor" });
  });

  test("empty at start of input", () => {
    expect(currentToken("", 0)).toEqual({ start: 0, word: "" });
  });

  test("caret mid-token only sees the prefix, not the whole word", () => {
    expect(currentToken("/write", 3)).toEqual({ start: 0, word: "/wr" });
  });
});

describe("parseCommand", () => {
  test("no leading slash is not a command", () => {
    expect(parseCommand("just some text")).toBeNull();
  });

  test("bare command with no params", () => {
    expect(parseCommand("/write")).toEqual({ name: "write", params: {}, text: "" });
  });

  test("bare flag param has no '='", () => {
    expect(parseCommand("/regen @beat")).toEqual({ name: "regen", params: { beat: "true" }, text: "" });
  });

  test("unquoted value param", () => {
    expect(parseCommand("/ask @character=alice how are you")).toEqual({
      name: "ask",
      params: { character: "alice" },
      text: "how are you",
    });
  });

  test("quoted value can contain spaces", () => {
    expect(parseCommand('/ask @character="alice chen" hello')).toEqual({
      name: "ask",
      params: { character: "alice chen" },
      text: "hello",
    });
  });

  test("params interleaved with free text collapse remaining whitespace", () => {
    const result = parseCommand("/inform @character=bob   the   war   has ended");
    expect(result).toEqual({ name: "inform", params: { character: "bob" }, text: "the war has ended" });
  });

  test("trailing param after text is still parsed out", () => {
    const result = parseCommand("/regen do the fight scene @beat");
    expect(result).toEqual({ name: "regen", params: { beat: "true" }, text: "do the fight scene" });
  });
});

const TEST_COMMANDS: CommandDef[] = [
  { name: "write", label: "Write", description: "continue", params: [] },
  {
    name: "ask", label: "Ask", description: "ask a character",
    params: [{ name: "character", description: "who to ask" }],
  },
];

describe("commandSuggestions", () => {
  test("suggests matching command names only at the start of input", () => {
    const result = commandSuggestions("/w", 2, TEST_COMMANDS);
    expect(result.map((s) => s.display)).toEqual(["/write"]);
  });

  test("no command suggestions once past the first token", () => {
    expect(commandSuggestions("hello /w", 8, TEST_COMMANDS)).toEqual([]);
  });

  test("param suggestions require a recognized leading command", () => {
    expect(commandSuggestions("/nope @c", 8, TEST_COMMANDS)).toEqual([]);
  });

  test("param suggestions are scoped to that command's own params", () => {
    const result = commandSuggestions("/ask @c", 7, TEST_COMMANDS);
    expect(result.map((s) => s.display)).toEqual(["@character"]);
  });

  test("a command with no params suggests nothing for '@'", () => {
    expect(commandSuggestions("/write @", 8, TEST_COMMANDS)).toEqual([]);
  });
});
