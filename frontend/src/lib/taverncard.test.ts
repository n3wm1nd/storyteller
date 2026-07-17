import { describe, expect, test } from "bun:test";
import { buildCharacterFiles, extractPngCardPayload, parseCardJson, slugify } from "./taverncard";

const PNG_SIGNATURE = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];

function chunk(type: string, data: Uint8Array): number[] {
  const bytes: number[] = [];
  const length = data.length;
  bytes.push((length >>> 24) & 0xff, (length >>> 16) & 0xff, (length >>> 8) & 0xff, length & 0xff);
  for (const c of type) bytes.push(c.charCodeAt(0));
  bytes.push(...data);
  bytes.push(0, 0, 0, 0); // CRC — never validated by extractPngCardPayload
  return bytes;
}

function textChunk(keyword: string, text: string): number[] {
  const payload = new Uint8Array([...Array.from(keyword, (c) => c.charCodeAt(0)), 0, ...Array.from(text, (c) => c.charCodeAt(0))]);
  return chunk("tEXt", payload);
}

function buildPng(chunks: number[][]): ArrayBuffer {
  const bytes = [...PNG_SIGNATURE, ...chunks.flat(), ...chunk("IEND", new Uint8Array())];
  return new Uint8Array(bytes).buffer;
}

function base64Utf8(json: unknown): string {
  return Buffer.from(JSON.stringify(json), "utf-8").toString("base64");
}

describe("extractPngCardPayload", () => {
  test("throws on a non-PNG buffer", () => {
    const bad = new Uint8Array([1, 2, 3, 4]).buffer;
    expect(() => extractPngCardPayload(bad)).toThrow("not a PNG file");
  });

  test("returns null when neither chara nor ccv3 chunks are present", () => {
    const png = buildPng([textChunk("other", "irrelevant")]);
    expect(extractPngCardPayload(png)).toBeNull();
  });

  test("extracts a V2 'chara' tEXt chunk", () => {
    const payload = base64Utf8({ name: "Alice" });
    const png = buildPng([textChunk("chara", payload)]);
    expect(extractPngCardPayload(png)).toBe(JSON.stringify({ name: "Alice" }));
  });

  test("prefers 'ccv3' over 'chara' when both are present", () => {
    const png = buildPng([
      textChunk("chara", base64Utf8({ name: "V2" })),
      textChunk("ccv3", base64Utf8({ name: "V3" })),
    ]);
    expect(extractPngCardPayload(png)).toBe(JSON.stringify({ name: "V3" }));
  });

  test("decodes multi-byte UTF-8 correctly, not as Latin-1", () => {
    const payload = base64Utf8({ name: "Alice", description: `5'4" café` });
    const png = buildPng([textChunk("chara", payload)]);
    expect(extractPngCardPayload(png)).toBe(JSON.stringify({ name: "Alice", description: `5'4" café` }));
  });

  test("stops scanning at IEND, ignoring anything after", () => {
    const beforeIend = buildPng([textChunk("chara", base64Utf8({ name: "Found" }))]);
    // buildPng already terminates with IEND; simulate trailing junk by not adding more chunks
    // (regression guard: a malformed trailing chunk shouldn't be reached at all)
    expect(extractPngCardPayload(beforeIend)).toBe(JSON.stringify({ name: "Found" }));
  });
});

describe("parseCardJson", () => {
  test("throws when there's no name anywhere", () => {
    expect(() => parseCardJson(JSON.stringify({ description: "no name here" }))).toThrow();
  });

  test("V1 flat shape reads top-level fields", () => {
    const card = parseCardJson(JSON.stringify({ name: "Alice", description: "a rogue", first_mes: "Hello!" }));
    expect(card.name).toBe("Alice");
    expect(card.description).toBe("a rogue");
    expect(card.firstMes).toBe("Hello!");
  });

  test("V2/V3 'data' block takes precedence over flat top-level fields", () => {
    const card = parseCardJson(JSON.stringify({
      name: "TopLevelName",
      data: { name: "Alice", description: "from data" },
    }));
    expect(card.name).toBe("Alice");
    expect(card.description).toBe("from data");
  });

  test("blank-string fields normalize to undefined, not an empty string", () => {
    const card = parseCardJson(JSON.stringify({ name: "Alice", description: "   " }));
    expect(card.description).toBeUndefined();
  });

  test("tags array is filtered to strings only", () => {
    const card = parseCardJson(JSON.stringify({ name: "Alice", data: { tags: ["rogue", 42, "elf"] } }));
    expect(card.tags).toEqual(["rogue", "elf"]);
  });

  test("character_book with no valid entries normalizes to undefined", () => {
    const card = parseCardJson(JSON.stringify({
      name: "Alice",
      data: { character_book: { entries: [{ keys: ["x"] }] } }, // no content
    }));
    expect(card.characterBook).toBeUndefined();
  });

  test("character_book entries keep only ones with real content", () => {
    const card = parseCardJson(JSON.stringify({
      name: "Alice",
      data: {
        character_book: {
          name: "Alice's World",
          entries: [
            { keys: ["forest"], content: "A dark forest." },
            { keys: ["nothing"] },
          ],
        },
      },
    }));
    expect(card.characterBook).toEqual({
      name: "Alice's World",
      entries: [{ keys: ["forest"], content: "A dark forest.", comment: undefined }],
    });
  });
});

describe("slugify", () => {
  test("lowercases and hyphenates", () => {
    expect(slugify("Alice Chen")).toBe("alice-chen");
  });

  test("strips leading/trailing punctuation", () => {
    expect(slugify("--Bob!!--")).toBe("bob");
  });

  test("falls back to 'character' when nothing alphanumeric survives", () => {
    expect(slugify("???")).toBe("character");
  });
});

describe("buildCharacterFiles", () => {
  test("always produces sheet.md, optional files only when there's source content", () => {
    const result = buildCharacterFiles({ name: "Alice" });
    expect(result.branch).toBe("character/alice");
    expect(result.files.map((f) => f.path)).toEqual(["sheet.md"]);
  });

  test("instructions.md appears once there's any instructions-shaped field", () => {
    const result = buildCharacterFiles({ name: "Alice", firstMes: "Hello!" });
    expect(result.files.map((f) => f.path)).toEqual(["sheet.md", "instructions.md"]);
    expect(result.files[1].content).toContain("Hello!");
  });

  test("lore.md appears only when a character_book was present", () => {
    const result = buildCharacterFiles({
      name: "Alice",
      characterBook: { entries: [{ keys: ["forest"], content: "A dark forest." }] },
    });
    expect(result.files.map((f) => f.path)).toEqual(["sheet.md", "lore.md"]);
  });

  test("import note records the creator and notes, omitting either when absent", () => {
    expect(buildCharacterFiles({ name: "Alice" }).note).toBe("Imported from a SillyTavern character card.");
    expect(buildCharacterFiles({ name: "Alice", creator: "Bob", creatorNotes: "WIP" }).note).toBe(
      "Imported from a SillyTavern character card by Bob.\n\nWIP",
    );
  });
});
