// Parsing for SillyTavern-format character cards ("Tavern Card" V1/V2/V3),
// dropped as either a plain JSON export or a PNG with the card embedded as
// metadata. Runs entirely client-side — the server never sees raw card
// bytes, only the already-mapped file content 'buildCharacterFiles' below
// produces (see Server.Writer.Session.Protocol's ImportCharacterCard). No
// library: PNG's chunk format is a handful of lines to walk directly, and
// the encoding is simple enough (a plain tEXt chunk, keyword "chara" or
// "ccv3", base64-encoded JSON) not to need one either.

export interface ParsedCard {
  name: string;
  description?: string;
  personality?: string;
  scenario?: string;
  creatorNotes?: string;
  creator?: string;
  tags?: string[];
  firstMes?: string;
  mesExample?: string;
  systemPrompt?: string;
  postHistoryInstructions?: string;
  alternateGreetings?: string[];
  characterBook?: {
    name?: string;
    entries: { keys: string[]; content: string; comment?: string }[];
  };
}

// ── PNG chunk extraction ────────────────────────────────────────────────────

const PNG_SIGNATURE = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a];

// Walks a PNG's chunk structure (8-byte signature, then repeated
// [u32 length][4-byte type][data][u32 crc]) looking for a tEXt chunk keyed
// "ccv3" (V3, preferred) or "chara" (V2) — the two keywords
// SillyTavern's own writer uses (src/character-card-parser.js). Returns the
// chunk's raw text payload (still base64-encoded JSON at this point), or
// null if neither is present.
export function extractPngCardPayload(buffer: ArrayBuffer): string | null {
  const bytes = new Uint8Array(buffer);
  for (let i = 0; i < PNG_SIGNATURE.length; i++) {
    if (bytes[i] !== PNG_SIGNATURE[i]) throw new Error("not a PNG file");
  }

  const view = new DataView(buffer);
  let offset = 8;
  let chara: string | null = null;
  let ccv3: string | null = null;

  while (offset + 8 <= bytes.length) {
    const length = view.getUint32(offset);
    const type = String.fromCharCode(bytes[offset + 4], bytes[offset + 5], bytes[offset + 6], bytes[offset + 7]);
    const dataStart = offset + 8;
    if (type === "tEXt") {
      const chunk = bytes.subarray(dataStart, dataStart + length);
      const nul = chunk.indexOf(0);
      if (nul !== -1) {
        const keyword = String.fromCharCode(...chunk.subarray(0, nul)).toLowerCase();
        const text = String.fromCharCode(...chunk.subarray(nul + 1));
        if (keyword === "chara") chara = text;
        if (keyword === "ccv3") ccv3 = text;
      }
    }
    if (type === "IEND") break;
    offset = dataStart + length + 4; // skip data + CRC
  }

  const payload = ccv3 ?? chara;
  return payload === null ? null : decodeBase64Utf8(payload);
}

// atob() decodes base64 into a "binary string" -- one JS char per raw byte,
// each code point in 0..255 -- not text. The card JSON is UTF-8, so any
// multi-byte character (curly quotes, accented letters, ...) is still
// UTF-8-encoded across several of those one-byte "characters" at this
// point; interpreting the binary string directly as the final text (as if
// it were Latin-1) is exactly what mangled "5'4"" into "5â€™4" before this
// fix. Re-decoding the byte sequence as UTF-8 turns it back into real text.
function decodeBase64Utf8(base64: string): string {
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return new TextDecoder("utf-8").decode(bytes);
}

// ── Card JSON normalization ─────────────────────────────────────────────────

// Normalizes whatever shape came out of the file — V1 (flat), V2/V3 (fields
// under `data`, spec-tagged) — into one 'ParsedCard'. SillyTavern always
// writes V1-shaped top-level fields alongside `data` for backward
// compatibility (see its own char-data.js), so a V2/V3 card's `data` block
// takes precedence when present, falling back to the flat fields otherwise.
export function parseCardJson(raw: string): ParsedCard {
  const parsed = JSON.parse(raw);
  const data = parsed.data ?? parsed;

  const name: string | undefined = data.name ?? parsed.name;
  if (!name || typeof name !== "string") throw new Error("character card has no name");

  return {
    name,
    description: asText(data.description),
    personality: asText(data.personality),
    scenario: asText(data.scenario),
    creatorNotes: asText(data.creator_notes ?? parsed.creatorcomment),
    creator: asText(data.creator),
    tags: Array.isArray(data.tags) ? data.tags.filter((t: unknown) => typeof t === "string") : undefined,
    firstMes: asText(data.first_mes ?? parsed.first_mes),
    mesExample: asText(data.mes_example ?? parsed.mes_example),
    systemPrompt: asText(data.system_prompt),
    postHistoryInstructions: asText(data.post_history_instructions),
    alternateGreetings: Array.isArray(data.alternate_greetings)
      ? data.alternate_greetings.filter((g: unknown) => typeof g === "string")
      : undefined,
    characterBook: normalizeCharacterBook(data.character_book),
  };
}

function asText(v: unknown): string | undefined {
  return typeof v === "string" && v.trim().length > 0 ? v : undefined;
}

function normalizeCharacterBook(book: unknown): ParsedCard["characterBook"] {
  if (!book || typeof book !== "object" || !Array.isArray((book as { entries?: unknown }).entries)) return undefined;
  const b = book as { name?: unknown; entries: unknown[] };
  const entries = b.entries
    .map((e) => {
      if (!e || typeof e !== "object") return null;
      const entry = e as { keys?: unknown; content?: unknown; comment?: unknown };
      const content = asText(entry.content);
      if (!content) return null;
      const keys = Array.isArray(entry.keys) ? entry.keys.filter((k): k is string => typeof k === "string") : [];
      return { keys, content, comment: asText(entry.comment) };
    })
    .filter((e): e is { keys: string[]; content: string; comment?: string } => e !== null);
  if (entries.length === 0) return undefined;
  return { name: asText(b.name), entries };
}

// ── Card -> branch files ────────────────────────────────────────────────────

export function slugify(name: string): string {
  const slug = name
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
  return slug || "character";
}

// Maps a parsed card onto the character branch's own file conventions (see
// WRITER.md): identity/personality prose goes to sheet.md, the
// chat-persona-flavored fields ST cards carry (opening message, example
// dialogue, system/post-history instructions) go to instructions.md for a
// future roleplay-flavored agent to pick up by convention, and an embedded
// lorebook — when present — becomes its own lore file rather than being
// force-fit into either. Sections with no source content are omitted
// entirely rather than left as empty headings.
export function buildCharacterFiles(card: ParsedCard): { branch: string; files: { path: string; content: string }[] } {
  const branch = `character/${slugify(card.name)}`;
  const files: { path: string; content: string }[] = [{ path: "sheet.md", content: buildSheet(card) }];

  const instructions = buildInstructions(card);
  if (instructions) files.push({ path: "instructions.md", content: instructions });

  const lore = buildLore(card);
  if (lore) files.push({ path: "lore.md", content: lore });

  return { branch, files };
}

function buildSheet(card: ParsedCard): string {
  const sections = [`# ${card.name}`];
  if (card.description) sections.push(card.description);
  if (card.personality) sections.push(`## Personality\n\n${card.personality}`);
  if (card.scenario) sections.push(`## Scenario\n\n${card.scenario}`);
  if (card.tags && card.tags.length > 0) sections.push(`Tags: ${card.tags.join(", ")}`);
  if (card.creatorNotes) sections.push(`## Creator Notes\n\n${card.creatorNotes}`);
  if (card.creator) sections.push(`_Imported from a SillyTavern character card by ${card.creator}._`);
  return sections.join("\n\n") + "\n";
}

function buildInstructions(card: ParsedCard): string | null {
  const sections: string[] = [];
  if (card.systemPrompt) sections.push(`## System Prompt\n\n${card.systemPrompt}`);
  if (card.firstMes) sections.push(`## Opening Message\n\n${card.firstMes}`);
  if (card.alternateGreetings && card.alternateGreetings.length > 0) {
    sections.push(`## Alternate Openings\n\n${card.alternateGreetings.map((g) => `- ${g}`).join("\n")}`);
  }
  if (card.mesExample) sections.push(`## Voice / Example Dialogue\n\n${card.mesExample}`);
  if (card.postHistoryInstructions) sections.push(`## Post-History Instructions\n\n${card.postHistoryInstructions}`);
  if (sections.length === 0) return null;
  return `# ${card.name} — Instructions\n\n${sections.join("\n\n")}\n`;
}

function buildLore(card: ParsedCard): string | null {
  if (!card.characterBook) return null;
  const title = card.characterBook.name ?? `${card.name}'s World`;
  const entries = card.characterBook.entries.map((e) => {
    const heading = e.comment ?? (e.keys.length > 0 ? e.keys.join(", ") : "Entry");
    return `## ${heading}\n\n${e.content}`;
  });
  return `# ${title}\n\n${entries.join("\n\n")}\n`;
}
