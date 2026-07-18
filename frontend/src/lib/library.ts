// A standalone mirror of Storyteller.Writer.Library.classifyPath (Haskell)
// — see WRITER.md's "Story structure" for the authoritative rule, shared in
// prose between the two implementations, not in code. This exists because
// the Explorer tab (filetree.tsx's buildTree) only ever has a raw path
// list, no /library/{name} WS connection to ask — everywhere else that
// already has that connection (library.tsx) stays server-authoritative,
// rendering the LibraryNode/ChapterUnit tree the server computed, exactly
// as before. Misclassification here is exactly as low-stakes as it is
// server-side: a wrongly-tagged path still opens and edits fine, it just
// gets a generic icon instead of a book-page one.

export type LibraryKind = "folder" | "unit" | "unit-outline" | "other";

// Extension-based image detection — filetree.tsx's binary leaves have no
// richer classification than raw bytes to go on (see buildTree's isBinary),
// so "is this an image" is judged the same crude way a browser's own file
// picker would, by suffix alone.
const IMAGE_EXTENSIONS = new Set(["png", "jpg", "jpeg", "gif", "webp", "bmp", "svg"]);

export function isImagePath(path: string): boolean {
  const ext = path.split(".").pop()?.toLowerCase();
  return !!ext && IMAGE_EXTENSIONS.has(ext);
}

// Custom drag MIME carrying an existing branch file's path — set by
// filetree.tsx when an image leaf is dragged, read by fileview.tsx's
// WireTickList drop zone to fetch that file's bytes and attach them as a
// new Image tick, the same "drop a File" path already used for OS drags
// (see uploadImageToTimeline), just sourced from the branch instead of disk.
export const IMAGE_DRAG_MIME = "application/x-storyteller-image-path";

// Same fixed vocabulary as Storyteller.Writer.Library.storyMarkers — keep
// the two lists in sync if this ever changes (see WRITER.md).
const STORY_MARKERS = new Set([
  "story", "stories",
  "book", "books",
  "text", "texts",
  "chapter", "chapters", "ch",
  "scene", "scenes",
]);

// Every alpha-only "word" in a single path segment, lowercased — digits and
// punctuation both act as separators and simply vanish, so "ch1" splits to
// ["ch"] and "01 - the first book" splits to ["the", "first", "book"].
function segmentWords(segment: string): string[] {
  return segment
    .toLowerCase()
    .replace(/[^a-z]+/g, " ")
    .trim()
    .split(/\s+/)
    .filter(Boolean);
}

function isMarkerSegment(segment: string): boolean {
  return segmentWords(segment).some((w) => STORY_MARKERS.has(w));
}

function dropExtensions(basename: string): string {
  const idx = basename.indexOf(".");
  return idx === -1 ? basename : basename.slice(0, idx);
}

// Classify a single file path. See this module's header and WRITER.md for
// the algorithm; 'outline.md' / '{stem}.outline.md' are self-marking (the
// name itself is already an unambiguous declaration, same trust extended to
// every other reserved filename in this codebase), so they short-circuit
// the general marker-word scan rather than also needing a marker nearby.
export function classifyPath(path: string): LibraryKind {
  const parts = path.split("/").filter(Boolean);
  const base = parts[parts.length - 1] ?? path;
  const ancestors = parts.slice(0, -1);

  if (base === "outline.md" || base.endsWith(".outline.md")) return "unit-outline";

  const eligible = [...ancestors, dropExtensions(base)].some(isMarkerSegment);
  return eligible ? "unit" : "other";
}

// Mirrors Server.Writer.File.summaryKindsFor (Haskell) -- which summary
// tiers, if any, a file's raw/summary toggle should offer. Advisory only
// (see WS-PROTOCOL.md's "Backend-authoritative vs. frontend-advisory
// duplication"): the server alone decides what actually gets pushed as a
// synthetic "summary" tick (see Server.Writer.File.summaryTicksFor), so a
// stale or wrong guess here is only ever a UI nit -- offering a toggle
// button for a tier that turns out to have nothing pushed for this path,
// never a data problem.
const JOURNAL_PATH = "journal.md";

function isLoreEligiblePath(path: string): boolean {
  const parts = path.split("/").filter(Boolean);
  if (parts[0] === "chat") return false;
  const base = parts[parts.length - 1];
  const isRootFile = (name: string) => base === name && parts.length === 1;
  return classifyPath(path) === "other" && !isRootFile("sheet.md") && !isRootFile("journal.md");
}

// Mirrors Storyteller.Writer.Agent.JournalSummarizer.journalKindFor --
// journal.md's summary tree is a recursive, open-ended tower of tiers
// (tier 0 = raw entries, tier n = tier (n-1)'s own chunks), not two fixed
// kinds -- listed generously (12 tiers covers 10^11 raw entries at the
// default group size of 10) since SummaryAccess.zoomLevels stops at the
// first tier that doesn't actually exist yet.
function journalKindFor(level: number): string {
  return `journal/L${level}`;
}

export function summaryKindsFor(path: string): string[] {
  if (classifyPath(path) === "unit") return ["prose/chapter"];
  if (path === JOURNAL_PATH) return Array.from({ length: 12 }, (_, i) => journalKindFor(i));
  if (isLoreEligiblePath(path)) return ["lore/article"];
  return [];
}

// Natural-sort comparator, mirroring Storyteller.Writer.Library.naturalKey
// (Haskell) — "ch2" sorts before "ch11", "2 - the sequel" before "14 - the
// finale", by comparing alternating digit/non-digit runs, digit runs by
// numeric value rather than by string. Purely a comparator: nothing here
// attaches stored numeric identity to a path, same caveat as the Haskell
// side. Used by filetree.tsx's buildTree, mirroring the same ordering
// /library/{name} already applies server-side.
export function naturalCompare(a: string, b: string): number {
  const re = /(\d+)|(\D+)/g;
  const ta = a.match(re) ?? [];
  const tb = b.match(re) ?? [];
  const len = Math.max(ta.length, tb.length);
  for (let i = 0; i < len; i++) {
    const x = ta[i] ?? "";
    const y = tb[i] ?? "";
    if (x === y) continue;
    const nx = /^\d/.test(x) ? Number(x) : null;
    const ny = /^\d/.test(y) ? Number(y) : null;
    if (nx !== null && ny !== null) {
      if (nx !== ny) return nx - ny;
      continue;
    }
    // A numeric token and a text token never actually compare equal above,
    // so exactly one of these fires — mirrors the Haskell side's Either Int
    // Text 'Ord' (a numeric token always sorts before a text token at the
    // point two names diverge in kind, an arbitrary but deterministic tie-break).
    if (nx !== null) return -1;
    if (ny !== null) return 1;
    return x < y ? -1 : 1;
  }
  return 0;
}
