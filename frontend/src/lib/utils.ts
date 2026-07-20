import type { WireTick, LoreNode } from "./ws";
import { branchDisplayName } from "./branches";
import { summaryKindIsIncremental } from "./library";

export type AnnotationMode = "hidden" | "dots" | "expanded";

export function tickChain(ticks: Record<string, WireTick>, head: string | null): WireTick[] {
  if (!head || !ticks[head]) return [];
  const chain: WireTick[] = [];
  let cur: string | null = head;
  const seen = new Set<string>();
  while (cur && ticks[cur] && !seen.has(cur)) {
    seen.add(cur);
    const t: WireTick = ticks[cur];
    if (t.kind !== "root") chain.push(t);
    cur = t.parent;
  }
  return chain.reverse();
}

// The rebase marker is stored (see uiStore.rebaseMarker) as the tickId of
// the tick the bar is "stuck to" — the first tick of whatever's currently
// suppressed after it — not the tick to append at. That's the one
// definition that survives every write made through the marker for free:
// the suppressed tick is always exactly one of the ids an 'at'-wrapped
// command's own tick.remap reports as rebased (see wsHelpers.atRebase /
// fileview.actions.ts's handleTickRemap), so keeping it current is a plain
// table lookup — remapTickId(table, marker) — the same treatment
// contextAtoms/contextAnnotations already get, no special-casing. The tick
// to actually send as the 'at' command's own pivot is derived fresh at
// send time as that tick's *parent* (see wsHelpers.atRebase) — which is
// automatically wherever the most recent write landed, again with nothing
// to track: a marker that never itself gets rebased (nothing written
// through it yet) has a pivot equal to its original parent; the moment
// something is written there, that same tick's parent has changed instead
// (new commit, new parent) — no separate "did the tip move" bookkeping.
//
// A marker sitting at the very end (nothing suppressed) has no "first
// suppressed tick" to be stuck to at all — that state is 'null', doubling
// as "not rebasing", since the two are behaviorally identical (append
// straight to head either way).
//
// The rebase marker only ever renders/drags at atom-row granularity (see
// fileview.tsx's RebaseDropZone, one per atom) — but non-atom ticks
// (presence, note, prompt) can sit between one atom and the next, and are
// invisible in that view. A drop zone drawn *below* an atom's block visually
// sits below those trailing ticks too, so what the marker resolves to has to
// be the tick right after all of them, not the tick right after the atom
// itself, or "at" would treat an already-settled tick (an existing
// enter/leave, a note) as part of the tail, silently un-happening it.
//
// Returns, for each atom's tickId, the tickId that becomes the marker's
// value if the bar is dropped right after that atom's trailing run — i.e.
// the chain tick immediately following the last one anchored to it. No
// entry means nothing follows that point at all (dropping there means
// "clear the marker", same as dropping past the last atom always has).
export function tailLeadTicks(chain: WireTick[]): Map<string, string> {
  const pivots = new Map<string, string>(); // atom tickId -> last trailing tick anchored to it
  let lastAtom: string | null = null;
  let lastTick: string | null = null;
  for (const t of chain) {
    if (t.kind === "atom") {
      if (lastAtom !== null) pivots.set(lastAtom, lastTick!);
      lastAtom = t.tickId;
    }
    lastTick = t.tickId;
  }
  if (lastAtom !== null) pivots.set(lastAtom, lastTick!);

  const indexOf = new Map(chain.map((t, i): [string, number] => [t.tickId, i]));
  const leads = new Map<string, string>();
  for (const [atomId, pivotId] of pivots) {
    const next = chain[indexOf.get(pivotId)! + 1];
    if (next) leads.set(atomId, next.tickId);
  }
  return leads;
}

// Every atom generated from the same instruction as `atomTickId` — the
// group "Correct this" (fileview.tsx's AtomBlock) regenerates as a unit.
// Generalizes chatview.tsx's exchangesFromChain (which pairs a chat.md
// prompt with exactly one atom, always) to prose written via chat.writer,
// where one instruction can produce several atoms (Ops.append per
// splitAtoms result) — so this walks the chain to find the nearest
// preceding "prompt" tick, then collects every contiguous "atom" tick
// after it, not just one. Returns null if no prompt tick precedes the atom
// at all (shouldn't happen for anything chat.writer produced, but a
// hand-authored/appended atom has no instruction to regenerate from).
export function promptGroupForAtom(ticks: Record<string, WireTick>, head: string | null, atomTickId: string): { promptTick: WireTick; atomTickIds: string[] } | null {
  const chain = tickChain(ticks, head);
  const atomIndex = chain.findIndex((t) => t.tickId === atomTickId);
  if (atomIndex === -1) return null;

  let promptIndex = -1;
  for (let i = atomIndex; i >= 0; i--) {
    if (chain[i].kind === "prompt") { promptIndex = i; break; }
  }
  if (promptIndex === -1) return null;

  const atomTickIds: string[] = [];
  for (let i = promptIndex + 1; i < chain.length && chain[i].kind === "atom"; i++) {
    atomTickIds.push(chain[i].tickId);
  }
  return { promptTick: chain[promptIndex], atomTickIds };
}

// A tick's message is stored verbatim (see Storyteller.Core.Types.decodePayload
// — no header line ever gets folded into it), so a one-line list preview
// can't peel anything off; it has to collapse whatever whitespace/newlines
// the payload actually contains and truncate to a fixed budget instead.
export function tickPreview(msg: string, maxLen = 140): string {
  const collapsed = msg.replace(/\s+/g, " ").trim();
  return collapsed.length > maxLen ? collapsed.slice(0, maxLen) + "…" : collapsed;
}

export function tickField(tick: WireTick, key: string): string | undefined {
  return tick.fields?.[key];
}

// Which character/{id} branches are currently active, derived by folding
// "presence" ticks (enter/leave, see WRITER.md) from root to head — not
// stored separately, since the chain is already the source of truth.
// Preserves first-entered order rather than tick order, so a character who
// re-enters after leaving doesn't jump to the end of the list.
export function activeCharacterBranches(ticks: Record<string, WireTick>, head: string | null): string[] {
  const active: string[] = [];
  for (const t of tickChain(ticks, head)) {
    if (t.kind !== "presence") continue;
    const character = tickField(t, "character");
    const event = tickField(t, "event");
    if (!character) continue;
    if (event === "enter") {
      if (!active.includes(character)) active.push(character);
    } else if (event === "leave") {
      const i = active.indexOf(character);
      if (i !== -1) active.splice(i, 1);
    }
  }
  return active;
}

// Every character/{id} branch that ever appeared in a presence tick on this
// chain, in first-appearance order — unlike 'activeCharacterBranches', a
// character who later left is not removed. Used for the "show all
// characters' presence" toggle in the file view.
export function allPresentCharacters(ticks: Record<string, WireTick>, head: string | null): string[] {
  const seen: string[] = [];
  for (const t of tickChain(ticks, head)) {
    if (t.kind !== "presence") continue;
    const character = tickField(t, "character");
    if (character && !seen.includes(character)) seen.push(character);
  }
  return seen;
}

// Which atom tickIds were written while `character` was present in the
// scene, derived by folding the branch's full chain (atoms and presence
// ticks interleaved) in order. An atom counts as "during" if the character
// was already active at the point that atom lands — i.e. entered before it,
// not yet left. Used to highlight a character's atoms in the file view on
// hover; not stored, recomputed on demand same as activeCharacterBranches.
export function presentDuringAtoms(ticks: Record<string, WireTick>, head: string | null, character: string): Set<string> {
  const result = new Set<string>();
  let active = false;
  for (const t of tickChain(ticks, head)) {
    if (t.kind === "presence" && tickField(t, "character") === character) {
      active = tickField(t, "event") === "enter";
    } else if (t.kind === "atom" && active) {
      result.add(t.tickId);
    }
  }
  return result;
}

// A path's own filename, extension stripped — the fallback title for
// anything that doesn't have a more specific display name of its own (a
// chapter with no heading yet, a codex entry), same "a filename was never
// meant to be read as a title, but it's what's left when there's nothing
// better" role library.tsx's chapter rows and codex.tsx's cards both need.
export function basenameNoExt(path: string): string {
  const base = path.split("/").pop() ?? path;
  const idx = base.lastIndexOf(".");
  return idx > 0 ? base.slice(0, idx) : base;
}

// Flatten a lore tree into its leaves, depth-first — shared by
// app/lore-selector.tsx's card grouping and app/fileview.tsx's mention
// autocomplete, both of which want a flat, searchable list rather than the
// folder tree.
export function flattenLore(nodes: LoreNode[], acc: LoreNode[] = []): LoreNode[] {
  for (const n of nodes) {
    if (n.children.length > 0) flattenLore(n.children, acc);
    else acc.push(n);
  }
  return acc;
}

// Display name for a character/{id} branch. Per WRITER.md's convention, the
// real name is the first Markdown H1 line in sheet.md — a nickname or a
// rename the branch id itself can't hold verbatim (spaces, special
// characters). Falls back to the id (decoded, prefix stripped) when no
// sheet content is available or it has no H1 line — the server never
// extracts this itself (see WS-PROTOCOL.md's "read is raw-but-complete"
// rule), so every caller either has sheet content to pass or accepts the
// id-based fallback.
export function characterDisplayName(branch: string, sheet?: string | null): string {
  const fallback = decodeURIComponent(branchDisplayName(branch));
  if (!sheet) return fallback;
  const h1 = sheet.split("\n").find((line) => line.startsWith("# "));
  return h1 ? h1.slice(2).trim() : fallback;
}

// Deterministic per-character color so a given character's presence bar
// stays the same hue across hovers/sessions instead of being reassigned.
// Hashes the display name, not the raw branch — character branches share
// the long "character/" prefix, which otherwise dominates a simple
// multiplicative hash and leaves only the last couple characters to
// perturb the result, producing near-identical hues for e.g. alice/bob.
// Muted (lower lightness/chroma) so the bar reads as a subtle marker, not
// a highlight competing with the prose.
export function characterColor(branch: string): string {
  const name = characterDisplayName(branch);
  let hash = 0;
  for (let i = 0; i < name.length; i++) hash = (hash * 31 + name.charCodeAt(i)) >>> 0;
  return `oklch(0.58 0.10 ${hash % 360})`;
}

// Where a character's journal timeline should land when the scene's own
// rebase marker (see fileview.tsx's RebaseHandle) is set — the journal has
// its own independent marker the writer can scrub freely, but it must jump
// to follow the scene marker whenever *that* changes, and clear when it's
// cleared ("if I go back in time, the characters do too"). Found via each
// journal atom's cross-branch 'refs' back to the scene atom it was tracked
// from (see Storyteller.Writer.Agent.Tracker) — the last journal atom whose
// ref sits at or before the scene marker's position is the nearest match,
// since journal entries are tracked in the same order as their source atoms.
export function nearestJournalMarker(
  journalTicks: Record<string, WireTick>, journalHead: string | null,
  sceneTicks: Record<string, WireTick>, sceneHead: string | null,
  sceneMarker: string | null,
): string | null {
  if (!sceneMarker) return null;
  const sceneAtoms = tickChain(sceneTicks, sceneHead).filter((t) => t.kind === "atom");
  const sceneIdxOf = new Map(sceneAtoms.map((t, i) => [t.tickId, i]));
  const markerIdx = sceneIdxOf.get(sceneMarker);
  if (markerIdx === undefined) return null;

  let best: string | null = null;
  for (const atom of tickChain(journalTicks, journalHead)) {
    if (atom.kind !== "atom") continue;
    const refIdx = atom.refs.map((r) => sceneIdxOf.get(r)).find((i) => i !== undefined);
    if (refIdx !== undefined && refIdx <= markerIdx) best = atom.tickId;
  }
  return best;
}

// A "character-answer" tick's own message is question and answer joined by
// a single NUL character, not split across separate fields — see
// Storyteller.Writer.Types.CharacterAnswer's own Haddock for why: both are
// free-form text that can contain embedded newlines (a multi-line
// question, a multi-paragraph answer), which a plain tick field can't hold
// safely, and a NUL is a character neither side could plausibly produce
// itself, unlike a chosen text delimiter. Splits into ["", message] if the
// separator is missing (malformed input this convention never produces
// itself), so a caller always gets a question/answer pair back, never a
// thrown error.
export function splitQuestionAnswer(message: string): [string, string] {
  const idx = message.indexOf("\0");
  return idx === -1 ? ["", message] : [message.slice(0, idx), message.slice(idx + 1)];
}

export function statusColor(status: string): string {
  switch (status) {
    case "connected":  return "var(--emerald)";
    case "connecting": return "var(--amber)";
    case "error":      return "var(--rose)";
    default:           return "var(--text-dim)";
  }
}

// Every real tick this summary occurrence covers, up to its own anchor
// (inclusive) — the read-only "what this summary covers" slice, computed
// client-side from ticks the connection already has loaded. Where the
// slice *starts* depends on the kind's behavior (library.ts's
// summaryKindIsIncremental): an incremental kind (journal) covers only
// what's new since the previous occurrence's anchor; a whole-file kind
// (chapter/lore) recompresses the entire file every pass, so every
// occurrence covers from the file's start.
export function summaryCoverageFor(fileTicks: WireTick[], summaryTick: WireTick): WireTick[] {
  // Server.Writer.File.summaryTicksFor hands each occurrence its own
  // boundaries directly: its single ref is its own anchor, and
  // fields.lowerBound (if present) is the previous occurrence's own anchor
  // -- the exclusive lower bound, meaningful only for an incremental kind.
  // No searching/sorting across every occurrence of the kind to guess
  // which one came before: there's exactly one right answer for a specific
  // tick, already on the tick itself.
  const [anchor] = summaryTick.refs;
  const lowerAnchor = summaryKindIsIncremental(summaryTick.fields?.kind ?? "")
    ? summaryTick.fields?.lowerBound
    : undefined;
  if (!anchor) return [];
  const upperIdx = fileTicks.findIndex((t) => t.tickId === anchor);
  if (upperIdx === -1) return [];
  const lowerIdx = lowerAnchor ? fileTicks.findIndex((t) => t.tickId === lowerAnchor) : -1;
  return fileTicks.slice(lowerIdx + 1, upperIdx + 1);
}
