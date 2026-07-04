import type { WireTick } from "./store";

export type AnnotationMode = "hidden" | "dots" | "expanded";

export function tickChain(ticks: Record<string, WireTick>, head: string | null): WireTick[] {
  if (!head || !ticks[head]) return [];
  const chain: WireTick[] = [];
  let cur: string | null = head;
  const seen = new Set<string>();
  while (cur && ticks[cur] && !seen.has(cur)) {
    seen.add(cur);
    const t = ticks[cur];
    if (t.kind !== "root") chain.push(t);
    cur = t.parent;
  }
  return chain.reverse();
}

export function tickPayload(msg: string): string {
  const nl = msg.indexOf("\n");
  return nl >= 0 ? msg.slice(nl + 1) : msg;
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

// Display name for a character/{id} branch. Per WRITER.md's convention, the
// real name is the first Markdown H1 line in sheet.md — a nickname or a
// rename the branch id itself can't hold verbatim (spaces, special
// characters). Falls back to the id (decoded, prefix stripped) when no
// sheet content is available or it has no H1 line — the server never
// extracts this itself (see WS-PROTOCOL.md's "read is raw-but-complete"
// rule), so every caller either has sheet content to pass or accepts the
// id-based fallback.
export function characterDisplayName(branch: string, sheet?: string | null): string {
  const stripped = branch.startsWith("character/") ? branch.slice("character/".length) : branch;
  const fallback = decodeURIComponent(stripped);
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

export function statusColor(status: string): string {
  switch (status) {
    case "connected":  return "var(--emerald)";
    case "connecting": return "var(--amber)";
    case "error":      return "var(--rose)";
    default:           return "var(--text-dim)";
  }
}
