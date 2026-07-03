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

// Display name for a character/{id} branch — id decoded, prefix stripped.
// Shared by the character sidebar and the ticks view's presence rendering.
export function characterDisplayName(branch: string): string {
  const stripped = branch.startsWith("character/") ? branch.slice("character/".length) : branch;
  return decodeURIComponent(stripped);
}

export function statusColor(status: string): string {
  switch (status) {
    case "connected":  return "var(--emerald)";
    case "connecting": return "var(--amber)";
    case "error":      return "var(--rose)";
    default:           return "var(--text-dim)";
  }
}
