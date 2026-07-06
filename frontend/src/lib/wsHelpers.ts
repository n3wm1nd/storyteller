// Small, pure helpers shared by more than one *.actions.ts file — tick-map
// bookkeeping and id-remapping that don't belong to any single connection
// scope. Nothing here touches the store or a connection; it's just data
// transforms every action file that walks WireTicks needs the same way.

import type { WireTick, Update, ChatPreviewEvent, FileCommand } from "./ws";

export function applyUpdate(ticks: Record<string, WireTick>, upd: Update): Record<string, WireTick> {
  const next = { ...ticks };
  for (const t of upd.ticks) next[t.tickId] = t;
  return next;
}

export function isChatPreviewEvent(evt: { type: string }): evt is ChatPreviewEvent {
  return evt.type === "chat.preview.start" || evt.type === "chat.preview"
      || evt.type === "chat.preview.thinking" || evt.type === "chat.preview.end";
}

// Apply a server-computed old->new tickId remap (from a rebase/replace/move)
// to every locally held tickId reference. The server is the source of truth
// for how ids move — this just relocates whatever we're tracking rather than
// leaving it pointing at an id that no longer exists.
export function remapTickId(table: Map<string, string>, id: string): string {
  return table.get(id) ?? id;
}

export function remapSet(table: Map<string, string>, ids: Set<string>): Set<string> {
  let changed = false;
  const next = new Set<string>();
  for (const id of ids) {
    const mapped = remapTickId(table, id);
    if (mapped !== id) changed = true;
    next.add(mapped);
  }
  return changed ? next : ids;
}

// Wrap a command in an "at" rebase if a marker is set, otherwise send it
// as-is. When rebasing, attaches every active character's journal position
// (see 'journalMarkers' and character-sidebar.tsx's nearestJournalMarker
// effect, which keeps it following the scene's own marker) as the `branches`
// field — skipping any character whose corresponding position couldn't be
// derived (null), rather than sending a meaningless entry. See SELECTION.md.
export function atRebase(marker: string | null, cmd: FileCommand, journalMarkers: Record<string, string | null>): FileCommand {
  if (!marker) return cmd;
  const branches = Object.entries(journalMarkers)
    .filter((entry): entry is [string, string] => entry[1] !== null)
    .map(([branch, tickId]) => ({ branch, tickId }));
  return { type: "at", tickId: marker, command: cmd, ...(branches.length > 0 ? { branches } : {}) };
}
