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
// as-is. 'marker' is the tick the bar is stuck to — the first tick of
// whatever's currently suppressed after it (see lib/utils.tailLeadTicks) —
// not the pivot to append at; the actual 'at' pivot sent to the server is
// that tick's *parent*, resolved fresh against 'ticks' right here. That's
// deliberate: the parent is automatically wherever the most recent write
// through this marker landed (a fresh drop, or the marker tick's own most
// recent relocation — see 'stashMarkerFixup' below), with nothing to
// separately track — "the bar follows along with each append" falls out of
// always resolving the pivot this late, not from advancing anything.
//
// When rebasing, attaches every active character's journal position (see
// 'journalMarkers' and character-sidebar.tsx's nearestJournalMarker effect,
// which keeps it following the scene's own marker) as the `branches` field
// — skipping any character whose corresponding position couldn't be
// derived (null), rather than sending a meaningless entry. See SELECTION.md.
export function atRebase(marker: string | null, ticks: Record<string, WireTick>, cmd: FileCommand, journalMarkers: Record<string, string | null>): FileCommand {
  const pivot = marker ? ticks[marker]?.parent : null;
  if (!pivot) return cmd;
  const branches = Object.entries(journalMarkers)
    .filter((entry): entry is [string, string] => entry[1] !== null)
    .map(([branch, tickId]) => ({ branch, tickId }));
  return { type: "at", tickId: pivot, command: cmd, ...(branches.length > 0 ? { branches } : {}) };
}

// How many ticks currently sit from (and including) the marker through to
// head — the one thing about "where the tail is" that a command run
// through this exact marker can't change, because that whole span is
// popped out before the command runs and replayed back verbatim,
// unchanged in count or order, once it's done (Storyteller.Core.Git.
// atGeneric). Returns null if 'marker' isn't in 'chain' at all.
export function tailLengthFrom(chain: WireTick[], marker: string): number | null {
  const idx = chain.findIndex((t) => t.tickId === marker);
  return idx === -1 ? null : chain.length - idx;
}

// Confirmed against the live server (see conversation): an in-place 'at'
// command gets exactly one 'update' in reply and *no* accompanying
// 'tick.remap' — that event only fires for a cross-branch cascade
// (Server.Writer.Run.storageNotify only reacts to StoryStorage's
// UpdateReferences reaching the *outer* interpreter; a single command's own
// tail-replay is entirely absorbed by its own per-command 'withStorage'
// buffering and never escapes as one). So there's no explicit old->new id
// to look up for the marker the way there is for contextAtoms/
// contextAnnotations — the *only* signal available that the command
// finished is that one plain 'update', and the only thing that can relocate
// the marker against it is the tail-length invariant above, not an id.
//
// This map is the one piece of connection bookkeeping in this file (see the
// header) — small and deliberately not part of the zustand store, since
// nothing renders off "is a fixup pending"; it's consumed and cleared
// entirely inside the 'update' handler that reacts to it (fileview.actions.
// ts's openFile). Keyed by path since rebaseMarker is scoped to whichever
// file is currently open, same as the marker itself.
const pendingMarkerFixups = new Map<string, number>();

export function stashMarkerFixup(path: string, chain: WireTick[], marker: string | null) {
  if (!marker) return;
  const len = tailLengthFrom(chain, marker);
  if (len !== null) pendingMarkerFixups.set(path, len);
}

// Consume this path's stashed tail length (if any) and relocate the marker
// against 'chain' — the tick that many positions before the end of it, same
// definition 'tailLengthFrom' used to record it, just solved for the tick
// instead of the count. Returns 'undefined' (not null) when there was
// nothing pending, so a caller can tell "no fixup to apply" apart from
// "the fixup resolved to no marker" (null — the tail now reaches all the
// way to the top of the chain, nothing left before it).
export function consumeMarkerFixup(path: string, chain: WireTick[]): string | null | undefined {
  const len = pendingMarkerFixups.get(path);
  if (len === undefined) return undefined;
  pendingMarkerFixups.delete(path);
  const idx = chain.length - len;
  return idx >= 0 ? chain[idx].tickId : null;
}
