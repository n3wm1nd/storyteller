"use client";

// WS handling for the ticks view (ticksview.tsx): branch-level tick
// operations that aren't scoped to a single file. Colocated with
// ticksview.tsx — see lib/serverCacheStore.ts's header for the write-access
// convention (none of these need it directly; they just send on the
// already-open branch connection).

import { getServerCache } from "@/lib/serverCacheStore";
import { dropFromSelection } from "@/lib/uiStore";

export function addNote(refTickId: string, text: string) {
  getServerCache()._branch?.send({ type: "add.note", refTickId, text });
}

export function moveTick(tickId: string, afterTickId?: string) {
  getServerCache()._branch?.send({ type: "move.tick", tickId, afterTickId });
}

export function deleteTickEntry(tickId: string) {
  getServerCache()._branch?.send({ type: "delete.tick", tickId });
  dropFromSelection([tickId]);
}
