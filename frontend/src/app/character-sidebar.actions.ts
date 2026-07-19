"use client";

// WS handling for the character sidebar (character-sidebar.tsx): character
// panel connections and their read-only journal file connections.
// Colocated with character-sidebar.tsx rather than living in a shared store
// file — see lib/serverCacheStore.ts's header for the write-access
// convention.

import { characterConn, fileConn } from "@/lib/ws";
import { getServerCache, mirrorServerEvent } from "@/lib/serverCacheStore";
import { useUI, dropFromSelection, setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { applyFileUpdate, isChatPreviewEvent, remapSet, atRebase } from "@/lib/wsHelpers";
import { clearPreviewDelayTimer, handleChatPreview } from "@/lib/chatPreview";

const JOURNAL_PATH = "journal.md";

export async function openCharacter(branch: string): Promise<void> {
  if (getServerCache().openCharacters[branch]) return;

  const label = `character:${branch}`;
  setConnStatus(label, "connecting");

  const cc = characterConn(branch);

  cc.onStatus((s) => {
    if (s !== "connected") setConnStatus(label, "connecting");
  });

  cc.subscribe((evt) => {
    bumpActivity(label);
    if (evt.type === "character.update") {
      mirrorServerEvent((s) => ({
        openCharacters: { ...s.openCharacters, [branch]: { branch, name: evt.name, sheet: evt.sheet ?? null, avatar: evt.avatar, conn: cc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "error") {
      setError(evt.message);
    }
  });

  try {
    await cc.connect();
    mirrorServerEvent((s) => ({
      openCharacters: { ...s.openCharacters, [branch]: { branch, name: branch, sheet: null, avatar: false, conn: cc } },
    }));
  } catch (err) {
    setConnStatus(label, "error");
    setError(String(err));
  }
}

export function closeCharacter(branch: string) {
  getServerCache().openCharacters[branch]?.conn.close();
  mirrorServerEvent((s) => {
    const next = { ...s.openCharacters };
    delete next[branch];
    return { openCharacters: next };
  });
  removeConn(`character:${branch}`);
}

// contextAtoms/contextAnnotations are shared, connection-agnostic selection
// sets — any tick.remap, whether from the main file connection or a
// character's journal connection, must keep them pointing at the right
// ids. Journal edits don't carry a persistent rebaseMarker in the store
// (the journal's own marker is local component state — see
// character-sidebar.tsx), so unlike the file view's remap handler this only
// remaps the shared sets, not a marker.
function handleContextRemap(mapping: [string, string][]) {
  const table = new Map(mapping);
  useUI.setState((s) => ({
    contextAtoms: remapSet(table, s.contextAtoms),
    contextAnnotations: remapSet(table, s.contextAnnotations),
  }));
}

// A character's journal is a plain file (see WRITER.md) — this is the same
// generic '/branch/{name}/{path}' connection the main file view uses, just
// scoped to the character's own branch instead of activeBranch, and opened
// lazily by the sidebar accordion rather than by file selection.
export async function openJournal(branch: string): Promise<void> {
  if (getServerCache().openJournals[branch]) return;

  const label = `journal:${branch}`;
  setConnStatus(label, "connecting");

  const jc = fileConn(branch, JOURNAL_PATH);

  jc.onStatus((s) => {
    if (s !== "connected") setConnStatus(label, "connecting");
  });

  jc.subscribe((evt) => {
    bumpActivity(label);
    if (evt.type === "file.present") {
      mirrorServerEvent((s) => ({
        openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: false, conn: jc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "file.absent") {
      mirrorServerEvent((s) => ({
        openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: true, conn: jc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "update") {
      clearPreviewDelayTimer();
      mirrorServerEvent((s) => {
        const prev = s.openJournals[branch];
        if (!prev) return {};
        return { openJournals: { ...s.openJournals, [branch]: { ...prev, ticks: applyFileUpdate(prev.ticks, evt), head: evt.head, absent: false }, preview: null } };
      });
    } else if (evt.type === "tick.remap") {
      handleContextRemap(evt.mapping);
    } else if (evt.type === "agent.log") {
      useUI.getState().addAgentLog(evt.level, evt.message);
    } else if (isChatPreviewEvent(evt)) {
      handleChatPreview(evt);
    } else if (evt.type === "error") {
      clearPreviewDelayTimer();
      mirrorServerEvent({ preview: null });
      setError(evt.message);
    }
  });

  try {
    await jc.connect();
    mirrorServerEvent((s) => ({
      openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: false, conn: jc } },
    }));
  } catch (err) {
    setConnStatus(label, "error");
    setError(String(err));
  }
}

export function closeJournal(branch: string) {
  getServerCache().openJournals[branch]?.conn.close();
  mirrorServerEvent((s) => {
    const next = { ...s.openJournals };
    delete next[branch];
    return { openJournals: next };
  });
  useUI.setState((s) => {
    const nextMarkers = { ...s.journalMarkers };
    delete nextMarkers[branch];
    return { journalMarkers: nextMarkers };
  });
  removeConn(`journal:${branch}`);
}

// Basic editing on a character's journal — same commands the main file
// view sends, just addressed at the journal's own connection. 'marker' is
// the journal's own local time-travel position (see character-sidebar.tsx
// / lib/utils.nearestJournalMarker), not the store's global 'rebaseMarker'
// (which is scoped to whichever scene file is open). atRebase needs the
// journal's own current ticks to resolve the marker's pivot fresh (see
// wsHelpers.atRebase) — read from the same open connection record the
// send itself goes through, not threaded in as a separate parameter. A
// journal has no sub-journals of its own, so its own 'journalMarkers'
// argument is always empty.
export function appendJournal(branch: string, content: string, marker: string | null) {
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "chat.append", content }, {}));
}

export function editJournalAtom(branch: string, tickId: string, content: string, marker: string | null) {
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "edit.atom", tickId, content }, {}));
  dropFromSelection([tickId]);
}

// Batched — one transaction for the whole selection, not one round trip
// per tick (see ws.ts's "delete.ticks": server sorts descendants-first,
// no client ordering needed).
export function deleteJournalTicks(branch: string, tickIds: string[], marker: string | null) {
  if (tickIds.length === 0) return;
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "delete.ticks", targets: tickIds }, {}));
  dropFromSelection(tickIds);
}

// Cycle a journal atom forward through its own alternates — same generic
// atom.swipe.cycle command the main file view sends (see fileview.actions.ts's
// cycleSwipe), just addressed at the journal's own connection.
export function cycleJournalSwipe(branch: string, tickId: string, marker: string | null) {
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "atom.swipe.cycle", tickId }, {}));
}

// Fix, scoped to the journal itself — this is what "rewrite to exclude
// knowledge of the gun" actually runs against: the character's own
// account, not the scene prose. Targets are journal atom ids (see
// character-sidebar.tsx's selection-intersection), sent as a chat.fixer
// on the journal's connection, same shape as the main view's chatFix.
export function journalFix(branch: string, text: string, targets: string[], marker: string | null) {
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "chat.fixer", text, targets }, {}));
}
