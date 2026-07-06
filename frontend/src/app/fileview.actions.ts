"use client";

// WS handling for the main file view (fileview.tsx): opening/closing a
// file's connection, atom edits, chat commands, and scene presence.
// Colocated with fileview.tsx rather than living in a shared store file —
// see lib/serverCacheStore.ts's header for the write-access convention.

import { fileConn } from "@/lib/ws";
import type { FileCommand, ContextItem, WireTick } from "@/lib/ws";
import { getServerCache, mirrorServerEvent } from "@/lib/serverCacheStore";
import { useUI, dropFromSelection, setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { applyUpdate, isChatPreviewEvent, remapTickId, remapSet, atRebase } from "@/lib/wsHelpers";
import { clearPreviewDelayTimer, schedulePreviewPlaceholder, handleChatPreview } from "@/lib/chatPreview";
import { tickChain } from "@/lib/utils";

export async function openFile(path: string): Promise<void> {
  const { activeBranch, openFiles } = getServerCache();
  if (!activeBranch) return;
  useUI.setState({ rebaseMarker: null, pendingSubmit: null });
  if (openFiles[path]) return;

  const label = `file:${path}`;
  setConnStatus(label, "connecting");

  const fc = fileConn(activeBranch, path);

  fc.onStatus((s) => {
    if (s !== "connected") setConnStatus(label, "connecting");
  });

  fc.subscribe((evt) => {
    bumpActivity(label);
    if (evt.type === "file.present") {
      mirrorServerEvent((s) => ({
        openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "file.absent") {
      mirrorServerEvent((s) => ({
        openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: true, conn: fc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "update") {
      clearPreviewDelayTimer();
      mirrorServerEvent((s) => {
        const prev = s.openFiles[path];
        if (!prev) return {};
        return {
          openFiles: {
            ...s.openFiles,
            [path]: { ...prev, ticks: applyUpdate(prev.ticks, evt), head: evt.head, absent: false },
          },
          preview: null,
        };
      });
    } else if (evt.type === "tick.remap") {
      handleTickRemap(path, evt.mapping);
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

  await fc.connect();
  mirrorServerEvent((s) => ({
    openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
  }));
}

// Explicit "new file" creation: opens the connection (same as selecting
// an existing file, which starts out absent), then sends file.create —
// the server introduces the path as its own empty tick, and the
// resulting file.present + update events flip 'absent' to false, same as
// any other write to a not-yet-tracked path would.
export async function createFile(path: string): Promise<void> {
  await openFile(path);
  getServerCache().openFiles[path]?.conn.send({ type: "file.create" });
}

export function closeFile(path: string) {
  getServerCache().openFiles[path]?.conn.close();
  mirrorServerEvent((s) => {
    const next = { ...s.openFiles };
    delete next[path];
    return { openFiles: next };
  });
  removeConn(`file:${path}`);
}

// After remapping, the marker sits at the (possibly-renamed) tick it was
// pinned to — but if the command that just ran wrote brand-new atoms right
// after it (e.g. an append issued while rebasing), the marker should follow
// them so a second command chains after the first instead of both landing
// on the same pivot. New atoms are never a "to" of the mapping — only
// pre-existing atoms that got rebased are — so we can tell them apart from
// the old tail without knowing anything about the specific command that ran.
function advanceMarker(marker: string, ticks: Record<string, WireTick>, head: string | null, toSet: Set<string>): string | null {
  const chain = tickChain(ticks, head).filter((t) => t.kind === "atom");
  const idx = chain.findIndex((t) => t.tickId === marker);
  if (idx === -1) return null; // no longer in the chain (e.g. deleted) — nothing sensible to fall back to
  let i = idx;
  while (i + 1 < chain.length && !toSet.has(chain[i + 1].tickId)) i++;
  return chain[i].tickId;
}

// contextAtoms/contextAnnotations are shared, connection-agnostic selection
// sets (see character-sidebar.actions.ts's 'openJournal') — any tick.remap,
// whether from the main file connection or a character's journal
// connection, must keep them pointing at the right ids.
function handleTickRemap(path: string, mapping: [string, string][]) {
  const table = new Map(mapping);
  const toSet = new Set(mapping.map(([, to]) => to));
  const fc = getServerCache().openFiles[path];
  useUI.setState((s) => {
    const marker = s.rebaseMarker ? remapTickId(table, s.rebaseMarker) : s.rebaseMarker;
    return {
      rebaseMarker: marker && fc ? advanceMarker(marker, fc.ticks, fc.head, toSet) : marker,
      contextAtoms: remapSet(table, s.contextAtoms),
      contextAnnotations: remapSet(table, s.contextAnnotations),
    };
  });
}

// Build the read-only context items for a chat command from the current
// atom/annotation selection — chain order, atoms then annotations, per
// SELECTION.md. contextAtoms/contextAnnotations are shared across the open
// file and every open journal, so a selection made in a character's journal
// is a genuine cross-branch reference when it rides along on a command sent
// to the main file's connection — tagged with its own branch, since only
// the *file's* branch is implied by the connection.
function buildContextItems(path: string): ContextItem[] {
  const cache = getServerCache();
  const ui = useUI.getState();
  const fc = cache.openFiles[path];
  if (!fc) return [];
  const chain = tickChain(fc.ticks, fc.head);
  const atoms       = chain.filter((t) => ui.contextAtoms.has(t.tickId));
  const annotations = chain.filter((t) => ui.contextAnnotations.has(t.tickId));
  const local: ContextItem[] = [...atoms, ...annotations].map((t) => ({
    tickId: t.tickId,
    kind: t.kind,
    content: t.content ?? t.message,
  }));

  const crossBranch: ContextItem[] = [];
  for (const [branch, jc] of Object.entries(cache.openJournals)) {
    const jchain = tickChain(jc.ticks, jc.head);
    const jAtoms       = jchain.filter((t) => ui.contextAtoms.has(t.tickId));
    const jAnnotations = jchain.filter((t) => ui.contextAnnotations.has(t.tickId));
    for (const t of [...jAtoms, ...jAnnotations]) {
      crossBranch.push({ tickId: t.tickId, kind: t.kind, content: t.content ?? t.message, branch });
    }
  }

  return [...local, ...crossBranch];
}

// Target ids for chat.fixer/chat.note, scoped to this file's own chain —
// contextAtoms is a shared, connection-agnostic set (a journal atom's id can
// be selected alongside a scene atom's, see character-sidebar.tsx), so a
// command sent on *this* file's connection must only carry the subset that
// actually belongs to it. Journal-selected ids are picked up by
// journalFix/deleteJournalAtom instead (see page.tsx's handleFix).
function buildContextTargets(path: string): string[] {
  const fc = getServerCache().openFiles[path];
  if (!fc) return [];
  return tickChain(fc.ticks, fc.head)
    .filter((t) => t.kind === "atom" && useUI.getState().contextAtoms.has(t.tickId))
    .map((t) => t.tickId);
}

// Send a chat.* command now, or hold it if a generation is already
// streaming on this connection (server processes one command at a time).
// 'buildCmd' receives the captured flowTid (HEAD at queue-time) only when
// the command is actually being queued — an immediate send has no
// in-flight generation to be provisional about.
function sendChatCommand(path: string, buildCmd: (flowTid?: string) => FileCommand) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  const ui = useUI.getState();
  if (getServerCache().preview !== null) {
    useUI.setState({ pendingSubmit: { path, cmd: buildCmd(fc.head ?? undefined) } });
  } else {
    schedulePreviewPlaceholder();
    fc.conn.send(atRebase(ui.rebaseMarker, buildCmd(undefined), ui.journalMarkers));
  }
}

// Presence is scoped to a file (a scene), not the whole branch — see
// WRITER.md — so this sends on the file connection, same as any other
// mutating file command, and gets rebase-at-marker for free via 'atRebase'.
export function enterScene(path: string, character: string) {
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(atRebase(ui.rebaseMarker, { type: "enter.scene", character }, ui.journalMarkers));
}

export function leaveScene(path: string, character: string) {
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(atRebase(ui.rebaseMarker, { type: "leave.scene", character }, ui.journalMarkers));
}

export function appendToFile(path: string, content: string) {
  const text = content.replace(/^\/+/, "");
  sendChatCommand(path, () => ({ type: "chat.append", content: text }));
}

export function editAtom(path: string, tickId: string, content: string) {
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(
    atRebase(ui.rebaseMarker, { type: "edit.atom", tickId, content }, ui.journalMarkers)
  );
  dropFromSelection([tickId]);
}

export function deleteAtom(path: string, tickId: string) {
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(
    atRebase(ui.rebaseMarker, { type: "delete.atom", tickId }, ui.journalMarkers)
  );
  dropFromSelection([tickId]);
}

// Both reuse the existing atom context selection ('contextAtoms') rather
// than introducing a separate selection mode — same targets Fix/Note
// already operate on.
export function mergeSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 2) return;
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(
    atRebase(ui.rebaseMarker, { type: "merge.atoms", targets }, ui.journalMarkers)
  );
  dropFromSelection(targets);
}

export function splitSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  const ui = useUI.getState();
  getServerCache().openFiles[path]?.conn.send(
    atRebase(ui.rebaseMarker, { type: "split.atoms", targets }, ui.journalMarkers)
  );
  dropFromSelection(targets);
}

export function chatWrite(path: string, text: string) {
  const context = buildContextItems(path);
  sendChatCommand(path, (flowTid) => ({ type: "chat.writer", text, context, flowTid }));
}

export function chatFix(path: string, text: string) {
  const context = buildContextItems(path);
  const targets = buildContextTargets(path);
  sendChatCommand(path, () => ({ type: "chat.fixer", text, context, targets }));
}

export function chatNote(path: string, text: string) {
  const targets = buildContextTargets(path);
  sendChatCommand(path, () => ({ type: "chat.note", text, targets }));
}

export function chatRegen(path: string, text: string, byBeat: boolean) {
  const context = buildContextItems(path);
  sendChatCommand(path, () => ({ type: "chat.regen", text, context, byBeat }));
}

export function chatOutline(path: string) {
  sendChatCommand(path, () => ({ type: "chat.outline" }));
}

// Discuss, don't write — see WRITER.md's chat/ convention and ChatView.
export function chatConverse(path: string, text: string) {
  sendChatCommand(path, () => ({ type: "chat.converse", text }));
}

// Regenerate a chat exchange: drop the old prompt/reply tick pair and
// resend the same text. Only ever called on the *last* exchange in a chat
// file — delete+re-append always lands at the new HEAD, so redoing a
// non-final turn would reorder it.
export function chatConverseRegen(path: string, promptTickId: string, atomTickId: string, text: string) {
  deleteAtom(path, atomTickId);
  deleteAtom(path, promptTickId);
  chatConverse(path, text);
}
