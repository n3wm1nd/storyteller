"use client";

// WS handling for the main file view (fileview.tsx): opening/closing a
// file's connection, atom edits, chat commands, and scene presence.
// Colocated with fileview.tsx rather than living in a shared store file —
// see lib/serverCacheStore.ts's header for the write-access convention.

import { fileConn } from "@/lib/ws";
import type { FileCommand, ContextItem } from "@/lib/ws";
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
      handleTickRemap(evt.mapping);
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
    await fc.connect();
    mirrorServerEvent((s) => ({
      openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
    }));
  } catch (err) {
    setConnStatus(label, "error");
    setError(String(err));
  }
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

// Whole-file delete: a forward tick, not a rebase (see
// Storyteller.Core.Create's Haddock) — path's whole tick history stays
// intact for anyone time-travelling to before the deletion, but the path
// itself drops out of the tree, so fileState on this same connection
// comes back present-but-headed-at-the-deletion-tick, not absent.
export function deleteFile(path: string) {
  sendFileCommand(path, { type: "delete" });
}

// Rename path's current lifetime to newPath — a rebase at the file's own
// creation tick (see Storage.Ops.renameFile), sent on the *old* path's
// connection since that's where the file currently lives. The server
// pushes file.absent on this connection once it lands (fileState on the
// old path genuinely empties out — rename rewrites those ticks' own
// atomPath, unlike delete's forward event); the caller is responsible for
// switching to the new path's connection afterward (see page.tsx's
// handleRenameFile), same as closing/reopening on any other path change.
export function renameFile(path: string, newPath: string) {
  sendFileCommand(path, { type: "rename", newPath });
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

// rebaseMarker (see lib/utils.tailLeadTicks / wsHelpers.atRebase) is the
// tick the bar is stuck to — the first tick of whatever's suppressed after
// it. Whenever a command runs through this marker, that exact tick is the
// first thing 'at' pops and later replays back on top (see
// Storyteller.Core.Git.atGeneric) — its parent changed, so it's always one
// of the "from" ids in the resulting mapping. That makes keeping it current
// a plain table lookup, no different from contextAtoms/contextAnnotations
// below — no walking the chain, no reasoning about where "this file's own
// tail" starts versus some other branch's cascade caught in the same
// broadcast.
function handleTickRemap(mapping: [string, string][]) {
  const table = new Map(mapping);
  useUI.setState((s) => ({
    rebaseMarker: s.rebaseMarker ? remapTickId(table, s.rebaseMarker) : s.rebaseMarker,
    contextAtoms: remapSet(table, s.contextAtoms),
    contextAnnotations: remapSet(table, s.contextAnnotations),
  }));
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

// Send a command on path's file connection, rebased at the current marker
// if one's set (see wsHelpers.atRebase) — the one place that reads
// 'rebaseMarker'/'journalMarkers' and this file's own live 'ticks' to
// resolve the actual 'at' pivot, so every mutating command below gets that
// resolution identically instead of repeating it.
function sendFileCommand(path: string, cmd: FileCommand) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  const ui = useUI.getState();
  fc.conn.send(atRebase(ui.rebaseMarker, fc.ticks, cmd, ui.journalMarkers));
}

// Send a chat.* command now, or hold it if a generation is already
// streaming on this connection (server processes one command at a time).
// 'buildCmd' receives the captured flowTid (HEAD at queue-time) only when
// the command is actually being queued — an immediate send has no
// in-flight generation to be provisional about.
function sendChatCommand(path: string, buildCmd: (flowTid?: string) => FileCommand) {
  const fc = getServerCache().openFiles[path];
  if (!fc) return;
  if (getServerCache().preview !== null) {
    useUI.setState({ pendingSubmit: { path, cmd: buildCmd(fc.head ?? undefined) } });
  } else {
    schedulePreviewPlaceholder();
    sendFileCommand(path, buildCmd(undefined));
  }
}

// Presence is scoped to a file (a scene), not the whole branch — see
// WRITER.md — so this sends on the file connection, same as any other
// mutating file command, and gets rebase-at-marker for free via 'atRebase'.
export function enterScene(path: string, character: string) {
  sendFileCommand(path, { type: "enter.scene", character });
}

export function leaveScene(path: string, character: string) {
  sendFileCommand(path, { type: "leave.scene", character });
}

export function appendToFile(path: string, content: string) {
  const text = content.replace(/^\/+/, "");
  sendChatCommand(path, () => ({ type: "chat.append", content: text }));
}

export function editAtom(path: string, tickId: string, content: string) {
  sendFileCommand(path, { type: "edit.atom", tickId, content });
  dropFromSelection([tickId]);
}

// Edit a chat prompt tick's text — see ws.ts's "edit.prompt": a prompt
// isn't file content, so this is a distinct command from editAtom.
export function editPrompt(path: string, tickId: string, content: string) {
  sendFileCommand(path, { type: "edit.prompt", tickId, content });
}

export function deleteAtom(path: string, tickId: string) {
  sendFileCommand(path, { type: "delete.atom", tickId });
  dropFromSelection([tickId]);
}

// Both reuse the existing atom context selection ('contextAtoms') rather
// than introducing a separate selection mode — same targets Fix/Note
// already operate on.
export function mergeSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 2) return;
  sendFileCommand(path, { type: "merge.atoms", targets });
  dropFromSelection(targets);
}

export function splitSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "split.atoms", targets });
  dropFromSelection(targets);
}

export function hideSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "hide.atoms", targets });
  dropFromSelection(targets);
}

export function unhideSelected(path: string) {
  const targets = buildContextTargets(path);
  if (targets.length < 1) return;
  sendFileCommand(path, { type: "unhide.atoms", targets });
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
