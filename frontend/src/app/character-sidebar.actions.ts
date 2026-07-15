"use client";

// WS handling for the character sidebar (character-sidebar.tsx): character
// panel connections and their read-only journal file connections.
// Colocated with character-sidebar.tsx rather than living in a shared store
// file — see lib/serverCacheStore.ts's header for the write-access
// convention.

import { branchConn, characterConn, fileConn, type BranchCommand } from "@/lib/ws";
import { getServerCache, mirrorServerEvent } from "@/lib/serverCacheStore";
import { useUI, dropFromSelection, setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { applyUpdate, isChatPreviewEvent, remapSet, atRebase } from "@/lib/wsHelpers";
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
        openCharacters: { ...s.openCharacters, [branch]: { branch, name: evt.name, sheet: evt.sheet ?? null, conn: cc } },
      }));
      setConnStatus(label, "connected");
    } else if (evt.type === "error") {
      setError(evt.message);
    }
  });

  try {
    await cc.connect();
    mirrorServerEvent((s) => ({
      openCharacters: { ...s.openCharacters, [branch]: { branch, name: branch, sheet: null, conn: cc } },
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
        return { openJournals: { ...s.openJournals, [branch]: { ...prev, ticks: applyUpdate(prev.ticks, evt), head: evt.head, absent: false }, preview: null } };
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

// Explicit, one-shot invocation of the raw Tracker agent (see
// Storyteller.Writer.Agent.Tracker) — copies new deltas into this
// character's journal.md, verbatim, with a cross-branch ref back to each
// source atom (that ref is what lets the journal panel's hover highlight
// find the matching atom in the main view). 'track' is a 'BranchCommand'
// sent on the *character* branch's own connection, not the currently open
// story branch's — so this opens a short-lived branch connection just to
// fire it, and closes it the moment the resulting update/error lands. No
// persistent state needed: the journal's own file connection (if open)
// receives the new ticks through its own push, same as any other write to
// that branch.
//
// 'onlyFile' omitted pulls every file on the source branch (not just the
// one currently open) into the journal in one call — presence gating
// (Server.Writer.Branch.onlyWhilePresent) still applies per atom, so this
// is safe (and cheap, see trackBranch's own Haddock on the shallow walk)
// to call for a character who isn't even in the current scene; see
// 'trackAllJournals' below for the sidebar's "Track All" button.
function trackOne(characterBranch: string, source: string, onlyFile: string | undefined) {
  const conn = branchConn(characterBranch);
  conn.subscribe((evt) => {
    if (evt.type === "update" || evt.type === "error") conn.close();
    if (evt.type === "error") setError(evt.message);
  });
  conn.connect().then(() => {
    conn.send({ type: "track", source, onlyFile, to: JOURNAL_PATH });
  }).catch(() => { conn.close(); });
}

export function trackJournal(characterBranch: string, fromPath: string) {
  const source = getServerCache().activeBranch;
  if (!source) return;
  trackOne(characterBranch, source, fromPath);
}

// The sidebar's "Track All" button: every known character branch (not just
// the ones present in the current scene), pulling every source file (not
// just whatever's open) into each one's own journal — see 'trackOne's
// Haddock for why this is safe/cheap to run indiscriminately, including for
// characters absent from every recent scene.
export function trackAllJournals(characterBranches: string[]) {
  const source = getServerCache().activeBranch;
  if (!source) return;
  for (const branch of characterBranches) trackOne(branch, source, undefined);
}

export const TASKS_PATH = "tasks.md";

// Same short-lived-connection shape as 'trackOne': fire the command on the
// character branch's own connection, close on the resulting update/error.
// Resolves once the command has actually landed (or failed) — not on send
// — so a caller that wants to refetch tasks.md afterward (see
// character-sidebar.tsx's TasksPanel) doesn't race the mutation.
function runTasksCommand(characterBranch: string, cmd: BranchCommand): Promise<void> {
  return new Promise((resolve) => {
    const conn = branchConn(characterBranch);
    conn.subscribe((evt) => {
      if (evt.type === "update" || evt.type === "error" || evt.type === "file.added") {
        conn.close();
        resolve();
      }
      if (evt.type === "error") setError(evt.message);
    });
    conn.connect().then(() => {
      conn.send(cmd);
    }).catch(() => { conn.close(); resolve(); });
  });
}

// Reconcile this character's tasks.md against whatever's new in their own
// journal since the last sync — see Storyteller.Writer.Agent.Tasks.syncTasks.
// Restricted to journal.md, same "only what this character actually
// witnessed" reasoning as trackJournal itself (the journal is already
// presence-gated on the way in).
export function syncTasks(characterBranch: string) {
  return runTasksCommand(characterBranch, { type: "sync.tasks", onlyFile: JOURNAL_PATH, to: TASKS_PATH });
}

// Propose new tasks for this character from their journal plus the active
// story's world lore — never the story's raw scene content (see
// Server.Writer.Branch.Protocol.SuggestTasks's own Haddock on why).
export function suggestTasks(characterBranch: string) {
  const loreSource = getServerCache().activeBranch;
  return runTasksCommand(characterBranch, {
    type: "suggest.tasks", loreSource: loreSource ?? undefined, onlyFile: JOURNAL_PATH, to: TASKS_PATH,
  });
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

export function deleteJournalAtom(branch: string, tickId: string, marker: string | null) {
  const jc = getServerCache().openJournals[branch];
  jc?.conn.send(atRebase(marker, jc.ticks, { type: "delete.atom", tickId }, {}));
  dropFromSelection([tickId]);
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
