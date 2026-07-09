"use client";

// WS handling for the left sidebar (sidebar.tsx: branch list, create/delete,
// file-tree drag-drop upload) plus the top-level "which branch is active"
// switch it triggers. Colocated with sidebar.tsx rather than living in a
// shared store file — see lib/serverCacheStore.ts's header for why that's
// safe (writes go through the loudly-named 'mirrorServerEvent', importable
// from anywhere).

import { sessionConn, branchConn, libraryConn, uploadBranchFile } from "@/lib/ws";
import { getServerCache, mirrorServerEvent } from "@/lib/serverCacheStore";
import { useUI, setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { applyUpdate, isChatPreviewEvent } from "@/lib/wsHelpers";
import { handleChatPreview, clearPreviewDelayTimer } from "@/lib/chatPreview";

export async function connect(): Promise<void> {
  setConnStatus("session", "connecting");
  useUI.setState({ error: null });

  const session = sessionConn();

  session.onStatus((s) => {
    if (s !== "connected") setConnStatus("session", "connecting");
  });

  session.subscribe((evt) => {
    bumpActivity("session");
    if (evt.type === "session.ready") {
      setConnStatus("session", "connected");
    } else if (evt.type === "branch.list") {
      // The one source of truth for which branches exist — pushed complete
      // on every create/delete (see Server.Writer.Session.Protocol), so
      // there's no separate incremental event to reconcile against it. Only
      // extra bit the client derives locally: if the active branch fell out
      // of this list, it was the one just deleted.
      mirrorServerEvent((s) => ({
        branches: evt.branches,
        activeBranch: s.activeBranch !== null && !evt.branches.includes(s.activeBranch) ? null : s.activeBranch,
      }));
    } else if (evt.type === "character.list") {
      mirrorServerEvent({ characterBranches: evt.characters });
    } else if (evt.type === "undo.log") {
      mirrorServerEvent({ undoEntries: evt.entries });
    } else if (evt.type === "error") {
      setError(evt.message);
    }
  });

  try {
    await session.connect();
    mirrorServerEvent({ _session: session });
  } catch (err) {
    setConnStatus("session", "error");
    setError(String(err));
  }
}

export function createBranch(name: string) {
  getServerCache()._session?.send({ type: "create-branch", branch: name });
}

export function deleteBranch(name: string) {
  getServerCache()._session?.send({ type: "delete-branch", branch: name });
}

// Jump the whole session (every branch, shared across every connected
// client) to any entry in the undo log — see app/undo-timeline.tsx.
export function resetToUndo(entryId: string) {
  getServerCache()._session?.send({ type: "undo.reset", entryId });
}

export async function selectBranch(name: string): Promise<void> {
  const prev = getServerCache()._branch;
  const prevLibrary = getServerCache()._library;
  const prevName = getServerCache().activeBranch;
  if (prev) {
    prev.close();
    if (prevName) removeConn(`branch:${prevName}`);
  }
  if (prevLibrary) {
    prevLibrary.close();
    if (prevName) removeConn(`library:${prevName}`);
  }

  for (const fc of Object.values(getServerCache().openFiles)) fc.conn.close();
  for (const cc of Object.values(getServerCache().openCharacters)) cc.conn.close();
  for (const jc of Object.values(getServerCache().openJournals)) jc.conn.close();

  const label = `branch:${name}`;
  mirrorServerEvent({
    activeBranch: name,
    files: [],
    ticks: {},
    branchHead: null,
    libraryTree: [],
    libraryChapters: [],
    openFiles: {},
    openCharacters: {},
    openJournals: {},
  });
  useUI.setState({
    journalMarkers: {},
    contextAtoms: new Set(),
    contextAnnotations: new Set(),
    rebaseMarker: null,
    pendingSubmit: null,
    hoverHighlight: null,
  });
  setConnStatus(label, "connecting");

  const branch = branchConn(name);

  branch.onStatus((s) => {
    if (s !== "connected") setConnStatus(label, "connecting");
  });

  branch.subscribe((evt) => {
    bumpActivity(label);
    if (evt.type === "branch.ready") {
      mirrorServerEvent({ files: [...evt.files].sort() });
      setConnStatus(label, "connected");
    } else if (evt.type === "file.added") {
      mirrorServerEvent((s) => ({
        files: s.files.includes(evt.path) ? s.files : [...s.files, evt.path].sort(),
      }));
    } else if (evt.type === "file.removed") {
      mirrorServerEvent((s) => ({ files: s.files.filter((f) => f !== evt.path) }));
    } else if (evt.type === "update") {
      clearPreviewDelayTimer();
      mirrorServerEvent((s) => ({ ticks: applyUpdate(s.ticks, evt), branchHead: evt.head, preview: null }));
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
    await branch.connect();
    mirrorServerEvent({ _branch: branch });
  } catch (err) {
    setConnStatus(label, "error");
    setError(String(err));
  }

  // A second, independent connection (see WS-PROTOCOL.md's /library/{name})
  // — it carries genuinely different, derived data (per-node kind, chapter
  // numbers, headings) the branch connection's own plain file list doesn't,
  // so it isn't worth trying to piggyback on 'branch' above.
  const libraryLabel = `library:${name}`;
  setConnStatus(libraryLabel, "connecting");

  const library = libraryConn(name);

  library.onStatus((s) => {
    if (s !== "connected") setConnStatus(libraryLabel, "connecting");
  });

  library.subscribe((evt) => {
    bumpActivity(libraryLabel);
    if (evt.type === "library.tree") {
      mirrorServerEvent({ libraryTree: evt.nodes, libraryChapters: evt.chapters });
      setConnStatus(libraryLabel, "connected");
    } else if (evt.type === "error") {
      setError(evt.message);
    }
  });

  try {
    await library.connect();
    mirrorServerEvent({ _library: library });
  } catch (err) {
    setConnStatus(libraryLabel, "error");
    setError(String(err));
  }
}

// Introduce a new chapter file, seeded with its heading — see
// WS-PROTOCOL.md's /library/{name} chapter.create. The resulting file
// itself reaches this connection (and every other open one) via the
// library/branch connections' own ref-move pushes, same as any other write;
// nothing needs to be applied optimistically here.
export function createChapter(path: string, name: string) {
  getServerCache()._library?.send({ type: "chapter.create", path, name });
}

// Drag-and-drop upload — one or more dropped files, written directly to
// their paths via HTTP PUT (see 'uploadBranchFile'), no chat-agent round
// trip and no WS/JSON text detour for the bytes. 'files' are already
// path-resolved (destination folder + dropped filename) by the caller.
// Each PUT is independent, so one file's fetch failing (surfaced via
// 'error', same as a WS command's) doesn't block the others; the branch
// connection's own ref-move notification isn't relied on here — the file
// list is updated optimistically on that upload's own success instead
// (see the FIXME in Server.Writer.Branch.Protocol).
export function uploadFiles(files: { path: string; content: Blob }[]) {
  const branch = getServerCache().activeBranch;
  if (!branch) return;
  files.forEach(({ path, content }) => {
    uploadBranchFile(branch, path, content)
      .then(() => mirrorServerEvent((s) => ({
        files: s.files.includes(path) ? s.files : [...s.files, path].sort(),
      })))
      .catch((err) => setError(err instanceof Error ? err.message : String(err)));
  });
}
