"use client";

import { create } from "zustand";
import {
  sessionConn, branchConn, fileConn,
  type StoryWS,
  type WireTick,
  type Update,
  type AgentLogEvent,
  type ErrorEvent,
  type SessionCommand, type SessionEvent,
  type BranchCommand,  type BranchEvent,
  type FileCommand,    type FileEvent,
} from "./ws";
import { tickChain } from "./utils";

export type { WireTick };

export type ConnStatus = "connecting" | "connected" | "disconnected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
  lastActivity?: number;
}

// A file connection: the WS handle, tick store, and current head.
export interface FileConn {
  path:   string;
  ticks:  Record<string, WireTick>;
  head:   string | null;
  absent: boolean;
  conn:   StoryWS<FileCommand, FileEvent>;
}

interface StoryState {
  conns: ConnInfo[];
  error: string | null;

  // Session level
  branches: string[];

  // Branch level
  activeBranch: string | null;
  files:        string[];
  ticks:        Record<string, WireTick>;
  branchHead:   string | null;

  // Open file connections keyed by path
  openFiles: Record<string, FileConn>;

  // Agent log entries (ephemeral, capped ring buffer)
  agentLogs: { level: string; message: string }[];

  // Context selection — local UI state, cleared on branch/file change
  contextAtoms:       Set<string>;
  contextAnnotations: Set<string>;

  // Rebase marker (CAD-style feature-tree rollback) — when set, mutating
  // file commands run rebased at this tick instead of at HEAD. Local UI
  // state, cleared on branch/file change.
  rebaseMarker: string | null;

  // Actions
  connect:        () => Promise<void>;
  createBranch:   (name: string) => void;
  deleteBranch:   (name: string) => void;
  selectBranch:   (name: string) => Promise<void>;
  openFile:       (path: string) => Promise<void>;
  closeFile:      (path: string) => void;
  appendToFile:   (path: string, content: string) => void;
  editAtom:       (path: string, tickId: string, content: string) => void;
  deleteAtom:     (path: string, tickId: string) => void;
  addNote:        (refTickId: string, text: string) => void;
  moveTick:       (tickId: string, afterTickId?: string) => void;
  deleteTickEntry:(tickId: string) => void;
  chatPrompt:              (path: string, text: string) => void;
  toggleContextAtom:       (tickId: string) => void;
  toggleContextAnnotation: (tickId: string) => void;
  clearContext:            () => void;
  clearAgentLogs:          () => void;
  setRebaseMarker:         (tickId: string | null) => void;

  _session: StoryWS<SessionCommand, SessionEvent> | null;
  _branch:  StoryWS<BranchCommand,  BranchEvent>  | null;
}

// ── Helpers ───────────────────────────────────────────────────────────────────

function setConnStatus(conns: ConnInfo[], label: string, status: ConnStatus): ConnInfo[] {
  const existing = conns.find((c) => c.label === label);
  if (existing) return conns.map((c) => c.label === label ? { ...c, status } : c);
  return [...conns, { label, status }];
}

function removeConn(conns: ConnInfo[], label: string): ConnInfo[] {
  return conns.filter((c) => c.label !== label);
}

function bumpActivity(conns: ConnInfo[], label: string): ConnInfo[] {
  return conns.map((c) => c.label === label ? { ...c, lastActivity: Date.now() } : c);
}

type Setter = (fn: (s: StoryState) => Partial<StoryState>) => void;

function applyUpdate(ticks: Record<string, WireTick>, upd: Update): Record<string, WireTick> {
  const next = { ...ticks };
  for (const t of upd.ticks) next[t.tickId] = t;
  return next;
}

function handleAgentLog(set: Setter, evt: AgentLogEvent) {
  set((s) => ({ agentLogs: [...s.agentLogs, { level: evt.level, message: evt.message }].slice(-200) }));
}

// Apply a server-computed old->new tickId remap (from a rebase/replace/move)
// to every locally held tickId reference. The server is the source of truth
// for how ids move — this just relocates whatever we're tracking rather than
// leaving it pointing at an id that no longer exists.
function remapTickId(table: Map<string, string>, id: string): string {
  return table.get(id) ?? id;
}

function remapSet(table: Map<string, string>, ids: Set<string>): Set<string> {
  let changed = false;
  const next = new Set<string>();
  for (const id of ids) {
    const mapped = remapTickId(table, id);
    if (mapped !== id) changed = true;
    next.add(mapped);
  }
  return changed ? next : ids;
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

function handleTickRemap(set: Setter, path: string, mapping: [string, string][]) {
  const table = new Map(mapping);
  const toSet = new Set(mapping.map(([, to]) => to));
  set((s) => {
    const fc = s.openFiles[path];
    const marker = s.rebaseMarker ? remapTickId(table, s.rebaseMarker) : s.rebaseMarker;
    return {
      rebaseMarker: marker && fc ? advanceMarker(marker, fc.ticks, fc.head, toSet) : marker,
      contextAtoms: remapSet(table, s.contextAtoms),
      contextAnnotations: remapSet(table, s.contextAnnotations),
    };
  });
}

function handleError(set: Setter, evt: ErrorEvent) {
  set(() => ({ error: evt.message }));
}

// Wrap a command in an "at" rebase if a marker is set, otherwise send it as-is.
function atRebase(marker: string | null, cmd: FileCommand): FileCommand {
  return marker ? { type: "at", tickId: marker, command: cmd } : cmd;
}

// ── Store ─────────────────────────────────────────────────────────────────────

export const useStory = create<StoryState>((set, get) => ({
  conns: [],
  error: null,
  branches: [],
  activeBranch: null,
  files: [],
  ticks: {},
  branchHead: null,
  openFiles: {},
  agentLogs: [],
  contextAtoms: new Set(),
  contextAnnotations: new Set(),
  rebaseMarker: null,
  _session: null,
  _branch: null,

  connect: async () => {
    set((s) => ({ conns: setConnStatus(s.conns, "session", "connecting"), error: null }));

    const session = sessionConn();

    session.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, "session", "connecting") }));
    });

    session.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, "session") }));
      if (evt.type === "session.ready") {
        set((s) => ({ conns: setConnStatus(s.conns, "session", "connected") }));
        get()._session?.send({ type: "list-branches" });
      } else if (evt.type === "branch.list") {
        set({ branches: evt.branches });
      } else if (evt.type === "branch.created") {
        set((s) => ({ branches: [...s.branches, evt.branch].sort() }));
      } else if (evt.type === "branch.deleted") {
        set((s) => ({
          branches: s.branches.filter((b) => b !== evt.branch),
          activeBranch: s.activeBranch === evt.branch ? null : s.activeBranch,
        }));
      } else if (evt.type === "error") {
        handleError(set, evt);
      }
    });

    try {
      await session.connect();
      set({ _session: session });
    } catch (err) {
      set((s) => ({ conns: setConnStatus(s.conns, "session", "error"), error: String(err) }));
    }
  },

  createBranch: (name) => {
    get()._session?.send({ type: "create-branch", branch: name });
  },

  deleteBranch: (name) => {
    get()._session?.send({ type: "delete-branch", branch: name });
  },

  selectBranch: async (name) => {
    const prev = get()._branch;
    const prevName = get().activeBranch;
    if (prev) {
      prev.close();
      if (prevName) set((s) => ({ conns: removeConn(s.conns, `branch:${prevName}`) }));
    }

    for (const fc of Object.values(get().openFiles)) fc.conn.close();

    const label = `branch:${name}`;
    set((s) => ({
      activeBranch: name,
      files: [],
      ticks: {},
      branchHead: null,
      openFiles: {},
      contextAtoms: new Set(),
      contextAnnotations: new Set(),
      rebaseMarker: null,
      conns: setConnStatus(s.conns, label, "connecting"),
    }));

    const branch = branchConn(name);

    branch.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    branch.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, label) }));
      if (evt.type === "branch.ready") {
        set((s) => ({
          files: [...evt.files].sort(),
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.added") {
        set((s) => ({
          files: s.files.includes(evt.path) ? s.files : [...s.files, evt.path].sort(),
        }));
      } else if (evt.type === "update") {
        set((s) => ({ ticks: applyUpdate(s.ticks, evt), branchHead: evt.head }));
      } else if (evt.type === "agent.log") {
        handleAgentLog(set, evt);
      } else if (evt.type === "error") {
        handleError(set, evt);
      }
    });

    await branch.connect();
    set({ _branch: branch });
  },

  openFile: async (path) => {
    const { activeBranch, openFiles } = get();
    if (!activeBranch) return;
    set({ rebaseMarker: null });
    if (openFiles[path]) return;

    const label = `file:${path}`;
    set((s) => ({ conns: setConnStatus(s.conns, label, "connecting") }));

    const fc = fileConn(activeBranch, path);

    fc.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    fc.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, label) }));
      if (evt.type === "file.present") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.absent") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: true, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "update") {
        set((s) => {
          const prev = s.openFiles[path];
          if (!prev) return {};
          return {
            openFiles: {
              ...s.openFiles,
              [path]: { ...prev, ticks: applyUpdate(prev.ticks, evt), head: evt.head, absent: false },
            },
          };
        });
      } else if (evt.type === "tick.remap") {
        handleTickRemap(set, path, evt.mapping);
      } else if (evt.type === "agent.log") {
        handleAgentLog(set, evt);
      } else if (evt.type === "error") {
        handleError(set, evt);
      }
    });

    await fc.connect();
    set((s) => ({
      openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
    }));
  },

  closeFile: (path) => {
    get().openFiles[path]?.conn.close();
    set((s) => {
      const next = { ...s.openFiles };
      delete next[path];
      return { openFiles: next, conns: removeConn(s.conns, `file:${path}`) };
    });
  },

  appendToFile: (path, content) => {
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "append", content: content.replace(/^\/+/, "") })
    );
  },

  editAtom: (path, tickId, content) => {
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "edit.atom", tickId, content })
    );
  },

  deleteAtom: (path, tickId) => {
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "delete.atom", tickId })
    );
  },

  addNote: (refTickId, text) => {
    get()._branch?.send({ type: "add.note", refTickId, text });
  },

  moveTick: (tickId, afterTickId) => {
    get()._branch?.send({ type: "move.tick", tickId, afterTickId });
  },

  deleteTickEntry: (tickId) => {
    get()._branch?.send({ type: "delete.tick", tickId });
  },

  chatPrompt: (path, text) => {
    get().openFiles[path]?.conn.send(atRebase(get().rebaseMarker, { type: "chat.prompt", text }));
  },

  toggleContextAtom: (tickId) => {
    set((s) => {
      const next = new Set(s.contextAtoms);
      if (next.has(tickId)) next.delete(tickId); else next.add(tickId);
      return { contextAtoms: next };
    });
  },

  toggleContextAnnotation: (tickId) => {
    set((s) => {
      const next = new Set(s.contextAnnotations);
      if (next.has(tickId)) next.delete(tickId); else next.add(tickId);
      return { contextAnnotations: next };
    });
  },

  clearContext: () => {
    set({ contextAtoms: new Set(), contextAnnotations: new Set() });
  },

  clearAgentLogs: () => {
    set({ agentLogs: [] });
  },

  setRebaseMarker: (tickId) => {
    set({ rebaseMarker: tickId });
  },
}));
