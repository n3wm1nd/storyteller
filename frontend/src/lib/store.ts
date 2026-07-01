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

function handleError(set: Setter, evt: ErrorEvent) {
  set(() => ({ error: evt.message }));
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
    get().openFiles[path]?.conn.send({ type: "append", content: content.replace(/^\/+/, "") });
  },

  editAtom: (path, tickId, content) => {
    get().openFiles[path]?.conn.send({ type: "edit.atom", tickId, content });
  },

  deleteAtom: (path, tickId) => {
    get().openFiles[path]?.conn.send({ type: "delete.atom", tickId });
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
    get()._branch?.send({ type: "chat.prompt", path, text });
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
}));
