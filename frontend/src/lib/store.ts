"use client";

import { create } from "zustand";
import {
  sessionConn, branchConn, fileConn,
  type StoryWS,
  type FileTick,
  type BranchTick,
  type IdMapping,
  type ContextItem,
  type SessionCommand, type SessionEvent,
  type BranchCommand,  type BranchEvent,
  type FileCommand,    type FileEvent,
} from "./ws";

export type { ContextItem };

export type { BranchTick };

export type { FileTick };

export type ConnStatus = "connecting" | "connected" | "disconnected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
  lastActivity?: number;  // Date.now() timestamp of last received message
}

// A file connection: the WS handle plus the tick list.
export interface FileConn {
  path: string;
  ticks: FileTick[];   // empty + absent=true means file.absent
  absent: boolean;
  conn: StoryWS<FileCommand, FileEvent>;
}

interface StoryState {
  conns: ConnInfo[];
  error: string | null;

  // Session level
  branches: string[];

  // Branch level
  activeBranch: string | null;
  files: string[];   // file paths only — content lives in openFiles
  ticks: BranchTick[];

  // Open file connections keyed by path
  openFiles: Record<string, FileConn>;

  // Agent log entries (ephemeral, capped ring buffer)
  agentLogs: { level: string; message: string }[];

  // Context selection (per file-connection; cleared on close/branch change)
  contextAtoms:       Set<string>;
  contextAnnotations: Set<string>;

  // Actions
  connect: () => Promise<void>;
  createBranch: (name: string) => void;
  deleteBranch: (name: string) => void;
  selectBranch: (name: string) => Promise<void>;
  openFile: (path: string) => Promise<void>;
  closeFile: (path: string) => void;
  appendToFile: (path: string, content: string) => void;
  editAtom: (path: string, tickId: string, content: string) => void;
  deleteAtom: (path: string, tickId: string) => void;
  addNote: (refTickId: string, text: string) => void;
  moveTick: (tickId: string, afterTickId?: string) => void;
  deleteTickEntry: (tickId: string) => void;
  toggleContextAtom: (tickId: string) => void;
  toggleContextAnnotation: (tickId: string) => void;
  clearContext: () => void;
  clearAgentLogs: () => void;
  chatPrompt: (path: string, text: string) => void;

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

// ── Store ─────────────────────────────────────────────────────────────────────

export const useStory = create<StoryState>((set, get) => ({
  conns: [],
  error: null,
  branches: [],
  activeBranch: null,
  files: [],
  ticks: [],
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
        set({ error: evt.message });
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

    // Close any open file connections from the previous branch.
    const { openFiles } = get();
    for (const fc of Object.values(openFiles)) fc.conn.close();

    const label = `branch:${name}`;
    set((s) => ({
      activeBranch: name,
      files: [],
      ticks: [],
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
      } else if (evt.type === "branch.ticks") {
        set({ ticks: evt.ticks });
      } else if (evt.type === "agent.log") {
        set((s) => ({
          agentLogs: [...s.agentLogs, { level: evt.level, message: evt.message }].slice(-200),
        }));
      } else if (evt.type === "ticks.invalidated") {
        // Apply id renames optimistically, then re-fetch for accuracy.
        const remap = new Map(evt.mapping.map((m: IdMapping) => [m.old, m.new]));
        set((s) => ({
          ticks: s.ticks.map((t) =>
            remap.has(t.tickId) ? { ...t, tickId: remap.get(t.tickId)! } : t
          ),
        }));
        get()._branch?.send({ type: "read.ticks" });
      } else if (evt.type === "error") {
        set({ error: evt.message });
      }
    });

    await branch.connect();
    set({ _branch: branch });
  },

  openFile: async (path) => {
    const { activeBranch, openFiles } = get();
    if (!activeBranch) return;
    if (openFiles[path]) return;  // already open

    const label = `file:${path}`;
    set((s) => ({ conns: setConnStatus(s.conns, label, "connecting") }));

    const fc = fileConn(activeBranch, path);

    fc.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    fc.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, label) }));
      if (evt.type === "file.ticks") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, ticks: evt.ticks, absent: false, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.absent") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, ticks: [], absent: true, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "tick.appended") {
        set((s) => {
          const prev = s.openFiles[path];
          const ticks = prev ? [...prev.ticks, evt.tick] : [evt.tick];
          return { openFiles: { ...s.openFiles, [path]: { path, ticks, absent: false, conn: fc } } };
        });
      } else if (evt.type === "atom.replaced") {
        // tickId changes on edit, so re-fetch to get consistent ids.
        fc.send({ type: "read" });
      } else if (evt.type === "atom.deleted") {
        set((s) => {
          const prev = s.openFiles[path];
          if (!prev) return {};
          const remap = new Map(evt.mapping.map((m: IdMapping) => [m.old, m.new]));
          const ticks = prev.ticks
            .filter((t) => t.tickId !== evt.oldTickId)
            .map((t) => remap.has(t.tickId) ? { ...t, tickId: remap.get(t.tickId)! } : t);
          return { openFiles: { ...s.openFiles, [path]: { ...prev, ticks } } };
        });
      } else if (evt.type === "atom.moved") {
        // Full re-fetch is safest — the chain order changed and all ids shifted.
        get().openFiles[path]?.conn.send({ type: "read" });
      } else if (evt.type === "agent.log") {
        set((s) => ({
          agentLogs: [...s.agentLogs, { level: evt.level, message: evt.message }].slice(-200),
        }));
      } else if (evt.type === "error") {
        set({ error: evt.message });
      }
    });

    await fc.connect();
    set((s) => ({ openFiles: { ...s.openFiles, [path]: { path, ticks: [], absent: false, conn: fc } } }));
  },

  closeFile: (path) => {
    const { openFiles } = get();
    openFiles[path]?.conn.close();
    set((s) => {
      const next = { ...s.openFiles };
      delete next[path];
      return {
        openFiles: next,
        contextAtoms: new Set(),
        contextAnnotations: new Set(),
        conns: removeConn(s.conns, `file:${path}`),
      };
    });
  },

  appendToFile: (path, content) => {
    const fc = get().openFiles[path];
    if (fc) {
      fc.conn.send({ type: "append", content: content.replace(/^\/+/, "") });
    }
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

  chatPrompt: (path, text) => {
    const { contextAtoms, contextAnnotations, openFiles } = get();
    const ticks = openFiles[path]?.ticks ?? [];

    let context: ContextItem[] | undefined;
    const hasContext = contextAtoms.size > 0 || contextAnnotations.size > 0;
    if (hasContext) {
      // Build context items in chain order (oldest first), atoms before their annotations.
      context = [];
      for (const tick of ticks) {
        if (tick.kind === "atom" && contextAtoms.has(tick.tickId)) {
          context.push({ tickId: tick.tickId, kind: tick.kind, content: tick.content ?? tick.message });
        } else if (tick.kind !== "atom" && contextAnnotations.has(tick.tickId)) {
          context.push({ tickId: tick.tickId, kind: tick.kind, content: tick.message });
        }
      }
    }

    get()._branch?.send({ type: "chat.prompt", path, text, ...(context ? { context } : {}) });
  },
}));
