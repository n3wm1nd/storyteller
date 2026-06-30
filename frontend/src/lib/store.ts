"use client";

import { create } from "zustand";
import {
  sessionConn, branchConn, fileConn,
  type StoryWS,
  type FileAtom,
  type IdMapping,
  type SessionCommand, type SessionEvent,
  type BranchCommand,  type BranchEvent,
  type FileCommand,    type FileEvent,
} from "./ws";

export type { FileAtom };

export type ConnStatus = "connecting" | "connected" | "disconnected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
}

// A file connection: the WS handle plus the atom chain.
export interface FileConn {
  path: string;
  atoms: FileAtom[];   // empty + absent=true means file.absent
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

  // Open file connections keyed by path
  openFiles: Record<string, FileConn>;

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

// ── Store ─────────────────────────────────────────────────────────────────────

export const useStory = create<StoryState>((set, get) => ({
  conns: [],
  error: null,
  branches: [],
  activeBranch: null,
  files: [],
  openFiles: {},
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
      openFiles: {},
      conns: setConnStatus(s.conns, label, "connecting"),
    }));

    const branch = branchConn(name);

    branch.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    branch.subscribe((evt) => {
      if (evt.type === "branch.ready") {
        set((s) => ({
          files: [...evt.files].sort(),
          conns: setConnStatus(s.conns, label, "connected"),
        }));
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
      if (evt.type === "file.atoms") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, atoms: evt.atoms, absent: false, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.absent") {
        set((s) => ({
          openFiles: { ...s.openFiles, [path]: { path, atoms: [], absent: true, conn: fc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "atom.appended") {
        set((s) => {
          const prev = s.openFiles[path];
          const atoms = prev ? [...prev.atoms, evt.atom] : [evt.atom];
          return { openFiles: { ...s.openFiles, [path]: { path, atoms, absent: false, conn: fc } } };
        });
      } else if (evt.type === "atom.replaced") {
        // tickId changes on edit, so patch by oldTickId then reload to get consistent ids.
        fc.send({ type: "read" });
      } else if (evt.type === "atom.deleted") {
        set((s) => {
          const prev = s.openFiles[path];
          if (!prev) return {};
          const remap = new Map(evt.mapping.map((m: IdMapping) => [m.old, m.new]));
          const atoms = prev.atoms
            .filter((a) => a.tickId !== evt.oldTickId)
            .map((a) => remap.has(a.tickId) ? { ...a, tickId: remap.get(a.tickId)! } : a);
          return { openFiles: { ...s.openFiles, [path]: { ...prev, atoms } } };
        });
      } else if (evt.type === "atom.moved") {
        // Full re-fetch is safest — the chain order changed and all ids shifted.
        get().openFiles[path]?.conn.send({ type: "read" });
      } else if (evt.type === "error") {
        set({ error: evt.message });
      }
    });

    await fc.connect();
    set((s) => ({ openFiles: { ...s.openFiles, [path]: { path, atoms: [], absent: false, conn: fc } } }));
  },

  closeFile: (path) => {
    const { openFiles } = get();
    openFiles[path]?.conn.close();
    set((s) => {
      const next = { ...s.openFiles };
      delete next[path];
      return {
        openFiles: next,
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
}));
