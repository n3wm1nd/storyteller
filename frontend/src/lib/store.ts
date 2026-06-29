"use client";

import { create } from "zustand";
import { sessionConn, branchConn, type StoryWS, type SessionCommand, type SessionEvent, type BranchCommand, type BranchEvent } from "./ws";

export type ConnStatus = "disconnected" | "connecting" | "connected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
}

interface StoryState {
  // Per-connection status (session + any open branch connections)
  conns: ConnInfo[];

  error: string | null;
  branches: string[];
  activeBranch: string | null;
  files: Record<string, string>;

  connect: () => Promise<void>;
  listBranches: () => void;
  createBranch: (name: string) => void;
  deleteBranch: (name: string) => void;
  selectBranch: (name: string) => Promise<void>;
  appendToFile: (path: string, content: string) => void;
  readFile: (path: string) => void;

  _session: StoryWS<SessionCommand, SessionEvent> | null;
  _branch: StoryWS<BranchCommand, BranchEvent> | null;
}

function setConnStatus(conns: ConnInfo[], label: string, status: ConnStatus): ConnInfo[] {
  const existing = conns.find((c) => c.label === label);
  if (existing) return conns.map((c) => c.label === label ? { ...c, status } : c);
  return [...conns, { label, status }];
}

function removeConn(conns: ConnInfo[], label: string): ConnInfo[] {
  return conns.filter((c) => c.label !== label);
}

export const useStory = create<StoryState>((set, get) => ({
  conns: [],
  error: null,
  branches: [],
  activeBranch: null,
  files: {},
  _session: null,
  _branch: null,

  connect: async () => {
    set((s) => ({ conns: setConnStatus(s.conns, "session", "connecting"), error: null }));

    const session = sessionConn();

    session.onStatus((s) => {
      const status: ConnStatus = s === "connected" ? "connected" : "connecting";
      set((st) => ({ conns: setConnStatus(st.conns, "session", status) }));
    });

    session.subscribe((evt) => {
      if (evt.type === "session.ready") {
        // Server is ready to receive commands — safe to request branch list.
        // Fires on every (re)connect since server state resets each time.
        set((s) => ({ conns: setConnStatus(s.conns, "session", "connected") }));
        get().listBranches();
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
      set((s) => ({
        conns: setConnStatus(s.conns, "session", "error"),
        error: String(err),
      }));
    }
  },

  listBranches: () => {
    get()._session?.send({ type: "list-branches" });
  },

  createBranch: (name) => {
    get()._session?.send({ type: "create-branch", branch: name });
  },

  deleteBranch: (name) => {
    get()._session?.send({ type: "delete-branch", branch: name });
  },

  selectBranch: async (name) => {
    const prev = get()._branch;
    const prevBranch = get().activeBranch;
    if (prev) {
      prev.close();
      if (prevBranch) set((s) => ({ conns: removeConn(s.conns, `branch:${prevBranch}`) }));
    }

    const label = `branch:${name}`;
    set((s) => ({
      activeBranch: name,
      files: {},
      conns: setConnStatus(s.conns, label, "connecting"),
    }));

    const branch = branchConn(name);

    branch.onStatus((s) => {
      const status: ConnStatus = s === "connected" ? "connected" : "connecting";
      set((st) => ({ conns: setConnStatus(st.conns, label, status) }));
    });

    branch.subscribe((evt) => {
      if (evt.type === "branch.ready") {
        set((s) => ({
          files: evt.files,
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.content" || evt.type === "file.updated") {
        set((s) => ({ files: { ...s.files, [evt.path]: evt.content } }));
      } else if (evt.type === "error") {
        set({ error: evt.message });
      }
    });

    await branch.connect();
    set({ _branch: branch });
  },

  appendToFile: (path, content) => {
    get()._branch?.send({ type: "append", path: path.replace(/^\/+/, ""), content });
  },

  readFile: (path) => {
    get()._branch?.send({ type: "read", path: path.replace(/^\/+/, "") });
  },
}));
