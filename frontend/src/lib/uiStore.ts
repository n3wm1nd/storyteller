"use client";

// Client-local, freely-writable state: selection, connection status,
// in-flight-command bookkeeping, anything a component wants to set to give
// immediate feedback ahead of a server response. None of this is a cache of
// server data — see lib/serverCacheStore.ts for that, which this store must
// never reach into and mutate directly (reading it is fine).

import { create } from "zustand";
import type { FileCommand } from "./ws";

export type ConnStatus = "connecting" | "connected" | "disconnected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
  lastActivity?: number;
}

interface UIState {
  conns: ConnInfo[];
  error: string | null;

  // Each active character's own independent time-travel position in their
  // journal (see lib/utils.nearestJournalMarker) — global, not local
  // component state, so it (a) keeps updating while the accordion row is
  // collapsed and (b) is addressable by both the journal panel's own scrub
  // handle and any future cross-character action (e.g. a unified delete).
  journalMarkers: Record<string, string | null>;

  // A single cross-component "glow these ticks" highlight — e.g. hovering a
  // journal entry highlights the main-view atom(s) it was tracked from (via
  // that atom's 'refs'). Deliberately global rather than routed through a
  // prop chain: any view keyed by tickId can read this directly and doesn't
  // need to know who set it or why. Rendered in fileview.tsx by folding it
  // into the same presence-bar mechanism (tickIds + color -> a colored run).
  hoverHighlight: { tickIds: Set<string>; color: string } | null;

  // Context selection — local UI state, cleared on branch/file change
  contextAtoms:       Set<string>;
  contextAnnotations: Set<string>;

  // Rebase marker (CAD-style feature-tree rollback) — when set, mutating
  // file commands run rebased at this tick instead of at HEAD. Local UI
  // state, cleared on branch/file change.
  rebaseMarker: string | null;

  // Agent log entries (ephemeral, capped ring buffer). Streamed from the
  // server like everything in serverCacheStore.ts, but explicitly *not*
  // part of synced state (see WS-PROTOCOL.md's "agent log" section) — a
  // user-triggered "clear" is legitimate here in a way it never is for the
  // real cache, which is exactly why this lives here and not there.
  agentLogs: { level: string; message: string }[];

  // A chat.writer/chat.fixer/chat.append command submitted while a previous
  // generation ('preview') was still in flight. Held here instead of sent
  // immediately (the server processes one command at a time per
  // connection), and flushed the moment the in-flight generation's
  // "chat.preview.end" arrives. See project notes on FlowWriter — a queued
  // chat.writer captures 'flowTid' (HEAD at queue-time) into its command
  // when it's built, not when it's flushed.
  pendingSubmit: { path: string; cmd: FileCommand } | null;

  setHoverHighlight:   (tickIds: Set<string>, color: string) => void;
  clearHoverHighlight: () => void;
  toggleContextAtom:       (tickId: string) => void;
  toggleContextAnnotation: (tickId: string) => void;
  clearContext:            () => void;
  clearAgentLogs:          () => void;
  setRebaseMarker:         (tickId: string | null) => void;
  setJournalMarker:        (branch: string, tickId: string | null) => void;
  addAgentLog:             (level: string, message: string) => void;
}

export const useUI = create<UIState>((set) => ({
  conns: [],
  error: null,
  journalMarkers: {},
  hoverHighlight: null,
  contextAtoms: new Set(),
  contextAnnotations: new Set(),
  rebaseMarker: null,
  pendingSubmit: null,
  agentLogs: [],

  setHoverHighlight: (tickIds, color) => set({ hoverHighlight: { tickIds, color } }),
  clearHoverHighlight: () => set({ hoverHighlight: null }),

  toggleContextAtom: (tickId) => set((s) => {
    const next = new Set(s.contextAtoms);
    if (next.has(tickId)) next.delete(tickId); else next.add(tickId);
    return { contextAtoms: next };
  }),

  toggleContextAnnotation: (tickId) => set((s) => {
    const next = new Set(s.contextAnnotations);
    if (next.has(tickId)) next.delete(tickId); else next.add(tickId);
    return { contextAnnotations: next };
  }),

  clearContext: () => set({ contextAtoms: new Set(), contextAnnotations: new Set() }),

  clearAgentLogs: () => set({ agentLogs: [] }),

  setRebaseMarker: (tickId) => set({ rebaseMarker: tickId }),

  setJournalMarker: (branch, tickId) => set((s) => ({ journalMarkers: { ...s.journalMarkers, [branch]: tickId } })),

  addAgentLog: (level, message) => set((s) => ({ agentLogs: [...s.agentLogs, { level, message }].slice(-200) })),
}));

// Selection is a temporary "about to act on this" marker, not a durable
// reference — once an action consumes its targets (merge/split/delete/edit),
// those ids are done being selected, regardless of what the server's remap
// eventually resolves them to. Called with the ids the just-sent command
// itself names, so it doesn't need to wait on (or reason about) the
// tick.remap round trip at all.
export function dropFromSelection(ids: string[]) {
  if (ids.length === 0) return;
  useUI.setState((s) => {
    const drop = new Set(ids);
    return {
      contextAtoms: new Set([...s.contextAtoms].filter((id) => !drop.has(id))),
      contextAnnotations: new Set([...s.contextAnnotations].filter((id) => !drop.has(id))),
    };
  });
}

export function setConnStatus(label: string, status: ConnStatus) {
  useUI.setState((s) => {
    const existing = s.conns.find((c) => c.label === label);
    const conns = existing
      ? s.conns.map((c) => (c.label === label ? { ...c, status } : c))
      : [...s.conns, { label, status }];
    return { conns };
  });
}

export function removeConn(label: string) {
  useUI.setState((s) => ({ conns: s.conns.filter((c) => c.label !== label) }));
}

export function bumpActivity(label: string) {
  useUI.setState((s) => ({ conns: s.conns.map((c) => (c.label === label ? { ...c, lastActivity: Date.now() } : c)) }));
}

export function setError(message: string) {
  useUI.setState({ error: message });
}
