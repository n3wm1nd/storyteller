"use client";

// Client-local, freely-writable state: selection, connection status,
// in-flight-command bookkeeping, anything a component wants to set to give
// immediate feedback ahead of a server response. None of this is a cache of
// server data — see lib/serverCacheStore.ts for that, which this store must
// never reach into and mutate directly (reading it is fine).

import { create } from "zustand";

export type ConnStatus = "connecting" | "connected" | "disconnected" | "error";

export interface ConnInfo {
  label: string;
  status: ConnStatus;
}

interface UIState {
  conns: ConnInfo[];
  // Per-label "message just arrived" timestamp, split out of `conns` itself
  // so a pulse ping on a background connection (any open file/character's
  // WS traffic bumps its own label on every message) doesn't force a
  // reference change on `conns` — which every render of the root page reads
  // for session status — and cascade a full-app re-render on every message.
  // Only the connection-list pulse animation (sidebar.tsx) subscribes to this.
  connActivity: Record<string, number>;
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
  // state, cleared on branch/file change. A real tickId, not a depth: an
  // independent write elsewhere on the same chain (another connection or
  // background agent appending straight to HEAD, outside this marker
  // entirely) must leave it pointing at the exact same tick — a
  // depth-from-head count would silently drift in that case, since HEAD
  // moved for a reason that has nothing to do with this marker. It only
  // ever needs correcting when a command sent *through* this marker itself
  // rebases the tail after it — see fileview.actions.ts's handling of
  // 'tick.remap' — never in response to unrelated chain growth.
  rebaseMarker: string | null;

  // Agent log entries (ephemeral, capped ring buffer). Streamed from the
  // server like everything in serverCacheStore.ts, but explicitly *not*
  // part of synced state (see WS-PROTOCOL.md's "agent log" section) — a
  // user-triggered "clear" is legitimate here in a way it never is for the
  // real cache, which is exactly why this lives here and not there.
  agentLogs: { level: string; message: string }[];

  // Answers to ask.character commands (ephemeral, capped ring buffer) —
  // same shape/lifetime as agentLogs: streamed from the server, explicitly
  // not part of synced state (the exchange itself is server-recorded as a
  // CharacterAnswer tick, but the client doesn't need to track that tick,
  // only show the answer once).
  characterAnswers: { character: string; question: string; answer: string }[];

  setHoverHighlight:   (tickIds: Set<string>, color: string) => void;
  clearHoverHighlight: () => void;
  toggleContextAtom:       (tickId: string) => void;
  toggleContextAnnotation: (tickId: string) => void;
  clearContext:            () => void;
  clearAgentLogs:          () => void;
  setRebaseMarker:         (tickId: string | null) => void;
  setJournalMarker:        (branch: string, tickId: string | null) => void;
  addAgentLog:             (level: string, message: string) => void;
  addCharacterAnswer:      (character: string, question: string, answer: string) => void;

  // A pending "insert this @mention into the active composer" request,
  // set by any UI surface that wants to drop a mention at the cursor
  // (currently the Codex tab's lore cards — see lore-selector.tsx's
  // ContextAwareLoreCard) and consumed by InputBar's effect, which
  // inserts `@[name](path) ` at the textarea's caret and clears the
  // request. The `ts` disambiguates repeated requests for the same
  // path (otherwise the effect wouldn't re-fire on identical content).
  // Null when no request is pending.
  pendingMention: { name: string; path: string; ts: number } | null;
  requestMention: (name: string, path: string) => void;
  clearPendingMention: () => void;

  // InputBar's own sticky send-mode ("write"/"fix"/"append"/"note"/"regen"/
  // "roleplay" — see fileview.tsx's AgentId), lifted out of InputBar's local
  // state so other surfaces can read "what agent is currently selected"
  // without prop-drilling through it — first reader is the Cost tab
  // (context-cost-sidebar.tsx), which estimates context for whichever agent
  // the user would actually send to right now. InputBar still owns writing
  // it (cycling, clicking a mode, and the initial per-file seed all happen
  // there); this is just the shared read/write cell.
  writerMode: string;
  setWriterMode: (mode: string) => void;

  // Which branch the .dsl file editor (app/dsl-file-view.tsx) resolves the
  // program against for its cost gutter — never the branch the file itself
  // lives on, which for a `context/*.dsl` is the contexts branch and has no
  // story content to resolve against at all. Sticky across file switches
  // (same reasoning as writerMode above: it's a standing "which branch am I
  // tuning this for" choice, not a per-file one), and validated against the
  // live branch list by its reader rather than kept in sync here.
  dslResolveBranch: string | null;
  setDslResolveBranch: (branch: string) => void;

  // The .dsl editor's current buffer, shared with its own sidebar (see
  // app/dsl-file-view.tsx) so the "what does this resolve to" pane follows
  // what's actually being typed rather than the last saved version. One
  // cell, not a map: exactly one .dsl editor is ever open, and it names the
  // path it belongs to so a stale draft can't be read against a file it
  // isn't from. Null when no .dsl editor is mounted.
  dslDraft: { path: string; text: string } | null;
  setDslDraft: (path: string, text: string) => void;
  clearDslDraft: () => void;
}

export const useUI = create<UIState>((set) => ({
  conns: [],
  connActivity: {},
  error: null,
  journalMarkers: {},
  hoverHighlight: null,
  contextAtoms: new Set(),
  contextAnnotations: new Set(),
  rebaseMarker: null,
  agentLogs: [],
  characterAnswers: [],

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

  addCharacterAnswer: (character, question, answer) => set((s) => ({
    characterAnswers: [...s.characterAnswers, { character, question, answer }].slice(-50),
  })),

  pendingMention: null,
  requestMention: (name, path) => set({ pendingMention: { name, path, ts: Date.now() } }),
  clearPendingMention: () => set({ pendingMention: null }),

  writerMode: "write",
  setWriterMode: (mode) => set({ writerMode: mode }),

  dslResolveBranch: null,
  setDslResolveBranch: (branch) => set({ dslResolveBranch: branch }),

  dslDraft: null,
  setDslDraft: (path, text) => set({ dslDraft: { path, text } }),
  clearDslDraft: () => set({ dslDraft: null }),
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
  useUI.setState((s) => ({ connActivity: { ...s.connActivity, [label]: Date.now() } }));
}

export function setError(message: string) {
  useUI.setState({ error: message });
}
