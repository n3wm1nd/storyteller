"use client";

import { create } from "zustand";
import {
  sessionConn, branchConn, fileConn, characterConn,
  uploadBranchFile,
  type StoryWS,
  type WireTick,
  type Update,
  type AgentLogEvent,
  type ChatPreviewEvent,
  type ErrorEvent,
  type SessionCommand,   type SessionEvent,
  type BranchCommand,    type BranchEvent,
  type FileCommand,      type FileEvent,
  type CharacterEvent,
  type CharacterSummary,
  type ContextItem,
} from "./ws";
import { tickChain } from "./utils";

const JOURNAL_PATH = "journal.md";

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

// A character connection: no tick chain, just the flat sidebar snapshot the
// server composes and re-pushes on every change.
export interface CharacterConn {
  branch: string;
  name:   string;
  sheet:  string | null;
  conn:   StoryWS<never, CharacterEvent>;
}

interface StoryState {
  conns: ConnInfo[];
  error: string | null;

  // Session level
  branches: string[];
  // character/* branch list with raw sheet.md content, kept live by the
  // server's own notifier (see Server.Writer.Session.Connection) — unlike
  // 'branches', this reflects character branches created by any connection
  // (e.g. Track/CharGen from a branch connection), not just this session's
  // own create-branch/delete-branch commands. Sheet content is raw (see
  // WS-PROTOCOL.md); decode into a display name via lib/utils.characterDisplayName.
  characterBranches: CharacterSummary[];

  // Branch level
  activeBranch: string | null;
  files:        string[];
  ticks:        Record<string, WireTick>;
  branchHead:   string | null;

  // Open file connections keyed by path
  openFiles: Record<string, FileConn>;

  // Open character connections keyed by branch name — membership is driven
  // by presence ticks (see 'activeCharacterBranches' in lib/utils), reconciled
  // by whichever component renders the character sidebar, not by the store
  // itself; this just holds whatever's currently open.
  openCharacters: Record<string, CharacterConn>;

  // Read-only journal.md file connections, keyed by character branch —
  // opened/closed following scene presence, same lifecycle as
  // openCharacters (see character-sidebar.tsx), independent of whether the
  // accordion row happens to be expanded: the marker below needs to track
  // continuously even while collapsed.
  openJournals: Record<string, FileConn>;

  // Each active character's own independent time-travel position in their
  // journal (see lib/utils.nearestJournalMarker) — global, not local
  // component state, so it (a) keeps updating while the accordion row is
  // collapsed and (b) is addressable by both the journal panel's own scrub
  // handle and any future cross-character action (e.g. a unified delete).
  journalMarkers: Record<string, string | null>;

  // Agent log entries (ephemeral, capped ring buffer)
  agentLogs: { level: string; message: string }[];

  // A single cross-component "glow these ticks" highlight — e.g. hovering a
  // journal entry highlights the main-view atom(s) it was tracked from (via
  // that atom's 'refs'). Deliberately global rather than routed through a
  // prop chain: any view keyed by tickId can read this directly and doesn't
  // need to know who set it or why. Rendered in fileview.tsx by folding it
  // into the same presence-bar mechanism (tickIds + color -> a colored run).
  hoverHighlight: { tickIds: Set<string>; color: string } | null;

  // Streamed LLM draft for the in-flight chat.prompt/chargen command, if
  // any — a best-effort preview only. Cleared on chat.preview.end, and
  // defensively on any update/error, since it may not match (or may not be
  // followed by) anything actually persisted. See WS-PROTOCOL.md.
  preview: { text: string; thinking: string } | null;

  // Context selection — local UI state, cleared on branch/file change
  contextAtoms:       Set<string>;
  contextAnnotations: Set<string>;

  // Rebase marker (CAD-style feature-tree rollback) — when set, mutating
  // file commands run rebased at this tick instead of at HEAD. Local UI
  // state, cleared on branch/file change.
  rebaseMarker: string | null;

  // A chat.writer/chat.fixer/chat.append command submitted while a previous
  // generation ('preview') was still in flight. Held here instead of sent
  // immediately (the server processes one command at a time per
  // connection), and flushed the moment the in-flight generation's
  // "chat.preview.end" arrives. See project notes on FlowWriter — a queued
  // chat.writer captures 'flowTid' (HEAD at queue-time) into its command
  // when it's built, not when it's flushed.
  pendingSubmit: { path: string; cmd: FileCommand } | null;

  // Actions
  connect:        () => Promise<void>;
  createBranch:   (name: string) => void;
  deleteBranch:   (name: string) => void;
  selectBranch:   (name: string) => Promise<void>;
  openFile:       (path: string) => Promise<void>;
  createFile:     (path: string) => Promise<void>;
  closeFile:      (path: string) => void;
  openCharacter:  (branch: string) => Promise<void>;
  closeCharacter: (branch: string) => void;
  openJournal:    (branch: string) => Promise<void>;
  closeJournal:   (branch: string) => void;
  trackJournal:   (characterBranch: string, fromPath: string) => void;
  appendJournal:     (branch: string, content: string, marker: string | null) => void;
  editJournalAtom:   (branch: string, tickId: string, content: string, marker: string | null) => void;
  deleteJournalAtom: (branch: string, tickId: string, marker: string | null) => void;
  journalFix:        (branch: string, text: string, targets: string[], marker: string | null) => void;
  setJournalMarker:  (branch: string, tickId: string | null) => void;
  setHoverHighlight:   (tickIds: Set<string>, color: string) => void;
  clearHoverHighlight: () => void;
  enterScene:     (path: string, character: string) => void;
  leaveScene:     (path: string, character: string) => void;
  appendToFile:   (path: string, content: string) => void;
  editAtom:       (path: string, tickId: string, content: string) => void;
  deleteAtom:     (path: string, tickId: string) => void;
  mergeSelected:  (path: string) => void;
  splitSelected:  (path: string) => void;
  uploadFiles:    (files: { path: string; content: Blob }[]) => void;
  addNote:        (refTickId: string, text: string) => void;
  moveTick:       (tickId: string, afterTickId?: string) => void;
  deleteTickEntry:(tickId: string) => void;
  chatWrite:                (path: string, text: string) => void;
  chatFix:                  (path: string, text: string) => void;
  chatNote:                 (path: string, text: string) => void;
  chatRegen:                (path: string, text: string, byBeat: boolean) => void;
  chatOutline:              (path: string) => void;
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

function isChatPreviewEvent(evt: { type: string }): evt is ChatPreviewEvent {
  return evt.type === "chat.preview.start" || evt.type === "chat.preview"
      || evt.type === "chat.preview.thinking" || evt.type === "chat.preview.end";
}

// Shows the preview strip (as a "Generating…" placeholder) a beat after a
// chat.prompt is sent, in case the real chat.preview.start takes a while to
// arrive — but only once enough time has passed that a fast agent wouldn't
// just flash it briefly. Cancelled the moment a real preview event or the
// command's actual result (update/error) arrives.
const PREVIEW_DELAY_MS = 1000;
let previewDelayTimer: ReturnType<typeof setTimeout> | null = null;

function clearPreviewDelayTimer() {
  if (previewDelayTimer !== null) {
    clearTimeout(previewDelayTimer);
    previewDelayTimer = null;
  }
}

function schedulePreviewPlaceholder(set: Setter) {
  clearPreviewDelayTimer();
  previewDelayTimer = setTimeout(() => {
    previewDelayTimer = null;
    set((s) => (s.preview === null ? { preview: { text: "", thinking: "" } } : {}));
  }, PREVIEW_DELAY_MS);
}

function handleChatPreview(set: Setter, get: () => StoryState, evt: ChatPreviewEvent) {
  clearPreviewDelayTimer();
  set((s) => {
    switch (evt.type) {
      case "chat.preview.start":
        return { preview: { text: "", thinking: "" } };
      case "chat.preview":
        return s.preview ? { preview: { ...s.preview, text: s.preview.text + evt.text } } : {};
      case "chat.preview.thinking":
        return s.preview ? { preview: { ...s.preview, thinking: s.preview.thinking + evt.text } } : {};
      case "chat.preview.end":
        return { preview: null };
    }
  });
  if (evt.type === "chat.preview.end") flushPendingSubmit(set, get);
}

// Send the submission that was held back while the previous generation was
// still streaming, now that it's done. Built at queue-time (see
// 'sendChatCommand'), so nothing left to do but send it.
function flushPendingSubmit(set: Setter, get: () => StoryState) {
  const pending = get().pendingSubmit;
  if (!pending) return;
  set({ pendingSubmit: null });
  get().openFiles[pending.path]?.conn.send(atRebase(get().rebaseMarker, pending.cmd, get().journalMarkers));
}

// Build the read-only context items for a chat command from the current
// atom/annotation selection — chain order, atoms then annotations, per
// SELECTION.md. contextAtoms/contextAnnotations are shared across the open
// file and every open journal (see 'openJournal' above), so a selection made
// in a character's journal is a genuine cross-branch reference when it rides
// along on a command sent to the main file's connection — tagged with its
// own branch, since only the *file's* branch is implied by the connection.
function buildContextItems(s: StoryState, path: string): ContextItem[] {
  const fc = s.openFiles[path];
  if (!fc) return [];
  const chain = tickChain(fc.ticks, fc.head);
  const atoms       = chain.filter((t) => s.contextAtoms.has(t.tickId));
  const annotations = chain.filter((t) => s.contextAnnotations.has(t.tickId));
  const local: ContextItem[] = [...atoms, ...annotations].map((t) => ({
    tickId: t.tickId,
    kind: t.kind,
    content: t.content ?? t.message,
  }));

  const crossBranch: ContextItem[] = [];
  for (const [branch, jc] of Object.entries(s.openJournals)) {
    const jchain = tickChain(jc.ticks, jc.head);
    const jAtoms       = jchain.filter((t) => s.contextAtoms.has(t.tickId));
    const jAnnotations = jchain.filter((t) => s.contextAnnotations.has(t.tickId));
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
function buildContextTargets(s: StoryState, path: string): string[] {
  const fc = s.openFiles[path];
  if (!fc) return [];
  return tickChain(fc.ticks, fc.head)
    .filter((t) => t.kind === "atom" && s.contextAtoms.has(t.tickId))
    .map((t) => t.tickId);
}

// Send a chat.* command now, or hold it if a generation is already
// streaming on this connection (server processes one command at a time).
// 'buildCmd' receives the captured flowTid (HEAD at queue-time) only when
// the command is actually being queued — an immediate send has no
// in-flight generation to be provisional about.
function sendChatCommand(get: () => StoryState, set: Setter, path: string, buildCmd: (flowTid?: string) => FileCommand) {
  const s = get();
  const fc = s.openFiles[path];
  if (!fc) return;
  if (s.preview !== null) {
    set({ pendingSubmit: { path, cmd: buildCmd(fc.head ?? undefined) } });
  } else {
    schedulePreviewPlaceholder(set);
    fc.conn.send(atRebase(s.rebaseMarker, buildCmd(undefined), s.journalMarkers));
  }
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

// contextAtoms/contextAnnotations are shared, connection-agnostic selection
// sets (see 'openJournal' below) — any tick.remap, whether from the main
// file connection or a character's journal connection, must keep them
// pointing at the right ids. Journal edits don't carry a persistent
// rebaseMarker in the store (the journal's own marker is local component
// state — see character-sidebar.tsx), so unlike 'handleTickRemap' this only
// remaps the shared sets, not a marker.
function handleContextRemap(set: Setter, mapping: [string, string][]) {
  const table = new Map(mapping);
  set((s) => ({
    contextAtoms: remapSet(table, s.contextAtoms),
    contextAnnotations: remapSet(table, s.contextAnnotations),
  }));
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

// Selection is a temporary "about to act on this" marker, not a durable
// reference — once an action consumes its targets (merge/split/delete/edit),
// those ids are done being selected, regardless of what the server's remap
// eventually resolves them to. Called with the ids the just-sent command
// itself names, so it doesn't need to wait on (or reason about) the
// tick.remap round trip at all.
function dropFromSelection(set: Setter, ids: string[]) {
  if (ids.length === 0) return;
  set((s) => {
    const drop = new Set(ids);
    const contextAtoms = new Set([...s.contextAtoms].filter((id) => !drop.has(id)));
    const contextAnnotations = new Set([...s.contextAnnotations].filter((id) => !drop.has(id)));
    return { contextAtoms, contextAnnotations };
  });
}

// Wrap a command in an "at" rebase if a marker is set, otherwise send it
// as-is. When rebasing, attaches every active character's journal position
// (see 'journalMarkers' and character-sidebar.tsx's nearestJournalMarker
// effect, which keeps it following the scene's own marker) as the `branches`
// field — skipping any character whose corresponding position couldn't be
// derived (null), rather than sending a meaningless entry. See SELECTION.md.
function atRebase(marker: string | null, cmd: FileCommand, journalMarkers: Record<string, string | null>): FileCommand {
  if (!marker) return cmd;
  const branches = Object.entries(journalMarkers)
    .filter((entry): entry is [string, string] => entry[1] !== null)
    .map(([branch, tickId]) => ({ branch, tickId }));
  return { type: "at", tickId: marker, command: cmd, ...(branches.length > 0 ? { branches } : {}) };
}

// ── Store ─────────────────────────────────────────────────────────────────────

export const useStory = create<StoryState>((set, get) => ({
  conns: [],
  error: null,
  branches: [],
  characterBranches: [],
  activeBranch: null,
  files: [],
  ticks: {},
  branchHead: null,
  openFiles: {},
  openCharacters: {},
  openJournals: {},
  journalMarkers: {},
  agentLogs: [],
  hoverHighlight: null,
  preview: null,
  contextAtoms: new Set(),
  contextAnnotations: new Set(),
  rebaseMarker: null,
  pendingSubmit: null,
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
      } else if (evt.type === "branch.list") {
        set({ branches: evt.branches });
      } else if (evt.type === "branch.created") {
        set((s) => ({ branches: [...s.branches, evt.branch].sort() }));
      } else if (evt.type === "branch.deleted") {
        set((s) => ({
          branches: s.branches.filter((b) => b !== evt.branch),
          activeBranch: s.activeBranch === evt.branch ? null : s.activeBranch,
        }));
      } else if (evt.type === "character.list") {
        set({ characterBranches: evt.characters });
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
    for (const cc of Object.values(get().openCharacters)) cc.conn.close();
    for (const jc of Object.values(get().openJournals)) jc.conn.close();

    const label = `branch:${name}`;
    set((s) => ({
      activeBranch: name,
      files: [],
      ticks: {},
      branchHead: null,
      openFiles: {},
      openCharacters: {},
      openJournals: {},
      journalMarkers: {},
      contextAtoms: new Set(),
      contextAnnotations: new Set(),
      rebaseMarker: null,
      pendingSubmit: null,
      hoverHighlight: null,
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
        clearPreviewDelayTimer();
        set((s) => ({ ticks: applyUpdate(s.ticks, evt), branchHead: evt.head, preview: null }));
      } else if (evt.type === "agent.log") {
        handleAgentLog(set, evt);
      } else if (isChatPreviewEvent(evt)) {
        handleChatPreview(set, get, evt);
      } else if (evt.type === "error") {
        clearPreviewDelayTimer();
        set({ preview: null });
        handleError(set, evt);
      }
    });

    await branch.connect();
    set({ _branch: branch });
  },

  openFile: async (path) => {
    const { activeBranch, openFiles } = get();
    if (!activeBranch) return;
    set({ rebaseMarker: null, pendingSubmit: null });
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
        clearPreviewDelayTimer();
        set((s) => {
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
        handleTickRemap(set, path, evt.mapping);
      } else if (evt.type === "agent.log") {
        handleAgentLog(set, evt);
      } else if (isChatPreviewEvent(evt)) {
        handleChatPreview(set, get, evt);
      } else if (evt.type === "error") {
        clearPreviewDelayTimer();
        set({ preview: null });
        handleError(set, evt);
      }
    });

    await fc.connect();
    set((s) => ({
      openFiles: { ...s.openFiles, [path]: { path, ticks: {}, head: null, absent: false, conn: fc } },
    }));
  },

  // Explicit "new file" creation: opens the connection (same as selecting
  // an existing file, which starts out absent), then sends file.create —
  // the server introduces the path as its own empty tick, and the
  // resulting file.present + update events flip 'absent' to false, same as
  // any other write to a not-yet-tracked path would.
  createFile: async (path) => {
    await get().openFile(path);
    get().openFiles[path]?.conn.send({ type: "file.create" });
  },

  closeFile: (path) => {
    get().openFiles[path]?.conn.close();
    set((s) => {
      const next = { ...s.openFiles };
      delete next[path];
      return { openFiles: next, conns: removeConn(s.conns, `file:${path}`) };
    });
  },

  openCharacter: async (branch) => {
    if (get().openCharacters[branch]) return;

    const label = `character:${branch}`;
    set((s) => ({ conns: setConnStatus(s.conns, label, "connecting") }));

    const cc = characterConn(branch);

    cc.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    cc.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, label) }));
      if (evt.type === "character.update") {
        set((s) => ({
          openCharacters: { ...s.openCharacters, [branch]: { branch, name: evt.name, sheet: evt.sheet ?? null, conn: cc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "error") {
        handleError(set, evt);
      }
    });

    await cc.connect();
    set((s) => ({
      openCharacters: { ...s.openCharacters, [branch]: { branch, name: branch, sheet: null, conn: cc } },
    }));
  },

  closeCharacter: (branch) => {
    get().openCharacters[branch]?.conn.close();
    set((s) => {
      const next = { ...s.openCharacters };
      delete next[branch];
      return { openCharacters: next, conns: removeConn(s.conns, `character:${branch}`) };
    });
  },

  // A character's journal is a plain file (see WRITER.md) — this is the same
  // generic '/branch/{name}/{path}' connection the main file view uses, just
  // scoped to the character's own branch instead of activeBranch, and opened
  // lazily by the sidebar accordion rather than by file selection.
  openJournal: async (branch) => {
    if (get().openJournals[branch]) return;

    const label = `journal:${branch}`;
    set((s) => ({ conns: setConnStatus(s.conns, label, "connecting") }));

    const jc = fileConn(branch, JOURNAL_PATH);

    jc.onStatus((s) => {
      if (s !== "connected")
        set((st) => ({ conns: setConnStatus(st.conns, label, "connecting") }));
    });

    jc.subscribe((evt) => {
      set((s) => ({ conns: bumpActivity(s.conns, label) }));
      if (evt.type === "file.present") {
        set((s) => ({
          openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: false, conn: jc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "file.absent") {
        set((s) => ({
          openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: true, conn: jc } },
          conns: setConnStatus(s.conns, label, "connected"),
        }));
      } else if (evt.type === "update") {
        set((s) => {
          const prev = s.openJournals[branch];
          if (!prev) return {};
          return { openJournals: { ...s.openJournals, [branch]: { ...prev, ticks: applyUpdate(prev.ticks, evt), head: evt.head, absent: false } } };
        });
      } else if (evt.type === "tick.remap") {
        handleContextRemap(set, evt.mapping);
      } else if (evt.type === "error") {
        handleError(set, evt);
      }
    });

    await jc.connect();
    set((s) => ({
      openJournals: { ...s.openJournals, [branch]: { path: JOURNAL_PATH, ticks: {}, head: null, absent: false, conn: jc } },
    }));
  },

  closeJournal: (branch) => {
    get().openJournals[branch]?.conn.close();
    set((s) => {
      const next = { ...s.openJournals };
      delete next[branch];
      const nextMarkers = { ...s.journalMarkers };
      delete nextMarkers[branch];
      return { openJournals: next, journalMarkers: nextMarkers, conns: removeConn(s.conns, `journal:${branch}`) };
    });
  },

  // Explicit, one-shot invocation of the raw Tracker agent (see
  // Storyteller.Writer.Agent.Tracker) — copies new deltas from the current
  // scene file into this character's journal.md, verbatim, with a
  // cross-branch ref back to each source atom (that ref is what lets the
  // journal panel's hover highlight find the matching atom in the main
  // view). 'track' is a 'BranchCommand' sent on the *character* branch's own
  // connection, not the currently open story branch's — so this opens a
  // short-lived branch connection just to fire it, and closes it the moment
  // the resulting update/error lands. No persistent state needed: the
  // journal's own file connection (if open) receives the new ticks through
  // its own push, same as any other write to that branch.
  trackJournal: (characterBranch, fromPath) => {
    const source = get().activeBranch;
    if (!source) return;
    const conn = branchConn(characterBranch);
    conn.subscribe((evt) => {
      if (evt.type === "update" || evt.type === "error") conn.close();
      if (evt.type === "error") handleError(set, evt);
    });
    conn.connect().then(() => {
      conn.send({ type: "track", source, files: [{ from: fromPath, to: JOURNAL_PATH }] });
    }).catch(() => { conn.close(); });
  },

  // Basic editing on a character's journal — same commands the main file
  // view sends, just addressed at the journal's own connection. 'marker' is
  // the journal's own local time-travel position (see character-sidebar.tsx
  // / lib/utils.nearestJournalMarker), not the store's global 'rebaseMarker'
  // (which is scoped to whichever scene file is open).
  appendJournal: (branch, content, marker) => {
    get().openJournals[branch]?.conn.send(atRebase(marker, { type: "chat.append", content }, {}));
  },

  editJournalAtom: (branch, tickId, content, marker) => {
    get().openJournals[branch]?.conn.send(atRebase(marker, { type: "edit.atom", tickId, content }, {}));
    dropFromSelection(set, [tickId]);
  },

  deleteJournalAtom: (branch, tickId, marker) => {
    get().openJournals[branch]?.conn.send(atRebase(marker, { type: "delete.atom", tickId }, {}));
    dropFromSelection(set, [tickId]);
  },

  // Fix, scoped to the journal itself — this is what "rewrite to exclude
  // knowledge of the gun" actually runs against: the character's own
  // account, not the scene prose. Targets are journal atom ids (see
  // character-sidebar.tsx's selection-intersection), sent as a chat.fixer
  // on the journal's connection, same shape as the main view's chatFix.
  journalFix: (branch, text, targets, marker) => {
    get().openJournals[branch]?.conn.send(atRebase(marker, { type: "chat.fixer", text, targets }, {}));
  },

  setJournalMarker: (branch, tickId) => {
    set((s) => ({ journalMarkers: { ...s.journalMarkers, [branch]: tickId } }));
  },

  setHoverHighlight: (tickIds, color) => {
    set({ hoverHighlight: { tickIds, color } });
  },

  clearHoverHighlight: () => {
    set({ hoverHighlight: null });
  },

  // Presence is scoped to a file (a scene), not the whole branch — see
  // WRITER.md — so this sends on the file connection, same as any other
  // mutating file command, and gets rebase-at-marker for free via 'atRebase'.
  enterScene: (path, character) => {
    get().openFiles[path]?.conn.send(atRebase(get().rebaseMarker, { type: "enter.scene", character }, get().journalMarkers));
  },

  leaveScene: (path, character) => {
    get().openFiles[path]?.conn.send(atRebase(get().rebaseMarker, { type: "leave.scene", character }, get().journalMarkers));
  },

  appendToFile: (path, content) => {
    const text = content.replace(/^\/+/, "");
    sendChatCommand(get, set, path, () => ({ type: "chat.append", content: text }));
  },

  editAtom: (path, tickId, content) => {
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "edit.atom", tickId, content }, get().journalMarkers)
    );
    dropFromSelection(set, [tickId]);
  },

  deleteAtom: (path, tickId) => {
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "delete.atom", tickId }, get().journalMarkers)
    );
    dropFromSelection(set, [tickId]);
  },

  // Both reuse the existing atom context selection ('contextAtoms') rather
  // than introducing a separate selection mode — same targets Fix/Note
  // already operate on.
  mergeSelected: (path) => {
    const targets = buildContextTargets(get(), path);
    if (targets.length < 2) return;
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "merge.atoms", targets }, get().journalMarkers)
    );
    dropFromSelection(set, targets);
  },

  splitSelected: (path) => {
    const targets = buildContextTargets(get(), path);
    if (targets.length < 1) return;
    get().openFiles[path]?.conn.send(
      atRebase(get().rebaseMarker, { type: "split.atoms", targets }, get().journalMarkers)
    );
    dropFromSelection(set, targets);
  },

  // Drag-and-drop upload — one or more dropped files, written directly to
  // their paths via HTTP PUT (see 'uploadBranchFile'), no chat-agent round
  // trip and no WS/JSON text detour for the bytes. 'files' are already
  // path-resolved (destination folder + dropped filename) by the caller.
  // Each PUT is independent, so one file's fetch failing (surfaced via
  // 'error', same as a WS command's) doesn't block the others; the branch
  // connection's own ref-move notification isn't relied on here — the file
  // list is updated optimistically on that upload's own success instead
  // (see the FIXME in Server.Writer.Branch.Protocol).
  uploadFiles: (files) => {
    const branch = get().activeBranch;
    if (!branch) return;
    files.forEach(({ path, content }) => {
      uploadBranchFile(branch, path, content)
        .then(() => set((s) => ({
          files: s.files.includes(path) ? s.files : [...s.files, path].sort(),
        })))
        .catch((err) => set({ error: err instanceof Error ? err.message : String(err) }));
    });
  },

  addNote: (refTickId, text) => {
    get()._branch?.send({ type: "add.note", refTickId, text });
  },

  moveTick: (tickId, afterTickId) => {
    get()._branch?.send({ type: "move.tick", tickId, afterTickId });
  },

  deleteTickEntry: (tickId) => {
    get()._branch?.send({ type: "delete.tick", tickId });
    dropFromSelection(set, [tickId]);
  },

  chatWrite: (path, text) => {
    const context = buildContextItems(get(), path);
    sendChatCommand(get, set, path, (flowTid) => ({ type: "chat.writer", text, context, flowTid }));
  },

  chatFix: (path, text) => {
    const context = buildContextItems(get(), path);
    const targets = buildContextTargets(get(), path);
    sendChatCommand(get, set, path, () => ({ type: "chat.fixer", text, context, targets }));
  },

  chatNote: (path, text) => {
    const targets = buildContextTargets(get(), path);
    sendChatCommand(get, set, path, () => ({ type: "chat.note", text, targets }));
  },

  chatRegen: (path, text, byBeat) => {
    const context = buildContextItems(get(), path);
    sendChatCommand(get, set, path, () => ({ type: "chat.regen", text, context, byBeat }));
  },

  chatOutline: (path) => {
    sendChatCommand(get, set, path, () => ({ type: "chat.outline" }));
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
