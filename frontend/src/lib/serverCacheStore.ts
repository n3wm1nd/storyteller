"use client";

// The shape of the client's cache of server-owned state, and nothing else —
// no WS connection logic lives here (see app/*.actions.ts, one per
// component, each owning the connections its own component needs).
//
// Reading the cache is unrestricted — any component can call
// `useServerCache`/`getServerCache` and interpret the data however it likes.
// Writing is a different story: the only legitimate writes are a
// connection's own event handler, mirroring whatever the server just
// pushed. If a component needs to *appear* to change this data ahead of the
// server's actual response (e.g. a draft-looking in-flight edit), that's an
// overlay in lib/uiStore.ts, layered on top at render time — never a write
// here. See WS-PROTOCOL.md's "the client never mutates synced data locally".
//
// This is enforced by naming, not by hiding the binding — `mirrorServerEvent`
// is exported and importable from any *.actions.ts file, wherever it lives.
// The friction is that its name says exactly what it's for: call it only
// from inside a connection's subscribe callback, mirroring an event the
// server actually sent. Reaching for it from a click handler or other
// user-triggered code path is the tell that something's wrong, and it reads
// as wrong at the call site, not just in a comment somewhere.

import { create } from "zustand";
import type {
  StoryWS, WireTick,
  SessionCommand, SessionEvent,
  BranchCommand,  BranchEvent,
  FileCommand,    FileEvent,
  CharacterEvent, CharacterSummary,
  LibraryCommand, LibraryEvent, LibraryNode, ChapterUnit,
  WireUndoEntry,
} from "./ws";

export type { WireTick };

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
  avatar: boolean;
  conn:   StoryWS<never, CharacterEvent>;
}

export interface ServerCacheState {
  // Session level
  branches: string[];
  // character/* branch list with raw sheet.md content, kept live by the
  // server's own notifier (see Server.Writer.Session.Connection) — unlike
  // 'branches', this reflects character branches created by any connection
  // (e.g. Track/CharGen from a branch connection), not just this session's
  // own create-branch/delete-branch commands. Sheet content is raw (see
  // WS-PROTOCOL.md); decode into a display name via lib/utils.characterDisplayName.
  characterBranches: CharacterSummary[];

  // The shared, session-wide undo log (Storyteller.Core.Undo) — chronological,
  // oldest first, kept live by the same session notifier as 'branches'/
  // 'characterBranches' (see sidebar.actions.ts). See app/undo-timeline.tsx.
  undoEntries: WireUndoEntry[];

  // Branch level
  activeBranch: string | null;
  files:        string[];
  ticks:        Record<string, WireTick>;
  branchHead:   string | null;

  // The active branch's book/chapter organizational tree — kept live by
  // /library/{name}'s own notifier, same lifecycle as '_branch' below (see
  // sidebar.actions.ts's selectBranch). Empty when no branch is active or
  // the branch has no recognized chapters yet — not an error state.
  libraryTree: LibraryNode[];

  // Every chapter number already paired with its own chapter file/beat
  // sheet, precomputed server-side (see Storyteller.Writer.Library.chapterUnits)
  // — this client never reconstructs that pairing itself; see library.tsx.
  libraryChapters: ChapterUnit[];

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

  // Streamed LLM draft for the in-flight chat.prompt/chargen command, if
  // any — a best-effort preview only. Cleared on chat.preview.end, and
  // defensively on any update/error, since it may not match (or may not be
  // followed by) anything actually persisted. See WS-PROTOCOL.md. Shared
  // across the branch connection (chargen) and file connections (chat.*) —
  // see lib/chatPreview.ts.
  preview: { text: string; thinking: string } | null;

  // The wire id of the command 'preview' belongs to, if it was sent with
  // one — what a Stop button targets via SessionCommand's "cancel". Same
  // lifecycle as 'preview': set right before sending, cleared everywhere
  // 'preview' is cleared. Null whenever 'preview' is null.
  previewCommandId: string | null;

  _session: StoryWS<SessionCommand, SessionEvent> | null;
  _branch:  StoryWS<BranchCommand,  BranchEvent>  | null;
  _library: StoryWS<LibraryCommand, LibraryEvent> | null;
}

const _store = create<ServerCacheState>(() => ({
  branches: [],
  characterBranches: [],
  undoEntries: [],
  activeBranch: null,
  files: [],
  ticks: {},
  branchHead: null,
  libraryTree: [],
  libraryChapters: [],
  openFiles: {},
  openCharacters: {},
  openJournals: {},
  preview: null,
  previewCommandId: null,
  _session: null,
  _branch: null,
  _library: null,
}));

// Read access — a hook (optionally with a selector, same convention as
// zustand's own) and a one-shot getter. Unrestricted; call from anywhere.
export function useServerCache(): ServerCacheState;
export function useServerCache<T>(selector: (s: ServerCacheState) => T): T;
export function useServerCache<T>(selector?: (s: ServerCacheState) => T) {
  return _store(selector ?? ((s: ServerCacheState) => s as unknown as T));
}

export function getServerCache(): ServerCacheState {
  return _store.getState();
}

// Write access — deliberately named for what it's for, not what it does.
// Call this only from inside a WS connection's `subscribe` callback, to
// mirror an event the server just sent. Never from UI-triggered code (a
// click handler, a form submit) — that's what lib/uiStore.ts is for.
export const mirrorServerEvent = _store.setState;
