"use client";

// Per-file state for the new context UI -- structured casual edits,
// the loaded named function (if any), and the live mention overlay.
// Mirrors lib/uiStore.ts's per-file conventions (selection, rebase
// marker): keyed by file path, cleared on file/branch change.
//
// Three modes, one per file at a time:
//
//   "default"  — no edits, nothing loaded. The next send omits the
//                wire's `context` field entirely; the server's
//                compiled-in default runs. This is the resting state
//                for a file the user hasn't touched.
//
//   "transient" — the user has toggled/added something in the casual
//                 panel, or a mention is in the composer. The
//                 structured state is composed into DSL source at send
//                 time (lib/dslCompose.ts); nothing is persisted.
//
//   "named"     — the user loaded a saved function from the contexts
//                 branch (or just authored one and saved). The
//                 function's name is what's sent (as a bare-name DSL
//                 program). Loading a named function clears any
//                 transient edits -- the named function is the entire
//                 context program now.
//
// Mode precedence at send: "named" overrides "transient" overrides
// "default". Mentions always overlay on top, regardless of base.

import { create } from "zustand";
import {
  DEFAULT_EDITS,
  type ContextEdits,
} from "./dslCompose";

// A single stable empty array used by every selector that wants
// `s.mentions[path] ?? []` without re-rendering on every store change
// -- creating a fresh `[]` each call makes zustand's reference equality
// see a new value every time, causing the "getSnapshot should be
// cached" infinite loop. Shared across modules so the identity of "the
// empty mention list" is genuinely the same reference everywhere.
export const EMPTY_MENTIONS: readonly string[] = Object.freeze([]);

export type CallContextMode = "default" | "transient" | "named";

export interface CallContextFileState {
  mode: CallContextMode;
  // Valid in "transient" mode; ignored otherwise.
  edits: ContextEdits;
  // Valid in "named" mode; the loaded function's name on the contexts
  // branch. null otherwise.
  namedName: string | null;
}

interface CallContextState {
  files: Record<string, CallContextFileState>;
  // Per-file mention overlay: character ids currently @-mentioned in
  // the composer for that file. Kept in this store (not derived from
  // the textarea) so the strip/panel can subscribe to it without
  // reaching into InputBar's local state. Driven by
  // mention-autocomplete.tsx's live parsing on every keystroke.
  mentions: Record<string, string[]>;

  setEdits: (path: string, edits: ContextEdits) => void;
  patchEdits: (path: string, patch: Partial<ContextEdits>) => void;
  loadNamed: (path: string, name: string) => void;
  clearNamed: (path: string) => void;
  resetToDefault: (path: string) => void;
  setMentions: (path: string, ids: string[]) => void;
  clearForFile: (path: string) => void;
  clearAll: () => void;
}

function freshFileState(): CallContextFileState {
  return {
    mode: "default",
    edits: { ...DEFAULT_EDITS, baseline: { ...DEFAULT_EDITS.baseline }, characters: [], extraFiles: [], excludedLorePaths: [] },
    namedName: null,
  };
}

export const useCallContext = create<CallContextState>((set) => ({
  files: {},
  mentions: {},

  setEdits: (path, edits) =>
    set((s) => ({
      files: {
        ...s.files,
        [path]: { mode: "transient", edits, namedName: null },
      },
    })),

  patchEdits: (path, patch) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      const nextEdits = { ...cur.edits, ...patch };
      return {
        files: {
          ...s.files,
          [path]: { mode: "transient", edits: nextEdits, namedName: null },
        },
      };
    }),

  loadNamed: (path, name) =>
    set((s) => ({
      files: {
        ...s.files,
        [path]: {
          mode: "named",
          // Keep `edits` around (not used in "named" mode) so a later
          // "clear named" can fall back to "default" without losing
          // anything the user had drafted in the casual panel before
          // loading. Whether that's the right UX is a Phase-2 call;
          // for now we preserve.
          edits: (s.files[path] ?? freshFileState()).edits,
          namedName: name,
        },
      },
    })),

  clearNamed: (path) =>
    set((s) => {
      const cur = s.files[path];
      if (!cur || cur.mode !== "named") return s;
      // Fall back to "default" -- not back to a prior "transient"
      // draft. The user explicitly cleared the named function; landing
      // them in their stale casual draft instead of the clean default
      // would surprise them. They can re-open the panel if they want.
      return {
        files: {
          ...s.files,
          [path]: freshFileState(),
        },
      };
    }),

  resetToDefault: (path) =>
    set((s) => ({
      files: {
        ...s.files,
        [path]: freshFileState(),
      },
    })),

  setMentions: (path, ids) =>
    set((s) => ({
      mentions: {
        ...s.mentions,
        [path]: ids,
      },
    })),

  clearForFile: (path) =>
    set((s) => {
      const files = { ...s.files };
      const mentions = { ...s.mentions };
      delete files[path];
      delete mentions[path];
      return { files, mentions };
    }),

  clearAll: () => set({ files: {}, mentions: {} }),
}));

// ─── Read helpers ─────────────────────────────────────────────────────────

// The send-time view of a file's context state -- one shape the wire
// site (fileview.actions.ts) can read without reasoning about modes.
// Returns null when nothing should be sent (omit the wire field).
export function getCallContext(path: string): {
  path: string;
  edits: ContextEdits;
  namedName: string | null;
  mentionCharacterIds: string[];
} {
  const s = useCallContext.getState();
  const f = s.files[path];
  return {
    path,
    edits: f?.edits ?? DEFAULT_EDITS,
    namedName: f?.mode === "named" ? f.namedName : null,
    mentionCharacterIds: s.mentions[path] ?? [],
  };
}

// True iff the next send for this file would emit a non-default
// program. Lights up the strip's "edited" affordance.
export function isFileDirty(path: string): boolean {
  const s = useCallContext.getState();
  const f = s.files[path];
  if (!f) return false;
  if (f.mode === "named") return true;
  if (f.mode === "transient") return true;
  return (s.mentions[path]?.length ?? 0) > 0;
}
