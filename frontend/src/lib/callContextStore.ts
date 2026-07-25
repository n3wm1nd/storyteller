"use client";

// Per-file state for the writer's casual context UI -- lore
// toggle/exclusions, the past-chapters mode, and the list of named pinned
// programs -- plus the live mention overlay. Mirrors lib/uiStore.ts's
// per-file conventions (selection, rebase marker): keyed by file path,
// cleared on file/branch change.
//
// This store used to also track a "named"/"transient"/"default" mode for
// a whole synthesized-or-loaded Context DSL program overriding the entire
// writer context, plus a `liveDslDrafts` mirror of the DSL editor's own
// unsaved textarea content. That design was rolled back (see
// dslCompose.ts's own header: full per-call DSL control over the *entire*
// writer context moved expertise away from the agent and doubled every
// piece of context-assembly knowledge across two hand-synced
// implementations) -- there's no longer a single "the program for this
// file" to track, just three small independent fields that each become
// their own wire field at send time (see composeWriterContextFields). A
// "named program" now means one more entry in `pinnedProgramNames`, not a
// whole-context override mode.

import { create } from "zustand";
import {
  DEFAULT_EDITS, toggleLorePathInProgram, toggleOtherPathInProgram,
  type ContextEdits, type PastChaptersMode,
} from "./dslCompose";

// A single stable empty array used by every selector that wants
// `s.mentions[path] ?? []` without re-rendering on every store change --
// creating a fresh `[]` each call makes zustand's reference equality see a
// new value every time, causing the "getSnapshot should be cached" infinite
// loop. Shared across modules so the identity of "the empty mention list"
// is genuinely the same reference everywhere.
export const EMPTY_MENTIONS: readonly string[] = Object.freeze([]);

// Same reasoning, for `s.files[path]?.pinnedProgramNames` -- a selector
// that falls back to a fresh `[]` on every call (rather than this one
// shared, frozen reference) breaks zustand's reference-equality check,
// which React then reports as "the result of getSnapshot should be
// cached" (an infinite re-render loop), not just a wasted re-render.
export const EMPTY_PINNED_PROGRAMS: readonly string[] = Object.freeze([]);

interface CallContextState {
  files: Record<string, ContextEdits>;
  // Per-file mention overlay: character ids currently @-mentioned in the
  // composer for that file. Kept in this store (not derived from the
  // textarea) so the strip/panel can subscribe to it without reaching into
  // InputBar's local state. Driven by mention-autocomplete.tsx's live
  // parsing on every keystroke.
  mentions: Record<string, string[]>;

  setLoreEnabled: (path: string, enabled: boolean) => void;
  // Toggle one lore path's inclusion via checkbox, against a starting
  // text `base` -- the committed project default (or its own JS-mirror
  // fallback) when `loreOverride` is still null, otherwise `loreOverride`
  // itself (see LoreRow's `resetTarget`/`draft`). Uses
  // dslCompose.ts's `toggleLorePathInProgram`, which regenerates only the
  // checkbox-owned prefix and leaves any hand-added text after it
  // untouched; `null` only if that prefix doesn't parse at all (nothing
  // truthful for a checkbox click to update, matching LoreRow's own
  // disabled-checkboxes state for that case).
  toggleLorePath: (filePath: string, base: string, entryPath: string) => void;
  // Direct edit of the override's own text.
  setLoreOverride: (path: string, program: string | null) => void;
  // Clears the lore override back to "nothing touched" -- what "Reset to
  // default" in LoreRow calls.
  resetLore: (path: string) => void;
  // 'setLoreEnabled'/'toggleLorePath'/'setLoreOverride'/'resetLore''s own
  // twins for `context.other` -- identical contract, just against
  // OtherRow's own candidate file set and `otherOverride`/`otherEnabled`.
  setOtherEnabled: (path: string, enabled: boolean) => void;
  toggleOtherPath: (filePath: string, base: string, entryPath: string) => void;
  setOtherOverride: (path: string, program: string | null) => void;
  resetOther: (path: string) => void;
  setPastChaptersMode: (path: string, mode: PastChaptersMode) => void;
  addPinnedProgram: (path: string, name: string) => void;
  removePinnedProgram: (path: string, name: string) => void;
  resetToDefault: (path: string) => void;
  setMentions: (path: string, ids: string[]) => void;
  clearForFile: (path: string) => void;
  clearAll: () => void;
}

function freshFileState(): ContextEdits {
  return { ...DEFAULT_EDITS, pinnedProgramNames: [] };
}

export const useCallContext = create<CallContextState>((set) => ({
  files: {},
  mentions: {},

  setLoreEnabled: (path, enabled) =>
    set((s) => ({
      files: { ...s.files, [path]: { ...(s.files[path] ?? freshFileState()), loreEnabled: enabled } },
    })),

  toggleLorePath: (filePath, base, entryPath) =>
    set((s) => {
      const cur = s.files[filePath] ?? freshFileState();
      const loreOverride = toggleLorePathInProgram(cur.loreOverride ?? base, entryPath);
      if (loreOverride === null) return s; // hand-edited text -- nothing truthful to toggle
      return { files: { ...s.files, [filePath]: { ...cur, loreOverride, loreEnabled: true } } };
    }),

  setLoreOverride: (path, program) =>
    set((s) => ({
      files: {
        ...s.files,
        [path]: {
          ...(s.files[path] ?? freshFileState()),
          loreOverride: program,
          loreEnabled: program === null ? (s.files[path] ?? freshFileState()).loreEnabled : true,
        },
      },
    })),

  resetLore: (path) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      return { files: { ...s.files, [path]: { ...cur, loreOverride: null } } };
    }),

  setOtherEnabled: (path, enabled) =>
    set((s) => ({
      files: { ...s.files, [path]: { ...(s.files[path] ?? freshFileState()), otherEnabled: enabled } },
    })),

  toggleOtherPath: (filePath, base, entryPath) =>
    set((s) => {
      const cur = s.files[filePath] ?? freshFileState();
      const otherOverride = toggleOtherPathInProgram(cur.otherOverride ?? base, entryPath);
      if (otherOverride === null) return s; // hand-edited text -- nothing truthful to toggle
      return { files: { ...s.files, [filePath]: { ...cur, otherOverride, otherEnabled: true } } };
    }),

  setOtherOverride: (path, program) =>
    set((s) => ({
      files: {
        ...s.files,
        [path]: {
          ...(s.files[path] ?? freshFileState()),
          otherOverride: program,
          otherEnabled: program === null ? (s.files[path] ?? freshFileState()).otherEnabled : true,
        },
      },
    })),

  resetOther: (path) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      return { files: { ...s.files, [path]: { ...cur, otherOverride: null } } };
    }),

  setPastChaptersMode: (path, mode) =>
    set((s) => ({
      files: { ...s.files, [path]: { ...(s.files[path] ?? freshFileState()), pastChaptersMode: mode } },
    })),

  addPinnedProgram: (path, name) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      if (cur.pinnedProgramNames.includes(name)) return s;
      return { files: { ...s.files, [path]: { ...cur, pinnedProgramNames: [...cur.pinnedProgramNames, name] } } };
    }),

  removePinnedProgram: (path, name) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      return {
        files: { ...s.files, [path]: { ...cur, pinnedProgramNames: cur.pinnedProgramNames.filter((n) => n !== name) } },
      };
    }),

  resetToDefault: (path) =>
    set((s) => ({ files: { ...s.files, [path]: freshFileState() } })),

  setMentions: (path, ids) =>
    set((s) => ({ mentions: { ...s.mentions, [path]: ids } })),

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

// The send-time view of a file's context state -- one shape the wire site
// (fileview.actions.ts) can read without touching the store directly.
export function getCallContext(path: string): {
  path: string;
  edits: ContextEdits;
  mentionCharacterIds: string[];
} {
  const s = useCallContext.getState();
  return {
    path,
    edits: s.files[path] ?? DEFAULT_EDITS,
    mentionCharacterIds: s.mentions[path] ?? [],
  };
}

// True iff the next send for this file would carry any non-default writer
// context field. Lights up the strip's "edited" affordance.
export function isFileDirty(path: string): boolean {
  const s = useCallContext.getState();
  const edits = s.files[path];
  const hasMentions = (s.mentions[path]?.length ?? 0) > 0;
  if (!edits) return hasMentions;
  return (
    edits.loreEnabled !== DEFAULT_EDITS.loreEnabled ||
    edits.loreOverride !== null ||
    edits.otherEnabled !== DEFAULT_EDITS.otherEnabled ||
    edits.otherOverride !== null ||
    edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode ||
    edits.pinnedProgramNames.length > 0 ||
    hasMentions
  );
}
