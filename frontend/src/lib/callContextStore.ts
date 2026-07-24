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
import { DEFAULT_EDITS, deriveLoreOverride, type ContextEdits, type PastChaptersMode } from "./dslCompose";

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
  // Toggle one lore path's inclusion via checkbox -- updates
  // `excludedLorePaths` and re-derives `loreOverride` from `allLorePaths`
  // in the same step (see dslCompose.ts's deriveLoreOverride), unless the
  // user has already hand-edited the program (loreOverrideHandEdited),
  // in which case a checkbox click is refused entirely rather than
  // silently clobbering what they wrote -- see LoreRow's own handling of
  // that case (checkboxes disable, a banner explains why).
  toggleLorePath: (filePath: string, allLorePaths: string[], entryPath: string) => void;
  // Direct edit of the override's own text -- always marks
  // loreOverrideHandEdited so future checkbox clicks no longer silently
  // regenerate over it.
  setLoreOverride: (path: string, program: string | null) => void;
  // Clears the lore override *and* every checkbox exclusion plus the
  // hand-edited latch in one step -- what "Reset to default" in LoreRow
  // calls. Distinct from setLoreOverride(path, null): that alone would
  // leave excludedLorePaths/loreOverrideHandEdited behind, so re-checking
  // a box would immediately regenerate the same excluded state right back.
  resetLore: (path: string) => void;
  setPastChaptersMode: (path: string, mode: PastChaptersMode) => void;
  addPinnedProgram: (path: string, name: string) => void;
  removePinnedProgram: (path: string, name: string) => void;
  resetToDefault: (path: string) => void;
  setMentions: (path: string, ids: string[]) => void;
  clearForFile: (path: string) => void;
  clearAll: () => void;
}

function freshFileState(): ContextEdits {
  return { ...DEFAULT_EDITS, excludedLorePaths: [], pinnedProgramNames: [] };
}

// A written lore program is a stronger statement than the plain toggle --
// setting one also forces `loreEnabled` true, so a user who writes "" (or
// any program) doesn't have to separately un-toggle a checkbox that would
// otherwise silently override what they just wrote (see
// dslCompose.ts's synthesizeLoreOverride: a written program wins outright,
// but leaving loreEnabled=false around would be a confusing, invisible
// trap the moment the override is later cleared back to null). Always
// marks loreOverrideHandEdited -- this is the direct-edit path (the
// CodeMirror editor's own onChange); checkbox clicks go through
// toggleLorePath instead, which derives the same field without setting
// that flag.
function withLoreOverride(cur: ContextEdits, program: string | null): ContextEdits {
  return program === null
    ? { ...cur, loreOverride: null, loreOverrideHandEdited: false }
    : { ...cur, loreOverride: program, loreEnabled: true, loreOverrideHandEdited: true };
}

export const useCallContext = create<CallContextState>((set) => ({
  files: {},
  mentions: {},

  setLoreEnabled: (path, enabled) =>
    set((s) => ({
      files: { ...s.files, [path]: { ...(s.files[path] ?? freshFileState()), loreEnabled: enabled } },
    })),

  toggleLorePath: (filePath, allLorePaths, entryPath) =>
    set((s) => {
      const cur = s.files[filePath] ?? freshFileState();
      if (cur.loreOverrideHandEdited) return s; // hand-edited program wins -- refuse to clobber it
      const excludedLorePaths = cur.excludedLorePaths.includes(entryPath)
        ? cur.excludedLorePaths.filter((p) => p !== entryPath)
        : [...cur.excludedLorePaths, entryPath];
      const loreOverride = deriveLoreOverride(allLorePaths, excludedLorePaths);
      return { files: { ...s.files, [filePath]: { ...cur, excludedLorePaths, loreOverride } } };
    }),

  setLoreOverride: (path, program) =>
    set((s) => ({
      files: { ...s.files, [path]: withLoreOverride(s.files[path] ?? freshFileState(), program) },
    })),

  resetLore: (path) =>
    set((s) => {
      const cur = s.files[path] ?? freshFileState();
      return {
        files: {
          ...s.files,
          [path]: { ...cur, loreOverride: null, loreOverrideHandEdited: false, excludedLorePaths: [] },
        },
      };
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
    edits.excludedLorePaths.length > 0 ||
    edits.loreOverride !== null ||
    edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode ||
    edits.pinnedProgramNames.length > 0 ||
    hasMentions
  );
}
