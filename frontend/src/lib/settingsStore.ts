"use client";

// User-local preferences that should survive a browser refresh but aren't
// server state (see lib/serverCacheStore.ts for that) and aren't ephemeral
// UI state either (see lib/uiStore.ts) — persisted to localStorage via
// zustand's persist middleware. Flat by design: expected to stay small, so
// one key per setting rather than a nested per-feature namespace.

import { create } from "zustand";
import { persist } from "zustand/middleware";
import type { PickerRule } from "./ws";

// One glob the user typed, plus an optional bucket override. Three states,
// not two — `bucket` must distinguish "no override yet" (untouched tags
// default to trash, see `defaultBucket`) from "explicitly trashed", since a
// tag a user has clicked back down to trash needs to stay trashed even if
// it's later reordered relative to other tags:
//   - `undefined` (key absent): no override — follows `defaultBucket()`
//   - `null`: explicit trash
//   - `number`: explicit bucket N
export interface FilterTag {
  pattern: string;
  bucket?: number | null;
}

// Patterns are matched in list order (`tags`' own order — see
// PreviewTreeNode/addPattern in context-source.tsx, which always appends),
// first match wins (Storyteller.Writer.Agent.ContextFilter.classifyPath) —
// so ordering the tags themselves is what used to need a separate
// include/exclude-only ("invert") toggle: a broad `**/*` catch-all placed
// *after* specific patterns reproduces "hide these", while specific
// patterns with nothing broad after them reproduces "show only these".
// There is no longer a mode flag — just the tag list and each tag's bucket.
//
// Only used by context-source.tsx's generic per-agent context sources now
// (summarizers, journal chunking, ...) — the Writer/character-branch case
// this originally also served (lore-selector.tsx's Off/Triggered/Always
// control, lib/loreTrigger.ts's trigger-scan) moved to the Context DSL
// (CONTEXT-DSL.md/Storyteller.Context.DSL.Library): ambient context for a
// Writer/FlowWriter call is now a directory convention the server resolves
// on its own (lore/**, chapters/**, style.md), not a client-curated
// bucket/trigger list. This type stays exactly as it was for the agents
// whose backend (Server.Writer.ContextView) hasn't made that same move.
export interface ContextFilter {
  tags: FilterTag[];
}

interface SettingsState {
  // Keyed by `${branch}.${agentId}:${sourceId}` — see context-source.tsx.
  // Describes the source itself, not any particular open file.
  contextFilters: Record<string, ContextFilter>;
  setContextFilter: (key: string, filter: ContextFilter) => void;
}

export const useSettings = create<SettingsState>()(
  persist(
    (set) => ({
      contextFilters: {},
      setContextFilter: (key, filter) =>
        set((s) => ({ contextFilters: { ...s.contextFilters, [key]: filter } })),
    }),
    {
      name: "storyteller.settings",
      // v1 -> v2: ContextFilter's shape changed from {patterns: string,
      // invert} to {tags: FilterTag[], invert} (bucket-picker rework, see
      // context-source.tsx). v2 -> v3: dropped the `invert` mode flag
      // entirely — ordering the tags themselves now covers what it did (see
      // ContextFilter's doc comment above). v3 -> v4: added `triggers`
      // (lore-mentions trigger-based inclusion). v4 -> v5: removed
      // `triggers` again — that whole mechanism moved to the Context DSL
      // (see ContextFilter's own doc comment). Old entries aren't worth
      // translating any of these times — this is client-local, low-stakes UI
      // config, not user content — so a version bump just resets
      // contextFilters wholesale rather than crashing on the old shape.
      version: 5,
      migrate: () => ({ contextFilters: {} }),
    }
  )
);

export function contextFilterKey(branch: string, sourceId: string): string {
  return `${branch}.${sourceId}`;
}

// Default bucket for a tag with no explicit override: trash. Matches
// PickerRule's own "unclaimed == trash" default (see
// Storyteller.Writer.Agent.ContextFilter), so an untouched tag behaves the
// same as no rule at all until the user clicks its badge to claim a bucket
// — see context-source.tsx.
export function defaultBucket(): number | null {
  return null;
}

// Pure tag-list mutations shared by every ContextFilter editor (currently
// context-source.tsx's typed-pattern/tree editor and codex.tsx's card
// grid) — each view calls these instead of forking its own copy, so "how a
// tag gets added/removed/bucketed" can't drift between them.

// Add a new tag for `pattern` (a typed glob, or an exact file path — both
// are just patterns as far as ContextFilter is concerned), no-op if already
// present. New tags start with `bucket: undefined` (falls back to
// `defaultBucket()`, i.e. trash) — same as clicking an unclaimed file in
// the tree editor.
export function addPatternTag(filter: ContextFilter, pattern: string): ContextFilter {
  if (!pattern || filter.tags.some((t) => t.pattern === pattern)) return filter;
  return { ...filter, tags: [...filter.tags, { pattern }] };
}

export function removeFilterTag(filter: ContextFilter, pattern: string): ContextFilter {
  return { ...filter, tags: filter.tags.filter((t) => t.pattern !== pattern) };
}

export function toggleFilterTag(filter: ContextFilter, pattern: string): ContextFilter {
  return filter.tags.some((t) => t.pattern === pattern)
    ? removeFilterTag(filter, pattern)
    : addPatternTag(filter, pattern);
}

export function cycleFilterTagBucket(filter: ContextFilter, pattern: string, next: (b: number | null) => number | null): ContextFilter {
  return {
    ...filter,
    tags: filter.tags.map((t) => {
      if (t.pattern !== pattern) return t;
      const current = t.bucket !== undefined ? t.bucket : defaultBucket();
      return { ...t, bucket: next(current) };
    }),
  };
}

// Compile a persisted {tags} filter into the PickerRule[] layout sent over
// the wire as a context.preview slot's own layout (see context-source.tsx)
// — the one place this translation happens, so the live preview a user
// sees always matches what's actually persisted. Rule order is tag order
// (first match wins server-side) — a broad `**/*` tag placed after
// specific ones acts as a catch-all for "hide these"; specific tags with
// nothing broad after them means only those show ("show only these").
// Empty tags list compiles to [] ("no layout configured"), matching the
// server's own no-filtering default.
export function toContextLayout(filter: ContextFilter): PickerRule[] {
  return filter.tags.map((t) => {
    const effective = t.bucket !== undefined ? t.bucket : defaultBucket();
    return { pattern: t.pattern, bucket: effective === null ? undefined : effective };
  });
}
