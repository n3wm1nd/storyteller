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
// not two — `bucket` must distinguish "no override yet" from "explicitly
// trashed", since in "show only these" mode the mode default is bucket 1,
// not trash, so trashing one specific tag needs its own explicit marker:
//   - `undefined` (key absent): no override — follows `defaultBucket(invert)`
//   - `null`: explicit trash, regardless of mode
//   - `number`: explicit bucket N, regardless of mode
export interface FilterTag {
  pattern: string;
  bucket?: number | null;
}

export interface ContextFilter {
  tags: FilterTag[];
  invert: boolean;
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
      // context-source.tsx). Old entries aren't worth translating — this is
      // client-local, low-stakes UI config, not user content — so a version
      // bump just resets contextFilters wholesale rather than crashing on
      // the old shape (tags.map on an old {patterns} object throwing
      // undefined is exactly the bug this guards against).
      version: 2,
      migrate: () => ({ contextFilters: {} }),
    }
  )
);

export function contextFilterKey(branch: string, sourceId: string): string {
  return `${branch}.${sourceId}`;
}

// Default bucket for tags with no explicit override — mirrors the two
// simple, familiar modes the old include/exclude toggle offered:
// "hide these" (bucket 1 is everything else, via the catch-all
// toContextLayout appends below) vs. "show only these" (bucket 1 is exactly
// the typed tags, nothing implicit added). A tag's own `bucket` overrides
// this once the user has clicked its badge — see context-source.tsx.
export function defaultBucket(invert: boolean): number | null {
  return invert ? 1 : null;
}

// Compile a persisted {tags, invert} filter into the PickerRule[] layout
// actually sent over the wire (chat.writer's contextLayout, or
// context.preview's slot layout) — the one place this translation happens,
// so the preview a user sees and the layout a real chat.writer call uses
// can never drift apart. Empty tags list compiles to [] ("no layout
// configured"), matching the server's own no-filtering default — not to
// "hide everything", which an empty non-inverted layout would otherwise
// mean per PickerRule's own semantics (see Storyteller.Writer.Agent.ContextFilter).
export function toContextLayout(filter: ContextFilter): PickerRule[] {
  if (filter.tags.length === 0) return [];
  const rules: PickerRule[] = filter.tags.map((t) => {
    const effective = t.bucket !== undefined ? t.bucket : defaultBucket(filter.invert);
    return { pattern: t.pattern, bucket: effective === null ? undefined : effective };
  });
  // "Hide these": whatever isn't explicitly claimed by a tag falls back to
  // bucket 1 via this trailing catch-all. "Show only these" (invert) omits
  // it — unclaimed already defaults to trash with no rule needed.
  if (!filter.invert) rules.push({ pattern: "**/*", bucket: 1 });
  return rules;
}
