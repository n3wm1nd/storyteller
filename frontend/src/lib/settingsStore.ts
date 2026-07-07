"use client";

// User-local preferences that should survive a browser refresh but aren't
// server state (see lib/serverCacheStore.ts for that) and aren't ephemeral
// UI state either (see lib/uiStore.ts) — persisted to localStorage via
// zustand's persist middleware. Flat by design: expected to stay small, so
// one key per setting rather than a nested per-feature namespace.

import { create } from "zustand";
import { persist } from "zustand/middleware";

export interface ContextFilter {
  patterns: string;
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
    { name: "storyteller.settings" }
  )
);

export function contextFilterKey(branch: string, sourceId: string): string {
  return `${branch}.${sourceId}`;
}
