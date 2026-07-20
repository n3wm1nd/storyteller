"use client";

// Codex tab: a read-only browse of the story branch's own lore (see
// lore-selector.tsx) — what actually feeds a Writer/FlowWriter call's
// ambient context is now a directory convention the Context DSL resolves
// server-side (lore/**, chapters/**, style.md), not anything curated here.

import { LoreSelector } from "./lore-selector";

export function CodexTab({ activeBranch }: { activeBranch: string | null }) {
  return <LoreSelector branch={activeBranch} />;
}
