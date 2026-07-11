"use client";

// Codex tab: the story branch's own lore curation, via the shared
// LoreSelector (see lore-selector.tsx) bound to the Writer agent's
// `writer:story` ContextFilter — the same key the Agents tab's Writer ->
// Story branch tree editor reads/writes, so either view is interchangeable
// on the same live filter.

import { LoreSelector } from "./lore-selector";
import { WRITER_STORY_SOURCE_ID } from "@/lib/agents";

export function CodexTab({ activeBranch }: { activeBranch: string | null }) {
  return <LoreSelector branch={activeBranch} sourceId={WRITER_STORY_SOURCE_ID} />;
}
