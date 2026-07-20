"use client";

// Codex tab: a browse of the story branch's own lore (see
// lore-selector.tsx) that's now wired into the new context system.
// Each card is a ContextAwareLoreCard showing the file's inclusion
// state for the active file's next send ("auto" by default, since
// lore/** is auto-included; "+ added" if it's in extraFiles; "@"
// if it's currently @mentioned in the composer), plus actions to
// preview content and drop an @mention at the composer's cursor.
//
// "auto" isn't actionable (there's no per-file opt-out today), but
// the badges give the user an at-a-glance map of what's actually
// reaching the model -- turning the Codex from a static roster into
// a live view of the next call's context state.

import { LoreSelector } from "./lore-selector";

export function CodexTab({ activeBranch, selectedFile }: {
  activeBranch: string | null;
  selectedFile: string | null;
}) {
  return <LoreSelector branch={activeBranch} selectedFile={selectedFile} />;
}
