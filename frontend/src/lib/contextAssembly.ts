// Pure composition of the Writer agent's context layout — split out of
// app/fileview.actions.ts's writerCommandContext so the "forced paths jump
// the curated layout" rule can be checked in isolation from the store/
// connection plumbing that gathers its inputs (buildContextItems,
// writerContextLayout, triggeredLoreForText).

import type { PickerRule } from "./ws";

// Merges a curated bucket-picker layout (settingsStore's ContextFilter, via
// toContextLayout) with paths forced to the front by an `@mention`
// (lib/mentions.ts) or a matched Triggered-lore alias (lib/loreTrigger.ts).
//
// An empty base layout already means "show everything" (see
// Storyteller.Writer.Agent.ContextFilter.classifyPath) — a forced path adds
// nothing there. Only a curated (non-empty) layout needs the
// force-inclusion, prepended so it claims ahead of whatever the curated
// layout would otherwise do (first-match-wins).
export function composeContextLayout(baseLayout: PickerRule[], forcedPaths: string[]): PickerRule[] {
  if (baseLayout.length === 0) return [];
  return [...forcedPaths.map((p) => ({ pattern: p, bucket: 1 })), ...baseLayout];
}
