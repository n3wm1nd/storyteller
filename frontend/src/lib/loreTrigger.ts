// Trigger-scan for the codex's "Triggered" inclusion mode (see
// settingsStore.ts's ContextFilter.triggers and lore-selector.tsx) —
// SillyTavern-lorebook-style automatic inclusion: an entry flagged
// Triggered gets pulled into context only when its name or one of its
// parsed aliases (Storyteller.Writer.Lore.parseAliases, see LoreNode.aliases)
// actually appears in the text being sent, exactly the same "found a
// reference, force it in" outcome as an explicit `@mention`
// (lib/mentions.ts) — see fileview.actions.ts's writerCommandContext, which
// merges this function's output into the same forced-bucket-1 path list.

import type { LoreNode } from "./ws";
import { basenameNoExt, flattenLore } from "./utils";

function escapeRegExp(s: string): string {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function mentionsWord(text: string, word: string): boolean {
  const trimmed = word.trim();
  if (!trimmed) return false;
  return new RegExp(`\\b${escapeRegExp(trimmed)}\\b`, "i").test(text);
}

// Every Triggered-flagged entry (see `triggeredPaths`, sourced from
// ContextFilter.triggers) whose name or an alias appears in `text` as a
// whole word — deduplicated, order not meaningful (the caller assigns them
// all the same bucket, same as it does for `@mention` paths).
export function triggeredLorePaths(
  text: string,
  loreTree: LoreNode[],
  triggeredPaths: ReadonlySet<string>,
): string[] {
  if (triggeredPaths.size === 0 || !text) return [];
  const hits: string[] = [];
  for (const entry of flattenLore(loreTree)) {
    if (!triggeredPaths.has(entry.path)) continue;
    const candidates = [basenameNoExt(entry.path), ...entry.aliases];
    if (candidates.some((c) => mentionsWord(text, c))) hits.push(entry.path);
  }
  return hits;
}
