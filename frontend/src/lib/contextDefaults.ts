"use client";

// The ONE place in the frontend allowed to hold a duplicate copy of a
// compiled-in Context DSL default's source text (see the project chat
// that settled this: no backend change to preserve `[defQuote| ... |]`
// source at runtime, one hand-kept JS mirror instead, kept in exactly
// this file so there is exactly one place to update when the Haskell
// source changes -- never inline literals scattered across editor
// components, which is what caused real drift bugs earlier).
//
// Every entry here is a verbatim copy of one `defQuote` body in
// `src/Storyteller/Context/DSL/Library.hs`. Keyed by the bare slot name
// (`lore`, not `context.lore`) the way `lib/agents.ts`'s `contextSlots`
// and `lib/contextBranch.ts`'s `context/<name>.dsl` convention both
// already name it.
//
// ── Keeping this in sync ────────────────────────────────────────────────
// If `Library.hs` changes one of the `defQuote` bodies below, copy the
// new source here too. There is no automated check for this (a source
// pretty-printer or a captured-source runtime field were both considered
// and explicitly deferred -- see the project chat) -- this file existing,
// alone, in one place, is the whole mitigation.
//
// Used only as the last-resort starting point for a slot's editor when
// NOTHING has ever been committed to `context/<slot>.dsl` on the branch
// (a real committed file is always ground truth over this) -- see
// dsl-file-editor.tsx's own header.
export const CONTEXT_DEFAULT_SOURCE: Record<string, string> = {
  // Storyteller.Context.DSL.Library.contextLoreDef
  lore: [
    '"## Story background"',
    "for f in lore/**/*:",
    "  x = loreEntry f",
    "  as f: x",
    "  x",
    "",
  ].join("\n"),

  // Storyteller.Context.DSL.Library.contextChaptersCompressedDef
  chaptersCompressed: [
    "x =",
    "  for f in chapters/**/*:",
    "    as f: chapterEntryCompressed f",
    '"## Chapters written so far (compressed)"',
    "in (x | sortBy):",
    "  for f in **/*:",
    "    y = read f",
    "    as f: y",
    "    y",
    "",
  ].join("\n"),
};
