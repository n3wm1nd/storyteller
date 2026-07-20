"use client";

// The frontend's structured-state ↔ DSL-source bridge for the Context DSL.
//
// The casual UI exposes context assembly as a small set of natural toggles
// and pickers ("Story lore", "Past chapters", "Style guide", a cast list,
// extra files) with no DSL visible anywhere. At send time those selections
// have to become an actual Context DSL program (see CONTEXT-DSL.md) the
// server can run as the `context` field of a `chat.writer` /
// `correct.group` command (frontend/src/lib/ws.ts:331,358). This module
// is the one place that translation happens.
//
// Three states matter to a send:
//
//   - "default" — no edits anywhere. The store's `mode` is "default" and
//     there are no per-call mention overlays. The wire's `context` field
//     is omitted entirely so the server's compiled-in default
//     (`Storyteller.Context.DSL.Library.contextQuery`, see that module's
//     own haddock) runs -- the casual user has expressed nothing, so
//     nothing custom should be sent.
//
//   - "transient" — the user has toggled, added, or removed something in
//     the casual panel (or a mention is in the composer). The structured
//     state is composed into a full DSL program inline and sent as the
//     `context` field's program text. Nothing is written to the contexts
//     branch unless the user explicitly promotes via "Save as...".
//
//   - "named" — the user has loaded (or just authored and saved) a named
//     function on the `contexts` branch. The wire field carries the
//     function's name as its entire program text -- a one-line DSL
//     program that's just a bare function call. Same wire path as the
//     transient case (program text in `context`), no special "name
//     resolution" mode: a bare identifier in a DSL body is already a
//     function call (`evalExpr`'s `EIdent` case), and the saved
//     function on the `contexts` branch is in scope the same way
//     `context.character` is. The frontend never reads the source back
//     to send it inline -- the name *is* the program.
//
// The default template mirrors what the compiled-in default produces
// (lore/**, chapters/**, style.md as separate named buckets) so a casual
// user toggling things off sees the same shape they'd have gotten for
// free, just minus what they removed -- never a surprise rewrite of the
// prompt's structure. See CONTEXT-DSL.md's "Worked examples" for the
// patterns this composes from.

// ─── Structured state ─────────────────────────────────────────────────────

// What the server's default convention would include, exposed as the
// casual editor's top-level toggles. The names are deliberately
// non-technical: the user sees "Story lore", "Past chapters", "Style
// guide" -- not "lore/** glob" or "context.lore binding". The keys here
// are an internal vocabulary that never surfaces in the UI.
export interface BaselineToggles {
  lore: boolean;
  chapters: boolean;
  style: boolean;
}

export const DEFAULT_BASELINE: BaselineToggles = {
  lore: true,
  chapters: true,
  style: true,
};

// A character identifier is the branch name without the `character/`
// prefix, matching how `enter.scene` / `ask.character` etc. already
// identify characters across the rest of the wire protocol
// (frontend/src/lib/ws.ts). Stored verbatim here, never normalized.
export interface CharacterAdd {
  id: string;        // e.g. "alice-chen"
  // "blurb" | "full" -- the casual default is "blurb" (acquaintance
  // summary), the power-user can switch to "full" if the scene needs
  // the character's complete tracked state. Matches the buckets
  // `context.character` itself exposes (see Library.hs:154-184).
  depth: "blurb" | "full";
}

// A specific extra file path the user picked from the lore/file tree
// beyond what the baseline convention would catch. Resolved against
// the calling branch's own working tree at runtime.
export interface FileAdd {
  path: string;       // e.g. "notes/battle-plan.md"
  asName?: string;    // optional friendly label; defaults to basename
}

// The structured shape the casual panel edits. One flat object, not a
// tree: the casual panel's sections (baseline / cast / extras) are a UI
// grouping only, not a state-shape concern. `baseline` toggles
// inclusions *in addition to* what `DEFAULT_BASELINE` would emit; if
// `baseline.lore === true`, the lore bucket is emitted, otherwise it
// isn't. There's no separate "removed" list -- a default-on thing
// toggled off is just `baseline.lore === false`.
export interface ContextEdits {
  baseline: BaselineToggles;
  characters: CharacterAdd[];
  extraFiles: FileAdd[];
  // Specific paths under lore/** to *exclude* even when baseline.lore
  // is on -- "include lore, but not lore/battle-log.md (too long)".
  // Implemented as a positive glob list (the synthesizer enumerates
  // what's left), not a real `exclude` filter, because exclude in this
  // DSL can only neuter content to empty, never actually shrink a key
  // set (Library.hs:66-79 documents the same gotcha for the default's
  // own `exclude` use).
  excludedLorePaths: string[];
}

export const DEFAULT_EDITS: ContextEdits = {
  baseline: { ...DEFAULT_BASELINE },
  characters: [],
  extraFiles: [],
  excludedLorePaths: [],
};

// True iff `edits` differs from `DEFAULT_EDITS` in any visible way.
// Used to decide whether to send anything at all (omit when false) and
// to light up the strip's "edited" affordance (true).
export function isDirty(edits: ContextEdits): boolean {
  if (edits.baseline.lore !== DEFAULT_BASELINE.lore) return true;
  if (edits.baseline.chapters !== DEFAULT_BASELINE.chapters) return true;
  if (edits.baseline.style !== DEFAULT_BASELINE.style) return true;
  if (edits.characters.length > 0) return true;
  if (edits.extraFiles.length > 0) return true;
  if (edits.excludedLorePaths.length > 0) return true;
  return false;
}

// ─── Synthesis ────────────────────────────────────────────────────────────

// Indentation helpers -- the DSL is layout-sensitive (statement per
// line, two-space indent under `as`/`in`/`for`/`let`), so building the
// source as a nested array of lines and then joining keeps the shape
// readable rather than hand-concatenating strings.
type Lines = string[];
function block(lines: Lines, indent = "  "): Lines {
  return lines.flatMap((l) => l.split("\n").map((ll) => indent + ll));
}

// `for f in GLOB: as f: read f` -- the canonical "include every file
// under GLOB, each labelled by its own path" fragment, what the default
// `contextLore`/`contextChapters` definitions both expand to. Returns
// null for an empty list (caller skips emitting the bucket entirely).
function globBucketLines(glob: string): Lines {
  return [
    `for f in ${glob}:`,
      "as f: read f",
  ];
}

// `context.character(id)` produces a five-bucket character Value
// (sheet, blurb, full, journal, journalFull). Inlining the full
// definition here would duplicate the host-side curation
// (`journalDelta`, `characterBlurb`) that the backend ships; the casual
// UI's "include Alice" should give the user *exactly* what
// `context.character` gives -- no more, no less -- so we emit a call
// and rely on the compiled-in `context.character` being callable. The
// bare call works in any contexts-branch override or wire query (the
// interpreter's free-identifier fallback covers it); depth selection
// happens via the named-entry read on the call's result.
//
// The `depth` toggle maps to which bucket the call's *default* emits:
// "blurb" forces blurb via re-export, "full" emits the full character
// (the call's own default -- see contextCharacter's last line,
// `blurb charname`, which we override here).
function characterLines(c: CharacterAdd): Lines {
  const label = c.id;
  if (c.depth === "blurb") {
    return [
      `as "${label}":`,
      `  in (context.character("${c.id}")):`,
      `    read "blurb"`,
    ];
  }
  // full -- emit the character's whole sheet/journal/full buckets as
  // one block, letting the consumer see everything the call produced.
  return [
    `as "${label}":`,
    `  context.character("${c.id}")`,
  ];
}

// `as "name": read "path" | orifempty ""` -- a single extra file,
// tolerant of being absent (the `orifempty ""` makes a missing file a
// no-op rather than a runtime failure, matching `contextStyle`'s own
// pattern).
function fileAddLines(f: FileAdd): Lines {
  const name = f.asName ?? f.path.split("/").pop()!.replace(/\.md$/, "");
  return [
    `as "${name}":`,
    `  read "${f.path}" | orifempty ""`,
  ];
}

// Composes the structured edits into a full 0-arity DSL program.
//
// Returns null when `edits` is structurally identical to the server's
// default -- a caller who has done nothing should send nothing. Any
// deviation, no matter how small, produces a full program (the
// synthesizer doesn't try to emit a diff; it re-emits the whole
// baseline-plus-edits shape). This matches the wire's "the program
// replaces the default" semantics (CONTEXT-DSL.md / Context.hs:174):
// there's no concept of "default plus additions" at the protocol level,
// so the frontend composes them client-side and sends the result.
export function synthesizeProgram(edits: ContextEdits): string | null {
  if (!isDirty(edits)) return null;

  const chunks: Lines = [];

  if (edits.baseline.lore) {
    chunks.push(`as "lore":`);
    // Excluded paths under lore/** aren't synthesized here -- the DSL's
    // `exclude` filter can only neuter content to empty, never shrink a
    // key set (Library.hs:66-79 documents the same gotcha for the
    // default's own `exclude` use), so an "everything except these"
    // glob can't be expressed without enumerating the kept paths, which
    // would require the lore tree at synthesis time. Phase-1 exposes
    // exclusions only via the power-user DSL editor (where the user
    // writes the real `exclude` themselves); the casual panel sticks to
    // positive additions. The field stays in `ContextEdits` so a future
    // Phase-2 synthesizer that does enumerate can fill it in.
    void edits.excludedLorePaths;
    chunks.push(...block(globBucketLines("lore/**/*")));
  }

  if (edits.baseline.chapters) {
    chunks.push(`as "chapters":`);
    // contextChapters' shape: a sortBy'd list with chapter headers
    // (Library.hs:100-110). Inlined here so the casual toggle produces
    // the same shape the default would.
    chunks.push(...block([
      `x =`,
      `  for f in chapters/**/*:`,
      `    as f:`,
      `      "## Chapter: %f%"`,
      `      > read f`,
      `in (x | sortBy):`,
      `  for f in **/*:`,
      `    as f: read f`,
    ]));
  }

  if (edits.baseline.style) {
    chunks.push(`as "style":`);
    chunks.push(...block([
      `read "style.md" | orifempty ""`,
    ]));
  }

  for (const c of edits.characters) {
    chunks.push(...characterLines(c));
  }

  for (const f of edits.extraFiles) {
    chunks.push(...fileAddLines(f));
  }

  return chunks.join("\n") + "\n";
}

// ─── Send-time composition ─────────────────────────────────────────────────

// The full send-time composition. Returns null = "send nothing" (omit
// the wire's `context` field entirely, server default runs); a string =
// the program text to put in the field. One function so the wire site
// (fileview.actions.ts) doesn't have to reason about layers.
export interface CallContext {
  edits: ContextEdits;
  // The composer's current parsed mentions (character ids). Driven
  // live from the textarea by mention-autocomplete.tsx.
  mentionCharacterIds: string[];
  // When non-null, the user has loaded a named function from the
  // contexts branch; its name overrides `edits` entirely -- the wire
  // carries just the name as program text.
  namedName: string | null;
}

export function composeSendProgram(ctx: CallContext): string | null {
  // Mention overlay lines are appended to whatever base program runs.
  const overlayLines: Lines = ctx.mentionCharacterIds.flatMap((id) => [
    `as "@${id}":`,
    `  in (context.character("${id}")):`,
    `    read "blurb"`,
  ]);
  const hasOverlay = overlayLines.length > 0;

  if (ctx.namedName !== null) {
    // A bare-name program (a one-line call). If there's a mention
    // overlay, it becomes a multi-line program: the call on line one,
    // then the overlay statements. Still ordinary DSL -- a bare
    // identifier statement followed by `as` statements.
    if (!hasOverlay) return ctx.namedName;
    return ctx.namedName + "\n" + overlayLines.join("\n") + "\n";
  }

  const base = synthesizeProgram(ctx.edits);
  if (!hasOverlay) return base;
  if (base === null) return overlayLines.join("\n") + "\n";
  return base + "\n" + overlayLines.join("\n") + "\n";
}
