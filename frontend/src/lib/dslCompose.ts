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
//     (`Storyteller.Context.DSL.Library.contextWriter`, see that module's
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
// (lore/**, chapters/**, style.md as separate named buckets, plus
// whoever presence ticks say is active) so a casual user toggling things
// off sees the same shape they'd have gotten for free, just minus what
// they removed -- never a surprise rewrite of the prompt's structure.
// See CONTEXT-DSL.md's "Worked examples" for the patterns this composes
// from.
//
// Each bucket is synthesized as a bare call to the server's own named
// definition (`context.lore`, `context.chapters`, `context.style`), not
// a hand-copied re-derivation of its DSL body. The backend went through
// exactly this bug once already (`Storyteller.Context.DSL.Library`'s own
// module haddock, and the fix history in `contextWriterDef`'s haddock):
// a second, drifting copy of "what counts as lore" is worse than no
// copy at all, and it also means a project's own committed override of
// `context.lore` etc. reaches a casual send the same way it reaches the
// server's own default -- a hand-inlined shape here could never see
// that override at all.
//
// Every program sent this way is wrapped in `context.writer`'s own
// `path:` parameter frame (see `synthesizeProgram`/`composeSendProgram`
// below) -- required, not cosmetic: an override whose declared arity
// doesn't match the definition it replaces is silently discarded server-
// side, no error anywhere, in favor of the compiled-in default
// (`Storyteller.Core.Context.resolveContextOverride`). A 0-arity program
// here would look like it worked and just quietly never take effect.

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

// `context.character(id)` produces a five-bucket character Value
// (sheet, blurb, full, journal, journalFull). Inlining the full
// definition here would duplicate the host-side curation
// (`journalDelta`, `characterBlurb`) that the backend ships; the casual
// UI's "include Alice" should give the user *exactly* what
// `context.character` gives -- no more, no less -- so we emit a call
// and rely on the compiled-in `context.character` being callable. The
// bare call works in any contexts-branch override or wire query (the
// interpreter's free-identifier fallback covers it); depth selection
// happens via named-entry reads on the call's result -- `read "x"`
// resolves a *named entry*, not a file, once repositioned into the
// call's own result via `in (...): ...` (see
// `Storyteller.Context.DSL.Compile.resolveRead`).
//
// `context.character`'s own bare/default statement is just
// `character.blurb charname` (its last line -- see that definition's
// own haddock), the same acquaintance-level line the "blurb" depth
// wants -- so both toggles read named entries explicitly rather than
// one of them leaning on the call's bare default, which would silently
// break if that default line ever changed.
//
// `x = ...; as "label": x; x` -- not just `as "label": ...` -- because a
// bare `as` block only ever contributes an *entry*, never the enclosing
// block's own default (see `Storyteller.Context.DSL.Compile.runStmts`'s
// `SAs` case); `renderText`/`renderMessages` (what actually reaches the
// LLM) read only that default (`rcContent`), never `rcEntries`. A user
// who explicitly picked "include Alice" needs Alice's content to be part
// of what the model sees, not just structurally reachable by name for
// nothing to ever read -- so this follows the same `x = ...; as f: x; x`
// idiom `contextLoreDef`'s own per-file loop already uses (Library.hs),
// to be both bare-emitted and named at once.
function characterLines(c: CharacterAdd): Lines {
  const label = c.id;
  if (c.depth === "blurb") {
    return [
      `x = in (context.character("${c.id}")): read "blurb"`,
      `as "${label}": x`,
      `x`,
    ];
  }
  // full -- sheet, everything else, and the curated journal, in the same
  // order `Storyteller.Context.DSL.Library.characterSummaryOf` reads
  // them in for every other consumer of a "full" character view.
  return [
    `x =`,
    `  in (context.character("${c.id}")):`,
    `    read "sheet"`,
    `    read "full"`,
    `    read "journal"`,
    `as "${label}": x`,
    `x`,
  ];
}

// `x = read "path" | orifempty ""; as "name": x; x` -- a single extra
// file, tolerant of being absent (the `orifempty ""` makes a missing
// file a no-op rather than a runtime failure, matching `contextStyle`'s
// own pattern) and, like `characterLines` above, both bare-emitted (so
// it actually reaches the model) and named (so it stays individually
// addressable).
function fileAddLines(f: FileAdd): Lines {
  const name = f.asName ?? f.path.split("/").pop()!.replace(/\.md$/, "");
  return [
    `x = read "${f.path}" | orifempty ""`,
    `as "${name}": x`,
    `x`,
  ];
}

// The auto-active-character fragment -- exactly `contextWriterDef`'s own
// trailing `for c in (charactersin path): as c: context.character c`
// (Library.hs), which folds in whoever presence ticks say is actually in
// this scene. Emitted only when the casual cast list is untouched (see
// `bodyLines`'s own comment): the moment a user picks characters by hand,
// their list becomes the sole source of truth, replacing this, rather
// than the two being merged and risking a same-name collision (`for c
// in ...: as c: ...` and a hand-added `as "<id>": ...` for the same
// present character would both try to bind the identical entry name,
// which the interpreter rejects as a duplicate `as` at runtime).
const activeCharacterLines: Lines = [
  `for c in (charactersin path):`,
  `  as c: context.character c`,
];

// Composes the structured edits into a full 1-arity DSL program body --
// everything `context.writer` itself needs *after* its own leading
// `path:` parameter line, unindented (the caller wraps and indents once,
// via `synthesizeProgram`\/`composeSendProgram`, rather than every
// fragment re-indenting itself relative to a frame it doesn't know
// about).
//
// Returns null when `edits` is structurally identical to the server's
// default -- a caller who has done nothing should send nothing. Any
// deviation, no matter how small, produces a full body (the synthesizer
// doesn't try to emit a diff; it re-emits the whole baseline-plus-edits
// shape). This matches the wire's "the program replaces the default"
// semantics (CONTEXT-DSL.md / Context.hs:174): there's no concept of
// "default plus additions" at the protocol level, so the frontend
// composes them client-side and sends the result.
function bodyLines(edits: ContextEdits): Lines | null {
  if (!isDirty(edits)) return null;

  const chunks: Lines = [];

  // Bare, not `as "lore": ...` -- these three mirror `contextWriterDef`'s
  // own real shape exactly (Library.hs: bare `context.lore`, bare `in
  // (context.chapters | exclude(path)): ...`, bare `context.other path`).
  // Nothing downstream ever reads a "lore"/"chapters"/"style" named entry
  // off `context.writer`'s own result, so wrapping these in `as` would
  // only give each bucket an entry nothing looks at while *also* failing
  // to reach the model at all (a bare `as` block doesn't feed the
  // enclosing default -- see `characterLines`'s own comment on that same
  // trap, which the casual "cast"/"extra files" additions genuinely need
  // to avoid, unlike these three).
  if (edits.baseline.lore) {
    // Excluded paths under lore/** aren't synthesized here -- the DSL's
    // `exclude` filter can only neuter content to empty, never shrink a
    // key set (`context.other`'s own haddock documents the same gotcha
    // for the default's own `exclude` use), so an "everything except
    // these" glob can't be expressed without enumerating the kept paths,
    // which would require the lore tree at synthesis time, and
    // `context.lore` itself takes no filtering parameter at all. Phase-1
    // exposes exclusions only via the power-user DSL editor (where the
    // user writes the real `exclude` themselves); the casual panel
    // sticks to positive additions. The field stays in `ContextEdits` so
    // a future Phase-2 synthesizer that does enumerate can fill it in.
    void edits.excludedLorePaths;
    chunks.push(`context.lore`);
  }

  if (edits.baseline.chapters) {
    // Excludes `path` itself -- mirrors `contextWriterDef`'s own `in
    // (context.chapters | exclude(path)): for f in **/*: read f`
    // (Library.hs), so a chapter already being written doesn't show up
    // as if it were prior content.
    chunks.push(`in (context.chapters | exclude(path)):`);
    chunks.push(...block([`for f in **/*: read f`]));
  }

  if (edits.baseline.style) {
    chunks.push(`context.style`);
  }

  // Cast list wins outright, presence auto-fill only when it's untouched
  // -- see `activeCharacterLines`'s own comment on why these two don't
  // merge.
  if (edits.characters.length === 0) {
    chunks.push(...activeCharacterLines);
  } else {
    for (const c of edits.characters) {
      chunks.push(...characterLines(c));
    }
  }

  for (const f of edits.extraFiles) {
    chunks.push(...fileAddLines(f));
  }

  return chunks;
}

// `bodyLines`, wrapped in the `path:` parameter frame every
// `context.writer` override needs. `Storyteller.Core.Context.
// resolveContextOverride` checks a staged override's own declared arity
// against the default's before accepting it -- silently, with no error
// surfaced anywhere -- and falls back to the compiled-in default on any
// mismatch (Context.hs:33-43, and the real end-to-end test at
// `Storyteller.Core.ContextSpec`'s `clientSubmittedContextProgramSpec`,
// which stages exactly this `"path:\n  ...\n"` shape). A 0-arity program
// here wouldn't error either -- it would just never take effect, which
// is worse: every "transient" edit sent without this frame was silently
// discarded server-side, indistinguishable from working.
export function synthesizeProgram(edits: ContextEdits): string | null {
  const lines = bodyLines(edits);
  if (lines === null) return null;
  return "path:\n" + block(lines).join("\n") + "\n";
}

// ─── Send-time composition ─────────────────────────────────────────────────

// The full send-time composition. Returns null = "send nothing" (omit
// the wire's `context` field entirely, server default runs); a string =
// the program text to put in the field. One function so the wire site
// (fileview.actions.ts) doesn't have to reason about layers.
export interface CallContext {
  path: string;
  edits: ContextEdits;
  // The composer's current parsed mentions (character ids). Driven
  // live from the textarea by mention-autocomplete.tsx.
  mentionCharacterIds: string[];
  // When non-null, the user has loaded a named function from the
  // contexts branch; its name overrides `edits` entirely -- the wire
  // calls it, passing `path` through (see below).
  namedName: string | null;
}

export function composeSendProgram(ctx: CallContext): string | null {
  // Mention overlay lines sit in the same `path:`-scoped body as
  // whatever else runs, not appended as a second top-level program --
  // there's only one parameter frame per wire program.
  const overlayLines: Lines = ctx.mentionCharacterIds.flatMap((id) => [
    `as "@${id}":`,
    `  in (context.character("${id}")):`,
    `    read "blurb"`,
  ]);

  let body: Lines;
  if (ctx.namedName !== null) {
    // A saved function on the `contexts` branch, called with `path`
    // exactly the way any other `context.*` definition needing it would
    // be -- see `Storyteller.Context.DSL.Library.contextWriter`'s own
    // Haddock on cross-definition reference being plain application, not
    // a special call form. The saved function itself has to be authored
    // as 1-arity (the same starter shape `dsl-editor.tsx`'s
    // `defaultStarter` provides) for this call to resolve to anything
    // useful; that's a property of what got saved, not of this call
    // site.
    body = [`${ctx.namedName} path`, ...overlayLines];
  } else {
    const edited = bodyLines(ctx.edits);
    if (edited === null && overlayLines.length === 0) return null;
    body = [...(edited ?? []), ...overlayLines];
  }

  return "path:\n" + block(body).join("\n") + "\n";
}
