"use client";

// The frontend's structured-state <-> wire-field bridge for the writer's
// per-call context slots.
//
// This module used to synthesize one whole Context DSL program standing in
// for the entire `context.writer` override (lore + chapters + other +
// characters + conversation history, all in one hand-built string kept in
// sync with the backend's own `contextWriterDef`). That design was rolled
// back (see the project chat: full per-call DSL control over the *entire*
// writer context moved expertise away from the agent, doubled every piece
// of context-assembly knowledge across two hand-synced implementations --
// this file and Library.hs -- and repeatedly drifted, the exact bug class
// this rollback exists to close) in favor of three small, independent wire
// slots on `chat.writer`/`correct.group` (see Server.Writer.File.Protocol's
// own Haddock on `ChatWriter`):
//
//   - `lore`: an optional Context DSL program overriding `context.lore` for
//     one call. Only synthesized when the user has touched the lore
//     toggle/exclusions -- untouched means omit the field, server's
//     compiled-in lore runs.
//   - `pastChaptersMode`: `"full"` | `"compressed"` -- a plain toggle, never
//     a program.
//   - `pinnedPrograms`: bare 0-arity program strings (typically just a name,
//     like `rules.magic`), each resolved server-side and folded into this
//     call's pinned/authors-notes content.
//
// Style, character identity, and "other notes" are entirely agent-owned now
// -- no client knob over any of them, and no casual-panel UI for them
// either (cast-list picking and ad-hoc extra-file adding were both dropped
// in this same pass; see the project chat).

// ─── Structured state ─────────────────────────────────────────────────────

export type PastChaptersMode = "full" | "compressed";

// The casual panel's own editable state for one file. Each field is
// independent -- there's no single synthesized "whole program" standing in
// for all of them together anymore, since each maps to its own wire field.
export interface ContextEdits {
  loreEnabled: boolean;
  // The lore override's own real body -- a bare 0-arity Context DSL
  // program, or `null` meaning "no custom program written yet" (the plain
  // on/off toggle, `loreEnabled` alone, decides what gets sent in that
  // case). There is no separately-tracked checkbox/exclusion state
  // anymore: the checkbox list (context-panel.tsx's LoreRow) is a pure
  // input METHOD onto this same field, never a second source of truth.
  // Checking a box regenerates `loreOverride` via `renderLoreProgram`;
  // whether the checkboxes can show anything at all is decided by
  // re-*parsing* this text on every render (see `parseLoreProgram`) --
  // if it matches what the generator itself would have produced, the
  // checkboxes reflect that; otherwise they go inert, because a hand
  // edit that changed the shape has nothing truthful left for them to
  // show ("the program is the ground truth; the checkboxes are a view
  // onto it, not the other way around" -- see the project chat that
  // settled this after `exclude()` on a bare `context.lore` reference
  // turned out not to work: `exclude` only shrinks `valueEntries`, never
  // `valueDefault`, and rendering a bare value reads only `valueDefault`
  // -- see Storyteller.Context.DSL.Compile's `shrinkEntries`/
  // Rendering.hs's `renderText`. `renderLoreProgram` below instead
  // reproduces `loreEntry`'s own per-file heading+content shape directly,
  // scoped to an explicit path list, which needs no reflatten and always
  // matches what it claims to show).
  loreOverride: string | null;
  pastChaptersMode: PastChaptersMode;
  // Named Context DSL functions to fold into this call's pinned content --
  // e.g. ["rules.magic"]. Each is sent verbatim as one `pinnedPrograms`
  // entry; the server resolves and renders it (see
  // Storyteller.Core.Context.resolveAdhoc0).
  pinnedProgramNames: string[];
}

export const DEFAULT_EDITS: ContextEdits = {
  loreEnabled: true,
  loreOverride: null,
  pastChaptersMode: "full",
  pinnedProgramNames: [],
};

// True iff `edits` differs from `DEFAULT_EDITS` in any visible way. Used to
// decide whether to send anything at all (omit every touched field's wire
// counterpart when false) and to light up the strip's "edited" affordance.
export function isDirty(edits: ContextEdits): boolean {
  if (edits.loreEnabled !== DEFAULT_EDITS.loreEnabled) return true;
  if (edits.loreOverride !== null) return true;
  if (edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode) return true;
  if (edits.pinnedProgramNames.length > 0) return true;
  return false;
}

// ─── Checkbox generator/parser ──────────────────────────────────────────────
//
// The checkbox list is a pure INPUT METHOD onto `loreOverride`'s own text,
// never a second source of truth for it (see `ContextEdits.loreOverride`'s
// own doc comment for why an `exclude()`-based approach doesn't work here).
// `renderLoreProgramPrefix` is the one and only shape the checkboxes ever
// write; `parseLoreProgram` recognizes exactly that shape as a PREFIX of
// the draft (not the whole thing) and hands back whatever follows it
// untouched, so the checkboxes can always answer "does the draft start
// with what I'd have generated" by re-parsing it, and a user can freely
// hand-write more DSL after the generated block without losing checkbox
// control over the part it owns.
//
// The empty-selection form (`renderLoreProgramPrefix([])`) is the empty
// string -- so "nothing chosen via checkboxes yet" is trivially a prefix
// of ANY draft, including a project's own hand-written default source
// (`contextLoreDef`'s glob-walking body, or any other real DSL). This is
// what makes the untouched/no-override case work correctly: it's not a
// special case, it's the same prefix rule with zero chosen paths.

// One `loreEntry [path]` line per chosen file -- a direct call to the
// real `loreEntry` library function (Storyteller.Context.DSL.Library),
// never a reproduction of its `"## %f%"` heading + `read f` body, so the
// per-file rendering can never drift from what the real default
// (`contextLoreDef`, which walks `lore/**/*` the same way) itself
// produces -- only *which files* is chosen here, never *how one file
// renders*. `[path]` is Parser.hs's bracket-glob literal (a single-match
// glob reference, unambiguous even for a path containing spaces or other
// characters a bare token can't hold) -- confirmed end-to-end against the
// real parser+evaluator in test/Storyteller/Core/ContextSpec.hs's
// "loreEntry [path]" cases, including one with spaces in the filename.
const LORE_PROGRAM_HEADER = '"## Story background"';

function renderLoreProgramPrefix(includedPaths: string[]): string {
  if (includedPaths.length === 0) return "";
  const lines = includedPaths.map((p) => `loreEntry [${p}]`).join("\n");
  return `${LORE_PROGRAM_HEADER}\n${lines}\n`;
}

// Whole-program convenience for a caller that just wants a complete,
// self-contained draft from a path list (e.g. seeding a brand-new
// override) -- the prefix with nothing after it.
export function renderLoreProgram(includedPaths: string[]): string {
  return renderLoreProgramPrefix(includedPaths);
}

// Recovers the chosen path list AND whatever text follows the generated
// prefix -- but ONLY when `program` is itself entirely checkbox-owned:
// either empty (zero selections, the checkboxes' own starting point) or an
// exact generated block (the header followed by one or more well-formed
// `loreEntry [path]` lines, optionally with more checkbox-owned lines
// after -- "rest" only ever holds trailing text the checkboxes still
// recognize as their own shape, via the same recursive match, never
// arbitrary prose). Anything else -- most importantly the real compiled
// default's own body (`contextLoreDef`'s `for f in lore/**/*: ...`, which
// happens to start with the identical "## Story background" banner) or
// any hand-written program -- returns `null`: there is no truthful
// "toggle one file" operation on source the checkboxes didn't generate,
// since blindly prepending a fresh block in front of it would either
// duplicate that banner or silently graft checkbox state onto content
// with completely different semantics (see the project chat: this was
// exactly the bug that produced two "## Story background" blocks
// concatenated when a first checkbox click started from the compiled
// default text). A caller with `null` here must disable its checkboxes
// entirely, not fall back to a best-effort zero-selection reading.
function parseLoreProgramPrefix(program: string): { paths: string[]; rest: string } | null {
  if (program === "") return { paths: [], rest: "" };
  if (!program.startsWith(LORE_PROGRAM_HEADER + "\n")) return null;
  let cursor = LORE_PROGRAM_HEADER.length + 1;
  const paths: string[] = [];
  // Each generated line is `loreEntry [<path>]\n` -- `]` can't appear
  // inside a real path (Parser.hs's bracket-glob has no escaping, so `]`
  // is unconditionally the terminator), making each line's own bounds
  // unambiguous with a plain indexOf scan, no JSON-style quoting/escaping
  // needed at all.
  for (;;) {
    if (!program.startsWith("loreEntry [", cursor)) break;
    const openIdx = cursor + "loreEntry [".length;
    const closeIdx = program.indexOf("]", openIdx);
    if (closeIdx === -1 || program[closeIdx + 1] !== "\n") break; // malformed -- not a generated line after all
    paths.push(program.slice(openIdx, closeIdx));
    cursor = closeIdx + 2;
  }
  if (paths.length === 0) return null; // header alone, or followed by non-generated text -- not checkbox-owned
  const rest = program.slice(cursor);
  return { paths, rest };
}

// The caller-facing form: the chosen paths, or `[]` if `program` isn't
// checkbox-owned at all (see `parseLoreProgramPrefix`'s own header) --
// callers that need to distinguish "zero selected" from "not ours to
// parse" (to disable checkboxes) should call `parseLoreProgramPrefix`
// directly instead.
export function parseLoreProgram(program: string): string[] {
  return parseLoreProgramPrefix(program)?.paths ?? [];
}

// Toggle one path's membership in the draft's own checkbox-owned prefix,
// leaving anything the user wrote after that prefix untouched. Returns
// `null` when `program` isn't checkbox-owned at all -- nothing truthful
// for a checkbox click to update (see `parseLoreProgramPrefix`'s own
// header); the caller must treat this the same as its own disabled state,
// never fall back to prepending onto unrecognized text.
export function toggleLorePathInProgram(program: string, entryPath: string): string | null {
  const parsed = parseLoreProgramPrefix(program);
  if (parsed === null) return null;
  const nextPaths = parsed.paths.includes(entryPath)
    ? parsed.paths.filter((p) => p !== entryPath)
    : [...parsed.paths, entryPath];
  return renderLoreProgramPrefix(nextPaths) + parsed.rest;
}

// The lore override's own body -- a bare 0-arity Context DSL program
// replacing `context.lore` for one call, or null when nothing about lore
// has been touched (omit the wire field, server's compiled-in
// `context.lore` runs, or this project's own committed
// `context/lore.dsl` override of it).
//
// `loreEnabled` is checked FIRST, ahead of `loreOverride` -- the top-level
// "Story lore" toggle is an explicit kill switch and must win regardless
// of whatever text happens to sit in `loreOverride` (e.g. from an earlier
// per-file checkbox selection): checking individual lore files, then
// disabling "Story lore" entirely, must send an empty program, not
// whatever the checkboxes last generated. `loreOverride` is left
// untouched by `setLoreEnabled` (callContextStore.ts) so re-enabling
// restores it -- this function is what has to apply the precedence, not
// the store clearing it out.
export function synthesizeLoreOverride(edits: ContextEdits): string | null {
  if (!edits.loreEnabled) return '"" \n';
  if (edits.loreOverride !== null) return edits.loreOverride;
  return null;
}

// ─── Send-time composition ─────────────────────────────────────────────────

export interface CallContext {
  path: string;
  edits: ContextEdits;
  // The composer's current parsed mentions (character ids). Driven live
  // from the textarea by mention-autocomplete.tsx. Folded into
  // `pinnedPrograms` as a per-mention blurb read, same as the old design.
  mentionCharacterIds: string[];
}

export interface WriterContextWireFields {
  lore?: string;
  pastChaptersMode?: PastChaptersMode;
  pinnedPrograms?: string[];
}

// The full send-time composition -- the wire's own optional fields for
// `chat.writer`/`correct.group`, each independent, each omitted when
// nothing calls for it. One function so the wire site
// (fileview.actions.ts) doesn't have to reason about which fields to set.
export function composeWriterContextFields(ctx: CallContext): WriterContextWireFields {
  const fields: WriterContextWireFields = {};

  const lore = synthesizeLoreOverride(ctx.edits);
  if (lore !== null) fields.lore = lore;

  if (ctx.edits.pastChaptersMode !== "full") fields.pastChaptersMode = ctx.edits.pastChaptersMode;

  const pinnedPrograms = [
    ...ctx.edits.pinnedProgramNames,
    ...ctx.mentionCharacterIds.map((id) => `in (context.character("${id}")): read "blurb"`),
  ];
  if (pinnedPrograms.length > 0) fields.pinnedPrograms = pinnedPrograms;

  return fields;
}
