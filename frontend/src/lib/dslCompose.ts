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
// this rollback exists to close) in favor of four small, independent wire
// slots on `chat.writer`/`correct.group` (see Server.Writer.File.Protocol's
// own Haddock on `ChatWriter`):
//
//   - `lore`: an optional Context DSL program overriding `context.lore` for
//     one call -- explicit lore/** entries the user curated (see WRITER.md).
//     Only synthesized when the user has touched the lore toggle/exclusions
//     -- untouched means omit the field, server's compiled-in lore runs.
//   - `other`: `lore`'s own twin for `context.other` -- the catch-all "loose
//     notes and drafts" bucket (anything not lore/chapters/style.md/chat
//     scratch). Same synthesis rule as `lore`.
//   - `pastChaptersMode`: `"full"` | `"compressed"` -- a plain toggle, never
//     a program.
//   - `pinnedPrograms`: bare 0-arity program strings (typically just a name,
//     like `rules.magic`), each resolved server-side and folded into this
//     call's pinned/authors-notes content.
//
// Style and character identity are entirely agent-owned now -- no client
// knob over either, and no casual-panel UI for them either (cast-list
// picking and ad-hoc extra-file adding were both dropped in this same pass;
// see the project chat).

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
  // 'loreEnabled'/'loreOverride''s own twin for `context.other` -- the
  // catch-all "loose notes and drafts" bucket (see this file's own header).
  // Same precedence/checkbox-input-method contract as the lore pair, just
  // scoped to `context.other`'s own candidate file set (context-panel.tsx's
  // OtherRow) instead of `context.lore`'s.
  otherEnabled: boolean;
  otherOverride: string | null;
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
  otherEnabled: true,
  otherOverride: null,
  pastChaptersMode: "full",
  pinnedProgramNames: [],
};

// True iff `edits` differs from `DEFAULT_EDITS` in any visible way. Used to
// decide whether to send anything at all (omit every touched field's wire
// counterpart when false) and to light up the strip's "edited" affordance.
export function isDirty(edits: ContextEdits): boolean {
  if (edits.loreEnabled !== DEFAULT_EDITS.loreEnabled) return true;
  if (edits.loreOverride !== null) return true;
  if (edits.otherEnabled !== DEFAULT_EDITS.otherEnabled) return true;
  if (edits.otherOverride !== null) return true;
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
// (`contextLoreDef`/`contextOtherDef`, both of which use `loreEntry` for
// the identical per-file framing) itself produces -- only *which files*
// is chosen here, never *how one file renders*. `[path]` is Parser.hs's
// bracket-glob literal (a single-match glob reference, unambiguous even
// for a path containing spaces or other characters a bare token can't
// hold) -- confirmed end-to-end against the real parser+evaluator in
// test/Storyteller/Core/ContextSpec.hs's "loreEntry [path]" cases,
// including one with spaces in the filename.
//
// `lore` and `other` share this exact generator/parser shape (both slots
// use `loreEntry` for their per-file framing -- see `contextLoreDef`\/
// `contextOtherDef`), so it's parametrized by header text rather than
// duplicated: `renderEntryProgramPrefix`/`parseEntryProgramPrefix` below
// are the one implementation both `LORE_PROGRAM_HEADER` and
// `OTHER_PROGRAM_HEADER` go through.
//
// The two slots differ in ARITY, though, and that's a real wire-format
// difference, not cosmetic: `context.lore` is 0-arity (`contextLoreDef`
// takes no parameter, see Library.hs), so its override is a bare,
// unparameterized body -- exactly what `LoreRow`'s checkboxes already
// generate. `context.other` is 1-arity (`contextOtherDef` is `path: ...`,
// framed against the file being written -- see that definition's own
// Haddock), so *any* override for it -- including the checkbox-generated
// one, and the disabled/"send nothing" case -- has to parse as a `path:`
// definition too, or 'Storyteller.Core.Context.resolveOverrideDefinition'
// rejects it on arity mismatch and the whole override silently falls
// back to the compiled-in default (see that module's own Haddock on
// "arity has to match exactly"). Confirmed against the real pretty-printed
// source at `GET /context-default/context.other`:
//
//   path:
//     "## Other notes"
//
//     for f in ...
//
// -- a `path:` header, then every line of the body indented one level
// (`Storyteller.Context.DSL.PrettyPrint`'s own convention). `other`'s
// generator/parser wrap/unwrap exactly that shape; `lore`'s don't wrap at
// all, since 0-arity has no parameter header to add.
const LORE_PROGRAM_HEADER = '"## Story background"';
const OTHER_PROGRAM_HEADER = '"## Other notes"';
const OTHER_PARAM_LINE = "path:";
const INDENT = "  ";

function renderEntryProgramPrefix(header: string, includedPaths: string[]): string {
  const lines = includedPaths.map((p) => `loreEntry [${p}]`).join("\n");
  // The header ALONE (no `loreEntry` lines) is itself a valid, real
  // checkbox-owned state -- "zero files selected," not "nothing." Once
  // the checkboxes have been engaged at all, unchecking every last one
  // must still keep the header, never collapse all the way back to the
  // true untouched sentinel (`""`, see parseEntryProgramPrefix's own
  // header) -- a project's real, intentional "include nothing" choice is
  // not the same state as "never touched this at all," and conflating
  // them was the bug that made the header silently vanish when the last
  // box was unchecked.
  return includedPaths.length === 0 ? `${header}\n` : `${header}\n${lines}\n`;
}

function indentBody(body: string): string {
  return body
    .split("\n")
    .map((line) => (line.length === 0 ? line : INDENT + line))
    .join("\n");
}

function dedentBody(body: string): string | null {
  const lines = body.split("\n");
  const dedented: string[] = [];
  for (const line of lines) {
    if (line.length === 0) { dedented.push(line); continue; }
    if (!line.startsWith(INDENT)) return null;
    dedented.push(line.slice(INDENT.length));
  }
  return dedented.join("\n");
}

// Whole-program convenience for a caller that just wants a complete,
// self-contained draft from a path list (e.g. seeding a brand-new
// override) -- the prefix with nothing after it.
export function renderLoreProgram(includedPaths: string[]): string {
  return renderEntryProgramPrefix(LORE_PROGRAM_HEADER, includedPaths);
}

// `context.other` is 1-arity -- wraps the identical entry-prefix shape in
// the `path:` header its own arity requires (see this section's own
// header), body indented one level to match the real pretty-printer's
// convention.
export function renderOtherProgram(includedPaths: string[]): string {
  return `${OTHER_PARAM_LINE}\n${indentBody(renderEntryProgramPrefix(OTHER_PROGRAM_HEADER, includedPaths))}`;
}

// Recovers the chosen path list AND whatever text follows the generated
// prefix. Only needs the BEGINNING of `program` to be recognizable, not
// the whole thing: `header` (e.g. `"## Story background"\n`) marks where
// checkbox-owned `loreEntry [path]` lines start, and matching stops the
// moment a line doesn't fit that shape -- everything from there on,
// whatever it is (the real compiled default's own glob-walking body,
// hand-written prose, nothing at all), is `rest`, carried through
// untouched by any insert/remove. This is a reliable, surgical operation
// regardless of what `rest` contains: a `loreEntry [path]` line only ever
// needs inserting into or removing from the recognized prefix, never
// touching what follows.
//
// The true untouched sentinel is `""` (checkboxes never engaged at all --
// see synthesizeLoreOverride/synthesizeOtherOverride/useLoreDraft);
// anything that doesn't start with `header` at all (a hand-edit that
// removed it, say) returns `null` -- there's no reliable insertion point
// without the header to anchor on.
function parseEntryProgramPrefix(header: string, program: string): { paths: string[]; rest: string } | null {
  if (program === "") return { paths: [], rest: "" };
  if (!program.startsWith(header + "\n")) return null;
  let cursor = header.length + 1;
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
  const rest = program.slice(cursor);
  return { paths, rest };
}

// `parseEntryProgramPrefix`'s own 1-arity counterpart: strips the leading
// `path:` header and one level of indentation before delegating to the
// shared prefix parser, then re-indents whatever `rest` it hands back so
// round-tripping (parse, toggle a box, re-render) always produces the
// exact same `path:`-wrapped shape. `null` whenever the `path:` header or
// the body's own indentation doesn't match -- a hand-edit that broke
// either has nothing truthful left for the checkboxes to anchor on, same
// "go inert" contract `parseEntryProgramPrefix` already has for lore.
function parseOtherEntryPrefix(program: string): { paths: string[]; rest: string } | null {
  if (program === "") return { paths: [], rest: "" };
  if (!program.startsWith(`${OTHER_PARAM_LINE}\n`)) return null;
  const dedented = dedentBody(program.slice(OTHER_PARAM_LINE.length + 1));
  if (dedented === null) return null;
  const parsed = parseEntryProgramPrefix(OTHER_PROGRAM_HEADER, dedented);
  if (parsed === null) return null;
  return { paths: parsed.paths, rest: indentBody(parsed.rest) };
}

// The caller-facing form: the chosen paths, or `[]` if `program` isn't
// checkbox-owned at all (see `parseEntryProgramPrefix`'s own header) --
// callers that need to distinguish "zero selected" from "not ours to
// parse" (to disable checkboxes) should call `isLoreProgramCheckboxOwned`\/
// `isOtherProgramCheckboxOwned` instead.
export function parseLoreProgram(program: string): string[] {
  return parseEntryProgramPrefix(LORE_PROGRAM_HEADER, program)?.paths ?? [];
}

export function parseOtherProgram(program: string): string[] {
  return parseOtherEntryPrefix(program)?.paths ?? [];
}

// True iff `program` is entirely checkbox-owned (the true untouched
// sentinel `""`, the header alone, or an exact generated block -- see
// parseEntryProgramPrefix's own header) -- what a caller like LoreRow\/
// OtherRow needs to decide whether its checkboxes have anything truthful
// to show/toggle, since `parseLoreProgram`\/`parseOtherProgram`'s own
// `[]` fallback can't distinguish "zero selected, but still ours" from
// "not ours at all."
export function isLoreProgramCheckboxOwned(program: string): boolean {
  return parseEntryProgramPrefix(LORE_PROGRAM_HEADER, program) !== null;
}

export function isOtherProgramCheckboxOwned(program: string): boolean {
  return parseOtherEntryPrefix(program) !== null;
}

// Toggle one path's membership in the draft's own checkbox-owned prefix,
// leaving anything the user wrote after that prefix untouched. Returns
// `null` when `program` isn't checkbox-owned at all -- nothing truthful
// for a checkbox click to update (see `parseEntryProgramPrefix`'s own
// header); the caller must treat this the same as its own disabled state,
// never fall back to prepending onto unrecognized text.
export function toggleLorePathInProgram(program: string, entryPath: string): string | null {
  const parsed = parseEntryProgramPrefix(LORE_PROGRAM_HEADER, program);
  if (parsed === null) return null;
  const nextPaths = parsed.paths.includes(entryPath)
    ? parsed.paths.filter((p) => p !== entryPath)
    : [...parsed.paths, entryPath];
  return renderEntryProgramPrefix(LORE_PROGRAM_HEADER, nextPaths) + parsed.rest;
}

export function toggleOtherPathInProgram(program: string, entryPath: string): string | null {
  const parsed = parseOtherEntryPrefix(program);
  if (parsed === null) return null;
  const nextPaths = parsed.paths.includes(entryPath)
    ? parsed.paths.filter((p) => p !== entryPath)
    : [...parsed.paths, entryPath];
  return `${OTHER_PARAM_LINE}\n${indentBody(renderEntryProgramPrefix(OTHER_PROGRAM_HEADER, nextPaths))}${parsed.rest}`;
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

// `synthesizeLoreOverride`'s own twin for `context.other`, identical
// precedence -- but `context.other`'s disabled/empty case has to be a
// `path:`-wrapped 1-arity program too (`path:\n  ""\n`), not the bare
// 0-arity `'"" \n'` lore sends: an override's arity has to match its
// slot's exactly (see this section's own header on
// `resolveOverrideDefinition`), and sending the wrong one would silently
// fall back to the compiled-in default instead of actually excluding
// everything.
export function synthesizeOtherOverride(edits: ContextEdits): string | null {
  if (!edits.otherEnabled) return `${OTHER_PARAM_LINE}\n${INDENT}"" \n`;
  if (edits.otherOverride !== null) return edits.otherOverride;
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
  other?: string;
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

  const other = synthesizeOtherOverride(ctx.edits);
  if (other !== null) fields.other = other;

  if (ctx.edits.pastChaptersMode !== "full") fields.pastChaptersMode = ctx.edits.pastChaptersMode;

  const pinnedPrograms = [
    ...ctx.edits.pinnedProgramNames,
    ...ctx.mentionCharacterIds.map((id) => `in (context.character("${id}")): read "blurb"`),
  ];
  if (pinnedPrograms.length > 0) fields.pinnedPrograms = pinnedPrograms;

  return fields;
}
