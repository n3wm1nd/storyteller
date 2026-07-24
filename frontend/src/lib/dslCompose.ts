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
  // Specific paths under lore/** to *exclude* even when loreEnabled is true
  // -- "include lore, but not lore/battle-log.md (too long)". A positive
  // glob list (the synthesizer enumerates what's left), not a real
  // `exclude` filter -- `exclude` in this DSL can only neuter content to
  // empty, never actually shrink a key set. This is the checkbox list's own
  // source of truth (see context-panel.tsx's LoreRow) -- `loreOverride`
  // below is *derived* from it (via synthesizeLoreOverride/
  // syncLoreOverrideFromCheckboxes) every time a checkbox changes, so
  // re-opening the panel shows the right checkboxes back, not just an
  // opaque already-generated program.
  excludedLorePaths: string[];
  // The lore override's own real body, in the user's own words -- a bare
  // 0-arity Context DSL program. Two ways it gets set: automatically,
  // derived from `excludedLorePaths` whenever a checkbox changes (the fast
  // path -- see syncLoreOverrideFromCheckboxes), or directly, by hand-
  // editing the generated code in the panel's CodeMirror editor. `null`
  // means "no custom program written yet" -- `loreEnabled` alone decides
  // what gets sent in that case (the plain on/off toggle, the original,
  // still-simplest case: nothing excluded, nothing hand-edited). A
  // non-null value always wins over the toggle.
  loreOverride: string | null;
  // True once the user has hand-edited `loreOverride`'s text directly,
  // rather than it only ever being a checkbox-derived synthesis. Once set,
  // checkbox clicks stop silently overwriting the user's own edit (see
  // LoreRow) -- editing always wins, per the explicit requirement that a
  // user who wants to hand-write the program still can, without checkbox
  // state clobbering it out from under them.
  loreOverrideHandEdited: boolean;
  pastChaptersMode: PastChaptersMode;
  // Named Context DSL functions to fold into this call's pinned content --
  // e.g. ["rules.magic"]. Each is sent verbatim as one `pinnedPrograms`
  // entry; the server resolves and renders it (see
  // Storyteller.Core.Context.resolveAdhoc0).
  pinnedProgramNames: string[];
}

export const DEFAULT_EDITS: ContextEdits = {
  loreEnabled: true,
  excludedLorePaths: [],
  loreOverride: null,
  loreOverrideHandEdited: false,
  pastChaptersMode: "full",
  pinnedProgramNames: [],
};

// True iff `edits` differs from `DEFAULT_EDITS` in any visible way. Used to
// decide whether to send anything at all (omit every touched field's wire
// counterpart when false) and to light up the strip's "edited" affordance.
export function isDirty(edits: ContextEdits): boolean {
  if (edits.loreEnabled !== DEFAULT_EDITS.loreEnabled) return true;
  if (edits.excludedLorePaths.length > 0) return true;
  if (edits.loreOverride !== null) return true;
  if (edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode) return true;
  if (edits.pinnedProgramNames.length > 0) return true;
  return false;
}

// ─── Synthesis ────────────────────────────────────────────────────────────

// One `read "path"` statement per included file, in `loreEntry`'s own
// "## Story background" + per-file shape (see
// Storyteller.Context.DSL.Library's `contextLoreDef`) -- close enough to
// the compiled-in default that a project reading the generated program
// recognizes it immediately, without reproducing `loreEntry`'s own `as`
// export (a per-call override has no name to export entries under -- see
// CONTEXT-DSL.md's Worked examples on `for`/multi-`read` shape).
//
// Always generates real code, even for the full-selection case (unlike
// deriveLoreOverride below, which treats "nothing excluded" as "don't
// override at all") -- this is what the checkbox UI's code preview shows
// so the editor is never left blank/placeholder text while the checkboxes
// show everything selected; see LoreRow's own `draft` computation.
export function renderLoreProgram(includedPaths: string[]): string {
  if (includedPaths.length === 0) return '"" \n';
  const reads = includedPaths.map((p) => `read "${p}"`).join("\n");
  return `"## Story background"\n${reads}\n`;
}

// Called from the checkbox UI (context-panel.tsx's LoreRow), not from the
// wire-composition path -- it needs the branch's live lore tree
// (lib/lore-selector.tsx's useLoreTree) to know what "everything except
// these" even means, and that's only available where a component is
// already subscribed to it, not in fileview.actions.ts's plain,
// hookless module. Every checkbox click re-derives `loreOverride` from
// scratch and writes the result straight into callContextStore -- by the
// time a command is actually sent, `loreOverride` already holds the
// finished program (see synthesizeLoreOverride below, which just forwards
// it).
//
// Unlike renderLoreProgram, deliberately returns null for the "nothing
// excluded" case -- an untouched selection should stay "no override sent"
// (the plain on/off toggle's own default posture), not a needlessly
// synthesized program that happens to match the compiled-in default.
export function deriveLoreOverride(allLorePaths: string[], excludedLorePaths: string[]): string | null {
  if (excludedLorePaths.length === 0) return null; // nothing excluded -- fall back to the plain toggle
  const included = allLorePaths.filter((p) => !excludedLorePaths.includes(p));
  return renderLoreProgram(included);
}

// The lore override's own body -- a bare 0-arity Context DSL program
// replacing `context.lore` for one call, or null when nothing about lore
// has been touched (omit the wire field, server's compiled-in
// `context.lore` runs). `loreOverride` is already the finished program by
// the time this runs (see deriveLoreOverride's own note) -- this function
// only decides what "untouched" means for the plain on/off toggle, the
// one case with no program at all.
export function synthesizeLoreOverride(edits: ContextEdits): string | null {
  if (edits.loreOverride !== null) return edits.loreOverride;
  if (!edits.loreEnabled) return '"" \n';
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
