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
  // empty, never actually shrink a key set.
  excludedLorePaths: string[];
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
  pastChaptersMode: "full",
  pinnedProgramNames: [],
};

// True iff `edits` differs from `DEFAULT_EDITS` in any visible way. Used to
// decide whether to send anything at all (omit every touched field's wire
// counterpart when false) and to light up the strip's "edited" affordance.
export function isDirty(edits: ContextEdits): boolean {
  if (edits.loreEnabled !== DEFAULT_EDITS.loreEnabled) return true;
  if (edits.excludedLorePaths.length > 0) return true;
  if (edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode) return true;
  if (edits.pinnedProgramNames.length > 0) return true;
  return false;
}

// ─── Synthesis ────────────────────────────────────────────────────────────

// The lore override's own body -- a bare 0-arity Context DSL program
// replacing `context.lore` for one call, or null when nothing about lore
// has been touched (omit the wire field, server's compiled-in
// `context.lore` runs).
//
// `excludedLorePaths` can't be expressed as a real `exclude` filter (see
// this module's own header on why: `exclude` only neuters content to
// empty, never shrinks a key set) -- Phase-1 only supports turning lore off
// entirely via `loreEnabled`; per-path exclusion synthesis is deferred
// until there's a glob primitive that can enumerate "everything except
// these" without walking the lore tree client-side. The field stays in
// `ContextEdits` so a future synthesizer can fill it in without a shape
// change.
export function synthesizeLoreOverride(edits: ContextEdits): string | null {
  if (edits.loreEnabled && edits.excludedLorePaths.length === 0) return null;
  void edits.excludedLorePaths;
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
