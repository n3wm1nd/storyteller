// A standalone mirror of Storyteller.Writer.Branches.classifyBranch
// (Haskell) — see WRITER.md's "Branch naming" for the authoritative rule,
// shared in prose between the two implementations, not in code. This
// exists because the plain "Branches" sidebar tab has no per-kind list
// from the server to render the way the "Characters" tab does (that one's
// already filtered server-side, via character.list/CharacterList) — it
// only ever gets the flat, unclassified branch-name list, same situation
// lib/library.ts's classifyPath is in for the Explorer tab (see that
// module's header).

export type BranchKind = "character" | "prompts" | "story";

// The one well-known, exact-match branch name — mirrors
// Storyteller.Core.Prompt.promptsBranchName.
const PROMPTS_BRANCH = "prompts";

// Classify a branch name. See this module's header and WRITER.md for the
// algorithm. There is no "other" case: a name matching neither
// "character/" nor the exact prompts branch is "story" by default, not an
// unrecognized fallback — a plain unprefixed name already works as a story
// branch everywhere in this codebase.
export function classifyBranch(name: string): BranchKind {
  if (name === PROMPTS_BRANCH) return "prompts";
  if (name.startsWith("character/")) return "character";
  return "story";
}

// A branch's own display name — its known prefix stripped, when present.
// Same "server hands over raw text, client decides further presentation"
// contract as lib/utils.ts's characterDisplayName, which layers sheet.md's
// own H1 on top of this same id-level fallback for the character case.
export function branchDisplayName(name: string): string {
  if (name.startsWith("character/")) return name.slice("character/".length);
  if (name.startsWith("story/")) return name.slice("story/".length);
  return name;
}
