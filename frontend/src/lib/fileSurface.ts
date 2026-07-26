// Which editing surface owns a file.
//
// This is the answer to "what *is* this file, as far as the UI is
// concerned" — and it is deliberately one answer, resolved once from
// (branch, path), rather than a set of scattered `path.endsWith(...)`
// checks at each place that needs to behave differently. A surface owns
// the whole centre pane, decides which centre tabs even exist, and
// supplies its own sidebar content; nothing about one surface's chrome is
// inherited by another.
//
// The distinction that matters: the prose/atom view is not "the default
// editor with extras", it is one surface among three. A `.dsl` or a prompt
// override is not prose with atom editing switched off — it's a different
// kind of thing, whose unit of change is the whole file (see
// Storage.Ops.saveWholeFile), with no atoms to select, no characters
// present in it, and no agent that writes into it. Everything those
// surfaces don't have follows from that one fact rather than from a list
// of exceptions.
//
// Adding a surface means adding a case here and a view/sidebar pair in
// app/ — never a new flag threaded through the prose view.

import { classifyBranch } from "./branches";
import { isChatFile } from "./agents";

export type FileSurface =
  // The atom/prose view: chapters, lore, journals — anything written by an
  // agent, tick by tick. app/prose-file-view.tsx.
  | "prose"
  // A Context DSL program (see CONTEXT-DSL.md). app/dsl-file-view.tsx.
  | "dsl"
  // A prompt override or its sampling config, on the "prompts" branch
  // (Storyteller.Core.Prompt). app/prompt-file-view.tsx.
  | "prompt";

export type CenterTab = "file" | "ticks" | "chat" | "agents";

export function fileSurfaceOf(branch: string | null, path: string | null): FileSurface {
  if (!path) return "prose";
  // Extension first, branch second: a `.dsl` is a DSL program wherever it
  // lives (the contexts branch by convention, but nothing enforces that),
  // whereas the prompts branch holds nothing that isn't a prompt override
  // — its `.md`/`.llmsettings.yaml` files are only prompt material because
  // of *where* they are, since those extensions mean something else
  // entirely anywhere else.
  if (path.endsWith(".dsl")) return "dsl";
  if (branch && classifyBranch(branch) === "prompts") return "prompt";
  return "prose";
}

// Which centre tabs apply to a file, in display order.
//
// "ticks" is branch-level (the whole chain, whatever is open), so it's
// always there. "agents" is a settings surface rather than a prose one —
// every editor can have agents worth configuring (an agent that helps
// write DSL, one that drafts prompt text), so it stays available
// everywhere; *which* agents it lists is the surface-dependent part, and
// that lives with the agent registry itself (see agents.ts's
// `agentsForSurface`), not here.
//
// "chat" is the one genuinely prose-only tab: it's this file's own
// conversation with a writing agent, which a program or a prompt override
// doesn't have.
export function centerTabsFor(surface: FileSurface, path: string | null): CenterTab[] {
  const chat: CenterTab[] = surface === "prose" && path && isChatFile(path) ? ["chat"] : [];
  return ["file", "ticks", ...chat, "agents"];
}
