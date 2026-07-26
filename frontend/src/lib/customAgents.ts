"use client";

// User-defined agents: agents a project invented for itself, with no
// compiled-in counterpart anywhere in the backend (see
// Storyteller.Writer.Agent.Custom).
//
// An agent *is* its files. There is no registry — not here, not on the
// server: committing `context/custom/critic.dsl` to the contexts branch
// creates an agent called "critic", deleting it removes one, and its
// behaviour is entirely that program (what it sees) plus, optionally,
// `agent/custom/critic.md` on the prompts branch (who it is) and
// `agent/custom/critic.llmsettings.yaml` (how it samples). That's the
// whole model, which is why discovery here is a glob over a file list the
// UI already keeps live rather than anything more structured: a registry
// would be a second place an agent could exist, and the two could disagree.
//
// Everything a discovered agent needs to appear in the UI is derived from
// its slug, and the two name-mapping functions below are exact mirrors of
// `customContextName`/`customPromptKey` in the Haskell module above — the
// convention is the contract between the two halves, so it's stated in
// both and asserted in test/Storyteller/CustomAgentSpec.hs.

import { useMemo } from "react";
import type { AgentDef } from "./agents";
import { isChatFile } from "./agents";
import type { CommandDef } from "./commands";
import { useBranchFiles } from "./branchFiles";
import { contextsBranchName, writeContextFunction, readContextDefault } from "./contextBranch";

// `context/custom/<slug>.dsl` on the contexts branch — the dotted name
// `context.custom.<slug>` with dots turned into slashes, the same rule
// every other context slot's path follows (Storyteller.Core.Context).
const CUSTOM_DIR = "context/custom/";
const DSL_SUFFIX = ".dsl";

// The compiled-in definition a brand-new agent's program starts as: the
// built-in Writer's own context, written in DSL. Fetched from the server
// rather than kept as a string here (see readContextDefault) — the
// template is a real library definition, pretty-printed from the actual
// parsed AST, so what a user sees on their first edit can't drift from
// what the backend would run.
export const CUSTOM_TEMPLATE_SLOT = "context.custom";

export function customContextName(slug: string): string {
  return `context.custom.${slug}`;
}

export function customPromptKey(slug: string): string {
  return `agent.custom.${slug}`;
}

// The mode/command id a custom agent is reached by in the composer,
// namespaced so it can never collide with a built-in mode ("write",
// "fix", …) whatever a project names its agent.
export function customAgentId(slug: string): `custom:${string}` {
  return `custom:${slug}`;
}

export function isCustomAgentId(id: string): boolean {
  return id.startsWith("custom:");
}

export function slugOfAgentId(id: string): string {
  return id.slice("custom:".length);
}

// Every custom agent on the contexts branch, from that branch's own file
// list. Pure so it can be read (and reasoned about) without a connection.
export function customAgentSlugs(files: string[]): string[] {
  return files
    .filter((p) => p.startsWith(CUSTOM_DIR) && p.endsWith(DSL_SUFFIX))
    .map((p) => p.slice(CUSTOM_DIR.length, -DSL_SUFFIX.length))
    .filter((slug) => slug.length > 0 && !slug.includes("/"))
    .sort((a, b) => a.localeCompare(b));
}

// "night-critic" -> "Night critic". The slug is the whole identity a
// project gives an agent (it names both of its files), so the label is
// derived from it rather than stored separately — a display name kept in
// its own file would be one more thing that can disagree with the name
// the agent actually answers to.
export function customAgentLabel(slug: string): string {
  const spaced = slug.replace(/[-_.]+/g, " ").trim();
  return spaced.charAt(0).toUpperCase() + spaced.slice(1);
}

// A discovered agent as an ordinary AgentDef — the same shape the static
// registry uses, so the Agents tab's prompt/sampling/context-slot editors
// work on one with no special casing at all. The only difference is when
// it's constructed: at runtime, from a file list, instead of at build time.
export function customAgentDef(slug: string): AgentDef {
  return {
    id: customAgentId(slug),
    label: customAgentLabel(slug),
    description: "A user-defined agent — its context program and prompt below are its entire behaviour.",
    category: "Custom agents",
    promptKeys: [customPromptKey(slug)],
    configRole: "prose",
    contextSlots: [customContextName(slug)],
    // Same applicability as the Writer it's modelled on: prose files, not
    // chat ones. A custom agent appends prose atoms exactly like the
    // writer does (Server.Writer.File.customWriter), so wherever the
    // writer makes no sense, neither does this.
    appliesTo: (path: string) => !isChatFile(path),
  };
}

// Live list of custom agents, kept current as .dsl files appear/vanish on
// the contexts branch — so creating an agent in the Agents tab makes it
// selectable in the composer without a reload.
export function useCustomAgents(): { slugs: string[]; agents: AgentDef[]; loading: boolean } {
  const files = useBranchFiles(contextsBranchName);
  const slugs = useMemo(() => (files ? customAgentSlugs(files) : []), [files]);
  const agents = useMemo(() => slugs.map(customAgentDef), [slugs]);
  return { slugs, agents, loading: files === null };
}

// Custom agents as slash commands, alongside the built-in ones — the same
// CommandDef the autocomplete popup already renders, so `/critic tighten
// this scene` completes and dispatches like any other command.
export function customAgentCommands(slugs: string[]): CommandDef[] {
  return slugs.map((slug) => ({
    name: slug,
    label: customAgentLabel(slug),
    description: "User-defined agent.",
    params: [],
  }));
}

// Create an agent: write its context program, seeded with the compiled-in
// template. Only the .dsl is written — a prompt file is optional, and its
// absence is meaningful (the agent runs on the backend's own default
// system prompt until someone commits one, exactly like an un-overridden
// built-in agent), so creating an empty one would turn "not customized
// yet" into "customized to say nothing".
export async function createCustomAgent(slug: string): Promise<void> {
  const template = await readContextDefault(CUSTOM_TEMPLATE_SLOT);
  await writeContextFunction(`custom.${slug}`, `${template.trimEnd()}\n`);
}
