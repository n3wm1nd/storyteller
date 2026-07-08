// The set of LLM-backed agents the UI can send a file to, and which files
// each one applies to. This is a purely frontend concept — the backend
// dispatch (Server.Writer.File.Dispatch) doesn't validate agent-vs-file-type
// at all, it just executes whatever command arrives (see Agents tab design
// discussion) — so this registry is the single place that decision lives,
// shared by the input-bar dropdown (fileview.tsx) and the Agents tab
// (agentstab.tsx).

// The whole-story outline is `outline.md` (optionally in a subdir). Chapter
// beat sheets are `ch{N}.outline.md` — those are outputs of the split, not
// inputs to it, so only the bare `outline.md` gets the "generate beat sheets"
// action (see WRITER.md).
export function isOutlineFile(path: string): boolean {
  const name = decodeURIComponent(path.split("/").pop() ?? "");
  return name === "outline.md";
}

// A chat file is any path under a top-level chat/ folder (see WRITER.md) —
// gets the chatbot view (chatview.tsx) as an alternative to the ordinary
// prose/atom file view, not a replacement for it.
export function isChatFile(path: string): boolean {
  return decodeURIComponent(path).split("/")[0] === "chat";
}

// A context source is one distinct FileSystem instance an agent actually
// reads from today — not a user-inventable "slot." Its 'mode' is fixed by
// how the agent consumes it (ambient: gathered eagerly and injected in
// full, e.g. Continuation.hs's gatherFileContext; on-demand: exposed as a
// tool surface the LLM may or may not call, e.g. Chat.hs's glob/read_file
// tools) — never a user choice. Two sources both named "story" happening to
// point at the same branch are still independent: each gets its own
// exclude/include-only config (see context-source.tsx), because they're
// separate FileSystem instances the agent was independently wired to.
export interface ContextSourceDef {
  id: string;
  label: string;
  mode: "ambient" | "on-demand";
}

export interface AgentDef {
  id: string;
  label: string;
  description: string;
  // Optional grouping label for the Agents tab's left-hand list (see
  // agentstab.tsx) — undefined agents render ungrouped, ahead of any
  // category. Not assigned on today's 5 agents; exists so the list degrades
  // to categories once there are enough agents that a flat list stops being
  // scannable, without a separate registry shape for that day.
  category?: string;
  // Dotted Prompt.hs lookup keys this agent reads (see Storyteller.Core.Prompt)
  // — each doubles as a path on the "prompts" branch (dots -> slashes, ".md"
  // suffix). Empty for agents that never touch an LLM (append/note).
  promptKeys: string[];
  // Empty means the agent genuinely has no configurable context today (e.g.
  // Fixer's real path — 'reworkAtom' takes the selected atoms directly, no
  // FileSystem effect at all — see Storyteller.Writer.Agent.ReplaceTool).
  contextSources: ContextSourceDef[];
  appliesTo: (path: string) => boolean;
}

const STORY_AMBIENT: ContextSourceDef = { id: "story", label: "Story branch", mode: "ambient" };

export const AGENTS: AgentDef[] = [
  {
    id: "writer",
    label: "Writer",
    description: "Continues prose from the selection or file end.",
    promptKeys: ["agent.writer.system", "agent.writer.instructions"],
    contextSources: [STORY_AMBIENT],
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "fixer",
    label: "Fixer",
    description: "Rewrites the selected atoms in place per an instruction.",
    promptKeys: ["agent.fixer.system", "agent.fixer.template"],
    contextSources: [],
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "regenBeatSheet",
    label: "Regen · beat sheet",
    description: "Regenerates a chapter to fit its beat sheet.",
    promptKeys: ["agent.outline.beatsheet.system", "agent.outline.beatsheet.template"],
    contextSources: [STORY_AMBIENT],
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "outlineSplit",
    label: "Outline split",
    description: "Splits the whole-story outline into per-chapter beat sheets.",
    promptKeys: ["agent.outline.split.system", "agent.outline.split.template"],
    contextSources: [STORY_AMBIENT],
    appliesTo: isOutlineFile,
  },
  {
    id: "chat",
    label: "Chat",
    description: "Conversational co-writing for chat/ files.",
    promptKeys: ["agent.chat.system"],
    contextSources: [{ id: "story", label: "Story branch", mode: "on-demand" }],
    appliesTo: isChatFile,
  },
];

export function promptKeyToPath(key: string): string {
  return key.split(".").join("/") + ".md";
}

// What a context source's mode actually means for the model, in plain
// language — shown under the source's label in the Agents tab (see
// agentstab.tsx) so "ambient"/"on-demand" reads as a consequence, not just
// a badge to decode.
export function contextModeDescription(mode: ContextSourceDef["mode"]): string {
  return mode === "ambient"
    ? "Every included file below is gathered up front and sent to the model in full, alongside every request this agent makes."
    : "The model can browse and read included files from this branch itself, as tool calls, if it judges them relevant — nothing here is sent unless it asks.";
}
