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
  // suffix). The first entry is always the namespace root, which is
  // implicitly the system prompt (and, via configKeyToPath, the sampling
  // config) — there's no separate ".system" leaf; any other entries are
  // secondary prompts (templates, standing instructions) nested under it.
  // Empty for agents that never touch an LLM (append/note).
  promptKeys: string[];
  // Which role's LLM this agent calls (see Storyteller.Core.LLM.Role) — only
  // meaningful when promptKeys is non-empty. Determines which sampling keys
  // are valid in this agent's config override (see configFieldsHint):
  // ProseModel has no HasReasoning instance, so "reasoning" is silently
  // ignored if written into a prose agent's override (Storyteller.Core.LLM.
  // Settings.ProseSettings has no such field to decode into).
  configRole?: "prose" | "agent";
  appliesTo: (path: string) => boolean;
}

export const AGENTS: AgentDef[] = [
  {
    id: "writer",
    label: "Writer",
    description: "Continues prose from the selection or file end.",
    promptKeys: ["agent.writer", "agent.writer.instructions"],
    configRole: "prose",
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "fixer",
    label: "Fixer",
    description: "Rewrites the selected atoms in place per an instruction.",
    promptKeys: ["agent.fixer", "agent.fixer.template"],
    configRole: "agent",
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "regenBeatSheet",
    label: "Regen · beat sheet",
    description: "Regenerates a chapter to fit its beat sheet.",
    promptKeys: ["agent.outline.beatsheet", "agent.outline.beatsheet.template"],
    configRole: "prose",
    appliesTo: (path) => !isChatFile(path),
  },
  {
    id: "outlineSplit",
    label: "Outline split",
    description: "Splits the whole-story outline into per-chapter beat sheets.",
    promptKeys: ["agent.outline.split", "agent.outline.split.template"],
    configRole: "agent",
    appliesTo: isOutlineFile,
  },
  {
    id: "chat",
    label: "Chat",
    description: "Conversational co-writing for chat/ files.",
    promptKeys: ["agent.chat"],
    configRole: "agent",
    appliesTo: isChatFile,
  },
  {
    id: "askCharacter",
    label: "Ask Character",
    description: "Answers a question in character, using only that character's own branch (sheet, journal) — not the scene, not any other character.",
    promptKeys: ["agent.ask-character"],
    configRole: "agent",
    appliesTo: (path) => !isChatFile(path),
  },
];

export function promptKeyToPath(key: string): string {
  return key.split(".").join("/") + ".md";
}

// Same dotted key, same "prompts" branch, different suffix — a
// $key.llmsettings.yaml sibling of $key.md that Storyteller.Core.Prompt's
// getConfig/getConfigWithPrompt reads for sampling overrides (temperature/
// maxTokens/reasoning), on top of a system prompt. Only ever meaningful for
// an agent's first promptKey (its systemKey) — that's the one key every
// agent already passes to getConfigWithPrompt on the backend.
export function configKeyToPath(key: string): string {
  return key.split(".").join("/") + ".llmsettings.yaml";
}

// Which top-level YAML keys an agent's config override actually does
// anything with — for hint text only, not validation (an unrecognized key
// is just silently ignored by the backend's decoder, see ProseSettings/
// AgentSettings in Storyteller.Core.LLM.Settings).
export function configFieldsHint(role: AgentDef["configRole"]): string[] {
  if (role === "agent") return ["temperature", "maxTokens", "reasoning"];
  if (role === "prose") return ["temperature", "maxTokens"];
  return [];
}
