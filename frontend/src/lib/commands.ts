// Slash commands for the prose input bar (fileview.tsx's InputBar) — one
// per routable agent already listed in AGENT_META there (write/fix/append/
// note/regen). This registry only adds the metadata AGENT_META doesn't
// carry: a description and per-parameter descriptions, so the autocomplete
// popup (command-autocomplete.tsx) has something to show. Dispatch still
// goes through the same onWrite/onFix/... callbacks InputBar already has —
// a command is just an alternate way to pick + parameterize one of them,
// not a new code path to the server.
export interface CommandParamDef {
  name: string;
  description: string;
  // Presence-only param, e.g. "@beat" — no "=value" expected.
  flag?: boolean;
}

export interface CommandDef {
  name: string;
  label: string;
  description: string;
  params: CommandParamDef[];
  // True for a command that fires on its own, with no body text — e.g.
  // /summarize just triggers a pass, there's nothing to say. Every other
  // command needs real text to be worth sending at all (InputBar's fire()
  // only dispatches a command when parsed.text is non-empty; this opts out
  // of that check for the one command where an empty body is the norm, not
  // an accidental blank send).
  noText?: boolean;
}

export const COMMANDS: CommandDef[] = [
  { name: "write", label: "Write", description: "Continue the prose from the selection or file end.", params: [] },
  { name: "fix", label: "Fix", description: "Rewrite the selected atoms in place per this instruction.", params: [] },
  { name: "append", label: "Append", description: "Append the text verbatim, instantly (no LLM).", params: [] },
  { name: "note", label: "Note", description: "Attach the text as a note on the current selection.", params: [] },
  {
    name: "regen", label: "Regen", description: "Regenerate this chapter to fit its beat sheet.",
    params: [{ name: "beat", description: "Regenerate beat-by-beat instead of the whole chapter.", flag: true }],
  },
  {
    name: "roleplay", label: "Roleplay",
    description: "Interrogate every character present in the scene, then write what happens next.",
    params: [],
  },
  // Not one of the write/fix/append/note/regen agents AGENT_META lists —
  // this doesn't edit the file at all, it asks a character a question,
  // answered from only their own branch (see Server.Writer.File.
  // askCharacter). Recorded on this branch, surfaced through the same
  // characterAnswers ring buffer the sidebar's per-character Ask panel
  // reads, not inline in the file — see uiStore.ts.
  {
    name: "ask", label: "Ask", description: "Ask a character a question, answered from only their own branch.",
    params: [{ name: "character", description: "Which character to ask (their branch id, e.g. character/alice)." }],
  },
  // Same shape as /ask, but the opposite direction: append the text
  // verbatim to that character's own journal (character-sidebar.tsx's
  // JournalPanel append box, just reached without leaving the composer) —
  // a quick structured way to correct/inform a character, alongside (not
  // instead of) writing to their journal by hand. Requires that
  // character's journal connection already open, same precondition the
  // manual append box has (see appendJournal in character-sidebar.actions.ts).
  {
    name: "inform", label: "Inform", description: "Append a fact directly to a character's journal, without leaving the composer.",
    params: [{ name: "character", description: "Which character to inform (their branch id, e.g. character/alice)." }],
  },
  // See Server.Writer.File.summaryTicksFor and lib/library.ts's
  // summaryKindsFor — this file's toolbar already exposes the same action
  // as a button when a summarizable file is open; the command is a
  // no-context-switch alternative, same relationship /ask has to the
  // sidebar's own Ask panel. Idempotent: free to fire with nothing new to
  // summarize, no params (which kind(s) to run is this file's own static
  // classification, not something worth asking the user to name).
  {
    name: "summarize", label: "Summarize",
    description: "Run a summarizer pass on this file — free to call, only does LLM work once there's actually something new to compress.",
    params: [], noText: true,
  },
];

// ChatView's input only ever sends a conversational turn (chatConverse) —
// none of write/fix/append/regen are atom-editing operations that make
// sense there. 'note' is the one exception: annotations are file-wide, not
// atom-scoped (see fileview.actions.ts's chatNote), so it still applies.
export const CHAT_COMMANDS: CommandDef[] = COMMANDS.filter((c) => c.name === "note");

export interface ParsedCommand {
  name: string;
  params: Record<string, string>;
  text: string;
}

// `/name @p1=v1 free text @p2="v 2"` — params can appear anywhere after the
// command name (right after it, interleaved, or trailing); whatever text is
// left once they're stripped out is the payload, per the syntax the user
// specified: params are `@key=value` or `@key="quoted value"`, flags are
// bare `@key`.
export function parseCommand(input: string): ParsedCommand | null {
  const trimmed = input.trim();
  const head = /^\/(\S+)/.exec(trimmed);
  if (!head) return null;
  const params: Record<string, string> = {};
  const rest = trimmed.slice(head[0].length).replace(
    /@(\w+)(?:=("([^"]*)"|(\S+)))?/g,
    (_all: string, key: string, _full: string, quoted: string | undefined, bare: string | undefined) => {
      params[key] = quoted !== undefined ? quoted : bare !== undefined ? bare : "true";
      return "";
    },
  );
  return { name: head[1], params, text: rest.trim().replace(/\s+/g, " ") };
}

export interface Suggestion {
  replaceStart: number;
  replaceEnd: number;
  insertText: string;
  display: string;
  description: string;
}

// The token the caret sits in: scan back to the nearest whitespace (or
// start of input). Completion only ever edits this token, never anything
// after the caret — a param typed earlier in the text is left alone.
// Exported for lib/mentions.ts's mentionSuggestions, which needs the exact
// same "what token is the caret in" scan for its own '@' trigger.
export function currentToken(text: string, cursor: number): { start: number; word: string } {
  let start = cursor;
  while (start > 0 && !/\s/.test(text[start - 1])) start--;
  return { start, word: text.slice(start, cursor) };
}

// Command-name completion only applies to the very first token (a command
// is only recognized at the start of input — see parseCommand). Param-name
// completion looks at that first token to know which CommandDef's params to
// offer, so "@" only suggests once a valid command name precedes it.
export function commandSuggestions(text: string, cursor: number, commands: CommandDef[] = COMMANDS): Suggestion[] {
  const { start, word } = currentToken(text, cursor);

  if (start === 0 && word.startsWith("/")) {
    const prefix = word.slice(1).toLowerCase();
    return commands.filter((c) => c.name.toLowerCase().startsWith(prefix)).map((c) => ({
      replaceStart: start,
      replaceEnd: cursor,
      insertText: `/${c.name} `,
      display: `/${c.name}`,
      description: c.description,
    }));
  }

  if (word.startsWith("@")) {
    const firstSpace = text.indexOf(" ");
    const firstToken = firstSpace === -1 ? text : text.slice(0, firstSpace);
    if (!firstToken.startsWith("/")) return [];
    const cmd = commands.find((c) => c.name === firstToken.slice(1));
    if (!cmd) return [];
    const prefix = word.slice(1).toLowerCase();
    return cmd.params.filter((p) => p.name.toLowerCase().startsWith(prefix)).map((p) => ({
      replaceStart: start,
      replaceEnd: cursor,
      insertText: `@${p.name}${p.flag ? " " : "="}`,
      display: `@${p.name}`,
      description: p.description,
    }));
  }

  return [];
}
