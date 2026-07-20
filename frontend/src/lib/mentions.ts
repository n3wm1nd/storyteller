// `@mention` support for the composer (fileview.tsx's InputBar) — pulls a
// specific lore entry into just one generation call, without touching the
// persisted writer:story filter. See app/mention-autocomplete.tsx for the
// popup hook this feeds, and app/fileview.actions.ts's chatWrite for where
// a mention actually becomes an extra context rule.
//
// The composer is a plain <textarea>, not a rich editor, so there's no real
// "chip" — accepting a suggestion inserts `@[DisplayName](path)`, unambiguous
// and easy to strip back out at send time. Nothing downstream of chatWrite
// ever sees this markup: resolveMentions turns it into plain `@DisplayName`
// for the actual prompt text before it's sent.

import type { Suggestion } from "./commands";
import { currentToken } from "./commands";
import type { LoreNode } from "./ws";
import { basenameNoExt } from "./utils";

// Trigger: a token starting with '@' that isn't inside a recognized
// '/command' line (that's already '@param=value' syntax — see
// commandSuggestions in commands.ts) — the caller (InputBar) is
// responsible for that split by only passing entries when mentions are
// actually enabled for the current mode/text; this function itself just
// matches whatever entries it's given against the typed prefix.
export function mentionSuggestions(text: string, cursor: number, entries: LoreNode[]): Suggestion[] {
  const { start, word } = currentToken(text, cursor);
  if (!word.startsWith("@")) return [];

  const prefix = word.slice(1).toLowerCase();
  return entries
    .filter((e) => basenameNoExt(e.path).toLowerCase().startsWith(prefix))
    .map((e) => {
      const name = basenameNoExt(e.path);
      return {
        replaceStart: start,
        replaceEnd: cursor,
        insertText: `@[${name}](${e.path}) `,
        display: `@${name}`,
        description: e.blurb,
      };
    });
}

const MENTION_RE = /@\[([^\]]+)\]\(([^)]+)\)/g;

// Strips every `@[Name](path)` back to plain `@Name` for the LLM-facing
// prompt text. A mention no longer forces anything into context (see
// app/fileview.actions.ts's writerCommandContext) — the referenced path,
// if it lives under the lore/** convention, is already part of the
// server's default context regardless of whether it's named in the
// prompt; if it isn't, mentioning it by name doesn't change that. So this
// is now purely a readability transform, not a context-selection input.
export function resolveMentions(text: string): string {
  return text.replace(MENTION_RE, (_all, name: string) => `@${name}`);
}

// Live extraction of every `@[Name](path)` mention currently in the
// composer text, classified by what it points at. Drives the per-call
// context overlay (callContextStore's `mentions[path]` → composed into
// the wire's `context` program at send). Only mentions of a character
// branch (`path === "character/<id>"`, the canonical shape the
// autocomplete inserts and Server.Writer.Session's characterBranches
// reports) become an inclusion -- a mention of a lore file or arbitrary
// path is informational only, since lore/** is already auto-included
// and arbitrary paths aren't a context primitive the casual UI exposes.
//
// Returns the character ids (no `character/` prefix), deduped, in
// document order. The caller (useMentionAutocomplete in
// mention-autocomplete.tsx, called from InputBar) wires this to
// callContextStore.setMentions on every keystroke.
export function extractCharacterMentions(text: string): string[] {
  const ids: string[] = [];
  const seen = new Set<string>();
  let m: RegExpExecArray | null;
  const re = new RegExp(MENTION_RE.source, "g");
  while ((m = re.exec(text)) !== null) {
    const path: string = m[2];
    const match = path.match(/^character\/(.+)$/);
    if (!match) continue;
    const id = match[1];
    if (seen.has(id)) continue;
    seen.add(id);
    ids.push(id);
  }
  return ids;
}
