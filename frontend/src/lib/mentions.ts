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

// Single pass over the raw composer text: strips every `@[Name](path)` back
// to plain `@Name` for the LLM-facing prompt, and collects the unique paths
// referenced — done together so the two can never drift out of sync with
// each other (e.g. a path collected from a match that then gets cleaned
// differently).
export function resolveMentions(text: string): { cleanText: string; paths: string[] } {
  const paths = new Set<string>();
  const cleanText = text.replace(MENTION_RE, (_all, name: string, path: string) => {
    paths.add(path);
    return `@${name}`;
  });
  return { cleanText, paths: [...paths] };
}
