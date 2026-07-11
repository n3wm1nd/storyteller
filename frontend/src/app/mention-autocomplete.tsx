"use client";

// Drives InputBar's '@mention' popup — a thin wrapper over the same
// cursor/accept/dismiss/arrow-key machinery command-autocomplete.tsx's
// '/command' completion uses, parameterized by mentionSuggestions
// (lib/mentions.ts) instead of commandSuggestions. Pass `entries: []` to
// disable (InputBar does this whenever mentions aren't valid for the
// current mode/text — see its own comment on `mentionsEnabled`).

import type { RefObject } from "react";
import { useSuggestionAutocomplete } from "./command-autocomplete";
import { mentionSuggestions } from "@/lib/mentions";
import type { LoreNode } from "@/lib/ws";

export function useMentionAutocomplete(
  text: string,
  setText: (t: string) => void,
  entries: LoreNode[],
  sharedRef?: RefObject<HTMLTextAreaElement | null>,
) {
  return useSuggestionAutocomplete(text, setText, (t, c) => mentionSuggestions(t, c, entries), sharedRef);
}
