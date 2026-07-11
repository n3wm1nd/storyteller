"use client";

// Drives InputBar's suggestion popups (fileview.tsx): "/command @param=value"
// completion here, "@mention" completion in mention-autocomplete.tsx — both
// thin wrappers over the same cursor/accept/dismiss/arrow-key machinery,
// parameterized only by which function turns (text, cursor) into
// Suggestion[]. Recomputes from the textarea's actual caret position, not
// just the trailing text, so completion still works after the caret has
// moved back into an earlier token.
import { useEffect, useRef, useState, type RefObject } from "react";
import { useFloating, offset, flip, shift, autoUpdate } from "@floating-ui/react";
import { COMMANDS, commandSuggestions, type CommandDef, type Suggestion } from "@/lib/commands";

export function useSuggestionAutocomplete(
  text: string,
  setText: (t: string) => void,
  suggestionsFn: (text: string, cursor: number) => Suggestion[],
  // Only one textarea DOM node actually exists — when InputBar mounts more
  // than one of these hooks against it (see mention-autocomplete.tsx), the
  // second must reuse the first's ref rather than owning its own, or its
  // accept()'s post-insert focus/selection restore would silently no-op.
  sharedRef?: RefObject<HTMLTextAreaElement | null>,
) {
  const ownRef = useRef<HTMLTextAreaElement>(null);
  const taRef = sharedRef ?? ownRef;
  const [cursor, setCursor] = useState(0);
  const [activeIndex, setActiveIndex] = useState(0);
  // Escape hides the popup for the current token without altering text;
  // any further edit un-hides it, since it's now a different token.
  const [dismissed, setDismissed] = useState(false);
  useEffect(() => { setDismissed(false); }, [text]);

  const suggestions = dismissed ? [] : suggestionsFn(text, cursor);
  useEffect(() => { setActiveIndex(0); }, [suggestions.map((s) => s.display).join("|")]);

  function accept(idx: number = activeIndex): boolean {
    const s = suggestions[idx];
    if (!s) return false;
    setText(text.slice(0, s.replaceStart) + s.insertText + text.slice(s.replaceEnd));
    const pos = s.replaceStart + s.insertText.length;
    requestAnimationFrame(() => {
      taRef.current?.setSelectionRange(pos, pos);
      taRef.current?.focus();
    });
    return true;
  }

  // Returns true when the key was consumed by the popup — callers should
  // treat that as "don't also run my own handling for this key".
  function onKeyDown(e: React.KeyboardEvent<HTMLTextAreaElement>): boolean {
    if (suggestions.length === 0) return false;
    // Shift+Tab is reserved for InputBar's mode cycling — leave it alone
    // even while suggestions are open, so it's always available.
    if (e.key === "Tab" && !e.shiftKey) { e.preventDefault(); accept(); return true; }
    if (e.key === "ArrowDown") { e.preventDefault(); setActiveIndex((i) => (i + 1) % suggestions.length); return true; }
    if (e.key === "ArrowUp") { e.preventDefault(); setActiveIndex((i) => (i - 1 + suggestions.length) % suggestions.length); return true; }
    if (e.key === "Escape") { setDismissed(true); return true; }
    if (e.key === "Enter" && !e.shiftKey && !e.metaKey && !e.ctrlKey) { e.preventDefault(); accept(); return true; }
    return false;
  }

  function onSelect(e: React.SyntheticEvent<HTMLTextAreaElement>) {
    setCursor(e.currentTarget.selectionStart);
  }

  return { taRef, suggestions, activeIndex, onKeyDown, onSelect, pick: (i: number) => accept(i) };
}

export function useCommandAutocomplete(text: string, setText: (t: string) => void, commands: CommandDef[] = COMMANDS) {
  return useSuggestionAutocomplete(text, setText, (t, c) => commandSuggestions(t, c, commands));
}

export type Autocomplete = ReturnType<typeof useSuggestionAutocomplete>;

// Anchored to `reference` (typically the composer's own textarea ref —
// see InputBar) via Floating UI rather than a hand-picked `bottom: "100%"`
// offset: `flip()` moves it below the anchor instead of off-screen when
// there's no room above (e.g. the composer sitting near the top of a short
// window), `shift()` nudges it back on-screen horizontally instead of
// clipping, and `autoUpdate` keeps it correctly placed across
// scroll/resize without a manual recompute. Same visual result as the old
// fixed offset in the common case; only the edge cases behave differently.
export function CommandSuggestionPopup({ suggestions, activeIndex, onPick, reference }: {
  suggestions: Suggestion[];
  activeIndex: number;
  onPick: (index: number) => void;
  reference: RefObject<HTMLElement | null>;
}) {
  const open = suggestions.length > 0;
  const { refs, floatingStyles } = useFloating({
    open,
    placement: "top-start",
    middleware: [offset(4), flip(), shift({ padding: 8 })],
    whileElementsMounted: autoUpdate,
  });

  useEffect(() => {
    refs.setReference(reference.current);
  }, [reference, refs]);

  if (!open) return null;
  return (
    <div ref={refs.setFloating} style={{
      ...floatingStyles, zIndex: 10,
      background: "var(--card)", border: "1px solid var(--border)", borderRadius: 6,
      boxShadow: "0 4px 16px oklch(0 0 0 / 0.4)", overflow: "hidden", minWidth: 220, maxWidth: 360,
    }}>
      {suggestions.map((s, i) => (
        <div
          key={s.display}
          onMouseDown={(e) => { e.preventDefault(); onPick(i); }}
          style={{
            display: "flex", flexDirection: "column", gap: 1, padding: "6px 10px", cursor: "pointer",
            background: i === activeIndex ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
          }}
        >
          <span style={{ fontSize: 11, fontFamily: "monospace", color: "var(--amber)", fontWeight: 600 }}>{s.display}</span>
          <span style={{ fontSize: 10, color: "var(--text-dim)" }}>{s.description}</span>
        </div>
      ))}
    </div>
  );
}
