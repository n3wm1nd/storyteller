"use client";

import { memo, useEffect, useLayoutEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import { ChevronDown, ChevronUp, History, Sparkles, Wrench, RefreshCw, EyeOff, Square, Users } from "lucide-react";
import { StickyNote, HelpCircle, Image as ImageIcon } from "lucide-react";
import { cancelGeneration, referenceImage } from "./fileview.actions";
import { useEditor, EditorContent, type Editor } from "@tiptap/react";
import { StarterKit } from "@tiptap/starter-kit";
import { Markdown } from "tiptap-markdown";
import { useFloating, offset, flip, shift, autoUpdate } from "@floating-ui/react";
import { type WireTick, useServerCache } from "@/lib/serverCacheStore";
import { type AnnotationMode, characterDisplayName, characterColor, splitQuestionAnswer, tailLeadTicks } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";
import { parseCommand, COMMANDS } from "@/lib/commands";
import { useCommandAutocomplete, CommandSuggestionPopup } from "./command-autocomplete";
import { useMentionAutocomplete } from "./mention-autocomplete";
import { useLoreTree, flattenLore } from "./lore-selector";
import { branchFileUrl, saveRawFile, saveRawFileAsNew } from "@/lib/ws";
import { IMAGE_DRAG_MIME, summaryKindLabel } from "@/lib/library";

// tiptap-markdown's Markdown extension adds `storage.markdown` at runtime
// (see TextEditPanel below) but ships no type augmentation for it, so
// @tiptap/core's own Storage interface has no idea it exists — declared
// once here rather than casting `editor.storage as any` at each call site.
// `parser`/`serializer` (undocumented, but present on every tiptap-markdown
// build — see node_modules/tiptap-markdown/dist/tiptap-markdown.es.js) are
// what 'snapshotOriginal'/'computeMinimalSave' below hook into: `parser.md`
// is the raw markdown-it instance (for source-line block boundaries) and
// `serializer` can render an arbitrary node/fragment, not just the whole
// document.
declare module "@tiptap/core" {
  interface Storage {
    markdown: {
      getMarkdown: () => string;
      parser: { md: { parse: (text: string, env: object) => MarkdownItToken[] } };
      serializer: { serialize: (content: ProsemirrorNode | ProsemirrorFragment) => string };
    };
  }
}

interface MarkdownItToken {
  level: number;
  map: [number, number] | null;
}

// Minimal structural subset of prosemirror-model's Node/Fragment used below
// — avoids adding a direct prosemirror-model dependency just for typing.
interface ProsemirrorNode {
  eq(other: ProsemirrorNode): boolean;
}
interface ProsemirrorFragment {
  content: ProsemirrorNode[];
}

// A character's presence, as a set of this file's own atom tickIds — not
// fromTickId/toTickId, since a character can enter/leave more than once
// within one file, producing more than one bar. The file view doesn't care
// why this set was populated (hover vs. a future pinned/always-on toggle,
// see lib/utils.presentDuringAtoms) — it just draws one line per contiguous
// run of membership, one lane per entry in the array.
export interface PresenceBar {
  character: string;
  color: string;
  tickIds: Set<string>;
}

// ── Markdown renderer ─────────────────────────────────────────────────────────

const mdComponents: React.ComponentProps<typeof ReactMarkdown>["components"] = {
  p: ({ children }) => (
    <p style={{ margin: "0 0 0.9em", fontSize: 13, lineHeight: 1.8, fontFamily: "Georgia, serif", color: "var(--text-body)" }}>
      {children}
    </p>
  ),
  h1: ({ children }) => <h1 style={{ margin: "0 0 0.6em", fontSize: 20, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h1>,
  h2: ({ children }) => <h2 style={{ margin: "0 0 0.5em", fontSize: 16, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h2>,
  h3: ({ children }) => <h3 style={{ margin: "0 0 0.4em", fontSize: 14, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h3>,
  blockquote: ({ children }) => (
    <blockquote style={{ margin: "0 0 0.9em", paddingLeft: 12, borderLeft: "3px solid var(--border)", color: "var(--text-muted)", fontStyle: "italic" }}>
      {children}
    </blockquote>
  ),
  code: ({ children }) => (
    <code style={{ fontFamily: "monospace", fontSize: 11, background: "oklch(0.18 0.01 60)", padding: "1px 4px", borderRadius: 3, color: "var(--text-label)" }}>
      {children}
    </code>
  ),
  pre: ({ children }) => (
    <pre style={{ margin: "0 0 0.9em", padding: "8px 10px", background: "oklch(0.18 0.01 60)", borderRadius: 5, overflowX: "auto", fontSize: 11, lineHeight: 1.6 }}>
      {children}
    </pre>
  ),
};

// Tighter variant for narrow, dense contexts (the journal sidebar panel) —
// same tags, much less whitespace and a smaller size tuned for a ~200px
// column rather than a full reading pane.
const compactMdComponents: React.ComponentProps<typeof ReactMarkdown>["components"] = {
  p: ({ children }) => <p style={{ margin: "0 0 0.35em", fontSize: 11, lineHeight: 1.45, color: "var(--text-body)" }}>{children}</p>,
  h1: ({ children }) => <h1 style={{ margin: "0 0 0.25em", fontSize: 13, color: "var(--text-heading)", fontWeight: 600 }}>{children}</h1>,
  h2: ({ children }) => <h2 style={{ margin: "0 0 0.25em", fontSize: 12, color: "var(--text-heading)", fontWeight: 600 }}>{children}</h2>,
  h3: ({ children }) => <h3 style={{ margin: "0 0 0.25em", fontSize: 11, color: "var(--text-heading)", fontWeight: 600 }}>{children}</h3>,
  blockquote: ({ children }) => (
    <blockquote style={{ margin: "0 0 0.35em", paddingLeft: 8, borderLeft: "2px solid var(--border)", color: "var(--text-muted)", fontStyle: "italic" }}>
      {children}
    </blockquote>
  ),
  code: ({ children }) => (
    <code style={{ fontFamily: "monospace", fontSize: 10, background: "oklch(0.18 0.01 60)", padding: "0 3px", borderRadius: 2, color: "var(--text-label)" }}>
      {children}
    </code>
  ),
  pre: ({ children }) => (
    <pre style={{ margin: "0 0 0.35em", padding: "5px 6px", background: "oklch(0.18 0.01 60)", borderRadius: 4, overflowX: "auto", fontSize: 10, lineHeight: 1.4 }}>
      {children}
    </pre>
  ),
};

// ── Atom block ────────────────────────────────────────────────────────────────

const AtomBlock = memo(function AtomBlock({ atom, isLast, inContext, swipeCount, onEdit, onToggleContext, onCycleSwipe, onHoverAtom, onHoverEnd, compact, onCorrect }: {
  atom: WireTick;
  isLast: boolean;
  inContext: boolean;
  // How many alternates (see Storyteller.Common.Swipe) sit in this atom's
  // own carousel — 0 hides the cycle control entirely. Cycling only
  // rotates what's already stored; generating a genuinely fresh
  // alternative is "Correct this" below.
  swipeCount: number;
  onEdit: (tickId: string, content: string) => void;
  onToggleContext: (tickId: string) => void;
  onCycleSwipe: (tickId: string) => void;
  // Optional cross-component "glow" hook (see store.ts's 'hoverHighlight') —
  // unused by the main file view, wired up by the journal panel so hovering
  // a tracked entry highlights the scene atom it came from (via 'atom.refs').
  onHoverAtom?: (tickIds: Set<string>) => void;
  onHoverEnd?: () => void;
  // Dense rendering for narrow, secondary views (the journal sidebar panel)
  // — smaller type/margins, no dedicated selection-bar gutter (ctrl-click
  // directly on the text still toggles context, same as the main view — the
  // always-visible bar is the part that isn't worth the width here), and no
  // tickId debug label. Everything else (edit, select, hover) is unchanged.
  compact?: boolean;
  // "Correct this" (see fileview.actions.ts's correctAtom) — one plain
  // click, no menu: regenerates the whole instruction-group this atom
  // belongs to, via the same agent/context that produced it, reading
  // whatever the group's Prompt tick currently says (independently
  // editable via PromptHeader above the group — edit that first if the
  // instruction itself was wrong, then click this). Omitted
  // (journal/compact views) hides the trigger entirely, same convention as
  // onHoverAtom. Fixing a character's journal first is likewise a separate
  // action (character-sidebar.tsx's Context/Journal panel) — regenerating
  // afterward is this same button, once the journal reflects the fix.
  onCorrect?: (tickId: string) => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  const content = atom.content ?? "";
  const hidden  = atom.fields?.hide === "true";

  function startEdit() {
    setDraft(content);
    setEditing(true);
    setTimeout(() => textareaRef.current?.focus(), 0);
  }

  function commitEdit() {
    const trimmed = draft.trim();
    if (trimmed && trimmed !== content.trim()) onEdit(atom.tickId, trimmed);
    setEditing(false);
  }

  const barColor = inContext
    ? "var(--amber)"
    : hovered ? "oklch(0.40 0.03 60)" : "oklch(0.20 0.01 60)";

  return (
    <div
      onMouseEnter={() => { setHovered(true); onHoverAtom?.(new Set(atom.refs)); }}
      onMouseLeave={() => { setHovered(false); onHoverEnd?.(); }}
      style={{
        position: "relative",
        paddingLeft: compact ? 0 : 10, marginLeft: compact ? 0 : -12,
        background: inContext ? "var(--amber-wash)" : "transparent",
        borderRadius: inContext ? 4 : 0,
        marginBottom: isLast ? 0 : editing ? (compact ? 6 : 16) : (compact ? 2 : undefined),
        opacity: hidden ? 0.45 : 1,
        transition: "background 0.15s, opacity 0.15s",
      }}
    >
      {!compact && (
        <div
          onClick={(e) => { e.stopPropagation(); onToggleContext(atom.tickId); }}
          title={inContext ? "Remove from context" : "Add to context"}
          style={{
            position: "absolute", left: 0, top: 0, bottom: 0,
            width: 10, cursor: "pointer", display: "flex", alignItems: "stretch",
          }}
        >
          <div style={{ width: 2, height: "100%", background: barColor, transition: "background 0.15s", borderRadius: 1 }} />
        </div>
      )}
      {hidden && (
        <span title="Hidden from an agent's context" style={{ position: "absolute", top: 2, left: compact ? -2 : 12 }}>
          <EyeOff style={{ width: 11, height: 11, color: "var(--text-ghost)" }} />
        </span>
      )}

      {editing ? (
        <div style={{ padding: compact ? "3px 0 6px" : "10px 0 14px" }}>
          <textarea
            ref={textareaRef}
            value={draft}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); commitEdit(); }
              if (e.key === "Escape") setEditing(false);
            }}
            style={{
              width: "100%", boxSizing: "border-box",
              minHeight: compact ? 44 : 80, resize: "vertical",
              background: "var(--surface-deep)",
              border: "1px solid var(--amber-border)",
              borderRadius: 4, padding: compact ? "4px 6px" : "6px 8px",
              color: "var(--text-primary)", fontSize: compact ? 11 : 14, lineHeight: compact ? 1.4 : 1.6,
              fontFamily: "inherit", outline: "none",
            }}
          />
          <div style={{ display: "flex", gap: 6, justifyContent: "flex-end", marginTop: 4 }}>
            <button onClick={() => setEditing(false)} style={{
              background: "none", border: "1px solid var(--border-subtle)", borderRadius: 3,
              color: "var(--text-ghost)", fontSize: 11, padding: "2px 8px", cursor: "pointer",
            }}>Cancel</button>
            <button onClick={commitEdit} style={{
              background: "var(--amber-tint)", border: "1px solid var(--amber-border)",
              borderRadius: 3, color: "var(--text-secondary)", fontSize: 11, padding: "2px 8px", cursor: "pointer",
            }}>Save</button>
          </div>
        </div>
      ) : (
        <div
          onDoubleClick={startEdit}
          onClick={(e) => { if (e.ctrlKey || e.metaKey) { e.preventDefault(); onToggleContext(atom.tickId); } }}
          style={compact ? { cursor: "default", outline: inContext ? "1px solid var(--amber-border)" : "none", outlineOffset: 2, borderRadius: 3 } : undefined}
        >
          <ReactMarkdown components={compact ? compactMdComponents : mdComponents}>{content}</ReactMarkdown>
        </div>
      )}

      {!compact && swipeCount > 0 && !editing && (
        <button
          onClick={(e) => { e.stopPropagation(); onCycleSwipe(atom.tickId); }}
          title="Cycle to the next alternate"
          style={{
            position: "absolute", bottom: 0, right: 60,
            display: "flex", alignItems: "center", gap: 3,
            fontSize: 9, padding: "1px 5px", borderRadius: 3, cursor: "pointer",
            background: "transparent",
            border: hovered ? "1px solid var(--border-subtle)" : "1px solid transparent",
            color: hovered ? "var(--text-dim)" : "transparent",
            transition: "color 0.15s, border-color 0.15s",
          }}
        >
          <RefreshCw style={{ width: 9, height: 9 }} />
          {swipeCount}
        </button>
      )}
      {!compact && onCorrect && !editing && (
        <button
          onClick={(e) => { e.stopPropagation(); onCorrect(atom.tickId); }}
          title="Correct this — regenerate with the same instruction and context (edit the prompt above first if that was the problem)"
          style={{
            position: "absolute", bottom: 0, right: swipeCount > 0 ? 95 : 60,
            display: "flex", alignItems: "center", padding: "1px 5px", borderRadius: 3, cursor: "pointer",
            background: "transparent",
            border: hovered ? "1px solid var(--border-subtle)" : "1px solid transparent",
            color: hovered ? "var(--text-dim)" : "transparent",
            transition: "color 0.15s, border-color 0.15s",
          }}
        >
          <RefreshCw style={{ width: 9, height: 9 }} />
        </button>
      )}
      {!compact && (
        <span style={{
          position: "absolute", bottom: 0, right: 0,
          fontSize: 9, fontFamily: "monospace", lineHeight: 1,
          color: hovered ? "var(--text-ghost)" : "transparent",
          userSelect: "all", transition: "color 0.15s", pointerEvents: "none",
        }}>
          {atom.tickId.slice(0, 12)}
        </span>
      )}
    </div>
  );
});

// ── Annotation card ───────────────────────────────────────────────────────────

const AnnotationCard = memo(function AnnotationCard({ tick, inContext, onToggleContext, activeBranch, onOpenSummary }: {
  tick: WireTick;
  inContext: boolean;
  onToggleContext: (tickId: string) => void;
  // Only an "image" tick needs this, to build its thumbnail's GET URL.
  activeBranch?: string | null;
  onOpenSummary?: (kind: string, tickId: string) => void;
}) {
  const [expanded, setExpanded] = useState(false);

  const isNote    = tick.kind === "note";
  const isAsk     = tick.kind === "character-answer";
  const isImage   = tick.kind === "image";
  const isSummary = tick.kind === "summary";
  if (!isNote && !isAsk && !isImage && !isSummary) return null;

  if (isSummary) {
    const kind = tick.fields?.kind ?? "";
    const text = tick.content ?? tick.message;
    const expandable = text.length > 140;
    const preview = expandable ? text.slice(0, 140) + "…" : text;
    return (
      <div
        onClick={(e) => {
          if (e.ctrlKey || e.metaKey) { onToggleContext(tick.tickId); return; }
          onOpenSummary?.(kind, tick.tickId);
        }}
        style={{
          margin: "4px 0 10px 12px", borderRadius: 5, padding: "5px 10px",
          background: "oklch(0.24 0.05 80 / 0.4)", border: "1px solid oklch(0.6 0.15 80 / 0.35)",
          outline: inContext ? "2px solid var(--amber)" : "none", outlineOffset: 1,
          cursor: "pointer",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <History style={{ width: 11, height: 11, color: "oklch(0.7 0.15 80)", flexShrink: 0 }} />
          <span style={{ fontSize: 12, color: "oklch(0.75 0.13 80)", fontStyle: "italic", flex: 1 }}>
            {summaryKindLabel(kind)}
          </span>
          {text && (
            <ChevronDown
              onClick={(e) => { e.stopPropagation(); setExpanded((v) => !v); }}
              style={{
                width: 10, height: 10, color: "var(--text-ghost)", flexShrink: 0,
                transform: expanded ? "rotate(180deg)" : "none", transition: "transform 0.15s",
              }}
            />
          )}
        </div>
        {text && (
          <div style={{ fontSize: 12, color: "var(--text-secondary)", lineHeight: 1.5, marginTop: 3 }}>
            {expanded ? text : preview}
          </div>
        )}
      </div>
    );
  }

  if (isImage) {
    const asset   = tick.fields?.asset;
    const caption = tick.message;
    return (
      <div
        onClick={(e) => { if (e.ctrlKey || e.metaKey) onToggleContext(tick.tickId); }}
        style={{
          margin: "4px 0 10px 12px", borderRadius: 5, padding: "6px 10px",
          background: "oklch(0.22 0.03 300 / 0.4)", border: "1px solid oklch(0.5 0.15 300 / 0.35)",
          outline: inContext ? "2px solid var(--amber)" : "none", outlineOffset: 1,
          display: "flex", alignItems: "center", gap: 8,
        }}
      >
        <ImageIcon style={{ width: 11, height: 11, color: "oklch(0.65 0.15 300)", flexShrink: 0 }} />
        {asset && activeBranch && (
          <img
            src={branchFileUrl(activeBranch, asset)}
            alt={caption || asset}
            style={{ maxWidth: 220, maxHeight: 160, borderRadius: 4, flexShrink: 0, display: "block" }}
          />
        )}
        {caption && (
          <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic" }}>{caption}</span>
        )}
      </div>
    );
  }

  // An ask has its own two-part shape (who was asked + the question, then
  // the answer). Which character answered is a field (same wire convention
  // Presence uses for its own "character" field), but the question and
  // answer themselves are both joined into 'message' — see
  // lib/utils.splitQuestionAnswer for why neither can be a plain tick
  // field (both are free-form, possibly multi-line text).
  if (isAsk) {
    const character = tick.fields?.character ?? "";
    const [question, answer] = splitQuestionAnswer(tick.message);
    const name       = characterDisplayName(character);
    const accentColor = characterColor(character);
    const expandable  = answer.length > 80;
    const preview     = expandable ? answer.slice(0, 80) + "…" : answer;

    return (
      <div
        onClick={(e) => {
          if (e.ctrlKey || e.metaKey) { onToggleContext(tick.tickId); return; }
          if (expandable) setExpanded((v) => !v);
        }}
        style={{
          margin: "4px 0 10px 12px", borderRadius: 5, padding: "5px 10px",
          background: "oklch(0.22 0.02 240 / 0.4)", border: `1px solid ${accentColor.replace(")", " / 0.35)")}`,
          outline: inContext ? `2px solid var(--amber)` : "none",
          outlineOffset: 1, cursor: expandable ? "pointer" : "default",
          transition: "outline 0.12s",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: 7 }}>
          <HelpCircle style={{ width: 11, height: 11, color: accentColor, flexShrink: 0 }} />
          <span style={{ fontSize: 11, color: accentColor, fontWeight: 600, flex: 1 }}>
            Asked {name}: <span style={{ fontWeight: 400, fontStyle: "italic", color: "var(--text-muted)" }}>{question}</span>
          </span>
          {expandable && (
            <ChevronDown style={{
              width: 10, height: 10, color: "var(--text-ghost)", flexShrink: 0,
              transform: expanded ? "rotate(180deg)" : "none", transition: "transform 0.15s",
            }} />
          )}
        </div>
        <div style={{ fontSize: 12, color: "var(--text-secondary)", lineHeight: 1.5, marginTop: 3 }}>
          {expanded ? answer : preview}
        </div>
      </div>
    );
  }

  // isAsk already returned above, so this is always a note by now.
  const accentColor = "oklch(0.55 0.15 240)";
  const bgColor     = "oklch(0.22 0.01 240 / 0.6)";
  const borderColor = "oklch(0.35 0.04 240 / 0.4)";
  const Icon        = StickyNote;
  const expandable  = tick.message.length > 60;
  const preview     = expandable ? tick.message.slice(0, 60) + "…" : tick.message;

  return (
    <div
      onClick={(e) => {
        if (e.ctrlKey || e.metaKey) { onToggleContext(tick.tickId); return; }
        if (expandable) setExpanded((v) => !v);
      }}
      style={{
        margin: "4px 0 10px 12px", borderRadius: 5,
        background: bgColor, border: `1px solid ${borderColor}`,
        outline: inContext ? `2px solid var(--amber)` : "none",
        outlineOffset: 1, cursor: expandable ? "pointer" : "default",
        transition: "outline 0.12s",
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 7, padding: "5px 10px" }}>
        <Icon style={{ width: 11, height: 11, color: accentColor, flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic", lineHeight: 1.5, flex: 1, opacity: expanded ? 1 : 0.85 }}>
          {expanded ? tick.message : preview}
        </span>
        {expandable && (
          <ChevronDown style={{
            width: 10, height: 10, color: "var(--text-ghost)", flexShrink: 0,
            transform: expanded ? "rotate(180deg)" : "none", transition: "transform 0.15s",
          }} />
        )}
      </div>
    </div>
  );
});

// ── Prompt header ─────────────────────────────────────────────────────────────
//
// The instruction that generated the atom group it leads (see
// WireTickList's promptBefore map) — rendered *above* that group, chat-
// style, rather than trailing the previous one the way note/ask
// annotations do; a prompt reads as "here's what was asked for next", not
// as commentary on what came before. Editable in place, same
// textarea-then-Save pattern AtomBlock's own atom editing uses — saves via
// the plain edit.prompt command (no regeneration side-effect; regenerating
// is a separate, explicit action on the atoms themselves).

const PromptHeader = memo(function PromptHeader({ tick, compact, onEditPrompt }: {
  tick: WireTick;
  // "dots" mode: one quiet line, chat-style. "expanded" mode: full card,
  // matching AnnotationCard's own visual weight for consistency.
  compact: boolean;
  onEditPrompt: (tickId: string, content: string) => void;
}) {
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState("");
  const [expanded, setExpanded] = useState(false);

  function startEdit() {
    setDraft(tick.message);
    setEditing(true);
  }
  function commitEdit() {
    const trimmed = draft.trim();
    if (trimmed && trimmed !== tick.message.trim()) onEditPrompt(tick.tickId, trimmed);
    setEditing(false);
  }

  if (editing) {
    return (
      <div style={{ margin: "4px 0 8px 12px" }}>
        <textarea
          autoFocus
          value={draft}
          onChange={(e) => setDraft(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); commitEdit(); }
            if (e.key === "Escape") setEditing(false);
          }}
          rows={compact ? 2 : 4}
          style={{
            width: "100%", boxSizing: "border-box", resize: "vertical", fontSize: compact ? 11 : 12, fontFamily: "inherit",
            background: "var(--surface-deep)", border: "1px solid var(--amber-border)", borderRadius: 4,
            padding: "4px 6px", color: "var(--text-primary)", outline: "none",
          }}
        />
        <div style={{ display: "flex", gap: 6, justifyContent: "flex-end", marginTop: 4 }}>
          <button onClick={() => setEditing(false)} style={{ background: "none", border: "1px solid var(--border-subtle)", borderRadius: 3, color: "var(--text-ghost)", fontSize: 10.5, padding: "2px 8px", cursor: "pointer" }}>Cancel</button>
          <button onClick={commitEdit} style={{ background: "var(--amber-tint)", border: "1px solid var(--amber-border)", borderRadius: 3, color: "var(--text-secondary)", fontSize: 10.5, padding: "2px 8px", cursor: "pointer" }}>Save</button>
        </div>
      </div>
    );
  }

  if (compact) {
    const preview = tick.message.length > 140 ? tick.message.slice(0, 140) + "…" : tick.message;
    return (
      <div
        onDoubleClick={startEdit}
        title="Double-click to edit — the instruction that generated what follows"
        style={{ display: "flex", alignItems: "center", gap: 6, margin: "6px 0 4px 12px", cursor: "text" }}
      >
        <Sparkles style={{ width: 10, height: 10, color: "var(--amber)", flexShrink: 0, opacity: 0.7 }} />
        <span style={{
          fontSize: 11, color: "var(--amber)", fontStyle: "italic", opacity: 0.75,
          overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        }}>
          {preview}
        </span>
      </div>
    );
  }

  const expandable = tick.message.length > 80;
  const preview = expandable ? tick.message.slice(0, 80) + "…" : tick.message;
  return (
    <div
      onDoubleClick={startEdit}
      style={{
        margin: "4px 0 10px 12px", borderRadius: 5, padding: "5px 10px",
        background: "var(--amber-wash)", border: "1px solid var(--amber-border)",
        cursor: expandable ? "pointer" : "text",
      }}
    >
      <div onClick={() => expandable && setExpanded((v) => !v)} style={{ display: "flex", alignItems: "center", gap: 7 }}>
        <Sparkles style={{ width: 11, height: 11, color: "var(--amber)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--amber)", fontStyle: "italic", lineHeight: 1.5, flex: 1, opacity: expanded ? 1 : 0.85 }}>
          {expanded ? tick.message : preview}
        </span>
        {expandable && (
          <ChevronDown style={{
            width: 10, height: 10, color: "var(--text-ghost)", flexShrink: 0,
            transform: expanded ? "rotate(180deg)" : "none", transition: "transform 0.15s",
          }} />
        )}
      </div>
    </div>
  );
});

// ── Annotation dots ───────────────────────────────────────────────────────────

const AnnotationDots = memo(function AnnotationDots({ annotations, contextAnnotations, onToggleContext, activeBranch, onOpenSummary }: {
  annotations: WireTick[];
  contextAnnotations: Set<string>;
  onToggleContext: (tickId: string) => void;
  activeBranch?: string | null;
  onOpenSummary?: (kind: string, tickId: string) => void;
}) {
  const [expandedId, setExpandedId] = useState<string | null>(null);

  // "prompt"-kind ticks never reach here — they're excluded from the
  // trailing-annotation grouping entirely and rendered by PromptHeader
  // instead, leading the atom group they generated (see WireTickList's
  // promptBefore map).
  function dotColor(ann: WireTick): string {
    if (ann.kind === "note")             return "oklch(0.55 0.15 240)";
    if (ann.kind === "character-answer") return characterColor(ann.fields?.character ?? "");
    if (ann.kind === "image")            return "oklch(0.65 0.15 300)";
    if (ann.kind === "summary")           return "oklch(0.7 0.15 80)";
    return "var(--text-dim)";
  }

  return (
    <div style={{ margin: "2px 0 10px 12px" }}>
      <div style={{ display: "flex", gap: 5, alignItems: "center", flexWrap: "wrap" }}>
        {annotations.map((ann) => {
          const inCtx  = contextAnnotations.has(ann.tickId);
          const isOpen = expandedId === ann.tickId;
          const color  = dotColor(ann);
          const isSummary = ann.kind === "summary";
          const title  = (() => {
            if (isSummary) {
              const text = ann.content ?? ann.message;
              return `${summaryKindLabel(ann.fields?.kind ?? "")}${text ? `\n${text.slice(0, 80)}` : ""}`;
            }
            if (ann.kind !== "character-answer") return ann.message.slice(0, 80);
            const [question, answer] = splitQuestionAnswer(ann.message);
            return `Asked ${characterDisplayName(ann.fields?.character ?? "")}: ${question}\n${answer.slice(0, 80)}`;
          })();
          return (
            <button
              key={ann.tickId}
              title={title}
              onClick={(e) => {
                if (e.ctrlKey || e.metaKey) { onToggleContext(ann.tickId); return; }
                // A summary dot always navigates straight to the split view
                // (see .summarization-ui.md) rather than expanding an inline
                // peek card the way every other annotation kind does.
                if (isSummary) { onOpenSummary?.(ann.fields?.kind ?? "", ann.tickId); return; }
                setExpandedId((id) => id === ann.tickId ? null : ann.tickId);
              }}
              style={{
                width: 8, height: 8, borderRadius: "50%", border: "none",
                background: color, cursor: "pointer", padding: 0, flexShrink: 0,
                outline: inCtx ? "2px solid var(--amber)" : isOpen ? `1px solid ${color}` : "none",
                outlineOffset: 2, opacity: isOpen ? 1 : 0.6,
                boxShadow: isOpen ? `0 0 3px 0px ${color}` : "none",
                transform: isOpen ? "scale(1.25)" : "scale(1)",
                transition: "transform 0.1s, outline 0.12s, box-shadow 0.12s, opacity 0.12s",
              }}
              onMouseEnter={(e) => { if (!isOpen) (e.currentTarget as HTMLElement).style.transform = "scale(1.35)"; }}
              onMouseLeave={(e) => { if (!isOpen) (e.currentTarget as HTMLElement).style.transform = "scale(1)"; }}
            />
          );
        })}
      </div>
      {expandedId && (() => {
        const ann = annotations.find((a) => a.tickId === expandedId);
        if (!ann) return null;
        return (
          <AnnotationCard
            tick={ann}
            inContext={contextAnnotations.has(ann.tickId)}
            onToggleContext={onToggleContext}
            activeBranch={activeBranch}
          />
        );
      })()}
    </div>
  );
});

// ── Rebase handle ─────────────────────────────────────────────────────────────
//
// A CAD feature-tree-style marker: dropping it after an atom means every
// subsequent command runs as if that atom were HEAD, then the (grayed-out)
// atoms below get rebased on top of whatever happens next.
//
// Rather than a permanent divider between every atom, the marker lives as one
// small draggable handle anchored at the bottom of the atom list (never
// overlapping the log strip / input bar below it, which are separate
// siblings). Grabbing it shows a preview line immediately, tracking the
// nearest atom boundary to the cursor anywhere on the page (not just while
// hovering the thin gap between atoms) — dropping commits it. The active
// marker (and the drag preview) render as a full line with a centered
// clock+label pill; clicking the pill clears the marker.

const RebaseHandle = memo(function RebaseHandle({ active, dragging, willClear, onDragStart }: {
  active: boolean;
  dragging: boolean;
  willClear: boolean;
  onDragStart: (e: React.MouseEvent) => void;
}) {
  return (
    <div
      onMouseDown={onDragStart}
      title={active ? "Drag to move the rebase marker" : "Drag onto an atom to rebase from there"}
      style={{
        position: "absolute", right: 14, bottom: 10, zIndex: 5,
        width: 22, height: 22, borderRadius: "50%",
        display: "flex", alignItems: "center", justifyContent: "center",
        background: willClear ? "var(--rose)" : active ? "var(--amber)" : "var(--surface-deep)",
        border: `1px solid ${willClear ? "var(--rose)" : active ? "var(--amber)" : "var(--border)"}`,
        color: willClear || active ? "oklch(0.15 0.01 60)" : "var(--text-ghost)",
        cursor: dragging ? "grabbing" : "grab",
        opacity: dragging ? 1 : active ? 0.85 : 0.45,
        boxShadow: dragging ? "0 2px 8px oklch(0 0 0 / 0.35)" : "none",
        transition: "opacity 0.15s, background 0.15s, box-shadow 0.15s",
      }}
    >
      <History style={{ width: 12, height: 12 }} />
    </div>
  );
});

const RebaseDropZone = memo(function RebaseDropZone({ isMarker, isCandidate, onDragStart }: {
  isMarker: boolean;
  isCandidate: boolean;
  onDragStart: (e: React.MouseEvent) => void;
}) {
  const lit = isMarker || isCandidate;
  return (
    <div
      onMouseDown={isMarker ? onDragStart : undefined}
      title={isMarker ? "Rebasing here — click to clear, drag to move" : undefined}
      style={{
        display: "flex", alignItems: "center", gap: 6, margin: "3px 0", padding: "2px 0",
        cursor: isMarker ? "grab" : "default", userSelect: "none",
      }}
    >
      <div style={{
        flex: 1, height: lit ? 2 : 1, borderRadius: 1,
        background: isMarker ? "var(--amber)" : isCandidate ? "var(--text-ghost)" : "transparent",
        transition: "background 0.1s, height 0.1s",
      }} />
      {lit && (
        <span style={{
          display: "flex", alignItems: "center", gap: 4, flexShrink: 0,
          fontSize: 9, color: isMarker ? "var(--amber)" : "var(--text-ghost)",
        }}>
          <History style={{ width: 10, height: 10 }} />
          {isMarker ? "rebasing here — drag to move, click to clear" : "rebase here"}
        </span>
      )}
      <div style={{
        flex: 1, height: lit ? 2 : 1, borderRadius: 1,
        background: isMarker ? "var(--amber)" : isCandidate ? "var(--text-ghost)" : "transparent",
        transition: "background 0.1s, height 0.1s",
      }} />
    </div>
  );
});

// ── Wire tick list ────────────────────────────────────────────────────────────

export function WireTickList({
  ticks, annotationMode, contextAtoms, contextAnnotations, resetKey,
  rebaseMarker, onSetRebaseMarker, presenceBars,
  onEdit, onToggleContextAtom, onToggleContextAnnotation, onCycleSwipe,
  onHoverAtom, onHoverEnd, compact, onCorrect, onEditPrompt,
  activeBranch, targetFile, onUploadImages, onOpenSummary,
}: {
  ticks: WireTick[];
  annotationMode: AnnotationMode;
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  resetKey: unknown;
  rebaseMarker: string | null;
  onSetRebaseMarker: (tickId: string | null) => void;
  presenceBars: PresenceBar[];
  onEdit: (tickId: string, content: string) => void;
  onHoverAtom?: (tickIds: Set<string>) => void;
  onHoverEnd?: () => void;
  onToggleContextAtom: (tickId: string) => void;
  onToggleContextAnnotation: (tickId: string) => void;
  onCycleSwipe: (tickId: string) => void;
  // Dense mode for embedding in a narrow, naturally-sized container (the
  // journal panel) instead of a full-page fill — see AtomBlock's own
  // 'compact' doc. Also drops the flex/overflow fill assumptions so the
  // element sizes to its content; the embedding container is expected to
  // supply its own maxHeight + overflow for the (rare) long-journal case.
  compact?: boolean;
  // "Correct this" (AtomBlock) / prompt editing (PromptHeader) — see their
  // own docs. Omitted entirely by the journal panel's compact usage
  // (compact mode hides both triggers regardless, but not passing these at
  // all keeps that call site honest about not supporting them).
  onCorrect?: (tickId: string) => void;
  onEditPrompt?: (tickId: string, content: string) => void;
  // Needed for image annotations' thumbnail URLs (activeBranch), and to
  // support dropping an image onto the prose (targetFile/onUploadImages) —
  // omitted entirely by the journal panel's compact usage, same as
  // onCorrect/onEditPrompt, which disables the drop zone there too.
  activeBranch?: string | null;
  targetFile?: string | null;
  onUploadImages?: (path: string, files: FileList | File[]) => void;
  // Clicking an inline "summary"-kind annotation always navigates to that
  // occurrence's split view (see .summarization-ui.md) rather than
  // expanding in place — omitted entirely by call sites with no summary
  // concept of their own (e.g. the journal panel).
  onOpenSummary?: (kind: string, tickId: string) => void;
}) {
  const contentKey = ticks.length > 0 ? `${ticks.length}:${ticks[ticks.length - 1].tickId}` : 0;
  const scrollRef = useAutoScroll<HTMLDivElement>(contentKey, resetKey, "end");

  const [imageDragOver, setImageDragOver] = useState(false);
  const canDropImage = !!(targetFile && onUploadImages);
  const handleImageDragOver = (e: React.DragEvent) => {
    if (!canDropImage) return;
    e.preventDefault();
    setImageDragOver(true);
  };
  const handleImageDragLeave = () => setImageDragOver(false);
  const handleImageDrop = (e: React.DragEvent) => {
    if (!canDropImage) return;
    e.preventDefault();
    setImageDragOver(false);
    // An existing branch image dragged out of the file tree (filetree.tsx)
    // carries its path via IMAGE_DRAG_MIME instead of real File objects —
    // it's already stored, so this just points a new Image tick at that
    // same asset (referenceImage) rather than re-uploading a duplicate copy
    // of its bytes the way an OS file drop (onUploadImages, below) has to.
    const draggedPath = e.dataTransfer.getData(IMAGE_DRAG_MIME);
    if (draggedPath) {
      referenceImage(targetFile!, draggedPath);
      return;
    }
    const images = Array.from(e.dataTransfer.files).filter((f) => f.type.startsWith("image/"));
    if (images.length > 0) onUploadImages!(targetFile!, images);
  };

  const atoms = ticks.filter((t) => t.kind === "atom");
  // How many alternates (see Storyteller.Common.Swipe) currently sit in a
  // given atom's own carousel — every "swipe"-kind tick in the full list
  // that references it.
  function swipeCountOf(tickId: string): number {
    return ticks.filter((t) => t.kind === "swipe" && t.refs.includes(tickId)).length;
  }
  // Maps each atom's own tickId to the id rebaseMarker should actually be
  // set to if dropped in the gap right below it — see lib/utils.tailLeadTicks.
  const leads = tailLeadTicks(ticks);
  const atomRefs = useRef<Map<string, HTMLDivElement>>(new Map());
  const contentRef = useRef<HTMLDivElement>(null);
  const [barRects, setBarRects] = useState<{ character: string; color: string; lane: number; top: number; height: number }[]>([]);

  const [dragging, setDragging] = useState(false);
  const [candidate, setCandidateState] = useState<string | null>(null);
  const candidateRef = useRef<string | null>(null);
  function setCandidate(id: string | null) {
    candidateRef.current = id;
    setCandidateState(id);
  }

  // Flip target once the cursor passes an atom's own vertical middle — that's
  // the point where it visually stops being "in" the previous atom and starts
  // being "in" this one — rather than nearest-center-of-any-atom, which drifts
  // off with uneven atom heights. Landing on (or past) the last atom collapses
  // to "clear": rebasing right at the last tick is a no-op on the server
  // anyway (nothing downstream to replay), so there's no reason to make the
  // user thread the needle between "select the last tick" and "release to
  // resume at the present" — dragging to the bottom just means the latter.
  //
  // The resolved value is what rebaseMarker should become — the tick right
  // after the atom's whole block, including any trailing presence/note/
  // prompt ticks (lib/utils.tailLeadTicks) — not the atom's own tickId, or
  // "at" would treat that trailing history as part of the tail to rebase
  // away instead of already-settled. No entry (dropped past the last real
  // tick in the chain) means "clear the marker" — nothing left to be stuck to.
  function nearestLeadId(clientY: number): string | null {
    if (atoms.length === 0) return null;
    let candidateAtom: WireTick | null = null;
    for (const atom of atoms) {
      const el = atomRefs.current.get(atom.tickId);
      if (!el) continue;
      if (clientY >= el.getBoundingClientRect().top + el.getBoundingClientRect().height / 2) candidateAtom = atom;
      else break;
    }
    return candidateAtom ? (leads.get(candidateAtom.tickId) ?? null) : null;
  }

  function runDrag(initialClientY: number) {
    setDragging(true);
    setCandidate(nearestLeadId(initialClientY));
    const onMove = (ev: MouseEvent) => setCandidate(nearestLeadId(ev.clientY));
    const onUp = () => {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
      setDragging(false);
      onSetRebaseMarker(candidateRef.current);
    };
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  function startDrag(e: React.MouseEvent) {
    e.preventDefault();
    runDrag(e.clientY);
  }

  // The active divider line is also a drag handle: a plain click (no
  // movement) clears the marker, same as before; moving past a small
  // threshold before release hands off into the same drag as the handle.
  function startDragFromDivider(e: React.MouseEvent) {
    e.preventDefault();
    const startX = e.clientX, startY = e.clientY;
    const onMove = (ev: MouseEvent) => {
      if (Math.hypot(ev.clientX - startX, ev.clientY - startY) > 4) {
        window.removeEventListener("mousemove", onMove);
        window.removeEventListener("mouseup", onUp);
        runDrag(ev.clientY);
      }
    };
    const onUp = () => {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
      onSetRebaseMarker(null);
    };
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  const atomIds = new Set(ticks.filter((t) => t.kind === "atom").map((t) => t.tickId));

  const annotationsFor = new Map<string, WireTick[]>();
  // The Prompt tick leading each atom group, keyed by that group's first
  // atom — unlike note/character-answer, a prompt reads forward ("here's
  // what was asked for next"), not as commentary trailing what came
  // before, so it's excluded from annotationsFor's backward-anchored
  // bucket entirely and rendered by PromptHeader instead (see below).
  const promptBefore = new Map<string, WireTick>();
  const leading: WireTick[] = [];
  let lastAtomId: string | null = null;
  let pendingPrompt: WireTick | null = null;
  for (const tick of ticks) {
    if (tick.kind === "atom") {
      if (pendingPrompt) { promptBefore.set(tick.tickId, pendingPrompt); pendingPrompt = null; }
      lastAtomId = tick.tickId;
    } else if (tick.kind === "prompt") {
      pendingPrompt = tick;
    } else {
      const refAtom = [...tick.refs].reverse().find((r) => atomIds.has(r));
      const anchor = refAtom ?? lastAtomId;
      if (anchor) {
        const arr = annotationsFor.get(anchor) ?? [];
        arr.push(tick);
        annotationsFor.set(anchor, arr);
      } else {
        // Predates any atom in this file — nothing to anchor to yet,
        // rendered as its own block ahead of the first atom instead.
        leading.push(tick);
      }
    }
  }

  const markerIdx = rebaseMarker ? atoms.findIndex((a) => leads.get(a.tickId) === rebaseMarker) : -1;

  // Recompute presence-bar pixel spans whenever the set of runs to draw
  // changes, or the content reflows for any other reason (editing a
  // textarea open, annotations toggling, window resize) — a ResizeObserver
  // on the content box covers all of those uniformly rather than trying to
  // enumerate every cause of a layout shift. Positions are read as
  // 'offsetTop'/'offsetHeight' relative to 'contentRef' (the nearest
  // positioned ancestor), so no scroll-position math is needed.
  const barsKey = presenceBars.map((b) => `${b.character}:${[...b.tickIds].sort().join(",")}`).join("|");
  const atomsKey = atoms.map((a) => a.tickId).join(",");
  useLayoutEffect(() => {
    function recompute() {
      const rects: typeof barRects = [];
      presenceBars.forEach((bar, lane) => {
        let runStart = -1;
        for (let i = 0; i <= atoms.length; i++) {
          const inRun = i < atoms.length && bar.tickIds.has(atoms[i].tickId);
          if (inRun && runStart === -1) runStart = i;
          if (!inRun && runStart !== -1) {
            const startEl = atomRefs.current.get(atoms[runStart].tickId);
            const endEl   = atomRefs.current.get(atoms[i - 1].tickId);
            if (startEl && endEl) {
              rects.push({
                character: bar.character, color: bar.color, lane,
                top: startEl.offsetTop,
                height: (endEl.offsetTop + endEl.offsetHeight) - startEl.offsetTop,
              });
            }
            runStart = -1;
          }
        }
      });
      setBarRects(rects);
    }
    recompute();
    const container = contentRef.current;
    if (!container) return;
    const ro = new ResizeObserver(recompute);
    ro.observe(container);
    return () => ro.disconnect();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [barsKey, atomsKey]);

  return (
    <div
      onDragOver={handleImageDragOver}
      onDragLeave={handleImageDragLeave}
      onDrop={handleImageDrop}
      style={{
        position: "relative", flex: compact ? undefined : 1, display: "flex", flexDirection: "column",
        overflow: compact ? "visible" : "hidden",
        background: imageDragOver ? "var(--amber-wash)" : "transparent",
        outline: imageDragOver ? "1px dashed var(--amber)" : "none", outlineOffset: -2,
      }}
    >
      <div ref={scrollRef} style={{ flex: compact ? undefined : 1, overflow: compact ? "visible" : "auto" }}>
        <div ref={contentRef} style={compact
          ? { padding: "6px 8px", position: "relative" }
          : { maxWidth: 680, margin: "0 auto", padding: "28px 32px 48px", position: "relative" }}>
          {barRects.map((r, idx) => (
            <div
              key={`${r.character}-${idx}`}
              title={characterDisplayName(r.character)}
              style={{
                // 'left' for an absolutely positioned child is measured from
                // the container's padding-box edge — i.e. left:0 sits at the
                // very start of the 32px padding, *before* it, not at the
                // content start after it. Atom text actually starts around
                // x=30 (32px container padding, minus AtomBlock's own -12
                // margin, plus its own +10 padding). Positive offsets here,
                // just inside that gutter, land next to the atom's own
                // selection bar (its hit-zone sits at x≈20 in this same
                // frame) — not negative ones pushing further past the edge.
                position: "absolute", top: r.top, height: r.height,
                left: 16 - r.lane * 3, width: 1.5, borderRadius: 0.75,
                background: r.color, pointerEvents: "none",
              }}
            />
          ))}
          {leading.length > 0 && annotationMode !== "hidden" && (
            annotationMode === "dots" ? (
              <AnnotationDots annotations={leading} contextAnnotations={contextAnnotations} onToggleContext={onToggleContextAnnotation} activeBranch={activeBranch} onOpenSummary={onOpenSummary} />
            ) : (
              leading.map((ann) => (
                <AnnotationCard
                  key={ann.tickId} tick={ann}
                  inContext={contextAnnotations.has(ann.tickId)}
                  onToggleContext={onToggleContextAnnotation}
                  activeBranch={activeBranch}
                  onOpenSummary={onOpenSummary}
                />
              ))
            )
          )}
          {atoms.map((atom, i) => {
            const anns = annotationsFor.get(atom.tickId) ?? [];
            const isLast = i === atoms.length - 1 && (annotationMode === "hidden" || anns.length === 0);
            const suppressed = markerIdx !== -1 && i > markerIdx;
            return (
              <div
                key={atom.tickId}
                ref={(el) => {
                  if (el) atomRefs.current.set(atom.tickId, el);
                  else atomRefs.current.delete(atom.tickId);
                }}
              >
                <div style={{
                  opacity: suppressed ? 0.35 : 1,
                  filter: suppressed ? "grayscale(0.7)" : "none",
                  pointerEvents: suppressed ? "none" : "auto",
                  transition: "opacity 0.15s, filter 0.15s",
                }}>
                  {annotationMode !== "hidden" && onEditPrompt && promptBefore.get(atom.tickId) && (
                    <PromptHeader
                      tick={promptBefore.get(atom.tickId)!}
                      compact={annotationMode === "dots"}
                      onEditPrompt={onEditPrompt}
                    />
                  )}
                  <AtomBlock
                    atom={atom} isLast={isLast}
                    inContext={contextAtoms.has(atom.tickId)}
                    swipeCount={swipeCountOf(atom.tickId)}
                    onEdit={onEdit}
                    onToggleContext={onToggleContextAtom}
                    onCycleSwipe={onCycleSwipe}
                    onHoverAtom={onHoverAtom}
                    onHoverEnd={onHoverEnd}
                    compact={compact}
                    onCorrect={onCorrect}
                  />
                  {annotationMode === "dots" && anns.length > 0 && (
                    <AnnotationDots annotations={anns} contextAnnotations={contextAnnotations} onToggleContext={onToggleContextAnnotation} activeBranch={activeBranch} onOpenSummary={onOpenSummary} />
                  )}
                  {annotationMode === "expanded" && anns.map((ann) => (
                    <AnnotationCard
                      key={ann.tickId} tick={ann}
                      inContext={contextAnnotations.has(ann.tickId)}
                      onToggleContext={onToggleContextAnnotation}
                      activeBranch={activeBranch}
                      onOpenSummary={onOpenSummary}
                    />
                  ))}
                </div>
                <RebaseDropZone
                  isMarker={!dragging && rebaseMarker !== null && rebaseMarker === leads.get(atom.tickId)}
                  isCandidate={dragging && candidate !== null && candidate === leads.get(atom.tickId)}
                  onDragStart={startDragFromDivider}
                />
              </div>
            );
          })}
        </div>
      </div>
      <RebaseHandle
        active={rebaseMarker !== null}
        dragging={dragging}
        willClear={dragging && candidate === null}
        onDragStart={startDrag}
      />
    </div>
  );
}

// ── Ordinary file content: ticks + chat/agent strip + input bar ─────────────
//
// "Viewing a file" is exactly this cluster (WireTickList, then
// ChatPreviewStrip/AgentLogStrip/InputBar below it) — the one place it's
// assembled, so anything that's genuinely "just another file" (a summary
// tier's own connection — see .summarization-ui.md) renders through this
// exact same component instead of a hand-rolled re-implementation that can
// drift from it. 'absent'/'absentMessage' swap the tick list for a plain
// message (still with InputBar below, so appending can create it) — same
// shape the plain file view and a summary tier both need, just worded
// differently.
export function FileContentView({
  ticks, emptyMessage, annotationMode, contextAtoms, contextAnnotations, resetKey,
  rebaseMarker, onSetRebaseMarker, presenceBars,
  onEdit, onToggleContextAtom, onToggleContextAnnotation, onCycleSwipe, onCorrect, onEditPrompt,
  activeBranch, targetFile, onUploadImages, onOpenSummary,
  agentLogs, onClearAgentLogs,
  enabled, contextAtomCount, contextAnnotationCount, rebasing, onClearRebase, onClearContext,
  onAppend, onWrite, onFix, onNote, onRegen, onRoleplay, onAsk, onInform, onSummarize,
}: {
  ticks: WireTick[];
  // Non-null replaces the tick list with this plain message — covers both
  // "doesn't exist yet" and "still loading" — the input bar cluster below
  // still renders either way, so appending can create/populate it.
  emptyMessage: string | null;
  annotationMode: AnnotationMode;
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  resetKey: unknown;
  rebaseMarker: string | null;
  onSetRebaseMarker: (tickId: string | null) => void;
  presenceBars: PresenceBar[];
  onEdit: (tickId: string, content: string) => void;
  onToggleContextAtom: (tickId: string) => void;
  onToggleContextAnnotation: (tickId: string) => void;
  onCycleSwipe: (tickId: string) => void;
  onCorrect?: (tickId: string) => void;
  onEditPrompt?: (tickId: string, content: string) => void;
  activeBranch: string | null;
  targetFile?: string | null;
  onUploadImages?: (path: string, files: FileList | File[]) => void;
  onOpenSummary?: (kind: string, tickId: string) => void;
  agentLogs: { level: string; message: string }[];
  onClearAgentLogs: () => void;
  enabled: boolean;
  contextAtomCount: number;
  contextAnnotationCount: number;
  rebasing: boolean;
  onClearRebase: () => void;
  onClearContext: () => void;
  onAppend: (text: string) => void;
  onWrite: (text: string) => void;
  onFix: (text: string) => void;
  onNote: (text: string) => void;
  onRegen: (text: string, byBeat: boolean) => void;
  onRoleplay: (text: string) => void;
  onAsk: (character: string, question: string) => void;
  onInform: (character: string, fact: string) => void;
  onSummarize: () => void;
}) {
  return (
    <>
      {emptyMessage !== null ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          {emptyMessage}
        </div>
      ) : (
        <WireTickList
          ticks={ticks} annotationMode={annotationMode}
          contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
          resetKey={resetKey}
          rebaseMarker={rebaseMarker}
          onSetRebaseMarker={onSetRebaseMarker}
          presenceBars={presenceBars}
          onEdit={onEdit}
          onToggleContextAtom={onToggleContextAtom}
          onToggleContextAnnotation={onToggleContextAnnotation}
          onCycleSwipe={onCycleSwipe}
          onCorrect={onCorrect}
          onEditPrompt={onEditPrompt}
          activeBranch={activeBranch}
          targetFile={targetFile}
          onUploadImages={onUploadImages}
          onOpenSummary={onOpenSummary}
        />
      )}
      <ChatPreviewStrip />
      <AgentLogStrip logs={agentLogs} onClear={onClearAgentLogs} />
      <InputBar
        enabled={enabled}
        activeBranch={activeBranch}
        contextAtomCount={contextAtomCount} contextAnnotationCount={contextAnnotationCount}
        rebasing={rebasing}
        onClearRebase={onClearRebase}
        onClearContext={onClearContext}
        onAppend={onAppend}
        onWrite={onWrite}
        onFix={onFix}
        onNote={onNote}
        onRegen={onRegen}
        onRoleplay={onRoleplay}
        onAsk={onAsk}
        onInform={onInform}
        onSummarize={onSummarize}
      />
    </>
  );
}

// A summary family's split view: a read-only top pane showing exactly what
// one specific occurrence covers (computed client-side via
// 'summaryCoverageFor', from ticks the main file connection already has
// loaded — no separate materialized tree needed), and a genuinely ordinary
// file view below it (see FileContentView just above) pointed at this
// tier's own connection instead of the real file. Nesting is just "open
// one more hop" — the bottom's own 'onOpenSummary' recurses through this
// exact same component one level deeper.
export function SummarySplitView({
  kind, nodePath, coveredTicks, onBack, showFullChain, onToggleFullChain, ...contentProps
}: {
  kind: string;
  nodePath: string[];
  coveredTicks: WireTick[];
  onBack: () => void;
  // The bottom pane defaults to just this occurrence's own delta (see
  // page.tsx's activeTicksChain) -- flipping this shows the family's
  // whole current chain instead, so a nested tier's own annotation
  // (which might sit outside this one occurrence's own delta) is never
  // more than one click away, instead of having to hunt down whichever
  // specific occurrence happens to cover it.
  showFullChain: boolean;
  onToggleFullChain: () => void;
} & Parameters<typeof FileContentView>[0]) {
  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      <div style={{
        flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)",
        fontSize: 10, color: "var(--text-ghost)", display: "flex", justifyContent: "space-between", alignItems: "center",
      }}>
        {/* nodePath is the full hop chain (see page.tsx's viewTarget doc) --
            one hop is just "a specific occurrence of this file's own kind",
            not nesting; only a *second* hop is genuinely one tier deeper. */}
        <span>{summaryKindLabel(kind)}{nodePath.length > 1 ? ` — tier ${nodePath.length - 1}` : ""}</span>
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <button
            onClick={onToggleFullChain}
            title={showFullChain ? "Showing the whole chain — click to show just this occurrence's own new ticks" : "Showing just this occurrence's own new ticks — click to show the whole chain"}
            style={{
              background: showFullChain ? "var(--amber-tint)" : "transparent",
              border: `1px solid ${showFullChain ? "var(--amber-border)" : "var(--border-subtle)"}`,
              color: showFullChain ? "var(--amber)" : "var(--text-ghost)",
              cursor: "pointer", fontSize: 10, borderRadius: 4, padding: "2px 7px",
            }}
          >
            {showFullChain ? "Whole chain" : "This pass only"}
          </button>
          <button onClick={onBack} style={{ background: "transparent", border: "none", color: "var(--text-ghost)", cursor: "pointer", fontSize: 10 }}>close</button>
        </div>
      </div>
      <div style={{ flex: "0 0 40%", overflow: "auto", borderBottom: "2px solid var(--border-subtle)" }}>
        <WireTickList
          ticks={coveredTicks}
          annotationMode="dots"
          contextAtoms={EMPTY_TICK_SET}
          contextAnnotations={EMPTY_TICK_SET}
          resetKey={contentProps.resetKey}
          rebaseMarker={null}
          onSetRebaseMarker={() => {}}
          presenceBars={[]}
          onEdit={() => {}}
          onToggleContextAtom={() => {}}
          onToggleContextAnnotation={() => {}}
          onCycleSwipe={() => {}}
          compact
        />
      </div>
      <div style={{ flex: 1, overflow: "auto", display: "flex", flexDirection: "column" }}>
        <FileContentView {...contentProps} />
      </div>
    </div>
  );
}

// ── Text/Source edit modes ────────────────────────────────────────────────────

// Whole-file text editing, bypassing atoms/positions entirely — for bulk
// changes (find/replace across a scene, pasting in a rewritten draft) that
// would be tedious to make atom-by-atom. Loads the file's current raw bytes
// over plain HTTP (same GET the download/embed path already uses — see
// lib/ws.branchFileUrl) rather than folding the tick chain client-side, so
// what's shown here always matches exactly what a save will diff against.
// Saving goes through 'saveRawFile' (PUT .../$raw/...), which reconciles
// server-side against the existing atom chain (Storage.Ops.commitFile) —
// unchanged paragraphs keep their atom ids, only what actually differs gets
// rewritten, so this stays close to a normal edit rather than a full replace.
export function RawEditPanel({ branch, path }: {
  branch: string;
  path: string;
}) {
  const [content, setContent] = useState<string | null>(null);
  const [savedContent, setSavedContent] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    setContent(null);
    fetch(branchFileUrl(branch, path))
      .then((res) => {
        if (!res.ok) throw new Error(`load failed: ${res.status}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text);
        setSavedContent(text);
        setLoading(false);
      })
      .catch((err) => {
        if (cancelled) return;
        setError(err instanceof Error ? err.message : String(err));
        setLoading(false);
      });
    return () => { cancelled = true; };
  }, [branch, path]);

  const dirty = content !== null && content !== savedContent;

  function save() {
    if (content === null || saving) return;
    setSaving(true);
    setError(null);
    saveRawFile(branch, path, content)
      .then(() => { setSavedContent(content); setSaving(false); })
      .catch((err) => {
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }

  // "Save as new": bypasses atom-diff reconciliation entirely -- for a
  // structural edit (reordered/reworked list content) that shouldn't be
  // tracked atom-by-atom against whatever was here before. No note/atom
  // continuity carried forward, contrast with an ordinary 'save'.
  function saveAsNew() {
    if (content === null || saving) return;
    setSaving(true);
    setError(null);
    saveRawFileAsNew(branch, path, content)
      .then(() => { setSavedContent(content); setSaving(false); })
      .catch((err) => {
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      <div style={{
        flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8, fontSize: 10,
      }}>
        <span style={{ color: "var(--text-ghost)" }}>Raw edit — whole-file text, reconciled against atoms on save</span>
        <span style={{ flex: 1 }} />
        {error && <span style={{ color: "var(--rose)" }}>{error}</span>}
        {dirty && !error && <span style={{ color: "var(--amber)" }}>unsaved</span>}
        <button
          onClick={saveAsNew}
          disabled={!dirty || saving}
          title="Replace this file's content wholesale — no atom-by-atom diff, no note/atom continuity carried forward"
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: "transparent",
            border: "1px solid var(--border-subtle)",
            color: dirty ? "var(--text-dim)" : "var(--text-ghost)",
          }}
        >
          Save as new
        </button>
        <button
          onClick={save}
          disabled={!dirty || saving}
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "var(--amber-tint)" : "transparent",
            border: "1px solid " + (dirty ? "var(--amber-border)" : "var(--border-subtle)"),
            color: dirty ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          {saving ? "Saving…" : "Save"}
        </button>
      </div>
      {loading ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          Loading…
        </div>
      ) : (
        <textarea
          value={content ?? ""}
          onChange={(e) => setContent(e.target.value)}
          onKeyDown={(e) => {
            if ((e.metaKey || e.ctrlKey) && e.key === "s") { e.preventDefault(); save(); }
          }}
          spellCheck={false}
          style={{
            flex: 1, width: "100%", resize: "none", border: "none", outline: "none",
            padding: "14px 18px", fontFamily: "ui-monospace, monospace", fontSize: 12.5,
            lineHeight: 1.6, color: "var(--text-body)", background: "transparent",
          }}
        />
      )}
    </div>
  );
}

// A shared no-op tick-id set for a read-only 'WireTickList' instance (the
// summary split view's top coverage pane, in page.tsx) — selection/rebase
// concepts don't apply there, so this is just a stable empty identity
// rather than allocating a fresh Set on every render.
export const EMPTY_TICK_SET: Set<string> = new Set();

// A save-time snapshot of the document's top-level blocks, kept in step
// with whatever's actually on the branch (loaded once, then refreshed after
// every successful save) — see 'computeMinimalSave' just below for why:
// re-serializing the *whole* doc on every save (tiptap-markdown's own
// canonical style — ATX headings, a fixed bullet marker, single blank
// lines — rarely matches whatever style the stored file actually used) was
// turning even a one-word edit into a full-document diff against the atom
// chain server-side, since nothing lined up byte-for-byte outside the
// edited paragraph either. 'nodes' and 'blockRanges' are parallel, one
// entry per top-level ProseMirror node / markdown-it block token; a
// mismatched count (HTML/DOM normalization occasionally merges or splits
// a block) is the signal that this file isn't safely diffable this way —
// 'snapshotOriginal' returns null, and 'save' below just falls back to
// sending the whole re-serialized document, exactly like before.
interface DocSnapshot {
  text: string;
  lines: string[];
  blockRanges: { start: number; end: number }[];
  nodes: ProsemirrorNode[];
}

function snapshotOriginal(editor: Editor, text: string): DocSnapshot | null {
  try {
    const tokens = editor.storage.markdown.parser.md.parse(text, {});
    const blockRanges: { start: number; end: number }[] = [];
    for (const t of tokens) {
      if (t.level === 0 && t.map) blockRanges.push({ start: t.map[0], end: t.map[1] });
    }
    const nodes = [...editor.state.doc.content.content];
    if (nodes.length !== blockRanges.length) return null;
    return { text, lines: text.split("\n"), blockRanges, nodes };
  } catch {
    return null;
  }
}

// Only the stretch of top-level blocks that actually changed gets
// re-serialized; an unchanged head and/or tail is sliced verbatim out of
// the last-saved text (byte-identical to what's already stored), so a
// small edit anywhere but the very front keeps the server's cheap
// prefix-matched reconciliation path instead of falling through to its
// O(atoms) full-history matcher. 'original' must correspond to whatever
// text is currently saved on the branch — the one invariant 'save' below
// has to maintain by re-snapshotting after every successful write.
function computeMinimalSave(editor: Editor, original: DocSnapshot): string {
  const oldNodes = original.nodes;
  const newNodes = editor.state.doc.content.content;
  const maxPrefix = Math.min(oldNodes.length, newNodes.length);

  let prefixLen = 0;
  while (prefixLen < maxPrefix && oldNodes[prefixLen].eq(newNodes[prefixLen])) prefixLen++;

  const maxSuffix = maxPrefix - prefixLen;
  let suffixLen = 0;
  while (
    suffixLen < maxSuffix &&
    oldNodes[oldNodes.length - 1 - suffixLen].eq(newNodes[newNodes.length - 1 - suffixLen])
  ) suffixLen++;

  if (prefixLen === oldNodes.length && prefixLen === newNodes.length) return original.text;

  const head = prefixLen > 0
    ? original.lines.slice(original.blockRanges[0].start, original.blockRanges[prefixLen - 1].end).join("\n")
    : "";
  const tail = suffixLen > 0
    ? original.lines.slice(
        original.blockRanges[oldNodes.length - suffixLen].start,
        original.blockRanges[oldNodes.length - 1].end,
      ).join("\n")
    : "";

  const middleNodes = newNodes.slice(prefixLen, newNodes.length - suffixLen);
  const middle = middleNodes.length > 0
    ? editor.storage.markdown.serializer.serialize(editor.schema.topNodeType.create(null, middleNodes))
    : "";

  return [head, middle, tail].filter((s) => s.length > 0).join("\n\n");
}

// Whole-file WYSIWYG editing — a Word-like surface (bold/strike/lists,
// natural copy-paste) over the same raw markdown 'RawEditPanel' edits, for
// quick editorial passes across a scene without threading through individual
// atoms. Loads/saves through the identical plumbing as raw mode (same GET/PUT
// pair, same server-side atom reconciliation) — only the editing widget
// differs: TipTap (via 'tiptap-markdown') owns the live document and speaks
// markdown in and out, rather than a controlled textarea string.
export function TextEditPanel({ branch, path }: {
  branch: string;
  path: string;
}) {
  const [savedContent, setSavedContent] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [dirty, setDirty] = useState(false);
  const savedContentRef = useRef<string | null>(null);
  const savingRef = useRef(false);
  const originalRef = useRef<DocSnapshot | null>(null);

  function save() {
    if (!editor || savingRef.current) return;
    const original = originalRef.current;
    const md = original ? computeMinimalSave(editor, original) : editor.storage.markdown.getMarkdown();
    savingRef.current = true;
    setSaving(true);
    setError(null);
    saveRawFile(branch, path, md)
      .then(() => {
        savedContentRef.current = md;
        setSavedContent(md);
        setDirty(false);
        // Must correspond exactly to what's now on the branch — null (not
        // the stale prior snapshot) on any mismatch, since a stale one
        // would make the next save's "unchanged head/tail" claim false.
        originalRef.current = snapshotOriginal(editor, md);
        savingRef.current = false;
        setSaving(false);
      })
      .catch((err) => {
        savingRef.current = false;
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }

  // "Save as new": bypasses atom-diff reconciliation entirely, so there's
  // no reason to bother with 'computeMinimalSave's head/middle/tail
  // slicing (that machinery exists purely to keep the server's diff small)
  // -- the full re-serialized document goes straight over, same as the
  // no-snapshot fallback 'save' itself uses.
  function saveAsNew() {
    if (!editor || savingRef.current) return;
    const md = editor.storage.markdown.getMarkdown();
    savingRef.current = true;
    setSaving(true);
    setError(null);
    saveRawFileAsNew(branch, path, md)
      .then(() => {
        savedContentRef.current = md;
        setSavedContent(md);
        setDirty(false);
        originalRef.current = snapshotOriginal(editor, md);
        savingRef.current = false;
        setSaving(false);
      })
      .catch((err) => {
        savingRef.current = false;
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }

  const editor = useEditor({
    extensions: [StarterKit, Markdown.configure({ html: false, transformPastedText: true })],
    content: "",
    immediatelyRender: false,
    onUpdate: ({ editor }) => setDirty(editor.storage.markdown.getMarkdown() !== savedContentRef.current),
    editorProps: {
      handleKeyDown: (_view, event) => {
        if ((event.metaKey || event.ctrlKey) && event.key === "s") { event.preventDefault(); save(); return true; }
        return false;
      },
    },
  });

  useEffect(() => {
    if (!editor) return;
    let cancelled = false;
    setLoading(true);
    setError(null);
    fetch(branchFileUrl(branch, path))
      .then((res) => {
        if (!res.ok) throw new Error(`load failed: ${res.status}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        editor.commands.setContent(text);
        savedContentRef.current = text;
        originalRef.current = snapshotOriginal(editor, text);
        setSavedContent(text);
        setDirty(false);
        setLoading(false);
      })
      .catch((err) => {
        if (cancelled) return;
        setError(err instanceof Error ? err.message : String(err));
        setLoading(false);
      });
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [branch, path, editor]);

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      <div style={{
        flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8, fontSize: 10,
      }}>
        <span style={{ color: "var(--text-ghost)" }}>Text edit — WYSIWYG, reconciled against atoms on save</span>
        <span style={{ flex: 1 }} />
        {error && <span style={{ color: "var(--rose)" }}>{error}</span>}
        {dirty && !error && <span style={{ color: "var(--amber)" }}>unsaved</span>}
        <button
          onClick={saveAsNew}
          disabled={!dirty || saving}
          title="Replace this file's content wholesale — no atom-by-atom diff, no note/atom continuity carried forward"
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: "transparent",
            border: "1px solid var(--border-subtle)",
            color: dirty ? "var(--text-dim)" : "var(--text-ghost)",
          }}
        >
          Save as new
        </button>
        <button
          onClick={save}
          disabled={!dirty || saving}
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "var(--amber-tint)" : "transparent",
            border: "1px solid " + (dirty ? "var(--amber-border)" : "var(--border-subtle)"),
            color: dirty ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          {saving ? "Saving…" : "Save"}
        </button>
      </div>
      {loading ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          Loading…
        </div>
      ) : (
        <div style={{ flex: 1, overflow: "auto", padding: "14px 18px" }}>
          <EditorContent
            editor={editor}
            className="tiptap-text-mode"
            style={{ fontFamily: "Georgia, serif", fontSize: 13, lineHeight: 1.8, color: "var(--text-body)" }}
          />
        </div>
      )}
    </div>
  );
}

// ── Agent log strip ───────────────────────────────────────────────────────────

export function AgentLogStrip({ logs, onClear }: {
  logs: { level: string; message: string }[];
  onClear: () => void;
}) {
  const containerRef = useRef<HTMLDivElement>(null);
  const [height, setHeight] = useState(0);
  const [collapsed, setCollapsed] = useState(false);

  useEffect(() => {
    const el = containerRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [logs, height]);

  useEffect(() => {
    if (logs.length > 0 && height === 0) { setHeight(102); setCollapsed(false); }
  }, [logs.length, height]);

  if (logs.length === 0) return null;

  if (collapsed) return (
    <div
      key={logs.length} className="log-pulse"
      onClick={() => setCollapsed(false)} title="Expand log"
      style={{ flexShrink: 0, height: 16, display: "flex", alignItems: "center", justifyContent: "center", borderTop: "1px solid oklch(0.17 0.01 60)", cursor: "pointer" }}
      onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.background = "oklch(0.16 0.01 60)"; }}
      onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.background = ""; }}
    >
      <ChevronUp style={{ width: 10, height: 10, color: "oklch(0.35 0.01 60)" }} />
    </div>
  );

  function levelColor(level: string) {
    if (level === "warning") return "var(--amber)";
    if (level === "error")   return "var(--rose)";
    return "oklch(0.38 0.01 60)";
  }

  function onDragHandleMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    const startY = e.clientY, startH = height;
    function onMove(ev: MouseEvent) { setHeight(Math.max(40, Math.min(300, startH + (startY - ev.clientY)))); }
    function onUp() { window.removeEventListener("mousemove", onMove); window.removeEventListener("mouseup", onUp); }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  const btnStyle: React.CSSProperties = {
    background: "none", border: "none", cursor: "pointer", padding: "0 4px",
    color: "oklch(0.35 0.01 60)", fontSize: 11, lineHeight: 1, transition: "color 0.12s",
  };

  return (
    <div style={{ flexShrink: 0, borderTop: "1px solid oklch(0.17 0.01 60)", background: "oklch(0.11 0.005 60)" }}>
      <div style={{ display: "flex", alignItems: "center", height: 16 }}>
        <div onMouseDown={onDragHandleMouseDown} style={{ flex: 1, height: "100%", cursor: "ns-resize" }} />
        <button style={btnStyle} title="Collapse" onClick={() => setCollapsed(true)}
          onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.color = "var(--text-ghost)"; }}
          onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.color = "oklch(0.35 0.01 60)"; }}
        ><ChevronDown style={{ width: 10, height: 10 }} /></button>
        <div onMouseDown={onDragHandleMouseDown} style={{ flex: 1, height: "100%", cursor: "ns-resize" }} />
        <button style={{ ...btnStyle, paddingRight: 8 }} title="Clear log" onClick={() => { onClear(); setHeight(0); }}
          onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.color = "var(--rose)"; }}
          onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.color = "oklch(0.35 0.01 60)"; }}
        >×</button>
      </div>
      <div ref={containerRef} style={{ height, overflow: "auto", padding: "0 14px 6px" }}>
        {logs.map((entry, i) => (
          <div key={i} style={{ display: "flex", gap: 8, lineHeight: 1.5 }}>
            <span style={{ fontSize: 9, color: levelColor(entry.level), fontFamily: "monospace", flexShrink: 0, paddingTop: 1 }}>{entry.level}</span>
            <span style={{ fontSize: 10, color: "oklch(0.42 0.01 60)", fontFamily: "monospace", whiteSpace: "pre-wrap", wordBreak: "break-all" }}>{entry.message}</span>
          </div>
        ))}
      </div>
    </div>
  );
}

// ── Chat preview strip ────────────────────────────────────────────────────────

// Ephemeral, best-effort draft of an in-flight chat.prompt/chargen call —
// see WS-PROTOCOL.md "Chat preview (streaming)". Purely a live look at
// tokens as they arrive; it disappears the moment the store clears
// `preview` (on chat.preview.end, or the real update/error superseding it).
//
// Subscribes to 'preview' itself rather than taking it as a prop: the
// stream can push several updates a second, so a parent (page.tsx) that
// merely forwarded the value would re-render its entire tree on every one.
// Reading it here confines that reconciliation to just this strip.
export function ChatPreviewStrip() {
  const preview = useServerCache((s) => s.preview);
  const containerRef = useAutoScroll<HTMLDivElement>(
    (preview?.text.length ?? 0) + (preview?.thinking.length ?? 0), preview === null, "end"
  );

  if (preview === null) return null;

  const isEmpty = preview.text.length === 0 && preview.thinking.length === 0;

  return (
    <div style={{ flexShrink: 0, borderTop: "1px solid var(--amber-border)", background: "oklch(0.16 0.02 65 / 0.5)" }}>
      <div ref={containerRef} style={{ maxHeight: 140, overflow: "auto", padding: "8px 16px" }}>
        {preview.thinking && (
          <div style={{ fontSize: 11, fontStyle: "italic", color: "var(--text-ghost)", marginBottom: 6, whiteSpace: "pre-wrap" }}>
            {preview.thinking}
          </div>
        )}
        {isEmpty ? (
          <div style={{ fontSize: 11, fontStyle: "italic", color: "var(--text-ghost)" }}>
            Generating…
          </div>
        ) : (
          <div style={{ fontSize: 12, fontFamily: "Georgia, serif", color: "var(--text-muted)", whiteSpace: "pre-wrap", lineHeight: 1.6 }}>
            {preview.text}
          </div>
        )}
      </div>
    </div>
  );
}

// ── Input bar ─────────────────────────────────────────────────────────────────

// Routable agents behind the input bar. 'write' is Writer (or FlowWriter,
// implicitly, when a generation is already in flight — see the store's
// chatWrite). 'fix' targets the current atom selection. 'append' is the
// instant, non-LLM verbatim insert. 'note' is also instant and non-LLM —
// attaches the text as an annotation on the current selection, or (with
// nothing selected) on the file's HEAD tick. 'regen' still covers beat-by-
// beat via the "/regen @beat" command (lib/commands.ts) — that's a command
// parameter, not a separate mode, so it doesn't get its own pill.
type AgentId = "write" | "fix" | "append" | "note" | "regen" | "roleplay";

// One color per mode so the input bar's own border/pill tells you where a
// plain (non-"/command") send will land, without having to check anything
// else. 'note' reuses the blue already used for annotation ticks elsewhere
// in this file (see WireTickList); the rest are fresh hues not otherwise
// claimed in the app's palette.
const MODE_COLOR: Record<AgentId, string> = {
  write:    "0.78 0.10 65",
  fix:      "0.68 0.12 200",
  append:   "0.62 0.02 60",
  note:     "0.60 0.15 240",
  regen:    "0.68 0.15 300",
  roleplay: "0.72 0.14 20",
};
function modeColor(id: AgentId, alpha?: number): string {
  return alpha === undefined ? `oklch(${MODE_COLOR[id]})` : `oklch(${MODE_COLOR[id]} / ${alpha})`;
}

const MODE_ORDER: AgentId[] = ["write", "fix", "append", "note", "regen", "roleplay"];

const AGENT_META: Record<AgentId, { label: string; title: string; icon: typeof Sparkles | null }> = {
  write:    { label: "Write",    title: "Send to writer agent",               icon: Sparkles },
  fix:      { label: "Fix",      title: "Send to fixer agent (edit targets)",  icon: Wrench },
  append:   { label: "Append",   title: "Append verbatim, instant",            icon: null },
  note:     { label: "Note",     title: "Attach as a note, instant",           icon: StickyNote },
  regen:    { label: "Regen",    title: "Regenerate this chapter to fit its beat sheet", icon: RefreshCw },
  roleplay: { label: "Roleplay", title: "Interrogate every character present, then write the scene", icon: Users },
};

export function InputBar({ enabled, activeBranch, contextAtomCount, contextAnnotationCount, rebasing, onClearRebase, onClearContext, onAppend, onWrite, onFix, onNote, onRegen, onRoleplay, onAsk, onInform, onSummarize }: {
  enabled: boolean;
  // The active branch's own /lore data feeds '@mention' completion (see
  // lib/mentions.ts) — current-branch-only for now, no cross-branch search.
  activeBranch: string | null;
  contextAtomCount: number;
  contextAnnotationCount: number;
  rebasing: boolean;
  onClearRebase: () => void;
  onClearContext: () => void;
  onAppend: (text: string) => void;
  onWrite:  (text: string) => void;
  onFix:    (text: string) => void;
  onNote:   (text: string) => void;
  onRegen:  (text: string, byBeat: boolean) => void;
  onRoleplay: (text: string) => void;
  // Not a mode in AGENT_META (this doesn't edit the file) — only reachable
  // via the "/ask @character=..." command, never the mode pill/dropdown.
  onAsk:    (character: string, question: string) => void;
  // Same — only reachable via "/inform @character=..." (see lib/commands.ts).
  onInform: (character: string, fact: string) => void;
  // Same — only reachable via "/summarize" (see lib/commands.ts), no
  // args: which kind(s) to run is this file's own static classification
  // (lib/library.ts's summaryKindsFor), same as the toolbar button.
  onSummarize: () => void;
}) {
  // Whether a chat.writer/fixer/regen/etc call is currently streaming on
  // this file's connection. Read directly (not passed as a prop) for the
  // same reason 'ChatPreviewStrip' does — a value that changes several
  // times a second shouldn't force whatever parent forwards it to
  // re-render. Swaps the mode-pill/send cluster below for a Stop button
  // (see fileview.actions.ts's 'cancelGeneration') while true.
  const generating = useServerCache((s) => s.preview !== null);
  const [text, setText] = useState("");
  const [height, setHeight] = useState(90);
  const [menuOpen, setMenuOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  // Positioning only — menuRef above still owns click-outside detection
  // over the whole button+dropdown cluster. flip()/shift() cover the mode
  // dropdown opening near a screen edge, same reasoning as
  // CommandSuggestionPopup's own Floating UI migration.
  const modeMenu = useFloating({
    open: menuOpen,
    placement: "top-end",
    middleware: [offset(4), flip(), shift({ padding: 8 })],
    whileElementsMounted: autoUpdate,
  });
  const auto = useCommandAutocomplete(text, setText);

  const hasContext = contextAtomCount > 0 || contextAnnotationCount > 0;

  // Explicit, sticky mode — Shift+Tab (or the pill/dropdown) cycles it, and
  // it stays put until you change it again. Seeded from context at mount
  // only (selection present → Fix, since that's the common case), not kept
  // in sync afterward: once you've picked a mode, it's yours until you pick
  // a different one, not silently swapped out from under you.
  const [mode, setMode] = useState<AgentId>(() => (hasContext ? "fix" : "write"));

  // Mentions only make sense for Write (the one mode with a contextLayout
  // channel — see fileview.actions.ts's chatWrite) and only outside a
  // '/command' line ('@' there is already command-param syntax, see
  // lib/commands.ts). Passing an empty entries list is how this hook gets
  // disabled — mentionSuggestions itself doesn't know about modes.
  const loreEntries = flattenLore(useLoreTree(activeBranch));
  const mentionsEnabled = mode === "write" && !text.trimStart().startsWith("/");
  const mentionAuto = useMentionAutocomplete(text, setText, mentionsEnabled ? loreEntries : [], auto.taRef);

  function cycleMode(dir: 1 | -1) {
    setMode((m) => {
      const i = MODE_ORDER.indexOf(m);
      return MODE_ORDER[(i + dir + MODE_ORDER.length) % MODE_ORDER.length];
    });
  }

  const actionFor: Record<AgentId, (t: string) => void> = {
    write: onWrite, fix: onFix, append: onAppend, note: onNote, regen: (t) => onRegen(t, false),
    roleplay: onRoleplay,
  };

  // A recognized leading "/command" always wins over the currently selected
  // mode — see lib/commands.ts.
  const commandActions: Record<string, (t: string, params: Record<string, string>) => void> = {
    write: (t) => onWrite(t), fix: (t) => onFix(t), append: (t) => onAppend(t), note: (t) => onNote(t),
    regen: (t, p) => onRegen(t, p.beat !== undefined),
    roleplay: (t) => onRoleplay(t),
    ask: (t, p) => { if (p.character) onAsk(p.character, t); },
    inform: (t, p) => { if (p.character) onInform(p.character, t); },
    summarize: () => onSummarize(),
  };

  function fire() {
    const raw = text.trim();
    if (!raw) return;
    const parsed = parseCommand(raw);
    const action = parsed && commandActions[parsed.name];
    const noText = parsed && COMMANDS.find((c) => c.name === parsed.name)?.noText;
    if (parsed && action) {
      if (parsed.text || noText) action(parsed.text, parsed.params);
    } else {
      actionFor[mode](raw);
    }
    setText("");
    setMenuOpen(false);
  }

  useEffect(() => {
    if (!menuOpen) return;
    function onDocClick(e: MouseEvent) {
      if (menuRef.current && !menuRef.current.contains(e.target as Node)) setMenuOpen(false);
    }
    document.addEventListener("mousedown", onDocClick);
    return () => document.removeEventListener("mousedown", onDocClick);
  }, [menuOpen]);

  function onDragHandleMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    const startY = e.clientY, startH = height;
    function onMove(ev: MouseEvent) { setHeight(Math.max(60, Math.min(400, startH + (startY - ev.clientY)))); }
    function onUp() { window.removeEventListener("mousemove", onMove); window.removeEventListener("mouseup", onUp); }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  function contextLabel() {
    const parts: string[] = [];
    if (contextAtomCount > 0) parts.push(`${contextAtomCount} atom${contextAtomCount !== 1 ? "s" : ""}`);
    if (contextAnnotationCount > 0) parts.push(`${contextAnnotationCount} annotation${contextAnnotationCount !== 1 ? "s" : ""}`);
    return parts.join(" · ") + " selected";
  }

  return (
    <div style={{
      flexShrink: 0, height,
      borderTop: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
      display: "flex", flexDirection: "column",
      opacity: enabled ? 1 : 0.4, pointerEvents: enabled ? "auto" : "none",
    }}>
      <div onMouseDown={onDragHandleMouseDown} style={{ height: 4, cursor: "ns-resize", flexShrink: 0 }} />
      {rebasing && (
        <div style={{ flexShrink: 0, padding: "2px 16px 0", display: "flex", alignItems: "center", gap: 6 }}>
          <History style={{ width: 11, height: 11, color: "var(--amber)" }} />
          <span style={{ fontSize: 10, color: "var(--amber)", fontStyle: "italic" }}>
            Writing rebased at the marker — later atoms will shift on top
          </span>
          <button onClick={onClearRebase} title="Clear rebase marker" style={{ background: "none", border: "none", cursor: "pointer", color: "var(--amber)", fontSize: 12, lineHeight: 1, padding: "0 2px", opacity: 0.7 }}>×</button>
        </div>
      )}
      {hasContext && (
        <div style={{ flexShrink: 0, padding: "2px 16px 0", display: "flex", alignItems: "center", gap: 6 }}>
          <span style={{ fontSize: 10, color: "var(--amber)", fontStyle: "italic" }}>{contextLabel()}</span>
          <button onClick={onClearContext} title="Clear context selection" style={{ background: "none", border: "none", cursor: "pointer", color: "var(--amber)", fontSize: 12, lineHeight: 1, padding: "0 2px", opacity: 0.7 }}>×</button>
        </div>
      )}
      <div style={{ flex: 1, display: "flex", gap: 8, alignItems: "stretch", padding: "4px 16px 10px", minHeight: 0 }}>
        <div style={{ position: "relative", flex: 1, display: "flex" }}>
          <CommandSuggestionPopup suggestions={auto.suggestions} activeIndex={auto.activeIndex} onPick={auto.pick} reference={auto.taRef} />
          <CommandSuggestionPopup suggestions={mentionAuto.suggestions} activeIndex={mentionAuto.activeIndex} onPick={mentionAuto.pick} reference={auto.taRef} />
          <textarea
            ref={auto.taRef}
            value={text}
            onChange={(e) => setText(e.target.value)}
            onSelect={(e) => { auto.onSelect(e); mentionAuto.onSelect(e); }}
            onClick={(e) => { auto.onSelect(e); mentionAuto.onSelect(e); }}
            onKeyDown={(e) => {
              if (auto.onKeyDown(e)) return;
              if (mentionAuto.onKeyDown(e)) return;
              if (e.key === "Tab" && e.shiftKey) { e.preventDefault(); cycleMode(1); return; }
              if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); fire(); }
            }}
            placeholder={enabled ? `⌘↵ send as ${AGENT_META[mode].label.toLowerCase()} · shift-tab cycles mode · "/" for commands` : "Open a file to write"}
            style={{
              flex: 1, resize: "none", fontFamily: "Georgia, serif", fontSize: 12,
              background: "var(--card)", border: `1px solid ${modeColor(mode, 0.35)}`, borderRadius: 6,
              color: "var(--foreground)", padding: "6px 8px", outline: "none",
            }}
          />
        </div>
        <div ref={menuRef} style={{ position: "relative", display: "flex", flexDirection: "column", gap: 4, justifyContent: "flex-end" }}>
          {generating ? (
            <button onClick={cancelGeneration} title="Stop generating" style={{
              padding: "4px 10px",
              background: "var(--amber-tint)", border: "1px solid var(--amber-border)",
              borderRadius: 5, color: "var(--amber)", cursor: "pointer",
              display: "flex", alignItems: "center", gap: 4, justifyContent: "center",
            }}>
              <Square style={{ width: 10, height: 10 }} fill="currentColor" />
            </button>
          ) : (
          <div style={{ display: "flex", gap: 2 }}>
            <button onClick={() => fire()} title={`${AGENT_META[mode].title} (⌘↵) — shift-tab to change mode`} style={{
              flex: 1, padding: "4px 10px",
              background: modeColor(mode, 0.15), border: `1px solid ${modeColor(mode, 0.4)}`,
              borderRadius: "5px 0 0 5px", color: modeColor(mode), fontWeight: 600, fontSize: 11, cursor: "pointer",
              display: "flex", alignItems: "center", gap: 4, justifyContent: "center",
            }}>
              {(() => { const Icon = AGENT_META[mode].icon; return Icon ? <Icon style={{ width: 10, height: 10 }} /> : null; })()}
              {AGENT_META[mode].label}
            </button>
            <button ref={modeMenu.refs.setReference} onClick={() => setMenuOpen((o) => !o)} title="Choose mode (shift-tab cycles)" style={{
              padding: "4px 5px",
              background: modeColor(mode, 0.15), border: `1px solid ${modeColor(mode, 0.4)}`,
              borderLeft: `1px solid ${modeColor(mode, 0.4)}`,
              borderRadius: "0 5px 5px 0", color: modeColor(mode), cursor: "pointer",
              display: "flex", alignItems: "center",
            }}>
              <ChevronDown style={{ width: 11, height: 11 }} />
            </button>
          </div>
          )}

          {!generating && menuOpen && (
            <div ref={modeMenu.refs.setFloating} style={{
              ...modeMenu.floatingStyles, zIndex: 10,
              background: "var(--card)", border: "1px solid var(--border)", borderRadius: 6,
              boxShadow: "0 4px 16px oklch(0 0 0 / 0.4)", overflow: "hidden", minWidth: 120,
            }}>
              {MODE_ORDER.map((id) => {
                const meta = AGENT_META[id];
                const Icon = meta.icon;
                return (
                  <button key={id} onClick={() => { setMode(id); setMenuOpen(false); }} title={meta.title} style={{
                    display: "flex", alignItems: "center", gap: 6, width: "100%",
                    padding: "6px 10px", background: id === mode ? modeColor(id, 0.12) : "transparent", border: "none",
                    color: modeColor(id), fontSize: 11, cursor: "pointer", textAlign: "left",
                  }}>
                    {Icon ? <Icon style={{ width: 11, height: 11 }} /> : <span style={{ width: 11 }} />}
                    {meta.label}
                  </button>
                );
              })}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
