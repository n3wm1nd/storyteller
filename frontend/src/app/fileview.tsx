"use client";

import { memo, useEffect, useLayoutEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import { ChevronDown, ChevronUp, History, Sparkles, Wrench, RefreshCw, EyeOff } from "lucide-react";
import { StickyNote } from "lucide-react";
import { useEditor, EditorContent } from "@tiptap/react";
import { StarterKit } from "@tiptap/starter-kit";
import { Markdown } from "tiptap-markdown";
import { type WireTick } from "@/lib/serverCacheStore";
import { type AnnotationMode, characterDisplayName, tailLeadTicks } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";
import { parseCommand } from "@/lib/commands";
import { useCommandAutocomplete, CommandSuggestionPopup } from "./command-autocomplete";
import { branchFileUrl, saveRawFile } from "@/lib/ws";

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

const AtomBlock = memo(function AtomBlock({ atom, isLast, inContext, swipeCount, onEdit, onToggleContext, onCycleSwipe, onHoverAtom, onHoverEnd, compact }: {
  atom: WireTick;
  isLast: boolean;
  inContext: boolean;
  // How many alternates (see Storyteller.Common.Swipe) sit in this atom's
  // own carousel — 0 hides the cycle control entirely. Display-only in
  // this pass: there's no way yet to *generate* a fresh alternative for a
  // plain prose atom (unlike chat's Regenerate), only to cycle through
  // ones that already exist.
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
        background: inContext ? "oklch(0.78 0.10 65 / 0.04)" : "transparent",
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
        <EyeOff
          title="Hidden from an agent's context"
          style={{ position: "absolute", top: 2, left: compact ? -2 : 12, width: 11, height: 11, color: "var(--text-ghost)" }}
        />
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
              border: "1px solid oklch(0.78 0.10 65 / 0.4)",
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
              background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.4)",
              borderRadius: 3, color: "var(--text-secondary)", fontSize: 11, padding: "2px 8px", cursor: "pointer",
            }}>Save</button>
          </div>
        </div>
      ) : (
        <div
          onDoubleClick={startEdit}
          onClick={(e) => { if (e.ctrlKey || e.metaKey) { e.preventDefault(); onToggleContext(atom.tickId); } }}
          style={compact ? { cursor: "default", outline: inContext ? "1px solid oklch(0.78 0.10 65 / 0.4)" : "none", outlineOffset: 2, borderRadius: 3 } : undefined}
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

const AnnotationCard = memo(function AnnotationCard({ tick, inContext, onToggleContext }: {
  tick: WireTick;
  inContext: boolean;
  onToggleContext: (tickId: string) => void;
}) {
  const [expanded, setExpanded] = useState(false);

  const isNote   = tick.kind === "note";
  const isPrompt = tick.kind === "prompt";
  if (!isNote && !isPrompt) return null;

  const accentColor = isNote ? "oklch(0.55 0.15 240)" : "var(--amber)";
  const bgColor     = isNote ? "oklch(0.22 0.01 240 / 0.6)" : "oklch(0.78 0.10 65 / 0.08)";
  const borderColor = isNote ? "oklch(0.35 0.04 240 / 0.4)" : "oklch(0.78 0.10 65 / 0.25)";
  const Icon        = isNote ? StickyNote : Sparkles;
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
        <span style={{ fontSize: 12, color: isNote ? "var(--text-muted)" : "var(--amber)", fontStyle: "italic", lineHeight: 1.5, flex: 1, opacity: expanded ? 1 : 0.85 }}>
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

const AnnotationDots = memo(function AnnotationDots({ annotations, contextAnnotations, onToggleContext }: {
  annotations: WireTick[];
  contextAnnotations: Set<string>;
  onToggleContext: (tickId: string) => void;
}) {
  const [expandedId, setExpandedId] = useState<string | null>(null);

  function dotColor(kind: string): string {
    if (kind === "note")   return "oklch(0.55 0.15 240)";
    if (kind === "prompt") return "var(--amber)";
    return "var(--text-dim)";
  }

  return (
    <div style={{ margin: "2px 0 10px 12px" }}>
      <div style={{ display: "flex", gap: 5, alignItems: "center", flexWrap: "wrap" }}>
        {annotations.map((ann) => {
          const inCtx  = contextAnnotations.has(ann.tickId);
          const isOpen = expandedId === ann.tickId;
          const color  = dotColor(ann.kind);
          return (
            <button
              key={ann.tickId}
              title={ann.message.slice(0, 80)}
              onClick={(e) => {
                if (e.ctrlKey || e.metaKey) { onToggleContext(ann.tickId); return; }
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
  onHoverAtom, onHoverEnd, compact,
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
}) {
  const contentKey = ticks.length > 0 ? `${ticks.length}:${ticks[ticks.length - 1].tickId}` : 0;
  const scrollRef = useAutoScroll<HTMLDivElement>(contentKey, resetKey, "end");

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
  const leading: WireTick[] = [];
  let lastAtomId: string | null = null;
  for (const tick of ticks) {
    if (tick.kind === "atom") {
      lastAtomId = tick.tickId;
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
    <div style={{ position: "relative", flex: compact ? undefined : 1, display: "flex", flexDirection: "column", overflow: compact ? "visible" : "hidden" }}>
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
              <AnnotationDots annotations={leading} contextAnnotations={contextAnnotations} onToggleContext={onToggleContextAnnotation} />
            ) : (
              leading.map((ann) => (
                <AnnotationCard
                  key={ann.tickId} tick={ann}
                  inContext={contextAnnotations.has(ann.tickId)}
                  onToggleContext={onToggleContextAnnotation}
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
                  />
                  {annotationMode === "dots" && anns.length > 0 && (
                    <AnnotationDots annotations={anns} contextAnnotations={contextAnnotations} onToggleContext={onToggleContextAnnotation} />
                  )}
                  {annotationMode === "expanded" && anns.map((ann) => (
                    <AnnotationCard
                      key={ann.tickId} tick={ann}
                      inContext={contextAnnotations.has(ann.tickId)}
                      onToggleContext={onToggleContextAnnotation}
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
          onClick={save}
          disabled={!dirty || saving}
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
            border: "1px solid " + (dirty ? "oklch(0.78 0.10 65 / 0.35)" : "var(--border-subtle)"),
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

  function save() {
    if (!editor || savingRef.current) return;
    const md = editor.storage.markdown.getMarkdown();
    savingRef.current = true;
    setSaving(true);
    setError(null);
    saveRawFile(branch, path, md)
      .then(() => {
        savedContentRef.current = md;
        setSavedContent(md);
        setDirty(false);
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
          onClick={save}
          disabled={!dirty || saving}
          style={{
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
            border: "1px solid " + (dirty ? "oklch(0.78 0.10 65 / 0.35)" : "var(--border-subtle)"),
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
export function ChatPreviewStrip({ preview }: {
  preview: { text: string; thinking: string } | null;
}) {
  const containerRef = useAutoScroll<HTMLDivElement>(
    (preview?.text.length ?? 0) + (preview?.thinking.length ?? 0), preview === null, "end"
  );

  if (preview === null) return null;

  const isEmpty = preview.text.length === 0 && preview.thinking.length === 0;

  return (
    <div style={{ flexShrink: 0, borderTop: "1px solid oklch(0.78 0.10 65 / 0.25)", background: "oklch(0.16 0.02 65 / 0.5)" }}>
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
type AgentId = "write" | "fix" | "append" | "note" | "regen";

// One color per mode so the input bar's own border/pill tells you where a
// plain (non-"/command") send will land, without having to check anything
// else. 'note' reuses the blue already used for annotation ticks elsewhere
// in this file (see WireTickList); the rest are fresh hues not otherwise
// claimed in the app's palette.
const MODE_COLOR: Record<AgentId, string> = {
  write:  "0.78 0.10 65",
  fix:    "0.68 0.12 200",
  append: "0.62 0.02 60",
  note:   "0.60 0.15 240",
  regen:  "0.68 0.15 300",
};
function modeColor(id: AgentId, alpha?: number): string {
  return alpha === undefined ? `oklch(${MODE_COLOR[id]})` : `oklch(${MODE_COLOR[id]} / ${alpha})`;
}

const MODE_ORDER: AgentId[] = ["write", "fix", "append", "note", "regen"];

const AGENT_META: Record<AgentId, { label: string; title: string; icon: typeof Sparkles | null }> = {
  write:  { label: "Write",  title: "Send to writer agent",               icon: Sparkles },
  fix:    { label: "Fix",    title: "Send to fixer agent (edit targets)",  icon: Wrench },
  append: { label: "Append", title: "Append verbatim, instant",            icon: null },
  note:   { label: "Note",   title: "Attach as a note, instant",           icon: StickyNote },
  regen:  { label: "Regen",  title: "Regenerate this chapter to fit its beat sheet", icon: RefreshCw },
};

export function InputBar({ enabled, contextAtomCount, contextAnnotationCount, rebasing, onClearRebase, onClearContext, onAppend, onWrite, onFix, onNote, onRegen, onAsk }: {
  enabled: boolean;
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
  // Not a mode in AGENT_META (this doesn't edit the file) — only reachable
  // via the "/ask @character=..." command, never the mode pill/dropdown.
  onAsk:    (character: string, question: string) => void;
}) {
  const [text, setText] = useState("");
  const [height, setHeight] = useState(90);
  const [menuOpen, setMenuOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  const auto = useCommandAutocomplete(text, setText);

  const hasContext = contextAtomCount > 0 || contextAnnotationCount > 0;

  // Explicit, sticky mode — Shift+Tab (or the pill/dropdown) cycles it, and
  // it stays put until you change it again. Seeded from context at mount
  // only (selection present → Fix, since that's the common case), not kept
  // in sync afterward: once you've picked a mode, it's yours until you pick
  // a different one, not silently swapped out from under you.
  const [mode, setMode] = useState<AgentId>(() => (hasContext ? "fix" : "write"));

  function cycleMode(dir: 1 | -1) {
    setMode((m) => {
      const i = MODE_ORDER.indexOf(m);
      return MODE_ORDER[(i + dir + MODE_ORDER.length) % MODE_ORDER.length];
    });
  }

  const actionFor: Record<AgentId, (t: string) => void> = {
    write: onWrite, fix: onFix, append: onAppend, note: onNote, regen: (t) => onRegen(t, false),
  };

  // A recognized leading "/command" always wins over the currently selected
  // mode — see lib/commands.ts.
  const commandActions: Record<string, (t: string, params: Record<string, string>) => void> = {
    write: (t) => onWrite(t), fix: (t) => onFix(t), append: (t) => onAppend(t), note: (t) => onNote(t),
    regen: (t, p) => onRegen(t, p.beat !== undefined),
    ask: (t, p) => { if (p.character) onAsk(p.character, t); },
  };

  function fire() {
    const raw = text.trim();
    if (!raw) return;
    const parsed = parseCommand(raw);
    const action = parsed && commandActions[parsed.name];
    if (parsed && action) {
      if (parsed.text) action(parsed.text, parsed.params);
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
          <CommandSuggestionPopup suggestions={auto.suggestions} activeIndex={auto.activeIndex} onPick={auto.pick} />
          <textarea
            ref={auto.taRef}
            value={text}
            onChange={(e) => setText(e.target.value)}
            onSelect={auto.onSelect}
            onClick={auto.onSelect}
            onKeyDown={(e) => {
              if (auto.onKeyDown(e)) return;
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
            <button onClick={() => setMenuOpen((o) => !o)} title="Choose mode (shift-tab cycles)" style={{
              padding: "4px 5px",
              background: modeColor(mode, 0.15), border: `1px solid ${modeColor(mode, 0.4)}`,
              borderLeft: `1px solid ${modeColor(mode, 0.4)}`,
              borderRadius: "0 5px 5px 0", color: modeColor(mode), cursor: "pointer",
              display: "flex", alignItems: "center",
            }}>
              <ChevronDown style={{ width: 11, height: 11 }} />
            </button>
          </div>

          {menuOpen && (
            <div style={{
              position: "absolute", bottom: "100%", right: 0, marginBottom: 4, zIndex: 10,
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
