"use client";

import { memo, useCallback, useMemo, useState } from "react";
import { useVirtualizer } from "@tanstack/react-virtual";
import { Sparkles, StickyNote, Trash2, MoveUp, MoveDown, MessageSquare, LogIn, LogOut } from "lucide-react";
import { type WireTick } from "@/lib/serverCacheStore";
import { tickPreview, tickField, characterDisplayName } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";

// A file hint (tickField(tick, "file")) rendered as a clickable jump to that
// file's own view — the one thing in a tick row that names something else
// concrete and navigable.
function FileLink({ file, onSelectFile }: { file: string; onSelectFile: (path: string) => void }) {
  return (
    <span
      onClick={(e) => { e.stopPropagation(); onSelectFile(file); }}
      title={`Open ${file}`}
      style={{ cursor: "pointer", textDecoration: "underline", textDecorationStyle: "dotted", textUnderlineOffset: 2 }}
      onMouseDown={(e) => e.stopPropagation()}
    >
      {file}
    </span>
  );
}

// ── Move button ───────────────────────────────────────────────────────────────

function MoveButton({ disabled, onClick, title, danger, children }: {
  disabled: boolean; onClick: () => void; title: string; danger?: boolean; children: React.ReactNode;
}) {
  return (
    <button
      onClick={disabled ? undefined : onClick}
      title={title}
      style={{
        width: 20, height: 20, display: "flex", alignItems: "center", justifyContent: "center",
        background: "none", border: "none", cursor: disabled ? "default" : "pointer",
        color: disabled ? "var(--text-disabled)" : danger ? "var(--rose)" : "var(--text-ghost)",
        borderRadius: 3, padding: 0,
        opacity: disabled ? 0.35 : 1,
        transition: "color 0.12s, opacity 0.12s",
      }}
    >
      {children}
    </button>
  );
}

// ── Tick row ──────────────────────────────────────────────────────────────────

// memo()'d because the list can run into the hundreds — with 400+ rows,
// only the handful whose own props actually changed (typically just the
// hover-related pair) should re-render, not the whole list. That only pays
// off because the parent passes stable per-row primitives (ids, not fresh
// closures) — see TicksView's onMoveUp/onMoveDown/onDelete below.
const TickRow = memo(function TickRow({
  tick, isFirst, isLast, prevTick, nextTick, afterNextTickId, related,
  onAddNote, onMoveUp, onMoveDown, onDelete, onHoverChange, onSelectFile,
}: {
  tick: WireTick;
  isFirst: boolean;
  isLast: boolean;
  prevTick: WireTick | undefined;
  nextTick: WireTick | undefined;
  afterNextTickId: string | undefined;
  // Whether the tick currently hovered elsewhere in the list references
  // this row, or is referenced by it — the "highlight referenced ticks on
  // hover" relation, computed once at the list level (see TicksView) since
  // it depends on every other row, not just this one.
  related: boolean;
  onAddNote: (refTickId: string, text: string) => void;
  onMoveUp: (tickId: string, afterTickId?: string) => void;
  onMoveDown: (tickId: string, afterTickId?: string) => void;
  onDelete: (tickId: string) => void;
  onHoverChange: (tickId: string | null) => void;
  onSelectFile: (path: string) => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [addingNote, setAddingNote] = useState(false);
  const [noteText, setNoteText] = useState("");

  const refsOf = (t: WireTick | undefined): string[] => t?.refs ?? [];

  // Ordering invariant: refs must be older (below); things that ref you must be newer (above).
  const canMoveUp   = !isFirst && !refsOf(prevTick).includes(tick.tickId);
  const canMoveDown = !isLast  && !refsOf(nextTick).includes(tick.tickId) && (!nextTick || !refsOf(tick).includes(nextTick.tickId));

  const tickContent = () => {
    if (tick.kind === "note") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <StickyNote style={{ width: 11, height: 11, color: "oklch(0.55 0.15 240)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPreview(tick.message)}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          → {(tick.refs?.[0] ?? "").slice(0, 12)}
        </span>
      </div>
    );
    if (tick.kind === "presence") {
      const character = tickField(tick, "character");
      const entering = tickField(tick, "event") === "enter";
      const Icon = entering ? LogIn : LogOut;
      return (
        <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
          <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
          <Icon style={{ width: 11, height: 11, color: entering ? "oklch(0.65 0.15 165)" : "oklch(0.65 0.15 25)", flexShrink: 0 }} />
          <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
            {character ? characterDisplayName(character) : "unknown"} {entering ? "enters the scene" : "leaves the scene"}
          </span>
        </div>
      );
    }
    if (tick.kind === "prompt") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <Sparkles style={{ width: 11, height: 11, color: "var(--amber)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--amber)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPreview(tick.message)}
        </span>
        {tickField(tick, "file") && (
          <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
            <FileLink file={tickField(tick, "file")!} onSelectFile={onSelectFile} />
          </span>
        )}
      </div>
    );
    return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <span style={{ fontSize: 13, color: "var(--text-secondary)", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {tickPreview(tick.message)}
        </span>
        {tickField(tick, "file") && (
          <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", transition: "color 0.15s", flexShrink: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", minWidth: 0 }}>
            <FileLink file={tickField(tick, "file")!} onSelectFile={onSelectFile} />
          </span>
        )}
        {tick.refs.length > 0 && (
          <span style={{ fontSize: 9, color: "var(--text-ghost)", flexShrink: 0 }}>
            {tick.refs.length} ref{tick.refs.length > 1 ? "s" : ""}
          </span>
        )}
      </div>
    );
  };

  return (
    <div
      onMouseEnter={() => { setHovered(true); onHoverChange(tick.tickId); }}
      onMouseLeave={() => { setHovered(false); onHoverChange(null); }}
      style={{
        borderBottom: "1px solid var(--border-subtle)", marginBottom: 2,
        background: related ? "var(--amber-wash)" : "transparent",
        boxShadow: related ? "inset 2px 0 0 var(--amber)" : "none",
        transition: "background 0.12s, box-shadow 0.12s",
      }}
    >
      <div style={{ position: "relative", display: "flex", alignItems: "center", padding: "5px 0", paddingRight: hovered ? 88 : 0, transition: "padding-right 0.12s" }}>
        {tickContent()}
        <div style={{ position: "absolute", right: 0, display: "flex", gap: 2, opacity: hovered ? 1 : 0, transition: "opacity 0.12s", pointerEvents: hovered ? "auto" : "none", background: "var(--surface-deep)", paddingLeft: 4 }}>
          <MoveButton disabled={!canMoveUp} onClick={() => onMoveUp(tick.tickId, prevTick?.tickId)} title="Move up"><MoveUp style={{ width: 10, height: 10 }} /></MoveButton>
          <MoveButton disabled={!canMoveDown} onClick={() => onMoveDown(tick.tickId, afterNextTickId)} title="Move down"><MoveDown style={{ width: 10, height: 10 }} /></MoveButton>
          {tick.kind === "atom" && (
            <MoveButton disabled={false} onClick={() => setAddingNote((v) => !v)} title="Add note"><MessageSquare style={{ width: 10, height: 10 }} /></MoveButton>
          )}
          <MoveButton disabled={false} onClick={() => onDelete(tick.tickId)} title="Delete tick" danger><Trash2 style={{ width: 10, height: 10 }} /></MoveButton>
        </div>
      </div>

      {addingNote && (
        <div style={{ display: "flex", gap: 6, padding: "4px 0 6px 20px", alignItems: "center" }}>
          <input
            autoFocus value={noteText}
            onChange={(e) => setNoteText(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && noteText.trim()) { onAddNote(tick.tickId, noteText.trim()); setNoteText(""); setAddingNote(false); }
              if (e.key === "Escape") { setAddingNote(false); setNoteText(""); }
            }}
            placeholder="Add annotation…"
            style={{ flex: 1, fontSize: 11, padding: "3px 7px", background: "var(--card)", border: "1px solid var(--border-subtle)", borderRadius: 4, color: "var(--foreground)", outline: "none" }}
          />
          <button
            onClick={() => { if (noteText.trim()) { onAddNote(tick.tickId, noteText.trim()); setNoteText(""); setAddingNote(false); } }}
            style={{ fontSize: 10, padding: "3px 8px", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", borderRadius: 4, color: "var(--amber)", cursor: "pointer" }}
          >Add</button>
        </div>
      )}
    </div>
  );
});

// ── Ticks view ────────────────────────────────────────────────────────────────

export function TicksView({
  activeBranch, ticks,
  onAddNote, onMoveTick, onDeleteTick, onSelectFile,
}: {
  activeBranch: string | null;
  ticks: WireTick[];
  onAddNote: (refTickId: string, text: string) => void;
  onMoveTick: (tickId: string, afterTickId?: string) => void;
  onDeleteTick: (tickId: string) => void;
  onSelectFile: (path: string) => void;
}) {
  const contentKey = ticks.length > 0 ? `${ticks.length}:${ticks[0].tickId}` : 0;
  const scrollRef = useAutoScroll<HTMLDivElement>(contentKey, activeBranch, "start");

  const [hoveredTickId, setHoveredTickId] = useState<string | null>(null);

  // Both directions: what the hovered tick points at, and whatever points
  // back at it — a hovered note's target and a hovered atom's annotations
  // are equally "referenced" from the reader's point of view.
  const relatedIds = useMemo(() => {
    if (!hoveredTickId) return new Set<string>();
    const hovered = ticks.find((t) => t.tickId === hoveredTickId);
    const ids = new Set<string>(hovered?.refs ?? []);
    for (const t of ticks) if (t.refs.includes(hoveredTickId)) ids.add(t.tickId);
    return ids;
  }, [hoveredTickId, ticks]);

  // A branch's chain routinely runs into the hundreds — mounting every row's
  // DOM subtree up front is exactly what made *opening* this tab noticeably
  // slow (scrolling was already fine, since that's just compositing already-
  // mounted layers). Windowing keeps the mount cost independent of chain
  // length: only the rows actually in view (plus overscan) ever exist as
  // real DOM nodes. Row height isn't fixed (wrapping text, the add-note
  // input toggling) so this measures each row after layout rather than
  // assuming a constant.
  const virtualizer = useVirtualizer({
    count: ticks.length,
    getScrollElement: () => scrollRef.current,
    estimateSize: () => 34,
    overscan: 12,
  });

  // Stable, identity-preserving handlers passed to every row — TickRow is
  // memo()'d specifically so that most of 400+ rows can skip re-rendering
  // when only the hovered pair changes, which only works if the callback
  // props aren't a fresh closure every render. These just forward the
  // tickId (and, for move, the target neighbor's id) to the already-stable
  // top-level actions.
  const handleMoveUp   = useCallback((tickId: string, afterTickId?: string) => onMoveTick(tickId, afterTickId), [onMoveTick]);
  const handleMoveDown = useCallback((tickId: string, afterTickId?: string) => onMoveTick(tickId, afterTickId), [onMoveTick]);
  const handleDelete   = useCallback((tickId: string) => onDeleteTick(tickId), [onDeleteTick]);

  if (!activeBranch) return (
    <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
      Select a branch to view ticks
    </div>
  );

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      <div style={{ flexShrink: 0, padding: "5px 16px", borderBottom: "1px solid var(--border-subtle)", display: "flex", alignItems: "center" }}>
        <span style={{ fontSize: 10, color: "var(--text-ghost)", marginLeft: "auto" }}>
          {ticks.length} tick{ticks.length !== 1 ? "s" : ""}
        </span>
      </div>

      {ticks.length === 0 ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          No ticks yet
        </div>
      ) : (
        <div ref={scrollRef} style={{ flex: 1, overflow: "auto" }}>
          <div style={{ maxWidth: 1100, margin: "0 auto", padding: "16px 32px 48px", position: "relative", height: virtualizer.getTotalSize() }}>
            {virtualizer.getVirtualItems().map((vRow) => {
              const i = vRow.index;
              const tick      = ticks[i];
              const isFirst   = i === 0;
              const isLast    = i === ticks.length - 1;
              const prevTick  = i > 0 ? ticks[i - 1] : undefined;
              const nextTick  = i + 1 < ticks.length ? ticks[i + 1] : undefined;
              // After a down-swap, tid's new older neighbor is whatever
              // currently sits two rows below it (nextTick's own nextTick) —
              // undefined (root) once that runs off the end of the list.
              const afterNextTickId = i + 2 < ticks.length ? ticks[i + 2].tickId : undefined;
              return (
                <div
                  key={tick.tickId}
                  ref={virtualizer.measureElement}
                  data-index={i}
                  style={{ position: "absolute", top: 0, left: 0, right: 0, transform: `translateY(${vRow.start}px)` }}
                >
                  <TickRow
                    tick={tick}
                    isFirst={isFirst} isLast={isLast} prevTick={prevTick} nextTick={nextTick}
                    afterNextTickId={afterNextTickId}
                    related={relatedIds.has(tick.tickId)}
                    onAddNote={onAddNote}
                    onMoveUp={handleMoveUp}
                    onMoveDown={handleMoveDown}
                    onDelete={handleDelete}
                    onHoverChange={setHoveredTickId}
                    onSelectFile={onSelectFile}
                  />
                </div>
              );
            })}
          </div>
        </div>
      )}
    </div>
  );
}
