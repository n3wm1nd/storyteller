"use client";

import { useState } from "react";
import { Sparkles, StickyNote, Trash2, MoveUp, MoveDown, MessageSquare } from "lucide-react";
import { type WireTick } from "@/lib/store";
import { tickPayload, tickField } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";

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

function TickRow({
  tick, allTicks, isFirst, isLast, prevTick,
  onAddNote, onMoveUp, onMoveDown, onDelete,
}: {
  tick: WireTick;
  allTicks: WireTick[];
  isFirst: boolean;
  isLast: boolean;
  prevTick: WireTick | undefined;
  onAddNote: (refTickId: string, text: string) => void;
  onMoveUp: () => void;
  onMoveDown: () => void;
  onDelete: () => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [addingNote, setAddingNote] = useState(false);
  const [noteText, setNoteText] = useState("");

  const refsOf = (t: WireTick): string[] => t.refs ?? [];
  const myIdx = allTicks.findIndex((t) => t.tickId === tick.tickId);
  const above = allTicks[myIdx - 1];
  const below = allTicks[myIdx + 1];

  // Ordering invariant: refs must be older (below); things that ref you must be newer (above).
  const canMoveUp   = !isFirst && !refsOf(above).includes(tick.tickId);
  const canMoveDown = !isLast  && !refsOf(below).includes(tick.tickId) && !refsOf(tick).includes(below?.tickId);

  const tickContent = () => {
    if (tick.kind === "note") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <StickyNote style={{ width: 11, height: 11, color: "oklch(0.55 0.15 240)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPayload(tick.message)}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          → {(tick.refs?.[0] ?? "").slice(0, 12)}
        </span>
      </div>
    );
    if (tick.kind === "prompt") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <Sparkles style={{ width: 11, height: 11, color: "var(--amber)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--amber)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPayload(tick.message)}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          {tickField(tick, "file")}
        </span>
      </div>
    );
    return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>{tick.tickId.slice(0, 12)}</span>
        <span style={{ fontSize: 13, color: "var(--text-secondary)", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {tickPayload(tick.message)}
        </span>
        {tickField(tick, "file") && (
          <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", transition: "color 0.15s", flexShrink: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", minWidth: 0 }}>
            {tickField(tick, "file")}
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
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => setHovered(false)}
      style={{ borderBottom: "1px solid var(--border-subtle)", marginBottom: 2 }}
    >
      <div style={{ position: "relative", display: "flex", alignItems: "center", padding: "5px 0", paddingRight: hovered ? 88 : 0, transition: "padding-right 0.12s" }}>
        {tickContent()}
        <div style={{ position: "absolute", right: 0, display: "flex", gap: 2, opacity: hovered ? 1 : 0, transition: "opacity 0.12s", pointerEvents: hovered ? "auto" : "none", background: "var(--surface-deep)", paddingLeft: 4 }}>
          <MoveButton disabled={!canMoveUp} onClick={onMoveUp} title="Move up"><MoveUp style={{ width: 10, height: 10 }} /></MoveButton>
          <MoveButton disabled={!canMoveDown} onClick={onMoveDown} title="Move down"><MoveDown style={{ width: 10, height: 10 }} /></MoveButton>
          {tick.kind === "atom" && (
            <MoveButton disabled={false} onClick={() => setAddingNote((v) => !v)} title="Add note"><MessageSquare style={{ width: 10, height: 10 }} /></MoveButton>
          )}
          <MoveButton disabled={false} onClick={onDelete} title="Delete tick" danger><Trash2 style={{ width: 10, height: 10 }} /></MoveButton>
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
            style={{ fontSize: 10, padding: "3px 8px", background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)", borderRadius: 4, color: "var(--amber)", cursor: "pointer" }}
          >Add</button>
        </div>
      )}
    </div>
  );
}

// ── Ticks view ────────────────────────────────────────────────────────────────

export function TicksView({
  activeBranch, ticks,
  onAddNote, onMoveTick, onDeleteTick,
}: {
  activeBranch: string | null;
  ticks: WireTick[];
  onAddNote: (refTickId: string, text: string) => void;
  onMoveTick: (tickId: string, afterTickId?: string) => void;
  onDeleteTick: (tickId: string) => void;
}) {
  const contentKey = ticks.length > 0 ? `${ticks.length}:${ticks[0].tickId}` : 0;
  const scrollRef = useAutoScroll<HTMLDivElement>(contentKey, activeBranch, "start");

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
          <div style={{ maxWidth: 1100, margin: "0 auto", padding: "16px 32px 48px" }}>
            {ticks.map((tick, i) => {
              const isFirst  = i === 0;
              const isLast   = i === ticks.length - 1;
              const prevTick = i > 0 ? ticks[i - 1] : undefined;
              return (
                <TickRow
                  key={tick.tickId}
                  tick={tick} allTicks={ticks}
                  isFirst={isFirst} isLast={isLast} prevTick={prevTick}
                  onAddNote={onAddNote}
                  onMoveUp={() => onMoveTick(tick.tickId, prevTick?.tickId)}
                  onMoveDown={() => !isLast && onMoveTick(tick.tickId, i + 2 < ticks.length ? ticks[i + 2].tickId : undefined)}
                  onDelete={() => onDeleteTick(tick.tickId)}
                />
              );
            })}
          </div>
        </div>
      )}
    </div>
  );
}
