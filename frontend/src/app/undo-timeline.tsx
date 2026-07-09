"use client";

// Top-bar undo timeline: a shared, live, session-wide history of every real
// write the story has ever made (Storyteller.Core.Undo), rendered as a row
// of dots — click one to jump there (undo *or* redo, since resetToUndo is
// symmetric; see sidebar.actions.ts's resetToUndo).
//
// The server-side log is flat, append-only, and carries no notion of
// "current" at all (see lib/ws.ts's WireUndoEntry doc) — a jump doesn't
// touch it. So "which dot is active" and "what would redo do" are both
// purely local state, kept as simple as the actual rule:
//
//   - the active dot defaults to the last (newest) one.
//   - clicking an earlier dot jumps there, makes *that* one active, and
//     stashes every dot that was ahead of it (the ones between it and the
//     old newest) into a list rendered right after the new active dot.
//   - the moment a real write lands (the entry list grows), the stash and
//     the override are both dropped — the active dot snaps back to being
//     "the last one" again, which is now the fresh write, same as if
//     nothing had ever been undone. The stashed dots aren't lost — they're
//     still real entries in the flat list, they just go back to rendering
//     in their own chronological slot instead of the trailing group.
//
// No separate "redo" concept needed beyond that: the stash *is* the redo
// target (its own last entry is exactly where the jump came from), and it
// naturally answers "can I still get back" by simply being non-empty.
//
// Right-aligned by default, scrolled to the active dot otherwise: the log
// can run to hundreds of entries (this is a real, ever-growing history,
// not a capped ring buffer), far more than the top bar has room for, and
// the active dot isn't always the last one once something's been undone.
// Both edges fade out (mask-image) instead of hard-cutting a dot mid-shape.

import { useEffect, useRef, useState } from "react";
import { Undo2, Redo2 } from "lucide-react";
import { useServerCache } from "@/lib/serverCacheStore";
import { resetToUndo } from "./sidebar.actions";
import type { WireUndoEntry } from "@/lib/ws";

const DOT = 7;
// Non-current dots render ~20% smaller than the current one — the size
// difference alone helps "current" read as the one that matters, on top of
// the fill/glow. The wrapper span below stays fixed at DOT so shrinking the
// button doesn't disturb the row's spacing/line-up math (DOT_GAP, the
// connecting-line width) — only the visible dot shrinks, centered in its slot.
const DOT_SMALL = 5.6;
const DOT_GAP = 18;

function Dot({ filled, faded, pulse, title, onClick }: {
  filled?: boolean;
  faded?: boolean;
  pulse?: boolean;
  title: string;
  onClick: () => void;
}) {
  const size = filled ? DOT : DOT_SMALL;
  return (
    <span style={{
      position: "relative", display: "inline-flex", alignItems: "center", justifyContent: "center",
      width: DOT, height: DOT, flexShrink: 0,
    }}>
      {pulse && (
        <span
          className="animate-ping"
          style={{
            position: "absolute", inset: -4, borderRadius: "50%",
            background: "var(--amber)", opacity: 0.55,
          }}
        />
      )}
      <button
        onClick={onClick}
        title={title}
        style={{
          width: size, height: size, borderRadius: "50%", padding: 0, cursor: "pointer",
          border: filled ? "none" : "1.5px solid var(--text-dim)",
          background: filled ? "var(--amber)" : "transparent",
          opacity: faded ? 0.45 : 1,
          boxShadow: filled ? "0 0 7px oklch(0.78 0.10 65 / 65%)" : "none",
        }}
      />
    </span>
  );
}

export function UndoTimeline() {
  const entries = useServerCache((s) => s.undoEntries);

  // The override + stash pair described in the module doc — 'null' means
  // "no undo pending, active is just the last entry." Set by clicking any
  // dot that isn't already the last one; cleared the instant the entry
  // count grows past what it was when the override was made (a real write
  // happened — see the effect below), regardless of what caused it (this
  // client's own edit, another client's, a background agent's).
  const [override, setOverride] = useState<{ activeId: string; stash: WireUndoEntry[]; countAtJump: number } | null>(null);

  useEffect(() => {
    if (override && entries.length > override.countAtJump) setOverride(null);
  }, [entries.length, override]);

  const activeId = override?.activeId ?? entries[entries.length - 1]?.id;
  const stash = override?.stash ?? [];
  // Only the non-stashed entries render in the main row — the stashed ones
  // (still just ordinary entries, unmodified) get their own trailing group
  // right after the active dot instead of their natural chronological slot.
  const stashedIds = new Set(stash.map((e) => e.id));
  const main = entries.filter((e) => !stashedIds.has(e.id));

  const [pulsing, setPulsing] = useState(false);
  const prevIdRef = useRef<string | undefined>(undefined);
  const scrollRef = useRef<HTMLDivElement>(null);
  const activeDotRef = useRef<HTMLDivElement>(null);

  // Scroll the active dot into view (centered, not flush against the
  // fade-out edge — see the manual scrollLeft math) and pulse it whenever
  // it changes, including on mount so a reload right after an undo still
  // shows the right dot.
  useEffect(() => {
    const prevId = prevIdRef.current;
    prevIdRef.current = activeId;
    if (activeId === undefined) return;

    const raf = requestAnimationFrame(() => {
      const container = scrollRef.current;
      const dot = activeDotRef.current;
      if (!container || !dot) return;
      const target = dot.offsetLeft + dot.offsetWidth / 2 - container.clientWidth / 2;
      const max = Math.max(0, container.scrollWidth - container.clientWidth);
      container.scrollTo({ left: Math.max(0, Math.min(target, max)), behavior: prevId === undefined ? "auto" : "smooth" });
    });

    if (prevId === undefined || prevId === activeId) return () => cancelAnimationFrame(raf);
    setPulsing(true);
    const t = setTimeout(() => setPulsing(false), 700);
    return () => { cancelAnimationFrame(raf); clearTimeout(t); };
  }, [activeId]);

  if (entries.length === 0) return null;

  function jump(entry: WireUndoEntry) {
    const idx = entries.findIndex((e) => e.id === entry.id);
    setOverride({ activeId: entry.id, stash: entries.slice(idx + 1), countAtJump: entries.length });
    resetToUndo(entry.id);
  }

  const mainActiveIdx = main.findIndex((e) => e.id === activeId);
  const undoTarget = mainActiveIdx > 0 ? main[mainActiveIdx - 1] : undefined;
  const redoTarget = stash.length > 0 ? stash[stash.length - 1] : undefined;

  return (
    <div style={{ display: "flex", alignItems: "center", flex: 1, minWidth: 0 }}>
      <div style={{ display: "flex", alignItems: "center", gap: 2, flexShrink: 0, marginLeft: 8 }}>
        <button
          onClick={() => undoTarget && jump(undoTarget)}
          disabled={!undoTarget}
          title={undoTarget ? `Undo — back to ${new Date(undoTarget.time).toLocaleString()}` : "Nothing to undo"}
          style={{
            width: 20, height: 20, display: "flex", alignItems: "center", justifyContent: "center",
            border: "none", background: "transparent", borderRadius: 4,
            cursor: undoTarget ? "pointer" : "default",
            color: undoTarget ? "var(--text-dim)" : "var(--text-ghost)", opacity: undoTarget ? 1 : 0.4,
          }}
        >
          <Undo2 style={{ width: 12, height: 12 }} />
        </button>
        <button
          onClick={() => redoTarget && jump(redoTarget)}
          disabled={!redoTarget}
          title={redoTarget ? `Redo — forward to ${new Date(redoTarget.time).toLocaleString()}` : "Nothing to redo"}
          style={{
            width: 20, height: 20, display: "flex", alignItems: "center", justifyContent: "center",
            border: "none", background: "transparent", borderRadius: 4,
            cursor: redoTarget ? "pointer" : "default",
            color: redoTarget ? "var(--text-dim)" : "var(--text-ghost)", opacity: redoTarget ? 1 : 0.4,
          }}
        >
          <Redo2 style={{ width: 12, height: 12 }} />
        </button>
      </div>
      <div
        ref={scrollRef}
        style={{
          display: "flex", alignItems: "center", padding: "0 14px",
          flex: 1, minWidth: 0, overflowX: "auto", overflowY: "hidden",
          scrollbarWidth: "none",
          WebkitMaskImage: "linear-gradient(to right, transparent, black 20px, black calc(100% - 20px), transparent)",
          maskImage: "linear-gradient(to right, transparent, black 20px, black calc(100% - 20px), transparent)",
        }}
      >
        <div style={{ display: "flex", alignItems: "center", gap: DOT_GAP }}>
          {main.flatMap((entry, i) => {
            const isActive = entry.id === activeId;
            const dots = [
              <div key={entry.id} ref={isActive ? activeDotRef : undefined} style={{ position: "relative", display: "flex", alignItems: "center" }}>
                {i > 0 && (
                  <div style={{ position: "absolute", right: DOT, width: DOT_GAP - DOT, height: 1, background: "var(--border-subtle)" }} />
                )}
                <Dot
                  filled={isActive}
                  pulse={isActive && pulsing}
                  title={new Date(entry.time).toLocaleString()}
                  onClick={() => jump(entry)}
                />
              </div>,
            ];
            // The stash — dots that were "ahead" at the moment of the jump
            // that made 'entry' active — renders as a trailing run right
            // after it, still real, still clickable (jumping to one just
            // resumes further along the same abandoned run).
            if (isActive) {
              stash.forEach((s) => dots.push(
                <div key={s.id} style={{ position: "relative", display: "flex", alignItems: "center" }}>
                  <div style={{ position: "absolute", right: DOT, width: DOT_GAP - DOT, height: 1, background: "var(--border-subtle)", opacity: 0.5 }} />
                  <Dot
                    faded
                    title={`Undone — ${new Date(s.time).toLocaleString()}. Click to jump back here.`}
                    onClick={() => jump(s)}
                  />
                </div>,
              ));
            }
            return dots;
          })}
        </div>
      </div>
    </div>
  );
}
