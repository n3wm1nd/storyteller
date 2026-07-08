"use client";

// Top-bar undo timeline: a shared, live, session-wide history of every
// point the story has passed through (Storyteller.Core.Undo), rendered as a
// row of dots — click one to jump there (undo *or* redo, since resetToUndo
// is symmetric; see sidebar.actions.ts's resetToUndo).
//
// The server-side log is a plain, ever-growing chain (see lib/ws.ts's
// WireUndoEntry doc) — there is no persisted tree. "Abandoned" branches are
// a client-side rendering choice, not server state: a later entry whose
// snapshot exactly repeats an earlier one's (server-flagged via
// 'revertsTo') means every entry strictly between them was left behind when
// that jump happened. This groups that run into a small, collapsed stub —
// deliberately not spelled out entry-by-entry, since a single clickable
// "go back down that path" dot is enough for a user to explore it further
// if they ever want to (they'll land back on the main line at that point,
// with the same grouping applied again from there).
//
// Right-aligned, not centered: the log can run to hundreds of entries (this
// is a real, ever-growing history, not a capped ring buffer), far more than
// the top bar has room for. Right-aligning means the newest/current entry
// — the one anyone actually cares about at a glance — is always the one
// still on screen once older entries clip off the left edge, rather than
// an arbitrary window landing wherever centering happens to put it. The
// clipped edge fades out (mask-image) instead of hard-cutting a dot mid-shape.
//
// Click-the-current-dot-again toggles back to wherever the last jump came
// from — a cheap "peek and return" gesture that needs no server-side tree
// support: it's just local memory of the two ends of the last jump WE made,
// invalidated the moment 'currentId' drifts away from that pair for any
// other reason (someone edits from the position we jumped to, another
// client moves the shared log, etc — seeing an unrecognized currentId is
// exactly the signal that the remembered pair is stale). A real "explore
// every branch" UI would need the log to carry actual tree structure
// server-side; this is deliberately not that.
//
// No color/shape-by-agent-type yet — WireUndoEntry carries no metadata
// about what kind of change produced an entry (agent vs. manual edit vs.
// import), so every dot looks the same today. Would need the undo log
// itself to record more than a ref snapshot to support that.

import { useEffect, useRef, useState } from "react";
import { useServerCache } from "@/lib/serverCacheStore";
import { resetToUndo } from "./sidebar.actions";
import type { WireUndoEntry } from "@/lib/ws";

interface TimelineGroups {
  main: WireUndoEntry[];
  // Keyed by the branch point's own entry id (the earlier entry a later one
  // reverted to) — the abandoned run's own last entry is the clickable tip.
  abandoned: Map<string, WireUndoEntry>;
}

// Walk chronologically, tracking which earlier entries are still "live"
// (on the eventual main line) vs. left behind by a later revert. A revert
// at index i back to index j buries every entry in (j, i) that isn't
// already buried — the entries between them never lead anywhere further
// forward, since the only continuation from that point on is entry i itself.
function groupTimeline(entries: WireUndoEntry[]): TimelineGroups {
  const idToIndex = new Map(entries.map((e, i) => [e.id, i]));
  const buried = new Array(entries.length).fill(false);
  const abandoned = new Map<string, WireUndoEntry>();

  entries.forEach((entry, i) => {
    if (entry.revertsTo === undefined || entry.revertsTo === null) return;
    const j = idToIndex.get(entry.revertsTo);
    if (j === undefined) return;
    for (let k = j + 1; k < i; k++) {
      if (!buried[k]) {
        buried[k] = true;
      }
    }
    // The abandoned run's tip is the last not-otherwise-buried entry right
    // before the revert — i.e. whatever the timeline's "current" position
    // was the instant before this jump.
    for (let k = i - 1; k > j; k--) {
      if (!buried[k] || k === i - 1) {
        abandoned.set(entries[j].id, entries[k]);
        break;
      }
    }
  });

  const main = entries.filter((_, i) => !buried[i]);
  return { main, abandoned };
}

const DOT = 7;
// Non-current dots render ~20% smaller than the current one — the size
// difference alone helps "current" read as the one that matters, on top of
// the fill/glow. The wrapper span below stays fixed at DOT so shrinking the
// button doesn't disturb the row's spacing/line-up math (DOT_GAP, the
// connecting-line width) — only the visible dot shrinks, centered in its slot.
const DOT_SMALL = 5.6;
const DOT_GAP = 18;

function Dot({ filled, faded, pulse, dim, title, onClick }: {
  filled?: boolean;
  faded?: boolean;
  pulse?: boolean;
  dim?: boolean;
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
          opacity: faded ? 0.45 : dim ? 0 : 1,
          boxShadow: filled ? "0 0 7px oklch(0.78 0.10 65 / 65%)" : "none",
          // Delay matches the row's own slide transition (260ms, see
          // UndoTimeline's 'sliding' transform) so the dot only starts
          // fading in once the bar has actually finished moving, not
          // partway through it.
          transition: "opacity 200ms ease 260ms",
        }}
      />
    </span>
  );
}

// The two ends of the last jump this client made — enough to support
// "click the current dot again to bounce back" without any server-side
// tree. See the module doc for why this is intentionally not more than that.
interface TogglePair { a: string; b: string }

export function UndoTimeline() {
  const entries = useServerCache((s) => s.undoEntries);
  const { main, abandoned } = groupTimeline(entries);
  const currentId = main[main.length - 1]?.id;

  const [toggle, setToggle] = useState<TogglePair | null>(null);
  // Exactly one of these is ever active for a given change — never both, so
  // the ping ring never overlaps the new dot's fade-in (that combination
  // read as a smear rather than two distinct beats). A plain append (an
  // edit, or a jump that lands past everything seen so far) gets the
  // slide-then-fade; anything else that moves 'currentId' (a backward jump)
  // gets the ping instead.
  const [pulsing, setPulsing] = useState(false);
  const [sliding, setSliding] = useState(false);
  const prevIdRef = useRef<string | undefined>(undefined);
  const prevMainIdsRef = useRef<string[]>([]);

  useEffect(() => {
    const ids = main.map((e) => e.id);
    const prevIds = prevMainIdsRef.current;
    const prevId = prevIdRef.current;
    prevMainIdsRef.current = ids;
    prevIdRef.current = currentId;

    if (currentId === undefined || prevId === undefined || prevId === currentId) return;

    // A currentId change we didn't just cause ourselves (e.g. another
    // client edited, or someone reset from elsewhere) makes any remembered
    // toggle pair stale — see the module doc.
    setToggle((t) => (t && (currentId === t.a || currentId === t.b) ? t : null));

    const isPlainAppend =
      prevIds.length > 0 && ids.length === prevIds.length + 1 && prevIds.every((id, i) => id === ids[i]);

    if (isPlainAppend) {
      setSliding(true);
      const raf = requestAnimationFrame(() => setSliding(false));
      return () => cancelAnimationFrame(raf);
    }

    setPulsing(true);
    const t = setTimeout(() => setPulsing(false), 700);
    return () => clearTimeout(t);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [currentId, main.length]);

  if (entries.length === 0) return null;

  function jump(targetId: string) {
    if (toggle && currentId === targetId && (targetId === toggle.a || targetId === toggle.b)) {
      const other = targetId === toggle.a ? toggle.b : toggle.a;
      setToggle({ a: targetId, b: other });
      resetToUndo(other);
    } else {
      if (currentId) setToggle({ a: currentId, b: targetId });
      resetToUndo(targetId);
    }
  }

  return (
    <div style={{
      display: "flex", alignItems: "center", padding: "0 14px",
      flex: 1, justifyContent: "flex-end", minWidth: 0, overflow: "hidden",
      WebkitMaskImage: "linear-gradient(to right, transparent, black 28px)",
      maskImage: "linear-gradient(to right, transparent, black 28px)",
    }}>
      <div style={{
        display: "flex", alignItems: "center", gap: DOT_GAP,
        transform: sliding ? `translateX(${DOT_GAP + DOT}px)` : "translateX(0)",
        transition: sliding ? "none" : "transform 260ms ease",
      }}>
        {main.map((entry, i) => {
          const stub = abandoned.get(entry.id);
          const isCurrent = entry.id === currentId;
          const isNewTail = sliding && i === main.length - 1;
          return (
            <div key={entry.id} style={{ position: "relative", display: "flex", alignItems: "center" }}>
              {i > 0 && (
                <div style={{ position: "absolute", right: DOT, width: DOT_GAP - DOT, height: 1, background: "var(--border-subtle)" }} />
              )}
              {stub && (
                <div style={{ position: "absolute", bottom: DOT + 4, left: DOT / 2 - 0.75, display: "flex", flexDirection: "column", alignItems: "center", gap: 3 }}>
                  <Dot
                    faded
                    title={`Abandoned — ${new Date(stub.time).toLocaleTimeString()}. Click to go back down this path.`}
                    onClick={() => jump(stub.id)}
                  />
                  <div style={{ width: 1, height: 6, background: "var(--border-subtle)" }} />
                </div>
              )}
              <Dot
                filled={isCurrent}
                pulse={isCurrent && pulsing}
                dim={isNewTail}
                title={new Date(entry.time).toLocaleString()}
                onClick={() => jump(entry.id)}
              />
            </div>
          );
        })}
      </div>
    </div>
  );
}
