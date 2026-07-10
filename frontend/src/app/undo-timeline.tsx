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
//     in their own chronological slot instead of the trailing group —
//     'motion.div's 'layout' prop animates that repositioning for free,
//     same key and everything, no special-casing the transition.
//
// No separate "redo" concept needed beyond that: the stash *is* the redo
// target (its own last entry is exactly where the jump came from), and it
// naturally answers "can I still get back" by simply being non-empty.
//
// Animation is delegated to framer-motion rather than hand-rolled: a
// dot appearing (a fresh write) or moving (a stash entry returning to its
// normal slot, or the whole row reflowing around either) is exactly the
// FLIP-style layout animation 'layout'/'AnimatePresence' exist to handle,
// and — critically — its animations are interruptible by construction: if
// a fresh 'undo.log' push lands mid-animation (a real write, or another
// client's), framer retargets smoothly from wherever the element currently
// is instead of restarting from a stale computed start point. Several
// earlier attempts at this used plain CSS transitions/manual
// requestAnimationFrame loops instead, and kept breaking exactly there —
// racing React's own render/commit/paint timing, or getting stomped by a
// re-render arriving mid-flight, neither of which framer's motion-value
// model is vulnerable to.
//
// Scrolled to the active dot, not pinned to the newest: the log can run to
// hundreds of entries (a real, ever-growing history, not a capped ring
// buffer), far more than the top bar has room for, and the active dot isn't
// always the last one once something's been undone. Both edges fade out
// (mask-image) instead of hard-cutting a dot mid-shape.

import { useEffect, useRef, useState } from "react";
import { motion, AnimatePresence, animate } from "framer-motion";
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

// The scroll pan (see UndoTimeline's active-dot effect) plays first; a
// dot's own mount-in fade/slide is delayed to start right after, so the two
// read as one staged motion — "the bar settles, then the new dot appears"
// — instead of both firing at once.
const SCROLL_DURATION = 0.2;
const DOT_FADE_DELAY = SCROLL_DURATION;

// Every dot stays the same brownish-amber base (BASE_L/BASE_C/BASE_H, the
// timeline's existing default) — 'kind' only nudges the *hue* a fraction of
// the way toward something distinguishable, the way a photo filter tints
// rather than recolors. That reads as "the same understated dot, gently
// colored" instead of a rainbow of separately-saturated badges, which a
// couple dozen of these sitting in peripheral vision in the top bar calls
// for. TINT_HUE borrows its target hues from fileview.tsx's MODE_COLOR
// (notes lean toward the same blue used for annotation ticks, etc.) purely
// as the *direction* to nudge toward, not the destination. "atom" (an
// ordinary prose write) sits exactly at the base hue, so it doubles as the
// fallback for a tag this client hasn't been taught — a future tick kind,
// or a write whose tick didn't decode a tag at all (e.g. a binary upload,
// or a branch deletion) — a dot for one of those just reads as the plain
// base color rather than erroring or rendering blank; see lib/ws.ts's
// WireUndoEntry doc for why 'kind' is opaque at this boundary.
const BASE_L = 0.68;
const BASE_C = 0.05;
const BASE_H = 65;
const TINT_AMOUNT = 0.4;
const TINT_HUE: Record<string, number> = {
  atom:     65,   // matches MODE_COLOR.write -- i.e. no shift from base at all
  note:     240,  // matches MODE_COLOR.note / fileview's dotColor
  fixup:    200,  // matches MODE_COLOR.fix
  prompt:   300,  // matches MODE_COLOR.regen -- an agent call in flight
  swipe:    60,   // matches MODE_COLOR.append -- a neutral, mechanical op
  presence: 140,  // scene enter/leave, not prose
  root:     30,   // branch creation, structural not content
};
function kindColor(kind: string | null, alpha?: number): string {
  const targetH = (kind && TINT_HUE[kind]) ?? BASE_H;
  const h = BASE_H + (targetH - BASE_H) * TINT_AMOUNT;
  const triple = `${BASE_L} ${BASE_C} ${h.toFixed(1)}`;
  return alpha === undefined ? `oklch(${triple})` : `oklch(${triple} / ${alpha})`;
}

function Dot({ filled, faded, color, title, onClick }: {
  filled?: boolean;
  faded?: boolean;
  color?: string;
  title: string;
  onClick: () => void;
}) {
  const size = filled ? DOT : DOT_SMALL;
  const c = color ?? kindColor(null);
  return (
    <button
      onClick={onClick}
      title={title}
      className="undo-dot"
      style={{
        width: size, height: size, borderRadius: "50%", padding: 0, cursor: "pointer",
        border: filled ? "none" : `1.5px solid ${c}`,
        background: filled ? c : "transparent",
        opacity: faded ? 0.55 : filled ? 1 : 0.7,
        boxShadow: filled ? `0 0 3px ${c.replace(")", " / 40%)")}` : "none",
      }}
    />
  );
}

// A one-shot ping ring for a jump landing on an already-existing dot (as
// opposed to a fresh write's dot, which gets its own mount animation via
// 'layout'/'initial' below instead — see the module doc). Framer's own
// mount/unmount lifecycle ('AnimatePresence' below) is what plays this
// exactly once: rendered only while 'justJumpedTo' names this entry,
// cleared via 'onAnimationComplete' once framer finishes it, not a timer.
function PulseRing({ color, onDone }: { color: string; onDone: () => void }) {
  return (
    <motion.span
      initial={{ opacity: 0.5, scale: 1 }}
      animate={{ opacity: 0, scale: 2.4 }}
      transition={{ duration: 0.6, ease: "easeOut" }}
      onAnimationComplete={onDone}
      style={{
        position: "absolute", inset: -4, borderRadius: "50%",
        background: color, pointerEvents: "none",
      }}
    />
  );
}

export function UndoTimeline() {
  const entries = useServerCache((s) => s.undoEntries);

  // The override + stash pair described in the module doc — 'null' means
  // "no undo pending, active is just the last entry." Set by clicking any
  // dot that isn't already the last one; stops applying the instant a real
  // write lands (from this client or anywhere else) — detected by
  // 'tailIdAtJump' (the id of whatever was the newest entry at the moment
  // of the jump) no longer matching the array's current tail, *not* by
  // comparing lengths: the server caps how much history it sends
  // (Server.Writer.Session.Dispatch.undoLogLimit), so once a session has
  // been running long enough to hit that cap, a real write pushes an old
  // entry off the front at the same time it adds a new one at the back —
  // the length never changes, so a length-based check would (and did)
  // silently stop clearing forever the moment a session crossed that cap.
  // Identity of the tail is exactly the fact that's actually true
  // regardless of how the array's front edge is being trimmed.
  //
  // Whether it still applies is computed fresh every render ('effective'
  // below) rather than cleared via a useEffect: an effect only runs after
  // the render it was triggered by has already committed, so the very
  // first render with the grown 'entries' would still show the stale
  // override for one frame — invisible most of the time, but exactly what
  // "the first change after an undo gets swallowed, the next one
  // self-corrects" looks like when it isn't. Deriving it inline means the
  // same render that first sees the new tail already shows the correct,
  // un-overridden state — nothing to catch up on next time. The state
  // itself is still cleared in an effect below, purely so a stale object
  // isn't held onto indefinitely; that cleanup is never load-bearing for
  // what's rendered.
  const [override, setOverride] = useState<{ activeId: string; stash: WireUndoEntry[]; tailIdAtJump: string | undefined } | null>(null);
  const currentTailId = entries[entries.length - 1]?.id;
  const effective = override && currentTailId === override.tailIdAtJump ? override : null;

  useEffect(() => {
    if (override && currentTailId !== override.tailIdAtJump) setOverride(null);
  }, [currentTailId, override]);

  const activeId = effective?.activeId ?? entries[entries.length - 1]?.id;
  const stash = effective?.stash ?? [];
  // Only the non-stashed entries render in the main row — the stashed ones
  // (still just ordinary entries, unmodified) get their own trailing group
  // right after the active dot instead of their natural chronological slot.
  const stashedIds = new Set(stash.map((e) => e.id));
  const main = entries.filter((e) => !stashedIds.has(e.id));

  // Set directly by 'jump()' at the moment a jump happens, rather than
  // inferred after the fact by diffing renders — 'jump()' already knows
  // it's a jump right when it's called, so there's nothing to infer.
  const [justJumpedTo, setJustJumpedTo] = useState<string | null>(null);

  const scrollRef = useRef<HTMLDivElement>(null);
  const activeDotRef = useRef<HTMLDivElement>(null);
  const mountedRef = useRef(false);

  // Keep the active dot in view whenever it changes — centered, not flush
  // against the fade-out edge, so it never lands in the masked zone. Panned
  // via framer's own imperative 'animate()' rather than raw scrollLeft math
  // in a requestAnimationFrame loop — same interruptible, well-tested engine
  // 'layout'/'motion.div' already use elsewhere in this component, instead
  // of a second, hand-rolled one with its own timing edge cases. Staged
  // *before* a freshly-appended dot's own fade-in (see 'SCROLL_DURATION'/
  // 'FADE_DELAY' below and the dot's own 'transition' prop) — the bar
  // settles into position first, then the new dot appears, rather than both
  // happening at once. Instant, not panned, on first mount — nothing to
  // animate *from* yet.
  useEffect(() => {
    const container = scrollRef.current;
    const dot = activeDotRef.current;
    if (!container || !dot) return;
    const target = dot.offsetLeft + dot.offsetWidth / 2 - container.clientWidth / 2;
    const max = Math.max(0, container.scrollWidth - container.clientWidth);
    const clamped = Math.max(0, Math.min(target, max));
    if (!mountedRef.current) {
      container.scrollLeft = clamped;
      mountedRef.current = true;
      return;
    }
    const controls = animate(container.scrollLeft, clamped, {
      duration: SCROLL_DURATION,
      ease: "easeOut",
      onUpdate: (v) => { container.scrollLeft = v; },
    });
    return () => controls.stop();
  }, [activeId]);

  if (entries.length === 0) return null;

  function jump(entry: WireUndoEntry) {
    const idx = entries.findIndex((e) => e.id === entry.id);
    setOverride({ activeId: entry.id, stash: entries.slice(idx + 1), tailIdAtJump: currentTailId });
    setJustJumpedTo(entry.id);
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
          <AnimatePresence initial={false}>
            {main.flatMap((entry, i) => {
              const isActive = entry.id === activeId;
              const dots = [
                <motion.div
                  key={entry.id}
                  layout
                  initial={{ opacity: 0, x: 6 }}
                  animate={{ opacity: 1, x: 0 }}
                  exit={{ opacity: 0 }}
                  transition={{
                    layout: { duration: SCROLL_DURATION, ease: "easeOut" },
                    opacity: { duration: 0.18, delay: DOT_FADE_DELAY },
                    x: { duration: 0.18, delay: DOT_FADE_DELAY },
                  }}
                  ref={isActive ? activeDotRef : undefined}
                  style={{ position: "relative", display: "flex", alignItems: "center" }}
                >
                  {i > 0 && (
                    <div style={{ position: "absolute", right: DOT, width: DOT_GAP - DOT, height: 1, background: "var(--border-subtle)" }} />
                  )}
                  {isActive && entry.id === justJumpedTo && (
                    <PulseRing color={kindColor(entry.kind)} onDone={() => setJustJumpedTo(null)} />
                  )}
                  <Dot
                    filled={isActive}
                    color={kindColor(entry.kind)}
                    title={`${new Date(entry.time).toLocaleString()}${entry.kind ? ` — ${entry.kind}` : ""}`}
                    onClick={() => jump(entry)}
                  />
                </motion.div>,
              ];
              // The stash — dots that were "ahead" at the moment of the jump
              // that made 'entry' active — renders as a trailing run right
              // after it, still real, still clickable (jumping to one just
              // resumes further along the same abandoned run). Same keys as
              // when they're back in 'main', so returning there later is a
              // layout move, not a fresh mount.
              if (isActive) {
                stash.forEach((s) => dots.push(
                  <motion.div
                    key={s.id}
                    layout
                    style={{ position: "relative", display: "flex", alignItems: "center" }}
                  >
                    <div style={{ position: "absolute", right: DOT, width: DOT_GAP - DOT, height: 1, background: "var(--border-subtle)", opacity: 0.5 }} />
                    <Dot
                      faded
                      color={kindColor(s.kind)}
                      title={`Undone — ${new Date(s.time).toLocaleString()}${s.kind ? ` — ${s.kind}` : ""}. Click to jump back here.`}
                      onClick={() => jump(s)}
                    />
                  </motion.div>,
                ));
              }
              return dots;
            })}
          </AnimatePresence>
        </div>
      </div>
    </div>
  );
}
