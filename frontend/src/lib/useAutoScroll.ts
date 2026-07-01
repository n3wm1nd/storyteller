"use client";

import { useEffect, useLayoutEffect, useRef } from "react";

const NEAR_EDGE_PX = 64;

/**
 * Keeps a scroll container pinned to one edge while content streams in.
 *
 * - Jumps to `edge` whenever `resetKey` changes (file switch, reconnect) or on mount.
 * - Otherwise only re-snaps to `edge` on a `contentKey` change if the user was
 *   already sitting near that edge — so appends follow along, but scrolling
 *   away to read something earlier is never interrupted.
 */
export function useAutoScroll<T extends HTMLElement>(
  contentKey: unknown,
  resetKey: unknown,
  edge: "start" | "end" = "end",
) {
  const ref = useRef<T>(null);
  const pinned = useRef(true);
  const lastReset = useRef<unknown>(undefined);

  function distanceFromEdge(el: T): number {
    return edge === "end" ? el.scrollHeight - el.scrollTop - el.clientHeight : el.scrollTop;
  }

  function scrollToEdge(el: T) {
    el.scrollTop = edge === "end" ? el.scrollHeight : 0;
  }

  useEffect(() => {
    const el = ref.current;
    if (!el) return;
    const onScroll = () => { pinned.current = distanceFromEdge(el) <= NEAR_EDGE_PX; };
    el.addEventListener("scroll", onScroll, { passive: true });
    return () => el.removeEventListener("scroll", onScroll);
  }, []);

  useLayoutEffect(() => {
    const el = ref.current;
    if (!el) return;
    if (lastReset.current !== resetKey) {
      lastReset.current = resetKey;
      pinned.current = true;
      scrollToEdge(el);
      return;
    }
    if (pinned.current) scrollToEdge(el);
  }, [contentKey, resetKey]);

  return ref;
}
