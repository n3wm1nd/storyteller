// Streamed chat-preview handling — shared because both the branch connection
// (chargen) and file connections (chat.writer/fixer/regen/outline) can emit
// ChatPreviewEvent and write into the same 'preview' cache field (see
// serverCacheStore.ts). Lives here rather than in either connection's own
// *.actions.ts file so neither has to import the other's internals for this.

import type { ChatPreviewEvent } from "./ws";
import { getServerCache, mirrorServerEvent } from "./serverCacheStore";

// Shows the preview strip (as a "Generating…" placeholder) a beat after a
// chat.prompt is sent, in case the real chat.preview.start takes a while to
// arrive — but only once enough time has passed that a fast agent wouldn't
// just flash it briefly. Cancelled the moment a real preview event or the
// command's actual result (update/error) arrives.
const PREVIEW_DELAY_MS = 1000;
let previewDelayTimer: ReturnType<typeof setTimeout> | null = null;

export function clearPreviewDelayTimer() {
  if (previewDelayTimer !== null) {
    clearTimeout(previewDelayTimer);
    previewDelayTimer = null;
  }
}

export function schedulePreviewPlaceholder() {
  clearPreviewDelayTimer();
  previewDelayTimer = setTimeout(() => {
    previewDelayTimer = null;
    if (getServerCache().preview === null) mirrorServerEvent({ preview: { text: "", thinking: "", progress: null } });
  }, PREVIEW_DELAY_MS);
}

// The server can flush one WS frame per token, and every "chat.preview"/
// "chat.preview.thinking" event only ever appends to what's already there —
// so instead of one store write (and downstream re-render) per token, buffer
// the deltas here and merge them into 'preview' at most once per animation
// frame. Nothing is lost by delaying the merge a few ms; only how often
// subscribers get to re-render drops, capped at the display's own refresh
// rate instead of the network's.
let pendingText = "";
let pendingThinking = "";
let flushScheduled = false;

function scheduleFlush() {
  if (flushScheduled) return;
  flushScheduled = true;
  requestAnimationFrame(() => {
    flushScheduled = false;
    if (pendingText === "" && pendingThinking === "") return;
    const p = getServerCache().preview;
    if (p) mirrorServerEvent({ preview: { text: p.text + pendingText, thinking: p.thinking + pendingThinking, progress: p.progress } });
    pendingText = "";
    pendingThinking = "";
  });
}

export function handleChatPreview(evt: ChatPreviewEvent) {
  clearPreviewDelayTimer();
  switch (evt.type) {
    case "chat.preview.start":
      pendingText = "";
      pendingThinking = "";
      mirrorServerEvent({ preview: { text: "", thinking: "", progress: null } });
      break;
    case "chat.preview":
      pendingText += evt.text;
      scheduleFlush();
      break;
    case "chat.preview.thinking":
      pendingThinking += evt.text;
      scheduleFlush();
      break;
    case "chat.preview.progress": {
      const p = getServerCache().preview;
      const progress = { processed: evt.processed, total: evt.total, updatedAt: Date.now() };
      mirrorServerEvent({ preview: p ? { ...p, progress } : { text: "", thinking: "", progress } });
      break;
    }
    case "chat.preview.end":
      pendingText = "";
      pendingThinking = "";
      mirrorServerEvent({ preview: null, previewCommandId: null });
      break;
  }
}
