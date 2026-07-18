// Streamed chat-preview handling — shared because both the branch connection
// (chargen) and file connections (chat.writer/fixer/regen/outline) can emit
// ChatPreviewEvent and write into the same 'preview' cache field (see
// serverCacheStore.ts). Lives here rather than in either connection's own
// *.actions.ts file so neither has to import the other's internals for this.

import type { ChatPreviewEvent } from "./ws";
import { getServerCache, mirrorServerEvent } from "./serverCacheStore";
import { useUI } from "./uiStore";
import { atRebase } from "./wsHelpers";

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
    if (getServerCache().preview === null) mirrorServerEvent({ preview: { text: "", thinking: "" } });
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
    if (p) mirrorServerEvent({ preview: { text: p.text + pendingText, thinking: p.thinking + pendingThinking } });
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
      mirrorServerEvent({ preview: { text: "", thinking: "" } });
      break;
    case "chat.preview":
      pendingText += evt.text;
      scheduleFlush();
      break;
    case "chat.preview.thinking":
      pendingThinking += evt.text;
      scheduleFlush();
      break;
    case "chat.preview.end":
      pendingText = "";
      pendingThinking = "";
      mirrorServerEvent({ preview: null, previewCommandId: null });
      break;
  }
  if (evt.type === "chat.preview.end") flushPendingSubmit();
}

// Send the submission that was held back while the previous generation was
// still streaming, now that it's done. Built at queue-time (see
// fileview.actions.ts's 'sendChatCommand'), so nothing left to do but
// attach a fresh id (same as an immediate send — see 'sendChatCommand') and
// send it.
function flushPendingSubmit() {
  const pending = useUI.getState().pendingSubmit;
  if (!pending) return;
  useUI.setState({ pendingSubmit: null });
  const fc = getServerCache().openFiles[pending.path];
  if (!fc) return;
  const cmd = { ...pending.cmd, id: crypto.randomUUID() };
  mirrorServerEvent({ previewCommandId: cmd.id });
  fc.conn.send(atRebase(useUI.getState().rebaseMarker, fc.ticks, cmd, useUI.getState().journalMarkers));
}
