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

export function handleChatPreview(evt: ChatPreviewEvent) {
  clearPreviewDelayTimer();
  switch (evt.type) {
    case "chat.preview.start":
      mirrorServerEvent({ preview: { text: "", thinking: "" } });
      break;
    case "chat.preview": {
      const p = getServerCache().preview;
      if (p) mirrorServerEvent({ preview: { ...p, text: p.text + evt.text } });
      break;
    }
    case "chat.preview.thinking": {
      const p = getServerCache().preview;
      if (p) mirrorServerEvent({ preview: { ...p, thinking: p.thinking + evt.text } });
      break;
    }
    case "chat.preview.end":
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
