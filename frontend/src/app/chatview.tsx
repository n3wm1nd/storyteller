"use client";

// Chat view: an alternative rendering of a file's tick chain for files under
// the chat/ convention (see WRITER.md) — the same Prompt/Atom ticks the
// ordinary file view shows as prose/annotations are instead paired into
// user/assistant bubbles. Not a separate connection or cache: this reads the
// same `openFiles[path]` state page.tsx already maintains for the "File" tab.

import { useState } from "react";
import { Send, RotateCcw } from "lucide-react";
import type { WireTick } from "@/lib/ws";
import { tickChain } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";
import { AgentLogStrip, ChatPreviewStrip } from "./fileview";

interface ChatExchange {
  promptTick: WireTick;
  atomTick: WireTick | null; // null while the reply is still in flight
}

// Pair each "prompt" tick with the "atom" tick immediately following it in
// the chain — that's always the reply to that prompt, since chatConverse
// stores the prompt tick then appends exactly one atom (see
// Server.Writer.File.chatConverse). Any other tick kind on a chat file
// (there shouldn't be any, by convention) is skipped.
function exchangesFromChain(chain: WireTick[]): ChatExchange[] {
  const exchanges: ChatExchange[] = [];
  for (let i = 0; i < chain.length; i++) {
    const t = chain[i];
    if (t.kind !== "prompt") continue;
    const next = chain[i + 1];
    exchanges.push({ promptTick: t, atomTick: next && next.kind === "atom" ? next : null });
  }
  return exchanges;
}

const bubbleBase: React.CSSProperties = {
  maxWidth: "72%", padding: "8px 12px", borderRadius: 10, fontSize: 12.5,
  lineHeight: 1.5, whiteSpace: "pre-wrap", wordBreak: "break-word",
};

export function ChatView({
  ticks, head, preview, agentLogs, onClearAgentLogs, onSend, onRegen,
}: {
  ticks: Record<string, WireTick>;
  head: string | null;
  preview: { text: string; thinking: string } | null;
  agentLogs: { level: string; message: string }[];
  onClearAgentLogs: () => void;
  onSend: (text: string) => void;
  onRegen: (promptTickId: string, atomTickId: string, text: string) => void;
}) {
  const [draft, setDraft] = useState("");
  const chain = tickChain(ticks, head);
  const exchanges = exchangesFromChain(chain);
  const lastIndex = exchanges.length - 1;
  const generating = preview !== null || (lastIndex >= 0 && exchanges[lastIndex].atomTick === null);

  const scrollRef = useAutoScroll<HTMLDivElement>(exchanges.length + (preview?.text.length ?? 0), head, "end");

  function submit() {
    const text = draft.trim();
    if (!text || generating) return;
    onSend(text);
    setDraft("");
  }

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", minHeight: 0 }}>
      <div ref={scrollRef} style={{ flex: 1, overflow: "auto", padding: "14px", display: "flex", flexDirection: "column", gap: 14 }}>
        {exchanges.length === 0 && preview === null && (
          <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
            Say something to start the conversation
          </div>
        )}
        {exchanges.map((ex, i) => {
          const atomTick = ex.atomTick;
          return (
            <div key={ex.promptTick.tickId} style={{ display: "flex", flexDirection: "column", gap: 8 }}>
              <div style={{ alignSelf: "flex-end", ...bubbleBase, background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)", color: "var(--text-heading)" }}>
                {ex.promptTick.message}
              </div>
              {atomTick && (
                <div style={{ alignSelf: "flex-start", display: "flex", flexDirection: "column", gap: 4, maxWidth: "72%" }}>
                  <div style={{ ...bubbleBase, maxWidth: "100%", background: "var(--surface-deep)", border: "1px solid var(--border-subtle)", color: "var(--text-muted)" }}>
                    {atomTick.content ?? atomTick.message}
                  </div>
                  {i === lastIndex && !generating && (
                    <button
                      onClick={() => onRegen(ex.promptTick.tickId, atomTick.tickId, ex.promptTick.message)}
                      title="Regenerate this reply"
                      style={{
                        alignSelf: "flex-start", display: "flex", alignItems: "center", gap: 4,
                        fontSize: 10, padding: "2px 6px", borderRadius: 4, cursor: "pointer",
                        background: "transparent", border: "1px solid var(--border-subtle)", color: "var(--text-dim)",
                      }}
                    >
                      <RotateCcw style={{ width: 10, height: 10 }} />
                      Regenerate
                    </button>
                  )}
                </div>
              )}
            </div>
          );
        })}
      </div>

      <ChatPreviewStrip preview={preview} />
      <AgentLogStrip logs={agentLogs} onClear={onClearAgentLogs} />

      <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "10px 14px", display: "flex", gap: 8 }}>
        <textarea
          value={draft}
          onChange={(e) => setDraft(e.target.value)}
          onKeyDown={(e) => { if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); submit(); } }}
          placeholder={generating ? "Waiting for a reply…" : "Message…"}
          disabled={generating}
          rows={2}
          style={{
            flex: 1, resize: "none", background: "var(--surface-deep)", border: "1px solid var(--border-subtle)",
            borderRadius: 6, color: "var(--foreground)", padding: "8px 10px", fontSize: 12.5, fontFamily: "inherit", outline: "none",
          }}
        />
        <button
          onClick={submit}
          disabled={generating || draft.trim().length === 0}
          title="Send"
          style={{
            display: "flex", alignItems: "center", justifyContent: "center", width: 36, alignSelf: "flex-end",
            height: 32, borderRadius: 6, border: "none", cursor: generating ? "default" : "pointer",
            background: generating || draft.trim().length === 0 ? "var(--surface-deep)" : "oklch(0.78 0.10 65 / 0.2)",
            color: generating || draft.trim().length === 0 ? "var(--text-dim)" : "var(--amber)",
          }}
        >
          <Send style={{ width: 14, height: 14 }} />
        </button>
      </div>
    </div>
  );
}
