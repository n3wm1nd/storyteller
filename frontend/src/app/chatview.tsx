"use client";

// Chat view: an alternative rendering of a file's tick chain for files under
// the chat/ convention (see WRITER.md) — the same Prompt/Atom ticks the
// ordinary file view shows as prose/annotations are instead paired into
// user/assistant bubbles. Not a separate connection or cache: this reads the
// same `openFiles[path]` state page.tsx already maintains for the "File" tab.

import { useRef, useState } from "react";
import { Send, RotateCcw } from "lucide-react";
import type { WireTick } from "@/lib/ws";
import { tickChain } from "@/lib/utils";
import { useAutoScroll } from "@/lib/useAutoScroll";
import { AgentLogStrip, ChatPreviewStrip } from "./fileview";
import { CHAT_COMMANDS, parseCommand } from "@/lib/commands";
import { useCommandAutocomplete, CommandSuggestionPopup } from "./command-autocomplete";

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

// Rough row estimate for the edit textarea — assistant replies can run to
// paragraphs, so a fixed row count either clips them or wastes space on a
// one-line user prompt. Chars-per-row is a guess (the bubble's actual width
// varies with panel size), not a measurement — good enough for an initial
// size, and 'resize: vertical' on the textarea covers the rest.
function estimateRows(text: string): number {
  const CHARS_PER_ROW = 60;
  const total = text.split("\n").reduce((sum, line) => sum + Math.max(1, Math.ceil(line.length / CHARS_PER_ROW)), 0);
  return Math.min(24, Math.max(3, total));
}

// A chat bubble that turns into a textarea on double-click — same
// double-click-to-edit / Cmd-Enter-to-commit / Escape-to-cancel convention
// as fileview.tsx's AtomBlock. Saving is the caller's job (see ChatView):
// a mid-conversation edit is a plain content rewrite, but editing the most
// recent user turn instead drops the stale reply and re-asks, since that
// reply was generated against the text being replaced.
function EditableBubble({
  content, align, bubbleStyle, disabled, onSave, onEditingChange,
}: {
  content: string;
  align: "flex-start" | "flex-end";
  bubbleStyle: React.CSSProperties;
  disabled: boolean;
  onSave: (text: string) => void;
  // The bubble's display width is capped (bubbleBase's 72%, see below), but
  // that's too narrow to comfortably edit a multi-paragraph assistant
  // reply in. The caller (ChatView) owns that cap on an outer wrapper div,
  // so editing needs to tell it to lift it for the duration.
  onEditingChange?: (editing: boolean) => void;
}) {
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  function setEditingState(v: boolean) {
    setEditing(v);
    onEditingChange?.(v);
  }

  function startEdit() {
    if (disabled) return;
    setDraft(content);
    setEditingState(true);
    setTimeout(() => textareaRef.current?.focus(), 0);
  }

  function commit() {
    const trimmed = draft.trim();
    if (trimmed && trimmed !== content.trim()) onSave(trimmed);
    setEditingState(false);
  }

  if (editing) {
    return (
      <div style={{ alignSelf: align, width: "100%", display: "flex", flexDirection: "column", gap: 4 }}>
        <textarea
          ref={textareaRef}
          value={draft}
          onChange={(e) => setDraft(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); commit(); }
            if (e.key === "Escape") setEditingState(false);
          }}
          rows={estimateRows(draft)}
          style={{
            width: "100%", boxSizing: "border-box", resize: "vertical",
            background: "var(--surface-deep)", border: "1px solid oklch(0.78 0.10 65 / 0.4)",
            borderRadius: 6, padding: "8px 10px", color: "var(--foreground)",
            fontSize: 12.5, lineHeight: 1.5, fontFamily: "inherit", outline: "none",
          }}
        />
        <div style={{ display: "flex", gap: 6, justifyContent: "flex-end" }}>
          <button onClick={() => setEditingState(false)} style={{
            background: "none", border: "1px solid var(--border-subtle)", borderRadius: 3,
            color: "var(--text-ghost)", fontSize: 10, padding: "2px 8px", cursor: "pointer",
          }}>Cancel</button>
          <button onClick={commit} style={{
            background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.4)",
            borderRadius: 3, color: "var(--text-secondary)", fontSize: 10, padding: "2px 8px", cursor: "pointer",
          }}>Save</button>
        </div>
      </div>
    );
  }

  return (
    <div onDoubleClick={startEdit} title={disabled ? undefined : "Double-click to edit"} style={{ alignSelf: align, ...bubbleStyle, cursor: disabled ? undefined : "text" }}>
      {content}
    </div>
  );
}

// Wraps the assistant reply + its Regenerate button. Holds its own editing
// state (can't be a plain inline div in ChatView's .map — the width cap
// needs to react to EditableBubble's internal edit toggle, and hooks can't
// live in a loop body) so the 72% display cap can lift only while editing.
function AssistantReply({ atomTick, disabled, showRegen, onSave, onRegen }: {
  atomTick: WireTick;
  disabled: boolean;
  showRegen: boolean;
  onSave: (text: string) => void;
  onRegen: () => void;
}) {
  const [editing, setEditing] = useState(false);
  return (
    <div style={{ alignSelf: "flex-start", display: "flex", flexDirection: "column", gap: 4, maxWidth: editing ? "100%" : "72%", width: editing ? "100%" : undefined }}>
      <EditableBubble
        align="flex-start"
        content={atomTick.content ?? atomTick.message}
        disabled={disabled}
        onEditingChange={setEditing}
        bubbleStyle={{ ...bubbleBase, maxWidth: "100%", background: "var(--surface-deep)", border: "1px solid var(--border-subtle)", color: "var(--text-muted)" }}
        onSave={onSave}
      />
      {showRegen && !editing && (
        <button
          onClick={onRegen}
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
  );
}

export function ChatView({
  ticks, head, preview, agentLogs, onClearAgentLogs, onSend, onNote, onRegen, onEditAtom, onEditPrompt,
}: {
  ticks: Record<string, WireTick>;
  head: string | null;
  preview: { text: string; thinking: string } | null;
  agentLogs: { level: string; message: string }[];
  onClearAgentLogs: () => void;
  onSend: (text: string) => void;
  onNote: (text: string) => void;
  onRegen: (promptTickId: string, atomTickId: string, text: string) => void;
  onEditAtom: (tickId: string, content: string) => void;
  onEditPrompt: (tickId: string, content: string) => void;
}) {
  const [draft, setDraft] = useState("");
  const auto = useCommandAutocomplete(draft, setDraft, CHAT_COMMANDS);
  const chain = tickChain(ticks, head);
  const exchanges = exchangesFromChain(chain);
  const lastIndex = exchanges.length - 1;
  const generating = preview !== null || (lastIndex >= 0 && exchanges[lastIndex].atomTick === null);

  const scrollRef = useAutoScroll<HTMLDivElement>(exchanges.length + (preview?.text.length ?? 0), head, "end");

  // A leading "/note" sends an annotation instead of a conversational turn
  // (see CHAT_COMMANDS) — anything else, slash or not, is plain conversation.
  function submit() {
    const raw = draft.trim();
    if (!raw || generating) return;
    const parsed = parseCommand(raw);
    if (parsed?.name === "note") {
      if (parsed.text) onNote(parsed.text);
    } else {
      onSend(raw);
    }
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
              <EditableBubble
                align="flex-end"
                content={ex.promptTick.message}
                disabled={generating}
                bubbleStyle={{ ...bubbleBase, background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)", color: "var(--text-heading)" }}
                onSave={(text) => {
                  // Editing the most recent turn: the existing reply was
                  // generated against the text being replaced, so it's
                  // dropped and re-asked rather than left stale (same path
                  // as the Regenerate button, just with new text).
                  if (i === lastIndex && atomTick) onRegen(ex.promptTick.tickId, atomTick.tickId, text);
                  else onEditPrompt(ex.promptTick.tickId, text);
                }}
              />
              {atomTick && (
                <AssistantReply
                  atomTick={atomTick}
                  disabled={generating}
                  showRegen={i === lastIndex && !generating}
                  onSave={(text) => onEditAtom(atomTick.tickId, text)}
                  onRegen={() => onRegen(ex.promptTick.tickId, atomTick.tickId, ex.promptTick.message)}
                />
              )}
            </div>
          );
        })}
      </div>

      <ChatPreviewStrip preview={preview} />
      <AgentLogStrip logs={agentLogs} onClear={onClearAgentLogs} />

      <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "10px 14px", display: "flex", gap: 8 }}>
        <div style={{ position: "relative", flex: 1, display: "flex" }}>
          <CommandSuggestionPopup suggestions={auto.suggestions} activeIndex={auto.activeIndex} onPick={auto.pick} />
          <textarea
            ref={auto.taRef}
            value={draft}
            onChange={(e) => setDraft(e.target.value)}
            onSelect={auto.onSelect}
            onClick={auto.onSelect}
            onKeyDown={(e) => {
              if (auto.onKeyDown(e)) return;
              if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); submit(); }
            }}
            placeholder={generating ? "Waiting for a reply…" : "Message… (/note to annotate)"}
            disabled={generating}
            rows={2}
            style={{
              flex: 1, resize: "none", background: "var(--surface-deep)", border: "1px solid var(--border-subtle)",
              borderRadius: 6, color: "var(--foreground)", padding: "8px 10px", fontSize: 12.5, fontFamily: "inherit", outline: "none",
            }}
          />
        </div>
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
