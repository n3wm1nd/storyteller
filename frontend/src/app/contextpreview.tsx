"use client";

// Context-preview tab: lets the author define the context slots a command
// would see for the open file — which files populate each slot, ambient
// (always injected) or on-demand (tool-surfaced only) — and watch the
// resolved result update live, without running any agent.
//
// Deliberately not wired through lib/serverCacheStore.ts: unlike a file's
// tick chain, nothing here is shared, synced state other components need to
// read — it's a single-consumer, request/response preview (see
// Server.Writer.ContextView's module header: every request is
// self-contained, nothing persists server-side). So the connection and its
// result live as local component state, same spirit as
// lib/serverCacheStore.ts's "one connection per component" convention, just
// without a global store slice to mirror into.

import { useEffect, useRef, useState } from "react";
import { ChevronRight, Eye, EyeOff, Plus, Trash2, RefreshCw } from "lucide-react";
import { contextViewConn } from "@/lib/ws";
import type { ContextSlot, ContextSlotPreview, ContextMode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";

interface SlotDraft {
  key: string;
  label: string;
  mode: ContextMode;
  include: string; // raw comma-separated glob patterns, as typed
  exclude: string;
}

let slotKeySeq = 0;
function newSlotDraft(label: string, mode: ContextMode): SlotDraft {
  slotKeySeq += 1;
  return { key: `slot-${slotKeySeq}`, label, mode, include: "", exclude: "" };
}

function toWireSlots(drafts: SlotDraft[]): ContextSlot[] {
  return drafts.map((d) => ({
    label: d.label.trim() || "untitled",
    mode: d.mode,
    filter: {
      include: d.include.split(",").map((s) => s.trim()).filter(Boolean),
      exclude: d.exclude.split(",").map((s) => s.trim()).filter(Boolean),
    },
  }));
}

const inputStyle: React.CSSProperties = {
  fontSize: 11, padding: "3px 7px", background: "var(--card)",
  border: "1px solid var(--border-subtle)", borderRadius: 5,
  color: "var(--foreground)", outline: "none",
};

function SlotEditorRow({ draft, onChange, onRemove }: {
  draft: SlotDraft;
  onChange: (next: SlotDraft) => void;
  onRemove: () => void;
}) {
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 4, padding: "8px 10px", borderBottom: "1px solid var(--border-subtle)" }}>
      <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
        <input
          value={draft.label}
          onChange={(e) => onChange({ ...draft, label: e.target.value })}
          placeholder="slot label"
          style={{ ...inputStyle, flex: 1, fontWeight: 500 }}
        />
        <button
          onClick={() => onChange({ ...draft, mode: draft.mode === "ambient" ? "on-demand" : "ambient" })}
          title="Toggle ambient (always injected) vs on-demand (tool-surfaced only)"
          style={{
            display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "3px 8px",
            borderRadius: 5, cursor: "pointer", border: "1px solid var(--border-subtle)",
            background: draft.mode === "ambient" ? "oklch(0.78 0.10 65 / 0.15)" : "oklch(0.58 0.10 200 / 0.12)",
            color: draft.mode === "ambient" ? "var(--amber)" : "oklch(0.68 0.12 200)",
            whiteSpace: "nowrap",
          }}
        >
          {draft.mode === "ambient" ? <Eye style={{ width: 10, height: 10 }} /> : <EyeOff style={{ width: 10, height: 10 }} />}
          {draft.mode}
        </button>
        <button
          onClick={onRemove}
          title="Remove slot"
          style={{ width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center", border: "none", background: "transparent", color: "var(--text-dim)", cursor: "pointer" }}
        >
          <Trash2 style={{ width: 11, height: 11 }} />
        </button>
      </div>
      <div style={{ display: "flex", gap: 6 }}>
        <input
          value={draft.include}
          onChange={(e) => onChange({ ...draft, include: e.target.value })}
          placeholder="include glob(s), e.g. characters/**, *.md — empty = everything"
          style={{ ...inputStyle, flex: 1 }}
        />
        <input
          value={draft.exclude}
          onChange={(e) => onChange({ ...draft, exclude: e.target.value })}
          placeholder="exclude glob(s)"
          style={{ ...inputStyle, flex: 1 }}
        />
      </div>
    </div>
  );
}

function SlotResult({ preview }: { preview: ContextSlotPreview }) {
  const [open, setOpen] = useState(true);
  return (
    <div style={{ borderBottom: "1px solid var(--border-subtle)" }}>
      <button
        onClick={() => setOpen((v) => !v)}
        style={{
          display: "flex", alignItems: "center", gap: 6, width: "100%", textAlign: "left",
          padding: "6px 10px", border: "none", background: "transparent", cursor: "pointer",
        }}
      >
        <ChevronRight style={{ width: 11, height: 11, flexShrink: 0, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s", color: "var(--text-dim)" }} />
        <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-heading)" }}>{preview.label}</span>
        <span style={{
          fontSize: 9, padding: "1px 6px", borderRadius: 8,
          background: preview.mode === "ambient" ? "oklch(0.78 0.10 65 / 0.15)" : "oklch(0.58 0.10 200 / 0.12)",
          color: preview.mode === "ambient" ? "var(--amber)" : "oklch(0.68 0.12 200)",
        }}>
          {preview.mode}
        </span>
        <span style={{ marginLeft: "auto", fontSize: 10, color: "var(--text-ghost)" }}>
          {preview.entries.length} file{preview.entries.length !== 1 ? "s" : ""}
        </span>
      </button>
      {open && (
        <div style={{ padding: "0 10px 8px 27px", display: "flex", flexDirection: "column", gap: 6 }}>
          {preview.entries.length === 0 ? (
            <div style={{ fontSize: 10, color: "var(--text-ghost)" }}>no files match this filter</div>
          ) : preview.entries.map((entry) => (
            <div key={entry.path}>
              <div style={{ fontSize: 10.5, color: "var(--text-secondary)", fontFamily: "monospace" }}>{entry.path}</div>
              {entry.content != null ? (
                <pre style={{
                  margin: "2px 0 0", fontSize: 10.5, lineHeight: 1.4, maxHeight: 160, overflow: "auto",
                  padding: "6px 8px", background: "var(--card)", border: "1px solid var(--border-subtle)",
                  borderRadius: 5, whiteSpace: "pre-wrap", color: "var(--text-muted)",
                }}>
                  {entry.content}
                </pre>
              ) : entry.blurb ? (
                <div style={{ fontSize: 10.5, fontStyle: "italic", color: "var(--text-ghost)" }}>{entry.blurb}</div>
              ) : null}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export function ContextPreviewPanel({ activeBranch, path }: { activeBranch: string | null; path: string }) {
  const [slots, setSlots] = useState<SlotDraft[]>(() => [newSlotDraft("branch-files", "ambient")]);
  const [result, setResult] = useState<ContextSlotPreview[] | null>(null);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);

  useEffect(() => {
    if (!activeBranch) return;
    const label = `context:${path}`;
    setConnStatus(label, "connecting");

    const conn = contextViewConn(activeBranch, path);
    connRef.current = conn;
    setResult(null);

    conn.onStatus((s) => {
      if (s !== "connected") setConnStatus(label, "connecting");
    });
    conn.subscribe((evt) => {
      bumpActivity(label);
      if (evt.type === "context.preview") setResult(evt.slots);
      else if (evt.type === "error") setError(evt.message);
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(label, "connected");
        conn.send({ type: "context.preview", slots: toWireSlots(slots) });
      } catch (err) {
        setConnStatus(label, "error");
        setError(String(err));
      }
    })();

    return () => {
      conn.close();
      connRef.current = null;
      removeConn(label);
    };
    // Slots are re-sent explicitly on every edit (see `commit`) — this effect
    // only needs to re-run when the connection target itself changes.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeBranch, path]);

  function commit(next: SlotDraft[]) {
    setSlots(next);
    connRef.current?.send({ type: "context.preview", slots: toWireSlots(next) });
  }

  function addSlot() {
    commit([...slots, newSlotDraft(`slot-${slots.length + 1}`, "ambient")]);
  }

  const byLabel = new Map((result ?? []).map((r) => [r.label, r]));

  return (
    <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
      <div style={{ width: 280, minWidth: 280, display: "flex", flexDirection: "column", borderRight: "1px solid var(--border-subtle)", overflow: "auto" }}>
        <div style={{ padding: "6px 10px", fontSize: 10, fontWeight: 600, color: "var(--text-dim)" }}>
          CONTEXT SLOTS
        </div>
        {slots.map((draft) => (
          <SlotEditorRow
            key={draft.key}
            draft={draft}
            onChange={(next) => commit(slots.map((s) => (s.key === draft.key ? next : s)))}
            onRemove={() => commit(slots.filter((s) => s.key !== draft.key))}
          />
        ))}
        <button
          onClick={addSlot}
          style={{
            display: "flex", alignItems: "center", gap: 4, margin: "8px 10px", fontSize: 10.5,
            padding: "4px 8px", borderRadius: 5, cursor: "pointer",
            background: "oklch(0.78 0.10 65 / 0.12)", border: "1px solid oklch(0.78 0.10 65 / 0.3)", color: "var(--amber)",
          }}
        >
          <Plus style={{ width: 10, height: 10 }} /> Add slot
        </button>
        <div style={{ padding: "6px 10px", fontSize: 9.5, color: "var(--text-ghost)", lineHeight: 1.5 }}>
          Preview only — nothing here is wired into Write/Fix/Chat yet. Edits
          re-send the whole slot list and get a fresh resolve back; nothing
          persists server-side between requests.
        </div>
      </div>

      <div style={{ flex: 1, overflow: "auto" }}>
        {result === null ? (
          <div style={{ display: "flex", alignItems: "center", gap: 6, padding: 16, fontSize: 11, color: "var(--text-ghost)" }}>
            <RefreshCw style={{ width: 11, height: 11 }} className="animate-spin" /> resolving…
          </div>
        ) : (
          slots.map((draft) => {
            const preview = byLabel.get(draft.label.trim() || "untitled");
            return preview ? <SlotResult key={draft.key} preview={preview} /> : null;
          })
        )}
      </div>
    </div>
  );
}
