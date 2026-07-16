"use client";

import { useEffect, useState } from "react";
import { ListChecks, Lightbulb } from "lucide-react";
import { branchFileUrl, saveRawFileAsNew } from "@/lib/ws";
import { TASKS_PATH } from "./tasks-panel.actions";

// ── Tasks panel ──────────────────────────────────────────────────────────────
//
// Experimental (see Storyteller.Writer.Agent.Tasks): Sync reconciles
// tasks.md against this character's own journal (completed/abandoned/
// changed goals), Suggest proposes new ones from the journal plus the
// story's lore (never the story's raw scene content — a character's
// suggestions must only draw on what they'd actually know). Every
// server-side pass, and every manual save here, replaces tasks.md's
// content wholesale (see Storage.Ops.checkpointFile/saveFileAsNew) —
// deliberately not the atom-by-atom editor interface WireTickList gives
// the journal, since there's no meaningful "which atom" for a hand-edited
// task list the way there is for a sequence of journal/scene entries.

// Plain whole-file textarea over tasks.md, loaded via the same raw-content
// GET the main file view's download/embed path uses, saved exclusively via
// 'saveRawFileAsNew' -- no diff-reconciled "Save" here at all (contrast
// fileview.tsx's RawEditPanel, which offers both), since a recreate is the
// only edit shape this file is meant to have. 'refreshToken' is bumped by
// the parent after a Sync/Suggest lands, forcing a refetch so this doesn't
// keep showing a now-stale local draft the agent has since rewritten
// server-side.
function TasksEditor({ branch, refreshToken }: { branch: string; refreshToken: number }) {
  const [content, setContent] = useState<string | null>(null);
  const [savedContent, setSavedContent] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    setContent(null);
    fetch(branchFileUrl(branch, TASKS_PATH))
      .then((res) => {
        // No tasks.md yet is the common, unremarkable first-time state
        // (nothing has been synced/suggested for this character yet) --
        // starts the editor on an empty draft rather than surfacing a 404
        // as if something were wrong.
        if (res.status === 404) return "";
        if (!res.ok) throw new Error(`load failed: ${res.status}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text);
        setSavedContent(text);
        setLoading(false);
      })
      .catch((err) => {
        if (cancelled) return;
        setError(err instanceof Error ? err.message : String(err));
        setLoading(false);
      });
    return () => { cancelled = true; };
  }, [branch, refreshToken]);

  const dirty = content !== null && content !== savedContent;

  function save() {
    if (content === null || saving) return;
    setSaving(true);
    setError(null);
    saveRawFileAsNew(branch, TASKS_PATH, content)
      .then(() => { setSavedContent(content); setSaving(false); })
      .catch((err) => {
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }

  return (
    <div style={{ marginTop: 6 }}>
      <div style={{ display: "flex", alignItems: "center", gap: 8, marginBottom: 4 }}>
        {error && <span style={{ fontSize: 9, color: "var(--rose)" }}>{error}</span>}
        {dirty && !error && <span style={{ fontSize: 9, color: "var(--amber)" }}>unsaved</span>}
        <span style={{ flex: 1 }} />
        <button
          onClick={save}
          disabled={!dirty || saving}
          title="Recreate tasks.md with this content — always a wholesale replacement, never an atom-by-atom edit"
          style={{
            fontSize: 9, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "var(--amber-tint)" : "transparent",
            border: "1px solid " + (dirty ? "var(--amber-border)" : "var(--border-subtle)"),
            color: dirty ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          {saving ? "Saving…" : "Save"}
        </button>
      </div>
      {loading ? (
        <div style={{ fontSize: 10, color: "var(--text-ghost)", fontStyle: "italic" }}>Loading…</div>
      ) : (
        <textarea
          value={content ?? ""}
          onChange={(e) => setContent(e.target.value)}
          placeholder="No tasks yet — write some, or click Suggest above."
          spellCheck={false}
          style={{
            width: "100%", height: 160, resize: "vertical", boxSizing: "border-box",
            padding: 8, fontFamily: "ui-monospace, monospace", fontSize: 11, lineHeight: 1.5,
            color: "var(--text-body)", background: "var(--card)",
            border: "1px solid var(--border-subtle)", borderRadius: 4, outline: "none",
          }}
        />
      )}
    </div>
  );
}

export function TasksPanel({ branch, onSyncTasks, onSuggestTasks }: {
  branch: string;
  // Typed to accept either shape (fire-and-forget or a promise this panel
  // can wait on) — actual callers (tasks-panel.actions.syncTasks/
  // suggestTasks) return a promise that resolves once the command has
  // landed, which 'Promise.resolve' below uses to know when to refetch.
  onSyncTasks: () => void | Promise<void>;
  onSuggestTasks: () => void | Promise<void>;
}) {
  // Bumped after Sync/Suggest resolves, so TasksEditor refetches instead of
  // showing a now-stale draft — see runTasksCommand in
  // tasks-panel.actions.ts.
  const [refreshToken, setRefreshToken] = useState(0);
  const bumpRefresh = () => setRefreshToken((n) => n + 1);

  return (
    <div style={{ marginTop: 8, paddingTop: 8, borderTop: "1px solid var(--border-subtle)" }}>
      <div style={{ display: "flex", alignItems: "center", marginBottom: 6, gap: 6 }}>
        <span style={{ fontSize: 9, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.06em", color: "var(--text-dim)", flex: 1 }}>
          Tasks
        </span>
        <button
          onClick={(e) => { e.stopPropagation(); Promise.resolve(onSyncTasks()).then(bumpRefresh); }}
          title="Experimental: reconcile tasks.md against this character's journal (completed/abandoned/changed goals)"
          style={{
            display: "flex", alignItems: "center", gap: 3, fontSize: 9, padding: "2px 6px",
            background: "transparent", border: "1px solid var(--border)", borderRadius: 4,
            color: "var(--text-label)", cursor: "pointer",
          }}
        >
          <ListChecks style={{ width: 9, height: 9 }} />
          Sync
        </button>
        <button
          onClick={(e) => { e.stopPropagation(); Promise.resolve(onSuggestTasks()).then(bumpRefresh); }}
          title="Experimental: propose new goals for this character from their journal and the story's lore"
          style={{
            display: "flex", alignItems: "center", gap: 3, fontSize: 9, padding: "2px 6px",
            background: "transparent", border: "1px solid var(--border)", borderRadius: 4,
            color: "var(--text-label)", cursor: "pointer",
          }}
        >
          <Lightbulb style={{ width: 9, height: 9 }} />
          Suggest
        </button>
      </div>
      <TasksEditor branch={branch} refreshToken={refreshToken} />
    </div>
  );
}
