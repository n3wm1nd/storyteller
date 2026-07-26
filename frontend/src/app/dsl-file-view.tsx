"use client";

// How a `.dsl` file looks when opened in the file view — a code editor,
// not the atom/prose surface every other file gets (page.tsx dispatches on
// the extension). Two reasons, one per layer:
//
//   * A Context DSL program has no internal structure worth tracking piece
//     by piece. Saves go through `saveRawFileWhole` (PUT /$raw/...?whole →
//     Storage.Ops.saveWholeFile), which keeps the file at exactly one atom
//     holding the whole source, so there are no paragraph-shaped atoms for
//     an atom view to show in the first place. An ordinary $raw save would
//     mint one atom per added stanza — right for prose, meaningless here.
//   * What you want to see for a program is its source and what it costs,
//     not a chat composer and presence bars. So: CodeMirror with the
//     per-statement cost gutter (code-cost-editor.tsx), and nothing else.
//
// The branch dropdown is the one thing this needs that no other editor
// does. A `context/*.dsl` lives on the *contexts* branch, which has no
// story content at all — costing the program against the branch the file
// happens to sit on would measure nothing. So the branch a real call would
// resolve against is chosen explicitly here, independent of the file's own
// branch, and remembered across file switches (uiStore's dslResolveBranch).

import { useCallback, useEffect, useMemo, useState } from "react";
import { Save } from "lucide-react";
import { branchFileUrl, saveRawFileWhole } from "@/lib/ws";
import type { LineCost } from "@/lib/ws";
import { useServerCache } from "@/lib/serverCacheStore";
import { useUI } from "@/lib/uiStore";
import { classifyBranch } from "@/lib/branches";
import { contextsBranchName } from "@/lib/contextBranch";
import { CodeCostEditor, useAdhocCostFetcher } from "./code-cost-editor";

// Is this a file the DSL editor owns? One place, so page.tsx's dispatch and
// anything else asking the same question can't drift apart.
export function isDslFile(path: string): boolean {
  return path.endsWith(".dsl");
}

// Branches a program can sensibly be resolved against: story branches only.
// A character branch holds one character's own journal/sheet, the prompts
// and contexts branches hold definitions rather than story content — none
// of them are what a real `chat.writer` call runs against.
function resolvableBranches(branches: string[]): string[] {
  return branches.filter((b) => classifyBranch(b) === "story" && b !== contextsBranchName);
}

export function DslFileView({ branch, path }: {
  branch: string;
  path: string;
}) {
  const branches         = useServerCache((s) => s.branches);
  const storedResolve    = useUI((s) => s.dslResolveBranch);
  const setStoredResolve = useUI((s) => s.setDslResolveBranch);

  const [content, setContent] = useState<string | null>(null);
  const [savedContent, setSavedContent] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [totalChars, setTotalChars] = useState<number | null>(null);

  const candidates = useMemo(() => resolvableBranches(branches), [branches]);

  // The stored choice only counts while it still names a live branch;
  // otherwise fall back to the file's own branch (right when a .dsl is
  // being edited on a story branch directly) and then to whatever story
  // branch there is. Deliberately derived on every render rather than
  // written back into the store on load: the store holds what the user
  // *chose*, and a transiently missing branch shouldn't overwrite it.
  const resolveBranch =
    (storedResolve && candidates.includes(storedResolve)) ? storedResolve
    : candidates.includes(branch) ? branch
    : candidates[0] ?? null;

  const fetchCosts = useAdhocCostFetcher(resolveBranch);

  // Same per-statement costs the gutter draws, summed for the header — one
  // request, two readers, rather than a second estimate of its own.
  const fetchAndTotal = useCallback(async (program: string): Promise<LineCost[] | null> => {
    const costs = await fetchCosts(program);
    setTotalChars(costs ? costs.reduce((sum, c) => sum + Math.max(0, c.chars), 0) : null);
    return costs;
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [resolveBranch]);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    setContent(null);
    fetch(branchFileUrl(branch, path))
      .then((res) => {
        // A .dsl that doesn't exist yet is an empty new program, not an
        // error — the file view reaches here for an absent path too.
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
  }, [branch, path]);

  const dirty = content !== null && content !== savedContent;

  const save = useCallback(() => {
    if (content === null || saving) return;
    setSaving(true);
    setError(null);
    saveRawFileWhole(branch, path, content)
      .then(() => { setSavedContent(content); setSaving(false); })
      .catch((err) => {
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }, [branch, path, content, saving]);

  return (
    <div
      style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}
      onKeyDown={(e) => {
        if ((e.metaKey || e.ctrlKey) && e.key === "s") { e.preventDefault(); save(); }
      }}
    >
      <div style={{
        flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8, fontSize: 10,
      }}>
        <span style={{ color: "var(--text-ghost)" }}>Context DSL — the whole file is one atom</span>

        <span style={{ flex: 1 }} />

        {totalChars !== null && resolveBranch && (
          <span style={{ color: "var(--text-ghost)" }} title="Sum of the per-statement estimates in the gutter, at ~4 chars/token">
            ≈{Math.round(totalChars / 4).toLocaleString()} tokens
          </span>
        )}

        <label style={{ display: "flex", alignItems: "center", gap: 4, color: "var(--text-ghost)" }}>
          resolve against
          <select
            value={resolveBranch ?? ""}
            onChange={(e) => setStoredResolve(e.target.value)}
            disabled={candidates.length === 0}
            title="Which branch's story content the cost estimate resolves this program against — a context/*.dsl lives on the contexts branch, which has none of its own"
            style={{
              fontSize: 10, padding: "1px 4px", borderRadius: 4,
              border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
              color: "var(--text-dim)", fontFamily: "monospace", outline: "none",
            }}
          >
            {candidates.length === 0 && <option value="">no story branch</option>}
            {candidates.map((b) => <option key={b} value={b}>{b}</option>)}
          </select>
        </label>

        {error && <span style={{ color: "var(--rose)" }}>{error}</span>}
        {dirty && !error && <span style={{ color: "var(--amber)" }}>unsaved</span>}
        <button
          onClick={save}
          disabled={!dirty || saving}
          title="Save the whole program as this file's single atom (⌘S / Ctrl+S)"
          style={{
            display: "inline-flex", alignItems: "center", gap: 4,
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "var(--amber-tint)" : "transparent",
            border: "1px solid " + (dirty ? "var(--amber-border)" : "var(--border-subtle)"),
            color: dirty ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          <Save style={{ width: 10, height: 10 }} />
          {saving ? "Saving…" : "Save"}
        </button>
      </div>

      {loading ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          Loading…
        </div>
      ) : (
        <CodeCostEditor
          value={content ?? ""}
          onChange={setContent}
          disabled={saving}
          fill
          placeholder="# A Context DSL program. See CONTEXT-DSL.md for the syntax."
          fetchCosts={fetchAndTotal}
        />
      )}
    </div>
  );
}
