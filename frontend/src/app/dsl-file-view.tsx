"use client";

// The DSL surface: how a `.dsl` file is edited (see lib/fileSurface.ts —
// this replaces the prose/atom view outright rather than being a mode
// inside it). Two reasons, one per layer:
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
//
// 'DslSidebar' below is this surface's own sidebar content — the resolved
// output of whatever is currently in the editor. The editor's buffer
// travels between the two through uiStore's dslDraft cell rather than a
// shared parent: page.tsx owns the panel, not what's in it.

import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import { Save } from "lucide-react";
import { branchFileUrl, saveRawFileWhole } from "@/lib/ws";
import type { LineCost, PreviewNode } from "@/lib/ws";
import { useServerCache } from "@/lib/serverCacheStore";
import { useUI } from "@/lib/uiStore";
import { classifyBranch } from "@/lib/branches";
import { contextsBranchName } from "@/lib/contextBranch";
import { CodeCostEditor, useAdhocCostFetcher, useAdhocPreviewFetcher } from "./code-cost-editor";

// Branches a program can sensibly be resolved against: story branches only.
// A character branch holds one character's own journal/sheet, the prompts
// and contexts branches hold definitions rather than story content — none
// of them are what a real `chat.writer` call runs against.
function resolvableBranches(branches: string[]): string[] {
  return branches.filter((b) => classifyBranch(b) === "story" && b !== contextsBranchName);
}

// What a program's own declared parameter is bound to when previewing or
// costing here (see Storyteller.Core.Context.resolveAdhoc). A `.dsl` file
// is edited on its own, against no particular story file, so the honest
// answer is the empty glob: it resolves to nothing, rather than to some
// arbitrary file the user never chose. Sending nothing at all is the one
// wrong answer — a `path:`-headed program (every custom agent's, and
// `context.other`'s) would have no argument to bind, and its preview and
// cost would both come back empty for a reason that has nothing to do
// with what it says.
const ADHOC_PATH_ARG = "[]";

export function DslFileView({ branch, path }: {
  branch: string;
  path: string;
}) {
  const branches         = useServerCache((s) => s.branches);
  const storedResolve    = useUI((s) => s.dslResolveBranch);
  const setStoredResolve = useUI((s) => s.setDslResolveBranch);
  const setDslDraft      = useUI((s) => s.setDslDraft);
  const clearDslDraft    = useUI((s) => s.clearDslDraft);

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
    const costs = await fetchCosts(program, ADHOC_PATH_ARG);
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

  // Publish the buffer for this surface's own sidebar (see DslSidebar).
  // Cleared on unmount so nothing downstream can read a draft belonging to
  // an editor that's no longer open.
  useEffect(() => {
    if (content !== null) setDslDraft(path, content);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [path, content]);
  useEffect(() => clearDslDraft, [clearDslDraft]);

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

// ── Sidebar ───────────────────────────────────────────────────────────────────

// This surface's own sidebar: what the program in the editor actually
// resolves to, on the branch the editor is pointed at. The counterpart to
// the gutter's per-statement cost — that answers "what does this line cost",
// this answers "what does the whole thing produce", which is the question
// you have while writing one.
//
// Resolved server-side through the same `context.preview.adhoc` command a
// real 0-arity slot resolution uses (Storyteller.Writer.Agent.ContextPreview.
// buildAdhocPreview), so a name reference inside the program (context.lore,
// say) sees this project's own committed override exactly as a real send
// would — never a client-side re-implementation of the DSL's semantics.
export function DslSidebar({ branch, path }: { branch: string; path: string }) {
  const draft         = useUI((s) => s.dslDraft);
  const storedResolve = useUI((s) => s.dslResolveBranch);
  const branches      = useServerCache((s) => s.branches);

  const candidates = useMemo(() => resolvableBranches(branches), [branches]);
  const resolveBranch =
    (storedResolve && candidates.includes(storedResolve)) ? storedResolve
    : candidates.includes(branch) ? branch
    : candidates[0] ?? null;

  const fetchPreview = useAdhocPreviewFetcher(resolveBranch);
  const [node, setNode] = useState<PreviewNode | null>(null);
  const [stale, setStale] = useState(false);
  const [failed, setFailed] = useState(false);
  const requestIdRef = useRef(0);

  // Only this editor's own draft — a stale cell from a previously open file
  // must never be rendered as if it were this one's.
  const program = draft && draft.path === path ? draft.text : null;

  // Debounced, superseded-request-safe — the same shape (and the same
  // reason) as CodeCostEditor's own cost fetch: a program mid-keystroke is
  // usually unparseable, and every resolution is a real server-side read.
  useEffect(() => {
    if (!program || !program.trim()) { setNode(null); setFailed(false); return; }
    setStale(true);
    const requestId = ++requestIdRef.current;
    const t = setTimeout(async () => {
      const result = await fetchPreview(program, ADHOC_PATH_ARG);
      if (requestId !== requestIdRef.current) return;
      setStale(false);
      setFailed(result === null);
      if (result !== null) setNode(result);
    }, PREVIEW_DEBOUNCE_MS);
    return () => clearTimeout(t);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [program, resolveBranch]);

  const totalChars = node ? countChars(node) : 0;

  return (
    <div style={{ flex: 1, minHeight: 0, display: "flex", flexDirection: "column", padding: "8px 8px 0" }}>
      <div style={{ flexShrink: 0, display: "flex", alignItems: "baseline", gap: 6, padding: "0 2px 6px" }}>
        <span style={{ fontSize: 11, color: "var(--text-label)" }}>Resolves to</span>
        <code style={{ fontSize: 9.5, fontFamily: "monospace", color: "var(--text-ghost)" }}>
          {resolveBranch ?? "no story branch"}
        </code>
        <span style={{ flex: 1 }} />
        {node && (
          <span style={{ fontSize: 9.5, color: stale ? "var(--text-ghost)" : "var(--amber)" }}>
            ≈{Math.round(totalChars / 4).toLocaleString()}t
          </span>
        )}
      </div>

      <div style={{ flex: 1, minHeight: 0, overflow: "auto", fontSize: 11, lineHeight: 1.5 }}>
        {failed ? (
          <div style={{ padding: "8px 2px", color: "var(--text-ghost)", fontStyle: "italic" }}>
            Doesn&apos;t resolve — an unfinished line, or a name this branch has no definition for.
          </div>
        ) : !node ? (
          <div style={{ padding: "8px 2px", color: "var(--text-ghost)", fontStyle: "italic" }}>
            {program ? "Resolving…" : "Nothing to resolve yet."}
          </div>
        ) : (
          <PreviewTree node={node} depth={0} stale={stale} />
        )}
      </div>
    </div>
  );
}

const PREVIEW_DEBOUNCE_MS = 700;

// Every rendered character under a node — the same "sum what's actually
// produced" figure the editor's own header shows from the cost side, so
// the two can be read against each other.
function countChars(node: PreviewNode): number {
  return node.content.reduce((sum, t) => sum + t.length, 0)
       + node.entries.reduce((sum, e) => sum + countChars(e.node), 0);
}

// One node of the resolved structure: its own text, then its named
// children — mirroring Storyteller.Context.DSL.Rendering.RenderedContext
// (and PreviewNode) exactly, rather than flattening it, since the `as
// "name":` structure is a real part of what the program produces.
function PreviewTree({ node, depth, stale }: { node: PreviewNode; depth: number; stale: boolean }) {
  return (
    <div style={{ opacity: stale ? 0.5 : 1, transition: "opacity 0.15s" }}>
      {node.content.map((text, i) => (
        <div key={i} style={{
          whiteSpace: "pre-wrap", wordBreak: "break-word",
          color: "var(--text-body)", padding: "2px 2px 6px",
          borderLeft: depth > 0 ? "1px solid var(--border-subtle)" : undefined,
          paddingLeft: depth > 0 ? 8 : 2,
        }}>
          {text}
        </div>
      ))}
      {node.entries.map((entry) => (
        <div key={entry.name} style={{ paddingLeft: depth > 0 ? 8 : 0, borderLeft: depth > 0 ? "1px solid var(--border-subtle)" : undefined }}>
          <div style={{ fontSize: 9.5, fontFamily: "monospace", color: "var(--sky)", padding: "2px 2px 1px" }}>
            {entry.name}
          </div>
          <PreviewTree node={entry.node} depth={depth + 1} stale={stale} />
        </div>
      ))}
    </div>
  );
}
