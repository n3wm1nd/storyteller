"use client";

// Per-(agent, context-source) exclude/include-only config, e.g. "Writer's
// story-branch (ambient) context". Each agent has a small, fixed set of real
// context sources (see lib/agents.ts) — this widget configures exactly one
// of them, it never invents new sources or lets the user rename/retarget it.
//
// The pattern list is just what gets filtered from that source's existing
// file listing — not a general slot/label mechanism (see prior design
// discussion: there's no backend concept of an arbitrarily-named,
// glob-populated "slot"). "Invert" swaps the same pattern list between
// exclude (hide these) and include-only (show only these). Patterns are
// entered one at a time (Enter commits the current input as a removable tag,
// Backspace on an empty input pops the last one) and still persist as the
// same newline-joined string via lib/settingsStore.ts — only the input
// widget is tokenized, not the underlying representation.
//
// Layout is a single column, not the old side-by-side split: the pattern bar
// only takes the height its content needs (wraps, never scrolls). The tree
// below is content-sized too (capped, with its own internal scrollbar past
// that) rather than flex-stretched to fill the detail pane — any leftover
// space belongs below the Prompts section on the page, not inside this box.
//
// Excluded files are never removed from the tree — 'ContextEntry.included'
// (see ws.ts, sourced from 'Storyteller.Writer.Agent.ContextPreview') marks
// them instead, so they render shaded in place. Clicking a file toggles the
// exact-path pattern; clicking a folder (its icon/name, not the expand
// chevron) toggles a "folder/**/*" pattern — both just call the same
// addPattern/removeTag pair the typed input uses.
//
// The filter itself persists client-side via lib/settingsStore.ts (nothing
// server-side stores it yet). The live preview it drives does not: that's a
// single-consumer, request/response resolve against the current file (see
// Server.Writer.ContextView's module header — every request is
// self-contained), so it stays local component state like
// lib/serverCacheStore.ts's "one connection per component" convention, just
// without a global store slice to mirror into.

import { useEffect, useRef, useState } from "react";
import { ChevronRight, Folder, FolderOpen, FileText, RefreshCw, X } from "lucide-react";
import { contextViewConn } from "@/lib/ws";
import type { ContextSlotPreview, ContextMode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { useSettings, contextFilterKey, type ContextFilter } from "@/lib/settingsStore";
import { buildTree, type TreeNode } from "./filetree";

const DEFAULT_FILTER: ContextFilter = { patterns: "", invert: false };

function isHidden(name: string): boolean {
  return name.startsWith(".");
}

function countFiles(node: TreeNode): number {
  return node.isDir ? node.children.reduce((n, c) => n + countFiles(c), 0) : 1;
}

function PreviewTreeNode({ node, depth, onToggleFile, onToggleFolder }: {
  node: TreeNode;
  depth: number;
  onToggleFile: (path: string) => void;
  onToggleFolder: (path: string) => void;
}) {
  const [open, setOpen] = useState(true);
  const pad = 8 + depth * 14;
  if (isHidden(node.name)) return null;

  if (node.isDir) {
    return (
      <div>
        <div style={{ display: "flex", alignItems: "center" }}>
          <button
            onClick={() => setOpen((v) => !v)}
            title={open ? "Collapse" : "Expand"}
            style={{
              display: "flex", alignItems: "center", flexShrink: 0,
              padding: `2px 2px 2px ${pad}px`, border: "none", background: "transparent",
              cursor: "pointer", color: "var(--text-dim)",
            }}
          >
            <ChevronRight style={{ width: 10, height: 10, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
          </button>
          <button
            onClick={() => onToggleFolder(node.path)}
            title={`Toggle ${node.path}/**/* in the pattern list`}
            style={{
              display: "flex", alignItems: "center", gap: 5, flex: 1, minWidth: 0, textAlign: "left",
              padding: "2px 8px 2px 2px", border: "none", background: "transparent",
              cursor: "pointer", borderRadius: 5, color: "var(--text-muted)", fontSize: 11,
            }}
          >
            {open ? <FolderOpen style={{ width: 11, height: 11, flexShrink: 0 }} /> : <Folder style={{ width: 11, height: 11, flexShrink: 0 }} />}
            <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
            <span style={{ marginLeft: "auto", fontSize: 9, color: "var(--text-ghost)" }}>{countFiles(node)}</span>
          </button>
        </div>
        {open && node.children.map((child) => (
          <PreviewTreeNode key={child.path} node={child} depth={depth + 1} onToggleFile={onToggleFile} onToggleFolder={onToggleFolder} />
        ))}
      </div>
    );
  }

  return (
    <button
      onClick={() => onToggleFile(node.path)}
      title={`Toggle ${node.path} in the pattern list`}
      style={{
        display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
        padding: `2px 8px 2px ${pad}px`, border: "none", background: "transparent", cursor: "pointer",
        color: node.included ? "var(--text-secondary)" : "var(--text-faint)",
        opacity: node.included ? 1 : 0.55,
        textDecoration: node.included ? "none" : "line-through",
      }}
    >
      <FileText style={{ width: 10, height: 10, flexShrink: 0, opacity: 0.6 }} />
      <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
    </button>
  );
}

export function ContextSourceConfig({ activeBranch, path, sourceId, label, mode }: {
  activeBranch: string | null;
  path: string;
  sourceId: string;
  label: string;
  mode: ContextMode;
}) {
  const filterKey = activeBranch ? contextFilterKey(activeBranch, sourceId) : null;
  const { patterns, invert } = useSettings((s) => (filterKey ? s.contextFilters[filterKey] : undefined) ?? DEFAULT_FILTER);
  const setContextFilter = useSettings((s) => s.setContextFilter);
  const [preview, setPreview] = useState<ContextSlotPreview | null>(null);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const [draft, setDraft] = useState("");

  const tags = patterns.split("\n").map((s) => s.trim()).filter(Boolean);

  function send(conn: ReturnType<typeof contextViewConn>, pat: string, inv: boolean) {
    const list = pat.split("\n").map((s) => s.trim()).filter(Boolean);
    conn.send({
      type: "context.preview",
      slots: [{ label, mode, filter: inv ? { include: list, exclude: [] } : { include: [], exclude: list } }],
    });
  }

  useEffect(() => {
    if (!activeBranch) return;
    const connLabel = `context:${sourceId}:${path}`;
    setConnStatus(connLabel, "connecting");

    const conn = contextViewConn(activeBranch, path);
    connRef.current = conn;
    setPreview(null);

    conn.onStatus((s) => {
      if (s !== "connected") setConnStatus(connLabel, "connecting");
    });
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.preview") setPreview(evt.slots[0] ?? null);
      else if (evt.type === "error") setError(evt.message);
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(connLabel, "connected");
        send(conn, patterns, invert);
      } catch (err) {
        setConnStatus(connLabel, "error");
        setError(String(err));
      }
    })();

    return () => {
      conn.close();
      connRef.current = null;
      removeConn(connLabel);
    };
    // patterns/invert are re-sent explicitly on every edit (see the two
    // onChange handlers below) — this effect only re-runs on target change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeBranch, path, sourceId]);

  function commitPatterns(next: string) {
    if (filterKey) setContextFilter(filterKey, { patterns: next, invert });
    if (connRef.current) send(connRef.current, next, invert);
  }

  function commitInvert(next: boolean) {
    if (filterKey) setContextFilter(filterKey, { patterns, invert: next });
    if (connRef.current) send(connRef.current, patterns, next);
  }

  function addPattern(val: string) {
    if (!val || tags.includes(val)) return;
    commitPatterns([...tags, val].join("\n"));
  }

  function removeTag(tag: string) {
    commitPatterns(tags.filter((t) => t !== tag).join("\n"));
  }

  function togglePattern(val: string) {
    if (tags.includes(val)) removeTag(val); else addPattern(val);
  }

  function addDraft() {
    const val = draft.trim();
    setDraft("");
    addPattern(val);
  }

  function onDraftKeyDown(e: React.KeyboardEvent<HTMLInputElement>) {
    if (e.key === "Enter") {
      e.preventDefault();
      addDraft();
    } else if (e.key === "Backspace" && draft === "" && tags.length > 0) {
      removeTag(tags[tags.length - 1]);
    }
  }

  const includedPaths = new Set(preview?.entries.filter((e) => e.included).map((e) => e.path) ?? []);
  const tree = preview ? buildTree(preview.entries.map((e) => e.path), undefined, includedPaths) : [];

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      <div style={{ flexShrink: 0, padding: 8, display: "flex", flexDirection: "column", gap: 6, borderBottom: "1px solid var(--border-subtle)" }}>
        <div style={{
          display: "flex", flexWrap: "wrap", alignItems: "center", gap: 4,
          padding: "3px 5px", background: "var(--card)",
          border: "1px solid var(--border-subtle)", borderRadius: 5,
        }}>
          {tags.map((tag) => (
            <span key={tag} style={{
              display: "flex", alignItems: "center", gap: 3, fontSize: 10.5, fontFamily: "monospace",
              padding: "2px 3px 2px 7px", borderRadius: 9, background: "var(--surface)", color: "var(--text-secondary)",
            }}>
              {tag}
              <button
                onClick={() => removeTag(tag)}
                title="Remove"
                style={{ display: "flex", border: "none", background: "none", cursor: "pointer", color: "var(--text-ghost)", padding: 2 }}
              >
                <X style={{ width: 9, height: 9 }} />
              </button>
            </span>
          ))}
          <input
            value={draft}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={onDraftKeyDown}
            onBlur={addDraft}
            placeholder={tags.length === 0 ? (invert ? "pattern to show, enter to add" : "pattern to hide, enter to add") : "add another…"}
            style={{
              flex: 1, minWidth: 100, border: "none", outline: "none", background: "transparent",
              fontSize: 11, fontFamily: "monospace", color: "var(--foreground)", padding: "3px 2px",
            }}
          />
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 10 }}>
          <label style={{ display: "flex", alignItems: "center", gap: 5, fontSize: 10.5, color: "var(--text-muted)", cursor: "pointer" }}>
            <input
              type="checkbox" checked={invert} onChange={(e) => commitInvert(e.target.checked)}
              style={{ accentColor: "var(--amber)", colorScheme: "dark" }}
            />
            Show only these, hide everything else
          </label>
          <span style={{ fontSize: 9.5, color: "var(--text-ghost)" }}>
            use <code>**/*</code> to reach files at any depth
          </span>
        </div>
      </div>

      <div style={{ maxHeight: 340, overflow: "auto" }}>
        {preview === null ? (
          <div style={{ display: "flex", alignItems: "center", gap: 6, padding: 12, fontSize: 11, color: "var(--text-ghost)" }}>
            <RefreshCw style={{ width: 11, height: 11 }} className="animate-spin" /> resolving…
          </div>
        ) : preview.entries.length === 0 ? (
          <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: 12 }}>no files in this branch</div>
        ) : (
          <div style={{ padding: "6px 4px" }}>
            {tree.map((node) => (
              <PreviewTreeNode
                key={node.path} node={node} depth={0}
                onToggleFile={togglePattern}
                onToggleFolder={(p) => togglePattern(`${p}/**/*`)}
              />
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
