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
// exclude (hide these) and include-only (show only these), so one textarea
// covers both cases without separate include/exclude fields.
//
// The filter itself persists client-side via lib/settingsStore.ts (nothing
// server-side stores it yet). The live preview it drives does not: that's a
// single-consumer, request/response resolve against the current file (see
// Server.Writer.ContextView's module header — every request is
// self-contained), so it stays local component state like
// lib/serverCacheStore.ts's "one connection per component" convention, just
// without a global store slice to mirror into.

import { useEffect, useLayoutEffect, useRef, useState } from "react";
import { ChevronRight, Folder, FolderOpen, FileText, RefreshCw } from "lucide-react";
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

function PreviewTreeNode({ node, depth }: { node: TreeNode; depth: number }) {
  const [open, setOpen] = useState(true);
  const pad = 8 + depth * 14;
  if (isHidden(node.name)) return null;

  if (node.isDir) {
    return (
      <div>
        <button
          onClick={() => setOpen((v) => !v)}
          style={{
            display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
            padding: `2px 8px 2px ${pad}px`, border: "none", background: "transparent",
            cursor: "pointer", borderRadius: 5, color: "var(--text-muted)", fontSize: 11,
          }}
        >
          <ChevronRight style={{ width: 10, height: 10, flexShrink: 0, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
          {open ? <FolderOpen style={{ width: 11, height: 11, flexShrink: 0 }} /> : <Folder style={{ width: 11, height: 11, flexShrink: 0 }} />}
          <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
          <span style={{ marginLeft: "auto", fontSize: 9, color: "var(--text-ghost)" }}>{countFiles(node)}</span>
        </button>
        {open && node.children.map((child) => <PreviewTreeNode key={child.path} node={child} depth={depth + 1} />)}
      </div>
    );
  }

  return (
    <div style={{
      display: "flex", alignItems: "center", gap: 5, padding: `2px 8px 2px ${pad}px`,
      color: "var(--text-secondary)", fontSize: 11,
    }}>
      <FileText style={{ width: 10, height: 10, flexShrink: 0, opacity: 0.6 }} />
      <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
    </div>
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
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  // Grows with content instead of being user-resizable — a resize handle on
  // a narrow sidebar column just invites dragging it into the tree below.
  useLayoutEffect(() => {
    const el = textareaRef.current;
    if (!el) return;
    el.style.height = "auto";
    el.style.height = `${el.scrollHeight}px`;
  }, [patterns]);

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

  const tree = preview ? buildTree(preview.entries.map((e) => e.path)) : [];

  return (
    <div style={{ height: "100%", display: "flex", overflow: "hidden" }}>
      <div style={{ width: 260, minWidth: 260, height: "100%", display: "flex", flexDirection: "column", borderRight: "1px solid var(--border-subtle)", overflow: "auto" }}>
        <textarea
          ref={textareaRef}
          value={patterns}
          onChange={(e) => commitPatterns(e.target.value)}
          placeholder={invert ? "one glob per line — only these are shown" : "one glob per line — these are hidden"}
          rows={3}
          style={{
            margin: 8, fontSize: 11, padding: "5px 7px", background: "var(--card)",
            border: "1px solid var(--border-subtle)", borderRadius: 5,
            color: "var(--foreground)", outline: "none", resize: "none", overflow: "hidden", fontFamily: "monospace",
          }}
        />
        <label style={{ display: "flex", alignItems: "center", gap: 5, margin: "0 8px 8px", fontSize: 10.5, color: "var(--text-muted)", cursor: "pointer" }}>
          <input type="checkbox" checked={invert} onChange={(e) => commitInvert(e.target.checked)} />
          Invert (include only these, hide everything else)
        </label>
        <div style={{ margin: "0 8px 8px", fontSize: 9.5, color: "var(--text-ghost)", lineHeight: 1.4 }}>
          <code>**</code> alone matches directories, not files — use <code>**/*</code> to reach files at any depth (e.g. <code>characters/**/*</code>).
        </div>
      </div>

      <div style={{ flex: 1, overflow: "auto" }}>
        {preview === null ? (
          <div style={{ display: "flex", alignItems: "center", gap: 6, padding: 12, fontSize: 11, color: "var(--text-ghost)" }}>
            <RefreshCw style={{ width: 11, height: 11 }} className="animate-spin" /> resolving…
          </div>
        ) : preview.entries.length === 0 ? (
          <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: 12 }}>no files match</div>
        ) : (
          <div style={{ padding: "6px 4px" }}>
            {tree.map((node) => <PreviewTreeNode key={node.path} node={node} depth={0} />)}
          </div>
        )}
      </div>
    </div>
  );
}
