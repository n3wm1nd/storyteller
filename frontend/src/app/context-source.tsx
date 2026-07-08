"use client";

// Per-(agent, context-source) bucket-picker config, e.g. "Writer's
// story-branch (ambient) context". Each agent has a small, fixed set of real
// context sources (see lib/agents.ts) — this widget configures exactly one
// of them, it never invents new sources or lets the user rename/retarget it.
//
// The tag list is a set of globs (see lib/settingsStore.ts's FilterTag) that
// get compiled into a PickerRule[] layout (lib/settingsStore.ts's
// toContextLayout) — same primitive Storyteller.Writer.Agent.ContextFilter
// uses server-side to claim-and-order a real chat.writer call's context, so
// what this widget previews is exactly what generation would see, not an
// approximation of it.
//
// One layer of control: a tag's bucket badge lets a user promote it from
// trash (the default for an untouched tag) to its own numbered group —
// click cycles trash -> 1 -> 2 -> 3 -> back to trash. Buckets are assembled
// into the final prompt in ascending order (bucket 1's files come first),
// so a lower number means "earlier in the prompt", not "higher priority".
//
// Patterns are matched in the order the tags list is in (first match wins,
// see toContextLayout/settingsStore.ts) — this is also what used to need a
// separate include/exclude-only toggle: put a broad `**/*` tag *after*
// specific ones to reproduce "hide these" (specific patterns claim their
// bucket or trash first, `**/*` mops up whatever's left); leave it out
// entirely to reproduce "show only these" (anything not explicitly claimed
// stays trashed, no catch-all needed).
//
// Layout is a single column, not the old side-by-side split: the pattern bar
// only takes the height its content needs (wraps, never scrolls). The tree
// below is content-sized too (capped, with its own internal scrollbar past
// that) rather than flex-stretched to fill the detail pane — any leftover
// space belongs below the Prompts section on the page, not inside this box.
//
// Trashed files are never removed from the tree — 'ContextEntry.bucket'
// (see ws.ts, sourced from 'Storyteller.Writer.Agent.ContextPreview') is
// null for them instead, so they render shaded in place; a claimed file
// shows a small badge for the bucket it landed in, same colour as that
// bucket's tags. Clicking a file toggles the exact-path pattern; clicking a
// folder (its icon/name, not the expand chevron) toggles a "folder/**/*"
// pattern — both just call the same addPattern/removeTag pair the typed
// input uses, defaulting to the current mode like any other new tag.
//
// The filter itself persists client-side via lib/settingsStore.ts (nothing
// server-side stores it yet). The live preview it drives does not: that's a
// single-consumer, request/response resolve against the current file (see
// Server.Writer.ContextView's module header — every request is
// self-contained), so it stays local component state like
// lib/serverCacheStore.ts's "one connection per component" convention, just
// without a global store slice to mirror into.

import { useEffect, useRef, useState } from "react";
import { ChevronRight, Folder, FolderOpen, FileText, RefreshCw, X, Trash2 } from "lucide-react";
import { contextViewConn } from "@/lib/ws";
import type { ContextSlotPreview, ContextMode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import {
  useSettings, contextFilterKey, defaultBucket, toContextLayout,
  type ContextFilter, type FilterTag,
} from "@/lib/settingsStore";
import { buildTree, type TreeNode } from "./filetree";

const DEFAULT_FILTER: ContextFilter = { tags: [] };

// Cycle order for a tag badge click: trash, then bucket 1..MAX_BUCKET, then
// back to trash. Small on purpose — this is for "a handful of named
// groups" (WRITER.md's outline/notes/chapters split has three), not a
// general-purpose numbering scheme; nothing stops a bucket above this from
// being reached by editing settings storage directly; a badge can only
// reach one via serial clicks.
const MAX_BUCKET = 4;

// Stable colour per bucket number, cycling through a small palette — used
// for both a tag's own badge and a matching file's badge in the tree, so
// the two are visually the same group at a glance. Trash gets its own fixed
// (muted/red) treatment, never one of these.
const BUCKET_HUES = [65, 200, 320, 140]; // amber, blue, magenta, green

function bucketColor(bucket: number, alpha = 1): string {
  const hue = BUCKET_HUES[(bucket - 1) % BUCKET_HUES.length];
  return `oklch(0.75 0.13 ${hue} / ${alpha})`;
}

function nextBucket(current: number | null): number | null {
  if (current === null) return 1;
  if (current >= MAX_BUCKET) return null;
  return current + 1;
}

function isHidden(name: string): boolean {
  return name.startsWith(".");
}

function countFiles(node: TreeNode): number {
  return node.isDir ? node.children.reduce((n, c) => n + countFiles(c), 0) : 1;
}

// Small round badge shared by tag chips and tree entries — a number on its
// bucket's colour, or a trash icon in a muted/red treatment. `onClick`
// absent renders it inert (used in the tree, which only ever displays a
// file's resolved bucket, never edits it directly).
function BucketBadge({ bucket, onClick, title }: {
  bucket: number | null;
  onClick?: () => void;
  title: string;
}) {
  const interactive = !!onClick;
  const style: React.CSSProperties = {
    display: "flex", alignItems: "center", justifyContent: "center",
    width: 15, height: 15, borderRadius: "50%", flexShrink: 0,
    fontSize: 9, fontWeight: 700, fontFamily: "monospace",
    border: "none", padding: 0, cursor: interactive ? "pointer" : "default",
    background: bucket === null ? "oklch(0.55 0.15 25 / 0.18)" : bucketColor(bucket, 0.22),
    color: bucket === null ? "oklch(0.65 0.18 25)" : bucketColor(bucket),
  };
  const content = bucket === null ? <Trash2 style={{ width: 9, height: 9 }} /> : bucket;
  return interactive ? (
    <button onClick={onClick} title={title} style={style}>{content}</button>
  ) : (
    <span title={title} style={style}>{content}</span>
  );
}

function PreviewTreeNode({ node, depth, bucketOf, onToggleFile, onToggleFolder }: {
  node: TreeNode;
  depth: number;
  bucketOf: (path: string) => number | null | undefined; // undefined = not in the preview at all
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
          <PreviewTreeNode key={child.path} node={child} depth={depth + 1} bucketOf={bucketOf} onToggleFile={onToggleFile} onToggleFolder={onToggleFolder} />
        ))}
      </div>
    );
  }

  const bucket = bucketOf(node.path) ?? null;
  const claimed = bucket !== null;
  return (
    <button
      onClick={() => onToggleFile(node.path)}
      title={`Toggle ${node.path} in the pattern list`}
      style={{
        display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
        padding: `2px 8px 2px ${pad}px`, border: "none", background: "transparent", cursor: "pointer",
        color: claimed ? "var(--text-secondary)" : "var(--text-faint)",
        opacity: claimed ? 1 : 0.55,
        textDecoration: claimed ? "none" : "line-through",
      }}
    >
      <FileText style={{ width: 10, height: 10, flexShrink: 0, opacity: 0.6 }} />
      <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", flex: 1 }}>{node.name}</span>
      <BucketBadge bucket={bucket} title={claimed ? `bucket ${bucket}` : "hidden from context"} />
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
  const filter = useSettings((s) => (filterKey ? s.contextFilters[filterKey] : undefined) ?? DEFAULT_FILTER);
  const setContextFilter = useSettings((s) => s.setContextFilter);
  const [preview, setPreview] = useState<ContextSlotPreview | null>(null);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const [draft, setDraft] = useState("");

  const { tags } = filter;

  function send(conn: ReturnType<typeof contextViewConn>, f: ContextFilter) {
    conn.send({
      type: "context.preview",
      slots: [{ label, mode, layout: toContextLayout(f) }],
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
        send(conn, filter);
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
    // filter is re-sent explicitly on every edit (see commitFilter below) —
    // this effect only re-runs on target change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeBranch, path, sourceId]);

  function commitFilter(next: ContextFilter) {
    if (filterKey) setContextFilter(filterKey, next);
    if (connRef.current) send(connRef.current, next);
  }

  function addPattern(val: string) {
    if (!val || tags.some((t) => t.pattern === val)) return;
    commitFilter({ tags: [...tags, { pattern: val }] });
  }

  function removeTag(pattern: string) {
    commitFilter({ tags: tags.filter((t) => t.pattern !== pattern) });
  }

  function togglePattern(val: string) {
    if (tags.some((t) => t.pattern === val)) removeTag(val); else addPattern(val);
  }

  function cycleTagBucket(pattern: string) {
    commitFilter({
      tags: tags.map((t) => {
        if (t.pattern !== pattern) return t;
        const current = t.bucket !== undefined ? t.bucket : defaultBucket();
        return { ...t, bucket: nextBucket(current) };
      }),
    });
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
      removeTag(tags[tags.length - 1].pattern);
    }
  }

  const bucketByPath = new Map(preview?.entries.map((e) => [e.path, e.bucket] as const) ?? []);
  const claimedPaths = new Set(preview?.entries.filter((e) => e.bucket !== null).map((e) => e.path) ?? []);
  const tree = preview ? buildTree(preview.entries.map((e) => e.path), undefined, claimedPaths) : [];

  const bucketCounts = new Map<number, number>();
  let hiddenCount = 0;
  for (const e of preview?.entries ?? []) {
    if (e.bucket === null) hiddenCount++;
    else bucketCounts.set(e.bucket, (bucketCounts.get(e.bucket) ?? 0) + 1);
  }
  const sortedBuckets = [...bucketCounts.keys()].sort((a, b) => a - b);

  return (
    <div style={{ display: "flex", flexDirection: "column" }}>
      <div style={{ flexShrink: 0, padding: 8, display: "flex", flexDirection: "column", gap: 6, borderBottom: "1px solid var(--border-subtle)" }}>
        <div style={{
          display: "flex", flexWrap: "wrap", alignItems: "center", gap: 4,
          padding: "3px 5px", background: "var(--card)",
          border: "1px solid var(--border-subtle)", borderRadius: 5,
        }}>
          {tags.map((tag) => {
            const effective = tag.bucket !== undefined ? tag.bucket : defaultBucket();
            return (
              <span key={tag.pattern} style={{
                display: "flex", alignItems: "center", gap: 4, fontSize: 10.5, fontFamily: "monospace",
                padding: "2px 3px 2px 7px", borderRadius: 9, background: "var(--surface)", color: "var(--text-secondary)",
              }}>
                {tag.pattern}
                <BucketBadge
                  bucket={effective}
                  onClick={() => cycleTagBucket(tag.pattern)}
                  title={effective === null ? "hidden — click to assign a bucket" : `bucket ${effective} — click to change`}
                />
                <button
                  onClick={() => removeTag(tag.pattern)}
                  title="Remove"
                  style={{ display: "flex", border: "none", background: "none", cursor: "pointer", color: "var(--text-ghost)", padding: 2 }}
                >
                  <X style={{ width: 9, height: 9 }} />
                </button>
              </span>
            );
          })}
          <input
            value={draft}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={onDraftKeyDown}
            onBlur={addDraft}
            placeholder={tags.length === 0 ? "pattern to claim, enter to add" : "add another…"}
            style={{
              flex: 1, minWidth: 100, border: "none", outline: "none", background: "transparent",
              fontSize: 11, fontFamily: "monospace", color: "var(--foreground)", padding: "3px 2px",
            }}
          />
        </div>
        <div style={{ display: "flex", alignItems: "center", gap: 10, flexWrap: "wrap" }}>
          <span style={{ fontSize: 9.5, color: "var(--text-ghost)", lineHeight: 1.5 }}>
            use <code>**/*</code> to reach files at any depth · click a tag&apos;s badge to give it its own bucket ·
            patterns are checked top-to-bottom, first match wins, so put a broad <code>**/*</code> catch-all after
            specific patterns, not before · lower bucket numbers come earlier in the assembled prompt
          </span>
        </div>
        {preview && (sortedBuckets.length > 0 || hiddenCount > 0) && (
          <div style={{ display: "flex", alignItems: "center", gap: 8, flexWrap: "wrap" }}>
            {sortedBuckets.map((b) => (
              <span key={b} style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, color: "var(--text-dim)" }}>
                <BucketBadge bucket={b} title={`bucket ${b}`} /> {bucketCounts.get(b)} file{bucketCounts.get(b) === 1 ? "" : "s"}
              </span>
            ))}
            {hiddenCount > 0 && (
              <span style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, color: "var(--text-dim)" }}>
                <BucketBadge bucket={null} title="hidden" /> {hiddenCount} hidden
              </span>
            )}
          </div>
        )}
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
                bucketOf={(p) => bucketByPath.get(p)}
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
