"use client";

// LoreSelector: a card-based, novelcrafter-style view onto a branch's
// bucket-picker ContextFilter (see lib/settingsStore.ts) — quick curation
// instead of context-source.tsx's raw glob/tree editor. Reused by two
// sites: codex.tsx's full-tab "Codex" view (branch = the active story
// branch, sourceId = WRITER_STORY_SOURCE_ID) and character-sidebar.tsx's
// per-character "Context" section (branch = that character's own branch,
// sourceId = CHARACTER_CONTEXT_SOURCE_ID) — both key off the exact same
// `branch`/`sourceId` pair for the candidate list, the assignment preview,
// and the settingsStore filter itself, so one component genuinely covers
// both with no site-specific branching inside it.
//
// Two independent data sources feed it:
//
//  - The candidate list (this component's own /lore/{branch} connection —
//    see Storyteller.Writer.Lore): that branch's freeform lore content,
//    with chapters/outlines/chat, sheet.md, journal.md, and binaries all
//    already excluded server-side — one uniform "get me all the lore for
//    this branch" definition, identical for a story branch or a character
//    branch. This is "the filesystem" the filter below gets applied to
//    (see the /lore vs /library design discussion) — never assembled by a
//    client-side glob or folder-name guess.
//  - The assignment (this component's own context.preview connection, the
//    same one context-source.tsx's ContextSourceConfig uses): which of
//    those candidates the real, persisted filter currently claims, and
//    into which bucket. Only 'ceBucket' is read from it — content/blurb are
//    ignored, since the lore tree already supplies description text for
//    every candidate, claimed or not.
//
// Both connections are owned locally (connect on mount, reconnect on
// `branch` change, close on unmount) rather than through a global
// serverCacheStore singleton — several instances (the Codex tab plus one
// per expanded character card) can be mounted at once, each against a
// different branch.
//
// A card's first click always lands it in bucket 1 (see 'nextBucket':
// trash -> 1 is the very first step of the cycle, so adding a fresh tag and
// immediately cycling it once already does the right thing with no special
// case). Clicking again cycles through the rest of the buckets same as the
// tree editor's own badge. The reset button clears the filter back to
// `{tags: []}` — for either site that's "no override configured", which
// reads as "show everything" (unchanged default behavior for anyone who
// never opens this UI).

import { useEffect, useRef, useState } from "react";
import { RotateCcw } from "lucide-react";
import { contextViewConn, loreConn } from "@/lib/ws";
import type { ContextSlotPreview, LoreNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import {
  useSettings, contextFilterKey, toContextLayout,
  addPatternTag, cycleFilterTagBucket,
  type ContextFilter,
} from "@/lib/settingsStore";
import { nextBucket } from "./bucket";
import { BucketBadge } from "./BucketBadge";
import { SectionHeader } from "./library";
import { basenameNoExt } from "@/lib/utils";

const DEFAULT_FILTER: ContextFilter = { tags: [] };

function collectLeaves(nodes: LoreNode[], acc: LoreNode[]): LoreNode[] {
  for (const n of nodes) {
    if (n.children.length > 0) collectLeaves(n.children, acc);
    else acc.push(n);
  }
  return acc;
}

interface LoreGroup {
  label: string;
  entries: LoreNode[];
}

// Group by top-level folder, flattening arbitrarily deep nesting within
// each into one section (folder position beyond the first segment carries
// no meaning for this grouping, same stance library.tsx's chapter pairing
// takes on nesting). Root-level files land in a trailing "Other" section.
function groupByTopFolder(tree: LoreNode[]): LoreGroup[] {
  const groups: LoreGroup[] = [];
  const other: LoreNode[] = [];
  for (const node of tree) {
    if (node.children.length > 0) groups.push({ label: node.name, entries: collectLeaves(node.children, []) });
    else other.push(node);
  }
  groups.sort((a, b) => a.label.localeCompare(b.label));
  if (other.length > 0) groups.push({ label: "Other", entries: other });
  return groups;
}

function LoreCard({ entry, bucket, compact, onClick }: {
  entry: LoreNode;
  bucket: number | null | undefined;
  compact?: boolean;
  onClick: () => void;
}) {
  const claimed = !!bucket;
  return (
    <button
      onClick={onClick}
      title={`Toggle ${entry.path} in this context filter`}
      style={{
        display: "flex", flexDirection: "column", gap: 4, textAlign: "left",
        width: compact ? 140 : 190, padding: compact ? "6px 8px" : "8px 10px", borderRadius: 7, cursor: "pointer",
        border: "1px solid var(--border-subtle)",
        background: "var(--card)", opacity: claimed ? 1 : 0.6,
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
        <span style={{
          flex: 1, minWidth: 0, fontSize: 11.5, fontWeight: 600, color: "var(--text-secondary)",
          overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        }}>
          {basenameNoExt(entry.path)}
        </span>
        <BucketBadge bucket={bucket ?? null} title={claimed ? `bucket ${bucket}` : "not included — click to add"} />
      </div>
      {!compact && (
        <span style={{
          fontSize: 10, color: "var(--text-ghost)", lineHeight: 1.4,
          display: "-webkit-box", WebkitLineClamp: 2, WebkitBoxOrient: "vertical", overflow: "hidden",
        }}>
          {entry.blurb || "—"}
        </span>
      )}
    </button>
  );
}

export function LoreSelector({ branch, sourceId, compact }: {
  branch: string | null;
  sourceId: string;
  compact?: boolean;
}) {
  const [loreTree, setLoreTree] = useState<LoreNode[]>([]);
  const filterKey = branch ? contextFilterKey(branch, sourceId) : null;
  const filter = useSettings((s) => (filterKey ? s.contextFilters[filterKey] : undefined) ?? DEFAULT_FILTER);
  const setContextFilter = useSettings((s) => s.setContextFilter);
  const [preview, setPreview] = useState<ContextSlotPreview | null>(null);
  const previewConnRef = useRef<ReturnType<typeof contextViewConn> | null>(null);

  function sendPreview(conn: ReturnType<typeof contextViewConn>, f: ContextFilter) {
    conn.send({
      type: "context.preview",
      slots: [{ label: "lore-selector", mode: "on-demand", layout: toContextLayout(f) }],
    });
  }

  // Candidate list — this component's own /lore/{branch} connection.
  useEffect(() => {
    if (!branch) { setLoreTree([]); return; }
    const connLabel = `lore:${branch}`;
    setConnStatus(connLabel, "connecting");
    setLoreTree([]);

    const conn = loreConn(branch);
    conn.onStatus((s) => {
      if (s !== "connected") setConnStatus(connLabel, "connecting");
    });
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "lore.tree") {
        setLoreTree(evt.nodes);
        setConnStatus(connLabel, "connected");
      } else if (evt.type === "error") {
        setError(evt.message);
      }
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(connLabel, "connected");
      } catch (err) {
        setConnStatus(connLabel, "error");
        setError(String(err));
      }
    })();

    return () => {
      conn.close();
      removeConn(connLabel);
    };
  }, [branch]);

  // Assignment — this component's own context.preview connection.
  useEffect(() => {
    if (!branch) return;
    const connLabel = `lore-assignment:${branch}:${sourceId}`;
    setConnStatus(connLabel, "connecting");

    const conn = contextViewConn(branch, "_lore-selector");
    previewConnRef.current = conn;
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
        sendPreview(conn, filter);
      } catch (err) {
        setConnStatus(connLabel, "error");
        setError(String(err));
      }
    })();

    return () => {
      conn.close();
      previewConnRef.current = null;
      removeConn(connLabel);
    };
    // filter is re-sent explicitly on every edit (see commitFilter below) —
    // this effect only re-runs on branch/sourceId change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [branch, sourceId]);

  function commitFilter(next: ContextFilter) {
    if (filterKey) setContextFilter(filterKey, next);
    if (previewConnRef.current) sendPreview(previewConnRef.current, next);
  }

  function onClickCard(path: string) {
    const withTag = filter.tags.some((t) => t.pattern === path) ? filter : addPatternTag(filter, path);
    commitFilter(cycleFilterTagBucket(withTag, path, nextBucket));
  }

  function onReset() {
    commitFilter({ tags: [] });
  }

  const bucketByPath = new Map(preview?.entries.map((e) => [e.path, e.bucket] as const) ?? []);
  const groups = groupByTopFolder(loreTree);
  const hasOverride = filter.tags.length > 0;

  return (
    <div style={{ flex: 1, overflow: "auto", padding: compact ? "2px 2px 8px" : "4px 4px 16px" }}>
      <div style={{ display: "flex", justifyContent: "flex-end", padding: compact ? "0 4px" : "0 10px" }}>
        <button
          onClick={onReset}
          disabled={!hasOverride}
          title="Reset to default — clears every override, showing everything again"
          style={{
            display: "flex", alignItems: "center", gap: 4, fontSize: 9.5, padding: "2px 6px",
            background: "transparent", border: "1px solid var(--border)", borderRadius: 4,
            color: hasOverride ? "var(--text-label)" : "var(--text-ghost)",
            cursor: hasOverride ? "pointer" : "default", opacity: hasOverride ? 1 : 0.5,
          }}
        >
          <RotateCcw style={{ width: 9, height: 9 }} />
          Reset
        </button>
      </div>

      {!branch ? (
        <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
          Select a branch to see its codex
        </div>
      ) : loreTree.length === 0 ? (
        <div style={{ padding: compact ? "6px 4px" : "12px 12px", fontSize: compact ? 10 : 11, color: "var(--text-ghost)" }}>
          {compact
            ? "No extra lore on this branch."
            : "No lore files yet — notes, world-building, and style-guide content you add to this branch (outside chapters/outlines/chat) will show up here."}
        </div>
      ) : (
        <>
          {!hasOverride && !compact && (
            <div style={{
              margin: "8px 10px 4px", padding: "8px 10px", borderRadius: 6, fontSize: 10.5, lineHeight: 1.5,
              background: "var(--surface)", border: "1px solid var(--border-subtle)", color: "var(--text-muted)",
            }}>
              Nothing curated yet — every file below is currently sent. Click an entry to start curating; once you
              add one, only added entries are included.
            </div>
          )}
          {groups.map((group) => (
            <div key={group.label}>
              {groups.length > 1 && <SectionHeader label={group.label} count={group.entries.length} />}
              <div style={{ display: "flex", flexWrap: "wrap", gap: compact ? 6 : 8, padding: compact ? "4px" : "0 10px 10px" }}>
                {group.entries.map((entry) => (
                  <LoreCard
                    key={entry.path}
                    entry={entry}
                    bucket={bucketByPath.get(entry.path)}
                    compact={compact}
                    onClick={() => onClickCard(entry.path)}
                  />
                ))}
              </div>
            </div>
          ))}
        </>
      )}
    </div>
  );
}
