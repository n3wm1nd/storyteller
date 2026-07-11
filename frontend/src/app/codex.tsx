"use client";

// Codex tab: a card-based, novelcrafter-style view onto the exact same data
// context-source.tsx's tree editor edits — the Writer agent's
// `writer:story` ContextFilter (see lib/settingsStore.ts) — just for quick
// curation instead of raw glob editing. Two independent data sources feed
// it:
//
//  - The candidate list ('loreTree' in lib/serverCacheStore.ts, pushed live
//    by /lore/{name} — see Storyteller.Writer.Lore): the branch's freeform
//    notes/world/style content, with chapters, outlines, chat/, and
//    binaries already excluded server-side. This is "the filesystem" the
//    filter below gets applied to (see the /lore vs /library design
//    discussion) — never assembled by a client-side glob or folder-name
//    guess.
//  - The assignment ('preview' state below, via the same one-shot
//    context.preview connection ContextSourceConfig itself uses): which of
//    those candidates the real, persisted filter currently claims, and
//    into which bucket. Only 'ceBucket' is read from it — content/blurb are
//    ignored, since the lore tree already supplies description text for
//    every candidate, claimed or not.
//
// A card's first click always lands it in bucket 1 (see 'nextBucket':
// trash -> 1 is the very first step of the cycle, so adding a fresh tag and
// immediately cycling it once already does the right thing with no special
// case). Clicking again cycles through the rest of the buckets same as the
// tree editor's own badge.

import { useEffect, useRef, useState } from "react";
import { contextViewConn } from "@/lib/ws";
import type { ContextSlotPreview } from "@/lib/ws";
import type { LoreNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { useServerCache } from "@/lib/serverCacheStore";
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

// The one context source Codex curates — Writer's own story-branch ambient
// context (see lib/agents.ts's STORY_AMBIENT and agentstab.tsx's
// `${selected.id}:${source.id}` key convention). Both this tab and the
// Agents tab's Writer -> Story branch tree editor read/write this exact
// settingsStore key, so either view can be used interchangeably on the same
// live filter.
const CODEX_SOURCE_ID = "writer:story";

function collectLeaves(nodes: LoreNode[], acc: LoreNode[]): LoreNode[] {
  for (const n of nodes) {
    if (n.children.length > 0) collectLeaves(n.children, acc);
    else acc.push(n);
  }
  return acc;
}

interface CodexGroup {
  label: string;
  entries: LoreNode[];
}

// Group by top-level folder, flattening arbitrarily deep nesting within
// each into one section (folder position beyond the first segment carries
// no meaning for this grouping, same stance library.tsx's chapter pairing
// takes on nesting). Root-level files land in a trailing "Other" section.
function groupByTopFolder(tree: LoreNode[]): CodexGroup[] {
  const groups: CodexGroup[] = [];
  const other: LoreNode[] = [];
  for (const node of tree) {
    if (node.children.length > 0) groups.push({ label: node.name, entries: collectLeaves(node.children, []) });
    else other.push(node);
  }
  groups.sort((a, b) => a.label.localeCompare(b.label));
  if (other.length > 0) groups.push({ label: "Other", entries: other });
  return groups;
}

function CodexCard({ entry, bucket, onClick }: {
  entry: LoreNode;
  bucket: number | null | undefined;
  onClick: () => void;
}) {
  const claimed = !!bucket;
  return (
    <button
      onClick={onClick}
      title={`Toggle ${entry.path} in the writer:story context filter`}
      style={{
        display: "flex", flexDirection: "column", gap: 4, textAlign: "left",
        width: 190, padding: "8px 10px", borderRadius: 7, cursor: "pointer",
        border: `1px solid ${claimed ? "var(--border-subtle)" : "var(--border-subtle)"}`,
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
      <span style={{
        fontSize: 10, color: "var(--text-ghost)", lineHeight: 1.4,
        display: "-webkit-box", WebkitLineClamp: 2, WebkitBoxOrient: "vertical", overflow: "hidden",
      }}>
        {entry.blurb || "—"}
      </span>
    </button>
  );
}

export function CodexTab({ activeBranch }: { activeBranch: string | null }) {
  const loreTree = useServerCache((s) => s.loreTree);
  const filterKey = activeBranch ? contextFilterKey(activeBranch, CODEX_SOURCE_ID) : null;
  const filter = useSettings((s) => (filterKey ? s.contextFilters[filterKey] : undefined) ?? DEFAULT_FILTER);
  const setContextFilter = useSettings((s) => s.setContextFilter);
  const [preview, setPreview] = useState<ContextSlotPreview | null>(null);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);

  function send(conn: ReturnType<typeof contextViewConn>, f: ContextFilter) {
    conn.send({
      type: "context.preview",
      slots: [{ label: "codex", mode: "on-demand", layout: toContextLayout(f) }],
    });
  }

  useEffect(() => {
    if (!activeBranch) return;
    const connLabel = `codex-assignment:${activeBranch}`;
    setConnStatus(connLabel, "connecting");

    const conn = contextViewConn(activeBranch, "_codex");
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
    // this effect only re-runs on branch change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeBranch]);

  function commitFilter(next: ContextFilter) {
    if (filterKey) setContextFilter(filterKey, next);
    if (connRef.current) send(connRef.current, next);
  }

  function onClickCard(path: string) {
    const withTag = filter.tags.some((t) => t.pattern === path) ? filter : addPatternTag(filter, path);
    commitFilter(cycleFilterTagBucket(withTag, path, nextBucket));
  }

  const bucketByPath = new Map(preview?.entries.map((e) => [e.path, e.bucket] as const) ?? []);
  const groups = groupByTopFolder(loreTree);

  return (
    <div style={{ flex: 1, overflow: "auto", padding: "4px 4px 16px" }}>
      {!activeBranch ? (
        <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
          Select a branch to see its codex
        </div>
      ) : loreTree.length === 0 ? (
        <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
          No lore files yet — notes, world-building, and style-guide content you add to this branch (outside
          chapters/outlines/chat) will show up here.
        </div>
      ) : (
        <>
          {filter.tags.length === 0 && (
            <div style={{
              margin: "8px 10px 4px", padding: "8px 10px", borderRadius: 6, fontSize: 10.5, lineHeight: 1.5,
              background: "var(--surface)", border: "1px solid var(--border-subtle)", color: "var(--text-muted)",
            }}>
              Nothing curated yet — every file below is currently sent to the Writer. Click an entry to start
              curating; once you add one, only added entries are included (the same tree also editable from the
              Agents tab&apos;s Writer → Story branch section).
            </div>
          )}
          {groups.map((group) => (
            <div key={group.label}>
              <SectionHeader label={group.label} count={group.entries.length} />
              <div style={{ display: "flex", flexWrap: "wrap", gap: 8, padding: "0 10px 10px" }}>
                {group.entries.map((entry) => (
                  <CodexCard
                    key={entry.path}
                    entry={entry}
                    bucket={bucketByPath.get(entry.path)}
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
