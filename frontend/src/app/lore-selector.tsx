"use client";

// LoreSelector: a card-based, novelcrafter-style view onto a branch's
// bucket-picker ContextFilter (see lib/settingsStore.ts) — quick curation
// instead of context-source.tsx's raw glob/tree editor. Reused by two
// sites: codex.tsx's full-tab "Codex" view (branch = the active story
// branch, sourceId = WRITER_STORY_SOURCE_ID) and character-sidebar.tsx's
// per-character "Context" section (branch = that character's own branch,
// sourceId = CHARACTER_CONTEXT_SOURCE_ID) — both key off the exact same
// `branch`/`sourceId` pair for the candidate list and the settingsStore
// filter itself, so one component genuinely covers both with no site-
// specific branching inside it.
//
// The candidate list is this component's own /lore/{branch} connection
// (see Storyteller.Writer.Lore): that branch's freeform lore content, with
// chapters/outlines/chat, sheet.md, journal.md, and binaries all already
// excluded server-side — one uniform "get me all the lore for this branch"
// definition, identical for a story branch or a character branch. Owned
// locally (connect on mount, reconnect on `branch` change, close on
// unmount) rather than through a global serverCacheStore singleton —
// several instances (the Codex tab plus one per expanded character card)
// can be mounted at once, each against a different branch.
//
// Each card exposes a 3-way Off / Triggered / Always control (see
// 'LoreState' below) derived directly from the persisted 'ContextFilter' —
// no server round-trip needed to know a card's own state, unlike
// context-source.tsx's tree editor, which needs a live context.preview
// resolution to show bucket assignment for arbitrary glob patterns. "Just
// include it" (Always, a fixed bucket-1 tag) and "ambient, pulled in only
// when mentioned" (Triggered) are the two choices this design is actually
// asking curators to make; precise bucket ordering is still there for power
// users via the raw glob editor, just not in this view. The reset button
// clears the filter back to `{tags: [], triggers: []}` — for either site
// that's "no override configured", which reads as "show everything"
// (unchanged default behavior for anyone who never opens this UI).

import { useEffect, useState } from "react";
import { RotateCcw, Trash2, Zap, Check } from "lucide-react";
import { loreConn } from "@/lib/ws";
import type { LoreNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import {
  useSettings, contextFilterKey,
  setAlwaysIncluded, setTriggered,
  type ContextFilter,
} from "@/lib/settingsStore";
import { SectionHeader } from "./library";
import { basenameNoExt, flattenLore } from "@/lib/utils";
import { bucketColor } from "./bucket";

// Re-exported for existing importers (app/fileview.tsx's mention
// autocomplete) — the implementation now lives in lib/utils.ts so
// lib/loreTrigger.ts can use it too without a lib -> app import.
export { flattenLore };

const DEFAULT_FILTER: ContextFilter = { tags: [], triggers: [] };

// This branch's live codex tree, via its own /lore/{branch} connection —
// owned locally (connect on mount, reconnect on `branch` change, close on
// unmount) rather than a global serverCacheStore singleton, since several
// callers (the Codex tab, one per expanded character card, and the
// composer's mention search) can all want a different branch's tree at
// once. Exported so mention-autocomplete.tsx can reuse the exact same
// connection lifecycle instead of forking a second copy.
export function useLoreTree(branch: string | null): LoreNode[] {
  const [loreTree, setLoreTree] = useState<LoreNode[]>([]);

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

  return loreTree;
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
    if (node.children.length > 0) groups.push({ label: node.name, entries: flattenLore(node.children) });
    else other.push(node);
  }
  groups.sort((a, b) => a.label.localeCompare(b.label));
  if (other.length > 0) groups.push({ label: "Other", entries: other });
  return groups;
}

// The three states a codex entry's inclusion can be in, as this simplified
// view exposes it — see the module header for why this replaces the raw
// glob editor's numbered bucket cycle here specifically.
type LoreState = "off" | "triggered" | "always";

function loreState(filter: ContextFilter, path: string): LoreState {
  if (filter.triggers.includes(path)) return "triggered";
  if (filter.tags.some((t) => t.pattern === path)) return "always";
  return "off";
}

const STATE_LABEL: Record<LoreState, string> = { off: "Off", triggered: "Triggered", always: "Always" };
const STATE_ICON: Record<LoreState, typeof Trash2> = { off: Trash2, triggered: Zap, always: Check };
// "Always" reuses bucket 1's own colour (setAlwaysIncluded always assigns
// bucket 1) so it reads as the same group as the raw glob editor's bucket-1
// tags; "Triggered" gets its own accent since it has no bucket equivalent.
const STATE_COLOR: Record<LoreState, string> = { off: "var(--text-ghost)", triggered: "var(--amber)", always: bucketColor(1) };

function InclusionControl({ state, onSelect }: { state: LoreState; onSelect: (s: LoreState) => void }) {
  return (
    <div style={{ display: "flex", gap: 2 }}>
      {(["off", "triggered", "always"] as const).map((s) => {
        const Icon = STATE_ICON[s];
        const active = state === s;
        return (
          <button
            key={s}
            onClick={(e) => { e.stopPropagation(); onSelect(s); }}
            title={s === "off" ? "Not included" : s === "triggered" ? "Include only when its name/alias is mentioned" : "Always included"}
            style={{
              display: "flex", alignItems: "center", justifyContent: "center",
              width: 18, height: 18, borderRadius: 5, flexShrink: 0, border: "none", padding: 0, cursor: "pointer",
              background: active ? `color-mix(in srgb, ${STATE_COLOR[s]} 22%, transparent)` : "transparent",
              color: active ? STATE_COLOR[s] : "var(--text-ghost)",
            }}
          >
            <Icon style={{ width: 10, height: 10 }} />
          </button>
        );
      })}
    </div>
  );
}

function LoreCard({ entry, state, compact, onSelect }: {
  entry: LoreNode;
  state: LoreState;
  compact?: boolean;
  onSelect: (s: LoreState) => void;
}) {
  return (
    <div
      title={`${entry.path} — ${STATE_LABEL[state]}`}
      style={{
        display: "flex", flexDirection: "column", gap: 4, textAlign: "left",
        width: compact ? 140 : 190, padding: compact ? "6px 8px" : "8px 10px", borderRadius: 7,
        border: "1px solid var(--border-subtle)",
        background: "var(--card)", opacity: state === "off" ? 0.6 : 1,
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
        <span style={{
          flex: 1, minWidth: 0, fontSize: 11.5, fontWeight: 600, color: "var(--text-secondary)",
          overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        }}>
          {basenameNoExt(entry.path)}
        </span>
        <InclusionControl state={state} onSelect={onSelect} />
      </div>
      {!compact && (
        <span style={{
          fontSize: 10, color: "var(--text-ghost)", lineHeight: 1.4,
          display: "-webkit-box", WebkitLineClamp: 2, WebkitBoxOrient: "vertical", overflow: "hidden",
        }}>
          {entry.blurb || "—"}
        </span>
      )}
      {!compact && entry.aliases.length > 0 && (
        <span style={{
          fontSize: 9.5, color: "var(--text-ghost)", fontStyle: "italic",
          overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        }}>
          aka: {entry.aliases.join(", ")}
        </span>
      )}
    </div>
  );
}

export function LoreSelector({ branch, sourceId, compact }: {
  branch: string | null;
  sourceId: string;
  compact?: boolean;
}) {
  const loreTree = useLoreTree(branch);
  const filterKey = branch ? contextFilterKey(branch, sourceId) : null;
  const filter = useSettings((s) => (filterKey ? s.contextFilters[filterKey] : undefined) ?? DEFAULT_FILTER);
  const setContextFilter = useSettings((s) => s.setContextFilter);

  function commitFilter(next: ContextFilter) {
    if (filterKey) setContextFilter(filterKey, next);
  }

  function onSelectState(path: string, state: LoreState) {
    switch (state) {
      case "off":       commitFilter(setTriggered(setAlwaysIncluded(filter, path, false), path, false)); break;
      case "triggered": commitFilter(setTriggered(filter, path, true)); break;
      case "always":    commitFilter(setAlwaysIncluded(filter, path, true)); break;
    }
  }

  function onReset() {
    commitFilter({ tags: [], triggers: [] });
  }

  const groups = groupByTopFolder(loreTree);
  const hasOverride = filter.tags.length > 0 || filter.triggers.length > 0;

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
                    state={loreState(filter, entry.path)}
                    compact={compact}
                    onSelect={(s) => onSelectState(entry.path, s)}
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
