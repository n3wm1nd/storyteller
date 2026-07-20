"use client";

// LoreSelector: a card-based, novelcrafter-style read-only browser onto a
// branch's codex — reused by two sites: codex.tsx's full-tab "Codex" view
// (the active story branch) and character-sidebar.tsx's per-character
// "Context" section (that character's own branch).
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
// There's no per-file curation control here anymore: what counts as
// ambient context for a Writer/FlowWriter call is now a directory
// convention the Context DSL resolves server-side (lore/**, chapters/**,
// style.md — see CONTEXT-DSL.md/Storyteller.Context.DSL.Library), not a
// per-file bucket a user assigns through this view. This is purely a
// browser now — organize a branch's files into the right directories to
// change what's included, the same way renaming a chapter into chapters/
// already worked. Aliases (see Storyteller.Writer.Lore.parseAliases) still
// show up per card, since @mention autocomplete (lib/mentions.ts) still
// reads them by name.

import { useEffect, useState } from "react";
import { loreConn } from "@/lib/ws";
import type { LoreNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { SectionHeader } from "./library";
import { basenameNoExt, flattenLore } from "@/lib/utils";

// Re-exported for existing importers (app/fileview.tsx's mention
// autocomplete) — the implementation now lives in lib/utils.ts.
export { flattenLore };

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

function LoreCard({ entry, compact }: { entry: LoreNode; compact?: boolean }) {
  return (
    <div
      title={entry.path}
      style={{
        display: "flex", flexDirection: "column", gap: 4, textAlign: "left",
        width: compact ? 140 : 190, padding: compact ? "6px 8px" : "8px 10px", borderRadius: 7,
        border: "1px solid var(--border-subtle)",
        background: "var(--card)",
      }}
    >
      <span style={{
        fontSize: 11.5, fontWeight: 600, color: "var(--text-secondary)",
        overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
      }}>
        {basenameNoExt(entry.path)}
      </span>
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

export function LoreSelector({ branch, compact }: {
  branch: string | null;
  compact?: boolean;
}) {
  const loreTree = useLoreTree(branch);
  const groups = groupByTopFolder(loreTree);

  return (
    <div style={{ flex: 1, overflow: "auto", padding: compact ? "2px 2px 8px" : "4px 4px 16px" }}>
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
        groups.map((group) => (
          <div key={group.label}>
            {groups.length > 1 && <SectionHeader label={group.label} count={group.entries.length} />}
            <div style={{ display: "flex", flexWrap: "wrap", gap: compact ? 6 : 8, padding: compact ? "4px" : "0 10px 10px" }}>
              {group.entries.map((entry) => (
                <LoreCard key={entry.path} entry={entry} compact={compact} />
              ))}
            </div>
          </div>
        ))
      )}
    </div>
  );
}
