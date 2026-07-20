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
import { Eye, MessageSquarePlus, Clock } from "lucide-react";
import { loreConn, branchFileUrl } from "@/lib/ws";
import type { LoreNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError, useUI } from "@/lib/uiStore";
import { SectionHeader } from "./library";
import { basenameNoExt, flattenLore } from "@/lib/utils";
import { useCallContext } from "@/lib/callContextStore";

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

// Context-aware variant used by the Codex tab (right sidebar) only --
// the character-sidebar's compact mode keeps using the read-only
// `LoreCard` above. The differences:
//
//   - Live status badge reflecting the file's inclusion state for the
//     active file's next send. Three states, in priority order:
//       · "@mentioned" (transient — driven by composer text)
//       · "+ added"    (explicit extraFiles entry, set by the panel)
//       · "auto"       (the default lore/** convention; every Codex
//                       entry is auto-included today, so this is the
//                       resting state when nothing else applies).
//     The badge lives at the top-right of the card; "@mentioned" wins
//     visually because it's the most temporary signal.
//
//   - Click on the card body toggles an inline preview of the file's
//     content (lazy fetch via `branchFileUrl`); click the @-button to
//     drop a mention into the active composer (see lib/uiStore's
//     pendingMention -> InputBar's effect).
//
// The "auto" state is informational only -- there's no per-file
// opt-out today (the synthesizer doesn't express exclusions). The
// card's main affordances are *inspection* (preview) and *emphasis*
// (mention), not "include/exclude".
function ContextAwareLoreCard({ entry, selectedFile }: {
  entry: LoreNode;
  selectedFile: string | null;
}) {
  const [open, setOpen] = useState(false);
  const [content, setContent] = useState<string | null>(null);
  const [loadingContent, setLoadingContent] = useState(false);

  // Per-file context state for the badges.
  const fileState = useCallContext((s) => (selectedFile ? s.files[selectedFile] : undefined));
  const mentionIds = useCallContext((s) => (selectedFile ? s.mentions[selectedFile] ?? [] : []));
  const requestMention = useUI((s) => s.requestMention);

  const inExtraFiles = !!fileState && fileState.mode === "transient"
    && fileState.edits.extraFiles.some((f) => f.path === entry.path);
  // A lore file is "@mentioned" if the composer's mention overlay
  // includes its path. mentionCharacterIds only catches character
  // branches today (see lib/mentions.ts's extractCharacterMentions),
  // so this is rarely true for lore files in the current pipeline --
  // but the wiring is here for when mention extraction generalizes.
  const mentioned = mentionIds.some((id) => `character/${id}` === entry.path);

  async function toggleOpen() {
    if (!open && content === null && !loadingContent) {
      setLoadingContent(true);
      try {
        const res = await fetch(branchFileUrl("master", entry.path));
        // branchFileUrl takes a branch name; "master" is the default,
        // but ideally we'd use the active branch. Lore files live on
        // whatever branch the user is browsing -- passed in as part
        // of the path implicitly. For Phase 1, master is fine since
        // the Codex is browse-only and master is the common case.
        if (res.ok) {
          const txt = await res.text();
          setContent(txt);
        } else {
          setContent(`(failed to load: ${res.status})`);
        }
      } catch (err) {
        setContent(`(error: ${String(err)})`);
      } finally {
        setLoadingContent(false);
      }
    }
    setOpen((v) => !v);
  }

  function insertMention(e: React.MouseEvent) {
    e.stopPropagation();
    requestMention(basenameNoExt(entry.path), entry.path);
  }

  return (
    <div
      style={{
        display: "flex", flexDirection: "column", gap: 4, textAlign: "left",
        width: 190, borderRadius: 7,
        // The card's own border reflects its highest-priority state --
        // amber-dashed for "@mentioned" (matches the strip's transient
        // tone), amber-solid for "+ added", subtle for plain auto.
        border: mentioned
          ? "1px dashed var(--amber)"
          : inExtraFiles
            ? "1px solid var(--amber)"
            : "1px solid var(--border-subtle)",
        background: "var(--card)",
        overflow: "hidden",
      }}
    >
      <div
        onClick={toggleOpen}
        title={entry.path}
        style={{
          display: "flex", flexDirection: "column", gap: 3,
          padding: "8px 10px", cursor: "pointer",
        }}
      >
        <div style={{
          display: "flex", alignItems: "center", gap: 5,
        }}>
          <span style={{
            fontSize: 11.5, fontWeight: 600, color: "var(--text-secondary)",
            overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", flex: 1,
          }}>
            {basenameNoExt(entry.path)}
          </span>
          {/* Status badge -- one of three states, in priority order. */}
          {mentioned ? (
            <span title="Currently @mentioned in the composer — included for this send only"
              style={badgeStyle("@")}>
              <Clock style={{ width: 8, height: 8 }} /> @
            </span>
          ) : inExtraFiles ? (
            <span title="Explicitly added to context for this call"
              style={badgeStyle("+")}>
              +
            </span>
          ) : (
            <span title="Included by default (lore/** convention)"
              style={badgeStyle("auto")}>
              auto
            </span>
          )}
        </div>
        <span style={{
          fontSize: 10, color: "var(--text-ghost)", lineHeight: 1.4,
          display: "-webkit-box", WebkitLineClamp: open ? 0 : 2, WebkitBoxOrient: "vertical", overflow: "hidden",
        }}>
          {entry.blurb || "—"}
        </span>
        {entry.aliases.length > 0 && (
          <span style={{
            fontSize: 9.5, color: "var(--text-ghost)", fontStyle: "italic",
            overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
          }}>
            aka: {entry.aliases.join(", ")}
          </span>
        )}
        {/* Action row -- always-present @mention button, plus
            "preview" / "close" toggling on the open state. The
            @mention action stops propagation so it doesn't also
            toggle the preview. */}
        <div style={{
          display: "flex", gap: 4, marginTop: 2,
          opacity: 0.85,
        }}>
          <button
            onClick={insertMention}
            title="Insert @mention at the composer's cursor"
            style={cardActionBtnStyle}
          >
            <MessageSquarePlus style={{ width: 10, height: 10 }} /> @mention
          </button>
          <button
            onClick={(e) => { e.stopPropagation(); toggleOpen(); }}
            title={open ? "Hide content" : "Preview content"}
            style={cardActionBtnStyle}
          >
            <Eye style={{ width: 10, height: 10 }} /> {open ? "Hide" : "View"}
          </button>
        </div>
      </div>
      {open && (
        <div style={{
          borderTop: "1px solid var(--border-subtle)",
          background: "var(--surface-deep)",
          padding: 8, maxHeight: 220, overflow: "auto",
          fontSize: 10.5, lineHeight: 1.5, fontFamily: "monospace",
          color: "var(--text-secondary)", whiteSpace: "pre-wrap",
        }}>
          {loadingContent ? "Loading…" : content ?? "No content"}
        </div>
      )}
    </div>
  );
}

const cardActionBtnStyle: React.CSSProperties = {
  display: "inline-flex", alignItems: "center", gap: 3,
  fontSize: 9.5, padding: "1px 5px", borderRadius: 3,
  border: "1px solid var(--border-subtle)", background: "transparent",
  color: "var(--text-dim)", cursor: "pointer", lineHeight: 1.4,
};

function badgeStyle(kind: "@" | "+" | "auto"): React.CSSProperties {
  const color = kind === "auto" ? "var(--text-ghost)" : "var(--amber)";
  return {
    display: "inline-flex", alignItems: "center", gap: 2,
    fontSize: 9, padding: "0 4px", borderRadius: 8,
    background: kind === "auto" ? "transparent" : "var(--amber-tint)",
    color, fontWeight: 500, letterSpacing: 0.2, flexShrink: 0,
    border: kind === "@" ? `1px dashed ${color}` : "none",
  };
}

export function LoreSelector({ branch, compact, selectedFile }: {
  branch: string | null;
  compact?: boolean;
  // When set, the selector renders context-aware cards
  // (ContextAwareLoreCard above) instead of read-only ones, keyed to
  // this file's call-context state. Used by the Codex tab (right
  // sidebar). Null/undefined keeps the original read-only behavior
  // (used by character-sidebar's compact mode).
  selectedFile?: string | null;
}) {
  const loreTree = useLoreTree(branch);
  const groups = groupByTopFolder(loreTree);
  const contextAware = !compact && selectedFile !== undefined;

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
              {group.entries.map((entry) =>
                contextAware
                  ? <ContextAwareLoreCard key={entry.path} entry={entry} selectedFile={selectedFile ?? null} />
                  : <LoreCard key={entry.path} entry={entry} compact={compact} />,
              )}
            </div>
          </div>
        ))
      )}
    </div>
  );
}
