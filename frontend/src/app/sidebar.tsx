"use client";

import { useState } from "react";
import { Folder, GitBranch, Plus, Users, BookOpen, Settings } from "lucide-react";
import { type ConnInfo } from "@/lib/uiStore";
import { type CharacterSummary, type LibraryNode, type ChapterUnit } from "@/lib/ws";
import { statusColor, characterDisplayName } from "@/lib/utils";
import { classifyBranch } from "@/lib/branches";
import { FileTree } from "./filetree";
import { LibraryTree } from "./library";

function flattenLibrary(nodes: LibraryNode[]): LibraryNode[] {
  return nodes.flatMap((n) => [n, ...flattenLibrary(n.children)]);
}

// One accent token per branch kind (see lib/branches.ts) — reuses the
// existing palette (globals.css) rather than inventing a new hue per
// feature. Character reuses the same --sky CharacterListItem's own active
// state uses below, so a character branch reads as "the same kind of
// thing" in both tabs; prompts gets violet (system/infrastructure, not
// user content); story falls back to the app's ordinary amber accent.
function branchKindTokens(kind: ReturnType<typeof classifyBranch>): { solid: string; tint: string } {
  if (kind === "character") return { solid: "var(--sky)", tint: "var(--sky-tint)" };
  if (kind === "prompts")   return { solid: "var(--violet)", tint: "var(--violet-tint)" };
  return { solid: "var(--amber)", tint: "var(--amber-tint)" };
}

// A small kind icon next to the branch's own full name (kept verbatim,
// prefix and all — this tab is the raw list, unlike "Characters" below),
// so the flat "Branches" tab still tells the three kinds apart by color at
// a glance instead of showing every raw branch name identically.
function BranchKindIcon({ kind, active }: { kind: ReturnType<typeof classifyBranch>; active: boolean }) {
  const style = { width: 10, height: 10, flexShrink: 0, color: active ? branchKindTokens(kind).solid : "var(--text-dim)" };
  if (kind === "character") return <Users style={style} />;
  if (kind === "prompts") return <Settings style={style} />;
  return <GitBranch style={style} />;
}

function BranchItem({ name, active, onSelect, onDelete }: {
  name: string; active: boolean; onSelect: () => void; onDelete: () => void;
}) {
  const [hover, setHover] = useState(false);
  const kind = classifyBranch(name);
  const { solid, tint } = branchKindTokens(kind);
  return (
    <div
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      style={{
        display: "flex", alignItems: "center", gap: 6,
        padding: "5px 8px",
        background: active ? tint : hover ? "var(--surface)" : "transparent",
        borderLeft: active ? `2px solid ${solid}` : "2px solid transparent",
        borderRadius: 5, cursor: "pointer",
      }}
    >
      <BranchKindIcon kind={kind} active={active} />
      <span onClick={onSelect} title={name} style={{
        flex: 1, fontSize: 12, color: active ? solid : "var(--text-secondary)",
        overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        fontWeight: active ? 500 : 400,
      }}>{name}</span>
      {hover && (
        <button onClick={(e) => { e.stopPropagation(); onDelete(); }} style={{
          background: "none", border: "none", cursor: "pointer",
          color: "var(--rose)", fontSize: 14, lineHeight: 1, padding: "0 2px", flexShrink: 0,
        }}>×</button>
      )}
    </div>
  );
}

function CharacterListItem({ character, active, onSelect, onDelete, onHoverStart, onHoverEnd }: {
  character: CharacterSummary; active: boolean; onSelect: () => void; onDelete: () => void;
  onHoverStart: () => void; onHoverEnd: () => void;
}) {
  const [hover, setHover] = useState(false);
  return (
    <div
      onMouseEnter={() => { setHover(true); onHoverStart(); }}
      onMouseLeave={() => { setHover(false); onHoverEnd(); }}
      style={{
        display: "flex", alignItems: "center", gap: 6,
        padding: "5px 8px",
        background: active ? "var(--sky-tint)" : hover ? "var(--surface)" : "transparent",
        borderLeft: active ? "2px solid var(--sky)" : "2px solid transparent",
        borderRadius: 5, cursor: "pointer",
      }}
    >
      <Users style={{ width: 11, height: 11, flexShrink: 0, color: active ? "var(--sky)" : "var(--text-dim)" }} />
      <span onClick={onSelect} style={{
        flex: 1, fontSize: 12, color: active ? "var(--sky-text)" : "var(--text-secondary)",
        overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
        fontWeight: active ? 500 : 400,
      }}>{characterDisplayName(character.branch, character.sheet)}</span>
      {hover && (
        <button onClick={(e) => { e.stopPropagation(); onDelete(); }} style={{
          background: "none", border: "none", cursor: "pointer",
          color: "var(--rose)", fontSize: 14, lineHeight: 1, padding: "0 2px", flexShrink: 0,
        }}>×</button>
      )}
    </div>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

export function LeftSidebar({
  tab, setTab,
  branches, characterBranches, activeBranch, files, selectedFile,
  libraryTree, libraryChapters,
  onSelectBranch, onSelectFile, onCreateFile, onDeleteFile, onRenameFile,
  onCreateBranch, onDeleteBranch,
  onCreateChapter,
  onHoverCharacter,
  onUploadFiles,
  conns, error,
}: {
  tab: "explorer" | "branches" | "characters" | "library";
  setTab: (t: "explorer" | "branches" | "characters" | "library") => void;
  branches: string[];
  // Live-tracked character/* branch list from the session connection (see
  // Server.Writer.Session.Connection's notifier) — kept up to date across
  // any connection creating/deleting one, not just this session's own
  // create-branch/delete-branch commands.
  characterBranches: CharacterSummary[];
  activeBranch: string | null;
  files: string[];
  selectedFile: string | null;
  // The active branch's book/chapter tree from its own /library/{name}
  // connection (see sidebar.actions.ts's selectBranch) — independent of
  // 'files' above, which is the plain, unclassified branch file list.
  libraryTree: LibraryNode[];
  // Chapter/outline pairing, precomputed server-side (see library.tsx's own
  // header comment for why this isn't reconstructed client-side).
  libraryChapters: ChapterUnit[];
  onSelectBranch: (b: string) => void;
  onSelectFile: (f: string) => void;
  onCreateFile: (path: string) => void;
  onDeleteFile: (path: string) => void;
  onRenameFile: (path: string, newPath: string) => void;
  onCreateBranch: (name: string) => void;
  onDeleteBranch: (name: string) => void;
  onCreateChapter: (path: string, name: string) => void;
  onHoverCharacter: (branch: string | null) => void;
  onUploadFiles: (files: { path: string; content: File }[]) => void;
  conns: ConnInfo[];
  error: string | null;
}) {
  const [newBranch, setNewBranch] = useState("");

  // 'files' (the Explorer tab's flat listing) carries no per-file metadata
  // of its own — cross-reference against the library tree's own 'binary'
  // flag (see ws.ts's LibraryNode) so both tabs treat the same paths
  // consistently instead of duplicating the classification.
  const binaryPaths = new Set(flattenLibrary(libraryTree).filter((n) => n.binary).map((n) => n.path));

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%", background: "var(--sidebar)" }}>
      <div style={{ flexShrink: 0, padding: "8px 8px 0", borderBottom: "1px solid var(--border-subtle)" }}>
        <div style={{
          display: "grid", gridTemplateColumns: "1fr 1fr", gap: 1,
          background: "var(--surface)", borderRadius: 6, padding: 2,
        }}>
          {(["explorer", "library", "branches", "characters"] as const).map((t) => (
            <button key={t} onClick={() => setTab(t)} style={{
              height: 26, display: "flex", alignItems: "center", justifyContent: "center",
              gap: 5, fontSize: 11, borderRadius: 4, border: "none", cursor: "pointer",
              background: tab === t ? "var(--surface-raised)" : "transparent",
              color: tab === t ? "var(--amber)" : "var(--text-disabled)",
              transition: "background 0.15s, color 0.15s",
            }}>
              {t === "explorer"
                ? <Folder style={{ width: 12, height: 12 }} />
                : t === "library"
                ? <BookOpen style={{ width: 12, height: 12 }} />
                : t === "branches"
                ? <GitBranch style={{ width: 12, height: 12 }} />
                : <Users style={{ width: 12, height: 12 }} />}
              {t === "explorer" ? "Explorer" : t === "library" ? "Library" : t === "branches" ? "Branches" : "Characters"}
            </button>
          ))}
        </div>
        <div style={{ height: 8 }} />
      </div>

      {tab === "explorer" && (
        <FileTree
          activeBranch={activeBranch} files={files} binaryPaths={binaryPaths} selectedFile={selectedFile}
          onSelectFile={onSelectFile} onCreateFile={onCreateFile} onDeleteFile={onDeleteFile} onRenameFile={onRenameFile} onUploadFiles={onUploadFiles}
        />
      )}

      {tab === "library" && (
        <LibraryTree
          activeBranch={activeBranch} tree={libraryTree} chapters={libraryChapters} selectedFile={selectedFile}
          onSelectFile={onSelectFile} onCreateChapter={onCreateChapter}
        />
      )}

      {tab === "branches" && (
        <div style={{ flex: 1, overflow: "auto", padding: "4px 4px" }}>
          <div style={{ padding: "4px 10px 6px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)", display: "flex", alignItems: "center", gap: 6 }}>
            <GitBranch style={{ width: 11, height: 11 }} />
            Branches
            <span style={{ marginLeft: "auto", fontWeight: 400 }}>{branches.length}</span>
          </div>
          {branches.map((b) => (
            <BranchItem key={b} name={b} active={b === activeBranch}
              onSelect={() => onSelectBranch(b)}
              onDelete={() => onDeleteBranch(b)} />
          ))}
          <div style={{ padding: "6px 8px 4px", display: "flex", gap: 4 }}>
            <input
              value={newBranch}
              onChange={(e) => setNewBranch(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === "Enter" && newBranch.trim()) {
                  onCreateBranch(newBranch.trim());
                  setNewBranch("");
                }
              }}
              placeholder="New branch…"
              style={{
                flex: 1, fontSize: 11, padding: "3px 7px",
                background: "var(--card)", border: "1px solid var(--border-subtle)",
                borderRadius: 5, color: "var(--foreground)", outline: "none",
              }}
            />
            <button
              onClick={() => { if (newBranch.trim()) { onCreateBranch(newBranch.trim()); setNewBranch(""); } }}
              style={{
                width: 24, height: 24, display: "flex", alignItems: "center", justifyContent: "center",
                background: "var(--amber-tint)", border: "1px solid var(--amber-border)",
                borderRadius: 5, color: "var(--amber)", cursor: "pointer", flexShrink: 0,
              }}
            >
              <Plus style={{ width: 11, height: 11 }} />
            </button>
          </div>
        </div>
      )}

      {tab === "characters" && (
        // Filtered/styled view over the same branch list — no dedicated
        // "characters" endpoint yet, see WRITER.md. Hovering an entry
        // highlights that character's atoms in the currently open file.
        <div style={{ flex: 1, overflow: "auto", padding: "4px 4px" }} onMouseLeave={() => onHoverCharacter(null)}>
          <div style={{ padding: "4px 10px 6px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)", display: "flex", alignItems: "center", gap: 6 }}>
            <Users style={{ width: 11, height: 11 }} />
            Characters
            <span style={{ marginLeft: "auto", fontWeight: 400 }}>{characterBranches.length}</span>
          </div>
          {characterBranches.length === 0 ? (
            <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
              No character branches — create one from the Branches tab (e.g. "character/alice")
            </div>
          ) : (
            characterBranches.map((c) => (
              <CharacterListItem key={c.branch} character={c} active={c.branch === activeBranch}
                onSelect={() => onSelectBranch(c.branch)}
                onDelete={() => onDeleteBranch(c.branch)}
                onHoverStart={() => onHoverCharacter(c.branch)}
                onHoverEnd={() => onHoverCharacter(null)} />
            ))
          )}
        </div>
      )}

      <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "6px 10px" }}>
        {conns.map((c) => (
          <div key={c.label} style={{ display: "flex", alignItems: "center", gap: 5, marginBottom: 2 }}>
            <div style={{ position: "relative", width: 5, height: 5, flexShrink: 0, color: statusColor(c.status) }}>
              <div style={{ width: 5, height: 5, borderRadius: "50%", background: "currentColor" }} />
              <div key={c.lastActivity} className={c.lastActivity ? "ws-pulse" : undefined} style={{ position: "absolute", inset: 0, borderRadius: "50%" }} />
            </div>
            <span style={{ fontSize: 9, color: "var(--text-dim)", fontFamily: "monospace", flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{c.label}</span>
            <span style={{ fontSize: 9, color: statusColor(c.status) }}>{c.status}</span>
          </div>
        ))}
        {error && <div style={{ fontSize: 9, color: "var(--rose)", lineHeight: 1.3, marginTop: 2 }}>{error}</div>}
      </div>
    </div>
  );
}
