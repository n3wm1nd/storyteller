"use client";

import { useState } from "react";
import { BookOpen, ListTree, FileText, FileWarning, Plus } from "lucide-react";
import type { LibraryNode, ChapterUnit } from "@/lib/ws";
import { branchFileUrl } from "@/lib/ws";

// ── Library tab (LeftSidebar) ─────────────────────────────────────────────────
//
// Renders the /library/{name} view (see WS-PROTOCOL.md, Storyteller.Writer.Library)
// as an actual organized list — chapters by title, their beat sheets folded
// in as a per-row field rather than a separate entry, everything else kept
// visually separate — rather than a second copy of the Explorer's raw file
// tree with different icons.
//
// Chapter/outline pairing-by-number ('chapters' prop) is computed
// server-side (Storyteller.Writer.Library.chapterUnits), not reconstructed
// here: "chapter N exists" is a real domain fact (either artifact existing
// already means the chapter exists as a concept, per WRITER.md's beat sheet
// being real planning content on its own), and the Summarizer agent will
// need the identical pairing later — duplicating that logic client-side
// would risk two independent, driftable answers to "what belongs to chapter
// N". Folder structure and everything unrecognized ('tree' prop) has no
// such domain question attached to it, so it's fine to just filter it
// client-side.
//
// Read-only except for 'chapter.create': opening any node's actual content
// still goes through the ordinary file connection, same as
// FileTree/CharacterSidebar's journal both do — this tab never opens its
// own file view, it reuses 'onSelectFile'.

function basenameNoExt(path: string): string {
  const base = path.split("/").pop() ?? path;
  const idx = base.lastIndexOf(".");
  return idx > 0 ? base.slice(0, idx) : base;
}

function pathNoExt(path: string): string {
  const idx = path.lastIndexOf(".");
  return idx > 0 ? path.slice(0, idx) : path;
}

// A chapter's own display label is its raw first line ('heading') — same
// "server hands over raw text, client decides" contract sheet.md's H1 gets
// (see lib/utils.characterDisplayName) — falling back to its filename (sans
// extension; a filename was never meant to be read as a title), or to a
// plain "Chapter N" when no prose file exists for it yet at all. Only
// 'chapter.create' seeds an actual "# Title" line; existing prose written
// another way (an agent, a raw edit) still has *some* first line, which can
// be a whole paragraph rather than a short title — truncated here for the
// sidebar's sake, not because a longer line is wrong content.
function chapterLabel(unit: ChapterUnit): string {
  const heading = unit.heading?.replace(/^#\s*/, "").trim();
  if (heading) return heading.length > 60 ? heading.slice(0, 60).trimEnd() + "…" : heading;
  return unit.chapterPath ? basenameNoExt(unit.chapterPath) : `Chapter ${unit.number}`;
}

// A chapter number with only a beat sheet so far derives its prose path from
// the outline's own — 'ch{N}.outline.md' -> 'ch{N}.md', same directory —
// purely a client-side convenience for "create the chapter now" below; the
// server itself never requires this shape (detection is freeform, see
// WRITER.md).
function chapterPathFromOutline(outlinePath: string): string {
  return outlinePath.replace(/\.outline\.md$/, ".md");
}

function collectLeaves(nodes: LibraryNode[], acc: LibraryNode[]): LibraryNode[] {
  for (const n of nodes) {
    if (n.kind === "folder") collectLeaves(n.children, acc);
    else acc.push(n);
  }
  return acc;
}

function SectionHeader({ label, count, muted }: { label: string; count: number; muted?: boolean }) {
  return (
    <div style={{
      padding: "8px 10px 4px", fontSize: 10, fontWeight: 600, textTransform: "uppercase",
      letterSpacing: "0.08em", color: muted ? "var(--text-ghost)" : "var(--text-dim)",
      display: "flex", alignItems: "center", gap: 6,
    }}>
      {label}
      <span style={{ marginLeft: "auto", fontWeight: 400 }}>{count}</span>
    </div>
  );
}

function Row({ active, muted, onClick, icon, label, title }: {
  active: boolean; muted?: boolean; onClick: () => void; icon: React.ReactNode; label: string; title?: string;
}) {
  return (
    <button onClick={onClick} title={title} style={{
      display: "flex", alignItems: "center", gap: 6, flex: 1, minWidth: 0, textAlign: "left",
      padding: "4px 10px 4px 12px",
      background: active ? "oklch(0.78 0.10 65 / 0.10)" : "transparent",
      color: active ? "var(--amber)" : muted ? "var(--text-ghost)" : "var(--text-secondary)",
      border: "none", borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent",
      cursor: "pointer", borderRadius: 5, fontSize: 12, fontWeight: active ? 500 : 400,
    }}>
      {icon}
      <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{label}</span>
    </button>
  );
}

// One chapter number, one row. The beat sheet (if any) is a small trailing
// icon-button on the same row, not a separate entry — clicking it opens the
// outline directly; clicking the row itself opens the chapter's prose, or,
// if there isn't one yet, creates it now (seeded from the outline's own
// path/number when a beat sheet already exists) and opens that.
function ChapterRow({ unit, selectedFile, onSelectFile, onCreateChapter }: {
  unit: ChapterUnit;
  selectedFile: string | null;
  onSelectFile: (f: string) => void;
  onCreateChapter: (path: string, name: string) => void;
}) {
  const hasChapter = !!unit.chapterPath;
  const label = chapterLabel(unit);

  function handleClick() {
    if (unit.chapterPath) {
      onSelectFile(unit.chapterPath);
    } else {
      const path = unit.outlinePath ? chapterPathFromOutline(unit.outlinePath) : `chapters/ch${unit.number}.md`;
      onCreateChapter(path, label);
      onSelectFile(path);
    }
  }

  return (
    <div style={{ display: "flex", alignItems: "center" }}>
      <Row active={hasChapter && selectedFile === unit.chapterPath} muted={!hasChapter} onClick={handleClick}
        icon={<BookOpen style={{ width: 12, height: 12, flexShrink: 0, opacity: hasChapter ? 0.8 : 0.35 }} />}
        label={label} />
      {unit.outlinePath && (
        <button
          title="Beat sheet"
          onClick={(e) => { e.stopPropagation(); onSelectFile(unit.outlinePath!); }}
          style={{
            flexShrink: 0, width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center",
            background: "transparent", border: "none", cursor: "pointer", marginRight: 4,
            color: selectedFile === unit.outlinePath ? "var(--amber)" : "var(--text-ghost)",
          }}
        >
          <ListTree style={{ width: 11, height: 11 }} />
        </button>
      )}
    </div>
  );
}

export function LibraryTree({
  activeBranch, tree, chapters, selectedFile, onSelectFile, onCreateChapter,
}: {
  activeBranch: string | null;
  tree: LibraryNode[];
  chapters: ChapterUnit[];
  selectedFile: string | null;
  onSelectFile: (f: string) => void;
  onCreateChapter: (path: string, name: string) => void;
}) {
  const [newChapterName, setNewChapterName] = useState("");
  const leaves = collectLeaves(tree, []);
  const storyOutlines = leaves.filter((n) => n.kind === "story-outline");
  const other = leaves.filter((n) => n.kind === "other").sort((a, b) => a.path.localeCompare(b.path));

  // Next free chapter number/path — reuses whichever chapters/ folder the
  // highest-numbered existing unit already lives in, or defaults to a
  // top-level chapters/ if there are none yet. Purely a client-side
  // convenience for the input below; the server itself never requires this
  // shape (chapter.create accepts any path — detection is freeform, see
  // WRITER.md).
  function nextChapterPath(): string {
    const last = chapters[chapters.length - 1];
    const lastPath = last?.chapterPath ?? last?.outlinePath;
    if (!last || !lastPath) return "chapters/ch1.md";
    const dir = lastPath.includes("/") ? lastPath.slice(0, lastPath.lastIndexOf("/")) : "";
    const nextNum = last.number + 1;
    return dir ? `${dir}/ch${nextNum}.md` : `ch${nextNum}.md`;
  }

  function handleCreateChapter() {
    const name = newChapterName.trim();
    if (!name) return;
    setNewChapterName("");
    onCreateChapter(nextChapterPath(), name);
  }

  return (
    <div style={{ flex: 1, overflow: "auto", padding: "4px 4px" }}>
      <div style={{
        padding: "5px 10px 4px", marginBottom: 2,
        display: "flex", alignItems: "center", gap: 6,
        borderBottom: "1px solid var(--border-subtle)",
      }}>
        <div style={{ width: 6, height: 6, borderRadius: "50%", background: activeBranch ? "var(--amber)" : "var(--text-dim)", flexShrink: 0 }} />
        <span style={{ fontSize: 11, fontWeight: 600, color: activeBranch ? "var(--amber)" : "var(--text-dim)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {activeBranch ?? "no branch"}
        </span>
      </div>

      {!activeBranch ? (
        <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
          Select a branch to see its book/chapter list
        </div>
      ) : (
        <>
          {storyOutlines.map((n) => (
            <Row key={n.path} active={selectedFile === n.path} onClick={() => onSelectFile(n.path)}
              icon={<ListTree style={{ width: 12, height: 12, flexShrink: 0, opacity: 0.8 }} />}
              label="Story Outline" />
          ))}

          <SectionHeader label="Chapters" count={chapters.length} />
          {chapters.length === 0 ? (
            <div style={{ padding: "4px 12px 10px", fontSize: 11, color: "var(--text-ghost)" }}>
              No chapters yet — add one below
            </div>
          ) : (
            chapters.map((unit) => (
              <ChapterRow key={unit.number} unit={unit} selectedFile={selectedFile}
                onSelectFile={onSelectFile} onCreateChapter={onCreateChapter} />
            ))
          )}

          {other.length > 0 && (
            <>
              <SectionHeader label="Notes & Other" count={other.length} muted />
              {other.map((n) => n.binary ? (
                // No atom history at all (see ws.ts's LibraryNode.binary) —
                // there's no tick chain for the prose/atom viewer to show,
                // and writing to it would just glue text onto whatever
                // binary content is actually there. Open the raw bytes
                // (same endpoint uploadFiles PUTs to) in a new tab instead.
                <Row key={n.path} active={false} muted
                  onClick={() => activeBranch && window.open(branchFileUrl(activeBranch, n.path), "_blank")}
                  icon={<FileWarning style={{ width: 11, height: 11, flexShrink: 0 }} />}
                  title="Binary file — opens raw, not editable here"
                  label={n.name} />
              ) : (
                <Row key={n.path} active={selectedFile === n.path} muted onClick={() => onSelectFile(n.path)}
                  icon={<FileText style={{ width: 11, height: 11, flexShrink: 0 }} />}
                  label={pathNoExt(n.path)} />
              ))}
            </>
          )}
        </>
      )}

      {activeBranch && (
        <div style={{ padding: "10px 8px 4px", display: "flex", gap: 4 }}>
          <input
            value={newChapterName}
            onChange={(e) => setNewChapterName(e.target.value)}
            onKeyDown={(e) => { if (e.key === "Enter") handleCreateChapter(); }}
            placeholder="New chapter name…"
            style={{
              flex: 1, fontSize: 11, padding: "3px 7px",
              background: "var(--card)", border: "1px solid var(--border-subtle)",
              borderRadius: 5, color: "var(--foreground)", outline: "none",
            }}
          />
          <button
            onClick={handleCreateChapter}
            style={{
              width: 24, height: 24, display: "flex", alignItems: "center", justifyContent: "center",
              background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)",
              borderRadius: 5, color: "var(--amber)", cursor: "pointer", flexShrink: 0,
            }}
          >
            <Plus style={{ width: 11, height: 11 }} />
          </button>
        </div>
      )}
    </div>
  );
}
