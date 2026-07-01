"use client";

import { useEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import {
  PanelLeftClose, PanelLeftOpen,
  Folder, FolderOpen, FileText, GitBranch, ChevronRight, ChevronDown, ChevronUp,
  Sparkles, Plus, MessageSquare, StickyNote, Trash2, MoveUp, MoveDown,
  Eye, EyeOff,
} from "lucide-react";
import { useStory, type ConnInfo, type WireTick } from "@/lib/store";

// Walk a tick map from head → oldest, return oldest-first ordered array.
// Skips root ticks (kind "root") — those are structural, not content.
function tickChain(ticks: Record<string, WireTick>, head: string | null): WireTick[] {
  if (!head || !ticks[head]) return [];
  const chain: WireTick[] = [];
  let cur: string | null = head;
  const seen = new Set<string>();
  while (cur && ticks[cur] && !seen.has(cur)) {
    seen.add(cur);
    const t = ticks[cur];
    if (t.kind !== "root") chain.push(t);
    cur = t.parent;
  }
  return chain.reverse();
}

// Extract payload from "type:<kind>\n<payload>" message format.
function tickPayload(msg: string): string {
  const nl = msg.indexOf("\n");
  return nl >= 0 ? msg.slice(nl + 1) : msg;
}

// Extract a named field from a WireTick's fields map.
function tickField(tick: WireTick, key: string): string | undefined {
  return tick.fields?.[key];
}


// ── Top bar ───────────────────────────────────────────────────────────────────

function TopBar({ sessionStatus, branches, activeBranch }: {
  sessionStatus: string;
  branches: string[];
  activeBranch: string | null;
}) {
  return (
    <div style={{
      height: 32, flexShrink: 0,
      background: "var(--topbar)",
      borderBottom: "1px solid var(--border-subtle)",
      display: "flex", alignItems: "center", gap: 0, padding: "0 12px",
    }}>
      <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
        <div style={{
          width: 10, height: 10, borderRadius: "50%",
          background: "var(--amber)",
          boxShadow: "0 0 10px oklch(0.78 0.10 65 / 40%)",
        }} />
        <span style={{ fontSize: 11, fontWeight: 700, letterSpacing: "0.08em", color: "var(--text-heading)" }}>
          STORYTELLER
        </span>
      </div>
      <div style={{ width: 1, height: 12, background: "var(--border-subtle)", margin: "0 10px" }} />
      <span style={{ fontSize: 10, color: "var(--amber)", fontWeight: 500 }}>Writer</span>
      {activeBranch && (
        <>
          <div style={{ width: 1, height: 12, background: "var(--border-subtle)", margin: "0 10px" }} />
          <span style={{ fontSize: 10, color: "var(--text-ghost)" }}>{activeBranch}</span>
        </>
      )}
      <div style={{ marginLeft: "auto", display: "flex", alignItems: "center", gap: 16, fontSize: 10, color: "var(--text-dim)" }}>
        <span>{branches.length} branch{branches.length !== 1 ? "es" : ""}</span>
        <div style={{ display: "flex", alignItems: "center", gap: 5 }}>
          <div style={{ width: 6, height: 6, borderRadius: "50%", background: statusColor(sessionStatus) }} />
          <span style={{ color: statusColor(sessionStatus) }}>{sessionStatus}</span>
        </div>
      </div>
    </div>
  );
}

// ── Left sidebar ──────────────────────────────────────────────────────────────

function LeftSidebar({
  tab, setTab,
  branches, activeBranch, files, selectedFile,
  onSelectBranch, onSelectFile,
  onCreateBranch, onDeleteBranch,
  conns, error,
}: {
  tab: "explorer" | "branches";
  setTab: (t: "explorer" | "branches") => void;
  branches: string[];
  activeBranch: string | null;
  files: string[];
  selectedFile: string | null;
  onSelectBranch: (b: string) => void;
  onSelectFile: (f: string) => void;
  onCreateBranch: (name: string) => void;
  onDeleteBranch: (name: string) => void;
  conns: ConnInfo[];
  error: string | null;
}) {
  const [newBranch, setNewBranch] = useState("");

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%", background: "var(--sidebar)" }}>
      {/* Tab bar */}
      <div style={{ flexShrink: 0, padding: "8px 8px 0", borderBottom: "1px solid var(--border-subtle)" }}>
        <div style={{ display: "flex", background: "var(--surface)", borderRadius: 6, padding: 2, gap: 1 }}>
          {(["explorer", "branches"] as const).map((t) => (
            <button key={t} onClick={() => setTab(t)} style={{
              flex: 1, height: 26, display: "flex", alignItems: "center", justifyContent: "center",
              gap: 5, fontSize: 11, borderRadius: 4, border: "none", cursor: "pointer",
              background: tab === t ? "var(--surface-raised)" : "transparent",
              color: tab === t ? "var(--amber)" : "var(--text-disabled)",
              transition: "background 0.15s, color 0.15s",
            }}>
              {t === "explorer"
                ? <Folder style={{ width: 12, height: 12 }} />
                : <GitBranch style={{ width: 12, height: 12 }} />}
              {t === "explorer" ? "Explorer" : "Branches"}
            </button>
          ))}
        </div>
        <div style={{ height: 8 }} />
      </div>

      {/* Explorer tab */}
      {tab === "explorer" && (
        <div style={{ flex: 1, overflow: "auto", padding: "4px 4px" }}>
          {/* Branch header */}
          <div style={{
            padding: "5px 10px 4px", marginBottom: 2,
            display: "flex", alignItems: "center", gap: 6,
            borderBottom: "1px solid var(--border-subtle)",
          }}>
            <div style={{ width: 6, height: 6, borderRadius: "50%", background: activeBranch ? "var(--amber)" : "var(--text-dim)", flexShrink: 0 }} />
            <span style={{ fontSize: 11, fontWeight: 600, color: activeBranch ? "var(--amber)" : "var(--text-dim)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
              {activeBranch ?? "no branch"}
            </span>
            {activeBranch && <span style={{ marginLeft: "auto", fontSize: 10, color: "var(--text-dim)", flexShrink: 0 }}>{files.length} files</span>}
          </div>

          {!activeBranch ? (
            <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
              Select a branch to browse files
            </div>
          ) : files.length === 0 ? (
            <div style={{ padding: "12px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
              Empty branch
            </div>
          ) : (
            buildTree(files).map((node) => (
              <FileTreeNode key={node.path} node={node} depth={0} selectedFile={selectedFile} onSelectFile={onSelectFile} />
            ))
          )}
        </div>
      )}

      {/* Branches tab */}
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
          {/* Add branch — inline, understated */}
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
                background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)",
                borderRadius: 5, color: "var(--amber)", cursor: "pointer", flexShrink: 0,
              }}
            >
              <Plus style={{ width: 11, height: 11 }} />
            </button>
          </div>
        </div>
      )}

      {/* Footer — conn status only */}
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

// Build a tree from flat paths for the file explorer
interface TreeNode {
  name: string;
  path: string;
  isDir: boolean;
  children: TreeNode[];
}

function buildTree(paths: string[]): TreeNode[] {
  const root: TreeNode[] = [];
  for (const path of [...paths].sort()) {
    const parts = path.split("/");
    let nodes = root;
    let builtPath = "";
    for (let i = 0; i < parts.length; i++) {
      builtPath = builtPath ? builtPath + "/" + parts[i] : parts[i];
      const isLast = i === parts.length - 1;
      const displayName = decodeURIComponent(parts[i]);
      let node = nodes.find((n) => n.name === displayName);
      if (!node) {
        node = { name: displayName, path: builtPath, isDir: !isLast, children: [] };
        nodes.push(node);
      }
      nodes = node.children;
    }
  }
  return root;
}

function FileTreeNode({ node, depth, selectedFile, onSelectFile }: {
  node: TreeNode; depth: number; selectedFile: string | null; onSelectFile: (p: string) => void;
}) {
  const [open, setOpen] = useState(true);
  const active = selectedFile === node.path;
  const pad = 8 + depth * 14;

  if (node.isDir) {
    return (
      <div>
        <button onClick={() => setOpen((v) => !v)} style={{
          display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
          padding: `3px 8px 3px ${pad}px`,
          background: "transparent", border: "none", cursor: "pointer", borderRadius: 5,
          color: "var(--text-muted)", fontSize: 11,
        }}>
          <ChevronRight style={{ width: 11, height: 11, flexShrink: 0, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
          {open
            ? <FolderOpen style={{ width: 12, height: 12, flexShrink: 0 }} />
            : <Folder style={{ width: 12, height: 12, flexShrink: 0 }} />}
          <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
        </button>
        {open && node.children.map((child) => (
          <FileTreeNode key={child.path} node={child} depth={depth + 1} selectedFile={selectedFile} onSelectFile={onSelectFile} />
        ))}
      </div>
    );
  }

  return (
    <button onClick={() => onSelectFile(node.path)} style={{
      display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
      padding: `3px 8px 3px ${pad}px`,
      background: active ? "oklch(0.78 0.10 65 / 0.10)" : "transparent",
      color: active ? "var(--amber)" : "var(--text-secondary)",
      border: "none", borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent",
      cursor: "pointer", borderRadius: 5, fontSize: 12, fontWeight: active ? 500 : 400,
    }}>
      <FileText style={{ width: 11, height: 11, flexShrink: 0, opacity: 0.6 }} />
      <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
    </button>
  );
}

function BranchItem({ name, active, onSelect, onDelete }: {
  name: string; active: boolean; onSelect: () => void; onDelete: () => void;
}) {
  const [hover, setHover] = useState(false);
  return (
    <div
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      style={{
        display: "flex", alignItems: "center", gap: 6,
        padding: "5px 8px",
        background: active ? "oklch(0.78 0.10 65 / 0.10)" : hover ? "var(--surface)" : "transparent",
        borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent",
        borderRadius: 5, cursor: "pointer",
      }}
    >
      <div style={{ width: 6, height: 6, borderRadius: "50%", flexShrink: 0, background: active ? "var(--amber)" : "var(--text-dim)" }} />
      <span onClick={onSelect} style={{
        flex: 1, fontSize: 12, color: active ? "var(--amber)" : "var(--text-secondary)",
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

// ── Annotation mode ───────────────────────────────────────────────────────────

export type AnnotationMode = "hidden" | "dots" | "expanded";

// ── Center toolbar strip ──────────────────────────────────────────────────────

function Toolbar({
  leftOpen, onToggleLeft,
  selectedFile, onCloseFile, onNewFile,
  centerTab, onCenterTab,
}: {
  leftOpen: boolean;
  onToggleLeft: () => void;
  selectedFile: string | null;
  onCloseFile: () => void;
  onNewFile: () => void;
  centerTab: "file" | "ticks";
  onCenterTab: (t: "file" | "ticks") => void;
}) {
  return (
    <div style={{
      height: 36, flexShrink: 0,
      background: "var(--surface-deep)",
      borderBottom: "1px solid var(--border-subtle)",
      display: "flex", alignItems: "stretch", gap: 0, padding: "0 4px 0 6px",
    }}>
      {/* Sidebar toggle */}
      <button onClick={onToggleLeft} style={{ ...iconBtnStyle, alignSelf: "center", marginRight: 2 }}>
        {leftOpen
          ? <PanelLeftClose style={{ width: 14, height: 14 }} />
          : <PanelLeftOpen style={{ width: 14, height: 14 }} />}
      </button>

      <div style={{ width: 1, height: 16, background: "var(--border-subtle)", alignSelf: "center", margin: "0 6px" }} />

      {/* File tab — label includes filename when open */}
      <button onClick={() => onCenterTab("file")} style={{
        padding: "0 10px", fontSize: 11, fontWeight: 500, display: "flex", alignItems: "center", gap: 6,
        border: "none", borderBottom: centerTab === "file" ? "2px solid var(--amber)" : "2px solid transparent",
        borderTop: "2px solid transparent", background: "transparent",
        color: centerTab === "file" ? "var(--amber)" : "var(--text-disabled)",
        cursor: "pointer", transition: "color 0.15s, border-color 0.15s", maxWidth: 320, minWidth: 0,
      }}>
        <span style={{ flexShrink: 0 }}>File</span>
        {selectedFile && <>
          <span style={{ color: "var(--border)", flexShrink: 0 }}>·</span>
          <span style={{
            fontSize: 10, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
            color: centerTab === "file" ? "var(--text-muted)" : "var(--text-dim)",
            fontWeight: 400,
          }}>{decodeURIComponent(selectedFile.split("/").pop() ?? "")}</span>
          <span onClick={(e) => { e.stopPropagation(); onCloseFile(); }} style={{
            fontSize: 13, lineHeight: 1, flexShrink: 0, opacity: 0.5, cursor: "pointer",
          }}>✕</span>
        </>}
      </button>

      {/* Ticks tab */}
      <button onClick={() => onCenterTab("ticks")} style={{
        padding: "0 10px", fontSize: 11, fontWeight: 500,
        border: "none", borderBottom: centerTab === "ticks" ? "2px solid var(--amber)" : "2px solid transparent",
        borderTop: "2px solid transparent", background: "transparent",
        color: centerTab === "ticks" ? "var(--amber)" : "var(--text-disabled)",
        cursor: "pointer", transition: "color 0.15s, border-color 0.15s",
      }}>Ticks</button>

      <span style={{ flex: 1 }} />

      {/* New file button */}
      {!selectedFile && (
        <button onClick={onNewFile} style={{
          alignSelf: "center", fontSize: 10, padding: "3px 8px",
          background: "transparent", border: "1px solid var(--border)",
          borderRadius: 5, color: "var(--text-label)", cursor: "pointer",
        }}>+ new file</button>
      )}
    </div>
  );
}

const iconBtnStyle: React.CSSProperties = {
  width: 26, height: 26, display: "flex", alignItems: "center", justifyContent: "center",
  background: "transparent", border: "none", cursor: "pointer",
  color: "var(--text-dim)", borderRadius: 5, flexShrink: 0,
};

// ── File tick list ────────────────────────────────────────────────────────────

const mdComponents: React.ComponentProps<typeof ReactMarkdown>["components"] = {
  p: ({ children }) => (
    <p style={{ margin: "0 0 0.9em", fontSize: 13, lineHeight: 1.8, fontFamily: "Georgia, serif", color: "var(--text-body)" }}>
      {children}
    </p>
  ),
  h1: ({ children }) => <h1 style={{ margin: "0 0 0.6em", fontSize: 20, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h1>,
  h2: ({ children }) => <h2 style={{ margin: "0 0 0.5em", fontSize: 16, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h2>,
  h3: ({ children }) => <h3 style={{ margin: "0 0 0.4em", fontSize: 14, fontFamily: "Georgia, serif", color: "var(--text-heading)", fontWeight: 600 }}>{children}</h3>,
  blockquote: ({ children }) => (
    <blockquote style={{ margin: "0 0 0.9em", paddingLeft: 12, borderLeft: "3px solid var(--border)", color: "var(--text-muted)", fontStyle: "italic" }}>
      {children}
    </blockquote>
  ),
  code: ({ children }) => (
    <code style={{ fontFamily: "monospace", fontSize: 11, background: "oklch(0.18 0.01 60)", padding: "1px 4px", borderRadius: 3, color: "var(--text-label)" }}>
      {children}
    </code>
  ),
  pre: ({ children }) => (
    <pre style={{ margin: "0 0 0.9em", padding: "8px 10px", background: "oklch(0.18 0.01 60)", borderRadius: 5, overflowX: "auto", fontSize: 11, lineHeight: 1.6 }}>
      {children}
    </pre>
  ),
};

function WireTickList({
  ticks, annotationMode, contextAtoms, contextAnnotations,
  onEdit, onToggleContextAtom, onToggleContextAnnotation,
}: {
  ticks: WireTick[];
  annotationMode: AnnotationMode;
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  onEdit: (tickId: string, content: string) => void;
  onToggleContextAtom: (tickId: string) => void;
  onToggleContextAnnotation: (tickId: string) => void;
}) {
  const atomIds = new Set(ticks.filter((t) => t.kind === "atom").map((t) => t.tickId));

  // For each non-atom tick, find the atom it should appear after.
  const annotationsFor = new Map<string, WireTick[]>();
  let lastAtomId: string | null = null;
  for (const tick of ticks) {
    if (tick.kind === "atom") {
      lastAtomId = tick.tickId;
    } else {
      const refAtom = [...tick.refs].reverse().find((r) => atomIds.has(r));
      const anchor = refAtom ?? lastAtomId;
      if (anchor) {
        const arr = annotationsFor.get(anchor) ?? [];
        arr.push(tick);
        annotationsFor.set(anchor, arr);
      }
    }
  }

  const atoms = ticks.filter((t) => t.kind === "atom");

  return (
    <div style={{ flex: 1, overflow: "auto" }}>
      <div style={{ maxWidth: 680, margin: "0 auto", padding: "28px 32px 48px" }}>
        {atoms.map((atom, i) => {
          const anns = annotationsFor.get(atom.tickId) ?? [];
          const isLast = i === atoms.length - 1 && (annotationMode === "hidden" || anns.length === 0);
          return (
            <div key={atom.tickId}>
              <AtomBlock
                atom={atom}
                isLast={isLast}
                inContext={contextAtoms.has(atom.tickId)}
                onEdit={(content) => onEdit(atom.tickId, content)}
                onToggleContext={() => onToggleContextAtom(atom.tickId)}
              />
              {annotationMode === "dots" && anns.length > 0 && (
                <AnnotationDots
                  annotations={anns}
                  contextAnnotations={contextAnnotations}
                  onToggleContext={onToggleContextAnnotation}
                />
              )}
              {annotationMode === "expanded" && anns.map((ann) => (
                <AnnotationCard
                  key={ann.tickId}
                  tick={ann}
                  inContext={contextAnnotations.has(ann.tickId)}
                  onToggleContext={(e) => { if (e.ctrlKey || e.metaKey) onToggleContextAnnotation(ann.tickId); }}
                />
              ))}
            </div>
          );
        })}
      </div>
    </div>
  );
}

function AnnotationCard({ tick, inContext, onToggleContext }: {
  tick: WireTick;
  inContext: boolean;
  onToggleContext: (e: React.MouseEvent) => void;
}) {
  const [expanded, setExpanded] = useState(false);

  const isNote   = tick.kind === "note";
  const isPrompt = tick.kind === "prompt";
  if (!isNote && !isPrompt) return null;

  const accentColor  = isNote ? "oklch(0.55 0.15 240)" : "var(--amber)";
  const bgColor      = isNote ? "oklch(0.22 0.01 240 / 0.6)" : "oklch(0.78 0.10 65 / 0.08)";
  const borderColor  = isNote ? "oklch(0.35 0.04 240 / 0.4)" : "oklch(0.78 0.10 65 / 0.25)";
  const Icon         = isNote ? StickyNote : Sparkles;
  const expandable   = tick.message.length > 60;
  const preview      = expandable ? tick.message.slice(0, 60) + "…" : tick.message;

  return (
    <div
      onClick={(e) => {
        if (e.ctrlKey || e.metaKey) { onToggleContext(e); return; }
        if (expandable) setExpanded((v) => !v);
      }}
      style={{
        margin: "4px 0 10px 12px",
        borderRadius: 5,
        background: bgColor,
        border: `1px solid ${borderColor}`,
        outline: inContext ? `2px solid var(--amber)` : "none",
        outlineOffset: 1,
        cursor: expandable ? "pointer" : "default",
        transition: "outline 0.12s",
      }}
    >
      <div style={{ display: "flex", alignItems: "center", gap: 7, padding: "5px 10px" }}>
        <Icon style={{ width: 11, height: 11, color: accentColor, flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: isNote ? "var(--text-muted)" : "var(--amber)", fontStyle: "italic", lineHeight: 1.5, flex: 1, opacity: expanded ? 1 : 0.85 }}>
          {expanded ? tick.message : preview}
        </span>
        {expandable && (
          <ChevronRight style={{
            width: 10, height: 10, color: "var(--text-ghost)", flexShrink: 0,
            transform: expanded ? "rotate(90deg)" : "none",
            transition: "transform 0.15s",
          }} />
        )}
      </div>
    </div>
  );
}

function AnnotationDots({ annotations, contextAnnotations, onToggleContext }: {
  annotations: WireTick[];
  contextAnnotations: Set<string>;
  onToggleContext: (tickId: string) => void;
}) {
  const [expandedId, setExpandedId] = useState<string | null>(null);

  function dotColor(kind: string): string {
    if (kind === "note")   return "oklch(0.55 0.15 240)";
    if (kind === "prompt") return "var(--amber)";
    return "var(--text-dim)";
  }

  return (
    <div style={{ margin: "2px 0 10px 12px" }}>
      <div style={{ display: "flex", gap: 5, alignItems: "center", flexWrap: "wrap" }}>
        {annotations.map((ann) => {
          const inCtx    = contextAnnotations.has(ann.tickId);
          const isOpen   = expandedId === ann.tickId;
          const color    = dotColor(ann.kind);
          return (
            <button
              key={ann.tickId}
              title={ann.message.slice(0, 80)}
              onClick={(e) => {
                if (e.ctrlKey || e.metaKey) { onToggleContext(ann.tickId); return; }
                setExpandedId((id) => id === ann.tickId ? null : ann.tickId);
              }}
              style={{
                width: 8, height: 8, borderRadius: "50%", border: "none",
                background: color,
                cursor: "pointer", padding: 0, flexShrink: 0,
                outline: inCtx ? "2px solid var(--amber)" : isOpen ? `1px solid ${color}` : "none",
                outlineOffset: 2,
                opacity: isOpen ? 1 : 0.6,
                boxShadow: isOpen ? `0 0 3px 0px ${color}` : "none",
                transform: isOpen ? "scale(1.25)" : "scale(1)",
                transition: "transform 0.1s, outline 0.12s, box-shadow 0.12s, opacity 0.12s",
              }}
              onMouseEnter={(e) => { if (!isOpen) (e.currentTarget as HTMLElement).style.transform = "scale(1.35)"; }}
              onMouseLeave={(e) => { if (!isOpen) (e.currentTarget as HTMLElement).style.transform = "scale(1)"; }}
            />
          );
        })}
      </div>
      {expandedId && (() => {
        const ann = annotations.find((a) => a.tickId === expandedId);
        if (!ann) return null;
        return (
          <AnnotationCard
            tick={ann}
            inContext={contextAnnotations.has(ann.tickId)}
            onToggleContext={(e) => { if (e.ctrlKey || e.metaKey) onToggleContext(ann.tickId); }}
          />
        );
      })()}
    </div>
  );
}

function AtomBlock({ atom, isLast, inContext, onEdit, onToggleContext }: {
  atom: WireTick;
  isLast: boolean;
  inContext: boolean;
  onEdit: (content: string) => void;
  onToggleContext: () => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  const content = atom.content ?? "";

  function startEdit() {
    setDraft(content);
    setEditing(true);
    setTimeout(() => textareaRef.current?.focus(), 0);
  }

  function commitEdit() {
    const trimmed = draft.trim();
    if (trimmed && trimmed !== content.trim()) onEdit(trimmed);
    setEditing(false);
  }

  function cancelEdit() {
    setEditing(false);
  }

  const barColor = inContext
    ? "var(--amber)"
    : hovered
      ? "oklch(0.40 0.03 60)"
      : "oklch(0.20 0.01 60)";

  return (
    <div
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => setHovered(false)}
      style={{
        position: "relative",
        paddingLeft: 10, marginLeft: -12,
        background: inContext ? "oklch(0.78 0.10 65 / 0.04)" : "transparent",
        borderRadius: inContext ? 4 : 0,
        marginBottom: isLast ? 0 : editing ? 16 : undefined,
        transition: "background 0.15s",
      }}
    >
      {/* Left selection bar — wider hit area, visually 2px */}
      <div
        onClick={(e) => { e.stopPropagation(); onToggleContext(); }}
        title={inContext ? "Remove from context" : "Add to context"}
        style={{
          position: "absolute", left: 0, top: 0, bottom: 0,
          width: 10, cursor: "pointer", display: "flex", alignItems: "stretch",
        }}
      >
        <div style={{
          width: 2, height: "100%",
          background: barColor,
          transition: "background 0.15s",
          borderRadius: 1,
        }} />
      </div>

      {editing ? (
        <div style={{ padding: "10px 0 14px" }}>
          <textarea
            ref={textareaRef}
            value={draft}
            onChange={(e) => setDraft(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); commitEdit(); }
              if (e.key === "Escape") cancelEdit();
            }}
            style={{
              width: "100%", boxSizing: "border-box",
              minHeight: 80, resize: "vertical",
              background: "var(--surface-deep)",
              border: "1px solid oklch(0.78 0.10 65 / 0.4)",
              borderRadius: 4, padding: "6px 8px",
              color: "var(--text-primary)", fontSize: 14, lineHeight: 1.6,
              fontFamily: "inherit", outline: "none",
            }}
          />
          <div style={{ display: "flex", gap: 6, justifyContent: "flex-end", marginTop: 4 }}>
            <button onClick={cancelEdit} style={{
              background: "none", border: "1px solid var(--border-subtle)", borderRadius: 3,
              color: "var(--text-ghost)", fontSize: 11, padding: "2px 8px", cursor: "pointer",
            }}>Cancel</button>
            <button onClick={commitEdit} style={{
              background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.4)",
              borderRadius: 3, color: "var(--text-secondary)", fontSize: 11, padding: "2px 8px", cursor: "pointer",
            }}>Save</button>
          </div>
        </div>
      ) : (
        <div
          onDoubleClick={startEdit}
          onClick={(e) => { if (e.ctrlKey || e.metaKey) { e.preventDefault(); onToggleContext(); } }}
        >
          <ReactMarkdown components={mdComponents}>{content}</ReactMarkdown>
        </div>
      )}


      {/* Tick id */}
      <span style={{
        position: "absolute", bottom: 0, right: 0,
        fontSize: 9, fontFamily: "monospace", lineHeight: 1,
        color: hovered ? "var(--text-ghost)" : "transparent",
        userSelect: "all", transition: "color 0.15s", pointerEvents: "none",
      }}>
        {atom.tickId.slice(0, 12)}
      </span>
    </div>
  );
}

// ── Agent log strip ───────────────────────────────────────────────────────────

function AgentLogStrip({ logs, onClear }: {
  logs: { level: string; message: string }[];
  onClear: () => void;
}) {
  const containerRef = useRef<HTMLDivElement>(null);
  const [height, setHeight] = useState(0);
  const [collapsed, setCollapsed] = useState(false);

  // Scroll to bottom on new entries, and when resizing (stay anchored to bottom)
  useEffect(() => {
    const el = containerRef.current;
    if (el) el.scrollTop = el.scrollHeight;
  }, [logs, height]);

  // Expand when first log arrives
  useEffect(() => {
    if (logs.length > 0 && height === 0) {
      setHeight(102);
      setCollapsed(false);
    }
  }, [logs.length, height]);

  if (logs.length === 0) return null;

  if (collapsed) return (
    <div
      key={logs.length}
      className="log-pulse"
      onClick={() => setCollapsed(false)}
      title="Expand log"
      style={{
        flexShrink: 0, height: 16, display: "flex", alignItems: "center", justifyContent: "center",
        borderTop: "1px solid oklch(0.17 0.01 60)", cursor: "pointer",
      }}
      onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.background = "oklch(0.16 0.01 60)"; }}
      onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.background = ""; }}
    >
      <ChevronUp style={{ width: 10, height: 10, color: "oklch(0.35 0.01 60)" }} />
    </div>
  );

  function levelColor(level: string) {
    if (level === "warning") return "var(--amber)";
    if (level === "error")   return "var(--rose)";
    return "oklch(0.38 0.01 60)";
  }

  function onDragHandleMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    const startY = e.clientY;
    const startH = height;
    function onMove(ev: MouseEvent) {
      setHeight(Math.max(40, Math.min(300, startH + (startY - ev.clientY))));
    }
    function onUp() {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
    }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  const btnStyle: React.CSSProperties = {
    background: "none", border: "none", cursor: "pointer", padding: "0 4px",
    color: "oklch(0.35 0.01 60)", fontSize: 11, lineHeight: 1,
    transition: "color 0.12s",
  };

  return (
    <div style={{
      flexShrink: 0,
      borderTop: "1px solid oklch(0.17 0.01 60)",
      background: "oklch(0.11 0.005 60)",
    }}>
      <div style={{ display: "flex", alignItems: "center", height: 16 }}>
        <div onMouseDown={onDragHandleMouseDown} style={{ flex: 1, height: "100%", cursor: "ns-resize" }} />
        <button style={btnStyle} title="Collapse" onClick={() => setCollapsed(true)}
          onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.color = "var(--text-ghost)"; }}
          onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.color = "oklch(0.35 0.01 60)"; }}
        ><ChevronDown style={{ width: 10, height: 10 }} /></button>
        <div onMouseDown={onDragHandleMouseDown} style={{ flex: 1, height: "100%", cursor: "ns-resize" }} />
        <button style={{ ...btnStyle, paddingRight: 8 }} title="Clear log" onClick={() => { onClear(); setHeight(0); }}
          onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.color = "var(--rose)"; }}
          onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.color = "oklch(0.35 0.01 60)"; }}
        >×</button>
      </div>
      <div ref={containerRef} style={{ height, overflow: "auto", padding: "0 14px 6px" }}>
        {logs.map((entry, i) => (
          <div key={i} style={{ display: "flex", gap: 8, lineHeight: 1.5 }}>
            <span style={{ fontSize: 9, color: levelColor(entry.level), fontFamily: "monospace", flexShrink: 0, paddingTop: 1 }}>
              {entry.level}
            </span>
            <span style={{ fontSize: 10, color: "oklch(0.42 0.01 60)", fontFamily: "monospace", whiteSpace: "pre-wrap", wordBreak: "break-all" }}>
              {entry.message}
            </span>
          </div>
        ))}
      </div>
    </div>
  );
}

// ── Input bar ─────────────────────────────────────────────────────────────────

function InputBar({ enabled, contextAtomCount, contextAnnotationCount, onClearContext, onAppend, onWrite }: {
  enabled: boolean;
  contextAtomCount: number;
  contextAnnotationCount: number;
  onClearContext: () => void;
  onAppend: (text: string) => void;
  onWrite:  (text: string) => void;
}) {
  const [text, setText] = useState("");
  const [height, setHeight] = useState(90);

  const hasContext = contextAtomCount > 0 || contextAnnotationCount > 0;

  function send(action: (t: string) => void) {
    const t = text.trim();
    if (t) { action(t); setText(""); }
  }

  function onDragHandleMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    const startY = e.clientY;
    const startH = height;
    function onMove(ev: MouseEvent) {
      setHeight(Math.max(60, Math.min(400, startH + (startY - ev.clientY))));
    }
    function onUp() {
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
    }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  function contextLabel() {
    const parts: string[] = [];
    if (contextAtomCount > 0) parts.push(`${contextAtomCount} atom${contextAtomCount !== 1 ? "s" : ""}`);
    if (contextAnnotationCount > 0) parts.push(`${contextAnnotationCount} annotation${contextAnnotationCount !== 1 ? "s" : ""}`);
    return parts.join(" · ") + " selected";
  }

  return (
    <div style={{
      flexShrink: 0, height,
      borderTop: "1px solid var(--border-subtle)",
      background: "var(--surface-deep)",
      display: "flex", flexDirection: "column",
      opacity: enabled ? 1 : 0.4, pointerEvents: enabled ? "auto" : "none",
    }}>
      <div onMouseDown={onDragHandleMouseDown} style={{ height: 4, cursor: "ns-resize", flexShrink: 0 }} />
      {hasContext && (
        <div style={{
          flexShrink: 0, padding: "2px 16px 0",
          display: "flex", alignItems: "center", gap: 6,
        }}>
          <span style={{ fontSize: 10, color: "var(--amber)", fontStyle: "italic" }}>{contextLabel()}</span>
          <button
            onClick={onClearContext}
            title="Clear context selection"
            style={{
              background: "none", border: "none", cursor: "pointer",
              color: "var(--amber)", fontSize: 12, lineHeight: 1, padding: "0 2px", opacity: 0.7,
            }}
          >×</button>
        </div>
      )}
      <div style={{ flex: 1, display: "flex", gap: 8, alignItems: "stretch", padding: "4px 16px 10px", minHeight: 0 }}>
        <textarea
          value={text}
          onChange={(e) => setText(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === "Enter" && e.metaKey  && !e.shiftKey) { e.preventDefault(); send(onAppend); }
            if (e.key === "Enter" && e.metaKey  &&  e.shiftKey) { e.preventDefault(); send(onWrite);  }
            if (e.key === "Enter" && e.ctrlKey  && !e.shiftKey) { e.preventDefault(); send(onAppend); }
            if (e.key === "Enter" && e.ctrlKey  &&  e.shiftKey) { e.preventDefault(); send(onWrite);  }
          }}
          placeholder={enabled ? "⌘↵ append · ⌘⇧↵ write" : "Open a file to write"}
          style={{
            flex: 1, resize: "none", fontFamily: "Georgia, serif", fontSize: 12,
            background: "var(--card)", border: "1px solid var(--border)", borderRadius: 6,
            color: "var(--foreground)", padding: "6px 8px", outline: "none",
          }}
        />
        <div style={{ display: "flex", flexDirection: "column", gap: 4, justifyContent: "flex-end" }}>
          <button
            onClick={() => send(onAppend)}
            title="Append verbatim (⌘↵)"
            style={{
              padding: "4px 10px", background: "var(--amber)", border: "none", borderRadius: 5,
              color: "oklch(0.15 0.01 60)", fontWeight: 600, fontSize: 11, cursor: "pointer",
            }}
          >Append</button>
          <button
            onClick={() => send(onWrite)}
            title="Send to writer agent (⌘⇧↵)"
            style={{
              padding: "4px 10px",
              background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.35)",
              borderRadius: 5, color: "var(--amber)", fontWeight: 600, fontSize: 11, cursor: "pointer",
              display: "flex", alignItems: "center", gap: 4,
            }}
          >
            <Sparkles style={{ width: 10, height: 10 }} />
            Write
          </button>
        </div>
      </div>
    </div>
  );
}

// ── Ticks view ────────────────────────────────────────────────────────────────

function TicksView({
  activeBranch, ticks,
  onAddNote, onMoveTick, onDeleteTick,
}: {
  activeBranch: string | null;
  ticks: WireTick[];
  onAddNote: (refTickId: string, text: string) => void;
  onMoveTick: (tickId: string, afterTickId?: string) => void;
  onDeleteTick: (tickId: string) => void;
}) {
  if (!activeBranch) return (
    <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
      Select a branch to view ticks
    </div>
  );

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      {/* Toolbar */}
      <div style={{
        flexShrink: 0, padding: "5px 16px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8,
      }}>
        <span style={{ fontSize: 10, color: "var(--text-ghost)", marginLeft: "auto" }}>
          {ticks.length} tick{ticks.length !== 1 ? "s" : ""}
        </span>
      </div>

      {ticks.length === 0 ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          No ticks yet
        </div>
      ) : (
        <div style={{ flex: 1, overflow: "auto" }}>
          <div style={{ maxWidth: 1100, margin: "0 auto", padding: "16px 32px 48px" }}>
            {ticks.map((tick, i) => {
              const isFirst = i === 0;
              const isLast  = i === ticks.length - 1;
              const prevTick = i > 0 ? ticks[i - 1] : undefined;
              return (
                <TickRow
                  key={tick.tickId}
                  tick={tick}
                  allTicks={ticks}
                  isFirst={isFirst}
                  isLast={isLast}
                  prevTick={prevTick}
                  onAddNote={onAddNote}
                  onMoveUp={() => onMoveTick(tick.tickId, prevTick?.tickId)}
                  onMoveDown={() => !isLast && onMoveTick(tick.tickId, i + 2 < ticks.length ? ticks[i + 2].tickId : undefined)}
                  onDelete={() => onDeleteTick(tick.tickId)}
                />
              );
            })}
          </div>
        </div>
      )}
    </div>
  );
}

function TickRow({
  tick, allTicks, isFirst, isLast, prevTick,
  onAddNote, onMoveUp, onMoveDown, onDelete,
}: {
  tick: WireTick;
  allTicks: WireTick[];
  isFirst: boolean;
  isLast: boolean;
  prevTick: WireTick | undefined;
  onAddNote: (refTickId: string, text: string) => void;
  onMoveUp: () => void;
  onMoveDown: () => void;
  onDelete: () => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [addingNote, setAddingNote] = useState(false);
  const [noteText, setNoteText] = useState("");

  // Ordering invariant: all ticks you reference must be below you (older);
  // all ticks that reference you must be above you (newer).
  // Moving one step: blocked if the adjacent tick would violate this.
  const refsOf = (t: WireTick): string[] => t.refs ?? [];

  const myIdx = allTicks.findIndex((t) => t.tickId === tick.tickId);
  const above = allTicks[myIdx - 1];
  const below = allTicks[myIdx + 1];

  // Can't move up if the tick above references us (it must stay above us).
  // Can't move down if the tick below references us, or if we reference it (it must stay below us).
  const canMoveUp   = !isFirst && !refsOf(above).includes(tick.tickId);
  const canMoveDown = !isLast  && !refsOf(below).includes(tick.tickId) && !refsOf(tick).includes(below?.tickId);

  const tickContent = () => {
    if (tick.kind === "note") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>
          {tick.tickId.slice(0, 12)}
        </span>
        <StickyNote style={{ width: 11, height: 11, color: "oklch(0.55 0.15 240)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--text-muted)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPayload(tick.message)}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          → {(tick.refs?.[0] ?? "").slice(0, 12)}
        </span>
      </div>
    );
    if (tick.kind === "prompt") return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>
          {tick.tickId.slice(0, 12)}
        </span>
        <Sparkles style={{ width: 11, height: 11, color: "var(--amber)", flexShrink: 0 }} />
        <span style={{ fontSize: 12, color: "var(--amber)", fontStyle: "italic", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "inherit" }}>
          {tickPayload(tick.message)}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          {tickField(tick, "file")}
        </span>
      </div>
    );
    return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>
          {tick.tickId.slice(0, 12)}
        </span>
        <span style={{ fontSize: 13, color: "var(--text-secondary)", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {tickPayload(tick.message)}
        </span>
        {tickField(tick, "file") && (
          <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", transition: "color 0.15s", flexShrink: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", minWidth: 0 }}>
            {tickField(tick, "file")}
          </span>
        )}
        {tick.refs.length > 0 && (
          <span style={{ fontSize: 9, color: "var(--text-ghost)", flexShrink: 0 }}>
            {tick.refs.length} ref{tick.refs.length > 1 ? "s" : ""}
          </span>
        )}
      </div>
    );
  };

  return (
    <div
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => setHovered(false)}
      style={{
        borderBottom: "1px solid var(--border-subtle)",
        marginBottom: 2,
      }}
    >
      <div style={{ position: "relative", display: "flex", alignItems: "center", padding: "5px 0", paddingRight: hovered ? 88 : 0, transition: "padding-right 0.12s" }}>
        {tickContent()}
        <div style={{ position: "absolute", right: 0, display: "flex", gap: 2, opacity: hovered ? 1 : 0, transition: "opacity 0.12s", pointerEvents: hovered ? "auto" : "none", background: "var(--surface-deep)", paddingLeft: 4 }}>
          <MoveButton disabled={!canMoveUp} onClick={onMoveUp} title="Move up">
            <MoveUp style={{ width: 10, height: 10 }} />
          </MoveButton>
          <MoveButton disabled={!canMoveDown} onClick={onMoveDown} title="Move down">
            <MoveDown style={{ width: 10, height: 10 }} />
          </MoveButton>
          {tick.kind === "atom" && (
            <MoveButton disabled={false} onClick={() => setAddingNote((v) => !v)} title="Add note">
              <MessageSquare style={{ width: 10, height: 10 }} />
            </MoveButton>
          )}
          <MoveButton disabled={false} onClick={onDelete} title="Delete tick" danger>
            <Trash2 style={{ width: 10, height: 10 }} />
          </MoveButton>
        </div>
      </div>

      {addingNote && (
        <div style={{ display: "flex", gap: 6, padding: "4px 0 6px 20px", alignItems: "center" }}>
          <input
            autoFocus
            value={noteText}
            onChange={(e) => setNoteText(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && noteText.trim()) {
                onAddNote(tick.tickId, noteText.trim());
                setNoteText("");
                setAddingNote(false);
              }
              if (e.key === "Escape") { setAddingNote(false); setNoteText(""); }
            }}
            placeholder="Add annotation…"
            style={{
              flex: 1, fontSize: 11, padding: "3px 7px",
              background: "var(--card)", border: "1px solid var(--border-subtle)",
              borderRadius: 4, color: "var(--foreground)", outline: "none",
            }}
          />
          <button
            onClick={() => { if (noteText.trim()) { onAddNote(tick.tickId, noteText.trim()); setNoteText(""); setAddingNote(false); } }}
            style={{
              fontSize: 10, padding: "3px 8px",
              background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)",
              borderRadius: 4, color: "var(--amber)", cursor: "pointer",
            }}
          >Add</button>
        </div>
      )}
    </div>
  );
}

function MoveButton({ disabled, onClick, title, danger, children }: {
  disabled: boolean; onClick: () => void; title: string; danger?: boolean; children: React.ReactNode;
}) {
  return (
    <button
      onClick={disabled ? undefined : onClick}
      title={title}
      style={{
        width: 20, height: 20, display: "flex", alignItems: "center", justifyContent: "center",
        background: "none", border: "none", cursor: disabled ? "default" : "pointer",
        color: disabled ? "var(--text-disabled)" : danger ? "var(--rose)" : "var(--text-ghost)",
        borderRadius: 3, padding: 0,
        opacity: disabled ? 0.35 : 1,
        transition: "color 0.12s, opacity 0.12s",
      }}
    >
      {children}
    </button>
  );
}

// ── Root ──────────────────────────────────────────────────────────────────────

export default function Home() {
  const {
    conns, error, branches, activeBranch, files, ticks, branchHead, openFiles,
    agentLogs,
    contextAtoms, contextAnnotations,
    connect, createBranch, deleteBranch, selectBranch, openFile, closeFile,
    appendToFile, editAtom, deleteAtom, addNote, moveTick, deleteTickEntry,
    toggleContextAtom, toggleContextAnnotation, clearContext, clearAgentLogs,
    chatPrompt,
  } = useStory();

  const [annotationMode, setAnnotationMode] = useState<AnnotationMode>("expanded");
  function cycleAnnotationMode() {
    setAnnotationMode((m) => m === "hidden" ? "dots" : m === "dots" ? "expanded" : "hidden");
  }

  const [leftOpen, setLeftOpen] = useState(true);
  const [leftWidth, setLeftWidth] = useState(260);
  const [isResizing, setIsResizing] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<"explorer" | "branches">("branches");
  const [centerTab, setCenterTab] = useState<"file" | "ticks">("file");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);
  const [showNewFile, setShowNewFile] = useState(false);
  const [newFilePath, setNewFilePath] = useState("");

  const sessionStatus = conns.find((c) => c.label === "session")?.status ?? "disconnected";

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => { connect(); }, []);

  const fileConn = selectedFile ? openFiles[selectedFile] : null;
  const fileTicks = tickChain(fileConn?.ticks ?? {}, fileConn?.head ?? null);
  const isAbsent = fileConn?.absent ?? false;
  const atomCount       = fileTicks.filter((t) => t.kind === "atom").length;
  const annotationCount = fileTicks.filter((t) => t.kind !== "atom").length;

  function handleSelectFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setSelectedFile(path);
    openFile(path);
    setCenterTab("file");
  }

  function handleCloseFile() {
    if (selectedFile) closeFile(selectedFile);
    setSelectedFile(null);
  }

  function handleSelectBranch(name: string) {
    handleCloseFile();
    selectBranch(name);
    setSidebarTab("explorer");
  }

  function handleCreateFile() {
    const path = newFilePath.trim().replace(/^\/+/, "");
    if (!path) return;
    setShowNewFile(false);
    setNewFilePath("");
    handleSelectFile(path);
  }

  // Sidebar resize drag
  function onSidebarResizeMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    setIsResizing(true);
    const startX = e.clientX;
    const startW = leftWidth;
    function onMove(ev: MouseEvent) {
      setLeftWidth(Math.max(180, Math.min(420, startW + (ev.clientX - startX))));
    }
    function onUp() {
      setIsResizing(false);
      window.removeEventListener("mousemove", onMove);
      window.removeEventListener("mouseup", onUp);
    }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100vh", overflow: "hidden", cursor: isResizing ? "col-resize" : undefined }}>
      <TopBar sessionStatus={sessionStatus} branches={branches} activeBranch={activeBranch} />

      <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
        {/* Left sidebar */}
        {leftOpen && (
          <div style={{ width: leftWidth, minWidth: leftWidth, position: "relative", display: "flex", flexDirection: "column" }}>
            <LeftSidebar
              tab={sidebarTab} setTab={setSidebarTab}
              branches={branches} activeBranch={activeBranch}
              files={files} selectedFile={selectedFile}
              onSelectBranch={handleSelectBranch}
              onSelectFile={handleSelectFile}
              onCreateBranch={createBranch}
              onDeleteBranch={deleteBranch}
              conns={conns} error={error}
            />
            {/* Resize handle */}
            <div
              onMouseDown={onSidebarResizeMouseDown}
              style={{
                position: "absolute", top: 0, right: 0, bottom: 0, width: 4,
                cursor: "col-resize", zIndex: 10,
                borderRight: "1px solid var(--border-subtle)",
                background: isResizing ? "oklch(0.25 0.015 60 / 0.4)" : "transparent",
                transition: "background 0.15s",
              }}
            />
          </div>
        )}

        {/* Center */}
        <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden", minWidth: 0 }}>
          <Toolbar
            leftOpen={leftOpen} onToggleLeft={() => setLeftOpen((v) => !v)}
            selectedFile={selectedFile}
            onCloseFile={handleCloseFile}
            onNewFile={() => setShowNewFile((v) => !v)}
            centerTab={centerTab} onCenterTab={setCenterTab}
          />

          {centerTab === "file" && <>
            {/* New file bar */}
            {showNewFile && (
              <div style={{
                flexShrink: 0, padding: "7px 14px",
                borderBottom: "1px solid var(--border-subtle)",
                background: "var(--card)", display: "flex", gap: 8, alignItems: "center",
              }}>
                <input
                  value={newFilePath}
                  onChange={(e) => setNewFilePath(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") handleCreateFile();
                    if (e.key === "Escape") setShowNewFile(false);
                  }}
                  placeholder="path/to/file.md"
                  autoFocus
                  style={{
                    flex: 1, fontSize: 11, padding: "4px 8px",
                    background: "var(--card)", border: "1px solid var(--border)",
                    borderRadius: 5, color: "var(--foreground)", outline: "none",
                  }}
                />
                <button onClick={handleCreateFile} style={{ fontSize: 11, padding: "4px 10px", background: "var(--amber)", border: "none", borderRadius: 5, color: "oklch(0.15 0.01 60)", fontWeight: 600, cursor: "pointer" }}>Open</button>
                <button onClick={() => setShowNewFile(false)} style={{ fontSize: 11, padding: "4px 8px", background: "transparent", border: "1px solid var(--border)", borderRadius: 5, color: "var(--text-label)", cursor: "pointer" }}>✕</button>
              </div>
            )}

            {/* File view toolbar: annotation mode + atom count */}
            {selectedFile && !isAbsent && fileTicks.length > 0 && (
              <div style={{
                flexShrink: 0, padding: "3px 14px",
                borderBottom: "1px solid var(--border-subtle)",
                display: "flex", alignItems: "center",
              }}>
                {contextAtoms.size > 0 && (
                  <button
                    onClick={() => {
                      // Delete tail-first: atoms after the deleted one get renumbered,
                      // so deleting from the end preserves earlier ids in the queue.
                      const ordered = fileTicks
                        .filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
                        .map((t) => t.tickId)
                        .reverse();
                      ordered.forEach((tickId) => selectedFile && deleteAtom(selectedFile, tickId));
                      clearContext();
                    }}
                    title={`Delete ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""}`}
                    style={{
                      display: "flex", alignItems: "center", gap: 4, fontSize: 10,
                      padding: "2px 7px", borderRadius: 4, cursor: "pointer",
                      background: "oklch(0.65 0.18 25 / 0.15)", border: "1px solid oklch(0.65 0.18 25 / 0.35)",
                      color: "var(--rose)",
                    }}
                  >
                    <Trash2 style={{ width: 10, height: 10 }} />
                    Delete {contextAtoms.size}
                  </button>
                )}
                <span style={{ flex: 1 }} />
                <span style={{ fontSize: 9, color: "var(--text-ghost)", marginRight: 8 }}>
                  {atomCount} atom{atomCount !== 1 ? "s" : ""}
                  {annotationCount > 0 && <> · {annotationCount} ann</>}
                </span>
                <button
                  onClick={cycleAnnotationMode}
                  title={annotationMode === "hidden" ? "Show annotation dots" : annotationMode === "dots" ? "Expand annotations" : "Hide annotations"}
                  style={{
                    width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center",
                    borderRadius: 4, cursor: "pointer", border: "none",
                    background: annotationMode !== "hidden" ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
                    color: annotationMode === "expanded" ? "var(--amber)" : annotationMode === "dots" ? "oklch(0.65 0.08 65)" : "var(--text-dim)",
                  }}
                >
                  {annotationMode === "hidden"
                    ? <EyeOff style={{ width: 11, height: 11 }} />
                    : annotationMode === "dots"
                      ? <Eye style={{ width: 11, height: 11, opacity: 0.6 }} />
                      : <Eye style={{ width: 11, height: 11 }} />}
                </button>
              </div>
            )}

            {!activeBranch ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                {sessionStatus === "connecting" ? "Connecting…" : sessionStatus === "connected" ? "Select a branch" : "Disconnected"}
              </div>
            ) : !selectedFile ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                Select a file or create a new one
              </div>
            ) : isAbsent ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                File does not exist yet — append to create it
              </div>
            ) : fileTicks.length === 0 && !isAbsent ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                Loading…
              </div>
            ) : (
              <WireTickList
                ticks={fileTicks}
                annotationMode={annotationMode}
                contextAtoms={contextAtoms}
                contextAnnotations={contextAnnotations}
                onEdit={(tickId, content) => selectedFile && editAtom(selectedFile, tickId, content)}
                onToggleContextAtom={toggleContextAtom}
                onToggleContextAnnotation={toggleContextAnnotation}
              />
            )}

            <AgentLogStrip logs={agentLogs} onClear={clearAgentLogs} />
            <InputBar
              enabled={selectedFile !== null}
              contextAtomCount={contextAtoms.size}
              contextAnnotationCount={contextAnnotations.size}
              onClearContext={clearContext}
              onAppend={(text) => selectedFile && appendToFile(selectedFile, text)}
              onWrite={(text)  => selectedFile && chatPrompt(selectedFile, text)}
            />
          </>}

          {centerTab === "ticks" && (
            <TicksView
              activeBranch={activeBranch}
              ticks={tickChain(ticks, branchHead).reverse()}
              onAddNote={addNote}
              onMoveTick={moveTick}
              onDeleteTick={deleteTickEntry}
            />
          )}
        </div>
      </div>

    </div>
  );
}

function statusColor(status: string): string {
  switch (status) {
    case "connected":  return "var(--emerald)";
    case "connecting": return "var(--amber)";
    case "error":      return "var(--rose)";
    default:           return "var(--text-dim)";
  }
}
