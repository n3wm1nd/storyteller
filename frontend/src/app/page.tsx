"use client";

import { useEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import {
  PanelLeftClose, PanelLeftOpen,
  Folder, FolderOpen, FileText, GitBranch, ChevronRight,
  Sparkles, Plus,
} from "lucide-react";
import { useStory, type ConnInfo, type FileAtom, type BranchTick } from "@/lib/store";
import { MessageSquare, StickyNote, Trash2, MoveUp, MoveDown } from "lucide-react";

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
            <div style={{ width: 5, height: 5, borderRadius: "50%", background: statusColor(c.status), flexShrink: 0 }} />
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
      let node = nodes.find((n) => n.name === parts[i]);
      if (!node) {
        node = { name: parts[i], path: builtPath, isDir: !isLast, children: [] };
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

// ── Center toolbar strip ──────────────────────────────────────────────────────

function Toolbar({
  leftOpen, onToggleLeft,
  selectedFile, atomCount, onCloseFile, onNewFile,
  centerTab, onCenterTab,
}: {
  leftOpen: boolean;
  onToggleLeft: () => void;
  selectedFile: string | null;
  atomCount: number;
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
          }}>{selectedFile.split("/").pop()}</span>
          {atomCount > 0 && (
            <span style={{ fontSize: 9, color: "var(--text-dim)", flexShrink: 0, fontWeight: 400 }}>
              {atomCount}
            </span>
          )}
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

// ── Atom chain ────────────────────────────────────────────────────────────────

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

function AtomChain({ atoms, onEdit, onDelete }: {
  atoms: FileAtom[];
  onEdit: (tickId: string, content: string) => void;
  onDelete: (tickId: string) => void;
}) {
  return (
    <div style={{ flex: 1, overflow: "auto" }}>
      <div style={{ maxWidth: 680, margin: "0 auto", padding: "28px 32px 48px" }}>
        {atoms.map((atom, i) => (
          <AtomBlock
            key={atom.tickId}
            atom={atom}
            isLast={i === atoms.length - 1}
            onEdit={(content) => onEdit(atom.tickId, content)}
            onDelete={() => onDelete(atom.tickId)}
          />
        ))}
      </div>
    </div>
  );
}

function AtomBlock({ atom, isLast, onEdit, onDelete }: {
  atom: FileAtom;
  isLast: boolean;
  onEdit: (content: string) => void;
  onDelete: () => void;
}) {
  const [hovered, setHovered] = useState(false);
  const [editing, setEditing] = useState(false);
  const [draft, setDraft] = useState("");
  const textareaRef = useRef<HTMLTextAreaElement>(null);

  function startEdit() {
    setDraft(atom.content);
    setEditing(true);
    setTimeout(() => textareaRef.current?.focus(), 0);
  }

  function commitEdit() {
    const trimmed = draft.trim();
    if (trimmed && trimmed !== atom.content.trim()) onEdit(trimmed);
    setEditing(false);
  }

  function cancelEdit() {
    setEditing(false);
  }

  return (
    <div
      onMouseEnter={() => setHovered(true)}
      onMouseLeave={() => setHovered(false)}
      style={{
        position: "relative",
        borderLeft: hovered ? "2px solid oklch(0.78 0.10 65 / 0.4)" : "2px solid transparent",
        paddingLeft: 10, marginLeft: -12,
        transition: "border-color 0.15s",
        marginBottom: isLast ? 0 : undefined,
      }}
    >
      {editing ? (
        <div>
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
        <div onDoubleClick={startEdit}>
          <ReactMarkdown components={mdComponents}>{atom.content}</ReactMarkdown>
        </div>
      )}

      {/* Hover controls */}
      {hovered && !editing && (
        <button
          onClick={onDelete}
          title="Delete atom"
          style={{
            position: "absolute", top: 0, right: 0,
            background: "none", border: "none", cursor: "pointer",
            color: "var(--text-ghost)", fontSize: 12, lineHeight: 1,
            padding: "2px 4px", opacity: 0.6,
            transition: "opacity 0.15s, color 0.15s",
          }}
          onMouseEnter={(e) => { (e.currentTarget as HTMLElement).style.opacity = "1"; (e.currentTarget as HTMLElement).style.color = "oklch(0.65 0.18 25)"; }}
          onMouseLeave={(e) => { (e.currentTarget as HTMLElement).style.opacity = "0.6"; (e.currentTarget as HTMLElement).style.color = "var(--text-ghost)"; }}
        >
          ×
        </button>
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

// ── Input bar ─────────────────────────────────────────────────────────────────

function InputBar({ enabled, onAppend, onWrite }: {
  enabled: boolean;
  onAppend: (text: string) => void;
  onWrite:  (text: string) => void;
}) {
  const [text, setText] = useState("");
  const [height, setHeight] = useState(90);

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

  return (
    <div style={{
      flexShrink: 0, height,
      borderTop: "1px solid var(--border-subtle)",
      background: "var(--surface-deep)",
      display: "flex", flexDirection: "column",
      opacity: enabled ? 1 : 0.4, pointerEvents: enabled ? "auto" : "none",
    }}>
      <div onMouseDown={onDragHandleMouseDown} style={{ height: 4, cursor: "ns-resize", flexShrink: 0 }} />
      <div style={{ flex: 1, display: "flex", gap: 8, alignItems: "stretch", padding: "0 16px 10px", minHeight: 0 }}>
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
  activeBranch, ticks, showNotes,
  onToggleNotes, onAddNote, onMoveTick, onDeleteTick,
}: {
  activeBranch: string | null;
  ticks: BranchTick[];
  showNotes: boolean;
  onToggleNotes: () => void;
  onAddNote: (refTickId: string, text: string) => void;
  onMoveTick: (tickId: string, afterTickId?: string) => void;
  onDeleteTick: (tickId: string) => void;
}) {
  if (!activeBranch) return (
    <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
      Select a branch to view ticks
    </div>
  );

  const visible = showNotes ? ticks : ticks.filter((t) => t.kind === "atom");

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      {/* Toolbar */}
      <div style={{
        flexShrink: 0, padding: "5px 16px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8,
      }}>
        <button
          onClick={onToggleNotes}
          title={showNotes ? "Hide notes" : "Show notes"}
          style={{
            display: "flex", alignItems: "center", gap: 4, fontSize: 10,
            padding: "2px 7px", borderRadius: 4, cursor: "pointer",
            background: showNotes ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
            border: showNotes ? "1px solid oklch(0.78 0.10 65 / 0.3)" : "1px solid var(--border-subtle)",
            color: showNotes ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          <StickyNote style={{ width: 10, height: 10 }} />
          Notes
        </button>
        <span style={{ fontSize: 10, color: "var(--text-ghost)", marginLeft: "auto" }}>
          {visible.length} tick{visible.length !== 1 ? "s" : ""}
        </span>
      </div>

      {visible.length === 0 ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          No ticks yet
        </div>
      ) : (
        <div style={{ flex: 1, overflow: "auto" }}>
          <div style={{ maxWidth: 1100, margin: "0 auto", padding: "16px 32px 48px" }}>
            {visible.map((tick, i) => {
              const isFirst = i === 0;
              const isLast  = i === visible.length - 1;
              const prevTick = i > 0 ? visible[i - 1] : undefined;
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
                  onMoveDown={() => !isLast && onMoveTick(tick.tickId, i + 2 < visible.length ? visible[i + 2].tickId : undefined)}
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
  tick: BranchTick;
  allTicks: BranchTick[];
  isFirst: boolean;
  isLast: boolean;
  prevTick: BranchTick | undefined;
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
  const refsOf = (t: BranchTick): string[] =>
    t.kind === "note" ? [t.ref] : t.kind === "atom" ? t.refs : [];

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
          {tick.text}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          → {tick.ref.slice(0, 12)}
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
          {tick.text}
        </span>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", flexShrink: 0, transition: "color 0.15s" }}>
          {tick.file}
        </span>
      </div>
    );
    return (
      <div style={{ display: "flex", alignItems: "baseline", gap: 10, flex: 1, minWidth: 0, fontFamily: "monospace" }}>
        <span style={{ fontSize: 9, color: hovered ? "var(--text-dim)" : "var(--text-ghost)", flexShrink: 0, userSelect: "all", transition: "color 0.15s" }}>
          {tick.tickId.slice(0, 12)}
        </span>
        <span style={{ fontSize: 13, color: "var(--text-secondary)", flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {tick.message}
        </span>
        {tick.file && (
          <span style={{ fontSize: 9, color: hovered ? "var(--text-ghost)" : "transparent", transition: "color 0.15s", flexShrink: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", minWidth: 0 }}>
            {tick.file}
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
    conns, error, branches, activeBranch, files, ticks, openFiles, showNotes,
    connect, createBranch, deleteBranch, selectBranch, openFile, closeFile,
    appendToFile, editAtom, deleteAtom, addNote, moveTick, deleteTickEntry, toggleNotes,
    chatPrompt,
  } = useStory();

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
  const atoms = fileConn?.atoms ?? [];
  const isAbsent = fileConn?.absent ?? false;

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
            selectedFile={selectedFile} atomCount={atoms.length}
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
            ) : atoms.length === 0 ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                Loading…
              </div>
            ) : (
              <AtomChain
                atoms={atoms}
                onEdit={(tickId, content) => selectedFile && editAtom(selectedFile, tickId, content)}
                onDelete={(tickId) => selectedFile && deleteAtom(selectedFile, tickId)}
              />
            )}

            <InputBar
              enabled={selectedFile !== null}
              onAppend={(text) => selectedFile && appendToFile(selectedFile, text)}
              onWrite={(text)  => selectedFile && chatPrompt(selectedFile, text)}
            />
          </>}

          {centerTab === "ticks" && (
            <TicksView
              activeBranch={activeBranch}
              ticks={ticks}
              showNotes={showNotes}
              onToggleNotes={toggleNotes}
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
