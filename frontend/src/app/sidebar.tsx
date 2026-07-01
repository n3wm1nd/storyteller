"use client";

import { useState } from "react";
import { Folder, FolderOpen, FileText, GitBranch, ChevronRight, Plus } from "lucide-react";
import { type ConnInfo } from "@/lib/store";
import { statusColor } from "@/lib/utils";

// ── File tree ─────────────────────────────────────────────────────────────────

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

// ── Sidebar ───────────────────────────────────────────────────────────────────

export function LeftSidebar({
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

      {tab === "explorer" && (
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
                background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.3)",
                borderRadius: 5, color: "var(--amber)", cursor: "pointer", flexShrink: 0,
              }}
            >
              <Plus style={{ width: 11, height: 11 }} />
            </button>
          </div>
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
