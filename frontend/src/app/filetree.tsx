"use client";

import { useState } from "react";
import { Folder, FolderOpen, FileText, ChevronRight, Plus } from "lucide-react";

// ── Tree building ─────────────────────────────────────────────────────────────

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

// Reads every dropped file as text and hands the caller (path, content)
// pairs resolved against the drop target folder — one upload command for
// the whole batch, per the "prefer multi-file over single-file" decision in
// TODO.md. Files are markdown/text by convention (see WRITER.md); no binary
// upload support.
async function resolveDroppedFiles(folderPath: string, fileList: FileList): Promise<{ path: string; content: string }[]> {
  const files = Array.from(fileList);
  return Promise.all(files.map(async (file) => ({
    path: folderPath ? `${folderPath}/${file.name}` : file.name,
    content: await file.text(),
  })));
}

function FileTreeNode({ node, depth, selectedFile, onSelectFile, onDropFiles }: {
  node: TreeNode; depth: number; selectedFile: string | null; onSelectFile: (p: string) => void;
  onDropFiles: (folderPath: string, files: FileList) => void;
}) {
  const [open, setOpen] = useState(true);
  const [dragOver, setDragOver] = useState(false);
  const active = selectedFile === node.path;
  const pad = 8 + depth * 14;

  if (node.isDir) {
    return (
      <div>
        <button
          onClick={() => setOpen((v) => !v)}
          onDragOver={(e) => { e.preventDefault(); e.stopPropagation(); setDragOver(true); }}
          onDragLeave={() => setDragOver(false)}
          onDrop={(e) => {
            e.preventDefault();
            e.stopPropagation();
            setDragOver(false);
            onDropFiles(node.path, e.dataTransfer.files);
          }}
          style={{
            display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
            padding: `3px 8px 3px ${pad}px`,
            background: dragOver ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
            border: "none", cursor: "pointer", borderRadius: 5,
            color: "var(--text-muted)", fontSize: 11,
          }}>
          <ChevronRight style={{ width: 11, height: 11, flexShrink: 0, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
          {open
            ? <FolderOpen style={{ width: 12, height: 12, flexShrink: 0 }} />
            : <Folder style={{ width: 12, height: 12, flexShrink: 0 }} />}
          <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
        </button>
        {open && node.children.map((child) => (
          <FileTreeNode key={child.path} node={child} depth={depth + 1} selectedFile={selectedFile} onSelectFile={onSelectFile} onDropFiles={onDropFiles} />
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

// ── File tree (Explorer tab content) ─────────────────────────────────────────
//
// Home for anything that acts on the branch's file tree as a whole —
// currently browsing + drag-and-drop upload, expected to grow rename,
// delete, new-file/new-folder creation, and moving (see TODO.md) as those
// land. Kept separate from LeftSidebar's Branches/Characters tabs (sidebar.tsx),
// which are plain list renderers over the branch list with no tree
// structure or file-level operations of their own.

export function FileTree({
  activeBranch, files, selectedFile, onSelectFile, onUploadFiles,
}: {
  activeBranch: string | null;
  files: string[];
  selectedFile: string | null;
  onSelectFile: (f: string) => void;
  onUploadFiles: (files: { path: string; content: string }[]) => void;
}) {
  const [rootDragOver, setRootDragOver] = useState(false);
  const [newFilePath, setNewFilePath] = useState("");

  function handleDropFiles(folderPath: string, fileList: FileList) {
    resolveDroppedFiles(folderPath, fileList).then(onUploadFiles);
  }

  // No dedicated folder-creation affordance yet — a path typed with "/"s
  // just nests under those segments via 'buildTree', same as a dropped
  // upload's destination. No rename yet either, so this is purely additive:
  // selecting a not-yet-existing path opens it "absent" (see WS-PROTOCOL.md)
  // and it's created for real on first write.
  function handleCreateFile() {
    const path = newFilePath.trim().replace(/^\/+/, "");
    if (!path) return;
    setNewFilePath("");
    onSelectFile(path);
  }

  return (
    <div
      onDragOver={(e) => { if (activeBranch) { e.preventDefault(); setRootDragOver(true); } }}
      onDragLeave={() => setRootDragOver(false)}
      onDrop={(e) => {
        if (!activeBranch) return;
        e.preventDefault();
        setRootDragOver(false);
        handleDropFiles("", e.dataTransfer.files);
      }}
      style={{
        flex: 1, overflow: "auto", padding: "4px 4px",
        background: rootDragOver ? "oklch(0.78 0.10 65 / 0.06)" : "transparent",
        outline: rootDragOver ? "1px dashed var(--amber)" : "none", outlineOffset: -2,
      }}
    >
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
          Empty branch — drop files here to upload
        </div>
      ) : (
        buildTree(files).map((node) => (
          <FileTreeNode key={node.path} node={node} depth={0} selectedFile={selectedFile} onSelectFile={onSelectFile} onDropFiles={handleDropFiles} />
        ))
      )}

      {activeBranch && (
        <div style={{ padding: "6px 8px 4px", display: "flex", gap: 4 }}>
          <input
            value={newFilePath}
            onChange={(e) => setNewFilePath(e.target.value)}
            onKeyDown={(e) => { if (e.key === "Enter") handleCreateFile(); }}
            placeholder="path/to/file.md"
            style={{
              flex: 1, fontSize: 11, padding: "3px 7px",
              background: "var(--card)", border: "1px solid var(--border-subtle)",
              borderRadius: 5, color: "var(--foreground)", outline: "none",
            }}
          />
          <button
            onClick={handleCreateFile}
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
