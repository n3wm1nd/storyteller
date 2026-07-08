"use client";

import { useState } from "react";
import { Folder, FolderOpen, FileText, FileWarning, ChevronRight, Plus, Trash2, Pencil } from "lucide-react";
import { branchFileUrl } from "@/lib/ws";

// ── Tree building ─────────────────────────────────────────────────────────────

export interface TreeNode {
  name: string;
  path: string;
  isDir: boolean;
  // No atom history at all (see ws.ts's LibraryNode.binary, which this is
  // sourced from — this flat file listing has no per-file metadata of its
  // own). Never open the prose/atom viewer for one of these.
  isBinary: boolean;
  // Whether a context-source filter currently includes this file (see
  // context-source.tsx) — always true for callers that don't pass
  // 'includedPaths' (the ordinary Explorer/Library trees), which have no
  // such concept and just ignore it.
  included: boolean;
  children: TreeNode[];
}

export function buildTree(paths: string[], binaryPaths: Set<string> = new Set(), includedPaths?: Set<string>): TreeNode[] {
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
        node = {
          name: displayName, path: builtPath, isDir: !isLast,
          isBinary: isLast && binaryPaths.has(builtPath),
          included: !includedPaths || !isLast || includedPaths.has(builtPath),
          children: [],
        };
        nodes.push(node);
      }
      nodes = node.children;
    }
  }
  return root;
}

// Hands the caller (path, content) pairs resolved against the drop target
// folder — 'content' is each dropped 'File' itself (a 'Blob'), uploaded as
// raw bytes over HTTP PUT (see 'uploadBranchFile'/sidebar.actions.ts's 'uploadFiles'),
// so unlike the old WS-JSON upload this isn't limited to text/markdown.
function resolveDroppedFiles(folderPath: string, fileList: FileList): { path: string; content: File }[] {
  return Array.from(fileList).map((file) => ({
    path: folderPath ? `${folderPath}/${file.name}` : file.name,
    content: file,
  }));
}

// Custom drag MIME type marking "this drag is an internal tree node being
// moved," not an OS file drop — a folder's own onDrop checks for this
// first so an in-tree drag never gets misread as an upload.
const MOVE_MIME = "application/x-storyteller-move";

function targetPathFor(folderPath: string, sourcePath: string): string {
  const name = sourcePath.split("/").pop() ?? sourcePath;
  return folderPath ? `${folderPath}/${name}` : name;
}

function FileTreeNode({
  node, depth, selectedFile, onSelectFile, onDropFiles, onMoveFile, activeBranch,
  renamingPath, onStartRename, onCommitRename, onCancelRename,
}: {
  node: TreeNode; depth: number; selectedFile: string | null; onSelectFile: (p: string) => void;
  onDropFiles: (folderPath: string, files: FileList) => void;
  onMoveFile: (sourcePath: string, newPath: string) => void;
  activeBranch: string | null;
  renamingPath: string | null;
  onStartRename: (path: string) => void;
  onCommitRename: (path: string, newName: string) => void;
  onCancelRename: () => void;
}) {
  const [open, setOpen] = useState(true);
  const [dragOver, setDragOver] = useState(false);
  const active = selectedFile === node.path;
  const pad = 8 + depth * 14;

  function handleDrop(e: React.DragEvent) {
    e.preventDefault();
    e.stopPropagation();
    setDragOver(false);
    const movedFrom = e.dataTransfer.getData(MOVE_MIME);
    if (movedFrom) {
      const dest = targetPathFor(node.path, movedFrom);
      if (dest !== movedFrom) onMoveFile(movedFrom, dest);
    } else {
      onDropFiles(node.path, e.dataTransfer.files);
    }
  }

  if (node.isDir) {
    return (
      <div>
        <button
          onClick={() => setOpen((v) => !v)}
          onDragOver={(e) => { e.preventDefault(); e.stopPropagation(); setDragOver(true); }}
          onDragLeave={() => setDragOver(false)}
          onDrop={handleDrop}
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
          <FileTreeNode
            key={child.path} node={child} depth={depth + 1} selectedFile={selectedFile}
            onSelectFile={onSelectFile} onDropFiles={onDropFiles} onMoveFile={onMoveFile} activeBranch={activeBranch}
            renamingPath={renamingPath} onStartRename={onStartRename} onCommitRename={onCommitRename} onCancelRename={onCancelRename}
          />
        ))}
      </div>
    );
  }

  if (renamingPath === node.path) {
    return (
      <RenameInput
        pad={pad} initialName={node.name}
        onCommit={(name) => onCommitRename(node.path, name)}
        onCancel={onCancelRename}
      />
    );
  }

  // Binary (no atom history): no tick chain for the prose/atom viewer to
  // show, and writing to it would just glue text onto whatever binary
  // content is actually there — open the raw bytes in a new tab instead
  // of calling onSelectFile (same endpoint uploadFiles PUTs to).
  if (node.isBinary) {
    return (
      <button
        draggable
        onDragStart={(e) => e.dataTransfer.setData(MOVE_MIME, node.path)}
        onDoubleClick={() => onStartRename(node.path)}
        onClick={() => activeBranch && window.open(branchFileUrl(activeBranch, node.path), "_blank")}
        title="Binary file — opens raw, not editable here (double-click to rename)"
        style={{
          display: "flex", alignItems: "center", gap: 5, width: "100%", textAlign: "left",
          padding: `3px 8px 3px ${pad}px`,
          background: "transparent", color: "var(--text-ghost)",
          border: "none", borderLeft: "2px solid transparent",
          cursor: "pointer", borderRadius: 5, fontSize: 12, fontWeight: 400,
        }}>
        <FileWarning style={{ width: 11, height: 11, flexShrink: 0, opacity: 0.6 }} />
        <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{node.name}</span>
      </button>
    );
  }

  return (
    <button
      draggable
      onDragStart={(e) => e.dataTransfer.setData(MOVE_MIME, node.path)}
      onDoubleClick={() => onStartRename(node.path)}
      onClick={() => onSelectFile(node.path)}
      title={`${node.path} (double-click to rename)`}
      style={{
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

// Inline rename editor, swapped in for a leaf node's own label — edits just
// the leaf name (moving to a different folder is drag-and-drop's job, not
// this input's), Enter commits, Escape/blur-with-no-change cancels.
function RenameInput({ pad, initialName, onCommit, onCancel }: {
  pad: number; initialName: string;
  onCommit: (name: string) => void;
  onCancel: () => void;
}) {
  const [value, setValue] = useState(initialName);
  return (
    <div style={{ display: "flex", alignItems: "center", padding: `3px 8px 3px ${pad}px` }}>
      <FileText style={{ width: 11, height: 11, flexShrink: 0, opacity: 0.6, marginRight: 5 }} />
      <input
        autoFocus
        value={value}
        onChange={(e) => setValue(e.target.value)}
        onFocus={(e) => e.target.select()}
        onKeyDown={(e) => {
          if (e.key === "Enter") {
            const trimmed = value.trim();
            if (trimmed && trimmed !== initialName) onCommit(trimmed); else onCancel();
          } else if (e.key === "Escape") {
            onCancel();
          }
        }}
        onBlur={() => {
          const trimmed = value.trim();
          if (trimmed && trimmed !== initialName) onCommit(trimmed); else onCancel();
        }}
        style={{
          flex: 1, fontSize: 12, padding: "1px 4px",
          background: "var(--card)", border: "1px solid var(--amber)",
          borderRadius: 3, color: "var(--foreground)", outline: "none",
        }}
      />
    </div>
  );
}

// ── File tree (Explorer tab content) ─────────────────────────────────────────
//
// Home for anything that acts on the branch's file tree as a whole —
// browsing, drag-and-drop upload, delete, rename/move (double-click to
// rename in place; drag a node onto a folder to move it — both are just
// Server.Writer.File.Protocol's "rename" command, see page.tsx's
// handleRenameFile), new-file creation. Kept separate from LeftSidebar's
// Branches/Characters tabs (sidebar.tsx), which are plain list renderers
// over the branch list with no tree structure or file-level operations of
// their own.

export function FileTree({
  activeBranch, files, binaryPaths, selectedFile, onSelectFile, onCreateFile, onDeleteFile, onRenameFile, onUploadFiles,
}: {
  activeBranch: string | null;
  files: string[];
  // Paths with no atom history at all — sourced from the library tree's
  // own per-node 'binary' flag (see ws.ts's LibraryNode), since this flat
  // file listing carries no per-file metadata of its own.
  binaryPaths?: Set<string>;
  selectedFile: string | null;
  onSelectFile: (f: string) => void;
  onCreateFile: (path: string) => void;
  onDeleteFile: (path: string) => void;
  onRenameFile: (path: string, newPath: string) => void;
  onUploadFiles: (files: { path: string; content: File }[]) => void;
}) {
  const [rootDragOver, setRootDragOver] = useState(false);
  const [newFilePath, setNewFilePath] = useState("");
  const [renamingPath, setRenamingPath] = useState<string | null>(null);

  function handleDropFiles(folderPath: string, fileList: FileList) {
    onUploadFiles(resolveDroppedFiles(folderPath, fileList));
  }

  function handleRootDrop(e: React.DragEvent) {
    e.preventDefault();
    setRootDragOver(false);
    const movedFrom = e.dataTransfer.getData(MOVE_MIME);
    if (movedFrom) {
      const dest = targetPathFor("", movedFrom);
      if (dest !== movedFrom) onRenameFile(movedFrom, dest);
    } else {
      handleDropFiles("", e.dataTransfer.files);
    }
  }

  function commitRename(path: string, newName: string) {
    setRenamingPath(null);
    const parts = path.split("/");
    parts[parts.length - 1] = newName;
    onRenameFile(path, parts.join("/"));
  }

  // No dedicated folder-creation affordance yet — a path typed with "/"s
  // just nests under those segments via 'buildTree', same as a dropped
  // upload's destination. Creation is now its own explicit tick
  // (file.create — see WS-PROTOCOL.md) rather than the path just sitting
  // "absent" until whatever's typed into it first lands.
  function handleCreateFile() {
    const path = newFilePath.trim().replace(/^\/+/, "");
    if (!path) return;
    setNewFilePath("");
    onCreateFile(path);
  }

  return (
    <div
      onDragOver={(e) => { if (activeBranch) { e.preventDefault(); setRootDragOver(true); } }}
      onDragLeave={() => setRootDragOver(false)}
      onDrop={(e) => { if (activeBranch) handleRootDrop(e); }}
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
        {activeBranch && selectedFile && (
          <button
            onClick={() => setRenamingPath(selectedFile)}
            title={`Rename ${decodeURIComponent(selectedFile)}`}
            style={{
              width: 18, height: 18, display: "flex", alignItems: "center", justifyContent: "center",
              background: "transparent", border: "none", borderRadius: 4,
              color: "var(--text-dim)", cursor: "pointer", flexShrink: 0, padding: 0,
            }}
          >
            <Pencil style={{ width: 11, height: 11 }} />
          </button>
        )}
        {activeBranch && selectedFile && (
          <button
            onClick={() => onDeleteFile(selectedFile)}
            title={`Delete ${decodeURIComponent(selectedFile)}`}
            style={{
              width: 18, height: 18, display: "flex", alignItems: "center", justifyContent: "center",
              background: "transparent", border: "none", borderRadius: 4,
              color: "var(--text-dim)", cursor: "pointer", flexShrink: 0, padding: 0,
            }}
          >
            <Trash2 style={{ width: 11, height: 11 }} />
          </button>
        )}
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
        buildTree(files, binaryPaths).map((node) => (
          <FileTreeNode
            key={node.path} node={node} depth={0} selectedFile={selectedFile}
            onSelectFile={onSelectFile} onDropFiles={handleDropFiles} onMoveFile={onRenameFile} activeBranch={activeBranch}
            renamingPath={renamingPath} onStartRename={setRenamingPath} onCommitRename={commitRename} onCancelRename={() => setRenamingPath(null)}
          />
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
