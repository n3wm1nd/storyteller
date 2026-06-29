"use client";

import { useEffect, useState } from "react";
import { useStory, type ConnInfo } from "@/lib/store";

export default function Home() {
  const {
    conns, error, branches, activeBranch, files, openFiles,
    connect, createBranch, deleteBranch, selectBranch, openFile, closeFile, appendToFile,
  } = useStory();

  const [newBranch, setNewBranch] = useState("");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);
  const [appendContent, setAppendContent] = useState("");
  const [newFilePath, setNewFilePath] = useState("");
  const [showNewFile, setShowNewFile] = useState(false);

  const sessionStatus = conns.find((c) => c.label === "session")?.status ?? "disconnected";

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => { connect(); }, []);

  const selectedFileConn = selectedFile ? openFiles[selectedFile] : null;
  const fileContent = selectedFileConn?.content ?? null;
  const hasOpenFile = selectedFile !== null && openFiles[selectedFile] !== undefined;

  function handleSelectFile(path: string) {
    if (selectedFile && selectedFile !== path) {
      closeFile(selectedFile);
    }
    setSelectedFile(path);
    openFile(path);
  }

  function handleDeselectFile() {
    if (selectedFile) closeFile(selectedFile);
    setSelectedFile(null);
  }

  function handleCreateFile() {
    const path = newFilePath.trim().replace(/^\/+/, "");
    if (!path) return;
    setShowNewFile(false);
    setNewFilePath("");
    handleSelectFile(path);
  }

  return (
    <div style={{ display: "flex", height: "100vh", overflow: "hidden" }}>
      {/* Sidebar */}
      <aside style={{
        width: 260, minWidth: 260,
        background: "var(--sidebar)",
        borderRight: "1px solid var(--border-subtle)",
        display: "flex", flexDirection: "column", overflow: "hidden",
      }}>
        <div style={{
          padding: "10px 16px",
          borderBottom: "1px solid var(--border-subtle)",
          background: "var(--topbar)",
          display: "flex", alignItems: "center", gap: 8,
        }}>
          <div style={{ width: 8, height: 8, borderRadius: "50%", flexShrink: 0, background: statusColor(sessionStatus) }} />
          <span style={{ fontWeight: 600, fontSize: 11, letterSpacing: "0.08em", textTransform: "uppercase", color: "var(--foreground)" }}>
            Storyteller
          </span>
        </div>

        <div style={{ flex: 1, overflow: "auto", padding: "8px 0" }}>
          <div style={{ padding: "4px 16px 8px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)" }}>
            Branches
          </div>
          {branches.length === 0 && sessionStatus === "connected" && (
            <div style={{ padding: "4px 16px", fontSize: 11, color: "var(--text-ghost)" }}>No branches yet</div>
          )}
          {branches.map((b) => (
            <BranchItem key={b} name={b} active={b === activeBranch}
              onSelect={() => { handleDeselectFile(); selectBranch(b); }}
              onDelete={() => deleteBranch(b)} />
          ))}
        </div>

        <div style={{ padding: 12, borderTop: "1px solid var(--border-subtle)", display: "flex", gap: 6 }}>
          <input value={newBranch} onChange={(e) => setNewBranch(e.target.value)}
            onKeyDown={(e) => { if (e.key === "Enter" && newBranch.trim()) { createBranch(newBranch.trim()); setNewBranch(""); } }}
            placeholder="New branch…" style={inputStyle} />
          <button onClick={() => { if (newBranch.trim()) { createBranch(newBranch.trim()); setNewBranch(""); } }}
            style={buttonStyle("var(--amber)")}>+</button>
        </div>

        <ConnStatusBar conns={conns} error={error} />
      </aside>

      {/* Main */}
      <main style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
        {activeBranch ? (
          <>
            {/* Branch header */}
            <div style={{
              padding: "0 16px",
              borderBottom: "1px solid var(--border-subtle)",
              background: "var(--surface-deep)",
              display: "flex", alignItems: "center", gap: 12, height: 40,
            }}>
              <span style={{ fontSize: 11, color: "var(--text-muted)" }}>branch /</span>
              <span style={{ fontSize: 13, fontWeight: 600, color: "var(--amber)" }}>{activeBranch}</span>
              <div style={{ marginLeft: "auto", display: "flex", alignItems: "center", gap: 8 }}>
                <span style={{ fontSize: 10, color: "var(--text-dim)" }}>{files.length} file{files.length !== 1 ? "s" : ""}</span>
                <button onClick={() => setShowNewFile((v) => !v)} style={{
                  background: showNewFile ? "oklch(0.78 0.10 65 / 0.15)" : "transparent",
                  border: "1px solid " + (showNewFile ? "var(--amber)" : "var(--border)"),
                  borderRadius: 5, color: showNewFile ? "var(--amber)" : "var(--text-label)",
                  cursor: "pointer", fontSize: 10, padding: "3px 8px",
                }}>+ new file</button>
              </div>
            </div>

            {/* New file bar */}
            {showNewFile && (
              <div style={{
                padding: "8px 16px", borderBottom: "1px solid var(--border-subtle)",
                background: "var(--card)", display: "flex", gap: 8, alignItems: "center",
              }}>
                <input value={newFilePath} onChange={(e) => setNewFilePath(e.target.value)}
                  onKeyDown={(e) => { if (e.key === "Enter") handleCreateFile(); if (e.key === "Escape") setShowNewFile(false); }}
                  placeholder="path/to/file.md" autoFocus style={{ ...inputStyle, flex: 1 }} />
                <button onClick={handleCreateFile} style={buttonStyle("var(--amber)")}>Open</button>
                <button onClick={() => setShowNewFile(false)} style={{ ...buttonStyle("var(--muted)"), color: "var(--text-label)" }}>✕</button>
              </div>
            )}

            <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
              {/* File list */}
              <div style={{ width: 220, borderRight: "1px solid var(--border-subtle)", overflow: "auto", padding: "8px 0" }}>
                <div style={{ padding: "4px 12px 8px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)" }}>
                  Files
                </div>
                {files.length === 0 && (
                  <div style={{ padding: "4px 12px", fontSize: 11, color: "var(--text-ghost)" }}>Empty branch</div>
                )}
                {files.map((p) => (
                  <button key={p} onClick={() => handleSelectFile(p)} style={{
                    display: "block", width: "100%", textAlign: "left",
                    padding: "5px 12px", fontSize: 11,
                    background: selectedFile === p ? "oklch(0.78 0.10 65 / 0.12)" : "transparent",
                    color: selectedFile === p ? "var(--amber)" : "var(--text-label)",
                    border: "none", cursor: "pointer",
                    borderLeft: selectedFile === p ? "2px solid var(--amber)" : "2px solid transparent",
                  }}>{p}</button>
                ))}
              </div>

              {/* File content */}
              <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
                {selectedFile ? (
                  <>
                    <div style={{
                      padding: "0 16px", borderBottom: "1px solid var(--border-subtle)",
                      background: "var(--surface-deep)", display: "flex", alignItems: "center", gap: 8, height: 34,
                    }}>
                      <span style={{ fontSize: 11, color: "var(--text-muted)", flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{selectedFile}</span>
                      <button onClick={handleDeselectFile} style={{ background: "none", border: "none", cursor: "pointer", color: "var(--text-dim)", fontSize: 14, padding: "0 2px" }}>✕</button>
                    </div>
                    {fileContent !== null ? (
                      <pre style={{
                        flex: 1, overflow: "auto", margin: 0, padding: 16,
                        fontSize: 12, lineHeight: 1.7, fontFamily: "Georgia, serif",
                        color: "var(--text-body)", whiteSpace: "pre-wrap", wordBreak: "break-word",
                      }}>{fileContent}</pre>
                    ) : (
                      <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                        File does not exist yet — append to create it
                      </div>
                    )}
                  </>
                ) : (
                  <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                    Select a file or create a new one
                  </div>
                )}
              </div>
            </div>

            {/* Append bar — only active when a file is open */}
            <div style={{
              padding: "10px 16px", borderTop: "1px solid var(--border-subtle)",
              background: "var(--surface-deep)", display: "flex", gap: 8, alignItems: "flex-start",
              opacity: hasOpenFile ? 1 : 0.4, pointerEvents: hasOpenFile ? "auto" : "none",
            }}>
              <textarea value={appendContent} onChange={(e) => setAppendContent(e.target.value)}
                placeholder={hasOpenFile ? `Append to ${selectedFile}…` : "Open a file to append"}
                rows={2} style={{ ...inputStyle, flex: 1, resize: "vertical", fontFamily: "Georgia, serif" }} />
              <button onClick={() => {
                if (selectedFile && appendContent.trim()) {
                  appendToFile(selectedFile, appendContent.trim());
                  setAppendContent("");
                }
              }} style={buttonStyle("var(--amber)")}>Append</button>
            </div>
          </>
        ) : (
          <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 13 }}>
            {sessionStatus === "connecting" ? "Connecting…"
             : sessionStatus === "error"    ? "Failed to connect"
             : sessionStatus === "connected"? "Select or create a branch"
             : "Disconnected"}
          </div>
        )}
      </main>
    </div>
  );
}

function BranchItem({ name, active, onSelect, onDelete }: { name: string; active: boolean; onSelect: () => void; onDelete: () => void }) {
  const [hover, setHover] = useState(false);
  return (
    <div onMouseEnter={() => setHover(true)} onMouseLeave={() => setHover(false)} style={{
      display: "flex", alignItems: "center", padding: "5px 8px 5px 16px",
      background: active ? "oklch(0.78 0.10 65 / 0.10)" : hover ? "oklch(0.18 0.012 60)" : "transparent",
      borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent", cursor: "pointer",
    }}>
      <span onClick={onSelect} style={{ flex: 1, fontSize: 12, color: active ? "var(--amber)" : "var(--text-label)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
        {name}
      </span>
      {hover && (
        <button onClick={(e) => { e.stopPropagation(); onDelete(); }}
          style={{ background: "none", border: "none", cursor: "pointer", padding: "0 4px", color: "var(--rose)", fontSize: 14, lineHeight: 1 }}>×</button>
      )}
    </div>
  );
}

function ConnStatusBar({ conns, error }: { conns: ConnInfo[]; error: string | null }) {
  return (
    <div style={{ borderTop: "1px solid var(--border-subtle)", padding: "8px 12px", display: "flex", flexDirection: "column", gap: 4 }}>
      {conns.map((c) => (
        <div key={c.label} style={{ display: "flex", alignItems: "center", gap: 6 }}>
          <div style={{ width: 6, height: 6, borderRadius: "50%", flexShrink: 0, background: statusColor(c.status) }} />
          <span style={{ fontSize: 10, color: "var(--text-dim)", fontFamily: "monospace" }}>{c.label}</span>
          <span style={{ marginLeft: "auto", fontSize: 10, color: statusColor(c.status) }}>{c.status}</span>
        </div>
      ))}
      {error && <div style={{ fontSize: 10, color: "var(--rose)", marginTop: 2, lineHeight: 1.4 }}>{error}</div>}
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

const inputStyle: React.CSSProperties = {
  background: "var(--card)", border: "1px solid var(--border)", borderRadius: 6,
  color: "var(--foreground)", fontSize: 11, padding: "5px 8px", outline: "none", width: "100%",
};

function buttonStyle(accent: string): React.CSSProperties {
  return {
    background: accent, border: "none", borderRadius: 6, color: "oklch(0.15 0.01 60)",
    cursor: "pointer", fontWeight: 600, fontSize: 11, padding: "5px 10px", whiteSpace: "nowrap", flexShrink: 0,
  };
}
