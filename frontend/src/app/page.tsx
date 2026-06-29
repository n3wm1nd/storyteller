"use client";

import { useEffect, useState } from "react";
import { useStory, type ConnInfo } from "@/lib/store";

export default function Home() {
  const {
    conns, error, branches, activeBranch, files,
    connect, createBranch, deleteBranch, selectBranch, appendToFile,
  } = useStory();

  const [newBranch, setNewBranch] = useState("");
  const [appendPath, setAppendPath] = useState("");
  const [appendContent, setAppendContent] = useState("");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);

  const sessionConn = conns.find((c) => c.label === "session");
  const overallStatus = sessionConn?.status ?? "disconnected";

  // connect is stable (zustand store method) — safe to call once on mount
  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => { connect(); }, []);

  const filePaths = Object.keys(files).sort();
  const fileContent = selectedFile ? files[selectedFile] : null;

  return (
    <div style={{ display: "flex", height: "100vh", overflow: "hidden" }}>
      {/* Sidebar */}
      <aside style={{
        width: 260,
        minWidth: 260,
        background: "var(--sidebar)",
        borderRight: "1px solid var(--border-subtle)",
        display: "flex",
        flexDirection: "column",
        overflow: "hidden",
      }}>
        {/* Header */}
        <div style={{
          padding: "10px 16px",
          borderBottom: "1px solid var(--border-subtle)",
          background: "var(--topbar)",
          display: "flex",
          alignItems: "center",
          gap: 8,
        }}>
          <div style={{
            width: 8, height: 8, borderRadius: "50%", flexShrink: 0,
            background: statusColor(overallStatus),
          }} />
          <span style={{ fontWeight: 600, fontSize: 11, letterSpacing: "0.08em", textTransform: "uppercase", color: "var(--foreground)" }}>
            Storyteller
          </span>
        </div>

        {/* Branch list */}
        <div style={{ flex: 1, overflow: "auto", padding: "8px 0" }}>
          <div style={{ padding: "4px 16px 8px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)" }}>
            Branches
          </div>
          {branches.length === 0 && overallStatus === "connected" && (
            <div style={{ padding: "4px 16px", fontSize: 11, color: "var(--text-ghost)" }}>
              No branches yet
            </div>
          )}
          {branches.map((b) => (
            <BranchItem
              key={b}
              name={b}
              active={b === activeBranch}
              onSelect={() => selectBranch(b)}
              onDelete={() => deleteBranch(b)}
            />
          ))}
        </div>

        {/* Create branch */}
        <div style={{
          padding: 12,
          borderTop: "1px solid var(--border-subtle)",
          display: "flex",
          gap: 6,
        }}>
          <input
            value={newBranch}
            onChange={(e) => setNewBranch(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter" && newBranch.trim()) {
                createBranch(newBranch.trim());
                setNewBranch("");
              }
            }}
            placeholder="New branch…"
            style={inputStyle}
          />
          <button
            onClick={() => { if (newBranch.trim()) { createBranch(newBranch.trim()); setNewBranch(""); } }}
            style={buttonStyle("var(--amber)")}
          >
            +
          </button>
        </div>

        {/* Connection status footer */}
        <ConnStatusBar conns={conns} error={error} />
      </aside>

      {/* Main content */}
      <main style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
        {activeBranch ? (
          <>
            {/* Branch header */}
            <div style={{
              padding: "10px 20px",
              borderBottom: "1px solid var(--border-subtle)",
              background: "var(--surface-deep)",
              display: "flex",
              alignItems: "center",
              gap: 12,
            }}>
              <span style={{ fontSize: 11, color: "var(--text-muted)" }}>branch /</span>
              <span style={{ fontSize: 13, fontWeight: 600, color: "var(--amber)" }}>{activeBranch}</span>
              <span style={{ marginLeft: "auto", fontSize: 10, color: "var(--text-dim)" }}>
                {filePaths.length} file{filePaths.length !== 1 ? "s" : ""}
              </span>
            </div>

            <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
              {/* File list */}
              <div style={{
                width: 220,
                borderRight: "1px solid var(--border-subtle)",
                overflow: "auto",
                padding: "8px 0",
              }}>
                <div style={{ padding: "4px 12px 8px", fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)" }}>
                  Files
                </div>
                {filePaths.length === 0 && (
                  <div style={{ padding: "4px 12px", fontSize: 11, color: "var(--text-ghost)" }}>
                    Empty branch
                  </div>
                )}
                {filePaths.map((p) => (
                  <button
                    key={p}
                    onClick={() => setSelectedFile(p)}
                    style={{
                      display: "block",
                      width: "100%",
                      textAlign: "left",
                      padding: "5px 12px",
                      fontSize: 11,
                      background: selectedFile === p ? "oklch(0.78 0.10 65 / 0.12)" : "transparent",
                      color: selectedFile === p ? "var(--amber)" : "var(--text-label)",
                      border: "none",
                      cursor: "pointer",
                      borderLeft: selectedFile === p ? "2px solid var(--amber)" : "2px solid transparent",
                    }}
                  >
                    {p}
                  </button>
                ))}
              </div>

              {/* File content */}
              <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
                {selectedFile ? (
                  <>
                    <div style={{
                      padding: "8px 16px",
                      borderBottom: "1px solid var(--border-subtle)",
                      fontSize: 11,
                      color: "var(--text-muted)",
                      background: "var(--surface-deep)",
                    }}>
                      {selectedFile}
                    </div>
                    <pre style={{
                      flex: 1,
                      overflow: "auto",
                      margin: 0,
                      padding: 16,
                      fontSize: 12,
                      lineHeight: 1.7,
                      fontFamily: "Georgia, serif",
                      color: "var(--text-body)",
                      whiteSpace: "pre-wrap",
                      wordBreak: "break-word",
                    }}>
                      {fileContent ?? ""}
                    </pre>
                  </>
                ) : (
                  <div style={{
                    flex: 1, display: "flex", alignItems: "center", justifyContent: "center",
                    color: "var(--text-ghost)", fontSize: 12,
                  }}>
                    Select a file to view its contents
                  </div>
                )}
              </div>
            </div>

            {/* Append bar */}
            <div style={{
              padding: "10px 16px",
              borderTop: "1px solid var(--border-subtle)",
              background: "var(--surface-deep)",
              display: "flex",
              gap: 8,
              alignItems: "flex-start",
            }}>
              <input
                value={appendPath}
                onChange={(e) => setAppendPath(e.target.value)}
                placeholder="path/to/file.md"
                style={{ ...inputStyle, width: 180, flexShrink: 0 }}
              />
              <textarea
                value={appendContent}
                onChange={(e) => setAppendContent(e.target.value)}
                placeholder="Content to append…"
                rows={2}
                style={{ ...inputStyle, flex: 1, resize: "vertical", fontFamily: "Georgia, serif" }}
              />
              <button
                onClick={() => {
                  if (appendPath.trim() && appendContent.trim()) {
                    appendToFile(appendPath.trim(), appendContent.trim());
                    setSelectedFile(appendPath.trim());
                    setAppendContent("");
                  }
                }}
                style={buttonStyle("var(--amber)")}
              >
                Append
              </button>
            </div>
          </>
        ) : (
          <div style={{
            flex: 1, display: "flex", flexDirection: "column",
            alignItems: "center", justifyContent: "center", gap: 8,
            color: "var(--text-ghost)",
          }}>
            <span style={{ fontSize: 13 }}>
              {overallStatus === "connecting" ? "Connecting to server…"
               : overallStatus === "error"    ? "Failed to connect"
               : overallStatus === "connected"? "Select or create a branch"
               : "Disconnected"}
            </span>
          </div>
        )}
      </main>
    </div>
  );
}

// ── Sub-components ────────────────────────────────────────────────────────────

function ConnStatusBar({ conns, error }: { conns: ConnInfo[]; error: string | null }) {
  return (
    <div style={{
      borderTop: "1px solid var(--border-subtle)",
      padding: "8px 12px",
      display: "flex",
      flexDirection: "column",
      gap: 4,
    }}>
      {conns.map((c) => (
        <div key={c.label} style={{ display: "flex", alignItems: "center", gap: 6 }}>
          <div style={{
            width: 6, height: 6, borderRadius: "50%", flexShrink: 0,
            background: statusColor(c.status),
          }} />
          <span style={{ fontSize: 10, color: "var(--text-dim)", fontFamily: "monospace" }}>
            {c.label}
          </span>
          <span style={{ marginLeft: "auto", fontSize: 10, color: statusColor(c.status) }}>
            {c.status}
          </span>
        </div>
      ))}
      {error && (
        <div style={{ fontSize: 10, color: "var(--rose)", marginTop: 2, lineHeight: 1.4 }}>
          {error}
        </div>
      )}
    </div>
  );
}

function BranchItem({ name, active, onSelect, onDelete }: {
  name: string; active: boolean;
  onSelect: () => void; onDelete: () => void;
}) {
  const [hover, setHover] = useState(false);
  return (
    <div
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      style={{
        display: "flex",
        alignItems: "center",
        padding: "5px 8px 5px 16px",
        background: active ? "oklch(0.78 0.10 65 / 0.10)" : hover ? "oklch(0.18 0.012 60)" : "transparent",
        borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent",
        cursor: "pointer",
      }}
    >
      <span
        onClick={onSelect}
        style={{ flex: 1, fontSize: 12, color: active ? "var(--amber)" : "var(--text-label)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}
      >
        {name}
      </span>
      {hover && (
        <button
          onClick={(e) => { e.stopPropagation(); onDelete(); }}
          style={{ background: "none", border: "none", cursor: "pointer", padding: "0 4px", color: "var(--rose)", fontSize: 14, lineHeight: 1 }}
          title="Delete branch"
        >
          ×
        </button>
      )}
    </div>
  );
}

// ── Styles ────────────────────────────────────────────────────────────────────

function statusColor(status: string): string {
  switch (status) {
    case "connected":    return "var(--emerald)";
    case "connecting":   return "var(--amber)";
    case "error":        return "var(--rose)";
    default:             return "var(--text-dim)";
  }
}

const inputStyle: React.CSSProperties = {
  background: "var(--card)",
  border: "1px solid var(--border)",
  borderRadius: 6,
  color: "var(--foreground)",
  fontSize: 11,
  padding: "5px 8px",
  outline: "none",
  width: "100%",
};

function buttonStyle(accent: string): React.CSSProperties {
  return {
    background: accent,
    border: "none",
    borderRadius: 6,
    color: "oklch(0.15 0.01 60)",
    cursor: "pointer",
    fontWeight: 600,
    fontSize: 11,
    padding: "5px 10px",
    whiteSpace: "nowrap",
    flexShrink: 0,
  };
}
