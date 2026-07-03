"use client";

import { useCallback, useEffect, useState } from "react";
import { PanelLeftClose, PanelLeftOpen, Eye, EyeOff, Trash2 } from "lucide-react";
import { useStory } from "@/lib/store";
import { tickChain, statusColor, type AnnotationMode } from "@/lib/utils";
import { LeftSidebar } from "./sidebar";
import { WireTickList, AgentLogStrip, ChatPreviewStrip, InputBar } from "./fileview";
import { TicksView } from "./ticksview";

// ── Top bar ───────────────────────────────────────────────────────────────────

function TopBar({ sessionStatus, branches, activeBranch }: {
  sessionStatus: string;
  branches: string[];
  activeBranch: string | null;
}) {
  return (
    <div style={{
      height: 32, flexShrink: 0,
      background: "var(--topbar)", borderBottom: "1px solid var(--border-subtle)",
      display: "flex", alignItems: "center", padding: "0 12px",
    }}>
      <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: "var(--amber)", boxShadow: "0 0 10px oklch(0.78 0.10 65 / 40%)" }} />
        <span style={{ fontSize: 11, fontWeight: 700, letterSpacing: "0.08em", color: "var(--text-heading)" }}>STORYTELLER</span>
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

// ── Toolbar ───────────────────────────────────────────────────────────────────

const iconBtnStyle: React.CSSProperties = {
  width: 26, height: 26, display: "flex", alignItems: "center", justifyContent: "center",
  background: "transparent", border: "none", cursor: "pointer",
  color: "var(--text-dim)", borderRadius: 5, flexShrink: 0,
};

function Toolbar({ leftOpen, onToggleLeft, selectedFile, onCloseFile, onNewFile, centerTab, onCenterTab }: {
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
      background: "var(--surface-deep)", borderBottom: "1px solid var(--border-subtle)",
      display: "flex", alignItems: "stretch", padding: "0 4px 0 6px",
    }}>
      <button onClick={onToggleLeft} style={{ ...iconBtnStyle, alignSelf: "center", marginRight: 2 }}>
        {leftOpen ? <PanelLeftClose style={{ width: 14, height: 14 }} /> : <PanelLeftOpen style={{ width: 14, height: 14 }} />}
      </button>
      <div style={{ width: 1, height: 16, background: "var(--border-subtle)", alignSelf: "center", margin: "0 6px" }} />

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
          <span style={{ fontSize: 10, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", color: centerTab === "file" ? "var(--text-muted)" : "var(--text-dim)", fontWeight: 400 }}>
            {decodeURIComponent(selectedFile.split("/").pop() ?? "")}
          </span>
          <span onClick={(e) => { e.stopPropagation(); onCloseFile(); }} style={{ fontSize: 13, lineHeight: 1, flexShrink: 0, opacity: 0.5, cursor: "pointer" }}>✕</span>
        </>}
      </button>

      <button onClick={() => onCenterTab("ticks")} style={{
        padding: "0 10px", fontSize: 11, fontWeight: 500,
        border: "none", borderBottom: centerTab === "ticks" ? "2px solid var(--amber)" : "2px solid transparent",
        borderTop: "2px solid transparent", background: "transparent",
        color: centerTab === "ticks" ? "var(--amber)" : "var(--text-disabled)",
        cursor: "pointer", transition: "color 0.15s, border-color 0.15s",
      }}>Ticks</button>

      <span style={{ flex: 1 }} />
      {!selectedFile && (
        <button onClick={onNewFile} style={{ alignSelf: "center", fontSize: 10, padding: "3px 8px", background: "transparent", border: "1px solid var(--border)", borderRadius: 5, color: "var(--text-label)", cursor: "pointer" }}>
          + new file
        </button>
      )}
    </div>
  );
}

// ── Root ──────────────────────────────────────────────────────────────────────

export default function Home() {
  const {
    conns, error, branches, activeBranch, files, ticks, branchHead, openFiles,
    agentLogs, preview, contextAtoms, contextAnnotations, rebaseMarker,
    connect, createBranch, deleteBranch, selectBranch, openFile, closeFile,
    appendToFile, editAtom, deleteAtom, addNote, moveTick, deleteTickEntry,
    toggleContextAtom, toggleContextAnnotation, clearContext, clearAgentLogs, chatWrite, chatFix,
    setRebaseMarker,
  } = useStory();

  const [annotationMode, setAnnotationMode] = useState<AnnotationMode>("expanded");
  const [leftOpen, setLeftOpen] = useState(true);
  const [leftWidth, setLeftWidth] = useState(260);
  const [isResizing, setIsResizing] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<"explorer" | "branches">("branches");
  const [centerTab, setCenterTab] = useState<"file" | "ticks">("file");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);
  const [showNewFile, setShowNewFile] = useState(false);
  const [newFilePath, setNewFilePath] = useState("");

  const sessionStatus = conns.find((c) => c.label === "session")?.status ?? "disconnected";

  function parsePath(pathname: string): { branch: string | null; file: string | null } {
    const parts = pathname.replace(/^\//, "").split("/").filter(Boolean);
    if (parts.length === 0) return { branch: null, file: null };
    return {
      branch: decodeURIComponent(parts[0]),
      file: parts.length > 1 ? parts.slice(1).map(decodeURIComponent).join("/") : null,
    };
  }

  function pushPath(branch: string | null, file: string | null) {
    if (!branch) { history.pushState(null, "", "/"); return; }
    const encodedFile = file ? "/" + file.split("/").map(encodeURIComponent).join("/") : "";
    history.pushState(null, "", `/${encodeURIComponent(branch)}${encodedFile}`);
  }

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => {
    const { branch, file } = parsePath(window.location.pathname);
    connect().then(() => {
      if (branch) {
        selectBranch(branch).then(() => {
          if (file) { setSelectedFile(file); openFile(file); setCenterTab("file"); }
          else setCenterTab("ticks");
        });
        setSidebarTab("explorer");
      }
    });

    const onPopState = () => {
      const { branch: b, file: f } = parsePath(window.location.pathname);
      if (b) { selectBranch(b); setSidebarTab("explorer"); }
      setSelectedFile(f);
      if (f) { openFile(f); setCenterTab("file"); }
      else setCenterTab("ticks");
    };
    window.addEventListener("popstate", onPopState);
    return () => window.removeEventListener("popstate", onPopState);
  }, []);

  const handleEditAtom = useCallback((tickId: string, content: string) => {
    if (selectedFile) editAtom(selectedFile, tickId, content);
  }, [selectedFile, editAtom]);

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
    pushPath(activeBranch, path);
  }

  function handleCloseFile() {
    if (selectedFile) closeFile(selectedFile);
    setSelectedFile(null);
    pushPath(activeBranch, null);
  }

  function handleSelectBranch(name: string) {
    handleCloseFile();
    selectBranch(name);
    setSidebarTab("explorer");
    pushPath(name, null);
  }

  function handleCreateFile() {
    const path = newFilePath.trim().replace(/^\/+/, "");
    if (!path) return;
    setShowNewFile(false);
    setNewFilePath("");
    if (activeBranch) handleSelectFile(path);
  }

  function onSidebarResizeMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    setIsResizing(true);
    const startX = e.clientX, startW = leftWidth;
    function onMove(ev: MouseEvent) { setLeftWidth(Math.max(180, Math.min(420, startW + (ev.clientX - startX)))); }
    function onUp() { setIsResizing(false); window.removeEventListener("mousemove", onMove); window.removeEventListener("mouseup", onUp); }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100vh", overflow: "hidden", cursor: isResizing ? "col-resize" : undefined }}>
      <TopBar sessionStatus={sessionStatus} branches={branches} activeBranch={activeBranch} />

      <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
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

        <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden", minWidth: 0 }}>
          <Toolbar
            leftOpen={leftOpen} onToggleLeft={() => setLeftOpen((v) => !v)}
            selectedFile={selectedFile}
            onCloseFile={handleCloseFile}
            onNewFile={() => setShowNewFile((v) => !v)}
            centerTab={centerTab} onCenterTab={(tab) => {
              setCenterTab(tab);
              pushPath(activeBranch, tab === "file" ? selectedFile : null);
            }}
          />

          {centerTab === "file" && <>
            {showNewFile && (
              <div style={{ flexShrink: 0, padding: "7px 14px", borderBottom: "1px solid var(--border-subtle)", background: "var(--card)", display: "flex", gap: 8, alignItems: "center" }}>
                <input
                  value={newFilePath} onChange={(e) => setNewFilePath(e.target.value)}
                  onKeyDown={(e) => { if (e.key === "Enter") handleCreateFile(); if (e.key === "Escape") setShowNewFile(false); }}
                  placeholder="path/to/file.md" autoFocus
                  style={{ flex: 1, fontSize: 11, padding: "4px 8px", background: "var(--card)", border: "1px solid var(--border)", borderRadius: 5, color: "var(--foreground)", outline: "none" }}
                />
                <button onClick={handleCreateFile} style={{ fontSize: 11, padding: "4px 10px", background: "var(--amber)", border: "none", borderRadius: 5, color: "oklch(0.15 0.01 60)", fontWeight: 600, cursor: "pointer" }}>Open</button>
                <button onClick={() => setShowNewFile(false)} style={{ fontSize: 11, padding: "4px 8px", background: "transparent", border: "1px solid var(--border)", borderRadius: 5, color: "var(--text-label)", cursor: "pointer" }}>✕</button>
              </div>
            )}

            {selectedFile && !isAbsent && fileTicks.length > 0 && (
              <div style={{ flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)", display: "flex", alignItems: "center" }}>
                {contextAtoms.size > 0 && (
                  <button
                    onClick={() => {
                      // Delete tail-first so earlier ids aren't invalidated.
                      fileTicks.filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
                        .map((t) => t.tickId).reverse()
                        .forEach((tickId) => selectedFile && deleteAtom(selectedFile, tickId));
                      clearContext();
                    }}
                    title={`Delete ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""}`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "oklch(0.65 0.18 25 / 0.15)", border: "1px solid oklch(0.65 0.18 25 / 0.35)", color: "var(--rose)" }}
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
                  onClick={() => setAnnotationMode((m) => m === "hidden" ? "dots" : m === "dots" ? "expanded" : "hidden")}
                  title={annotationMode === "hidden" ? "Show annotation dots" : annotationMode === "dots" ? "Expand annotations" : "Hide annotations"}
                  style={{ width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: annotationMode !== "hidden" ? "oklch(0.78 0.10 65 / 0.15)" : "transparent", color: annotationMode === "expanded" ? "var(--amber)" : annotationMode === "dots" ? "oklch(0.65 0.08 65)" : "var(--text-dim)" }}
                >
                  {annotationMode === "hidden" ? <EyeOff style={{ width: 11, height: 11 }} /> : <Eye style={{ width: 11, height: 11, opacity: annotationMode === "dots" ? 0.6 : 1 }} />}
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
            ) : fileTicks.length === 0 ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                Loading…
              </div>
            ) : (
              <WireTickList
                ticks={fileTicks} annotationMode={annotationMode}
                contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
                resetKey={selectedFile}
                rebaseMarker={rebaseMarker}
                onSetRebaseMarker={setRebaseMarker}
                onEdit={handleEditAtom}
                onToggleContextAtom={toggleContextAtom}
                onToggleContextAnnotation={toggleContextAnnotation}
              />
            )}

            <ChatPreviewStrip preview={preview} />
            <AgentLogStrip logs={agentLogs} onClear={clearAgentLogs} />
            <InputBar
              enabled={selectedFile !== null}
              contextAtomCount={contextAtoms.size} contextAnnotationCount={contextAnnotations.size}
              rebasing={rebaseMarker !== null}
              onClearRebase={() => setRebaseMarker(null)}
              onClearContext={clearContext}
              onAppend={(text) => selectedFile && appendToFile(selectedFile, text)}
              onWrite={(text)  => selectedFile && chatWrite(selectedFile, text)}
              onFix={(text)    => selectedFile && chatFix(selectedFile, text)}
            />
          </>}

          {centerTab === "ticks" && (
            <TicksView
              activeBranch={activeBranch}
              ticks={tickChain(ticks, branchHead).reverse()}
              onAddNote={addNote} onMoveTick={moveTick} onDeleteTick={deleteTickEntry}
            />
          )}
        </div>
      </div>
    </div>
  );
}
