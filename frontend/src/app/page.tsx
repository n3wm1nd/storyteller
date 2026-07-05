"use client";

import { useCallback, useEffect, useState } from "react";
import { PanelLeftClose, PanelLeftOpen, PanelRightClose, PanelRightOpen, Eye, EyeOff, Trash2, Users, ListTree, Combine, Split } from "lucide-react";
import { useStory } from "@/lib/store";
import { tickChain, statusColor, presentDuringAtoms, allPresentCharacters, characterColor, type AnnotationMode } from "@/lib/utils";
import { LeftSidebar } from "./sidebar";
import { WireTickList, AgentLogStrip, ChatPreviewStrip, InputBar, type PresenceBar } from "./fileview";
import { TicksView } from "./ticksview";
import { CharacterSidebar } from "./character-sidebar";

// The whole-story outline is `outline.md` (optionally in a subdir). Chapter
// beat sheets are `ch{N}.outline.md` — those are outputs of the split, not
// inputs to it, so only the bare `outline.md` gets the "generate beat sheets"
// action (see WRITER.md).
function isOutlineFile(path: string): boolean {
  const name = decodeURIComponent(path.split("/").pop() ?? "");
  return name === "outline.md";
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

function Toolbar({ leftOpen, onToggleLeft, rightOpen, onToggleRight, selectedFile, onCloseFile, centerTab, onCenterTab }: {
  leftOpen: boolean;
  onToggleLeft: () => void;
  rightOpen: boolean;
  onToggleRight: () => void;
  selectedFile: string | null;
  onCloseFile: () => void;
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
      <div style={{ width: 1, height: 16, background: "var(--border-subtle)", alignSelf: "center", margin: "0 6px" }} />
      <button onClick={onToggleRight} style={{ ...iconBtnStyle, alignSelf: "center" }}>
        {rightOpen ? <PanelRightClose style={{ width: 14, height: 14 }} /> : <PanelRightOpen style={{ width: 14, height: 14 }} />}
      </button>
    </div>
  );
}

// ── Root ──────────────────────────────────────────────────────────────────────

export default function Home() {
  const {
    conns, error, branches, characterBranches, activeBranch, files, ticks, branchHead, openFiles,
    openCharacters, openJournals, journalMarkers, agentLogs, preview, contextAtoms, contextAnnotations, rebaseMarker,
    hoverHighlight, connect, createBranch, deleteBranch, selectBranch, openFile, createFile, closeFile,
    openCharacter, closeCharacter, openJournal, closeJournal, trackJournal,
    editJournalAtom, deleteJournalAtom, journalFix, setJournalMarker, appendJournal,
    setHoverHighlight, clearHoverHighlight, enterScene, leaveScene,
    appendToFile, editAtom, deleteAtom, mergeSelected, splitSelected, addNote, moveTick, deleteTickEntry, uploadFiles,
    toggleContextAtom, toggleContextAnnotation, clearContext, clearAgentLogs, chatWrite, chatFix, chatNote, chatRegen, chatOutline,
    setRebaseMarker,
  } = useStory();

  const [annotationMode, setAnnotationMode] = useState<AnnotationMode>("expanded");
  const [showAllPresence, setShowAllPresence] = useState(false);
  const [leftOpen, setLeftOpen] = useState(true);
  const [leftWidth, setLeftWidth] = useState(260);
  const [isResizing, setIsResizing] = useState(false);
  const [rightOpen, setRightOpen] = useState(true);
  const [rightWidth, setRightWidth] = useState(260);
  const [isResizingRight, setIsResizingRight] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<"explorer" | "branches" | "characters">("branches");
  const [hoveredCharacter, setHoveredCharacter] = useState<string | null>(null);
  const [centerTab, setCenterTab] = useState<"file" | "ticks">("file");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);

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
  // Presence is scoped to the open file (a scene), not the whole branch —
  // see WRITER.md — so this folds the file's own chain, not the branch-wide
  // one. "Show all" (persistent, toolbar toggle) wins over hover —
  // first-appearance order both here and in 'allPresentCharacters' itself,
  // so lane 0 (closest to the text) is always whoever entered first, per
  // the same ordering 'activeCharacterBranches' already uses for the sidebar.
  const fileChainTicks = fileConn?.ticks ?? {};
  const fileChainHead  = fileConn?.head ?? null;
  const presenceBars: PresenceBar[] = showAllPresence
    ? allPresentCharacters(fileChainTicks, fileChainHead).map((c) => ({
        character: c, color: characterColor(c), tickIds: presentDuringAtoms(fileChainTicks, fileChainHead, c),
      }))
    : hoveredCharacter
    ? [{ character: hoveredCharacter, color: characterColor(hoveredCharacter), tickIds: presentDuringAtoms(fileChainTicks, fileChainHead, hoveredCharacter) }]
    : [];
  // Global cross-component highlight (e.g. hovering a journal entry in the
  // character sidebar) — folded into the same bar mechanism as an extra
  // lane, since it's the same shape (tickIds + color -> a colored run).
  if (hoverHighlight) presenceBars.push({ character: "__hover__", color: hoverHighlight.color, tickIds: hoverHighlight.tickIds });

  function handleSelectFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setSelectedFile(path);
    openFile(path);
    setCenterTab("file");
    pushPath(activeBranch, path);
  }

  function handleCreateFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setSelectedFile(path);
    createFile(path);
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

  // Selection (contextAtoms) is shared across the main scene and every open
  // journal (see character-sidebar.tsx) — deleting/fixing "the selection"
  // therefore has to sweep every chain that might contain a selected id, not
  // just the currently open scene file. A given tickId only ever appears in
  // the one chain it actually belongs to, so this is just "check each open
  // chain for members of the shared set," not a routing decision.
  function handleDeleteSelected() {
    if (selectedFile) {
      fileTicks.filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
        .map((t) => t.tickId).reverse()
        .forEach((tickId) => deleteAtom(selectedFile, tickId));
    }
    for (const [branch, jc] of Object.entries(openJournals)) {
      tickChain(jc.ticks, jc.head).filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
        .map((t) => t.tickId).reverse()
        .forEach((tickId) => deleteJournalAtom(branch, tickId, journalMarkers[branch] ?? null));
    }
    clearContext();
  }

  function handleFix(text: string) {
    if (selectedFile) {
      const hasSelection = fileTicks.some((t) => t.kind === "atom" && contextAtoms.has(t.tickId));
      if (hasSelection) chatFix(selectedFile, text);
    }
    for (const [branch, jc] of Object.entries(openJournals)) {
      const targets = tickChain(jc.ticks, jc.head)
        .filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
        .map((t) => t.tickId);
      if (targets.length > 0) journalFix(branch, text, targets, journalMarkers[branch] ?? null);
    }
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

  function onRightSidebarResizeMouseDown(e: React.MouseEvent) {
    e.preventDefault();
    setIsResizingRight(true);
    const startX = e.clientX, startW = rightWidth;
    function onMove(ev: MouseEvent) { setRightWidth(Math.max(200, Math.min(420, startW - (ev.clientX - startX)))); }
    function onUp() { setIsResizingRight(false); window.removeEventListener("mousemove", onMove); window.removeEventListener("mouseup", onUp); }
    window.addEventListener("mousemove", onMove);
    window.addEventListener("mouseup", onUp);
  }

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100vh", overflow: "hidden", cursor: (isResizing || isResizingRight) ? "col-resize" : undefined }}>
      <TopBar sessionStatus={sessionStatus} branches={branches} activeBranch={activeBranch} />

      <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
        {leftOpen && (
          <div style={{ width: leftWidth, minWidth: leftWidth, position: "relative", display: "flex", flexDirection: "column" }}>
            <LeftSidebar
              tab={sidebarTab} setTab={setSidebarTab}
              branches={branches} characterBranches={characterBranches} activeBranch={activeBranch}
              files={files} selectedFile={selectedFile}
              onSelectBranch={handleSelectBranch}
              onSelectFile={handleSelectFile}
              onCreateFile={handleCreateFile}
              onCreateBranch={createBranch}
              onDeleteBranch={deleteBranch}
              onHoverCharacter={setHoveredCharacter}
              onUploadFiles={uploadFiles}
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
            rightOpen={rightOpen} onToggleRight={() => setRightOpen((v) => !v)}
            selectedFile={selectedFile}
            onCloseFile={handleCloseFile}
            centerTab={centerTab} onCenterTab={(tab) => {
              setCenterTab(tab);
              pushPath(activeBranch, tab === "file" ? selectedFile : null);
            }}
          />

          {centerTab === "file" && <>
            {selectedFile && !isAbsent && fileTicks.length > 0 && (
              <div style={{ flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)", display: "flex", alignItems: "center" }}>
                {contextAtoms.size > 0 && (
                  <button
                    onClick={handleDeleteSelected}
                    title={`Delete ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""}`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "oklch(0.65 0.18 25 / 0.15)", border: "1px solid oklch(0.65 0.18 25 / 0.35)", color: "var(--rose)" }}
                  >
                    <Trash2 style={{ width: 10, height: 10 }} />
                    Delete {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 2 && (
                  <button
                    onClick={() => mergeSelected(selectedFile)}
                    title={`Merge ${contextAtoms.size} selected atoms into one`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.35)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <Combine style={{ width: 10, height: 10 }} />
                    Merge {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 1 && (
                  <button
                    onClick={() => splitSelected(selectedFile)}
                    title={`Re-split ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""} at paragraph/heading boundaries`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.35)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <Split style={{ width: 10, height: 10 }} />
                    Split {contextAtoms.size}
                  </button>
                )}
                {selectedFile && isOutlineFile(selectedFile) && (
                  <button
                    onClick={() => chatOutline(selectedFile)}
                    title="Generate a per-chapter beat sheet for each chapter this outline implies"
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "oklch(0.78 0.10 65 / 0.15)", border: "1px solid oklch(0.78 0.10 65 / 0.35)", color: "var(--amber)", marginLeft: contextAtoms.size > 0 ? 6 : 0 }}
                  >
                    <ListTree style={{ width: 10, height: 10 }} />
                    Generate beat sheets
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
                <button
                  onClick={() => setShowAllPresence((v) => !v)}
                  title={showAllPresence ? "Hide character presence bars" : "Show character presence bars"}
                  style={{ width: 22, height: 22, marginLeft: 2, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: showAllPresence ? "oklch(0.58 0.10 200 / 0.15)" : "transparent", color: showAllPresence ? "oklch(0.68 0.12 200)" : "var(--text-dim)" }}
                >
                  <Users style={{ width: 11, height: 11 }} />
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
                presenceBars={presenceBars}
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
              onFix={handleFix}
              onNote={(text)   => selectedFile && chatNote(selectedFile, text)}
              onRegen={(text, byBeat) => selectedFile && chatRegen(selectedFile, text, byBeat)}
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

        {rightOpen && (
          <div style={{ width: rightWidth, minWidth: rightWidth, position: "relative", display: "flex", flexDirection: "column" }}>
            <div
              onMouseDown={onRightSidebarResizeMouseDown}
              style={{
                position: "absolute", top: 0, left: 0, bottom: 0, width: 4,
                cursor: "col-resize", zIndex: 10,
                borderLeft: "1px solid var(--border-subtle)",
                background: isResizingRight ? "oklch(0.25 0.015 60 / 0.4)" : "transparent",
                transition: "background 0.15s",
              }}
            />
            <CharacterSidebar
              selectedFile={selectedFile}
              characterBranches={characterBranches}
              ticks={fileChainTicks} head={fileChainHead} rebaseMarker={rebaseMarker}
              openCharacters={openCharacters}
              openCharacter={openCharacter} closeCharacter={closeCharacter}
              openJournals={openJournals}
              openJournal={openJournal} closeJournal={closeJournal}
              journalMarkers={journalMarkers} setJournalMarker={setJournalMarker}
              trackJournal={trackJournal} editJournalAtom={editJournalAtom} appendJournal={appendJournal}
              contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
              toggleContextAtom={toggleContextAtom} toggleContextAnnotation={toggleContextAnnotation}
              onHoverAtoms={setHoverHighlight} onHoverEnd={clearHoverHighlight}
              enterScene={enterScene} leaveScene={leaveScene}
            />
          </div>
        )}
      </div>
    </div>
  );
}
