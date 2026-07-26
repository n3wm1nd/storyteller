"use client";

import { useEffect, useMemo, useState } from "react";
import { PanelLeftClose, PanelLeftOpen, PanelRightClose, PanelRightOpen } from "lucide-react";
import { useServerCache } from "@/lib/serverCacheStore";
import { useUI } from "@/lib/uiStore";
import { connect, createBranch, deleteBranch, selectBranch, uploadFiles, createChapter, importCharacterCard } from "./sidebar.actions";
import {
  openFile, createFile, deleteFile, renameFile, checkpointFile, closeFile,
  chatConverse, chatConverseRegen, cycleSwipe, editAtom, editPrompt, chatNote,
} from "./fileview.actions";
import { addNote, moveTick, deleteTickEntry } from "./ticksview.actions";
import { tickChain, statusColor } from "@/lib/utils";
import { LeftSidebar } from "./sidebar";
import { ProseFileView, ProseSidebar } from "./prose-file-view";
import { DslFileView, DslSidebar } from "./dsl-file-view";
import { PromptFileView, PromptSidebar } from "./prompt-file-view";
import { fileSurfaceOf, centerTabsFor, type CenterTab } from "@/lib/fileSurface";
import { ChatView } from "./chatview";
import { TicksView } from "./ticksview";
import { AgentsTab } from "./agentstab";
import { isChatFile } from "@/lib/agents";
import { UndoTimeline } from "./undo-timeline";

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
        <div style={{ width: 10, height: 10, borderRadius: "50%", background: "var(--amber)", boxShadow: "0 0 10px var(--amber-border)" }} />
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
      <UndoTimeline />
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

// 'tabs' is the open file's own surface talking (see lib/fileSurface.ts's
// centerTabsFor) — this component renders whatever it's handed rather than
// deciding for itself which tabs a file supports, so a new surface never
// means editing the toolbar.
function Toolbar({ leftOpen, onToggleLeft, rightOpen, onToggleRight, rightAvailable, selectedFile, onCloseFile, centerTab, onCenterTab, tabs }: {
  leftOpen: boolean;
  onToggleLeft: () => void;
  rightOpen: boolean;
  onToggleRight: () => void;
  rightAvailable: boolean;
  selectedFile: string | null;
  onCloseFile: () => void;
  centerTab: CenterTab;
  onCenterTab: (t: CenterTab) => void;
  tabs: CenterTab[];
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

      {tabs.filter((t) => t !== "file").map((t) => (
        <button key={t} onClick={() => onCenterTab(t)}
          title={t === "agents" ? "Configure the agents available for this editor: their context slots and prompt overrides" : undefined}
          style={{
            padding: "0 10px", fontSize: 11, fontWeight: 500,
            border: "none", borderBottom: centerTab === t ? "2px solid var(--amber)" : "2px solid transparent",
            borderTop: "2px solid transparent", background: "transparent",
            color: centerTab === t ? "var(--amber)" : "var(--text-disabled)",
            cursor: "pointer", transition: "color 0.15s, border-color 0.15s",
          }}>{t === "ticks" ? "Ticks" : t === "chat" ? "Chat" : "Agents"}</button>
      ))}

      <span style={{ flex: 1 }} />
      {rightAvailable && <>
        <div style={{ width: 1, height: 16, background: "var(--border-subtle)", alignSelf: "center", margin: "0 6px" }} />
        <button onClick={onToggleRight} style={{ ...iconBtnStyle, alignSelf: "center" }}>
          {rightOpen ? <PanelRightClose style={{ width: 14, height: 14 }} /> : <PanelRightOpen style={{ width: 14, height: 14 }} />}
        </button>
      </>}
    </div>
  );
}

// ── Root ──────────────────────────────────────────────────────────────────────

export default function Home() {
  // Selected field-by-field rather than one bulk destructure off the store
  // hook — with no selector, zustand subscribes to *every* field, so any
  // unrelated slice changing (a different open file's ticks, a background
  // character update) would re-render this entire root component and, with
  // it, every unmemoized tab below. Per-field selectors mean each only
  // triggers a re-render when the thing it actually reads changes.
  const branches          = useServerCache((s) => s.branches);
  const characterBranches = useServerCache((s) => s.characterBranches);
  const activeBranch      = useServerCache((s) => s.activeBranch);
  const files             = useServerCache((s) => s.files);
  const ticks             = useServerCache((s) => s.ticks);
  const branchHead        = useServerCache((s) => s.branchHead);
  const libraryTree       = useServerCache((s) => s.libraryTree);
  const libraryChapters   = useServerCache((s) => s.libraryChapters);
  const openFiles         = useServerCache((s) => s.openFiles);
  // 'preview' (the in-flight streamed draft) is deliberately not read here —
  // it can update several times a second, and this component owns the whole
  // page tree, so subscribing here would reconcile all of it on every token.
  // 'ChatPreviewStrip'/'InputBar'/'ChatView' each subscribe to it directly
  // instead, confining that reconciliation to just themselves.

  const conns               = useUI((s) => s.conns);
  const error               = useUI((s) => s.error);
  const agentLogs           = useUI((s) => s.agentLogs);
  const clearAgentLogs      = useUI((s) => s.clearAgentLogs);

  const [leftOpen, setLeftOpen] = useState(true);
  const [leftWidth, setLeftWidth] = useState(260);
  const [isResizing, setIsResizing] = useState(false);
  const [rightOpen, setRightOpen] = useState(true);
  const [rightWidth, setRightWidth] = useState(260);
  const [isResizingRight, setIsResizingRight] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<"explorer" | "branches" | "characters" | "library">("branches");
  const [hoveredCharacter, setHoveredCharacter] = useState<string | null>(null);
  const [centerTab, setCenterTab] = useState<CenterTab>("file");
  // What the center file pane currently displays — a *single* atomic value,
  // not several separately-managed pieces of state kept in sync via
  // reset/restore effects. 'summary', when present, names one alt-chain
  // connection to show instead of the real file:
  //   - 'kind' is the family.
  //   - 'hops' is a chain of Summary tick ids, exactly mirroring
  //     Server/Writer/File/Connection.hs's own "{branch}@{kind}#tid1#tid2..."
  //     target grammar. There's no separate "which occurrence" vs "which
  //     nested tier" concept — openTarget resolves *any* hop the same way
  //     (seed the connection from that tick's own altHead, content-
  //     addressed, cascading a re-mint back up on write), so clicking any
  //     annotation — a sibling occurrence or a nested tier's own — is the
  //     identical operation: push its id as one more hop. Every hop chain
  //     is equally live/editable, exactly like opening any other file at
  //     a specific point in its history and continuing to write from
  //     there — there is no read-only tier here. Empty hops means "this
  //     family's current live state," same as opening a brand new file
  //     needs no history behind it either.
  //
  // This whole value is exactly what the URL encodes (see parsePath/
  // pushPath) — every transition that changes it goes through one of the
  // navigate*/close* functions below, which update this *and* push the URL
  // in the same place, and the popstate/mount handlers set it straight
  // from a freshly parsed URL. There is deliberately no separate "restore
  // vs. reset" bookkeeping anywhere: this value and the URL are the same
  // fact, so there's nothing to keep in sync.
  const [viewTarget, setViewTarget] = useState<{
    file: string;
    summary: { kind: string; hops: string[] } | null;
  } | null>(null);
  const selectedFile = viewTarget?.file ?? null;
  const viewingSummary = viewTarget?.summary ?? null;

  const sessionStatus = conns.find((c) => c.label === "session")?.status ?? "disconnected";

  // Which editing surface owns the open file (see lib/fileSurface.ts). The
  // one decision the whole file area hangs off: it picks the centre view,
  // supplies the right panel's content, and says which centre tabs exist.
  // Nothing below asks "is this a .dsl" or "are we on the prompts branch"
  // a second time.
  const surface = fileSurfaceOf(activeBranch, selectedFile);
  const centerTabs = centerTabsFor(surface, selectedFile);

  // Selecting a file whose surface doesn't have the tab currently showing
  // (leaving a chat/ file for a .dsl, say) lands back on the file itself
  // rather than on a tab that no longer exists — otherwise the centre pane
  // would render nothing at all.
  const tabsKey = centerTabs.join(",");
  useEffect(() => {
    if (!centerTabs.includes(centerTab)) setCenterTab("file");
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [tabsKey, centerTab]);

  // Same "reserved literal ahead of a name that could be anything" shape as
  // the backend's own /branch/{name}/file/{path} split (see app/Server.hs) —
  // a branch could otherwise be named the same as some future top-level
  // route, and a file within it the same as some future reserved segment
  // under a branch, so both "branch" and "file" are fixed prefixes a real
  // name can never coincide with, rather than the name sitting bare at
  // whatever position happens to be unclaimed today.
  // A viewed summary rides along as query params rather than extra path
  // segments — 'kind'/'hops' (tick ids) can both contain characters that
  // would need their own escaping scheme in a path segment, and
  // URLSearchParams already handles that for free. Deep-linkable down to
  // the exact hop chain, since that really is "which page you're on" (see
  // 'viewTarget's own doc).
  function parsePath(pathname: string, search: string): {
    branch: string | null; file: string | null;
    summary: { kind: string; hops: string[] } | null;
  } {
    const parts = pathname.replace(/^\//, "").split("/").filter(Boolean);
    if (parts.length < 2 || parts[0] !== "branch") return { branch: null, file: null, summary: null };
    const params = new URLSearchParams(search);
    const kind = params.get("summary");
    const hops = params.get("at");
    return {
      branch: decodeURIComponent(parts[1]),
      file: parts.length > 3 && parts[2] === "file" ? parts.slice(3).map(decodeURIComponent).join("/") : null,
      summary: kind ? { kind, hops: hops ? hops.split(",").map(decodeURIComponent) : [] } : null,
    };
  }

  function pushPath(
    branch: string | null, file: string | null,
    summary?: { kind: string; hops: string[] } | null,
  ) {
    if (!branch) { history.pushState(null, "", "/"); return; }
    const encodedFile = file ? "/file/" + file.split("/").map(encodeURIComponent).join("/") : "";
    let query = "";
    if (summary) {
      const params = new URLSearchParams();
      params.set("summary", summary.kind);
      if (summary.hops.length > 0) params.set("at", summary.hops.map(encodeURIComponent).join(","));
      query = "?" + params.toString();
    }
    history.pushState(null, "", `/branch/${encodeURIComponent(branch)}${encodedFile}${query}`);
  }

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => {
    const { branch, file, summary } = parsePath(window.location.pathname, window.location.search);
    connect().then(() => {
      if (branch) {
        selectBranch(branch).then(() => {
          if (file) {
            setViewTarget({ file, summary });
            openFile(file); setCenterTab(isChatFile(file) ? "chat" : "file");
          } else setCenterTab("ticks");
        });
        setSidebarTab("explorer");
      }
    });

    const onPopState = () => {
      const { branch: b, file: f, summary } = parsePath(window.location.pathname, window.location.search);
      if (b) { selectBranch(b); setSidebarTab("explorer"); }
      setViewTarget(f ? { file: f, summary } : null);
      if (f) { openFile(f); setCenterTab(isChatFile(f) ? "chat" : "file"); }
      else setCenterTab("ticks");
    };
    window.addEventListener("popstate", onPopState);
    return () => window.removeEventListener("popstate", onPopState);
  }, []);

  // Every place that changes which summary (if any) is being viewed goes
  // through one of these two — updating 'viewTarget' and pushing the
  // matching URL together, in one place, rather than as two separately-
  // timed effects that can race (the bug this replaced: a ref-based
  // "pending restore" fighting a "reset on file change" effect). 'hops' is
  // exactly the clicked summary tick's own id chain, carried straight
  // through — never re-resolved against some other list later.
  function navigateToSummary(kind: string, hops: string[]) {
    if (!selectedFile) return;
    setViewTarget({ file: selectedFile, summary: { kind, hops } });
    pushPath(activeBranch, selectedFile, { kind, hops });
  }

  function closeSummaryView() {
    if (!selectedFile) return;
    setViewTarget({ file: selectedFile, summary: null });
    pushPath(activeBranch, selectedFile, null);
  }

  const fileConn = selectedFile ? openFiles[selectedFile] : null;
  const fileChainTicks = fileConn?.ticks ?? {};
  const fileChainHead  = fileConn?.head ?? null;
  // The prose surface owns everything else derived from this connection —
  // the atom chain, summary tiers, presence, selection actions (see
  // prose-file-view.tsx). What stays here is only what a *non*-prose
  // consumer needs: the Chat tab reads the same chain, and the Ticks tab
  // reads the branch's.
  // Same reasoning as fileTicks above, but for the whole-branch chain the
  // Ticks tab shows (which can run into the hundreds) — without this, every
  // unrelated re-render while that tab is open re-walks and re-reverses the
  // entire branch history and hands TicksView a new array identity, which
  // then can't tell "nothing changed" from "everything changed".
  const branchTicksNewestFirst = useMemo(
    () => tickChain(ticks, branchHead).reverse(),
    [ticks, branchHead],
  );

  function handleSelectFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setViewTarget({ file: path, summary: null });
    openFile(path);
    setCenterTab(isChatFile(path) ? "chat" : "file");
    pushPath(activeBranch, path);
  }

  function handleCreateFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setViewTarget({ file: path, summary: null });
    createFile(path);
    setCenterTab(isChatFile(path) ? "chat" : "file");
    pushPath(activeBranch, path);
  }

  function handleDeleteFile(path: string) {
    deleteFile(path);
    closeFile(path);
    if (selectedFile === path) setViewTarget(null);
    pushPath(activeBranch, null);
  }

  // Covers both the rename UI action and drag-to-move (dropping a file
  // onto a folder in the tree) — a move is just a rename to a path under
  // the target folder, same server command either way.
  function handleRenameFile(path: string, newPath: string) {
    renameFile(path, newPath);
    closeFile(path);
    if (selectedFile === path) {
      setViewTarget({ file: newPath, summary: null });
      openFile(newPath);
      setCenterTab(isChatFile(newPath) ? "chat" : "file");
      pushPath(activeBranch, newPath);
    }
  }

  // Unlike delete/rename, the path itself never changes — the already-open
  // connection's own ref-move notification picks up the resulting update,
  // same as any other in-place edit, so there's nothing else to do here.
  function handleCheckpointFile(path: string) {
    checkpointFile(path);
  }

  function handleCloseFile() {
    if (selectedFile) closeFile(selectedFile);
    setViewTarget(null);
    pushPath(activeBranch, null);
  }

  function handleSelectBranch(name: string) {
    handleCloseFile();
    selectBranch(name);
    setSidebarTab("explorer");
    pushPath(name, null);
  }

  // Agents tab -> "open this prompt override in the main file view" (see
  // agentstab.tsx's PromptOverrides). Branch switch is async — only
  // openFile/pushPath once selectBranch's new connection is up, same
  // sequencing the initial-load effect above uses. Skips the branch-switch
  // round trip entirely when already on "prompts" (e.g. editing one
  // override's file, then jumping to a different one).
  function handleJumpToPrompt(path: string) {
    handleCloseFile();
    setSidebarTab("explorer");
    if (activeBranch === "prompts") {
      setViewTarget({ file: path, summary: null });
      openFile(path);
      setCenterTab("file");
      pushPath("prompts", path);
    } else {
      selectBranch("prompts").then(() => {
        setViewTarget({ file: path, summary: null });
        openFile(path);
        setCenterTab("file");
        pushPath("prompts", path);
      });
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
              libraryTree={libraryTree} libraryChapters={libraryChapters}
              onSelectBranch={handleSelectBranch}
              onSelectFile={handleSelectFile}
              onCreateFile={handleCreateFile}
              onDeleteFile={handleDeleteFile}
              onRenameFile={handleRenameFile}
              onCheckpointFile={handleCheckpointFile}
              onCreateBranch={createBranch}
              onDeleteBranch={deleteBranch}
              onCreateChapter={createChapter}
              onHoverCharacter={setHoveredCharacter}
              onUploadFiles={uploadFiles}
              onImportCharacterCard={importCharacterCard}
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
            rightAvailable={centerTab === "file" && selectedFile !== null}
            selectedFile={selectedFile}
            onCloseFile={handleCloseFile}
            tabs={centerTabs}
            centerTab={centerTab} onCenterTab={(tab) => {
              setCenterTab(tab);
              // This only ever switches which top-level tab is showing —
              // 'viewTarget' itself doesn't change, so the URL it pushes
              // must still reflect whatever it already was (including a
              // currently-open summary view), not silently drop it.
              pushPath(activeBranch, tab === "file" ? selectedFile : null, tab === "file" ? viewingSummary : null);
            }}
          />

          {/* The file area is exactly "which surface owns this file, render
              it" — see lib/fileSurface.ts. Each surface replaces the pane
              outright; none of them is a mode inside another. */}
          {centerTab === "file" && <>
            {!activeBranch ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                {sessionStatus === "connecting" ? "Connecting…" : sessionStatus === "connected" ? "Select a branch" : "Disconnected"}
              </div>
            ) : !selectedFile ? (
              <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                Select a file or create a new one
              </div>
            ) : surface === "dsl" ? (
              <DslFileView branch={activeBranch} path={selectedFile} />
            ) : surface === "prompt" ? (
              <PromptFileView branch={activeBranch} path={selectedFile} />
            ) : (
              <ProseFileView
                branch={activeBranch}
                path={selectedFile}
                viewingSummary={viewingSummary}
                onNavigateToSummary={navigateToSummary}
                onCloseSummary={closeSummaryView}
                hoveredCharacter={hoveredCharacter}
              />
            )}
          </>}


          {centerTab === "ticks" && (
            <TicksView
              activeBranch={activeBranch}
              ticks={branchTicksNewestFirst}
              onAddNote={addNote} onMoveTick={moveTick} onDeleteTick={deleteTickEntry}
              onSelectFile={handleSelectFile}
            />
          )}

          {centerTab === "chat" && selectedFile && (
            <ChatView
              ticks={fileChainTicks} head={fileChainHead}
              agentLogs={agentLogs} onClearAgentLogs={clearAgentLogs}
              onSend={(text) => chatConverse(selectedFile, text)}
              onNote={(text) => chatNote(selectedFile, text)}
              onRegen={(promptTickId, atomTickId, text) => chatConverseRegen(selectedFile, promptTickId, atomTickId, text)}
              onCycleSwipe={(tickId) => cycleSwipe(selectedFile, tickId)}
              onEditAtom={(tickId, content) => editAtom(selectedFile, tickId, content)}
              onEditPrompt={(tickId, content) => editPrompt(selectedFile, tickId, content)}
            />
          )}

          {centerTab === "agents" && selectedFile && (
            <AgentsTab path={selectedFile} branch={activeBranch} surface={surface} onJumpToPrompt={handleJumpToPrompt} />
          )}
        </div>

        {/* The right panel: page.tsx owns the shell (width, drag handle),
            the open file's surface owns what's inside it. A sidebar is
            part of the editor you're in — scene presence and cost belong
            to prose, a resolved-program preview to the DSL editor, the
            agents reading a key to a prompt override — so there is no
            fixed content here to gate per surface. */}
        {rightOpen && centerTab === "file" && activeBranch && selectedFile && (
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
            {surface === "dsl" ? (
              <DslSidebar branch={activeBranch} path={selectedFile} />
            ) : surface === "prompt" ? (
              <PromptSidebar path={selectedFile} onOpenFile={handleSelectFile} />
            ) : (
              <ProseSidebar branch={activeBranch} path={selectedFile} />
            )}
          </div>
        )}
      </div>
    </div>
  );
}
