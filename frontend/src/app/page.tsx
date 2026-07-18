"use client";

import { useCallback, useEffect, useMemo, useState } from "react";
import { PanelLeftClose, PanelLeftOpen, PanelRightClose, PanelRightOpen, Eye, EyeOff, Trash2, Users, ListTree, Combine, Split, FileCode, Pilcrow, BookMarked } from "lucide-react";
import { useServerCache } from "@/lib/serverCacheStore";
import { useUI } from "@/lib/uiStore";
import { connect, createBranch, deleteBranch, selectBranch, uploadFiles, uploadImageToTimeline, createChapter, importCharacterCard } from "./sidebar.actions";
import {
  openFile, createFile, deleteFile, renameFile, checkpointFile, closeFile, enterScene, leaveScene, askCharacter,
  appendToFile, editAtom, editPrompt, deleteAtom, mergeSelected, splitSelected,
  hideSelected, unhideSelected,
  chatWrite, roleplayWrite, chatFix, chatNote, chatRegen, chatOutline,
  chatConverse, chatConverseRegen, cycleSwipe, correctAtom, summarizeThisFile,
  openSummaryTier, closeSummaryTier, editSummaryAtom, appendSummary, cycleSummarySwipe,
} from "./fileview.actions";
import {
  openCharacter, closeCharacter, openJournal, closeJournal,
  editJournalAtom, deleteJournalAtom, journalFix, appendJournal, cycleJournalSwipe,
} from "./character-sidebar.actions";
import { trackJournal, trackAllJournals } from "./tracker.actions";
import { syncTasks, suggestTasks } from "./tasks-panel.actions";
import { addNote, moveTick, deleteTickEntry } from "./ticksview.actions";
import { tickChain, statusColor, presentDuringAtoms, allPresentCharacters, characterColor, type AnnotationMode } from "@/lib/utils";
import { LeftSidebar } from "./sidebar";
import { WireTickList, AgentLogStrip, ChatPreviewStrip, InputBar, RawEditPanel, TextEditPanel, SummaryTierPanel, type PresenceBar } from "./fileview";
import { summaryKindsFor } from "@/lib/library";
import { ChatView } from "./chatview";
import { TicksView } from "./ticksview";
import { CharacterSidebar } from "./character-sidebar";
import { CodexTab } from "./codex";
import { AgentsTab } from "./agentstab";
import { isOutlineFile, isChatFile } from "@/lib/agents";
import { UndoTimeline } from "./undo-timeline";

// Display label for a summarizer kind — see lib/library.ts's
// summaryKindsFor for which kinds a given file can offer. journal.md's own
// kinds are "journal/L0", "journal/L1", ... (see
// Storyteller.Writer.Agent.JournalSummarizer.journalKindFor) — tier 0 reads
// as "Chunks", every tier above it as "Level N" (rarely reached in
// practice; the tree only grows a level once the one below it has produced
// a full group's worth of its own output).
function summaryKindLabel(kind: string): string {
  switch (kind) {
    case "prose/chapter": return "Chapter summary";
    case "lore/article":  return "Article summary";
    case "journal/L0":    return "Chunks";
    default: {
      const m = /^journal\/L(\d+)$/.exec(kind);
      return m ? `Level ${m[1]}` : kind;
    }
  }
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

function Toolbar({ leftOpen, onToggleLeft, rightOpen, onToggleRight, rightAvailable, selectedFile, onCloseFile, centerTab, onCenterTab }: {
  leftOpen: boolean;
  onToggleLeft: () => void;
  rightOpen: boolean;
  onToggleRight: () => void;
  rightAvailable: boolean;
  selectedFile: string | null;
  onCloseFile: () => void;
  centerTab: "file" | "ticks" | "chat" | "agents";
  onCenterTab: (t: "file" | "ticks" | "chat" | "agents") => void;
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

      {selectedFile && isChatFile(selectedFile) && (
        <button onClick={() => onCenterTab("chat")} style={{
          padding: "0 10px", fontSize: 11, fontWeight: 500,
          border: "none", borderBottom: centerTab === "chat" ? "2px solid var(--amber)" : "2px solid transparent",
          borderTop: "2px solid transparent", background: "transparent",
          color: centerTab === "chat" ? "var(--amber)" : "var(--text-disabled)",
          cursor: "pointer", transition: "color 0.15s, border-color 0.15s",
        }}>Chat</button>
      )}

      {selectedFile && (
        <button onClick={() => onCenterTab("agents")} title="Configure the agents available for this file: their context slots and prompt overrides"
          style={{
          padding: "0 10px", fontSize: 11, fontWeight: 500,
          border: "none", borderBottom: centerTab === "agents" ? "2px solid var(--amber)" : "2px solid transparent",
          borderTop: "2px solid transparent", background: "transparent",
          color: centerTab === "agents" ? "var(--amber)" : "var(--text-disabled)",
          cursor: "pointer", transition: "color 0.15s, border-color 0.15s",
        }}>Agents</button>
      )}

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
  const openCharacters    = useServerCache((s) => s.openCharacters);
  const openJournals      = useServerCache((s) => s.openJournals);
  const openSummaries     = useServerCache((s) => s.openSummaries);
  // 'preview' (the in-flight streamed draft) is deliberately not read here —
  // it can update several times a second, and this component owns the whole
  // page tree, so subscribing here would reconcile all of it on every token.
  // 'ChatPreviewStrip'/'InputBar'/'ChatView' each subscribe to it directly
  // instead, confining that reconciliation to just themselves.

  const conns               = useUI((s) => s.conns);
  const error               = useUI((s) => s.error);
  const journalMarkers      = useUI((s) => s.journalMarkers);
  const agentLogs           = useUI((s) => s.agentLogs);
  const characterAnswers    = useUI((s) => s.characterAnswers);
  const contextAtoms        = useUI((s) => s.contextAtoms);
  const contextAnnotations  = useUI((s) => s.contextAnnotations);
  const rebaseMarker        = useUI((s) => s.rebaseMarker);
  const hoverHighlight      = useUI((s) => s.hoverHighlight);
  const setJournalMarker       = useUI((s) => s.setJournalMarker);
  const setHoverHighlight      = useUI((s) => s.setHoverHighlight);
  const clearHoverHighlight    = useUI((s) => s.clearHoverHighlight);
  const toggleContextAtom       = useUI((s) => s.toggleContextAtom);
  const toggleContextAnnotation = useUI((s) => s.toggleContextAnnotation);
  const clearContext            = useUI((s) => s.clearContext);
  const clearAgentLogs          = useUI((s) => s.clearAgentLogs);
  const setRebaseMarker         = useUI((s) => s.setRebaseMarker);

  const [annotationMode, setAnnotationMode] = useState<AnnotationMode>("expanded");
  const [showAllPresence, setShowAllPresence] = useState(false);
  const [leftOpen, setLeftOpen] = useState(true);
  const [leftWidth, setLeftWidth] = useState(260);
  const [isResizing, setIsResizing] = useState(false);
  const [rightOpen, setRightOpen] = useState(true);
  const [rightWidth, setRightWidth] = useState(260);
  const [isResizingRight, setIsResizingRight] = useState(false);
  const [sidebarTab, setSidebarTab] = useState<"explorer" | "branches" | "characters" | "library">("branches");
  // Which of the right panel's two switchable views is showing — scene
  // presence (the panel's original, and still default, content) or the
  // codex card grid (see codex.tsx). A plain local switch for now, same
  // as LeftSidebar's own tab strip, not yet promoted to a shared type.
  const [rightTab, setRightTab] = useState<"characters" | "codex">("characters");
  const [hoveredCharacter, setHoveredCharacter] = useState<string | null>(null);
  const [centerTab, setCenterTab] = useState<"file" | "ticks" | "chat" | "agents">("file");
  const [selectedFile, setSelectedFile] = useState<string | null>(null);
  const [viewMode, setViewMode] = useState<"blocks" | "text" | "source">("blocks");
  // null = raw (whatever viewMode shows); otherwise the summarizer kind
  // currently displayed instead, via SummaryTierPanel — its own connection
  // (openSummaries) is opened/closed by the effect below as this changes.
  const [summaryKind, setSummaryKind] = useState<string | null>(null);

  const sessionStatus = conns.find((c) => c.label === "session")?.status ?? "disconnected";

  // Same "reserved literal ahead of a name that could be anything" shape as
  // the backend's own /branch/{name}/file/{path} split (see app/Server.hs) —
  // a branch could otherwise be named the same as some future top-level
  // route, and a file within it the same as some future reserved segment
  // under a branch, so both "branch" and "file" are fixed prefixes a real
  // name can never coincide with, rather than the name sitting bare at
  // whatever position happens to be unclaimed today.
  function parsePath(pathname: string): { branch: string | null; file: string | null } {
    const parts = pathname.replace(/^\//, "").split("/").filter(Boolean);
    if (parts.length < 2 || parts[0] !== "branch") return { branch: null, file: null };
    return {
      branch: decodeURIComponent(parts[1]),
      file: parts.length > 3 && parts[2] === "file" ? parts.slice(3).map(decodeURIComponent).join("/") : null,
    };
  }

  function pushPath(branch: string | null, file: string | null) {
    if (!branch) { history.pushState(null, "", "/"); return; }
    const encodedFile = file ? "/file/" + file.split("/").map(encodeURIComponent).join("/") : "";
    history.pushState(null, "", `/branch/${encodeURIComponent(branch)}${encodedFile}`);
  }

  // eslint-disable-next-line react-hooks/exhaustive-deps
  useEffect(() => {
    const { branch, file } = parsePath(window.location.pathname);
    connect().then(() => {
      if (branch) {
        selectBranch(branch).then(() => {
          if (file) { setSelectedFile(file); openFile(file); setCenterTab(isChatFile(file) ? "chat" : "file"); }
          else setCenterTab("ticks");
        });
        setSidebarTab("explorer");
      }
    });

    const onPopState = () => {
      const { branch: b, file: f } = parsePath(window.location.pathname);
      if (b) { selectBranch(b); setSidebarTab("explorer"); }
      setSelectedFile(f);
      if (f) { openFile(f); setCenterTab(isChatFile(f) ? "chat" : "file"); }
      else setCenterTab("ticks");
    };
    window.addEventListener("popstate", onPopState);
    return () => window.removeEventListener("popstate", onPopState);
  }, []);

  // The Text/Source view mode is per-file, ephemeral UI state — never carry
  // it over to whatever gets selected next (a stale unsaved buffer for a
  // different path would be actively misleading). Same for which summary
  // tier (if any) is being shown.
  useEffect(() => { setViewMode("blocks"); setSummaryKind(null); }, [selectedFile]);

  // A summary tier's own connection — opened only while its tab is
  // actually selected, closed the moment it isn't (a different tab, a
  // different file, or navigating away entirely) rather than kept alive
  // speculatively. See fileview.actions.ts's openSummaryTier.
  useEffect(() => {
    if (!selectedFile || !summaryKind) return;
    openSummaryTier(selectedFile, summaryKind);
    return () => closeSummaryTier(selectedFile, summaryKind);
  }, [selectedFile, summaryKind]);

  const handleEditAtom = useCallback((tickId: string, content: string) => {
    if (selectedFile) editAtom(selectedFile, tickId, content);
  }, [selectedFile, editAtom]);

  const fileConn = selectedFile ? openFiles[selectedFile] : null;
  const fileChainTicks = fileConn?.ticks ?? {};
  const fileChainHead  = fileConn?.head ?? null;
  const isAbsent = fileConn?.absent ?? false;
  // tickChain walks the whole chain and allocates a fresh reversed array —
  // memoized on the conn's own ticks/head so an unrelated re-render of this
  // component (a different file's WS traffic, a sidebar hover, etc.) doesn't
  // redo that walk for no reason.
  const fileTicks = useMemo(
    () => tickChain(fileChainTicks, fileChainHead),
    [fileChainTicks, fileChainHead],
  );
  const atomCount       = useMemo(() => fileTicks.filter((t) => t.kind === "atom").length, [fileTicks]);
  const annotationCount = useMemo(() => fileTicks.filter((t) => t.kind !== "atom").length, [fileTicks]);
  // Synthetic "summary" ticks (see Server.Writer.File.summaryTicksFor)
  // never sit on the chain 'tickChain' walks from head — they carry no
  // parent — so they're read straight off the raw per-connection map
  // instead of out of fileTicks.
  const summaryTicks = useMemo(
    () => Object.values(fileChainTicks).filter((t) => t.kind === "summary"),
    [fileChainTicks],
  );
  // Every tier that already has a tick, plus exactly one more -- the next
  // kind in line that doesn't yet ("stop at the first gap", the same rule
  // Storyteller.Writer.Agent.SummaryAccess.zoomLevels itself uses) -- so a
  // never-summarized file still offers one tab to manually create its
  // first tier, without listing all twelve of journal.md's generous
  // L0..L11 candidates up front.
  const availableSummaryKinds = useMemo(() => {
    if (!selectedFile) return [];
    const allKinds = summaryKindsFor(selectedFile);
    const existing = allKinds.filter((k) => summaryTicks.some((t) => t.fields?.kind === k));
    const nextNew = allKinds.find((k) => !existing.includes(k));
    return nextNew ? [...existing, nextNew] : existing;
  }, [selectedFile, summaryTicks]);
  // The currently-selected tier's own connection (see the effect above
  // that opens/closes it) — a plain second FileConn, not a special case;
  // 'summaryTicksChain' walks it exactly the way 'fileTicks' walks the
  // real file's own.
  const summaryConn = selectedFile && summaryKind ? openSummaries[`${selectedFile}::${summaryKind}`] : undefined;
  const summaryTicksChain = useMemo(
    () => tickChain(summaryConn?.ticks ?? {}, summaryConn?.head ?? null),
    [summaryConn],
  );
  // Shared by the toolbar's "Summarize" button and the "/summarize" input
  // command (see lib/commands.ts). Scoped to exactly this file (see
  // Server.Writer.File.summarizePath's own Haddock: never regenerates
  // some other stale file just because it shares a kind) — journal.md's
  // own case still drives its whole recursive tier tree in one call
  // (Storyteller.Writer.Agent.JournalSummarizer.journalSummarize), it's
  // just dispatched per-file now like everything else.
  const summarizeCurrentFile = useCallback(() => {
    if (!selectedFile || !activeBranch) return;
    if (summaryKindsFor(selectedFile).length === 0) return;
    summarizeThisFile(selectedFile);
  }, [selectedFile, activeBranch]);
  // Presence is scoped to the open file (a scene), not the whole branch —
  // see WRITER.md — so this folds the file's own chain, not the branch-wide
  // one. "Show all" (persistent, toolbar toggle) wins over hover —
  // first-appearance order both here and in 'allPresentCharacters' itself,
  // so lane 0 (closest to the text) is always whoever entered first, per
  // the same ordering 'activeCharacterBranches' already uses for the sidebar.
  const presenceBars: PresenceBar[] = useMemo(() => {
    const bars: PresenceBar[] = showAllPresence
      ? allPresentCharacters(fileChainTicks, fileChainHead).map((c) => ({
          character: c, color: characterColor(c), tickIds: presentDuringAtoms(fileChainTicks, fileChainHead, c),
        }))
      : hoveredCharacter
      ? [{ character: hoveredCharacter, color: characterColor(hoveredCharacter), tickIds: presentDuringAtoms(fileChainTicks, fileChainHead, hoveredCharacter) }]
      : [];
    // Global cross-component highlight (e.g. hovering a journal entry in the
    // character sidebar) — folded into the same bar mechanism as an extra
    // lane, since it's the same shape (tickIds + color -> a colored run).
    if (hoverHighlight) bars.push({ character: "__hover__", color: hoverHighlight.color, tickIds: hoverHighlight.tickIds });
    return bars;
  }, [showAllPresence, hoveredCharacter, hoverHighlight, fileChainTicks, fileChainHead]);

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
    setSelectedFile(path);
    openFile(path);
    setCenterTab(isChatFile(path) ? "chat" : "file");
    pushPath(activeBranch, path);
  }

  function handleCreateFile(path: string) {
    if (selectedFile && selectedFile !== path) closeFile(selectedFile);
    setSelectedFile(path);
    createFile(path);
    setCenterTab(isChatFile(path) ? "chat" : "file");
    pushPath(activeBranch, path);
  }

  function handleDeleteFile(path: string) {
    deleteFile(path);
    closeFile(path);
    if (selectedFile === path) setSelectedFile(null);
    pushPath(activeBranch, null);
  }

  // Covers both the rename UI action and drag-to-move (dropping a file
  // onto a folder in the tree) — a move is just a rename to a path under
  // the target folder, same server command either way.
  function handleRenameFile(path: string, newPath: string) {
    renameFile(path, newPath);
    closeFile(path);
    if (selectedFile === path) {
      setSelectedFile(newPath);
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
    setSelectedFile(null);
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
      setSelectedFile(path);
      openFile(path);
      setCenterTab("file");
      pushPath("prompts", path);
    } else {
      selectBranch("prompts").then(() => {
        setSelectedFile(path);
        openFile(path);
        setCenterTab("file");
        pushPath("prompts", path);
      });
    }
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
      if (!jc) continue;
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
      if (!jc) continue;
      const targets = tickChain(jc.ticks, jc.head)
        .filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
        .map((t) => t.tickId);
      if (targets.length > 0) journalFix(branch, text, targets, journalMarkers[branch] ?? null);
    }
  }

  const handleCorrect = useCallback((tickId: string) => {
    if (selectedFile) correctAtom(selectedFile, tickId);
  }, [selectedFile]);

  const handleCycleSwipe = useCallback((tickId: string) => {
    if (selectedFile) cycleSwipe(selectedFile, tickId);
  }, [selectedFile]);

  const handleEditPrompt = useCallback((tickId: string, content: string) => {
    if (selectedFile) editPrompt(selectedFile, tickId, content);
  }, [selectedFile]);

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
            rightAvailable={centerTab === "file"}
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
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--rose-tint)", border: "1px solid var(--rose-border)", color: "var(--rose)" }}
                  >
                    <Trash2 style={{ width: 10, height: 10 }} />
                    Delete {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 2 && (
                  <button
                    onClick={() => mergeSelected(selectedFile)}
                    title={`Merge ${contextAtoms.size} selected atoms into one`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <Combine style={{ width: 10, height: 10 }} />
                    Merge {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 1 && (
                  <button
                    onClick={() => splitSelected(selectedFile)}
                    title={`Re-split ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""} at paragraph/heading boundaries`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <Split style={{ width: 10, height: 10 }} />
                    Split {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 1 && (
                  <button
                    onClick={() => hideSelected(selectedFile)}
                    title={`Hide ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""} from an agent's context (stays in the file)`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <EyeOff style={{ width: 10, height: 10 }} />
                    Hide {contextAtoms.size}
                  </button>
                )}
                {contextAtoms.size >= 1 && (
                  <button
                    onClick={() => unhideSelected(selectedFile)}
                    title={`Unhide ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""}`}
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
                  >
                    <Eye style={{ width: 10, height: 10 }} />
                    Unhide {contextAtoms.size}
                  </button>
                )}
                {selectedFile && isOutlineFile(selectedFile) && (
                  <button
                    onClick={() => chatOutline(selectedFile)}
                    title="Generate a per-chapter beat sheet for each chapter this outline implies"
                    style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: contextAtoms.size > 0 ? 6 : 0 }}
                  >
                    <ListTree style={{ width: 10, height: 10 }} />
                    Generate beat sheets
                  </button>
                )}
                {selectedFile && activeBranch && summaryKindsFor(selectedFile).length > 0 && <>
                  {availableSummaryKinds.map((kind, i) => (
                    <button
                      key={kind}
                      onClick={() => setSummaryKind(summaryKind === kind ? null : kind)}
                      title={
                        summaryKind === kind
                          ? "Back to raw — the live, editable content"
                          : `${summaryKindLabel(kind)} — editable, from the last summarize pass (or manually written)`
                      }
                      style={{ fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", marginLeft: i === 0 ? 8 : 4, background: summaryKind === kind ? "var(--amber-tint)" : "transparent", border: "1px solid " + (summaryKind === kind ? "var(--amber-border)" : "var(--border-subtle)"), color: summaryKind === kind ? "var(--amber)" : "var(--text-dim)" }}
                    >
                      {summaryKindLabel(kind)}
                    </button>
                  ))}
                  <button
                    onClick={summarizeCurrentFile}
                    title="Run a summarize pass — free to call, only does LLM work once there's actually something new to compress (also available as /summarize)"
                    style={{ fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", marginLeft: 4, background: "transparent", border: "1px solid var(--border-subtle)", color: "var(--text-dim)" }}
                  >
                    Summarize
                  </button>
                </>}
                <span style={{ flex: 1 }} />
                <span style={{ fontSize: 9, color: "var(--text-ghost)", marginRight: 8 }}>
                  {atomCount} atom{atomCount !== 1 ? "s" : ""}
                  {annotationCount > 0 && <> · {annotationCount} ann</>}
                </span>
                <button
                  onClick={() => setAnnotationMode((m) => m === "hidden" ? "dots" : m === "dots" ? "expanded" : "hidden")}
                  title={annotationMode === "hidden" ? "Show annotation dots" : annotationMode === "dots" ? "Expand annotations" : "Hide annotations"}
                  style={{ width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: annotationMode !== "hidden" ? "var(--amber-tint)" : "transparent", color: annotationMode === "expanded" ? "var(--amber)" : annotationMode === "dots" ? "var(--amber-muted)" : "var(--text-dim)" }}
                >
                  {annotationMode === "hidden" ? <EyeOff style={{ width: 11, height: 11 }} /> : <Eye style={{ width: 11, height: 11, opacity: annotationMode === "dots" ? 0.6 : 1 }} />}
                </button>
                <button
                  onClick={() => setShowAllPresence((v) => !v)}
                  title={showAllPresence ? "Hide character presence bars" : "Show character presence bars"}
                  style={{ width: 22, height: 22, marginLeft: 2, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: showAllPresence ? "var(--sky-tint)" : "transparent", color: showAllPresence ? "var(--sky)" : "var(--text-dim)" }}
                >
                  <Users style={{ width: 11, height: 11 }} />
                </button>
                <button
                  onClick={() => setViewMode("blocks")}
                  title="Blocks — atom/outliner view"
                  style={{ width: 22, height: 22, marginLeft: 2, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: viewMode === "blocks" ? "var(--amber-tint)" : "transparent", color: viewMode === "blocks" ? "var(--amber)" : "var(--text-dim)" }}
                >
                  <ListTree style={{ width: 11, height: 11 }} />
                </button>
                <button
                  onClick={() => setViewMode("text")}
                  title="Text — WYSIWYG markdown editor for the whole file"
                  style={{ width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: viewMode === "text" ? "var(--amber-tint)" : "transparent", color: viewMode === "text" ? "var(--amber)" : "var(--text-dim)" }}
                >
                  <Pilcrow style={{ width: 11, height: 11 }} />
                </button>
                <button
                  onClick={() => setViewMode("source")}
                  title="Source — edit the whole file as raw markdown"
                  style={{ width: 22, height: 22, display: "flex", alignItems: "center", justifyContent: "center", borderRadius: 4, cursor: "pointer", border: "none", background: viewMode === "source" ? "var(--amber-tint)" : "transparent", color: viewMode === "source" ? "var(--amber)" : "var(--text-dim)" }}
                >
                  <FileCode style={{ width: 11, height: 11 }} />
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
            ) : summaryKind !== null ? (
              summaryConn ? (
                <SummaryTierPanel
                  branch={activeBranch} kind={summaryKind}
                  ticks={summaryTicksChain} absent={summaryConn.absent}
                  onEdit={(tickId, content) => editSummaryAtom(selectedFile, summaryKind, tickId, content)}
                  onAppend={(text) => appendSummary(selectedFile, summaryKind, text)}
                  onCycleSwipe={(tickId) => cycleSummarySwipe(selectedFile, summaryKind, tickId)}
                />
              ) : (
                <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
                  Loading…
                </div>
              )
            ) : viewMode === "source" ? (
              <RawEditPanel branch={activeBranch} path={selectedFile} />
            ) : viewMode === "text" ? (
              <TextEditPanel branch={activeBranch} path={selectedFile} />
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
                onCycleSwipe={handleCycleSwipe}
                onCorrect={handleCorrect}
                onEditPrompt={handleEditPrompt}
                activeBranch={activeBranch}
                targetFile={selectedFile}
                onUploadImages={uploadImageToTimeline}
              />
            )}

            {viewMode === "blocks" && <>
              <ChatPreviewStrip />
              <AgentLogStrip logs={agentLogs} onClear={clearAgentLogs} />
              <InputBar
                enabled={selectedFile !== null}
                activeBranch={activeBranch}
                contextAtomCount={contextAtoms.size} contextAnnotationCount={contextAnnotations.size}
                rebasing={rebaseMarker !== null}
                onClearRebase={() => setRebaseMarker(null)}
                onClearContext={clearContext}
                onAppend={(text) => selectedFile && appendToFile(selectedFile, text)}
                onWrite={(text)  => selectedFile && chatWrite(selectedFile, text)}
                onFix={handleFix}
                onNote={(text)   => selectedFile && chatNote(selectedFile, text)}
                onRegen={(text, byBeat) => selectedFile && chatRegen(selectedFile, text, byBeat)}
                onRoleplay={(text) => selectedFile && roleplayWrite(selectedFile, text)}
                onAsk={(character, question) => selectedFile && askCharacter(selectedFile, character, question)}
                onInform={(character, fact) => appendJournal(character, fact, journalMarkers[character] ?? null)}
                onSummarize={summarizeCurrentFile}
              />
            </>}
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
            <AgentsTab activeBranch={activeBranch} path={selectedFile} onJumpToPrompt={handleJumpToPrompt} />
          )}
        </div>

        {rightOpen && centerTab === "file" && (
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
            <div style={{ flexShrink: 0, padding: "8px 8px 0" }}>
              <div style={{
                display: "grid", gridTemplateColumns: "1fr 1fr", gap: 1,
                background: "var(--surface)", borderRadius: 6, padding: 2,
              }}>
                {(["characters", "codex"] as const).map((t) => (
                  <button key={t} onClick={() => setRightTab(t)} style={{
                    height: 26, display: "flex", alignItems: "center", justifyContent: "center",
                    gap: 5, fontSize: 11, borderRadius: 4, border: "none", cursor: "pointer",
                    background: rightTab === t ? "var(--surface-raised)" : "transparent",
                    color: rightTab === t ? "var(--amber)" : "var(--text-disabled)",
                    transition: "background 0.15s, color 0.15s",
                  }}>
                    {t === "characters" ? <Users style={{ width: 12, height: 12 }} /> : <BookMarked style={{ width: 12, height: 12 }} />}
                    {t === "characters" ? "Characters" : "Codex"}
                  </button>
                ))}
              </div>
              <div style={{ height: 8 }} />
            </div>

            <div style={{ flex: 1, minHeight: 0, display: "flex", flexDirection: "column" }}>
              {rightTab === "characters" ? (
                <CharacterSidebar
                  selectedFile={selectedFile}
                  characterBranches={characterBranches}
                  ticks={fileChainTicks} head={fileChainHead} rebaseMarker={rebaseMarker}
                  openCharacters={openCharacters}
                  openCharacter={openCharacter} closeCharacter={closeCharacter}
                  openJournals={openJournals}
                  openJournal={openJournal} closeJournal={closeJournal}
                  journalMarkers={journalMarkers} setJournalMarker={setJournalMarker}
                  trackJournal={trackJournal}
                  onTrackAll={() => trackAllJournals(characterBranches.map((c) => c.branch))}
                  syncTasks={syncTasks} suggestTasks={suggestTasks}
                  editJournalAtom={editJournalAtom} cycleJournalSwipe={cycleJournalSwipe} appendJournal={appendJournal}
                  contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
                  toggleContextAtom={toggleContextAtom} toggleContextAnnotation={toggleContextAnnotation}
                  onHoverAtoms={setHoverHighlight} onHoverEnd={clearHoverHighlight}
                  enterScene={enterScene} leaveScene={leaveScene}
                  askCharacter={askCharacter} characterAnswers={characterAnswers}
                />
              ) : (
                <CodexTab activeBranch={activeBranch} />
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
