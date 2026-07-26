"use client";

// The prose surface: the atom view and everything that only makes sense
// alongside it (see lib/fileSurface.ts for what a surface is).
//
// This is one of three peers, not "the file view with extras". Everything
// here is downstream of one fact — a prose file is a chain of atoms an
// agent writes into — and that's why all of it lives together: selecting
// and merging/splitting/hiding atoms, annotations, who's present in the
// scene, summary tiers, the input bar, and the sidebar's own three tabs.
// A surface with no atoms and no agent writing into it (a DSL program, a
// prompt override) doesn't switch these off; it never had them.
//
// Two exports, and page.tsx's own file area is exactly "pick a surface,
// render its view and its sidebar":
//
//   ProseFileView    — the centre pane (toolbar strip + blocks/text/source,
//                      or the summary split view).
//   ProseSidebar     — the right panel's *content* (Characters/Codex/Cost).
//                      page.tsx owns only the panel's width and drag
//                      handle; what's inside belongs to the surface.
//
// Both take the file's identity and read everything else — chain state,
// selection, markers — from the stores directly, the same way any other
// component in this app does. That's what makes them peers rather than
// props-forwarding shims: page.tsx no longer holds prose-only state
// (which view mode, which annotation mode, which sidebar tab) at all.

import { useCallback, useEffect, useMemo, useState } from "react";
import { Eye, EyeOff, Trash2, Users, ListTree, Combine, Split, FileCode, Pilcrow, BookMarked, Gauge } from "lucide-react";
import { useServerCache } from "@/lib/serverCacheStore";
import { useUI } from "@/lib/uiStore";
import {
  openFile, closeFile, enterScene, leaveScene, askCharacter,
  appendToFile, editAtom, editPrompt, deleteTicks, mergeSelected, splitSelected,
  hideSelected, unhideSelected,
  chatWrite, roleplayWrite, chatFix, chatNote, chatRegen, chatOutline,
  cycleSwipe, correctAtom, summarizeThisFile, createSummaryManual,
  summaryConnBranch, summaryConnKey,
} from "./fileview.actions";
import {
  openCharacter, closeCharacter, openJournal, closeJournal,
  editJournalAtom, deleteJournalTicks, journalFix, appendJournal, cycleJournalSwipe,
} from "./character-sidebar.actions";
import { trackJournal, trackAllJournals } from "./tracker.actions";
import { syncTasks, suggestTasks } from "./tasks-panel.actions";
import { uploadImageToTimeline } from "./sidebar.actions";
import { tickChain, presentDuringAtoms, allPresentCharacters, characterColor, summaryCoverageFor, type AnnotationMode } from "@/lib/utils";
import { FileContentView, SummarySplitView, RawEditPanel, TextEditPanel, SummarizeMenu, type PresenceBar } from "./fileview";
import { summaryKindsFor } from "@/lib/library";
import { CharacterSidebar } from "./character-sidebar";
import { CodexTab } from "./codex";
import { ContextCostSidebar } from "./context-cost-sidebar";
import { isOutlineFile } from "@/lib/agents";

export interface ViewedSummary { kind: string; hops: string[] }

interface ProseFileViewProps {
  branch: string;
  path: string;
  // Which summary tier (if any) is open — page.tsx's own 'viewTarget',
  // since it's URL state (deep-linkable) rather than view state.
  viewingSummary: ViewedSummary | null;
  onNavigateToSummary: (kind: string, hops: string[]) => void;
  onCloseSummary: () => void;
  // Hovering a character in the left sidebar draws that character's
  // presence bar here — the one piece of cross-panel state that isn't in a
  // store.
  hoveredCharacter: string | null;
}

export function ProseFileView({
  branch, path, viewingSummary, onNavigateToSummary, onCloseSummary, hoveredCharacter,
}: ProseFileViewProps) {
  const openFiles    = useServerCache((s) => s.openFiles);
  const openJournals = useServerCache((s) => s.openJournals);

  const contextAtoms        = useUI((s) => s.contextAtoms);
  const contextAnnotations  = useUI((s) => s.contextAnnotations);
  const rebaseMarker        = useUI((s) => s.rebaseMarker);
  const hoverHighlight      = useUI((s) => s.hoverHighlight);
  const journalMarkers      = useUI((s) => s.journalMarkers);
  const agentLogs           = useUI((s) => s.agentLogs);
  const clearAgentLogs      = useUI((s) => s.clearAgentLogs);
  const setRebaseMarker     = useUI((s) => s.setRebaseMarker);
  const clearContext        = useUI((s) => s.clearContext);
  const toggleContextAtom       = useUI((s) => s.toggleContextAtom);
  const toggleContextAnnotation = useUI((s) => s.toggleContextAnnotation);

  // Prose-view display state — owned here, not by the page: which of the
  // three view modes, how annotations are shown, whether every character's
  // presence bar is pinned. None of it means anything to another surface.
  const [viewMode, setViewMode] = useState<"blocks" | "text" | "source">("blocks");
  const [annotationMode, setAnnotationMode] = useState<AnnotationMode>("expanded");
  const [showAllPresence, setShowAllPresence] = useState(false);
  const [showFullSummaryChain, setShowFullSummaryChain] = useState(false);

  // The Text/Source view mode is per-file, ephemeral UI state — never carry
  // it over to whatever gets selected next (a stale unsaved buffer for a
  // different path would be actively misleading).
  useEffect(() => { setViewMode("blocks"); }, [path]);
  // Same reasoning — a fresh occurrence/kind/hop starts back at the
  // default "just this pass's own delta" view.
  useEffect(() => { setShowFullSummaryChain(false); }, [viewingSummary]);

  const fileConn = openFiles[path] ?? null;
  const fileChainTicks = fileConn?.ticks ?? {};
  const fileChainHead  = fileConn?.head ?? null;
  const isAbsent = fileConn?.absent ?? false;
  // tickChain walks the whole chain and allocates a fresh reversed array —
  // memoized on the conn's own ticks/head so an unrelated re-render (a
  // different file's WS traffic, a sidebar hover) doesn't redo that walk.
  const fileTicks = useMemo(
    () => tickChain(fileChainTicks, fileChainHead),
    [fileChainTicks, fileChainHead],
  );
  const atomCount = useMemo(() => fileTicks.filter((t) => t.kind === "atom").length, [fileTicks]);
  // "summary" ticks (see Server.Writer.File.summaryTicksFor) are always
  // 'wtParent: null' on the wire, at any depth — 'Storage.Tick.fileTicksOf'/
  // 'relatedTicksOf' (which every connection's own chain goes through,
  // real branch or alt-chain alike) already relinks *around* any tick with
  // no real file footprint, so a summary tick's own actual git parent is
  // never a valid position in that relinked view — 'tickChain' correctly
  // never finds it via '.parent', so it's read straight off the raw
  // per-connection map instead of out of 'fileTicks', and positioned by
  // its own 'wtRefs' anchor against the atoms already present, exactly
  // like 'annotationsFor' (fileview.tsx) already positions a note.
  const summaryTicks = useMemo(
    () => Object.values(fileChainTicks).filter((t) => t.kind === "summary"),
    [fileChainTicks],
  );
  const annotationCount = useMemo(
    () => fileTicks.filter((t) => t.kind !== "atom").length + summaryTicks.length,
    [fileTicks, summaryTicks],
  );

  // The viewed occurrence's *parent* scope — the chain its own boundaries
  // (anchor/lowerBound/prevAltHead) index into. An occurrence's boundaries
  // are always positions in the chain it was pushed on, which is the scope
  // one hop above it, at any depth: for a single hop that's the real file
  // itself (open throughout anyway); deeper, it's the summary connection
  // one hop shorter, kept open alongside by the connection effect below.
  // One uniform rule — no depth special case anywhere downstream.
  const parentKey = viewingSummary && viewingSummary.hops.length >= 2
    ? summaryConnKey(path, viewingSummary.kind, viewingSummary.hops.slice(0, -1))
    : path;
  const parentConn = openFiles[parentKey] ?? null;
  const parentTicks = useMemo(
    () => (parentKey === path ? fileTicks : tickChain(parentConn?.ticks ?? {}, parentConn?.head ?? null)),
    [parentKey, path, fileTicks, parentConn],
  );
  // The exact summary occurrence the split view's top (read-only coverage)
  // pane slices its "what informed this" excerpt against — the last hop,
  // looked up directly in its parent scope's own tick map (occurrence
  // ticks ride along on every connection's push, at any depth). With no
  // hop at all (the family live view — where the Summarize button lands,
  // since the pass it fires hasn't produced its occurrence yet), the
  // kind's newest occurrence stands in: occurrences arrive oldest-first
  // per kind and applyFileUpdate re-adds them in push order, so the last
  // matching entry is the newest. Resolved at display time, so the view
  // upgrades itself the moment the freshly-fired pass's push lands — no
  // navigation event needed. The kind check only guards a hand-typed/stale
  // URL hop that names some non-summary tick.
  const lastHop = viewingSummary?.hops.at(-1);
  const occurrenceTick = !viewingSummary ? undefined
    : lastHop ? parentConn?.ticks[lastHop]
    : Object.values(parentConn?.ticks ?? {})
        .filter((t) => t.kind === "summary" && t.fields?.kind === viewingSummary.kind)
        .at(-1);
  const viewingOccurrence = occurrenceTick?.kind === "summary" ? occurrenceTick : null;

  // A summary family's own connection — genuinely just another file
  // connection (see fileview.actions.ts's openFile), opened at
  // "{branch}@{kind}#hops" instead of the plain branch, under its own key
  // (see summaryConnBranch/summaryConnKey) so it never collides with the
  // real file's own entry, which stays open throughout (the split view's
  // top coverage pane reads it the whole time). Every hop chain — empty
  // (this family's current live state), one hop (a specific occurrence),
  // or several (nested tiers) — opens exactly the same way: there is no
  // read-only tier, no "only the latest is live" special case.
  useEffect(() => {
    if (!viewingSummary) return;
    const { kind, hops } = viewingSummary;
    // The viewed scope itself, plus — once nested — its parent scope, which
    // the coverage pane and the "this pass only" slice both read. At one
    // hop the parent is the real file's own connection, open throughout
    // anyway, so nothing extra is opened.
    const chains = hops.length >= 2 ? [hops, hops.slice(0, -1)] : [hops];
    const keys = chains.map((h) => summaryConnKey(path, kind, h));
    chains.forEach((h, i) => openFile(path, { branch: summaryConnBranch(branch, kind, h), key: keys[i] }));
    return () => keys.forEach((key) => closeFile(path, { key }));
  }, [path, viewingSummary, branch]);

  // 'activeKey' is whichever 'openFiles' entry the main content pane is
  // currently editing — the real file itself, or (while viewing a summary,
  // at any hop depth) its own tier connection. Every action below that
  // edits "whatever's currently on screen" (atom edits, correct/swipe/
  // prompt edits, and every InputBar action) is keyed off this, not the
  // path directly — since a summary tier is genuinely just another
  // 'openFiles' entry, no separate code path is needed for it anywhere.
  const summaryKey = viewingSummary
    ? summaryConnKey(path, viewingSummary.kind, viewingSummary.hops)
    : null;
  const activeKey = viewingSummary ? summaryKey : path;

  // The main view's own WireTickList needs summary ticks folded in
  // alongside the real chain so its annotation-anchoring logic picks them
  // up as inline annotations — 'fileTicks' alone never contains them (see
  // above). 'fileTicks' itself stays the pure real-atom chain for
  // 'summaryCoverageFor', which slices strictly by atom position.
  const mainViewTicks = useMemo(() => [...fileTicks, ...summaryTicks], [fileTicks, summaryTicks]);
  const activeConn = activeKey ? openFiles[activeKey] : null;
  // Same fold as 'mainViewTicks' above, and for the same reason: a nested
  // tier's own further Summary tick rides along on *this* connection's own
  // push with 'wtParent: null', so a plain tickChain walk drops it just as
  // it would for the real file — this is what makes a deeper nested
  // annotation actually show up inline in the bottom pane.
  //
  // Sliced down to just this occurrence's own delta when a specific one is
  // being viewed — the same "an atom's own data is just what it appended,
  // not the whole growing file" principle Storyteller.Common.Summary.
  // occurrenceDelta already applies server-side, applied here to the tick
  // *list*: fields.prevAltHead, when present, is the previous occurrence's
  // own alt-chain tip, so everything strictly after it in this
  // connection's own chain is what this pass actually added.
  //
  // 'nested' gets exactly the same "is this in the new part" test as an
  // atom, not a blanket exemption: a nested occurrence's own single ref
  // (its anchor) is a position within this same 'chain', so whether it
  // belongs to this delta is just "does its anchor fall after 'lowerIdx'".
  const activeTicksChain = useMemo(() => {
    if (!viewingSummary) return fileTicks;
    const chain = tickChain(activeConn?.ticks ?? {}, activeConn?.head ?? null);
    const nested = Object.values(activeConn?.ticks ?? {}).filter((t) => t.kind === "summary");
    if (!viewingOccurrence || showFullSummaryChain) return [...chain, ...nested];

    const prevAltHead = viewingOccurrence.fields?.prevAltHead;
    const chainIdx = new Map(chain.map((t, i) => [t.tickId, i]));
    const lowerIdx = prevAltHead ? chainIdx.get(prevAltHead) ?? -1 : -1;

    const newChain  = chain.slice(lowerIdx + 1);
    const newNested = nested.filter((t) => (chainIdx.get(t.refs[0]) ?? -1) > lowerIdx);
    return [...newChain, ...newNested];
  }, [viewingSummary, viewingOccurrence, activeConn, fileTicks, showFullSummaryChain]);

  // Presence is scoped to the open file (a scene), not the whole branch —
  // see WRITER.md — so this folds the file's own chain, not the branch-wide
  // one. "Show all" (persistent, toolbar toggle) wins over hover —
  // first-appearance order both here and in 'allPresentCharacters', so lane
  // 0 (closest to the text) is always whoever entered first.
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

  const handleEditAtom = useCallback((tickId: string, content: string) => {
    if (activeKey) editAtom(activeKey, tickId, content);
  }, [activeKey]);
  const handleCorrect = useCallback((tickId: string) => {
    if (activeKey) correctAtom(activeKey, tickId);
  }, [activeKey]);
  const handleCycleSwipe = useCallback((tickId: string) => {
    if (activeKey) cycleSwipe(activeKey, tickId);
  }, [activeKey]);
  const handleEditPrompt = useCallback((tickId: string, content: string) => {
    if (activeKey) editPrompt(activeKey, tickId, content);
  }, [activeKey]);

  // Shared by the toolbar's "Summarize" button and the "/summarize" input
  // command (see lib/commands.ts). Scoped to exactly this file (see
  // Server.Writer.File.summarizePath's own Haddock: never regenerates some
  // other stale file just because it shares a kind).
  const summarizeCurrentFile = useCallback(() => {
    const kinds = summaryKindsFor(path);
    if (kinds.length === 0) return;
    summarizeThisFile(path);
    // Land in this family's current live state (empty hops) — no specific
    // occurrence to point at yet, just fired the pass that creates one.
    onNavigateToSummary(kinds[0], []);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [path]);
  // "New" — the manual-creation sibling: same command, same coverage-finding
  // and rebase-marker positioning server-side, just no LLM call and empty
  // content, for writing into directly.
  const createManualSummary = useCallback(() => {
    const kinds = summaryKindsFor(path);
    if (kinds.length === 0) return;
    createSummaryManual(path);
    onNavigateToSummary(kinds[0], []);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [path]);

  // Selection (contextAtoms/contextAnnotations) is shared across the main
  // scene and every open journal (see character-sidebar.tsx) — deleting/
  // fixing "the selection" therefore has to sweep every chain that might
  // contain a selected id, not just the currently open scene file. A given
  // tickId only ever appears in the one chain it actually belongs to, so
  // this is just "check each open chain for members of the shared set,"
  // not a routing decision.
  //
  // Delete is the one bulk action universal across every tick kind, not
  // just atoms (unlike Merge/Split/Hide, which only ever make sense for
  // atoms/prose) — an annotation (note, prompt, summary occurrence) is a
  // real, deletable chain tick exactly like an atom is. Both sets, and
  // (for the main file) 'summaryTicks', feed one batched 'deleteTicks'
  // call per chain — the server sorts descendants-first internally. When
  // viewing a summary tier, its own connection is a separate 'openFiles'
  // entry and needs its own sweep — a selected tick living only in that
  // tier's chain would otherwise silently not get deleted.
  function handleDeleteSelected() {
    const selected = new Set([...contextAtoms, ...contextAnnotations]);
    const targets = [...fileTicks, ...summaryTicks]
      .filter((t) => selected.has(t.tickId))
      .map((t) => t.tickId);
    deleteTicks(path, targets);
    if (viewingSummary && activeKey && activeKey !== path) {
      const tierTargets = activeTicksChain
        .filter((t) => selected.has(t.tickId))
        .map((t) => t.tickId);
      deleteTicks(activeKey, tierTargets);
    }
    for (const [jBranch, jc] of Object.entries(openJournals)) {
      if (!jc) continue;
      const jTargets = tickChain(jc.ticks, jc.head)
        .filter((t) => selected.has(t.tickId))
        .map((t) => t.tickId);
      deleteJournalTicks(jBranch, jTargets, journalMarkers[jBranch] ?? null);
    }
    clearContext();
  }

  function handleFix(text: string) {
    if (activeKey) {
      const hasSelection = activeTicksChain.some((t) => t.kind === "atom" && contextAtoms.has(t.tickId));
      if (hasSelection) chatFix(activeKey, text);
    }
    for (const [jBranch, jc] of Object.entries(openJournals)) {
      if (!jc) continue;
      const targets = tickChain(jc.ticks, jc.head)
        .filter((t) => t.kind === "atom" && contextAtoms.has(t.tickId))
        .map((t) => t.tickId);
      if (targets.length > 0) journalFix(jBranch, text, targets, journalMarkers[jBranch] ?? null);
    }
  }

  const selectionCount = contextAtoms.size + contextAnnotations.size;

  return (
    <>
      {!isAbsent && fileTicks.length > 0 && (
        <div style={{ flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)", display: "flex", alignItems: "center" }}>
          {selectionCount > 0 && (
            <button
              onClick={handleDeleteSelected}
              title={`Delete ${selectionCount} selected tick${selectionCount !== 1 ? "s" : ""} — any kind: atoms, notes, prompts, summary occurrences`}
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--rose-tint)", border: "1px solid var(--rose-border)", color: "var(--rose)" }}
            >
              <Trash2 style={{ width: 10, height: 10 }} />
              Delete {selectionCount}
            </button>
          )}
          {contextAtoms.size >= 2 && (
            <button
              onClick={() => mergeSelected(path)}
              title={`Merge ${contextAtoms.size} selected atoms into one`}
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
            >
              <Combine style={{ width: 10, height: 10 }} />
              Merge {contextAtoms.size}
            </button>
          )}
          {contextAtoms.size >= 1 && (
            <button
              onClick={() => splitSelected(path)}
              title={`Re-split ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""} at paragraph/heading boundaries`}
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
            >
              <Split style={{ width: 10, height: 10 }} />
              Split {contextAtoms.size}
            </button>
          )}
          {contextAtoms.size >= 1 && (
            <button
              onClick={() => hideSelected(path)}
              title={`Hide ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""} from an agent's context (stays in the file)`}
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
            >
              <EyeOff style={{ width: 10, height: 10 }} />
              Hide {contextAtoms.size}
            </button>
          )}
          {contextAtoms.size >= 1 && (
            <button
              onClick={() => unhideSelected(path)}
              title={`Unhide ${contextAtoms.size} selected atom${contextAtoms.size !== 1 ? "s" : ""}`}
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: 6 }}
            >
              <Eye style={{ width: 10, height: 10 }} />
              Unhide {contextAtoms.size}
            </button>
          )}
          {isOutlineFile(path) && (
            <button
              onClick={() => chatOutline(path)}
              title="Generate a per-chapter beat sheet for each chapter this outline implies"
              style={{ display: "flex", alignItems: "center", gap: 4, fontSize: 10, padding: "2px 7px", borderRadius: 4, cursor: "pointer", background: "var(--amber-tint)", border: "1px solid var(--amber-border)", color: "var(--amber)", marginLeft: contextAtoms.size > 0 ? 6 : 0 }}
            >
              <ListTree style={{ width: 10, height: 10 }} />
              Generate beat sheets
            </button>
          )}
          {summaryKindsFor(path).length > 0 && (
            <SummarizeMenu onAuto={summarizeCurrentFile} onManual={createManualSummary} />
          )}
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
          {viewingSummary === null && <>
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
          </>}
        </div>
      )}

      {viewingSummary !== null ? (
        !activeConn ? (
          <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
            Loading…
          </div>
        ) : (
          <SummarySplitView
            kind={viewingSummary.kind}
            nodePath={viewingSummary.hops}
            coveredTicks={viewingOccurrence ? summaryCoverageFor(parentTicks, viewingOccurrence) : []}
            onBack={onCloseSummary}
            showFullChain={showFullSummaryChain}
            onToggleFullChain={() => setShowFullSummaryChain((v) => !v)}
            ticks={activeTicksChain}
            emptyMessage={activeConn.absent ? "Nothing here yet — write below to create it" : null}
            annotationMode={annotationMode}
            contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
            resetKey={activeKey}
            rebaseMarker={rebaseMarker}
            onSetRebaseMarker={setRebaseMarker}
            presenceBars={[]}
            onEdit={handleEditAtom}
            onToggleContextAtom={toggleContextAtom}
            onToggleContextAnnotation={toggleContextAnnotation}
            onCycleSwipe={handleCycleSwipe}
            onCorrect={handleCorrect}
            onEditPrompt={handleEditPrompt}
            activeBranch={branch}
            onOpenSummary={(kind, tickId) => onNavigateToSummary(kind, [...viewingSummary.hops, tickId])}
            agentLogs={agentLogs} onClearAgentLogs={clearAgentLogs}
            enabled={activeKey !== null}
            contextAtomCount={contextAtoms.size} contextAnnotationCount={contextAnnotations.size}
            rebasing={rebaseMarker !== null}
            onClearRebase={() => setRebaseMarker(null)}
            onClearContext={clearContext}
            onAppend={(text) => activeKey && appendToFile(activeKey, text)}
            onWrite={(text) => activeKey && chatWrite(activeKey, text)}
            onFix={handleFix}
            onNote={(text) => activeKey && chatNote(activeKey, text)}
            onRegen={(text, byBeat) => activeKey && chatRegen(activeKey, text, byBeat)}
            onRoleplay={(text) => activeKey && roleplayWrite(activeKey, text)}
            onAsk={(character, question) => activeKey && askCharacter(activeKey, character, question)}
            onInform={(character, fact) => appendJournal(character, fact, journalMarkers[character] ?? null)}
            onSummarize={summarizeCurrentFile}
          />
        )
      ) : viewMode === "source" ? (
        isAbsent ? (
          <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
            File does not exist yet — append to create it
          </div>
        ) : <RawEditPanel branch={branch} path={path} />
      ) : viewMode === "text" ? (
        isAbsent ? (
          <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
            File does not exist yet — append to create it
          </div>
        ) : <TextEditPanel branch={branch} path={path} />
      ) : (
        <FileContentView
          ticks={mainViewTicks}
          emptyMessage={isAbsent ? "File does not exist yet — append to create it" : fileTicks.length === 0 ? "Loading…" : null}
          annotationMode={annotationMode}
          contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
          resetKey={path}
          rebaseMarker={rebaseMarker}
          onSetRebaseMarker={setRebaseMarker}
          presenceBars={presenceBars}
          onEdit={handleEditAtom}
          onToggleContextAtom={toggleContextAtom}
          onToggleContextAnnotation={toggleContextAnnotation}
          onCycleSwipe={handleCycleSwipe}
          onCorrect={handleCorrect}
          onEditPrompt={handleEditPrompt}
          activeBranch={branch}
          targetFile={path}
          onUploadImages={uploadImageToTimeline}
          onOpenSummary={(kind, tickId) => onNavigateToSummary(kind, [tickId])}
          agentLogs={agentLogs} onClearAgentLogs={clearAgentLogs}
          enabled={activeKey !== null}
          contextAtomCount={contextAtoms.size} contextAnnotationCount={contextAnnotations.size}
          rebasing={rebaseMarker !== null}
          onClearRebase={() => setRebaseMarker(null)}
          onClearContext={clearContext}
          onAppend={(text) => activeKey && appendToFile(activeKey, text)}
          onWrite={(text)  => activeKey && chatWrite(activeKey, text)}
          onFix={handleFix}
          onNote={(text)   => activeKey && chatNote(activeKey, text)}
          onRegen={(text, byBeat) => activeKey && chatRegen(activeKey, text, byBeat)}
          onRoleplay={(text) => activeKey && roleplayWrite(activeKey, text)}
          onAsk={(character, question) => activeKey && askCharacter(activeKey, character, question)}
          onInform={(character, fact) => appendJournal(character, fact, journalMarkers[character] ?? null)}
          onSummarize={summarizeCurrentFile}
        />
      )}
    </>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

// The prose surface's own sidebar content: who's in this scene, the codex,
// and what this file's next agent call would cost. All three are prose
// questions — which is exactly why they live here rather than in page.tsx:
// another surface's sidebar answers different questions entirely (see
// dsl-file-view.tsx's DslSidebar), and neither has to know the other
// exists. Which tab is showing is likewise this surface's own state.
export function ProseSidebar({ branch, path }: { branch: string; path: string }) {
  const characterBranches = useServerCache((s) => s.characterBranches);
  const openFiles         = useServerCache((s) => s.openFiles);
  const openCharacters    = useServerCache((s) => s.openCharacters);
  const openJournals      = useServerCache((s) => s.openJournals);

  const rebaseMarker            = useUI((s) => s.rebaseMarker);
  const journalMarkers          = useUI((s) => s.journalMarkers);
  const characterAnswers        = useUI((s) => s.characterAnswers);
  const contextAtoms            = useUI((s) => s.contextAtoms);
  const contextAnnotations      = useUI((s) => s.contextAnnotations);
  const setJournalMarker        = useUI((s) => s.setJournalMarker);
  const setHoverHighlight       = useUI((s) => s.setHoverHighlight);
  const clearHoverHighlight     = useUI((s) => s.clearHoverHighlight);
  const toggleContextAtom       = useUI((s) => s.toggleContextAtom);
  const toggleContextAnnotation = useUI((s) => s.toggleContextAnnotation);

  const [tab, setTab] = useState<"characters" | "codex" | "cost">("characters");

  const fileConn = openFiles[path] ?? null;
  const fileChainTicks = fileConn?.ticks ?? {};
  const fileChainHead  = fileConn?.head ?? null;

  return (
    <>
      <div style={{ flexShrink: 0, padding: "8px 8px 0" }}>
        <div style={{
          display: "grid", gridTemplateColumns: "1fr 1fr 1fr", gap: 1,
          background: "var(--surface)", borderRadius: 6, padding: 2,
        }}>
          {(["characters", "codex", "cost"] as const).map((t) => (
            <button key={t} onClick={() => setTab(t)} style={{
              height: 26, display: "flex", alignItems: "center", justifyContent: "center",
              gap: 5, fontSize: 11, borderRadius: 4, border: "none", cursor: "pointer",
              background: tab === t ? "var(--surface-raised)" : "transparent",
              color: tab === t ? "var(--amber)" : "var(--text-disabled)",
              transition: "background 0.15s, color 0.15s",
            }}>
              {t === "characters" ? <Users style={{ width: 12, height: 12 }} /> : t === "codex" ? <BookMarked style={{ width: 12, height: 12 }} /> : <Gauge style={{ width: 12, height: 12 }} />}
              {t === "characters" ? "Characters" : t === "codex" ? "Codex" : "Cost"}
            </button>
          ))}
        </div>
        <div style={{ height: 8 }} />
      </div>

      <div style={{ flex: 1, minHeight: 0, display: "flex", flexDirection: "column" }}>
        {tab === "characters" ? (
          <CharacterSidebar
            selectedFile={path}
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
        ) : tab === "codex" ? (
          <CodexTab activeBranch={branch} selectedFile={path} />
        ) : (
          <ContextCostSidebar
            activeBranch={branch} selectedFile={path}
            fileChainTicks={fileChainTicks} fileChainHead={fileChainHead}
          />
        )}
      </div>
    </>
  );
}
