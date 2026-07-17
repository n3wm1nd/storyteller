"use client";

import { useEffect, useRef, useState } from "react";
import { Users, UserPlus, UserMinus, ChevronDown, ChevronRight, History, RefreshCw, HelpCircle } from "lucide-react";
import { type CharacterConn, type FileConn, type WireTick } from "@/lib/serverCacheStore";
import { type CharacterSummary } from "@/lib/ws";
import { tickChain, activeCharacterBranches, characterDisplayName as displayName, characterColor, nearestJournalMarker, presentDuringAtoms } from "@/lib/utils";
import { WireTickList } from "./fileview";
import { LoreSelector } from "./lore-selector";
import { CHARACTER_CONTEXT_SOURCE_ID } from "@/lib/agents";
import { TasksPanel } from "./tasks-panel";
import { CharacterAvatar } from "./avatar";

// ── Journal panel ────────────────────────────────────────────────────────────
//
// Reuses WireTickList in 'compact' mode (see fileview.tsx) rather than a
// bespoke read-only renderer — double-click-to-edit, ctrl-click-to-select,
// and the rebase-marker/suppression machinery are exactly what basic journal
// editing needs. Selection itself is the same shared contextAtoms/
// contextAnnotations set the main view uses (a journal atom's id never
// appears in the scene's own chain and vice versa, so one flat set is
// safe) — but delete/fix stay on the main view's existing controls (see
// page.tsx), rather than duplicating a second delete button/fix input here.

const JOURNAL_DISPLAY_LIMIT = 30;

function JournalPanel({
  branch, journal, journalMarker, onSetJournalMarker,
  contextAtoms, contextAnnotations, onToggleContextAtom, onToggleContextAnnotation,
  onTrack, onHoverAtoms, onHoverEnd, onEditAtom, onCycleSwipe, onAppend,
}: {
  branch: string;
  journal: FileConn | undefined;
  // Global, per-branch — tracked continuously (see CharacterSidebar) even
  // while this panel isn't mounted, so it's already correct the moment the
  // accordion row expands.
  journalMarker: string | null;
  onSetJournalMarker: (tickId: string | null) => void;
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  onToggleContextAtom: (tickId: string) => void;
  onToggleContextAnnotation: (tickId: string) => void;
  onTrack: () => void;
  onHoverAtoms: (tickIds: Set<string>, color: string) => void;
  onHoverEnd: () => void;
  onEditAtom: (tickId: string, content: string, marker: string | null) => void;
  onCycleSwipe: (tickId: string, marker: string | null) => void;
  onAppend: (text: string, marker: string | null) => void;
}) {
  const color = characterColor(branch);
  const journalTicks = journal?.ticks ?? {};
  const journalHead = journal?.head ?? null;
  const fullChain = tickChain(journalTicks, journalHead);
  const atoms = fullChain.filter((t) => t.kind === "atom");
  // A journal is meant to keep growing forever (every scene, every
  // character) -- rendering all of it gets expensive and mostly unread far
  // past the recent tail, so the sidebar only ever shows the last
  // JOURNAL_DISPLAY_LIMIT entries. Nothing is dropped server-side; the full
  // history is still there in journal.md itself (open it directly, same as
  // any other file, for anything older than this cap).
  const truncatedCount = Math.max(0, fullChain.length - JOURNAL_DISPLAY_LIMIT);
  const chain = truncatedCount > 0 ? fullChain.slice(-JOURNAL_DISPLAY_LIMIT) : fullChain;

  const [draft, setDraft] = useState("");
  function submitAppend() {
    const text = draft.trim();
    if (!text) return;
    onAppend(text, journalMarker);
    setDraft("");
  }

  // Jump to the most recent entries by default, once, the first time this
  // panel has something to show -- 'journal' arrives async (starts as
  // "Loading…", see below, before the scrollable div even exists), so this
  // is keyed off content actually appearing rather than mount itself.
  // 'scrolledRef' stops it from re-firing on every later update (e.g. a
  // Track landing new atoms while the accordion is still open), which
  // would otherwise yank focus away from wherever the user has since
  // scrolled to.
  const scrollRef = useRef<HTMLDivElement | null>(null);
  const scrolledRef = useRef(false);
  useEffect(() => {
    if (scrolledRef.current) return;
    const el = scrollRef.current;
    if (!el) return;
    el.scrollTop = el.scrollHeight;
    scrolledRef.current = true;
  }, [chain.length]);

  return (
    <div style={{ marginTop: 8, paddingTop: 8, borderTop: "1px solid var(--border-subtle)" }}>
      <div style={{ display: "flex", alignItems: "center", marginBottom: 6, gap: 6 }}>
        <span style={{ fontSize: 9, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.06em", color: "var(--text-dim)", flex: 1 }}>
          Journal
        </span>
        <button
          onClick={(e) => { e.stopPropagation(); onTrack(); }}
          title="Track new scene content into this character's journal"
          style={{
            display: "flex", alignItems: "center", gap: 3, fontSize: 9, padding: "2px 6px",
            background: "transparent", border: "1px solid var(--border)", borderRadius: 4,
            color: "var(--text-label)", cursor: "pointer",
          }}
        >
          <RefreshCw style={{ width: 9, height: 9 }} />
          Track
        </button>
      </div>

      {!journal || (journal.head === null && !journal.absent) ? (
        <div style={{ fontSize: 10, color: "var(--text-ghost)", fontStyle: "italic" }}>Loading…</div>
      ) : atoms.length === 0 ? (
        <div style={{ fontSize: 10, color: "var(--text-ghost)", fontStyle: "italic" }}>No journal entries yet</div>
      ) : (
        // maxHeight (not a fixed height) so a short journal doesn't leave
        // dead space below it — it only starts scrolling internally once
        // entries actually exceed the cap. Only one accordion row can be
        // expanded at a time (see 'expandedBranch' below), so this can
        // afford to use most of the viewport rather than a small fixed cap.
        // overflow must be "auto", not "hidden" — WireTickList's compact
        // mode (fileview.tsx) deliberately leaves scrolling to the caller.
        <div ref={scrollRef} style={{ maxHeight: "70vh", display: "flex", flexDirection: "column", overflow: "auto", borderRadius: 4 }}>
          {truncatedCount > 0 && (
            <div style={{ fontSize: 9, color: "var(--text-ghost)", fontStyle: "italic", padding: "2px 4px 6px" }}>
              {truncatedCount} earlier {truncatedCount === 1 ? "entry" : "entries"} hidden — open {branch}/journal.md directly to see the full history
            </div>
          )}
          <WireTickList
            ticks={chain}
            annotationMode="hidden"
            contextAtoms={contextAtoms}
            contextAnnotations={contextAnnotations}
            resetKey={branch}
            rebaseMarker={journalMarker}
            onSetRebaseMarker={onSetJournalMarker}
            presenceBars={[]}
            onEdit={(tickId, content) => onEditAtom(tickId, content, journalMarker)}
            onToggleContextAtom={onToggleContextAtom}
            onToggleContextAnnotation={onToggleContextAnnotation}
            onCycleSwipe={(tickId) => onCycleSwipe(tickId, journalMarker)}
            onHoverAtom={(tickIds) => onHoverAtoms(tickIds, color)}
            onHoverEnd={onHoverEnd}
            compact
          />
        </div>
      )}

      {journal && (
        <div style={{ display: "flex", gap: 4, marginTop: 6 }}>
          <input
            value={draft} onChange={(e) => setDraft(e.target.value)}
            onKeyDown={(e) => { if (e.key === "Enter") { e.preventDefault(); submitAppend(); } }}
            placeholder={journalMarker ? "Append at the marker…" : "Append to journal…"}
            style={{ flex: 1, fontSize: 10, padding: "3px 6px", background: "var(--card)", border: "1px solid var(--border)", borderRadius: 4, color: "var(--foreground)", outline: "none" }}
          />
          <button
            onClick={submitAppend}
            disabled={!draft.trim()}
            style={{ fontSize: 10, padding: "3px 8px", background: "var(--amber)", border: "none", borderRadius: 4, color: "oklch(0.15 0.01 60)", fontWeight: 600, cursor: draft.trim() ? "pointer" : "default", opacity: draft.trim() ? 1 : 0.5 }}
          >
            Add
          </button>
        </div>
      )}
    </div>
  );
}

// ── Ask panel ────────────────────────────────────────────────────────────────
//
// Ask this character a question, answered from only their own branch (see
// Server.Writer.File.askCharacter) — not the scene, not any other character.
// The exchange is recorded server-side as a CharacterAnswer tick on the
// scene's own branch (not the character's — see that module's own Haddock),
// but this panel doesn't read that tick back; it just shows whatever's
// arrived in useUI's characterAnswers ring buffer for this character,
// oldest first, same ephemeral-log treatment as AgentLogStrip.

function AskPanel({ answers, onAsk }: { answers: { question: string; answer: string }[]; onAsk: (question: string) => void }) {
  const [draft, setDraft] = useState("");
  function submitAsk() {
    const question = draft.trim();
    if (!question) return;
    onAsk(question);
    setDraft("");
  }

  return (
    <div style={{ marginTop: 8, paddingTop: 8, borderTop: "1px solid var(--border-subtle)" }}>
      <span style={{ fontSize: 9, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.06em", color: "var(--text-dim)" }}>
        Ask
      </span>
      {answers.length > 0 && (
        <div style={{ marginTop: 6, display: "flex", flexDirection: "column", gap: 6, maxHeight: "30vh", overflow: "auto" }}>
          {answers.map((a, i) => (
            <div key={i} style={{ fontSize: 10 }}>
              <div style={{ color: "var(--text-dim)", fontStyle: "italic" }}>{a.question}</div>
              <div style={{ color: "var(--text-secondary)", marginTop: 2 }}>{a.answer}</div>
            </div>
          ))}
        </div>
      )}
      <div style={{ display: "flex", gap: 4, marginTop: 6 }}>
        <input
          value={draft} onChange={(e) => setDraft(e.target.value)}
          onKeyDown={(e) => { if (e.key === "Enter") { e.preventDefault(); submitAsk(); } }}
          placeholder="Ask them something…"
          style={{ flex: 1, fontSize: 10, padding: "3px 6px", background: "var(--card)", border: "1px solid var(--border)", borderRadius: 4, color: "var(--foreground)", outline: "none" }}
        />
        <button
          onClick={submitAsk}
          disabled={!draft.trim()}
          style={{ display: "flex", alignItems: "center", gap: 3, fontSize: 10, padding: "3px 8px", background: "transparent", border: "1px solid var(--border)", borderRadius: 4, color: "var(--text-label)", cursor: draft.trim() ? "pointer" : "default", opacity: draft.trim() ? 1 : 0.5 }}
        >
          <HelpCircle style={{ width: 10, height: 10 }} />
          Ask
        </button>
      </div>
    </div>
  );
}

// ── Context panel ────────────────────────────────────────────────────────────
//
// This character's own curated lore, via the shared LoreSelector (see
// lore-selector.tsx) bound to CHARACTER_CONTEXT_SOURCE_ID — the same
// bucket-picker ContextFilter model the story branch's Codex tab uses, just
// scoped to this character's own branch. sheet.md/journal.md never appear
// as candidates here at all (excluded server-side, see
// Storyteller.Writer.Lore) — this panel only ever shows extra files beyond
// those two, which is usually nothing.

function ContextPanel({ branch }: { branch: string }) {
  return (
    <div style={{ marginTop: 8, paddingTop: 8, borderTop: "1px solid var(--border-subtle)" }}>
      <div style={{ display: "flex", alignItems: "center", marginBottom: 2, gap: 6 }}>
        <span style={{ fontSize: 9, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.06em", color: "var(--text-dim)", flex: 1 }}>
          Context
        </span>
      </div>
      <LoreSelector branch={branch} sourceId={CHARACTER_CONTEXT_SOURCE_ID} compact />
    </div>
  );
}

// ── Character card ───────────────────────────────────────────────────────────

function CharacterCard({
  branch, conn, journal, expanded, onToggleExpand, onLeave,
  journalMarker, onSetJournalMarker, journalBehind,
  contextAtoms, contextAnnotations, onToggleContextAtom, onToggleContextAnnotation,
  onTrack, onSyncTasks, onSuggestTasks, onHoverAtoms, onHoverEnd, onEditAtom, onCycleSwipe, onAppend,
  onAsk, answers, presentTickIds,
}: {
  branch: string;
  conn: CharacterConn | undefined;
  journal: FileConn | undefined;
  expanded: boolean;
  onToggleExpand: () => void;
  onLeave: () => void;
  journalMarker: string | null;
  onSetJournalMarker: (tickId: string | null) => void;
  // true when the scene has atoms the journal's last tracked ref doesn't
  // reach yet -- not an error (see 'CharacterSidebar's own computation),
  // just a nudge that a Track is due.
  journalBehind: boolean;
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  onToggleContextAtom: (tickId: string) => void;
  onToggleContextAnnotation: (tickId: string) => void;
  onTrack: () => void;
  onSyncTasks: () => void;
  onSuggestTasks: () => void;
  onHoverAtoms: (tickIds: Set<string>, color: string) => void;
  onHoverEnd: () => void;
  onEditAtom: (tickId: string, content: string, marker: string | null) => void;
  onCycleSwipe: (tickId: string, marker: string | null) => void;
  onAppend: (text: string, marker: string | null) => void;
  onAsk: (question: string) => void;
  answers: { question: string; answer: string }[];
  // This character's own atom ids in the current scene (see
  // lib/utils.presentDuringAtoms) — precomputed by CharacterSidebar (which
  // already has the scene's ticks/head) so the card itself doesn't need
  // either as a prop just to answer "hovering me, what do I highlight".
  presentTickIds: Set<string>;
}) {
  const connected = conn !== undefined;
  // 'conn.name' (from the /character/{branch} connection's CharacterState)
  // is just the branch id with the prefix stripped — the server never
  // extracts a display name from the sheet (see WS-PROTOCOL.md's
  // "read is raw-but-complete" rule). Decode the real name from the raw
  // sheet content here, same as the character list in sidebar.tsx.
  const name = displayName(branch, conn?.sheet);
  // Same per-character color the journal panel/hover-highlight already use
  // (lib/utils.characterColor) rather than a uniform "connected" green —
  // this dot is the character's own identity marker, dimmed rather than
  // recolored when the connection itself isn't up yet. Swapped for an
  // actual avatar.png below when the branch has one (see
  // Server.Writer.Character's charHasAvatar) -- the dot remains the
  // fallback for a character with no card-derived portrait.
  const color = characterColor(branch);
  const [hover, setHover] = useState(false);

  return (
    <div
      onMouseEnter={() => { setHover(true); onHoverAtoms(presentTickIds, color); }}
      onMouseLeave={() => { setHover(false); onHoverEnd(); }}
      style={{
        border: "1px solid var(--border-subtle)", borderRadius: 6,
        background: hover ? "var(--surface)" : "var(--card)", padding: "8px 10px", marginBottom: 6,
      }}
    >
      <div
        onClick={onToggleExpand}
        style={{ display: "flex", alignItems: "center", gap: 6, cursor: "pointer" }}
      >
        {expanded ? <ChevronDown style={{ width: 11, height: 11, color: "var(--text-dim)", flexShrink: 0 }} />
                  : <ChevronRight style={{ width: 11, height: 11, color: "var(--text-dim)", flexShrink: 0 }} />}
        <CharacterAvatar
          branch={branch} hasAvatar={conn?.avatar ?? false} color={color} size={14}
          fallback={<div style={{ width: 6, height: 6, borderRadius: "50%", flexShrink: 0, background: connected ? color : "var(--text-dim)" }} />}
        />
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-heading)", flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {name}
        </span>
        {journalMarker && (
          <span title="Time-travelling — following the scene's marker unless manually scrubbed" style={{ display: "flex", flexShrink: 0 }}>
            <History style={{ width: 10, height: 10, color: "var(--amber)" }} />
          </span>
        )}
        {journalBehind && (
          <span
            title="Journal hasn't caught up with the scene yet — not an error, just click Track (or Track All) when you're ready"
            style={{ display: "flex", flexShrink: 0 }}
          >
            <RefreshCw style={{ width: 10, height: 10, color: "var(--text-dim)" }} />
          </span>
        )}
        {hover && (
          <button
            onClick={(e) => { e.stopPropagation(); onLeave(); }}
            title="Leave the scene"
            style={{
              background: "none", border: "none", cursor: "pointer",
              color: "var(--text-dim)",
              display: "flex", alignItems: "center", padding: 2, flexShrink: 0,
            }}>
            <UserMinus style={{ width: 12, height: 12 }} />
          </button>
        )}
      </div>

      {expanded && (
        <>
          <ContextPanel branch={branch} />
          <JournalPanel
            branch={branch} journal={journal}
            journalMarker={journalMarker} onSetJournalMarker={onSetJournalMarker}
            contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
            onToggleContextAtom={onToggleContextAtom} onToggleContextAnnotation={onToggleContextAnnotation}
            onTrack={onTrack}
            onHoverAtoms={onHoverAtoms} onHoverEnd={onHoverEnd}
            onEditAtom={onEditAtom} onCycleSwipe={onCycleSwipe} onAppend={onAppend}
          />
          <TasksPanel branch={branch} onSyncTasks={onSyncTasks} onSuggestTasks={onSuggestTasks} />
          <AskPanel answers={answers} onAsk={onAsk} />
        </>
      )}
    </div>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

export function CharacterSidebar({
  selectedFile, characterBranches, ticks, head, rebaseMarker, openCharacters,
  openCharacter, closeCharacter, openJournals, openJournal, closeJournal,
  journalMarkers, setJournalMarker, trackJournal, onTrackAll, syncTasks, suggestTasks, editJournalAtom, cycleJournalSwipe, appendJournal,
  contextAtoms, contextAnnotations, toggleContextAtom, toggleContextAnnotation,
  onHoverAtoms, onHoverEnd, enterScene, leaveScene, askCharacter, characterAnswers,
}: {
  // Presence is scoped to a file (a scene), not the whole branch — see
  // WRITER.md — so this sidebar reflects whichever file is open, not the
  // branch as a whole. 'ticks'/'head' are that file's own projected chain.
  selectedFile: string | null;
  // Live-tracked character/* list with raw sheet content (see
  // Server.Writer.Session.Connection) — used here so the "add to scene"
  // picker can show real character names, not just branch ids.
  characterBranches: CharacterSummary[];
  ticks: Record<string, WireTick>;
  head: string | null;
  // When set (time-travel/rebase mode — see fileview.tsx's RebaseHandle),
  // the scene shown is "as of this tick" rather than live HEAD.
  rebaseMarker: string | null;
  openCharacters: Record<string, CharacterConn>;
  openCharacter: (branch: string) => void;
  closeCharacter: (branch: string) => void;
  openJournals: Record<string, FileConn>;
  openJournal: (branch: string) => void;
  closeJournal: (branch: string) => void;
  journalMarkers: Record<string, string | null>;
  setJournalMarker: (branch: string, tickId: string | null) => void;
  trackJournal: (characterBranch: string, fromPath: string) => void;
  // "Track All" — every known character branch, every source file, into
  // each one's own journal (see tracker.actions.trackAllJournals);
  // deliberately not scoped to 'active' (the current scene's characters),
  // since the point is catching up characters who weren't clicked through
  // individually, including ones absent from the current scene entirely.
  onTrackAll: () => void;
  // Experimental tasks.md sync/suggest, per character -- see
  // tasks-panel.actions.syncTasks/suggestTasks.
  syncTasks: (characterBranch: string) => void;
  suggestTasks: (characterBranch: string) => void;
  editJournalAtom: (branch: string, tickId: string, content: string, marker: string | null) => void;
  cycleJournalSwipe: (branch: string, tickId: string, marker: string | null) => void;
  appendJournal: (branch: string, content: string, marker: string | null) => void;
  // Shared, connection-agnostic tick selection — the same sets the main file
  // view uses. Delete/Fix on this selection are driven from the main view's
  // own controls (page.tsx), not duplicated here.
  contextAtoms: Set<string>;
  contextAnnotations: Set<string>;
  toggleContextAtom: (tickId: string) => void;
  toggleContextAnnotation: (tickId: string) => void;
  onHoverAtoms: (tickIds: Set<string>, color: string) => void;
  onHoverEnd: () => void;
  enterScene: (path: string, character: string) => void;
  leaveScene: (path: string, character: string) => void;
  askCharacter: (path: string, character: string, question: string) => void;
  // Ring buffer of every character.answered event received so far this
  // session (see lib/uiStore's characterAnswers) — filtered per branch
  // below, same shape AgentLogStrip reads agentLogs in.
  characterAnswers: { character: string; question: string; answer: string }[];
}) {
  const effectiveHead = rebaseMarker ?? head;
  const active = activeCharacterBranches(ticks, effectiveHead);
  const activeKey = active.join("|");
  const [showAdd, setShowAdd] = useState(false);
  const [expandedBranch, setExpandedBranch] = useState<string | null>(null);
  const rebasing = rebaseMarker !== null;

  // "Behind" (not synced) purely as a display hint, not an error: the
  // scene's own last atom vs. the ref the journal's own last tracked atom
  // carries (each 'copyAtom' commit records the source tick id it was
  // copied from — see Storyteller.Writer.Agent.Tracker). Unequal doesn't
  // mean anything went wrong: it's the normal state right after new scene
  // content lands, or right after entering the scene, until the next Track.
  // No atoms in the scene yet -> nothing to compare, so no flag either way.
  const sceneAtoms = tickChain(ticks, effectiveHead).filter((t) => t.kind === "atom");
  const lastSceneAtomId = sceneAtoms.length > 0 ? sceneAtoms[sceneAtoms.length - 1].tickId : null;
  function isJournalBehind(branch: string): boolean {
    if (!lastSceneAtomId) return false;
    const journal = openJournals[branch];
    if (!journal) return false;
    const journalAtomsWithRefs = tickChain(journal.ticks, journal.head)
      .filter((t) => t.kind === "atom" && t.refs.length > 0);
    const lastSyncedRef = journalAtomsWithRefs.length > 0
      ? journalAtomsWithRefs[journalAtomsWithRefs.length - 1].refs[0]
      : null;
    return lastSyncedRef !== lastSceneAtomId;
  }

  // Presence ticks (see lib/utils.activeCharacterBranches) are the source of
  // truth for who's active — this keeps exactly that set of character and
  // journal connections open (opening/closing both together), whether the
  // change came from this sidebar or another connection. The journal is
  // opened here — not lazily on accordion-expand — because its marker (just
  // below) must keep tracking even while the row is collapsed.
  useEffect(() => {
    for (const b of active) {
      if (!openCharacters[b]) openCharacter(b);
      if (!openJournals[b]) openJournal(b);
    }
    for (const b of Object.keys(openCharacters)) if (!active.includes(b)) closeCharacter(b);
    for (const b of Object.keys(openJournals)) if (!active.includes(b)) closeJournal(b);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeKey]);

  // Every active character's journal marker follows the scene's own marker
  // (see lib/utils.nearestJournalMarker) — recomputed whenever the scene
  // marker changes, or a journal's data arrives/updates (e.g. the initial
  // load right after opening, or a fresh Track). Not gated on the accordion
  // being expanded — "if I go back in time, the characters do too" has to
  // hold regardless of what's currently visible.
  const journalHeadsKey = active.map((b) => `${b}:${openJournals[b]?.head ?? ""}`).join("|");
  useEffect(() => {
    for (const b of active) {
      const jc = openJournals[b];
      if (!jc) continue;
      setJournalMarker(b, nearestJournalMarker(jc.ticks, jc.head, ticks, head, rebaseMarker));
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [rebaseMarker, journalHeadsKey, activeKey]);

  // A character who leaves the scene can't stay the expanded accordion row.
  useEffect(() => {
    if (expandedBranch && !active.includes(expandedBranch)) setExpandedBranch(null);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [activeKey]);

  const available = characterBranches.filter((c) => !active.includes(c.branch));

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%", background: "var(--sidebar)" }}>
      <div style={{
        flexShrink: 0, padding: "8px 10px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 6,
      }}>
        <Users style={{ width: 12, height: 12, color: "var(--text-dim)" }} />
        <span style={{ fontSize: 10, fontWeight: 600, textTransform: "uppercase", letterSpacing: "0.08em", color: "var(--text-dim)", flex: 1 }}>
          Scene
        </span>
        <span style={{ fontSize: 10, color: "var(--text-dim)" }}>{active.length}</span>
        {characterBranches.length > 0 && (
          <button
            onClick={onTrackAll}
            title="Track every character's journal up to date, scene-wide — including characters not currently in this scene"
            style={{
              display: "flex", alignItems: "center", gap: 3, fontSize: 9, padding: "2px 6px",
              background: "transparent", border: "1px solid var(--border)", borderRadius: 4,
              color: "var(--text-label)", cursor: "pointer",
            }}
          >
            <RefreshCw style={{ width: 9, height: 9 }} />
            Track All
          </button>
        )}
      </div>

      {rebasing && (
        <div style={{
          flexShrink: 0, padding: "5px 10px", borderBottom: "1px solid var(--border-subtle)",
          background: "var(--amber-wash)", display: "flex", alignItems: "center", gap: 5,
        }}>
          <History style={{ width: 11, height: 11, color: "var(--amber)", flexShrink: 0 }} />
          <span style={{ fontSize: 9, color: "var(--amber)", fontStyle: "italic" }}>Scene as of the rebase marker</span>
        </div>
      )}

      <div style={{ flex: 1, overflow: "auto", padding: "8px" }}>
        {!selectedFile ? (
          <div style={{ fontSize: 11, color: "var(--text-ghost)" }}>Open a file to see its scene</div>
        ) : active.length === 0 ? (
          <div style={{ fontSize: 11, color: "var(--text-ghost)" }}>No characters in this scene</div>
        ) : (
          active.map((b) => (
            <CharacterCard
              key={b} branch={b} conn={openCharacters[b]} journal={openJournals[b]}
              expanded={expandedBranch === b}
              onToggleExpand={() => setExpandedBranch((cur) => cur === b ? null : b)}
              onLeave={() => leaveScene(selectedFile, b)}
              journalMarker={journalMarkers[b] ?? null}
              onSetJournalMarker={(tickId) => setJournalMarker(b, tickId)}
              journalBehind={isJournalBehind(b)}
              contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
              onToggleContextAtom={toggleContextAtom} onToggleContextAnnotation={toggleContextAnnotation}
              onTrack={() => selectedFile && trackJournal(b, selectedFile)}
              onSyncTasks={() => syncTasks(b)} onSuggestTasks={() => suggestTasks(b)}
              onHoverAtoms={onHoverAtoms} onHoverEnd={onHoverEnd}
              onEditAtom={(tickId, content, marker) => editJournalAtom(b, tickId, content, marker)}
              onCycleSwipe={(tickId, marker) => cycleJournalSwipe(b, tickId, marker)}
              onAppend={(text, marker) => appendJournal(b, text, marker)}
              onAsk={(question) => selectedFile && askCharacter(selectedFile, b, question)}
              answers={characterAnswers.filter((a) => a.character === b)}
              presentTickIds={presentDuringAtoms(ticks, effectiveHead, b)}
            />
          ))
        )}
      </div>

      {selectedFile && (
        <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "6px 8px" }}>
          {showAdd ? (
            <div>
              {available.length === 0 ? (
                <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: "4px 2px" }}>No other character branches</div>
              ) : (
                available.map((c) => (
                  <button
                    key={c.branch}
                    onClick={() => { enterScene(selectedFile, c.branch); setShowAdd(false); }}
                    style={{
                      display: "block", width: "100%", textAlign: "left",
                      fontSize: 11, padding: "4px 6px", borderRadius: 4,
                      background: "transparent", border: "none", cursor: "pointer",
                      color: "var(--text-secondary)",
                    }}
                  >
                    {displayName(c.branch, c.sheet)}
                  </button>
                ))
              )}
              <button onClick={() => setShowAdd(false)} style={{
                fontSize: 10, marginTop: 4, background: "none", border: "none", cursor: "pointer", color: "var(--text-dim)", padding: "2px 4px",
              }}>Cancel</button>
            </div>
          ) : (
            <button
              onClick={() => setShowAdd(true)}
              style={{
                display: "flex", alignItems: "center", gap: 5, width: "100%", justifyContent: "center",
                fontSize: 10, padding: "5px 8px", background: "transparent",
                border: "1px solid var(--border)", borderRadius: 5, color: "var(--text-label)", cursor: "pointer",
              }}
            >
              <UserPlus style={{ width: 11, height: 11 }} />
              Add to scene
            </button>
          )}
        </div>
      )}
    </div>
  );
}
