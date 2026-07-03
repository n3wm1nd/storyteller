"use client";

import { useEffect, useState } from "react";
import { Users, UserPlus, X, ChevronDown, ChevronRight, History, RefreshCw } from "lucide-react";
import { type CharacterConn, type FileConn, type WireTick } from "@/lib/store";
import { tickChain, activeCharacterBranches, characterDisplayName as displayName, characterColor, nearestJournalMarker } from "@/lib/utils";
import { WireTickList } from "./fileview";

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

function JournalPanel({
  branch, journal, journalMarker, onSetJournalMarker,
  contextAtoms, contextAnnotations, onToggleContextAtom, onToggleContextAnnotation,
  onTrack, onHoverAtoms, onHoverEnd, onEditAtom,
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
}) {
  const color = characterColor(branch);
  const journalTicks = journal?.ticks ?? {};
  const journalHead = journal?.head ?? null;
  const chain = tickChain(journalTicks, journalHead);
  const atoms = chain.filter((t) => t.kind === "atom");

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
        // entries actually exceed the cap.
        <div style={{ maxHeight: 420, display: "flex", flexDirection: "column", overflow: "hidden", borderRadius: 4 }}>
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
            onHoverAtom={(tickIds) => onHoverAtoms(tickIds, color)}
            onHoverEnd={onHoverEnd}
            compact
          />
        </div>
      )}
    </div>
  );
}

// ── Character card ───────────────────────────────────────────────────────────

function CharacterCard({
  branch, conn, journal, expanded, onToggleExpand, onLeave,
  journalMarker, onSetJournalMarker,
  contextAtoms, contextAnnotations, onToggleContextAtom, onToggleContextAnnotation,
  onTrack, onHoverAtoms, onHoverEnd, onEditAtom,
}: {
  branch: string;
  conn: CharacterConn | undefined;
  journal: FileConn | undefined;
  expanded: boolean;
  onToggleExpand: () => void;
  onLeave: () => void;
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
}) {
  const connected = conn !== undefined;
  const name = conn?.name ?? displayName(branch);

  return (
    <div style={{
      border: "1px solid var(--border-subtle)", borderRadius: 6,
      background: "var(--card)", padding: "8px 10px", marginBottom: 6,
    }}>
      <div
        onClick={onToggleExpand}
        style={{ display: "flex", alignItems: "center", gap: 6, cursor: "pointer" }}
      >
        {expanded ? <ChevronDown style={{ width: 11, height: 11, color: "var(--text-dim)", flexShrink: 0 }} />
                  : <ChevronRight style={{ width: 11, height: 11, color: "var(--text-dim)", flexShrink: 0 }} />}
        <div style={{ width: 6, height: 6, borderRadius: "50%", flexShrink: 0, background: connected ? "var(--emerald)" : "var(--text-dim)" }} />
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-heading)", flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {name}
        </span>
        {journalMarker && (
          <span title="Time-travelling — following the scene's marker unless manually scrubbed" style={{ display: "flex", flexShrink: 0 }}>
            <History style={{ width: 10, height: 10, color: "var(--amber)" }} />
          </span>
        )}
        <button
          onClick={(e) => { e.stopPropagation(); onLeave(); }}
          title="Remove from scene"
          style={{
            background: "none", border: "none", cursor: "pointer",
            color: "var(--text-dim)",
            display: "flex", alignItems: "center", padding: 2, flexShrink: 0,
          }}>
          <X style={{ width: 12, height: 12 }} />
        </button>
      </div>

      {expanded && (
        <JournalPanel
          branch={branch} journal={journal}
          journalMarker={journalMarker} onSetJournalMarker={onSetJournalMarker}
          contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
          onToggleContextAtom={onToggleContextAtom} onToggleContextAnnotation={onToggleContextAnnotation}
          onTrack={onTrack} onHoverAtoms={onHoverAtoms} onHoverEnd={onHoverEnd}
          onEditAtom={onEditAtom}
        />
      )}
    </div>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

export function CharacterSidebar({
  selectedFile, branches, ticks, head, rebaseMarker, openCharacters,
  openCharacter, closeCharacter, openJournals, openJournal, closeJournal,
  journalMarkers, setJournalMarker, trackJournal, editJournalAtom,
  contextAtoms, contextAnnotations, toggleContextAtom, toggleContextAnnotation,
  onHoverAtoms, onHoverEnd, enterScene, leaveScene,
}: {
  // Presence is scoped to a file (a scene), not the whole branch — see
  // WRITER.md — so this sidebar reflects whichever file is open, not the
  // branch as a whole. 'ticks'/'head' are that file's own projected chain.
  selectedFile: string | null;
  branches: string[];
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
  editJournalAtom: (branch: string, tickId: string, content: string, marker: string | null) => void;
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
}) {
  const effectiveHead = rebaseMarker ?? head;
  const active = activeCharacterBranches(ticks, effectiveHead);
  const activeKey = active.join("|");
  const [showAdd, setShowAdd] = useState(false);
  const [expandedBranch, setExpandedBranch] = useState<string | null>(null);
  const rebasing = rebaseMarker !== null;

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

  const available = branches.filter((b) => b.startsWith("character/") && !active.includes(b));

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
      </div>

      {rebasing && (
        <div style={{
          flexShrink: 0, padding: "5px 10px", borderBottom: "1px solid var(--border-subtle)",
          background: "oklch(0.78 0.10 65 / 0.10)", display: "flex", alignItems: "center", gap: 5,
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
              contextAtoms={contextAtoms} contextAnnotations={contextAnnotations}
              onToggleContextAtom={toggleContextAtom} onToggleContextAnnotation={toggleContextAnnotation}
              onTrack={() => selectedFile && trackJournal(b, selectedFile)}
              onHoverAtoms={onHoverAtoms} onHoverEnd={onHoverEnd}
              onEditAtom={(tickId, content, marker) => editJournalAtom(b, tickId, content, marker)}
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
                available.map((b) => (
                  <button
                    key={b}
                    onClick={() => { enterScene(selectedFile, b); setShowAdd(false); }}
                    style={{
                      display: "block", width: "100%", textAlign: "left",
                      fontSize: 11, padding: "4px 6px", borderRadius: 4,
                      background: "transparent", border: "none", cursor: "pointer",
                      color: "var(--text-secondary)",
                    }}
                  >
                    {displayName(b)}
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
