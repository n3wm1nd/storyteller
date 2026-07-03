"use client";

import { useEffect, useState } from "react";
import { Users, UserPlus, X, ChevronDown, ChevronUp, History } from "lucide-react";
import { type CharacterConn, type WireTick } from "@/lib/store";
import { activeCharacterBranches } from "@/lib/utils";

function displayName(branch: string): string {
  const stripped = branch.startsWith("character/") ? branch.slice("character/".length) : branch;
  return decodeURIComponent(stripped);
}

// ── Character card ───────────────────────────────────────────────────────────

const SHEET_PREVIEW_LEN = 220;

function CharacterCard({ branch, conn, onLeave, leaveDisabled }: {
  branch: string;
  conn: CharacterConn | undefined;
  onLeave: () => void;
  leaveDisabled: boolean;
}) {
  const [expanded, setExpanded] = useState(false);
  const connected = conn !== undefined;
  const name  = conn?.name ?? displayName(branch);
  const sheet = conn?.sheet ?? null;
  const truncated = sheet !== null && sheet.length > SHEET_PREVIEW_LEN;
  const shown = sheet && !expanded && truncated ? sheet.slice(0, SHEET_PREVIEW_LEN) + "…" : sheet;

  return (
    <div style={{
      border: "1px solid var(--border-subtle)", borderRadius: 6,
      background: "var(--card)", padding: "8px 10px", marginBottom: 6,
    }}>
      <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
        <div style={{ width: 6, height: 6, borderRadius: "50%", flexShrink: 0, background: connected ? "var(--emerald)" : "var(--text-dim)" }} />
        <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-heading)", flex: 1, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {name}
        </span>
        <button
          onClick={leaveDisabled ? undefined : onLeave}
          disabled={leaveDisabled}
          title={leaveDisabled ? "Can't change presence while viewing a past rebase marker" : "Remove from scene"}
          style={{
            background: "none", border: "none", cursor: leaveDisabled ? "default" : "pointer",
            color: "var(--text-dim)", opacity: leaveDisabled ? 0.35 : 1,
            display: "flex", alignItems: "center", padding: 2, flexShrink: 0,
          }}>
          <X style={{ width: 12, height: 12 }} />
        </button>
      </div>

      {sheet ? (
        <>
          <div style={{ fontSize: 11, color: "var(--text-secondary)", lineHeight: 1.4, marginTop: 6, whiteSpace: "pre-wrap" }}>
            {shown}
          </div>
          {truncated && (
            <button onClick={() => setExpanded((v) => !v)} style={{
              display: "flex", alignItems: "center", gap: 3, marginTop: 4, fontSize: 9,
              background: "none", border: "none", cursor: "pointer", color: "var(--text-dim)", padding: 0,
            }}>
              {expanded ? <ChevronUp style={{ width: 10, height: 10 }} /> : <ChevronDown style={{ width: 10, height: 10 }} />}
              {expanded ? "Show less" : "Show more"}
            </button>
          )}
        </>
      ) : (
        <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", marginTop: 6 }}>
          {connected ? "No sheet yet" : "Connecting…"}
        </div>
      )}
    </div>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

export function CharacterSidebar({
  activeBranch, branches, ticks, branchHead, rebaseMarker, openCharacters,
  openCharacter, closeCharacter, enterScene, leaveScene,
}: {
  activeBranch: string | null;
  branches: string[];
  ticks: Record<string, WireTick>;
  branchHead: string | null;
  // When set (time-travel/rebase mode — see fileview.tsx's RebaseHandle),
  // the scene shown is "as of this tick" rather than live HEAD. Tick ids are
  // shared across the whole branch chain, so a marker set while rebasing one
  // file's atoms is still a valid point to fold presence ticks up to here.
  rebaseMarker: string | null;
  openCharacters: Record<string, CharacterConn>;
  openCharacter: (branch: string) => void;
  closeCharacter: (branch: string) => void;
  enterScene: (character: string) => void;
  leaveScene: (character: string) => void;
}) {
  const effectiveHead = rebaseMarker ?? branchHead;
  const active = activeCharacterBranches(ticks, effectiveHead);
  const activeKey = active.join("|");
  const [showAdd, setShowAdd] = useState(false);
  const rebasing = rebaseMarker !== null;

  // Presence ticks (see lib/utils.activeCharacterBranches) are the source of
  // truth for who's active — this just keeps exactly that set of character
  // connections open, opening ones that newly entered and closing ones that
  // left, whether the change came from this sidebar or another connection.
  useEffect(() => {
    for (const b of active) if (!openCharacters[b]) openCharacter(b);
    for (const b of Object.keys(openCharacters)) if (!active.includes(b)) closeCharacter(b);
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
        {!activeBranch ? (
          <div style={{ fontSize: 11, color: "var(--text-ghost)" }}>Select a branch</div>
        ) : active.length === 0 ? (
          <div style={{ fontSize: 11, color: "var(--text-ghost)" }}>No characters in this scene</div>
        ) : (
          active.map((b) => (
            <CharacterCard key={b} branch={b} conn={openCharacters[b]} onLeave={() => leaveScene(b)} leaveDisabled={rebasing} />
          ))
        )}
      </div>

      {activeBranch && rebasing && (
        <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "8px", fontSize: 9, color: "var(--text-ghost)", textAlign: "center" }}>
          Adding/removing characters is disabled while viewing a rebase marker
        </div>
      )}

      {activeBranch && !rebasing && (
        <div style={{ flexShrink: 0, borderTop: "1px solid var(--border-subtle)", padding: "6px 8px" }}>
          {showAdd ? (
            <div>
              {available.length === 0 ? (
                <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: "4px 2px" }}>No other character branches</div>
              ) : (
                available.map((b) => (
                  <button
                    key={b}
                    onClick={() => { enterScene(b); setShowAdd(false); }}
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
