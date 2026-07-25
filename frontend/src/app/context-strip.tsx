"use client";

// Layer 0 of the context UI: the always-visible one-line summary above
// the InputBar. The casual user's entire exposure to "what the LLM will
// see" -- reads as a status line, not as a DSL anything.
//
// Reflects the four independent per-call slots (see dslCompose.ts's own
// header): a lore toggle, an other toggle, a past-chapters mode, and a
// list of pinned snippet names -- plus the live @mention overlay. There's
// no more "named function replaces everything" mode: pinning a saved
// snippet just adds one more chip, same as any other pinned program.
//
// "Edit as code →" lives in the corner, behind an explicit click --
// power-user territory, never the default surface.
//
// Styling matches the existing InputBar context strip (the "N atoms
// selected" line above the textarea, see fileview.tsx's InputBar): same
// font sizes, same amber accent for "selection present" affordances,
// same clickable × to clear.

import { useMemo } from "react";
import { BookOpen, FileText, Pin, X, Code2, Clock } from "lucide-react";
import { useCallContext, isFileDirty, EMPTY_MENTIONS } from "@/lib/callContextStore";
import { DEFAULT_EDITS, isLoreProgramCheckboxOwned } from "@/lib/dslCompose";
import { useServerCache } from "@/lib/serverCacheStore";
import { characterDisplayName } from "@/lib/utils";

interface ContextStripProps {
  path: string;
  onOpenPanel: () => void;
}

// One chip in the summary line -- a label, an optional remove handler
// (absent for read-only chips), and an icon. The `tone` controls visual
// state:
//   - added:     an explicit, persistent-for-this-call selection
//   - removed:   a baseline item toggled off
//   - transient: a per-command-only inclusion (e.g. @mention) -- dashed
//     border + clock icon make "this won't survive the next send"
//     visible at a glance, distinct from explicit adds.
function Chip({
  icon, label, onRemove, tone = "added", title,
}: {
  icon: React.ReactNode;
  label: string;
  onRemove?: () => void;
  tone?: "added" | "removed" | "transient";
  title?: string;
}) {
  const toneColor = {
    added:     "var(--accent, var(--amber))",
    removed:   "var(--text-ghost)",
    transient: "var(--accent, var(--amber))",
  }[tone];
  const transient = tone === "transient";
  return (
    <span
      title={title}
      style={{
        display: "inline-flex", alignItems: "center", gap: 3,
        fontSize: 10.5, padding: "1px 6px 1px 5px", borderRadius: 9,
        background: "var(--surface)",
        color: toneColor,
        textDecoration: tone === "removed" ? "line-through" : "none",
        border: transient ? "1px dashed var(--accent, var(--amber))" : "none",
      }}
    >
      {icon}
      <span style={{ maxWidth: 140, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
        {label}
      </span>
      {transient && (
        <span style={{
          fontSize: 8.5, fontStyle: "italic", opacity: 0.75,
          marginLeft: 1, letterSpacing: 0.3,
        }}>
          1send
        </span>
      )}
      {onRemove && (
        <button
          onClick={(e) => { e.stopPropagation(); onRemove(); }}
          title="Remove"
          style={{
            display: "flex", border: "none", background: "none", cursor: "pointer",
            color: "var(--text-ghost)", padding: 0, marginLeft: 1, lineHeight: 1,
          }}
        >
          <X style={{ width: 9, height: 9 }} />
        </button>
      )}
    </span>
  );
}

export function ContextStrip({ path, onOpenPanel }: ContextStripProps) {
  const edits = useCallContext((s) => s.files[path]) ?? DEFAULT_EDITS;
  const mentionIds = useCallContext((s) => s.mentions[path] ?? EMPTY_MENTIONS);
  const dirty = isFileDirty(path);
  const characterBranches = useServerCache((s) => s.characterBranches);

  const resetToDefault = useCallContext((s) => s.resetToDefault);
  const setLoreEnabled = useCallContext((s) => s.setLoreEnabled);
  const resetLore = useCallContext((s) => s.resetLore);
  const setPastChaptersMode = useCallContext((s) => s.setPastChaptersMode);
  const removePinnedProgram = useCallContext((s) => s.removePinnedProgram);

  const chips = useMemo(() => {
    const out: React.ReactNode[] = [];

    if (edits.loreOverride !== null) {
      out.push(
        <Chip
          key="lore"
          icon={<BookOpen style={{ width: 10, height: 10 }} />}
          label={isLoreProgramCheckboxOwned(edits.loreOverride) ? "Story lore: partial" : "Story lore: custom"}
          tone="added"
          onRemove={() => resetLore(path)}
        />,
      );
    } else if (edits.loreEnabled !== DEFAULT_EDITS.loreEnabled) {
      out.push(
        <Chip
          key="lore"
          icon={<BookOpen style={{ width: 10, height: 10 }} />}
          label="Story lore"
          tone={edits.loreEnabled ? "added" : "removed"}
          onRemove={() => setLoreEnabled(path, !edits.loreEnabled)}
        />,
      );
    }
    if (edits.pastChaptersMode !== DEFAULT_EDITS.pastChaptersMode) {
      out.push(
        <Chip
          key="pastChapters"
          icon={<FileText style={{ width: 10, height: 10 }} />}
          label={`Past chapters: ${edits.pastChaptersMode}`}
          tone="added"
          onRemove={() => setPastChaptersMode(path, "full")}
        />,
      );
    }
    for (const name of edits.pinnedProgramNames) {
      out.push(
        <Chip
          key={`pinned-${name}`}
          icon={<Pin style={{ width: 10, height: 10 }} />}
          label={name}
          tone="added"
          onRemove={() => removePinnedProgram(path, name)}
        />,
      );
    }
    for (const id of mentionIds) {
      const branch = `character/${id}`;
      const match = characterBranches.find((cb) => cb.branch === branch);
      out.push(
        <Chip
          key={`mention-${id}`}
          icon={<Clock style={{ width: 10, height: 10 }} />}
          label={`@${characterDisplayName(branch, match?.sheet)}`}
          tone="transient"
          title="Included via @mention — added for the next send only, then cleared automatically."
        />,
      );
    }
    return out;
  }, [edits, mentionIds, characterBranches, path, setLoreEnabled, resetLore, setPastChaptersMode, removePinnedProgram]);

  const isPureDefault = !dirty;

  return (
    <div
      onClick={onOpenPanel}
      title="Click to edit context for this call"
      style={{
        flexShrink: 0,
        display: "flex", alignItems: "center", gap: 8,
        padding: "3px 12px 3px 10px",
        borderBottom: "1px solid var(--border-subtle)",
        background: dirty ? "var(--accent-tint, var(--amber-tint))" : "transparent",
        cursor: "pointer",
        userSelect: "none",
        transition: "background 0.12s",
      }}
    >
      <span
        style={{
          fontSize: 10, fontWeight: 500, letterSpacing: 0.2,
          color: dirty ? "var(--accent, var(--amber))" : "var(--text-ghost)",
          textTransform: "uppercase",
        }}
      >
        Context
      </span>

      {isPureDefault ? (
        <span style={{ fontSize: 11, color: "var(--text-dim)" }}>
          Default · lore, past chapters, style guide
        </span>
      ) : chips.length > 0 ? (
        <span style={{ display: "inline-flex", alignItems: "center", gap: 4, flexWrap: "wrap", minWidth: 0 }}>
          {chips}
        </span>
      ) : (
        <span style={{ fontSize: 11, color: "var(--accent, var(--amber))" }}>
          Custom
        </span>
      )}

      <span style={{ marginLeft: "auto", display: "flex", alignItems: "center", gap: 6 }}>
        {dirty && (
          <button
            onClick={(e) => { e.stopPropagation(); resetToDefault(path); }}
            title="Reset to default"
            style={{
              fontSize: 10, padding: "1px 6px", borderRadius: 9,
              border: "1px solid var(--border-subtle)", background: "var(--card)",
              color: "var(--text-dim)", cursor: "pointer",
            }}
          >
            Reset
          </button>
        )}
        <span
          style={{
            display: "inline-flex", alignItems: "center", gap: 3,
            fontSize: 10, color: "var(--text-ghost)",
          }}
          title="Edit as code (advanced)"
        >
          <Code2 style={{ width: 10, height: 10 }} /> DSL
        </span>
      </span>
    </div>
  );
}
