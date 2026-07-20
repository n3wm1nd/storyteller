"use client";

// Layer 0 of the context UI: the always-visible one-line summary above
// the InputBar. The casual user's entire exposure to "what the LLM will
// see" -- reads as a status line, not as a DSL anything.
//
// Three presentation modes, driven by callContextStore:
//
//   - default + no mentions:
//       "Default context (lore · chapters · style)"
//       No affordance to do anything; clicking opens the panel where
//       the user can edit. This is what 99% of sends look like.
//
//   - default + mentions, or transient:
//       "Custom: <chips for what's added/removed>"
//       Each chip is removable inline (×); removing the last one
//       returns to default. A subtle "edited" border on the strip
//       itself flags the change without making it loud.
//
//   - named:
//       "Using: <function name>"
//       With a clear way back to default ("Clear") so a user doesn't
//       feel trapped in their own saved function.
//
// "Edit as code →" lives in the corner, behind an explicit click --
// power-user territory, never the default surface. Mentions and pins
// are surfaced as natural-language counts, not DSL.
//
// Styling matches the existing InputBar context strip (the "N atoms
// selected" line above the textarea, see fileview.tsx's InputBar): same
// font sizes, same amber accent for "selection present" affordances,
// same clickable × to clear. The new strip stacks above any existing
// selection/rebase strips in the InputBar.

import { useMemo } from "react";
import { BookOpen, FileText, Sparkles, Users, X, Code2, Library, Clock } from "lucide-react";
import { useCallContext, isFileDirty, EMPTY_MENTIONS } from "@/lib/callContextStore";
import { DEFAULT_EDITS, type CharacterAdd } from "@/lib/dslCompose";
import type { CharacterSummary } from "@/lib/ws";
import { useServerCache } from "@/lib/serverCacheStore";
import { characterDisplayName } from "@/lib/utils";

interface ContextStripProps {
  path: string;
  onOpenPanel: () => void;
}

// One chip in the summary line -- a label, an optional remove handler
// (absent for read-only chips like the baseline default), and an icon.
// The `tone` controls visual state:
//   - default: read-only baseline item (e.g. "Story lore" pre-toggle)
//   - added:   an explicit, persistent-for-this-call selection
//   - removed: a baseline item toggled off
//   - named:   a loaded saved function
//   - transient: a per-command-only inclusion (e.g. @mention) -- dashed
//     border + clock icon make "this won't survive the next send"
//     visible at a glance, distinct from explicit adds.
function Chip({
  icon, label, onRemove, tone = "default", title,
}: {
  icon: React.ReactNode;
  label: string;
  onRemove?: () => void;
  tone?: "default" | "added" | "removed" | "named" | "transient";
  title?: string;
}) {
  const toneColor = {
    default:   "var(--text-dim)",
    added:     "var(--accent, var(--amber))",
    removed:   "var(--text-ghost)",
    named:     "var(--accent, var(--amber))",
    transient: "var(--accent, var(--amber))",
  }[tone];
  const transient = tone === "transient";
  return (
    <span
      title={title}
      style={{
        display: "inline-flex", alignItems: "center", gap: 3,
        fontSize: 10.5, padding: "1px 6px 1px 5px", borderRadius: 9,
        background: tone === "default" ? "transparent" : "var(--surface)",
        color: toneColor,
        textDecoration: tone === "removed" ? "line-through" : "none",
        // Dashed border marks this chip as impermanent -- the only
        // visually-distinct tone in the strip, so a glance tells you
        // "this goes away after the next send" without reading.
        border: transient ? "1px dashed var(--accent, var(--amber))" : "none",
      }}
    >
      {icon}
      <span style={{ maxWidth: 140, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
        {label}
      </span>
      {/* The "1 send" badge is the explicit signal: this inclusion is
          bound to the next send, not a standing selection. */}
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

function characterLabel(c: CharacterAdd, characterBranches: CharacterSummary[]): string {
  const branch = `character/${c.id}`;
  const match = characterBranches.find((cb) => cb.branch === branch);
  return characterDisplayName(branch, match?.sheet);
}

export function ContextStrip({ path, onOpenPanel }: ContextStripProps) {
  const fileState = useCallContext((s) => s.files[path]);
  const mentionIds = useCallContext((s) => s.mentions[path] ?? []);
  const dirty = isFileDirty(path);
  const characterBranches = useServerCache((s) => s.characterBranches);

  const resetToDefault = useCallContext((s) => s.resetToDefault);
  const clearNamed = useCallContext((s) => s.clearNamed);
  const setEdits = useCallContext((s) => s.setEdits);
  const patchEdits = useCallContext((s) => s.patchEdits);

  // Build the chip list once per render. Each baseline toggle that
  // differs from default gets a chip (added or removed). Characters and
  // extra files always show (they have no "default" state). Mentions
  // appear with an `@` prefix so they read as "added by mention".
  const chips = useMemo(() => {
    const out: React.ReactNode[] = [];
    const edits = fileState?.edits ?? DEFAULT_EDITS;

    if (fileState?.mode !== "named") {
      // Baseline diffs vs. default
      if (edits.baseline.lore !== DEFAULT_EDITS.baseline.lore) {
        out.push(
          <Chip
            key="lore"
            icon={<BookOpen style={{ width: 10, height: 10 }} />}
            label="Story lore"
            tone={edits.baseline.lore ? "added" : "removed"}
            onRemove={() => patchEdits(path, { baseline: { ...edits.baseline, lore: !edits.baseline.lore } })}
          />,
        );
      }
      if (edits.baseline.chapters !== DEFAULT_EDITS.baseline.chapters) {
        out.push(
          <Chip
            key="chapters"
            icon={<FileText style={{ width: 10, height: 10 }} />}
            label="Past chapters"
            tone={edits.baseline.chapters ? "added" : "removed"}
            onRemove={() => patchEdits(path, { baseline: { ...edits.baseline, chapters: !edits.baseline.chapters } })}
          />,
        );
      }
      if (edits.baseline.style !== DEFAULT_EDITS.baseline.style) {
        out.push(
          <Chip
            key="style"
            icon={<Sparkles style={{ width: 10, height: 10 }} />}
            label="Style guide"
            tone={edits.baseline.style ? "added" : "removed"}
            onRemove={() => patchEdits(path, { baseline: { ...edits.baseline, style: !edits.baseline.style } })}
          />,
        );
      }
      // Explicit character adds
      for (const c of edits.characters) {
        out.push(
          <Chip
            key={`char-${c.id}`}
            icon={<Users style={{ width: 10, height: 10 }} />}
            label={characterLabel(c, characterBranches)}
            tone="added"
            onRemove={() =>
              setEdits(path, {
                ...edits,
                characters: edits.characters.filter((x) => x.id !== c.id),
              })
            }
          />,
        );
      }
      // Mention-driven adds
      for (const id of mentionIds) {
        if (edits.characters.some((c) => c.id === id)) continue; // already shown above
        out.push(
          <Chip
            key={`mention-${id}`}
            icon={<Clock style={{ width: 10, height: 10 }} />}
            label={`@${characterLabel({ id, depth: "blurb" }, characterBranches)}`}
            tone="transient"
            title={`Included via @mention — added for the next send only, then cleared automatically. (Make it permanent: add the character in the panel.)`}
          />,
        );
      }
      // Explicit extra files
      for (const f of edits.extraFiles) {
        out.push(
          <Chip
            key={`file-${f.path}`}
            icon={<FileText style={{ width: 10, height: 10 }} />}
            label={f.asName ?? f.path.split("/").pop()!}
            tone="added"
            onRemove={() =>
              setEdits(path, {
                ...edits,
                extraFiles: edits.extraFiles.filter((x) => x.path !== f.path),
              })
            }
          />,
        );
      }
    }
    return out;
  }, [fileState, mentionIds, characterBranches, path, patchEdits, setEdits]);

  // Empty-by-design chips list + no mentions + no named function = "pure
  // default" presentation -- the resting state, no edits shown.
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

      {fileState?.mode === "named" ? (
        <>
          <Library style={{ width: 11, height: 11, color: "var(--accent, var(--amber))" }} />
          <span style={{ fontSize: 11, color: "var(--foreground)" }}>
            Using <code style={{ fontFamily: "monospace", fontSize: 10.5 }}>{fileState.namedName}</code>
          </span>
          <button
            onClick={(e) => { e.stopPropagation(); clearNamed(path); }}
            title="Stop using this function — return to default"
            style={{
              display: "flex", alignItems: "center", gap: 3,
              fontSize: 10, padding: "1px 6px", borderRadius: 9,
              border: "1px solid var(--border-subtle)", background: "var(--card)",
              color: "var(--text-dim)", cursor: "pointer",
            }}
          >
            <X style={{ width: 9, height: 9 }} /> Clear
          </button>
        </>
      ) : isPureDefault ? (
        <span style={{ fontSize: 11, color: "var(--text-dim)" }}>
          Default · lore, past chapters, style guide
        </span>
      ) : chips.length > 0 ? (
        <span style={{ display: "inline-flex", alignItems: "center", gap: 4, flexWrap: "wrap", minWidth: 0 }}>
          {chips}
        </span>
      ) : (
        // Dirty but no chips (e.g. a mention is the only addition) —
        // surface a generic "custom" label so the strip still reads as
        // changed.
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
