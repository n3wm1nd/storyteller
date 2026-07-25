"use client";

// Layer 1 / Layer 2 of the context UI. Layer 1 (the default view) is the
// casual editor: plain-language toggles, no DSL visible. Layer 2 (the
// "Edit as code" view, behind one click) is the power-user surface: a
// small pinned-snippet editor + a saved-snippets library.
//
// The panel mounts as an expandable region above the InputBar's textarea
// (fileview.tsx). It's dismissable (Esc, click-outside, or the close
// button); state changes persist in callContextStore regardless of
// whether the panel is open, so closing it doesn't lose edits.
//
// The casual editor's vocabulary is deliberately small now -- a lore
// toggle, a past-chapters mode toggle, and a list of named pinned
// snippets (e.g. "rules.magic") to fold into this call's authors-notes
// content. Style, character identity, and "other notes" are entirely
// agent-owned (see Server.Writer.File.Protocol's own Haddock on
// `ChatWriter`'s three wire slots) -- there's no cast-list picker or
// extra-file picker anymore; full per-call DSL control over the *entire*
// writer context was rolled back (see dslCompose.ts's own header), and
// those two pickers went with it. They may come back in a narrower form
// later, but aren't part of this pass.
//
// "Save as..." authors a small snippet (a single 0-arity Context DSL
// definition, e.g. `"the rules of magic"` or a real `read`/`for` body) on
// the contexts branch, then adds its name to this file's
// `pinnedProgramNames` -- the direct replacement for the old "load a
// whole context program" flow.

import { useEffect, useState } from "react";
import {
  BookOpen, FileText, X, Save, Code2, ChevronLeft, ChevronRight, Pencil, Check,
} from "lucide-react";
import { useCallContext } from "@/lib/callContextStore";
import { DEFAULT_EDITS, parseLoreProgram, type PastChaptersMode } from "@/lib/dslCompose";
import { writeContextFunction, readContextFunction, slugifyFunctionName, isValidFunctionName } from "@/lib/contextBranch";
import { CONTEXT_DEFAULT_SOURCE } from "@/lib/contextDefaults";
import { setError } from "@/lib/uiStore";
import { DSLEditor } from "./dsl-editor";
import { ContextLibrary } from "./context-library";
import { CodeCostEditor, useAdhocCostFetcher } from "./code-cost-editor";
import { useLoreTree, flattenLore } from "./lore-selector";

interface ContextPanelProps {
  path: string;
  branch: string;
  onClose: () => void;
}

// ─── Toggle (baseline) ────────────────────────────────────────────────────

function ToggleRow({
  icon, label, hint, checked, onChange,
}: {
  icon: React.ReactNode;
  label: string;
  hint: string;
  checked: boolean;
  onChange: (next: boolean) => void;
}) {
  return (
    <label
      style={{
        display: "flex", alignItems: "center", gap: 8,
        padding: "6px 4px", cursor: "pointer", userSelect: "none",
      }}
    >
      <input
        type="checkbox"
        checked={checked}
        onChange={(e) => onChange(e.target.checked)}
        style={{ accentColor: "var(--accent, var(--amber))" }}
      />
      <span style={{ display: "flex", alignItems: "center", gap: 6, color: checked ? "var(--foreground)" : "var(--text-dim)" }}>
        {icon}
        <span>
          <div style={{ fontSize: 12 }}>{label}</div>
          <div style={{ fontSize: 10, color: "var(--text-ghost)" }}>{hint}</div>
        </span>
      </span>
    </label>
  );
}

// ─── Lore row (toggle + inline expandable override editor) ───────────────

// "Story lore" isn't just an on/off switch -- clicking the row's own label
// (not the checkbox) expands it into a real DSLFileEditor (draft-backed
// mode) over this call's `context.lore` override (see dslCompose.ts's
// `loreOverride` field/`synthesizeLoreOverride`). What it's editing is
// always explicit: the section header inside the expansion names the
// exact wire field (`lore`) this text becomes.
//
// The source file is ground truth: with no per-call override yet, the
// draft starts from this project's own committed `context/lore.dsl` if
// one exists, else the compiled-in default's mirrored source (see
// lib/contextDefaults.ts's own header on why that's a hand-kept JS copy,
// not something fetched live). Checking a box regenerates `loreOverride`
// via `renderLoreProgram` (dslCompose.ts) and shows the generated code
// live -- "quick toggle" and "see/edit the code" are the same surface.
// Checkbox state itself is never separately stored: it's recovered by
// *parsing* the current draft (`parseLoreProgram`) on every render, so a
// hand edit that doesn't match the generator's own shape leaves the
// checkboxes with nothing truthful to show, and they go inert -- see
// dslCompose.ts's own header on why this replaced an earlier, broken
// `exclude()`-based attempt.
function LoreRow({ path, branch }: { path: string; branch: string }) {
  const edits = useCallContext((s) => s.files[path]) ?? DEFAULT_EDITS;
  const setLoreEnabled = useCallContext((s) => s.setLoreEnabled);
  const setLoreOverride = useCallContext((s) => s.setLoreOverride);
  const toggleLorePath = useCallContext((s) => s.toggleLorePath);
  const resetLore = useCallContext((s) => s.resetLore);
  const [expanded, setExpanded] = useState(false);
  const fetchCosts = useAdhocCostFetcher(expanded ? branch : null);
  const loreTree = useLoreTree(expanded ? branch : null);
  // The file currently being written is never a checkbox choice here --
  // the server always excludes it from context.lore on its own (see
  // Storyteller.Context.DSL.Library.contextLoreWithoutDef), since
  // writeAgent already frames it separately as the file being continued.
  // Offering it as an includable/excludable lore entry would be
  // misleading either way: checked, it wouldn't actually add anything
  // (already-excluded server-side); unchecked, there'd be nothing for
  // toggling it to do.
  const allEntries = flattenLore(loreTree).filter((e) => e.path !== path);
  const allPaths = allEntries.map((e) => e.path);

  // Ground truth for "no override touched yet": the committed project
  // file, or (only if nothing was ever committed) the mirrored default
  // source -- never a bare `context.lore` name reference, which resolves
  // correctly server-side at send time but can't be shown/edited here as
  // if it were real content of its own (see contextDefaults.ts).
  const [committedDefault, setCommittedDefault] = useState<string | null | undefined>(undefined);
  useEffect(() => {
    let cancelled = false;
    setCommittedDefault(undefined);
    readContextFunction("lore")
      .then((text) => { if (!cancelled) setCommittedDefault(text); })
      .catch(() => { if (!cancelled) setCommittedDefault(null); });
    return () => { cancelled = true; };
  }, [branch]);
  const resetTarget = committedDefault ?? CONTEXT_DEFAULT_SOURCE.lore ?? "";

  const hasOverride = edits.loreOverride !== null;
  const draft = edits.loreOverride ?? resetTarget;
  // parseLoreProgram matches a checkbox-generated PREFIX of the draft --
  // an exact generated block, or nothing at all (zero paths). There is no
  // "conflict"/disabled state: real default source (contextLoreDef's own
  // glob-walking body included) that happens to share the generator's
  // banner line just reads as zero paths, same as any other text with no
  // recognized prefix -- see dslCompose.ts's own header.
  const checkedPaths = parseLoreProgram(draft);

  return (
    <div style={{ borderRadius: 5, background: expanded ? "var(--card)" : "transparent", overflow: "hidden" }}>
      <div style={{ display: "flex", alignItems: "center", gap: 4 }}>
        <input
          type="checkbox"
          checked={edits.loreEnabled}
          onChange={(e) => setLoreEnabled(path, e.target.checked)}
          style={{ accentColor: "var(--accent, var(--amber))", marginLeft: 4 }}
        />
        <button
          onClick={() => setExpanded((v) => !v)}
          title={expanded ? "Collapse" : "Choose which lore entries to include, or edit the block directly"}
          style={{
            display: "flex", alignItems: "center", gap: 6, flex: 1, minWidth: 0,
            padding: "6px 4px", border: "none", background: "none", textAlign: "left", cursor: "pointer",
            color: edits.loreEnabled ? "var(--foreground)" : "var(--text-dim)",
          }}
        >
          <BookOpen style={{ width: 11, height: 11 }} />
          <span>
            <div style={{ fontSize: 12, display: "flex", alignItems: "center", gap: 5 }}>
              Story lore
              {hasOverride && (
                <span style={{
                  fontSize: 9, padding: "1px 5px", borderRadius: 7,
                  background: "var(--accent-tint, var(--amber-tint))", color: "var(--accent, var(--amber))",
                }}>
                  {`${checkedPaths.length}/${allPaths.length}`}
                </span>
              )}
            </div>
            <div style={{ fontSize: 10, color: "var(--text-ghost)" }}>
              {hasOverride ? "Some entries excluded for this call" : "This project's context.lore"}
            </div>
          </span>
          <ChevronRight style={{
            width: 10, height: 10, marginLeft: "auto", flexShrink: 0,
            transform: expanded ? "rotate(90deg)" : "none", transition: "transform 0.15s",
          }} />
        </button>
      </div>

      {expanded && (
        <div style={{ padding: "0 6px 8px", display: "flex", flexDirection: "column", gap: 8 }}>
          {allEntries.length === 0 ? (
            <div style={{ fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic", padding: "2px 2px" }}>
              {loreTree.length === 0 ? "Loading lore entries…" : "No lore entries on this branch yet."}
            </div>
          ) : (
            <div>
              <div style={{ display: "flex", alignItems: "center", gap: 6, padding: "2px 2px 4px" }}>
                <span style={{ fontSize: 10, color: "var(--text-ghost)", flex: 1 }}>
                  {checkedPaths.length} of {allPaths.length} included
                </span>
              </div>
              <div style={{
                display: "flex", flexDirection: "column", gap: 1, maxHeight: 130, overflowY: "auto",
                border: "1px solid var(--border-subtle)", borderRadius: 5, padding: 3,
              }}>
                {allEntries.map((entry) => {
                  const included = checkedPaths.includes(entry.path);
                  return (
                    <label
                      key={entry.path}
                      title={entry.path}
                      style={{
                        display: "flex", alignItems: "center", gap: 6, padding: "3px 5px", borderRadius: 3,
                        cursor: "pointer",
                      }}
                    >
                      <input
                        type="checkbox"
                        checked={included}
                        onChange={() => toggleLorePath(path, resetTarget, entry.path)}
                        style={{ accentColor: "var(--accent, var(--amber))" }}
                      />
                      <span style={{
                        fontSize: 11, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
                        color: included ? "var(--foreground)" : "var(--text-ghost)",
                      }}>
                        {entry.path}
                      </span>
                    </label>
                  );
                })}
              </div>
            </div>
          )}

          <div>
            <div style={{
              display: "flex", alignItems: "center", gap: 6, fontSize: 10, color: "var(--text-ghost)",
              padding: "2px 2px 5px",
            }}>
              <Pencil style={{ width: 9, height: 9 }} />
              This call&apos;s <code style={{ fontFamily: "monospace" }}>lore</code> wire field, generated from the
              checkboxes above — edit directly to take full control instead.
            </div>
            <CodeCostEditor
              value={draft}
              onChange={(next) => setLoreOverride(path, next === resetTarget ? null : next)}
              placeholder='e.g. read "lore/**"'
              fetchCosts={fetchCosts}
              minHeight="90px"
            />
          </div>
          {hasOverride && (
            <button
              onClick={() => resetLore(path)}
              style={{ ...dialogBtnStyle, alignSelf: "flex-start", display: "inline-flex", alignItems: "center", gap: 4 }}
            >
              <Check style={{ width: 10, height: 10 }} /> Reset to default
            </button>
          )}
        </div>
      )}
    </div>
  );
}

// ─── Save-as dialog ───────────────────────────────────────────────────────

function SaveAsDialog({
  onCancel, onSave,
}: {
  onCancel: () => void;
  onSave: (name: string, source: string) => void;
}) {
  const [raw, setRaw] = useState("");
  const [source, setSource] = useState('"the rules of magic"\n');
  const slug = slugifyFunctionName(raw);
  const valid = slug.length > 0 && isValidFunctionName(slug) && source.trim().length > 0;
  return (
    <div style={{
      display: "flex", flexDirection: "column", gap: 6,
      padding: 8, background: "var(--card)", border: "1px solid var(--border-subtle)", borderRadius: 5,
    }}>
      <div style={{ fontSize: 11, color: "var(--text-secondary)" }}>
        Save a snippet to pin whenever it's relevant:
      </div>
      <div style={{ display: "flex", gap: 4, alignItems: "center" }}>
        <input
          value={raw}
          onChange={(e) => setRaw(e.target.value)}
          placeholder="e.g. rules.magic"
          autoFocus
          onKeyDown={(e) => { if (e.key === "Escape") onCancel(); }}
          style={{
            flex: 1, padding: "3px 6px", fontSize: 11,
            border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
            color: "var(--foreground)", borderRadius: 4, outline: "none",
          }}
        />
        <code style={{ fontSize: 10, color: slug ? "var(--text-dim)" : "var(--text-ghost)", fontFamily: "monospace", minWidth: 80 }}>
          {slug || "—"}
        </code>
      </div>
      <textarea
        value={source}
        onChange={(e) => setSource(e.target.value)}
        spellCheck={false}
        placeholder='"the rules of magic"'
        style={{
          minHeight: 70, padding: "4px 6px", fontFamily: "monospace", fontSize: 11,
          border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
          color: "var(--foreground)", borderRadius: 4, outline: "none", resize: "vertical",
        }}
      />
      <div style={{ display: "flex", gap: 4, justifyContent: "flex-end" }}>
        <button onClick={onCancel} style={dialogBtnStyle}>Cancel</button>
        <button
          onClick={() => valid && onSave(slug, source)}
          disabled={!valid}
          style={{
            ...dialogBtnStyle,
            background: valid ? "var(--accent, var(--amber))" : "var(--surface)",
            color: valid ? "var(--surface-deep)" : "var(--text-ghost)",
            border: "1px solid var(--accent, var(--amber))",
            cursor: valid ? "pointer" : "default",
          }}
        >
          Save
        </button>
      </div>
    </div>
  );
}

const dialogBtnStyle: React.CSSProperties = {
  fontSize: 10.5, padding: "2px 8px", borderRadius: 4,
  border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
  color: "var(--text-secondary)", cursor: "pointer",
};

// ─── Main panel ───────────────────────────────────────────────────────────

export function ContextPanel({ path, branch, onClose }: ContextPanelProps) {
  const edits = useCallContext((s) => s.files[path]) ?? DEFAULT_EDITS;
  const setPastChaptersMode = useCallContext((s) => s.setPastChaptersMode);
  const addPinnedProgram = useCallContext((s) => s.addPinnedProgram);
  const removePinnedProgram = useCallContext((s) => s.removePinnedProgram);
  const resetToDefault = useCallContext((s) => s.resetToDefault);

  const [view, setView] = useState<"casual" | "dsl">("casual");
  const [saveAsOpen, setSaveAsOpen] = useState(false);
  const [saving, setSaving] = useState(false);

  // Esc closes the panel.
  useEffect(() => {
    function onKey(e: KeyboardEvent) {
      if (e.key === "Escape") onClose();
    }
    document.addEventListener("keydown", onKey);
    return () => document.removeEventListener("keydown", onKey);
  }, [onClose]);

  async function handleSaveAs(name: string, source: string) {
    setSaving(true);
    try {
      await writeContextFunction(name, source);
      addPinnedProgram(path, name);
      setSaveAsOpen(false);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  return (
    <div
      style={{
        flexShrink: 0, maxHeight: 360, display: "flex", flexDirection: "column",
        borderBottom: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
      }}
    >
      {/* Header */}
      <div style={{
        display: "flex", alignItems: "center", gap: 6, padding: "5px 10px",
        borderBottom: "1px solid var(--border-subtle)",
      }}>
        <span style={{ fontSize: 11, fontWeight: 500, color: "var(--foreground)", flex: 1 }}>
          Context for this call
        </span>
        {view === "dsl" && (
          <button onClick={() => setView("casual")} style={headerBtnStyle}>
            <ChevronLeft style={{ width: 10, height: 10 }} /> Back
          </button>
        )}
        <button
          onClick={() => setView((v) => (v === "casual" ? "dsl" : "casual"))}
          title="Edit pinned snippets as DSL (advanced)"
          style={{
            ...headerBtnStyle,
            background: view === "dsl" ? "var(--accent-tint, var(--amber-tint))" : "transparent",
            color: view === "dsl" ? "var(--accent, var(--amber))" : "var(--text-dim)",
          }}
        >
          <Code2 style={{ width: 10, height: 10 }} /> DSL
        </button>
        <button onClick={onClose} title="Close" style={headerBtnStyle}>
          <X style={{ width: 11, height: 11 }} />
        </button>
      </div>

      {/* Body */}
      <div style={{ flex: 1, overflowY: "auto", padding: "8px 12px" }}>
        {view === "dsl" ? (
          // Layer 2 -- power-user
          <PowerUserView path={path} branch={branch} />
        ) : saveAsOpen ? (
          <SaveAsDialog onCancel={() => setSaveAsOpen(false)} onSave={handleSaveAs} />
        ) : (
          // Layer 1 -- casual editor
          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            <section>
              <SectionLabel>Standing context</SectionLabel>
              <div style={{ display: "flex", flexDirection: "column" }}>
                <LoreRow path={path} branch={branch} />
                <label style={{
                  display: "flex", alignItems: "center", gap: 8,
                  padding: "6px 4px", userSelect: "none",
                }}>
                  <FileText style={{ width: 11, height: 11, color: "var(--text-dim)" }} />
                  <span style={{ fontSize: 12, color: "var(--foreground)" }}>Past chapters</span>
                  <select
                    value={edits.pastChaptersMode}
                    onChange={(e) => setPastChaptersMode(path, e.target.value as PastChaptersMode)}
                    style={{
                      marginLeft: "auto", fontSize: 10.5, padding: "1px 4px", borderRadius: 3,
                      border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
                      color: "var(--text-secondary)",
                    }}
                  >
                    <option value="full">Full</option>
                    <option value="compressed">Compressed</option>
                  </select>
                </label>
              </div>
            </section>

            <section>
              <SectionLabel>Pinned snippets</SectionLabel>
              {edits.pinnedProgramNames.length === 0 ? (
                <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", padding: "4px 0" }}>
                  Nothing pinned for this call.
                </div>
              ) : (
                <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
                  {edits.pinnedProgramNames.map((name) => (
                    <div key={name} style={{
                      display: "flex", alignItems: "center", gap: 6, padding: "3px 6px",
                      background: "var(--card)", borderRadius: 5, fontSize: 11,
                    }}>
                      <code style={{ flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "monospace" }}>
                        {name}
                      </code>
                      <button
                        onClick={() => removePinnedProgram(path, name)}
                        title="Remove"
                        style={{ border: "none", background: "none", cursor: "pointer", color: "var(--text-ghost)", padding: 2 }}
                      >
                        <X style={{ width: 10, height: 10 }} />
                      </button>
                    </div>
                  ))}
                </div>
              )}
              <div style={{ marginTop: 4 }}>
                <SectionLabel>Saved snippets</SectionLabel>
                <ContextLibrary path={path} />
              </div>
            </section>

            <div style={{
              display: "flex", gap: 6, alignItems: "center",
              padding: "6px 0 0", borderTop: "1px solid var(--border-subtle)",
            }}>
              <button
                onClick={() => setSaveAsOpen(true)}
                disabled={saving}
                style={{
                  ...dialogBtnStyle,
                  display: "inline-flex", alignItems: "center", gap: 4,
                  opacity: saving ? 0.6 : 1,
                }}
              >
                <Save style={{ width: 10, height: 10 }} /> Save new snippet…
              </button>
              <span style={{ flex: 1 }} />
              <button
                onClick={() => resetToDefault(path)}
                style={dialogBtnStyle}
              >
                Reset
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

const headerBtnStyle: React.CSSProperties = {
  display: "inline-flex", alignItems: "center", gap: 3,
  fontSize: 10.5, padding: "2px 7px", borderRadius: 4,
  border: "1px solid var(--border-subtle)", background: "transparent",
  color: "var(--text-dim)", cursor: "pointer",
};

function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <div style={{
      fontSize: 9.5, fontWeight: 500, letterSpacing: 0.4, textTransform: "uppercase",
      color: "var(--text-ghost)", marginBottom: 4,
    }}>
      {children}
    </div>
  );
}

// ─── Power-user view (DSL editor + library) ───────────────────────────────

function PowerUserView({ path, branch }: { path: string; branch: string }) {
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
      <DSLEditor path={path} branch={branch} />
      <div>
        <SectionLabel>Saved snippets</SectionLabel>
        <ContextLibrary path={path} />
      </div>
    </div>
  );
}
