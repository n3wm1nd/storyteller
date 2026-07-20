"use client";

// Layer 1 / Layer 2 of the context UI. Layer 1 (the default view) is
// the casual editor: plain-language toggles and pickers, no DSL
// visible. Layer 2 (the "Edit as code" view, behind one click) is the
// power-user surface: the actual DSL text + a saved-functions library.
//
// The panel mounts as an expandable region above the InputBar's
// textarea (fileview.tsx). It's dismissable (Esc, click-outside, or the
// close button); state changes persist in callContextStore regardless
// of whether the panel is open, so closing it doesn't lose edits.
//
// The casual editor's vocabulary is deliberately small -- three
// baseline toggles ("Story lore", "Past chapters", "Style guide"), one
// character multi-select with depth, one file picker for extras. Every
// selection is positive ("include this") -- there's no negation in the
// casual UI. Removing a default-on baseline item is a toggle, not a
// special "exclusion" concept. Anything more nuanced (a glob exclusion,
// a for-loop, a cross-branch read) is the power-user's job in Layer 2.
//
// "Save as..." promotes the current structured state to a named
// function on the contexts branch (lib/contextBranch.ts) -- the user
// names it, we synthesize DSL source, write the file, and switch this
// file's mode to "named" with that name loaded. The casual state is
// then dormant until "Clear named function" or "Reset to default".

import { useEffect, useMemo, useState } from "react";
import {
  BookOpen, FileText, Sparkles, Users, X, Plus, Save, Code2,
  ChevronLeft, Search, AlertCircle,
} from "lucide-react";
import { useCallContext } from "@/lib/callContextStore";
import {
  DEFAULT_EDITS, type ContextEdits, type CharacterAdd, type FileAdd,
} from "@/lib/dslCompose";
import type { CharacterSummary, LoreNode } from "@/lib/ws";
import { useServerCache } from "@/lib/serverCacheStore";
import { characterDisplayName } from "@/lib/utils";
import {
  writeContextFunction, slugifyFunctionName, isValidFunctionName,
} from "@/lib/contextBranch";
import { setError } from "@/lib/uiStore";
import { useLoreTree } from "./lore-selector";
import { DSLEditor } from "./dsl-editor";
import { ContextLibrary } from "./context-library";

interface ContextPanelProps {
  path: string;
  activeBranch: string | null;
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

// ─── Character picker ─────────────────────────────────────────────────────

function CharacterPicker({
  characters, characterBranches, onAdd, onRemove, onDepthChange,
}: {
  characters: CharacterAdd[];
  characterBranches: CharacterSummary[];
  onAdd: (c: CharacterAdd) => void;
  onRemove: (id: string) => void;
  onDepthChange: (id: string, depth: "blurb" | "full") => void;
}) {
  const [pickerOpen, setPickerOpen] = useState(false);
  const available = characterBranches
    .map((cb) => cb.branch.replace(/^character\//, ""))
    .filter((id) => !characters.some((c) => c.id === id));

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
      {characters.length === 0 ? (
        <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", padding: "4px 0" }}>
          No characters added — the scene's present characters are still included by default.
        </div>
      ) : (
        characters.map((c) => {
          const branch = `character/${c.id}`;
          return (
            <div key={c.id} style={{
              display: "flex", alignItems: "center", gap: 6, padding: "3px 6px",
              background: "var(--card)", borderRadius: 5, fontSize: 11,
            }}>
              <Users style={{ width: 10, height: 10, color: "var(--text-dim)" }} />
              <span style={{ flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                {characterDisplayName(branch,
                  characterBranches.find((cb) => cb.branch === branch)?.sheet)}
              </span>
              <select
                value={c.depth}
                onChange={(e) => onDepthChange(c.id, e.target.value as "blurb" | "full")}
                title="How much of this character to include"
                style={{
                  fontSize: 10, padding: "1px 3px", borderRadius: 3,
                  border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
                  color: "var(--text-secondary)",
                }}
              >
                <option value="blurb">Brief</option>
                <option value="full">Full</option>
              </select>
              <button
                onClick={() => onRemove(c.id)}
                title="Remove"
                style={{ border: "none", background: "none", cursor: "pointer", color: "var(--text-ghost)", padding: 2 }}
              >
                <X style={{ width: 10, height: 10 }} />
              </button>
            </div>
          );
        })
      )}
      <div style={{ position: "relative" }}>
        <button
          onClick={() => setPickerOpen((v) => !v)}
          disabled={available.length === 0}
          style={{
            display: "inline-flex", alignItems: "center", gap: 4,
            fontSize: 10.5, padding: "2px 8px", borderRadius: 4,
            border: "1px dashed var(--border-subtle)", background: "transparent",
            color: available.length === 0 ? "var(--text-ghost)" : "var(--text-dim)",
            cursor: available.length === 0 ? "default" : "pointer",
          }}
        >
          <Plus style={{ width: 10, height: 10 }} /> Add character
        </button>
        {pickerOpen && available.length > 0 && (
          <div style={{
            position: "absolute", bottom: "100%", left: 0, marginBottom: 4,
            background: "var(--surface-deep)", border: "1px solid var(--border-subtle)",
            borderRadius: 5, boxShadow: "0 4px 12px rgba(0,0,0,0.3)",
            maxHeight: 200, overflowY: "auto", minWidth: 180, zIndex: 10,
          }}>
            {available.map((id) => {
              const branch = `character/${id}`;
              return (
                <button
                  key={id}
                  onClick={() => { onAdd({ id, depth: "blurb" }); setPickerOpen(false); }}
                  style={{
                    display: "block", width: "100%", textAlign: "left",
                    padding: "4px 8px", border: "none", background: "transparent",
                    cursor: "pointer", color: "var(--text-secondary)", fontSize: 11,
                  }}
                >
                  {characterDisplayName(branch,
                    characterBranches.find((cb) => cb.branch === branch)?.sheet)}
                </button>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
}

// ─── File picker (extras) ─────────────────────────────────────────────────

function FilePicker({
  extras, loreEntries, onAdd, onRemove,
}: {
  extras: FileAdd[];
  loreEntries: LoreNode[];
  onAdd: (f: FileAdd) => void;
  onRemove: (path: string) => void;
}) {
  const [pickerOpen, setPickerOpen] = useState(false);
  const [filter, setFilter] = useState("");

  const flatPaths = useMemo(() => {
    const out: string[] = [];
    const walk = (nodes: LoreNode[]) => {
      for (const n of nodes) {
        if (n.children.length === 0) out.push(n.path);
        else walk(n.children);
      }
    };
    walk(loreEntries);
    return out;
  }, [loreEntries]);

  const filtered = filter
    ? flatPaths.filter((p) => p.toLowerCase().includes(filter.toLowerCase()))
    : flatPaths;

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 4 }}>
      {extras.length === 0 ? (
        <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", padding: "4px 0" }}>
          No extra files — anything under <code>lore/</code> or <code>chapters/</code> is already included.
        </div>
      ) : (
        extras.map((f) => (
          <div key={f.path} style={{
            display: "flex", alignItems: "center", gap: 6, padding: "3px 6px",
            background: "var(--card)", borderRadius: 5, fontSize: 11,
          }}>
            <FileText style={{ width: 10, height: 10, color: "var(--text-dim)" }} />
            <code style={{ flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap", fontFamily: "monospace" }}>
              {f.path}
            </code>
            <button
              onClick={() => onRemove(f.path)}
              title="Remove"
              style={{ border: "none", background: "none", cursor: "pointer", color: "var(--text-ghost)", padding: 2 }}
            >
              <X style={{ width: 10, height: 10 }} />
            </button>
          </div>
        ))
      )}
      <div style={{ position: "relative" }}>
        <button
          onClick={() => setPickerOpen((v) => !v)}
          disabled={flatPaths.length === 0}
          style={{
            display: "inline-flex", alignItems: "center", gap: 4,
            fontSize: 10.5, padding: "2px 8px", borderRadius: 4,
            border: "1px dashed var(--border-subtle)", background: "transparent",
            color: flatPaths.length === 0 ? "var(--text-ghost)" : "var(--text-dim)",
            cursor: flatPaths.length === 0 ? "default" : "pointer",
          }}
        >
          <Plus style={{ width: 10, height: 10 }} /> Add file
        </button>
        {pickerOpen && (
          <div style={{
            position: "absolute", bottom: "100%", left: 0, marginBottom: 4,
            background: "var(--surface-deep)", border: "1px solid var(--border-subtle)",
            borderRadius: 5, boxShadow: "0 4px 12px rgba(0,0,0,0.3)",
            maxHeight: 240, overflowY: "auto", minWidth: 240, zIndex: 10,
            display: "flex", flexDirection: "column",
          }}>
            <div style={{
              display: "flex", alignItems: "center", gap: 4,
              padding: "4px 6px", borderBottom: "1px solid var(--border-subtle)",
            }}>
              <Search style={{ width: 10, height: 10, color: "var(--text-ghost)" }} />
              <input
                value={filter}
                onChange={(e) => setFilter(e.target.value)}
                placeholder="filter…"
                autoFocus
                style={{
                  flex: 1, minWidth: 0, border: "none", outline: "none",
                  background: "transparent", color: "var(--foreground)", fontSize: 11,
                }}
              />
            </div>
            <div style={{ flex: 1, overflowY: "auto" }}>
              {filtered.length === 0 ? (
                <div style={{ padding: 8, fontSize: 10, color: "var(--text-ghost)" }}>no matches</div>
              ) : filtered.slice(0, 80).map((p) => (
                <button
                  key={p}
                  onClick={() => { onAdd({ path: p }); setPickerOpen(false); setFilter(""); }}
                  style={{
                    display: "block", width: "100%", textAlign: "left",
                    padding: "3px 8px", border: "none", background: "transparent",
                    cursor: "pointer", color: "var(--text-secondary)",
                    fontSize: 10.5, fontFamily: "monospace",
                  }}
                >
                  {p}
                </button>
              ))}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

// ─── Save-as dialog ───────────────────────────────────────────────────────

function SaveAsDialog({
  onCancel, onSave,
}: {
  onCancel: () => void;
  onSave: (name: string) => void;
}) {
  const [raw, setRaw] = useState("");
  const slug = slugifyFunctionName(raw);
  const valid = slug.length > 0 && isValidFunctionName(slug);
  return (
    <div style={{
      display: "flex", flexDirection: "column", gap: 6,
      padding: 8, background: "var(--card)", border: "1px solid var(--border-subtle)", borderRadius: 5,
    }}>
      <div style={{ fontSize: 11, color: "var(--text-secondary)" }}>
        Save current selections as a reusable function:
      </div>
      <div style={{ display: "flex", gap: 4, alignItems: "center" }}>
        <input
          value={raw}
          onChange={(e) => setRaw(e.target.value)}
          placeholder="e.g. Alice battle scene"
          autoFocus
          onKeyDown={(e) => { if (e.key === "Enter" && valid) onSave(slug); if (e.key === "Escape") onCancel(); }}
          style={{
            flex: 1, padding: "3px 6px", fontSize: 11,
            border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
            color: "var(--foreground)", borderRadius: 4, outline: "none",
          }}
        />
        <code style={{ fontSize: 10, color: valid ? "var(--text-dim)" : "var(--text-ghost)", fontFamily: "monospace", minWidth: 80 }}>
          {slug || "—"}
        </code>
      </div>
      <div style={{ display: "flex", gap: 4, justifyContent: "flex-end" }}>
        <button onClick={onCancel} style={dialogBtnStyle}>Cancel</button>
        <button
          onClick={() => valid && onSave(slug)}
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

export function ContextPanel({ path, activeBranch, onClose }: ContextPanelProps) {
  const fileState = useCallContext((s) => s.files[path]);
  const setEdits = useCallContext((s) => s.setEdits);
  const patchEdits = useCallContext((s) => s.patchEdits);
  const loadNamed = useCallContext((s) => s.loadNamed);
  const resetToDefault = useCallContext((s) => s.resetToDefault);

  const characterBranches = useServerCache((s) => s.characterBranches);

  const [view, setView] = useState<"casual" | "dsl">("casual");
  const [saveAsOpen, setSaveAsOpen] = useState(false);
  const [saving, setSaving] = useState(false);

  // The branch's own lore tree -- reuses the same /lore/{branch}
  // connection lifecycle the Codex tab and the mention autocomplete
  // already use. Owned locally by this hook (connects on mount, closes
  // on unmount); a single connection is fine here because the panel
  // unmounts when collapsed.
  const loreEntries = useLoreTree(activeBranch);

  // Esc closes the panel.
  useEffect(() => {
    function onKey(e: KeyboardEvent) {
      if (e.key === "Escape") onClose();
    }
    document.addEventListener("keydown", onKey);
    return () => document.removeEventListener("keydown", onKey);
  }, [onClose]);

  const edits: ContextEdits = fileState?.edits ?? DEFAULT_EDITS;

  function setBaseline(key: keyof ContextEdits["baseline"], value: boolean) {
    patchEdits(path, { baseline: { ...edits.baseline, [key]: value } });
  }
  function addCharacter(c: CharacterAdd) {
    patchEdits(path, { characters: [...edits.characters.filter((x) => x.id !== c.id), c] });
  }
  function removeCharacter(id: string) {
    patchEdits(path, { characters: edits.characters.filter((x) => x.id !== id) });
  }
  function setCharacterDepth(id: string, depth: "blurb" | "full") {
    patchEdits(path, {
      characters: edits.characters.map((c) => (c.id === id ? { ...c, depth } : c)),
    });
  }
  function addFile(f: FileAdd) {
    if (edits.extraFiles.some((x) => x.path === f.path)) return;
    patchEdits(path, { extraFiles: [...edits.extraFiles, f] });
  }
  function removeFile(p: string) {
    patchEdits(path, { extraFiles: edits.extraFiles.filter((x) => x.path !== p) });
  }

  async function handleSaveAs(name: string) {
    setSaving(true);
    try {
      const { synthesizeProgram } = await import("@/lib/dslCompose");
      const source = synthesizeProgram(edits);
      if (source === null) {
        setError("Nothing to save — selections match the default.");
        return;
      }
      await writeContextFunction(name, source);
      loadNamed(path, name);
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
          title="Edit as DSL (advanced)"
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
          <PowerUserView path={path} />
        ) : fileState?.mode === "named" ? (
          // Casual view, but a named function is loaded -- the panel
          // becomes a read-only summary of what's in effect, with
          // affordances to clear or to switch to the DSL view (the
          // named function's source is editable there).
          <NamedLoadedView path={path} />
        ) : saveAsOpen ? (
          <SaveAsDialog onCancel={() => setSaveAsOpen(false)} onSave={handleSaveAs} />
        ) : (
          // Layer 1 -- casual editor
          <div style={{ display: "flex", flexDirection: "column", gap: 10 }}>
            <section>
              <SectionLabel>Standing context</SectionLabel>
              <div style={{ display: "flex", flexDirection: "column" }}>
                <ToggleRow
                  icon={<BookOpen style={{ width: 11, height: 11 }} />}
                  label="Story lore"
                  hint="Everything under lore/**"
                  checked={edits.baseline.lore}
                  onChange={(v) => setBaseline("lore", v)}
                />
                <ToggleRow
                  icon={<FileText style={{ width: 11, height: 11 }} />}
                  label="Past chapters"
                  hint="Earlier chapters for continuity"
                  checked={edits.baseline.chapters}
                  onChange={(v) => setBaseline("chapters", v)}
                />
                <ToggleRow
                  icon={<Sparkles style={{ width: 11, height: 11 }} />}
                  label="Style guide"
                  hint="The project's style.md"
                  checked={edits.baseline.style}
                  onChange={(v) => setBaseline("style", v)}
                />
              </div>
            </section>

            <section>
              <SectionLabel>Additional characters</SectionLabel>
              <CharacterPicker
                characters={edits.characters}
                characterBranches={characterBranches}
                onAdd={addCharacter}
                onRemove={removeCharacter}
                onDepthChange={setCharacterDepth}
              />
            </section>

            <section>
              <SectionLabel>Extra files</SectionLabel>
              <FilePicker
                extras={edits.extraFiles}
                loreEntries={loreEntries}
                onAdd={addFile}
                onRemove={removeFile}
              />
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
                <Save style={{ width: 10, height: 10 }} /> Save as…
              </button>
              <span style={{ fontSize: 10, color: "var(--text-ghost)", flex: 1 }}>
                Promotes these selections to a reusable named function
              </span>
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

// ─── Named-function-loaded view ───────────────────────────────────────────

function NamedLoadedView({ path }: { path: string }) {
  const fileState = useCallContext((s) => s.files[path]);
  const clearNamed = useCallContext((s) => s.clearNamed);
  if (!fileState || fileState.mode !== "named") return null;
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 8, padding: "4px 0" }}>
      <div style={{
        display: "flex", alignItems: "center", gap: 6,
        padding: 8, background: "var(--card)", border: "1px solid var(--border-subtle)", borderRadius: 5,
      }}>
        <AlertCircle style={{ width: 12, height: 12, color: "var(--accent, var(--amber))" }} />
        <div style={{ flex: 1, minWidth: 0 }}>
          <div style={{ fontSize: 11, color: "var(--foreground)" }}>
            Using saved function
          </div>
          <code style={{ fontSize: 11, fontFamily: "monospace", color: "var(--accent, var(--amber))" }}>
            {fileState.namedName}
          </code>
        </div>
      </div>
      <div style={{ fontSize: 11, color: "var(--text-dim)" }}>
        The selections below are dormant — the named function is the entire context program.
        Edit its source under the DSL tab, or clear it to return to the casual editor.
      </div>
      <button onClick={() => clearNamed(path)} style={dialogBtnStyle}>
        Clear named function
      </button>
    </div>
  );
}

// ─── Power-user view (DSL editor + library) ───────────────────────────────

function PowerUserView({ path }: { path: string }) {
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 8 }}>
      <DSLEditor path={path} />
      <div>
        <SectionLabel>Saved functions</SectionLabel>
        <ContextLibrary path={path} />
      </div>
    </div>
  );
}
