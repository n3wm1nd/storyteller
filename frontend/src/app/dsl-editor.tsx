"use client";

// Layer 2 — the pinned-snippet DSL editor. The power-user's surface,
// behind one click from the casual panel ("DSL" toggle in the header).
//
// This used to be a whole-context-program editor (context.writer's own
// full shape, casual-state-seeded). That design was rolled back (see
// dslCompose.ts's own header) -- a snippet authored here is always a
// 0-arity Context DSL program, one of possibly several folded into a
// call's pinned/authors-notes content (see Server.Writer.File.Protocol's
// `ChatWriter`'s `pinnedPrograms` field), never a whole-context override.
//
// Starts blank (or from a one-line example) for a new snippet; "Save"
// prompts for a name and pins it for this call immediately. Loading an
// existing saved snippet (via the library below) shows its source,
// editable in place -- "Save" then writes back to the same name.
//
// The editor itself is a plain textarea with monospace, no syntax
// highlighting -- the DSL is small enough that a real highlighter
// (CodeMirror etc.) isn't justified here.

import { useEffect, useState } from "react";
import { Save, RefreshCw, AlertCircle, Check } from "lucide-react";
import { useCallContext } from "@/lib/callContextStore";
import {
  readContextFunction, writeContextFunction,
  slugifyFunctionName, isValidFunctionName,
} from "@/lib/contextBranch";
import { setError } from "@/lib/uiStore";

interface DSLEditorProps {
  path: string;
}

const BLANK_STARTER = `"the rules of magic"\n`;

export function DSLEditor({ path }: DSLEditorProps) {
  const addPinnedProgram = useCallContext((s) => s.addPinnedProgram);

  const [draft, setDraft] = useState<string>(BLANK_STARTER);
  const [loadedName, setLoadedName] = useState<string | null>(null);
  const [dirty, setDirty] = useState(false);
  const [loading, setLoading] = useState(false);
  const [saving, setSaving] = useState(false);
  const [namePromptOpen, setNamePromptOpen] = useState(false);
  const [rawName, setRawName] = useState("");

  function onChange(e: React.ChangeEvent<HTMLTextAreaElement>) {
    setDraft(e.target.value);
    setDirty(true);
  }

  async function saveExisting() {
    if (!loadedName) { setNamePromptOpen(true); return; }
    setSaving(true);
    try {
      await writeContextFunction(loadedName, draft);
      setDirty(false);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  async function saveNew(name: string) {
    setSaving(true);
    try {
      await writeContextFunction(name, draft);
      addPinnedProgram(path, name);
      setLoadedName(name);
      setDirty(false);
      setNamePromptOpen(false);
      setRawName("");
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  const slug = slugifyFunctionName(rawName);
  const nameValid = slug.length > 0 && isValidFunctionName(slug);

  return (
    <div style={{ display: "flex", flexDirection: "column", gap: 6 }}>
      <div style={{
        display: "flex", alignItems: "center", gap: 6, fontSize: 11,
        color: "var(--text-secondary)",
      }}>
        {loadedName ? (
          <>
            <span>Editing</span>
            <code style={{ fontFamily: "monospace", color: "var(--accent, var(--amber))" }}>{loadedName}</code>
            {dirty
              ? <span style={{ color: "var(--text-ghost)" }}>· unsaved</span>
              : <span style={{ color: "var(--text-ghost)", display: "inline-flex", alignItems: "center", gap: 3 }}><Check style={{ width: 10, height: 10 }} /> saved</span>
            }
          </>
        ) : (
          <span style={{ color: "var(--text-ghost)" }}>New snippet (unsaved)</span>
        )}
        <span style={{ marginLeft: "auto", display: "flex", gap: 4 }}>
          <button
            onClick={() => loadedName ? saveExisting() : setNamePromptOpen(true)}
            disabled={saving || (!dirty && loadedName !== null) || loading}
            title={loadedName ? "Save changes" : "Save and pin for this call"}
            style={btnStyle}
          >
            <Save style={{ width: 10, height: 10 }} /> {loadedName ? "Save" : "Save as…"}
          </button>
          {loadedName && (
            <button
              onClick={() => {
                setLoading(true);
                readContextFunction(loadedName)
                  .then((src) => { setDraft(src); setDirty(false); })
                  .catch((err) => setError(String(err)))
                  .finally(() => setLoading(false));
              }}
              disabled={loading}
              title="Revert to saved"
              style={btnStyle}
            >
              <RefreshCw style={{ width: 10, height: 10 }} />
            </button>
          )}
        </span>
      </div>

      {namePromptOpen && (
        <div style={{
          display: "flex", gap: 4, alignItems: "center",
          padding: 6, background: "var(--card)", border: "1px solid var(--border-subtle)", borderRadius: 5,
        }}>
          <AlertCircle style={{ width: 11, height: 11, color: "var(--text-dim)" }} />
          <input
            value={rawName}
            onChange={(e) => setRawName(e.target.value)}
            placeholder="snippet name (e.g. rules.magic)"
            autoFocus
            onKeyDown={(e) => {
              if (e.key === "Enter" && nameValid) saveNew(slug);
              if (e.key === "Escape") setNamePromptOpen(false);
            }}
            style={{
              flex: 1, minWidth: 0, padding: "3px 6px", fontSize: 11,
              border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
              color: "var(--foreground)", borderRadius: 4, outline: "none",
              fontFamily: "monospace",
            }}
          />
          <code style={{ fontSize: 10, color: nameValid ? "var(--text-dim)" : "var(--text-ghost)", fontFamily: "monospace", minWidth: 80 }}>
            {slug || "—"}
          </code>
          <button onClick={() => setNamePromptOpen(false)} style={btnStyle}>Cancel</button>
          <button
            onClick={() => nameValid && saveNew(slug)}
            disabled={!nameValid || saving}
            style={{
              ...btnStyle,
              background: nameValid ? "var(--accent, var(--amber))" : "var(--surface)",
              color: nameValid ? "var(--surface-deep)" : "var(--text-ghost)",
              cursor: nameValid ? "pointer" : "default",
            }}
          >
            Save
          </button>
        </div>
      )}

      <textarea
        value={loading ? "Loading…" : draft}
        onChange={onChange}
        disabled={loading || saving}
        spellCheck={false}
        placeholder={`# A 0-arity Context DSL snippet.\n# See CONTEXT-DSL.md for the full syntax.`}
        style={{
          width: "100%", minHeight: 160, maxHeight: 320, resize: "vertical",
          padding: "6px 8px", fontFamily: "monospace", fontSize: 11, lineHeight: 1.5,
          background: "var(--card)", color: "var(--foreground)",
          border: "1px solid var(--border-subtle)", borderRadius: 5, outline: "none",
        }}
      />
      <div style={{ fontSize: 9.5, color: "var(--text-ghost)", lineHeight: 1.4 }}>
        Saved snippets are stored on the <code>contexts</code> branch as <code>context/&lt;name&gt;.dsl</code>,
        and are 0-arity -- one self-contained piece of content, folded into this call's pinned content by name.
        Loading a snippet from the library below replaces this draft.
      </div>
    </div>
  );
}

const btnStyle: React.CSSProperties = {
  display: "inline-flex", alignItems: "center", gap: 3,
  fontSize: 10.5, padding: "2px 7px", borderRadius: 4,
  border: "1px solid var(--border-subtle)", background: "var(--surface-deep)",
  color: "var(--text-dim)", cursor: "pointer",
};
