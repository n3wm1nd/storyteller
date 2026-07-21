"use client";

// Layer 2 — the DSL source editor. The power-user's surface, behind one
// click from the casual panel ("DSL" toggle in the header).
//
// Two paths into the editor:
//
//   - A named function is already loaded: the editor shows that
//     function's source (read fresh from the contexts branch on mount),
//     editable in place. "Save" writes back to the same name.
//
//   - No named function loaded: the editor starts from a one-shot
//     draft, seeded from the current casual state's synthesized source
//     (so flipping from casual to DSL doesn't lose what was selected).
//     "Save" prompts for a name (DSL is always saved, never sent
//     unnamed -- see the design conversation 2026-07-20); after save,
//     the named function is loaded and subsequent edits save in place.
//
// The editor itself is a plain textarea with monospace and very light
// syntax cueing via CSS (comments dimmed). The DSL is small enough
// that a real highlighter (CodeMirror etc.) isn't justified here --
// the existing raw-file editor (fileview.tsx's TextEditPanel) is the
// pattern: textarea + tiptap-markdown for prose, plain textarea for
// code. This is the code case.

import { useEffect, useRef, useState } from "react";
import { Save, RefreshCw, AlertCircle, Check } from "lucide-react";
import { useCallContext } from "@/lib/callContextStore";
import { synthesizeProgram, DEFAULT_EDITS } from "@/lib/dslCompose";
import {
  readContextFunction, writeContextFunction,
  slugifyFunctionName, isValidFunctionName,
} from "@/lib/contextBranch";
import { setError } from "@/lib/uiStore";

interface DSLEditorProps {
  path: string;
}

export function DSLEditor({ path }: DSLEditorProps) {
  const fileState = useCallContext((s) => s.files[path]);
  const loadNamed = useCallContext((s) => s.loadNamed);

  const namedName = fileState?.mode === "named" ? fileState.namedName : null;

  // Lazy initial state for `draft`: when entering the DSL view without a
  // named function loaded, pre-fill with the synthesized program from
  // the current casual-panel selections. This is the "configure with
  // toggles, fine-tune in code" path -- the user clicks around the
  // casual panel, gets a setup they like, opens DSL, and the code is
  // already there as a starting point. (Loading a named function still
  // happens in the effect below -- it's async via the HTTP file API.)
  const [draft, setDraft] = useState<string>(() => {
    if (namedName) return ""; // will be replaced by readContextFunction
    return synthesizeProgram(fileState?.edits ?? DEFAULT_EDITS) ?? defaultStarter();
  });
  // Whether the current draft was pre-filled from casual selections
  // (rather than authored from scratch or loaded from a named function).
  // Lets us show "Pre-filled from your selections" until the user
  // starts editing, after which it just becomes "New function".
  const [seededFromCasual, setSeededFromCasual] = useState<boolean>(() => !namedName);
  const [loadedName, setLoadedName] = useState<string | null>(namedName);
  const [dirty, setDirty] = useState(false);
  const [loading, setLoading] = useState(false);
  const [saving, setSaving] = useState(false);
  const [namePromptOpen, setNamePromptOpen] = useState(false);
  const [rawName, setRawName] = useState("");
  const taRef = useRef<HTMLTextAreaElement>(null);

  // Load a named function's source on mount or when the loaded function
  // changes. The seeding-from-casual case is handled by `draft`'s lazy
  // initial state above; this effect is only for the async fetch path.
  useEffect(() => {
    if (!namedName) return;
    let cancelled = false;
    setLoading(true);
    setDirty(false);
    setSeededFromCasual(false);
    setLoadedName(namedName);
    readContextFunction(namedName)
      .then((src) => { if (!cancelled) { setDraft(src); setLoadedName(namedName); } })
      .catch((err) => {
        if (!cancelled) {
          setError(String(err));
          setDraft("");
        }
      })
      .finally(() => { if (!cancelled) setLoading(false); });
    return () => { cancelled = true; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [namedName]);

  function onChange(e: React.ChangeEvent<HTMLTextAreaElement>) {
    setDraft(e.target.value);
    setDirty(true);
    // The user has taken control -- the draft is no longer "the
    // auto-synthesized version of your selections", it's "your code".
    // Drop the seed hint from the header.
    if (seededFromCasual) setSeededFromCasual(false);
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
      loadNamed(path, name);
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
          <span style={{ color: "var(--text-ghost)" }}>
            {seededFromCasual
              ? <>Pre-filled from your selections · edit freely</>
              : <>New function (unsaved)</>
            }
          </span>
        )}
        <span style={{ marginLeft: "auto", display: "flex", gap: 4 }}>
          <button
            onClick={() => loadedName ? saveExisting() : setNamePromptOpen(true)}
            disabled={saving || (!dirty && loadedName !== null) || loading}
            title={loadedName ? "Save changes" : "Save as new function"}
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
            placeholder="function name (e.g. alice-battle)"
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
        ref={taRef}
        value={loading ? "Loading…" : draft}
        onChange={onChange}
        disabled={loading || saving}
        spellCheck={false}
        placeholder={`# DSL source for this context function\n# See CONTEXT-DSL.md for the full syntax.`}
        style={{
          width: "100%", minHeight: 160, maxHeight: 320, resize: "vertical",
          padding: "6px 8px", fontFamily: "monospace", fontSize: 11, lineHeight: 1.5,
          background: "var(--card)", color: "var(--foreground)",
          border: "1px solid var(--border-subtle)", borderRadius: 5, outline: "none",
        }}
      />
      <div style={{ fontSize: 9.5, color: "var(--text-ghost)", lineHeight: 1.4 }}>
        Saved functions are stored on the <code>contexts</code> branch as <code>context/&lt;name&gt;.dsl</code>.
        Loading a function from the library below replaces this draft.
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

// A starter for a brand-new function -- mirrors `contextWriterDef`'s own
// real shape (Library.hs) as closely as a casual starting point should,
// so the power-user's blank canvas isn't actually blank, just editable.
// They can delete what they don't want and write the rest by hand.
//
// 1-arity, not 0 -- `path:` is `context.writer`'s own declared parameter,
// and a project's override has to match that arity exactly or it's
// silently discarded in favor of the compiled-in default
// (`Storyteller.Core.Context.resolveContextOverride`, Context.hs:33-43).
// A starter missing this line would produce a function that looks fine,
// saves fine, and does nothing the moment it's ever used to override
// `context.writer` -- so this isn't a style choice, it's the one thing
// this starter cannot omit.
//
// Calls the server's own named definitions (`context.lore`,
// `context.chapters`, `context.style`, `context.character`) rather than
// re-deriving their DSL bodies here -- the same fix `lib/dslCompose.ts`'s
// `synthesizeProgram` got, for the same reason: a second, hand-copied
// shape drifts from the real one, and can never see a project's own
// committed override of `context.lore` etc. the way a bare call does. A
// power-user who wants to see or borrow the real glob/sortBy machinery
// underneath can load that definition itself from the library below, or
// read CONTEXT-DSL.md's own worked examples -- this starter's job is
// just to not hand someone a blank textarea.
//
// Bare statements, not `as "lore": ...` -- an `as` block only ever
// contributes a *named entry*, never the enclosing block's own default
// (`renderText`/`renderMessages`, what actually reaches the model, read
// only the default). A starter that wrapped these in `as` would parse,
// save, and resolve without error, yet send the model nothing -- exactly
// mirrors `contextWriterDef`'s own real shape instead (Library.hs).
function defaultStarter(): string {
  return [
    `# Default-shape starter -- edit freely. See CONTEXT-DSL.md.`,
    `path:`,
    `  context.lore`,
    `  in (context.chapters | exclude(path)):`,
    `    for f in **/*: read f`,
    `  context.style`,
    `  for c in (charactersin path):`,
    `    as c: context.character c`,
    ``,
  ].join("\n");
}
