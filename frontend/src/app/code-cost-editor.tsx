"use client";

// A CodeMirror-backed textarea replacement for editing a Context DSL
// snippet, with a gutter showing each statement's own measured cost
// (chars, ~tokens) inline next to its source line -- the in-editor
// counterpart to context-cost-sidebar.tsx's per-snippet breakdown.
//
// Costs come from the same `context.cost.adhoc` command
// (Storyteller.Writer.Agent.ContextCost.buildAdhocProgramCosts) the
// sidebar already used; this component only adds the rendering side
// (a CodeMirror gutter keyed by `LineCost.line`) plus its own debounced
// fetch, so it works standalone without the sidebar mounted.
//
// Syntax highlighting comes from lib/dslLanguage.ts (a stream tokenizer
// mirroring Storyteller.Context.DSL.Parser's own lexer). Still no full
// language package -- bracket matching and indent rules would mean
// modelling the DSL's indentation-sensitive layout, which highlighting
// doesn't need and nothing has asked for yet.

import { useEffect, useMemo, useRef, useState } from "react";
import CodeMirror from "@uiw/react-codemirror";
import { EditorView, gutter, GutterMarker } from "@codemirror/view";
import { StateField, StateEffect } from "@codemirror/state";
import { contextViewConn } from "@/lib/ws";
import { contextDslHighlighting } from "@/lib/dslLanguage";
import type { LineCost, PreviewNode } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity } from "@/lib/uiStore";

const DEBOUNCE_MS = 600;

// One ad-hoc `context.cost.adhoc` connection, opened lazily and reused
// across requests -- resolves any bare 0-arity Context DSL program
// against `branch` (a pinned snippet, a lore override draft, whatever the
// caller is currently editing) via
// Storyteller.Writer.Agent.ContextCost.buildAdhocProgramCosts. Shared by
// every CodeCostEditor call site (dsl-editor.tsx's pinned-snippet editor,
// context-panel.tsx's inline lore-override editor) rather than duplicated
// per site, since the connection itself has no opinion on what it's
// estimating -- only the caller's own draft text does.
export function useAdhocCostFetcher(branch: string | null | undefined) {
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const pendingRef = useRef<{ resolve: (c: LineCost[] | null) => void } | null>(null);

  useEffect(() => {
    if (!branch) return;
    const connLabel = `context.cost.adhoc:editor:${branch}`;
    setConnStatus(connLabel, "connecting");
    const conn = contextViewConn(branch, "");
    connRef.current = conn;
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.cost") {
        pendingRef.current?.resolve(evt.costs);
        pendingRef.current = null;
      } else if (evt.type === "error") {
        pendingRef.current?.resolve(null);
        pendingRef.current = null;
      }
    });
    (async () => {
      try {
        await conn.connect();
        setConnStatus(connLabel, "connected");
      } catch {
        setConnStatus(connLabel, "error");
      }
    })();
    return () => {
      conn.close();
      connRef.current = null;
      removeConn(connLabel);
    };
  }, [branch]);

  return async (program: string): Promise<LineCost[] | null> => {
    const conn = connRef.current;
    if (!conn) return null;
    return new Promise((resolve) => {
      pendingRef.current = { resolve };
      conn.send({ type: "context.cost.adhoc", program });
    });
  };
}

// 'useAdhocCostFetcher''s own sibling for `context.preview.adhoc` (see
// Storyteller.Writer.Agent.ContextPreview.buildAdhocPreview): resolves
// and renders a bare 0-arity Context DSL program directly, the way a
// project-default context slot or a saved pinned snippet's own editor
// needs to preview -- never staged as a whole `context.writer` override
// the way contextViewConn's plain `context.preview` command works (that
// one is DSLEditor/RawEditPanel's whole-file preview, a different
// question). A separate connection from useAdhocCostFetcher (rather than
// sharing one) so a caller that only wants one of cost/preview doesn't
// pay for the other's connection lifecycle -- DSLFileEditor below uses
// both together.
export function useAdhocPreviewFetcher(branch: string | null | undefined) {
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const pendingRef = useRef<{ resolve: (n: PreviewNode | null) => void } | null>(null);

  useEffect(() => {
    if (!branch) return;
    const connLabel = `context.preview.adhoc:editor:${branch}`;
    setConnStatus(connLabel, "connecting");
    const conn = contextViewConn(branch, "");
    connRef.current = conn;
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.preview") {
        pendingRef.current?.resolve(evt.result);
        pendingRef.current = null;
      } else if (evt.type === "error") {
        pendingRef.current?.resolve(null);
        pendingRef.current = null;
      }
    });
    (async () => {
      try {
        await conn.connect();
        setConnStatus(connLabel, "connected");
      } catch {
        setConnStatus(connLabel, "error");
      }
    })();
    return () => {
      conn.close();
      connRef.current = null;
      removeConn(connLabel);
    };
  }, [branch]);

  return async (program: string): Promise<PreviewNode | null> => {
    const conn = connRef.current;
    if (!conn) return null;
    return new Promise((resolve) => {
      pendingRef.current = { resolve };
      conn.send({ type: "context.preview.adhoc", program });
    });
  };
}

// ─── Cost gutter ────────────────────────────────────────────────────────

const setCosts = StateEffect.define<Map<number, number>>();

const costsField = StateField.define<Map<number, number>>({
  create: () => new Map(),
  update(value, tr) {
    for (const e of tr.effects) if (e.is(setCosts)) return e.value;
    return value;
  },
});

class CostMarker extends GutterMarker {
  constructor(
    private chars: number,
    private maxChars: number,
  ) {
    super();
  }

  eq(other: CostMarker) {
    return this.chars === other.chars && this.maxChars === other.maxChars;
  }

  toDOM() {
    const span = document.createElement("span");
    const tokens = Math.round(this.chars / 4);
    span.textContent = this.chars > 0 ? `${tokens.toLocaleString()}t` : "";
    const pct = this.maxChars > 0 ? this.chars / this.maxChars : 0;
    span.style.cssText = `
      display: inline-block; min-width: 34px; padding: 0 6px 0 0;
      font-family: monospace; font-size: 9.5px; text-align: right;
      color: ${pct > 0.01 ? "var(--accent, #d29922)" : "var(--text-ghost, #666)"};
      opacity: ${0.35 + pct * 0.65};
    `;
    return span;
  }
}

const costGutter = gutter({
  class: "cm-cost-gutter",
  lineMarker(view, line) {
    const costs = view.state.field(costsField);
    const lineNumber = view.state.doc.lineAt(line.from).number;
    const chars = costs.get(lineNumber);
    if (chars === undefined) return null;
    const maxChars = Math.max(1, ...costs.values());
    return new CostMarker(chars, maxChars);
  },
  initialSpacer: () => new CostMarker(0, 1),
});

// No CodeMirror theme preset (light/dark) -- both bake in an opaque
// background that fights the app's own [data-theme]/prefers-color-scheme
// variables (see globals.css) rather than following them. Transparent
// backgrounds throughout plus `var(--foreground)`/`var(--card)` let the
// surrounding panel's own background (already themed) show through, the
// same way every other text surface in this app is styled.
const baseTheme = EditorView.theme({
  "&": { fontSize: "11px", backgroundColor: "transparent" },
  ".cm-content": { fontFamily: "monospace", padding: "6px 0", color: "var(--foreground)", caretColor: "var(--foreground)" },
  // `caret-color` above only styles a native text-input caret -- CM6 draws
  // its own cursor as a `.cm-cursor` element via the `drawSelection`
  // extension (part of `basicSetup`), themed separately. Without an
  // explicit rule here it can render invisible against this app's
  // transparent/dark surfaces, which is what made a genuinely focused,
  // editable instance look uneditable (no visible caret at all).
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: "var(--foreground)", borderLeftWidth: "1.5px" },
  "&.cm-focused .cm-cursor": { borderLeftColor: "var(--foreground)" },
  ".cm-gutters": { backgroundColor: "transparent", border: "none" },
  ".cm-cost-gutter": { borderRight: "1px solid var(--border-subtle)" },
  ".cm-activeLine": { backgroundColor: "transparent" },
  ".cm-activeLineGutter": { backgroundColor: "transparent" },
  ".cm-selectionBackground": { backgroundColor: "var(--accent-tint, var(--amber-tint))" },
  "&.cm-focused .cm-selectionBackground": { backgroundColor: "var(--accent-tint, var(--amber-tint))" },
});

// ─── Component ──────────────────────────────────────────────────────────

interface CodeCostEditorProps {
  value: string;
  onChange: (next: string) => void;
  disabled?: boolean;
  placeholder?: string;
  minHeight?: string;
  // Fill the parent instead of sitting in a fixed 160–320px box — for a
  // whole-pane instance (dsl-file-view.tsx) rather than one embedded in a
  // panel among other controls (dsl-editor.tsx, context-panel.tsx). The
  // parent must be a flex column with a bounded height of its own; the
  // editor owns the scrolling either way.
  fill?: boolean;
  // Fetches costs for `program` (the current draft) — the caller owns the
  // WS connection (see dsl-editor.tsx), this component only decides when
  // to ask and how to render what comes back.
  fetchCosts: (program: string) => Promise<LineCost[] | null>;
}

export function CodeCostEditor({
  value, onChange, disabled, placeholder, minHeight = "160px", fill, fetchCosts,
}: CodeCostEditorProps) {
  const viewRef = useRef<EditorView | null>(null);
  const [loading, setLoading] = useState(false);
  const requestIdRef = useRef(0);

  const extensions = useMemo(
    () => [contextDslHighlighting, costGutter, costsField, baseTheme, EditorView.lineWrapping],
    [],
  );

  useEffect(() => {
    if (!value.trim()) {
      viewRef.current?.dispatch({ effects: setCosts.of(new Map()) });
      return;
    }
    const requestId = ++requestIdRef.current;
    setLoading(true);
    const t = setTimeout(async () => {
      const costs = await fetchCosts(value);
      if (requestId !== requestIdRef.current) return; // superseded by a newer edit
      setLoading(false);
      const byLine = new Map<number, number>();
      for (const c of costs ?? []) {
        byLine.set(c.line, (byLine.get(c.line) ?? 0) + Math.max(0, c.chars));
      }
      viewRef.current?.dispatch({ effects: setCosts.of(byLine) });
    }, DEBOUNCE_MS);
    return () => clearTimeout(t);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [value]);

  return (
    <div style={fill
      ? { position: "relative", flex: 1, minHeight: 0, display: "flex", flexDirection: "column" }
      : { position: "relative" }}>
      <CodeMirror
        value={value}
        onChange={onChange}
        editable={!disabled}
        placeholder={placeholder}
        extensions={extensions}
        theme="none"
        basicSetup={{ lineNumbers: true, foldGutter: false, highlightActiveLine: false }}
        height={fill ? "100%" : undefined}
        style={fill
          ? { flex: 1, minHeight: 0, overflow: "auto", background: "transparent" }
          : {
              minHeight, maxHeight: 320, overflow: "auto",
              border: "1px solid var(--border-subtle)", borderRadius: 5,
              background: "var(--card)",
            }}
        onCreateEditor={(view) => { viewRef.current = view; }}
      />
      {loading && (
        <div style={{
          position: "absolute", top: 4, right: 6, fontSize: 9, color: "var(--text-ghost)",
        }}>
          estimating…
        </div>
      )}
    </div>
  );
}
