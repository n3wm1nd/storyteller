"use client";

// A right-sidebar panel that keeps a live per-line token/size estimate of
// the context program the *next* call for `path` would actually send --
// same derivation fileview.actions.ts's `writerCommandContext` uses
// (`getCallContext` + `composeSendProgram`), so the sidebar can never show
// a different program than a real send would use. Backed by
// Storyteller.Writer.Agent.ContextCost's ablation-based estimate (see that
// module's own Haddock): each row is one statement, its own cost measured
// by re-running the whole program with just that statement removed and
// diffing the rendered size -- correct even when a filter's effect on a
// line's contribution is non-additive, since nothing here inspects what
// any filter does.
//
// Owned locally (connect on mount, reconnect on branch change, close on
// unmount) -- the same lifecycle lore-selector.tsx's `useLoreTree` already
// established for this family of "one ad-hoc WS connection per open
// panel" hooks. Requests are debounced against the composed program
// (not fired on every keystroke): an estimate re-runs the whole DSL
// program once per candidate line, materially more expensive than a
// plain preview, so this waits for edits to settle for a beat before
// asking.

import { useEffect, useMemo, useRef, useState } from "react";
import { Gauge, RefreshCw, AlertCircle } from "lucide-react";
import { contextViewConn } from "@/lib/ws";
import type { LineCost } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { useCallContext, EMPTY_MENTIONS } from "@/lib/callContextStore";
import { alwaysSynthesizeProgram, DEFAULT_EDITS, type ContextEdits } from "@/lib/dslCompose";

const DEBOUNCE_MS = 500;

// The program to estimate for `path` -- what a real send would actually
// compose, *fully spelled out* rather than the wire's own "omit the field,
// let the server's compiled-in context.writer run" shorthand
// (composeSendProgram's null case): an ablation-based cost breakdown has
// nothing to attribute lines to if it's only ever handed an opaque call to
// a definition it can't see inside, so this always expands to the real
// lore/chapters/style/character fragments (see `alwaysSynthesizeProgram`),
// even when that happens to match `DEFAULT_EDITS` exactly.
//
// `liveDraft`, when non-null, wins outright over the composed program --
// this is the DSL editor's own current (possibly unsaved) textarea
// content (see `callContextStore`'s `liveDslDraft` field, written by
// dsl-editor.tsx on every keystroke): the whole point of a live-updating
// cost sidebar is to reflect what's actually being typed, not just
// whatever was last saved.
function programFor(
  path: string, edits: ContextEdits, namedName: string | null,
  mentionCharacterIds: readonly string[], liveDraft: string | null,
): string {
  if (liveDraft !== null) return liveDraft;
  if (namedName !== null) return `path:\n  ${namedName} path\n`;
  return alwaysSynthesizeProgram(edits);
}

// Live-updating, per-`path`: connects to the same /$context/{path}
// connection context-library.tsx's preview affordance would use, sends
// `context.cost` whenever the composed program settles (debounced), and
// returns the most recent result plus loading/error state.
function useContextCost(branch: string | null, path: string | null) {
  const edits = useCallContext((s) => (path ? s.files[path]?.edits : undefined)) ?? undefined;
  const namedName = useCallContext((s) => (path && s.files[path]?.mode === "named" ? s.files[path].namedName : null)) ?? null;
  const mentionCharacterIds = useCallContext((s) => (path ? s.mentions[path] : undefined)) ?? EMPTY_MENTIONS;
  const liveDraft = useCallContext((s) => (path ? s.liveDslDrafts[path] : undefined)) ?? null;

  const [costs, setCosts] = useState<LineCost[] | null>(null);
  // The program that actually produced `costs` -- kept alongside it
  // (not just read live) so a line's own source snippet is looked up
  // against the exact text the estimate is *for*, even if the composed
  // program has already moved on to a newer value while this response
  // was still in flight.
  const [costsProgram, setCostsProgram] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const pendingProgramRef = useRef<string | null>(null);

  const program = useMemo(() => {
    if (!path) return null;
    return programFor(path, edits ?? DEFAULT_EDITS, namedName, mentionCharacterIds, liveDraft);
  }, [path, edits, namedName, mentionCharacterIds, liveDraft]);

  // Connection lifecycle: one per (branch, path), matching useLoreTree's
  // own convention.
  useEffect(() => {
    if (!branch || !path) { connRef.current = null; return; }
    const connLabel = `context.cost:${branch}:${path}`;
    setConnStatus(connLabel, "connecting");

    const conn = contextViewConn(branch, path);
    connRef.current = conn;
    conn.onStatus((s) => {
      if (s !== "connected") setConnStatus(connLabel, "connecting");
    });
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.cost") {
        setCosts(evt.costs);
        setCostsProgram(pendingProgramRef.current);
        setLoading(false);
      } else if (evt.type === "error") {
        setError(evt.message);
        setLoading(false);
      }
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(connLabel, "connected");
      } catch (err) {
        setConnStatus(connLabel, "error");
        setError(String(err));
      }
    })();

    return () => {
      conn.close();
      connRef.current = null;
      removeConn(connLabel);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [branch, path]);

  // Debounced request: whenever the composed program settles, ask for a
  // fresh estimate. Fires immediately on the connection's first mount
  // too (see the `program !== null` guard), not just on subsequent edits.
  useEffect(() => {
    if (!program) return;
    setLoading(true);
    const t = setTimeout(() => {
      pendingProgramRef.current = program;
      connRef.current?.send({ type: "context.cost", path: path!, program });
    }, DEBOUNCE_MS);
    return () => clearTimeout(t);
  }, [program, path]);

  return { costs, costsProgram, loading };
}

// The exact source text of one ablation candidate's own statement --
// looked up by `line`/`col` against the program that produced this
// estimate (1-based, matching Storyteller.Context.DSL.AST.Pos). Falls
// back to a plain "line N" label if the program's own line is somehow
// shorter than expected (defensive only; shouldn't happen against a
// program this sidebar itself just sent).
function sourceSnippet(program: string, line: number, col: number): string {
  const lines = program.split("\n");
  const text = lines[line - 1] ?? "";
  return text.slice(col - 1).trim() || text.trim();
}

interface ContextCostSidebarProps {
  activeBranch: string | null;
  selectedFile: string | null;
}

export function ContextCostSidebar({ activeBranch, selectedFile }: ContextCostSidebarProps) {
  const { costs, costsProgram, loading } = useContextCost(activeBranch, selectedFile);

  if (!selectedFile) {
    return (
      <div style={{ padding: 12, fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic" }}>
        Open a file to see its context cost.
      </div>
    );
  }

  const total = costs?.reduce((sum, c) => sum + c.chars, 0) ?? 0;
  // Rough chars-per-token heuristic -- good enough for "where's the
  // budget going," not a claim of exact provider tokenization.
  const totalTokensEst = Math.round(total / 4);
  const maxChars = costs?.reduce((m, c) => Math.max(m, c.chars), 0) ?? 0;

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      <div style={{
        display: "flex", alignItems: "center", gap: 6, padding: "8px 10px",
        borderBottom: "1px solid var(--border-subtle)", flexShrink: 0,
      }}>
        <Gauge style={{ width: 12, height: 12, color: "var(--text-dim)" }} />
        <span style={{ fontSize: 11, fontWeight: 500, color: "var(--foreground)", flex: 1 }}>
          Context cost
        </span>
        {loading && <RefreshCw className="animate-spin" style={{ width: 11, height: 11, color: "var(--text-ghost)" }} />}
      </div>

      {costs && (
        <div style={{
          padding: "8px 10px", borderBottom: "1px solid var(--border-subtle)",
          fontSize: 11, color: "var(--text-secondary)", flexShrink: 0,
        }}>
          <div style={{ fontSize: 16, fontWeight: 600, color: "var(--foreground)" }}>
            ~{totalTokensEst.toLocaleString()} tokens
          </div>
          <div style={{ fontSize: 10, color: "var(--text-ghost)" }}>
            {total.toLocaleString()} characters across {costs.length} line{costs.length === 1 ? "" : "s"}
          </div>
        </div>
      )}

      <div style={{ flex: 1, overflowY: "auto", padding: "6px 8px" }}>
        {!costs && loading && (
          <div style={{ fontSize: 11, color: "var(--text-ghost)", padding: 8 }}>Estimating…</div>
        )}
        {costs && costs.length === 0 && (
          <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", padding: 8 }}>
            Nothing to measure -- this program has no ablatable lines.
          </div>
        )}
        {costs?.map((c, i) => {
          const snippet = costsProgram ? sourceSnippet(costsProgram, c.line, c.col) : "";
          const pct = total > 0 ? (c.chars / total) * 100 : 0;
          return (
            <div
              key={`${c.line}:${c.col}:${i}`}
              title={snippet}
              style={{ display: "flex", flexDirection: "column", gap: 2, padding: "5px 4px" }}
            >
              <div style={{ display: "flex", alignItems: "baseline", gap: 6, fontSize: 10.5 }}>
                <code style={{ color: "var(--text-ghost)", flexShrink: 0 }}>L{c.line}</code>
                <code style={{
                  flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap",
                  color: "var(--text-secondary)", fontFamily: "monospace",
                }}>
                  {snippet}
                </code>
              </div>
              <div style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 10, color: "var(--text-ghost)" }}>
                <span style={{ flex: 1 }}>
                  {Math.round(c.chars / 4).toLocaleString()} tok · {c.chars.toLocaleString()} ch
                </span>
                <span>{pct.toFixed(0)}%</span>
              </div>
              <div style={{
                height: 4, borderRadius: 2, background: "var(--surface)", overflow: "hidden",
              }}>
                <div style={{
                  height: "100%", width: maxChars > 0 ? `${Math.max(2, (c.chars / maxChars) * 100)}%` : "0%",
                  background: "var(--accent, var(--amber))", borderRadius: 2,
                }} />
              </div>
            </div>
          );
        })}
      </div>

      <div style={{
        display: "flex", alignItems: "center", gap: 4, padding: "6px 10px",
        borderTop: "1px solid var(--border-subtle)", fontSize: 9.5, color: "var(--text-ghost)",
        flexShrink: 0,
      }}>
        <AlertCircle style={{ width: 10, height: 10, flexShrink: 0 }} />
        Estimated by re-running the program with each line removed -- an approximation, not a real tokenizer.
      </div>
    </div>
  );
}
