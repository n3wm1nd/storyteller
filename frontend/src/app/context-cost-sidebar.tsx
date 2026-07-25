"use client";

// A right-sidebar panel showing a live token/size estimate of the FULL
// context the currently-selected InputBar agent (`useUI`'s `writerMode`
// — see fileview.tsx's AgentId) would actually send, plus this file's own
// pinned snippets (`pinnedProgramNames` — see callContextStore.ts/
// dslCompose.ts's own header on the writer context's three-slot model).
//
// For "write" mode, this is ONE Context DSL program, sent as ONE
// `context.cost.adhoc` request -- not several independent per-piece
// queries. `writeAgent` (Storyteller.Writer.Agent.Write.hs) assembles
// lore + chapters + other + style + every active character's own
// context as one real call, so the estimate has to be one real
// evaluation of that same combination. There is no "slot" concept on
// this side once the program exists: a single program is just source
// text, and the view below reads each returned `LineCost.line` straight
// back against that same text to show its own real source line -- no
// grouping, no per-piece labels standing in for what's actually there
// (see buildWriteProgram). Backed by Storyteller.Writer.Agent.
// ContextCost.buildAdhocProgramCosts's ablation-based estimate (see that
// module's own Haddock): each row is one statement, its own cost measured
// by re-running the whole program with just that statement removed and
// diffing the rendered size.
//
// The program (buildWriteProgram) is:
//   <the live lore draft's own real text -- see useLoreDraft>
//   context.chapters
//   context.other "<path>"
//   context.style
//   context.character "<name>"   -- one call per currently-active character
//   <pinned name>                -- one call per pinned program
//
// Every line after the lore draft is a bare function-call reference --
// safe to concatenate as sibling top-level statements, since each is
// self-contained and doesn't carry its own competing "## Story
// background"-shaped prelude the way a copy of the lore *default's own
// source* would. The lore draft is different: it's real, possibly
// multi-statement source text (the panel's checkbox-generated block, or a
// hand-edit, or the compiled default's own body when untouched), so it's
// spliced in as-is, not re-derived or reconstructed -- see useLoreDraft's
// own header for why this must be the literal text LoreRow itself holds,
// never a bare `context.lore` name reference (that would resolve the
// *branch's* default, blind to any per-call override) and never a
// hand-synthesized reconstruction of the checkbox state.
//
// Character summaries and this file's own conversation/tick history are
// still not representable here: `context.character` per active character
// closes the "each character's own context" gap, but `writeAgent` reshapes
// that through `characterSummaryOf "journal"` and threads in real tick
// history (Storage.Tick.fileTicksOf) that has no DSL form at all -- see
// this component's own "Not included" note. Only the "write" agent is
// modeled this way for now (see the project chat: fix/regen/roleplay
// reduce to other, differently-shaped context paths — not worth
// replicating yet).
//
// Owned locally (connect on mount, reconnect on branch change, close on
// unmount) -- the same lifecycle lore-selector.tsx's `useLoreTree` already
// established for this family of "one ad-hoc WS connection per open
// panel" hooks. The combined program is debounced (not re-sent on every
// keystroke); pinned snippets (shown separately, any agent) keep their
// own independent per-name requests since they're a genuinely separate
// question ("what does this one saved snippet cost on its own") from
// "what does this call cost as a whole".

import { useEffect, useMemo, useRef, useState } from "react";
import { Gauge, RefreshCw, AlertCircle, Info } from "lucide-react";
import { contextViewConn } from "@/lib/ws";
import type { LineCost, WireTick } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError, useUI } from "@/lib/uiStore";
import { useCallContext, EMPTY_PINNED_PROGRAMS } from "@/lib/callContextStore";
import { useLoreDraft } from "@/lib/contextBranch";
import { activeCharacterBranches } from "@/lib/utils";
import { branchDisplayName } from "@/lib/branches";

const DEBOUNCE_MS = 500;

interface SnippetCost {
  name: string;
  costs: LineCost[] | null;
  loading: boolean;
}

// One `context.cost.adhoc` request for a whole program -- one round trip,
// one settle, one loading state. Not a per-entry fan-out (see this
// module's own header on why that was wrong for "what does this call, as
// a whole, cost").
function useProgramCost(branch: string | null, path: string | null, program: string | null) {
  const [costs, setCosts] = useState<LineCost[] | null>(null);
  const [loading, setLoading] = useState(false);
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);

  useEffect(() => {
    if (!branch || !path) { connRef.current = null; return; }
    const connLabel = `context.cost.adhoc:writeAgent:${branch}:${path}`;
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
        setLoading(false);
      } else if (evt.type === "error") {
        setError(evt.message);
        setCosts(null);
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

  useEffect(() => {
    if (!program) { setCosts(null); setLoading(false); return; }
    setLoading(true);
    const t = setTimeout(() => {
      connRef.current?.send({ type: "context.cost.adhoc", program });
    }, DEBOUNCE_MS);
    return () => clearTimeout(t);
  }, [program]);

  return { costs, loading };
}

// Live-updating, one request per pinned name -- kept as its own
// independent per-snippet question (see this module's own header),
// unrelated to the combined write-agent program above.
function usePinnedSnippetCosts(branch: string | null, path: string | null) {
  const pinnedNames = useCallContext((s) => (path ? s.files[path]?.pinnedProgramNames : undefined)) ?? EMPTY_PINNED_PROGRAMS;
  const [bySnippet, setBySnippet] = useState<Record<string, SnippetCost>>({});
  const connRef = useRef<ReturnType<typeof contextViewConn> | null>(null);
  const pendingNameRef = useRef<string | null>(null);

  useEffect(() => {
    if (!branch || !path) { connRef.current = null; return; }
    const connLabel = `context.cost.adhoc:pinned:${branch}:${path}`;
    setConnStatus(connLabel, "connecting");

    const conn = contextViewConn(branch, path);
    connRef.current = conn;
    conn.onStatus((s) => {
      if (s !== "connected") setConnStatus(connLabel, "connecting");
    });
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.cost") {
        const name = pendingNameRef.current;
        if (name) setBySnippet((prev) => ({ ...prev, [name]: { name, costs: evt.costs, loading: false } }));
      } else if (evt.type === "error") {
        setError(evt.message);
        const name = pendingNameRef.current;
        if (name) setBySnippet((prev) => ({ ...prev, [name]: { name, costs: null, loading: false } }));
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

  useEffect(() => {
    if (pinnedNames.length === 0) { setBySnippet({}); return; }
    const t = setTimeout(() => {
      setBySnippet((prev) => {
        const next: Record<string, SnippetCost> = {};
        for (const name of pinnedNames) next[name] = prev[name] ?? { name, costs: null, loading: true };
        return next;
      });
      (async () => {
        for (const name of pinnedNames) {
          pendingNameRef.current = name;
          setBySnippet((prev) => ({ ...prev, [name]: { name, costs: prev[name]?.costs ?? null, loading: true } }));
          connRef.current?.send({ type: "context.cost.adhoc", program: name });
          await new Promise((res) => setTimeout(res, 50));
        }
      })();
    }, DEBOUNCE_MS);
    return () => clearTimeout(t);
  }, [pinnedNames.join(" ")]); // eslint-disable-line react-hooks/exhaustive-deps

  return pinnedNames.map((name) => bySnippet[name] ?? { name, costs: null, loading: true });
}

interface ContextCostSidebarProps {
  activeBranch: string | null;
  selectedFile: string | null;
  fileChainTicks: Record<string, WireTick>;
  fileChainHead: string | null;
}

// One row per ablated statement on a source line -- `col` marks where
// each statement's own text starts, so a line with two costed statements
// (e.g. `as f: x`, where the `as f:` wrapper and its nested body `x` are
// each independently ablatable -- see Storyteller.Writer.Agent.
// ContextCost.positions's own Haddock on why nested-block positions are
// real candidates too) shows two rows, each its own bar/percentage. Each
// row shows the FULL line, every time, with only its own referenced span
// highlighted (bold, full opacity) and the rest of the line dimmed --
// extracting just the bare substring loses which part of the line it
// was (two rows both reading bare "x" tell you nothing); showing it
// in place, highlighted, tells you exactly where each cost center sits.
function SourceLineRow({ sourceLine, costs, maxChars }: { sourceLine: string; costs: LineCost[]; maxChars: number }) {
  const total = costs.reduce((sum, c) => sum + c.chars, 0);
  return (
    <>
      {costs.map((c, i) => {
        const start = c.col - 1;
        const end = i + 1 < costs.length ? costs[i + 1].col - 1 : sourceLine.length;
        const pct = total > 0 ? (c.chars / total) * 100 : 0;
        return (
          <div key={`${c.line}:${c.col}:${i}`} style={{ display: "flex", flexDirection: "column", gap: 2, padding: "3px 4px" }}>
            <div style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 10, color: "var(--text-ghost)" }}>
              <code style={{
                flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis",
                whiteSpace: "nowrap", fontFamily: "monospace",
              }}>
                {sourceLine.trim() === "" ? (
                  <span style={{ fontStyle: "italic" }}>(blank line)</span>
                ) : (
                  <>
                    <span style={{ color: "var(--text-ghost)", opacity: 0.5 }}>{sourceLine.slice(0, start)}</span>
                    <span style={{ color: "var(--foreground)", fontWeight: 600 }}>{sourceLine.slice(start, end)}</span>
                    <span style={{ color: "var(--text-ghost)", opacity: 0.5 }}>{sourceLine.slice(end)}</span>
                  </>
                )}
              </code>
              <span style={{ flexShrink: 0 }}>{Math.round(c.chars / 4).toLocaleString()} tok</span>
              <span style={{ flexShrink: 0 }}>{pct.toFixed(0)}%</span>
            </div>
            <div style={{ height: 3, borderRadius: 2, background: "var(--surface)", overflow: "hidden" }}>
              <div style={{
                height: "100%", width: maxChars > 0 ? `${Math.max(2, (c.chars / maxChars) * 100)}%` : "0%",
                background: "var(--accent, var(--amber))", borderRadius: 2,
              }} />
            </div>
          </div>
        );
      })}
    </>
  );
}

function CostRows({ costs, maxChars }: { costs: LineCost[]; maxChars: number }) {
  const total = costs.reduce((sum, c) => sum + c.chars, 0);
  return (
    <>
      {costs.map((c, i) => {
        const pct = total > 0 ? (c.chars / total) * 100 : 0;
        return (
          <div key={`${c.line}:${c.col}:${i}`} style={{ display: "flex", flexDirection: "column", gap: 2, padding: "3px 4px" }}>
            <div style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 10, color: "var(--text-ghost)" }}>
              <code style={{ flexShrink: 0 }}>L{c.line}</code>
              <span style={{ flex: 1 }}>{Math.round(c.chars / 4).toLocaleString()} tok</span>
              <span>{pct.toFixed(0)}%</span>
            </div>
            <div style={{ height: 3, borderRadius: 2, background: "var(--surface)", overflow: "hidden" }}>
              <div style={{
                height: "100%", width: maxChars > 0 ? `${Math.max(2, (c.chars / maxChars) * 100)}%` : "0%",
                background: "var(--accent, var(--amber))", borderRadius: 2,
              }} />
            </div>
          </div>
        );
      })}
    </>
  );
}

// The combined write-agent program's own source, built once from real
// per-piece text -- lore's own live draft (verbatim), then one bare
// function-call line per remaining piece `writeAgent` itself resolves.
// This is genuinely ONE program from here on: no slot bookkeeping, no
// name labels standing in for pieces of it -- the cost view below reads
// each returned `LineCost.line` straight back against this same text to
// show its own real source, exactly like any other source-mapped
// diagnostic would.
function buildWriteProgram(path: string, loreProgram: string, activeCharNames: string[], pinnedNames: readonly string[]): string {
  return [
    loreProgram,
    "context.chapters",
    // Bracket-glob, not a quoted string -- the DSL-source convention for
    // a literal path (see the checkbox-generated lore block's own
    // `loreEntry [path]`, dslCompose.ts). No DSL source anywhere calls
    // context.other with a literal path (every real call site passes a
    // bound `path` identifier -- Library.hs:458's `context.other path`),
    // so this doesn't match an existing precedent either way, but
    // `[...]` is what marks "this is a path" in this DSL's own surface
    // syntax; `exclude(path)` inside contextOtherDef ends up matching
    // the same text regardless of which syntax produced it, so this is a
    // convention choice, not a behavior difference.
    `context.other [${path}]`,
    "context.style",
    // context.character's charname is a NAME, not a path -- a quoted
    // string is the right kind of literal here, unrelated to the
    // path/bracket-glob question above.
    ...activeCharNames.map((name) => `context.character ${JSON.stringify(name)}`),
    ...pinnedNames,
  ].join("\n");
}

export function ContextCostSidebar({ activeBranch, selectedFile, fileChainTicks, fileChainHead }: ContextCostSidebarProps) {
  const mode = useUI((s) => s.writerMode);
  const pinnedNames = useCallContext((s) => (selectedFile ? s.files[selectedFile]?.pinnedProgramNames : undefined)) ?? EMPTY_PINNED_PROGRAMS;
  const pinned = usePinnedSnippetCosts(activeBranch, selectedFile);
  // Display-only, local to this panel -- see writeLinesGrouped's own
  // header for what each toggle does.
  const [hideZeroCost, setHideZeroCost] = useState(true);
  const [sortByCost, setSortByCost] = useState(false);

  // The exact same lore text a real send would put on the wire -- never a
  // bare `context.lore` reference, never a reconstruction (see
  // useLoreDraft's own header).
  const { resetTarget: loreDefault, sendText: loreSendText } =
    useLoreDraft(selectedFile, mode === "write" ? activeBranch : null);
  const loreProgram = loreSendText ?? loreDefault;

  // Presence is scoped to the open file (a scene), same as
  // writeAgent/activeCharacterContext itself (activeCharactersFor) --
  // reusing the same activeCharacterBranches util the file view's own
  // presence bars already use, not a second derivation.
  const activeCharNames = useMemo(
    () => activeCharacterBranches(fileChainTicks, fileChainHead).map(branchDisplayName),
    [fileChainTicks, fileChainHead],
  );

  const writeProgram = useMemo(
    () => (mode === "write" && selectedFile
      // selectedFile is a raw, still-URL-encoded path segment (see
      // filetree.tsx/page.tsx's own decodeURIComponent calls) -- but
      // context.other's `path` argument gets glob-matched against real,
      // decoded entry keys in the tree (Storyteller.Context.DSL.Compile's
      // globMatches), so an encoded path like "file%20with%20spaces.md"
      // would silently match nothing. Decode once here, the same
      // boundary every other display/use site in this app already
      // decodes at.
      ? buildWriteProgram(decodeURIComponent(selectedFile), loreProgram, activeCharNames, pinnedNames)
      : null),
    [mode, selectedFile, loreProgram, activeCharNames, pinnedNames],
  );
  const writeProgramLines = useMemo(() => writeProgram?.split("\n") ?? [], [writeProgram]);
  const { costs: writeCosts, loading: writeLoading } = useProgramCost(activeBranch, selectedFile, writeProgram);

  if (!selectedFile) {
    return (
      <div style={{ padding: 12, fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic" }}>
        Open a file to see its context cost.
      </div>
    );
  }

  const writeTotal = writeCosts?.reduce((sum, c) => sum + c.chars, 0) ?? 0;
  const writeMax = writeCosts?.reduce((m, c) => Math.max(m, c.chars), 0) ?? 0;
  // Grouped by source line -- a line can carry more than one ablated
  // statement (e.g. `as f: x` has its own cost for the `as f:` wrapper
  // and a separate one for `x`, the nested body it binds), so a line
  // renders once with each of its own statements as its own row (see
  // SourceLineRow), not duplicated per statement.
  //
  // `hideZeroCost` drops lines whose statements *all* round to 0 tok --
  // real signal (a nested `as`/`for` wrapper genuinely contributing
  // nothing on its own, distinct from the body it wraps) but usually
  // noise for "where am I paying," so it's opt-in to see, not opt-in to
  // hide. `sortByCost` reorders the visible lines by their own highest
  // single statement's cost (descending) instead of source order -- the
  // per-row bar already shows weight at a glance either way, this is
  // purely about which lines surface first when there are many.
  const writeLinesGrouped = useMemo(() => {
    const byLine = new Map<number, LineCost[]>();
    for (const c of writeCosts ?? []) {
      byLine.set(c.line, [...(byLine.get(c.line) ?? []), c].sort((a, b) => a.col - b.col));
    }
    let entries = [...byLine.entries()];
    if (hideZeroCost) entries = entries.filter(([, cs]) => cs.some((c) => c.chars >= 4));
    entries.sort(sortByCost
      ? ([, a], [, b]) => Math.max(...b.map((c) => c.chars)) - Math.max(...a.map((c) => c.chars))
      : ([a], [b]) => a - b);
    return entries;
  }, [writeCosts, hideZeroCost, sortByCost]);

  return (
    <div style={{ display: "flex", flexDirection: "column", height: "100%" }}>
      <div style={{
        display: "flex", alignItems: "center", gap: 6, padding: "8px 10px",
        borderBottom: "1px solid var(--border-subtle)", flexShrink: 0,
      }}>
        <Gauge style={{ width: 12, height: 12, color: "var(--text-dim)" }} />
        <span style={{ fontSize: 11, fontWeight: 500, color: "var(--foreground)", flex: 1 }}>
          Context cost — {mode} agent
        </span>
      </div>

      <div style={{ flex: 1, overflowY: "auto", padding: "6px 8px" }}>
        {mode === "write" ? (
          <>
            <div style={{
              display: "flex", alignItems: "center", gap: 6, fontSize: 10.5,
              color: "var(--text-dim)", padding: "2px 2px 8px",
            }}>
              <span>One combined call, as writeAgent assembles it</span>
              {writeCosts && !writeLoading && (
                <span style={{ marginLeft: "auto", fontWeight: 500, color: "var(--foreground)" }}>
                  ~{Math.round(writeTotal / 4).toLocaleString()} tok total
                </span>
              )}
              {writeLoading && <RefreshCw className="animate-spin" style={{ width: 10, height: 10, color: "var(--text-ghost)" }} />}
            </div>
            <div style={{ display: "flex", gap: 4, padding: "0 2px 8px" }}>
              <button
                onClick={() => setHideZeroCost((v) => !v)}
                style={{
                  fontSize: 9.5, padding: "2px 7px", borderRadius: 10, cursor: "pointer",
                  border: "1px solid var(--border-subtle)",
                  background: hideZeroCost ? "var(--accent-tint, var(--amber-tint))" : "transparent",
                  color: hideZeroCost ? "var(--accent, var(--amber))" : "var(--text-ghost)",
                }}
              >
                Hide 0-tok lines
              </button>
              <button
                onClick={() => setSortByCost((v) => !v)}
                style={{
                  fontSize: 9.5, padding: "2px 7px", borderRadius: 10, cursor: "pointer",
                  border: "1px solid var(--border-subtle)",
                  background: sortByCost ? "var(--accent-tint, var(--amber-tint))" : "transparent",
                  color: sortByCost ? "var(--accent, var(--amber))" : "var(--text-ghost)",
                }}
              >
                Sort by cost
              </button>
            </div>
            {writeCosts && writeLinesGrouped.map(([lineNo, lineCosts]) => (
              <SourceLineRow
                key={lineNo}
                sourceLine={writeProgramLines[lineNo - 1] ?? ""}
                costs={lineCosts}
                maxChars={writeMax}
              />
            ))}
            <div style={{
              display: "flex", gap: 6, fontSize: 10, color: "var(--text-ghost)",
              padding: "6px 4px", borderTop: "1px solid var(--border-subtle)", marginTop: 4,
            }}>
              <Info style={{ width: 11, height: 11, flexShrink: 0, marginTop: 1 }} />
              <span>
                Not included: each active character&apos;s own journal-summary reshaping
                and this file&apos;s own conversation history — both computed by the agent
                itself, not representable as DSL source to preview here.
              </span>
            </div>
          </>
        ) : (
          <div style={{ fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic", padding: 8 }}>
            Context cost estimation isn&apos;t modeled for the {mode} agent yet — switch
            InputBar to Write to see its context breakdown.
          </div>
        )}

        {pinned.length > 0 && mode !== "write" && (
          <div style={{ marginTop: 14, paddingTop: 10, borderTop: "1px solid var(--border-subtle)" }}>
            <div style={{ fontSize: 10.5, color: "var(--text-dim)", padding: "2px 2px 8px" }}>
              Pinned snippets (any agent)
            </div>
            {pinned.map(({ name, costs, loading }) => {
              const total = costs?.reduce((sum, c) => sum + c.chars, 0) ?? 0;
              const maxChars = costs?.reduce((m, c) => Math.max(m, c.chars), 0) ?? 0;
              return (
                <div key={name} style={{ marginBottom: 10 }}>
                  <div style={{
                    display: "flex", alignItems: "center", gap: 6, fontSize: 11,
                    color: "var(--foreground)", marginBottom: 3,
                  }}>
                    <code style={{ fontFamily: "monospace", color: "var(--accent, var(--amber))" }}>{name}</code>
                    {loading && <RefreshCw className="animate-spin" style={{ width: 10, height: 10, color: "var(--text-ghost)" }} />}
                    {costs && (
                      <span style={{ marginLeft: "auto", fontSize: 10, color: "var(--text-ghost)" }}>
                        ~{Math.round(total / 4).toLocaleString()} tok
                      </span>
                    )}
                  </div>
                  {costs && <CostRows costs={costs} maxChars={maxChars} />}
                </div>
              );
            })}
          </div>
        )}
      </div>

      <div style={{
        display: "flex", alignItems: "center", gap: 4, padding: "6px 10px",
        borderTop: "1px solid var(--border-subtle)", fontSize: 9.5, color: "var(--text-ghost)",
        flexShrink: 0,
      }}>
        <AlertCircle style={{ width: 10, height: 10, flexShrink: 0 }} />
        Estimated by re-running each statement removed -- an approximation, not a real tokenizer.
      </div>
    </div>
  );
}
