"use client";

// Agents tab: Windows-settings-style master/detail. Left is a fixed list of
// the agents that apply to the open file (see lib/agents.ts for the registry
// and the appliesTo predicates — e.g. Chat only for chat/* files, Outline
// split only for outline.md); clicking one swaps the right-hand detail pane,
// there is no accordion/expand-collapse. Most agents have no real "settings"
// at all (no toggles/fields) and that's expected to stay the norm — the one
// real, functional setting today is the context filter (contextpreview.tsx /
// ContextSourceConfig, previewed live against the server), so the detail
// pane gives that room instead of squeezing it behind a click. Below it,
// which of that agent's prompts currently have an override committed on the
// "prompts" branch vs. falling back to the compiled-in default
// (Storyteller.Core.Prompt) — an overridden prompt's text is expandable and
// editable in place (see PromptEditor); a default one isn't, because its
// text only exists as a literal in Haskell source, unreachable over the
// wire — there's nothing to fetch or fall back to showing.

import { useEffect, useState } from "react";
import {
  ChevronRight, FileText, Layers,
  PenLine, Wrench, RefreshCw, Split, MessageSquare, Bot,
} from "lucide-react";
import { AGENTS, promptKeyToPath, contextModeDescription, type AgentDef } from "@/lib/agents";
import { branchConn, branchFileUrl, uploadBranchFile } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { ContextSourceConfig } from "./context-source";

const AGENT_ICONS: Record<string, typeof PenLine> = {
  writer: PenLine,
  fixer: Wrench,
  regenBeatSheet: RefreshCw,
  outlineSplit: Split,
  chat: MessageSquare,
};

// One connection for the whole tab — every agent's prompt list reads the
// same branch file list, just checks different paths in it.
function usePromptFiles() {
  const [files, setFiles] = useState<string[] | null>(null);

  useEffect(() => {
    const label = "branch:prompts";
    setConnStatus(label, "connecting");
    const conn = branchConn("prompts");

    conn.subscribe((evt) => {
      bumpActivity(label);
      if (evt.type === "branch.ready") setFiles(evt.files);
      else if (evt.type === "file.added") setFiles((fs) => (fs ? [...fs, evt.path] : fs));
      else if (evt.type === "file.removed") setFiles((fs) => (fs ? fs.filter((f) => f !== evt.path) : fs));
    });

    (async () => {
      try {
        await conn.connect();
        setConnStatus(label, "connected");
      } catch {
        setConnStatus(label, "error");
      }
    })();

    return () => { conn.close(); removeConn(label); };
  }, []);

  return files;
}

function SectionLabel({ icon: Icon, children }: { icon: typeof Layers; children: React.ReactNode }) {
  return (
    <div style={{
      padding: "10px 16px 6px", fontSize: 10, fontWeight: 600, color: "var(--text-dim)",
      textTransform: "uppercase", letterSpacing: "0.08em", display: "flex", alignItems: "center", gap: 6,
    }}>
      <Icon style={{ width: 11, height: 11 }} />
      {children}
    </div>
  );
}

// Fetches an overridden prompt's text on expand (raw GET against the
// "prompts" branch — same HTTP endpoint the file-embed/download path uses,
// see lib/ws.ts's branchFileUrl) and saves edits back with a plain PUT
// (uploadBranchFile), i.e. a full-content replace, not the atom
// chain-editing pipeline — appropriate here since a prompt override is a
// single opaque blob, not chain-tracked prose (see
// Server.Writer.Branch.hs's "deposit, not a claim" doc comment).
function PromptEditor({ path }: { path: string }) {
  const [content, setContent] = useState<string | null>(null);
  const [draft, setDraft] = useState("");
  const [saving, setSaving] = useState(false);
  const [loadError, setLoadError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setContent(null);
    setLoadError(null);
    fetch(branchFileUrl("prompts", path))
      .then((res) => {
        if (!res.ok) throw new Error(`${res.status} ${res.statusText}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text);
        setDraft(text);
      })
      .catch((err) => { if (!cancelled) setLoadError(String(err)); });
    return () => { cancelled = true; };
  }, [path]);

  async function save() {
    setSaving(true);
    try {
      await uploadBranchFile("prompts", path, new Blob([draft], { type: "text/markdown" }));
      setContent(draft);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  if (loadError) {
    return <div style={{ padding: "6px 8px", fontSize: 10.5, color: "oklch(0.65 0.18 25)" }}>failed to load: {loadError}</div>;
  }
  if (content === null) {
    return (
      <div style={{ display: "flex", alignItems: "center", gap: 6, padding: "6px 8px", fontSize: 10.5, color: "var(--text-ghost)" }}>
        <RefreshCw style={{ width: 10, height: 10 }} className="animate-spin" /> loading…
      </div>
    );
  }

  const dirty = draft !== content;
  return (
    <div style={{ padding: "6px 8px 8px", display: "flex", flexDirection: "column", gap: 5 }}>
      <textarea
        value={draft}
        onChange={(e) => setDraft(e.target.value)}
        rows={10}
        style={{
          width: "100%", resize: "vertical", fontFamily: "monospace", fontSize: 11, lineHeight: 1.5,
          padding: 8, borderRadius: 5, border: "1px solid var(--border-subtle)",
          background: "var(--card)", color: "var(--foreground)",
        }}
      />
      <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
        <button
          onClick={save}
          disabled={!dirty || saving}
          style={{
            fontSize: 10.5, padding: "3px 10px", borderRadius: 4, border: "1px solid var(--border-subtle)",
            background: dirty ? "oklch(0.78 0.10 65 / 0.15)" : "var(--surface)",
            color: dirty ? "var(--amber)" : "var(--text-ghost)",
            cursor: dirty && !saving ? "pointer" : "default",
          }}
        >
          {saving ? "saving…" : dirty ? "save" : "saved"}
        </button>
        {dirty && !saving && (
          <button
            onClick={() => setDraft(content)}
            style={{ fontSize: 10.5, padding: "3px 10px", borderRadius: 4, border: "none", background: "none", color: "var(--text-ghost)", cursor: "pointer" }}
          >
            revert
          </button>
        )}
      </div>
    </div>
  );
}

function PromptOverrides({ promptKeys, files }: { promptKeys: string[]; files: string[] | null }) {
  const [expanded, setExpanded] = useState<Set<string>>(new Set());

  if (promptKeys.length === 0) {
    return <div style={{ padding: "0 16px 12px", fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>Instant, non-LLM action — no prompts.</div>;
  }

  function toggle(key: string) {
    setExpanded((prev) => {
      const next = new Set(prev);
      if (next.has(key)) next.delete(key); else next.add(key);
      return next;
    });
  }

  return (
    <div style={{ padding: "0 14px 12px", display: "flex", flexDirection: "column", gap: 4 }}>
      {promptKeys.map((key) => {
        const active = files?.includes(promptKeyToPath(key)) ?? false;
        const open = active && expanded.has(key);
        return (
          <div key={key} style={{ borderRadius: 5, background: "var(--surface)", overflow: "hidden" }}>
            <button
              onClick={() => active && toggle(key)}
              disabled={!active}
              style={{
                display: "flex", alignItems: "center", gap: 6, fontSize: 10.5, width: "100%",
                padding: "5px 8px", border: "none", background: "none", textAlign: "left",
                cursor: active ? "pointer" : "default",
              }}
            >
              {active ? (
                <ChevronRight style={{ width: 10, height: 10, color: "var(--text-dim)", flexShrink: 0, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
              ) : (
                <FileText style={{ width: 10, height: 10, color: "var(--text-dim)", flexShrink: 0 }} />
              )}
              <span style={{ fontFamily: "monospace", color: "var(--text-secondary)" }}>{key}</span>
              <span style={{
                marginLeft: "auto", fontSize: 9, padding: "1px 6px", borderRadius: 8,
                background: active ? "oklch(0.78 0.10 65 / 0.15)" : "var(--card)",
                color: active ? "var(--amber)" : "var(--text-ghost)",
                border: active ? "1px solid oklch(0.78 0.10 65 / 0.35)" : "1px solid var(--border-subtle)",
              }}>
                {active ? "override" : "default"}
              </span>
            </button>
            {open && <PromptEditor path={promptKeyToPath(key)} />}
          </div>
        );
      })}
    </div>
  );
}

// Groups by first appearance, category-less agents form a leading, unlabeled
// group — so a mix of categorized/uncategorized agents still renders
// sensibly instead of requiring an all-or-nothing migration.
function groupByCategory(agents: AgentDef[]): { category: string | null; agents: AgentDef[] }[] {
  const groups: { category: string | null; agents: AgentDef[] }[] = [];
  for (const agent of agents) {
    const category = agent.category ?? null;
    const existing = groups.find((g) => g.category === category);
    if (existing) existing.agents.push(agent);
    else groups.push({ category, agents: [agent] });
  }
  return groups;
}

export function AgentsTab({ activeBranch, path }: { activeBranch: string | null; path: string }) {
  const applicable = AGENTS.filter((a) => a.appliesTo(path));
  const [selectedId, setSelectedId] = useState<string | null>(applicable[0]?.id ?? null);
  const promptFiles = usePromptFiles();

  if (applicable.length === 0) {
    return (
      <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
        No agents apply to this file.
      </div>
    );
  }

  const selected = applicable.find((a) => a.id === selectedId) ?? applicable[0];

  return (
    <div style={{ flex: 1, display: "flex", overflow: "hidden" }}>
      <div style={{ width: 190, minWidth: 190, borderRight: "1px solid var(--border-subtle)", background: "var(--sidebar)", overflow: "auto", padding: 6 }}>
        {groupByCategory(applicable).map((group, i) => (
          <div key={group.category ?? `_ungrouped_${i}`}>
            {group.category && (
              <div style={{
                padding: "8px 9px 3px", fontSize: 9.5, fontWeight: 600, color: "var(--text-dim)",
                textTransform: "uppercase", letterSpacing: "0.08em",
              }}>
                {group.category}
              </div>
            )}
            {group.agents.map((agent) => {
              const Icon = AGENT_ICONS[agent.id] ?? Bot;
              const active = agent.id === selected.id;
              return (
                <button
                  key={agent.id}
                  onClick={() => setSelectedId(agent.id)}
                  style={{
                    display: "flex", alignItems: "center", gap: 8, width: "100%", textAlign: "left",
                    padding: "7px 9px", marginBottom: 1, border: "none", cursor: "pointer",
                    borderRadius: 5,
                    background: active ? "oklch(0.78 0.10 65 / 0.10)" : "transparent",
                    borderLeft: active ? "2px solid var(--amber)" : "2px solid transparent",
                  }}
                >
                  <Icon style={{ width: 13, height: 13, flexShrink: 0, color: active ? "var(--amber)" : "var(--text-dim)" }} />
                  <span style={{ fontSize: 12, fontWeight: active ? 500 : 400, color: active ? "var(--amber)" : "var(--text-secondary)" }}>{agent.label}</span>
                </button>
              );
            })}
          </div>
        ))}
      </div>

      <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "auto" }}>
        <div style={{ padding: "12px 16px", borderBottom: "1px solid var(--border-subtle)" }}>
          <div style={{ fontSize: 13, fontWeight: 600, color: "var(--text-heading)" }}>{selected.label}</div>
          <div style={{ fontSize: 11, color: "var(--text-ghost)", marginTop: 2 }}>{selected.description}</div>
        </div>

        <SectionLabel icon={Layers}>Context</SectionLabel>
        {selected.contextSources.length === 0 ? (
          <div style={{ padding: "0 16px 16px", fontSize: 11, color: "var(--text-ghost)", fontStyle: "italic" }}>
            No configurable context — reads only the selected content directly.
          </div>
        ) : selected.contextSources.map((source) => (
          <div key={source.id} style={{ margin: "0 14px 14px", border: "1px solid var(--border-subtle)", borderRadius: 6, overflow: "hidden" }}>
            <div style={{ padding: "8px 10px", background: "var(--surface-deep)", borderBottom: "1px solid var(--border-subtle)" }}>
              <div style={{ display: "flex", alignItems: "center", gap: 6 }}>
                <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-heading)" }}>{source.label}</span>
                <span style={{
                  fontSize: 9, padding: "1px 6px", borderRadius: 8,
                  background: source.mode === "ambient" ? "oklch(0.78 0.10 65 / 0.15)" : "oklch(0.58 0.10 200 / 0.12)",
                  color: source.mode === "ambient" ? "var(--amber)" : "oklch(0.68 0.12 200)",
                }}>
                  {source.mode === "ambient" ? "sent every time" : "available on demand"}
                </span>
              </div>
              <p style={{ margin: "5px 0 0", fontSize: 10.5, color: "var(--text-muted)", lineHeight: 1.5, maxWidth: 560 }}>
                {contextModeDescription(source.mode)}
              </p>
            </div>
            <ContextSourceConfig activeBranch={activeBranch} path={path} sourceId={`${selected.id}:${source.id}`} label={source.label} mode={source.mode} />
          </div>
        ))}

        <SectionLabel icon={FileText}>Prompts</SectionLabel>
        <PromptOverrides promptKeys={selected.promptKeys} files={promptFiles} />
      </div>
    </div>
  );
}
