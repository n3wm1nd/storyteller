"use client";

// Agents tab: one collapsible section per agent that applies to the open
// file (see lib/agents.ts for the registry and the appliesTo predicates —
// e.g. Chat only for chat/* files, Outline split only for outline.md).
// Each section holds that agent's own context-slot preview (contextpreview.tsx,
// still preview-only — see its header comment) and, below it, which of its
// prompts currently have an override committed on the "prompts" branch vs.
// falling back to the compiled-in default (Storyteller.Core.Prompt).

import { useEffect, useState } from "react";
import { ChevronDown, ChevronRight, FileText } from "lucide-react";
import { AGENTS, promptKeyToPath } from "@/lib/agents";
import { branchConn } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity } from "@/lib/uiStore";
import { ContextSourceConfig } from "./context-source";

// One connection for the whole tab — every section reads the same branch
// file list, just checks different paths in it.
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

function PromptOverrides({ promptKeys, files }: { promptKeys: string[]; files: string[] | null }) {
  if (promptKeys.length === 0) {
    return <div style={{ padding: "8px 10px", fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>Instant, non-LLM action — no prompts.</div>;
  }
  return (
    <div style={{ padding: "6px 10px", display: "flex", flexDirection: "column", gap: 4 }}>
      {promptKeys.map((key) => {
        const active = files?.includes(promptKeyToPath(key)) ?? false;
        return (
          <div key={key} style={{ display: "flex", alignItems: "center", gap: 6, fontSize: 10.5 }}>
            <FileText style={{ width: 10, height: 10, color: "var(--text-dim)", flexShrink: 0 }} />
            <span style={{ fontFamily: "monospace", color: "var(--text-secondary)" }}>{key}</span>
            <span style={{
              marginLeft: "auto", fontSize: 9, padding: "1px 6px", borderRadius: 8,
              background: active ? "oklch(0.78 0.10 65 / 0.15)" : "var(--card)",
              color: active ? "var(--amber)" : "var(--text-ghost)",
              border: active ? "1px solid oklch(0.78 0.10 65 / 0.35)" : "1px solid var(--border-subtle)",
            }}>
              {active ? "override" : "default"}
            </span>
          </div>
        );
      })}
    </div>
  );
}

export function AgentsTab({ activeBranch, path }: { activeBranch: string | null; path: string }) {
  const applicable = AGENTS.filter((a) => a.appliesTo(path));
  const [openId, setOpenId] = useState<string | null>(applicable[0]?.id ?? null);
  const promptFiles = usePromptFiles();

  return (
    <div style={{ flex: 1, overflow: "auto" }}>
      {applicable.length === 0 && (
        <div style={{ padding: 16, fontSize: 11, color: "var(--text-ghost)" }}>No agents apply to this file.</div>
      )}
      {applicable.map((agent) => {
        const open = openId === agent.id;
        return (
          <div key={agent.id} style={{ borderBottom: "1px solid var(--border-subtle)" }}>
            <button
              onClick={() => setOpenId(open ? null : agent.id)}
              style={{
                display: "flex", alignItems: "center", gap: 6, width: "100%", textAlign: "left",
                padding: "8px 10px", border: "none", background: "transparent", cursor: "pointer",
              }}
            >
              {open ? <ChevronDown style={{ width: 12, height: 12, color: "var(--text-dim)" }} /> : <ChevronRight style={{ width: 12, height: 12, color: "var(--text-dim)" }} />}
              <span style={{ fontSize: 12, fontWeight: 600, color: "var(--text-heading)" }}>{agent.label}</span>
              <span style={{ marginLeft: 8, fontSize: 10.5, color: "var(--text-ghost)" }}>{agent.description}</span>
            </button>
            {open && (
              <div style={{ display: "flex", flexDirection: "column" }}>
                <div style={{ padding: "2px 10px 6px", fontSize: 10, fontWeight: 600, color: "var(--text-dim)" }}>
                  CONTEXT
                </div>
                {agent.contextSources.length === 0 ? (
                  <div style={{ padding: "0 10px 8px", fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>
                    No configurable context — reads only the selected content directly.
                  </div>
                ) : agent.contextSources.map((source) => (
                  <div key={source.id} style={{ borderTop: "1px solid var(--border-subtle)" }}>
                    <div style={{ display: "flex", alignItems: "center", gap: 6, padding: "6px 10px" }}>
                      <span style={{ fontSize: 11, fontWeight: 600, color: "var(--text-heading)" }}>{source.label}</span>
                      <span style={{
                        fontSize: 9, padding: "1px 6px", borderRadius: 8,
                        background: source.mode === "ambient" ? "oklch(0.78 0.10 65 / 0.15)" : "oklch(0.58 0.10 200 / 0.12)",
                        color: source.mode === "ambient" ? "var(--amber)" : "oklch(0.68 0.12 200)",
                      }}>
                        {source.mode}
                      </span>
                    </div>
                    <div style={{ height: 260, borderTop: "1px solid var(--border-subtle)", borderBottom: "1px solid var(--border-subtle)" }}>
                      <ContextSourceConfig activeBranch={activeBranch} path={path} sourceId={`${agent.id}:${source.id}`} label={source.label} mode={source.mode} />
                    </div>
                  </div>
                ))}
                <div style={{ padding: "6px 10px 2px", fontSize: 10, fontWeight: 600, color: "var(--text-dim)" }}>
                  PROMPTS
                </div>
                <PromptOverrides promptKeys={agent.promptKeys} files={promptFiles} />
              </div>
            )}
          </div>
        );
      })}
    </div>
  );
}
