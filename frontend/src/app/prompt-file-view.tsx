"use client";

// The prompt surface: how a file on the `prompts` branch is edited (see
// lib/fileSurface.ts). Everything on that branch is agent-facing text —
// `<dotted/key>.md` is an agent's system prompt or template,
// `<dotted/key>.llmsettings.yaml` its sampling override (see
// Storyteller.Core.Prompt's getPrompt/getConfig).
//
// Why this isn't the prose view: an override is a single unit of text that
// a backend call reads whole. It has no atoms to select, nobody is present
// in it, and no agent writes into it — the Agents tab's own editor
// (agentstab.tsx) has always treated it that way, and reaching it through
// "jump to prompt" used to drop you into an atom view that disagreed.
// Saves here go through `saveRawFileWhole` for the same reason the DSL
// editor does: one atom holding the whole file (Storage.Ops.saveWholeFile),
// so the file stays atom-tracked (undo, no bogus "binary" flag in the
// library) without being diffed into paragraph-shaped pieces.
//
// Format-aware only where it earns it: the sampling config is YAML, so it
// gets the sensible-for-YAML editor affordances, while prompt text is
// prose meant for an LLM and gets a plain wrapped textarea rather than
// code chrome. Nothing here offers "revert to the compiled-in default" —
// a default prompt's text only exists at its Haskell call site (the same
// reason agentstab.tsx can't show one either), so there is nothing honest
// to revert to.

import { useCallback, useEffect, useMemo, useState } from "react";
import { Save } from "lucide-react";
import { branchFileUrl, saveRawFileWhole } from "@/lib/ws";
import { AGENTS, promptKeyToPath, configKeyToPath, configFieldsHint } from "@/lib/agents";

// The dotted Prompt.hs key a path on the prompts branch came from — the
// inverse of promptKeyToPath/configKeyToPath. Null for a path matching
// neither shape (nothing today writes one, but the branch is an ordinary
// branch and could hold one).
export function promptKeyOf(path: string): { key: string; kind: "prompt" | "config" } | null {
  if (path.endsWith(".llmsettings.yaml")) {
    return { key: path.slice(0, -".llmsettings.yaml".length).split("/").join("."), kind: "config" };
  }
  if (path.endsWith(".md")) {
    return { key: path.slice(0, -".md".length).split("/").join("."), kind: "prompt" };
  }
  return null;
}

export function PromptFileView({ branch, path }: { branch: string; path: string }) {
  const [content, setContent] = useState<string | null>(null);
  const [savedContent, setSavedContent] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const meta = promptKeyOf(path);
  const isConfig = meta?.kind === "config";

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    setContent(null);
    fetch(branchFileUrl(branch, path))
      .then((res) => {
        // No override committed yet is the ordinary case, not an error —
        // this editor is how the first one gets written.
        if (res.status === 404) return "";
        if (!res.ok) throw new Error(`load failed: ${res.status}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text);
        setSavedContent(text);
        setLoading(false);
      })
      .catch((err) => {
        if (cancelled) return;
        setError(err instanceof Error ? err.message : String(err));
        setLoading(false);
      });
    return () => { cancelled = true; };
  }, [branch, path]);

  const dirty = content !== null && content !== savedContent;

  const save = useCallback(() => {
    if (content === null || saving) return;
    setSaving(true);
    setError(null);
    saveRawFileWhole(branch, path, content)
      .then(() => { setSavedContent(content); setSaving(false); })
      .catch((err) => {
        setError(err instanceof Error ? err.message : String(err));
        setSaving(false);
      });
  }, [branch, path, content, saving]);

  return (
    <div style={{ flex: 1, display: "flex", flexDirection: "column", overflow: "hidden" }}>
      <div style={{
        flexShrink: 0, padding: "3px 14px", borderBottom: "1px solid var(--border-subtle)",
        display: "flex", alignItems: "center", gap: 8, fontSize: 10,
      }}>
        <span style={{ color: "var(--text-ghost)" }}>
          {isConfig ? "Sampling config (YAML)" : "Prompt override"}
          {meta && <> · <code style={{ fontFamily: "monospace", color: "var(--text-dim)" }}>{meta.key}</code></>}
        </span>
        <span style={{ flex: 1 }} />
        {error && <span style={{ color: "var(--rose)" }}>{error}</span>}
        {dirty && !error && <span style={{ color: "var(--amber)" }}>unsaved</span>}
        <button
          onClick={save}
          disabled={!dirty || saving}
          title="Save the whole file as its single atom (⌘S / Ctrl+S)"
          style={{
            display: "inline-flex", alignItems: "center", gap: 4,
            fontSize: 10, padding: "2px 10px", borderRadius: 4,
            cursor: dirty && !saving ? "pointer" : "default",
            background: dirty ? "var(--amber-tint)" : "transparent",
            border: "1px solid " + (dirty ? "var(--amber-border)" : "var(--border-subtle)"),
            color: dirty ? "var(--amber)" : "var(--text-dim)",
          }}
        >
          <Save style={{ width: 10, height: 10 }} />
          {saving ? "Saving…" : "Save"}
        </button>
      </div>

      {loading ? (
        <div style={{ flex: 1, display: "flex", alignItems: "center", justifyContent: "center", color: "var(--text-ghost)", fontSize: 12 }}>
          Loading…
        </div>
      ) : (
        <textarea
          value={content ?? ""}
          onChange={(e) => setContent(e.target.value)}
          onKeyDown={(e) => {
            if ((e.metaKey || e.ctrlKey) && e.key === "s") { e.preventDefault(); save(); }
          }}
          spellCheck={!isConfig}
          placeholder={isConfig
            ? "# temperature: 0.9\n# maxTokens: 4000"
            : "The system prompt this agent runs with. Empty means the compiled-in default."}
          style={{
            flex: 1, width: "100%", resize: "none", border: "none", outline: "none",
            padding: "14px 18px", background: "transparent", color: "var(--text-body)",
            // YAML is whitespace-significant and read structurally; prompt
            // text is prose for an LLM and reads better set like prose.
            fontFamily: isConfig ? "ui-monospace, monospace" : "inherit",
            fontSize: isConfig ? 12 : 13,
            lineHeight: isConfig ? 1.5 : 1.7,
            whiteSpace: isConfig ? "pre" : "pre-wrap",
          }}
        />
      )}
    </div>
  );
}

// ── Sidebar ───────────────────────────────────────────────────────────────────

// This surface's own sidebar: who actually reads this file. A prompt
// override is only meaningful through the agent(s) that look its key up,
// and that mapping already exists in the agent registry — so rather than
// guessing from the path, this reads the same list the Agents tab does.
// The sibling file (a prompt's config, or the config's prompt) is one
// click away, since editing one almost always means looking at the other.
export function PromptSidebar({ path, onOpenFile }: {
  path: string;
  onOpenFile: (path: string) => void;
}) {
  const meta = promptKeyOf(path);

  // Every agent that reads this key, and whether it's that agent's system
  // prompt (its first key — the one getConfigWithPrompt is called with) or
  // a secondary template.
  const readers = useMemo(() => {
    if (!meta) return [];
    return AGENTS
      .filter((a) => a.promptKeys.includes(meta.key))
      .map((a) => ({ agent: a, isSystem: a.promptKeys[0] === meta.key }));
  }, [meta]);

  const siblingPath = meta
    ? (meta.kind === "prompt" ? configKeyToPath(meta.key) : promptKeyToPath(meta.key))
    : null;

  return (
    <div style={{ flex: 1, minHeight: 0, overflow: "auto", padding: "10px 10px 0", fontSize: 11 }}>
      <div style={{ fontSize: 11, color: "var(--text-label)", paddingBottom: 6 }}>Read by</div>

      {readers.length === 0 ? (
        <div style={{ color: "var(--text-ghost)", fontStyle: "italic", lineHeight: 1.5 }}>
          No agent in the registry reads this key — an override for something the
          UI doesn&apos;t list, or a stale file.
        </div>
      ) : (
        readers.map(({ agent, isSystem }) => (
          <div key={agent.id} style={{
            padding: "6px 8px", marginBottom: 6, borderRadius: 5,
            background: "var(--card)", border: "1px solid var(--border-subtle)",
          }}>
            <div style={{ display: "flex", alignItems: "baseline", gap: 6 }}>
              <span style={{ color: "var(--text-body)" }}>{agent.label}</span>
              <span style={{ fontSize: 9.5, color: "var(--text-ghost)" }}>
                {isSystem ? "system prompt" : "template"}
              </span>
            </div>
            <div style={{ fontSize: 10, color: "var(--text-ghost)", lineHeight: 1.45, paddingTop: 2 }}>
              {agent.description}
            </div>
            {isSystem && agent.configRole && (
              <div style={{ fontSize: 9.5, color: "var(--text-ghost)", paddingTop: 4, fontFamily: "monospace" }}>
                {configFieldsHint(agent.configRole).join(" · ")}
              </div>
            )}
          </div>
        ))
      )}

      {siblingPath && (
        <>
          <div style={{ fontSize: 11, color: "var(--text-label)", padding: "8px 0 6px" }}>
            {meta?.kind === "prompt" ? "Sampling config" : "Prompt text"}
          </div>
          <button
            onClick={() => onOpenFile(siblingPath)}
            style={{
              width: "100%", textAlign: "left", padding: "5px 8px", borderRadius: 5,
              background: "transparent", border: "1px solid var(--border-subtle)",
              color: "var(--text-dim)", cursor: "pointer",
              fontFamily: "monospace", fontSize: 10,
            }}
          >
            {siblingPath}
          </button>
        </>
      )}
    </div>
  );
}
