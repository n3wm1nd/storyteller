"use client";

// Agents tab: Windows-settings-style master/detail. Left is a fixed list of
// the agents that apply to the open file (see lib/agents.ts for the registry
// and the appliesTo predicates — e.g. Chat only for chat/* files, Outline
// split only for outline.md); clicking one swaps the right-hand detail pane,
// there is no accordion/expand-collapse. Most agents have no real "settings"
// at all (no toggles/fields) and that's expected to stay the norm.
//
// Three sections per agent: Prompts, Sampling, and (for agents with a
// named, overridable context slot — see lib/agents.ts's contextSlots)
// Context defaults. Prompts/Sampling cover which of an agent's prompts
// currently have an override committed on the "prompts" branch vs.
// falling back to the compiled-in default (Storyteller.Core.Prompt).
// Context defaults is the project-wide equivalent for a Context DSL slot
// (Storyteller.Core.Context) — e.g. Writer's "context.lore" — read off
// the "contexts" branch the same way (dots -> slashes, ".dsl" suffix, see
// lib/contextBranch.ts). This is the *default* every call falls back to;
// context-panel.tsx (above the input bar) is the *per-call* override that
// always wins over it — "Agents tab = defaults, input bar = override".
// An overridden prompt/context slot's text is expandable and editable in
// place; a default one isn't, because its text only exists as a literal
// in Haskell source, unreachable over the wire — there's nothing to fetch
// or fall back to showing.

import { useEffect, useState } from "react";
import {
  ChevronRight, FileText, Sliders, BookOpen,
  PenLine, Wrench, RefreshCw, Split, MessageSquare, Bot,
} from "lucide-react";
import { AGENTS, promptKeyToPath, configKeyToPath, configFieldsHint, type AgentDef } from "@/lib/agents";
import { branchConn, branchFileUrl, uploadBranchFile } from "@/lib/ws";
import { setConnStatus, removeConn, bumpActivity, setError } from "@/lib/uiStore";
import { contextFunctionUrl, contextsBranchName } from "@/lib/contextBranch";
import { renderLoreProgram } from "@/lib/dslCompose";
import { useLoreTree, flattenLore } from "./lore-selector";

const AGENT_ICONS: Record<string, typeof PenLine> = {
  writer: PenLine,
  fixer: Wrench,
  regenBeatSheet: RefreshCw,
  outlineSplit: Split,
  chat: MessageSquare,
};

// One connection per branch for the whole tab — every agent's prompt (or
// context-slot) list reads the same branch file list, just checks
// different paths in it. Shared by usePromptFiles ("prompts") and
// useContextFiles ("contexts") below.
function useBranchFiles(branchName: string) {
  const [files, setFiles] = useState<string[] | null>(null);

  useEffect(() => {
    const label = `branch:${branchName}`;
    setConnStatus(label, "connecting");
    const conn = branchConn(branchName);

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
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [branchName]);

  return files;
}

function usePromptFiles() {
  return useBranchFiles("prompts");
}

function useContextFiles() {
  return useBranchFiles(contextsBranchName);
}

function SectionLabel({ icon: Icon, children }: { icon: typeof FileText; children: React.ReactNode }) {
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
    return <div style={{ padding: "6px 8px", fontSize: 10.5, color: "var(--rose)" }}>failed to load: {loadError}</div>;
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
            background: dirty ? "var(--amber-tint)" : "var(--surface)",
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

function PromptOverrides({ promptKeys, files, onJumpToPrompt }: {
  promptKeys: string[];
  files: string[] | null;
  onJumpToPrompt: (path: string) => void;
}) {
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
        const path = promptKeyToPath(key);
        const active = files?.includes(path) ?? false;
        const open = active && expanded.has(key);
        return (
          <div key={key} style={{ borderRadius: 5, background: "var(--surface)", overflow: "hidden" }}>
            <div style={{ display: "flex", alignItems: "center" }}>
              {active && (
                <button
                  onClick={() => toggle(key)}
                  title={open ? "Collapse" : "Expand to view/edit"}
                  style={{
                    display: "flex", alignItems: "center", flexShrink: 0,
                    padding: "5px 0 5px 8px", border: "none", background: "none", cursor: "pointer", color: "var(--text-dim)",
                  }}
                >
                  <ChevronRight style={{ width: 10, height: 10, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
                </button>
              )}
              <button
                onClick={() => onJumpToPrompt(path)}
                title={`Open ${path} in the file view`}
                style={{
                  display: "flex", alignItems: "center", gap: 6, fontSize: 10.5, flex: 1, minWidth: 0,
                  padding: active ? "5px 8px 5px 4px" : "5px 8px", border: "none", background: "none", textAlign: "left",
                  cursor: "pointer",
                }}
              >
                {!active && <FileText style={{ width: 10, height: 10, color: "var(--text-dim)", flexShrink: 0 }} />}
                <span style={{ fontFamily: "monospace", color: "var(--text-secondary)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{key}</span>
                <span style={{
                  marginLeft: "auto", fontSize: 9, padding: "1px 6px", borderRadius: 8, flexShrink: 0,
                  background: active ? "var(--amber-tint)" : "var(--card)",
                  color: active ? "var(--amber)" : "var(--text-ghost)",
                  border: active ? "1px solid var(--amber-border)" : "1px solid var(--border-subtle)",
                }}>
                  {active ? "override" : "default"}
                </span>
              </button>
            </div>
            {open && <PromptEditor path={path} />}
          </div>
        );
      })}
    </div>
  );
}

// Same fetch/edit/save shape as PromptEditor, against the config key's
// .llmsettings.yaml sibling instead of its .md file — see
// lib/agents.ts's configKeyToPath. A blank/missing file just means "no
// overrides," same as a missing prompt file means "use the compiled-in
// default" — Storyteller.Core.Prompt.getConfig falls back to the caller's
// defaults on a missing or unparseable file.
function ConfigEditor({ path, fieldsHint }: { path: string; fieldsHint: string[] }) {
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
      await uploadBranchFile("prompts", path, new Blob([draft], { type: "application/yaml" }));
      setContent(draft);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  if (loadError) {
    return <div style={{ padding: "6px 8px", fontSize: 10.5, color: "var(--rose)" }}>failed to load: {loadError}</div>;
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
      <div style={{ fontSize: 9.5, color: "var(--text-ghost)" }}>
        Recognized keys: {fieldsHint.join(", ")}
      </div>
      <textarea
        value={draft}
        onChange={(e) => setDraft(e.target.value)}
        placeholder={`temperature: 0.8\nmaxTokens: 2048`}
        rows={5}
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
            background: dirty ? "var(--amber-tint)" : "var(--surface)",
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

// One row, not a list: unlike prompts (several independent keys per agent),
// there is exactly one config override per agent, filed under its systemKey
// (promptKeys[0] — the same key every agent already passes to
// getConfigWithPrompt on the backend). No agent, no row.
function ConfigOverride({ agent, files, onJumpToPrompt }: {
  agent: AgentDef;
  files: string[] | null;
  onJumpToPrompt: (path: string) => void;
}) {
  const [open, setOpen] = useState(false);
  const key = agent.promptKeys[0];

  if (!key) {
    return <div style={{ padding: "0 16px 12px", fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>Instant, non-LLM action — no sampling config.</div>;
  }

  const path = configKeyToPath(key);
  const active = files?.includes(path) ?? false;
  const fieldsHint = configFieldsHint(agent.configRole);

  return (
    <div style={{ padding: "0 14px 12px" }}>
      <div style={{ borderRadius: 5, background: "var(--surface)", overflow: "hidden" }}>
        <div style={{ display: "flex", alignItems: "center" }}>
          {active && (
            <button
              onClick={() => setOpen((o) => !o)}
              title={open ? "Collapse" : "Expand to view/edit"}
              style={{
                display: "flex", alignItems: "center", flexShrink: 0,
                padding: "5px 0 5px 8px", border: "none", background: "none", cursor: "pointer", color: "var(--text-dim)",
              }}
            >
              <ChevronRight style={{ width: 10, height: 10, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
            </button>
          )}
          <button
            onClick={() => onJumpToPrompt(path)}
            title={`Open ${path} in the file view`}
            style={{
              display: "flex", alignItems: "center", gap: 6, fontSize: 10.5, flex: 1, minWidth: 0,
              padding: active ? "5px 8px 5px 4px" : "5px 8px", border: "none", background: "none", textAlign: "left",
              cursor: "pointer",
            }}
          >
            {!active && <FileText style={{ width: 10, height: 10, color: "var(--text-dim)", flexShrink: 0 }} />}
            <span style={{ fontFamily: "monospace", color: "var(--text-secondary)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{key}</span>
            <span style={{
              marginLeft: "auto", fontSize: 9, padding: "1px 6px", borderRadius: 8, flexShrink: 0,
              background: active ? "var(--amber-tint)" : "var(--card)",
              color: active ? "var(--amber)" : "var(--text-ghost)",
              border: active ? "1px solid var(--amber-border)" : "1px solid var(--border-subtle)",
            }}>
              {active ? "override" : "default"}
            </span>
          </button>
        </div>
        {open && <ConfigEditor path={path} fieldsHint={fieldsHint} />}
      </div>
    </div>
  );
}

// Same fetch/edit/save shape as PromptEditor, against the "contexts"
// branch instead of "prompts" — a Context DSL slot's compiled-in default
// (Storyteller.Core.Context.buildContextLibrary) works exactly like a
// PromptStorage default: no override file present means "use the literal
// in Haskell source," so there's nothing to fetch until one exists. Also
// handles *creating* that first override (prompts/config rely on jumping
// to the file view to create a file there; a Context DSL slot has no such
// file-view surface, so this owns create inline via a starter textarea).
function ContextSlotEditor({ path }: { path: string }) {
  const [content, setContent] = useState<string | null>(null);
  const [draft, setDraft] = useState("");
  const [saving, setSaving] = useState(false);
  const [loadError, setLoadError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setContent(null);
    setLoadError(null);
    fetch(contextFunctionUrl(path))
      .then((res) => {
        if (res.status === 404) return null; // no override yet — start blank
        if (!res.ok) throw new Error(`${res.status} ${res.statusText}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text ?? "");
        setDraft(text ?? "");
      })
      .catch((err) => { if (!cancelled) setLoadError(String(err)); });
    return () => { cancelled = true; };
  }, [path]);

  async function save() {
    setSaving(true);
    try {
      await uploadBranchFile(contextsBranchName, `context/${path}.dsl`, new Blob([draft]));
      setContent(draft);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  if (loadError) {
    return <div style={{ padding: "6px 8px", fontSize: 10.5, color: "var(--rose)" }}>failed to load: {loadError}</div>;
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
        placeholder={'A 0-arity Context DSL program, e.g.:\n\nread "lore/**"'}
        rows={6}
        spellCheck={false}
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
            background: dirty ? "var(--amber-tint)" : "var(--surface)",
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

// The project-wide default for "context.lore", edited with the identical
// checkbox-list + generated-code surface context-panel.tsx's LoreRow uses
// for a per-call override -- same component family, different persistence
// target (this writes context/lore.dsl on the "contexts" branch directly,
// not callContextStore). Reusing the exact same generation
// (renderLoreProgram) keeps "what a checkbox produces" identical whether
// a user is setting the project default here or a one-off override there.
function LoreSlotEditor({ branch }: { branch: string | null }) {
  const [content, setContent] = useState<string | null>(null);
  const [draft, setDraft] = useState("");
  const [excluded, setExcluded] = useState<string[]>([]);
  const [saving, setSaving] = useState(false);
  const [loadError, setLoadError] = useState<string | null>(null);
  const loreTree = useLoreTree(branch);
  const allEntries = flattenLore(loreTree);
  const allPaths = allEntries.map((e) => e.path);

  useEffect(() => {
    let cancelled = false;
    setContent(null);
    setLoadError(null);
    fetch(contextFunctionUrl("lore"))
      .then((res) => {
        if (res.status === 404) return null; // no override yet
        if (!res.ok) throw new Error(`${res.status} ${res.statusText}`);
        return res.text();
      })
      .then((text) => {
        if (cancelled) return;
        setContent(text ?? "");
        setDraft(text ?? "");
      })
      .catch((err) => { if (!cancelled) setLoadError(String(err)); });
    return () => { cancelled = true; };
  }, []);

  async function save(nextDraft: string) {
    setSaving(true);
    try {
      await uploadBranchFile(contextsBranchName, "context/lore.dsl", new Blob([nextDraft]));
      setContent(nextDraft);
      setDraft(nextDraft);
    } catch (err) {
      setError(String(err));
    } finally {
      setSaving(false);
    }
  }

  function toggleEntry(entryPath: string) {
    const next = excluded.includes(entryPath) ? excluded.filter((p) => p !== entryPath) : [...excluded, entryPath];
    setExcluded(next);
    const included = allPaths.filter((p) => !next.includes(p));
    save(renderLoreProgram(included));
  }

  if (loadError) {
    return <div style={{ padding: "6px 8px", fontSize: 10.5, color: "var(--rose)" }}>failed to load: {loadError}</div>;
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
    <div style={{ padding: "6px 8px 8px", display: "flex", flexDirection: "column", gap: 8 }}>
      {!branch ? (
        <div style={{ fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>No active branch to browse lore entries from.</div>
      ) : allEntries.length === 0 ? (
        <div style={{ fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>
          {loreTree.length === 0 ? "Loading lore entries…" : "No lore entries on this branch yet."}
        </div>
      ) : (
        <div>
          <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: "0 2px 4px" }}>
            {allPaths.length - excluded.length} of {allPaths.length} included by default
          </div>
          <div style={{
            display: "flex", flexDirection: "column", gap: 1, maxHeight: 130, overflowY: "auto",
            border: "1px solid var(--border-subtle)", borderRadius: 5, padding: 3,
          }}>
            {allEntries.map((entry) => {
              const included = !excluded.includes(entry.path);
              return (
                <label
                  key={entry.path}
                  title={entry.path}
                  style={{ display: "flex", alignItems: "center", gap: 6, padding: "3px 5px", borderRadius: 3, cursor: "pointer" }}
                >
                  <input
                    type="checkbox"
                    checked={included}
                    onChange={() => toggleEntry(entry.path)}
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
        <div style={{ fontSize: 10, color: "var(--text-ghost)", padding: "0 2px 4px" }}>
          Generated from the checkboxes above — edit directly for full control.
        </div>
        <textarea
          value={draft}
          onChange={(e) => setDraft(e.target.value)}
          placeholder={'A 0-arity Context DSL program, e.g.:\n\nread "lore/**"'}
          rows={6}
          spellCheck={false}
          style={{
            width: "100%", resize: "vertical", fontFamily: "monospace", fontSize: 11, lineHeight: 1.5,
            padding: 8, borderRadius: 5, border: "1px solid var(--border-subtle)",
            background: "var(--card)", color: "var(--foreground)",
          }}
        />
      </div>
      <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
        <button
          onClick={() => save(draft)}
          disabled={!dirty || saving}
          style={{
            fontSize: 10.5, padding: "3px 10px", borderRadius: 4, border: "1px solid var(--border-subtle)",
            background: dirty ? "var(--amber-tint)" : "var(--surface)",
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

// Context defaults section — one row per lib/agents.ts's contextSlots
// entry, same expand/edit-in-place shape as PromptOverrides but against
// the "contexts" branch. Unlike prompts, a slot with no override yet is
// still clickable (opens ContextSlotEditor blank) rather than needing a
// file-view jump first, since there's no file-view surface for these.
function ContextSlotOverrides({ contextSlots, files, branch }: {
  contextSlots: string[];
  files: string[] | null;
  branch: string | null;
}) {
  const [expanded, setExpanded] = useState<Set<string>>(new Set());

  if (contextSlots.length === 0) {
    return <div style={{ padding: "0 16px 12px", fontSize: 10.5, color: "var(--text-ghost)", fontStyle: "italic" }}>No overridable context slots for this agent.</div>;
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
      {contextSlots.map((key) => {
        const bareName = key.startsWith("context.") ? key.slice("context.".length) : key;
        const path = `context/${bareName}.dsl`;
        const active = files?.includes(path) ?? false;
        const open = expanded.has(key);
        return (
          <div key={key} style={{ borderRadius: 5, background: "var(--surface)", overflow: "hidden" }}>
            <div style={{ display: "flex", alignItems: "center" }}>
              <button
                onClick={() => toggle(key)}
                title={open ? "Collapse" : "Expand to view/edit"}
                style={{
                  display: "flex", alignItems: "center", flexShrink: 0,
                  padding: "5px 0 5px 8px", border: "none", background: "none", cursor: "pointer", color: "var(--text-dim)",
                }}
              >
                <ChevronRight style={{ width: 10, height: 10, transform: open ? "rotate(90deg)" : "none", transition: "transform 0.15s" }} />
              </button>
              <div style={{
                display: "flex", alignItems: "center", gap: 6, fontSize: 10.5, flex: 1, minWidth: 0,
                padding: "5px 8px 5px 4px",
              }}>
                <span style={{ fontFamily: "monospace", color: "var(--text-secondary)", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{key}</span>
                <span style={{
                  marginLeft: "auto", fontSize: 9, padding: "1px 6px", borderRadius: 8, flexShrink: 0,
                  background: active ? "var(--amber-tint)" : "var(--card)",
                  color: active ? "var(--amber)" : "var(--text-ghost)",
                  border: active ? "1px solid var(--amber-border)" : "1px solid var(--border-subtle)",
                }}>
                  {active ? "override" : "default"}
                </span>
              </div>
            </div>
            {open && (
              bareName === "lore"
                ? <LoreSlotEditor branch={branch} />
                : <ContextSlotEditor path={bareName} />
            )}
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

export function AgentsTab({ path, branch, onJumpToPrompt }: {
  path: string;
  branch: string | null;
  onJumpToPrompt: (path: string) => void;
}) {
  const applicable = AGENTS.filter((a) => a.appliesTo(path));
  const [selectedId, setSelectedId] = useState<string | null>(applicable[0]?.id ?? null);
  const promptFiles = usePromptFiles();
  const contextFiles = useContextFiles();

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
                    background: active ? "var(--amber-wash)" : "transparent",
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

        <SectionLabel icon={FileText}>Prompts</SectionLabel>
        <PromptOverrides promptKeys={selected.promptKeys} files={promptFiles} onJumpToPrompt={onJumpToPrompt} />

        <SectionLabel icon={Sliders}>Sampling</SectionLabel>
        <ConfigOverride agent={selected} files={promptFiles} onJumpToPrompt={onJumpToPrompt} />

        {selected.contextSlots && selected.contextSlots.length > 0 && (
          <>
            <SectionLabel icon={BookOpen}>Context defaults</SectionLabel>
            <ContextSlotOverrides contextSlots={selected.contextSlots} files={contextFiles} branch={branch} />
          </>
        )}
      </div>
    </div>
  );
}
