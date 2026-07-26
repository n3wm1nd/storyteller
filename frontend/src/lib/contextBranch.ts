"use client";

// Saved context-function files on the `contexts` branch -- the
// power-user / "Save as..." side of the new context UI. The casual
// panel's transient edits never touch this branch; only an explicit
// save (the panel's "Save as..." button or the DSL editor's "Save")
// writes a `.dsl` file here.
//
// All file I/O goes through the existing HTTP file API
// (`branchFileUrl`/`uploadBranchFile` in lib/ws.ts) -- no new endpoints.
// The contexts branch is just another branch from this layer's
// perspective; `contextsBranchName` matches the backend's
// `Storyteller.Core.Context.contextsBranchName` ("contexts", see that
// module's own haddock for the convention).
//
// Listing: the same `/branch/contexts` WebSocket connection
// `branchConn` already uses for any branch (see lib/ws.ts:branchConn).
// `branch.ready` carries a `files: string[]` we filter to `context/*.dsl`.
// Kept as an opt-in hook (the library component calls it on mount) so
// the cost is paid only when the user opens the library browser.

import { useEffect, useState } from "react";
import { branchFileUrl, contextDefaultUrl, saveRawFileWhole, branchConn, contextViewConn } from "./ws";
import type { BranchEvent } from "./ws";
import { useCallContext } from "./callContextStore";
import { DEFAULT_EDITS, synthesizeLoreOverride, synthesizeOtherOverride } from "./dslCompose";
import { setConnStatus, removeConn, bumpActivity } from "./uiStore";

export const contextsBranchName = "contexts";
const DSL_DIR = "context"; // matches Core.Context's dotted-name → path rule

function dslPath(name: string): string {
  return `${DSL_DIR}/${name}.dsl`;
}

// ─── Single-file read/write ───────────────────────────────────────────────

export function contextFunctionUrl(name: string): string {
  return branchFileUrl(contextsBranchName, dslPath(name));
}

export async function readContextFunction(name: string): Promise<string> {
  // A straight GET via the file endpoint. A 404 means "no such saved
  // function" -- surface as a thrown Error so the caller (the library
  // browser or the load-by-name flow) can present it cleanly.
  const res = await fetch(contextFunctionUrl(name));
  if (!res.ok) {
    if (res.status === 404) throw new Error(`No saved context function named "${name}"`);
    throw new Error(`read failed: ${res.status} ${name}`);
  }
  return res.text();
}

// A compiled-in context slot's own default source (e.g. `context.lore`),
// the real thing -- pretty-printed server-side from the actual parsed
// Definition (see app/Server.hs's `GET /context-default/{name}` and
// Storyteller.Context.DSL.PrettyPrint), not a hand-kept JS copy. `name`
// is the dotted slot name, distinct from `readContextFunction`'s bare
// saved-snippet name. A 404 means the name has no compiled-in default at
// all (a hostLibrary-only or project-only name).
export async function readContextDefault(name: string): Promise<string> {
  const res = await fetch(contextDefaultUrl(name));
  if (!res.ok) {
    if (res.status === 404) throw new Error(`No compiled-in default for "${name}"`);
    throw new Error(`read failed: ${res.status} ${name}`);
  }
  return res.text();
}

// The lore slot's exact draft text for one open file/branch -- the same
// derivation `context-panel.tsx`'s `LoreRow` uses to seed and display its
// own editor, extracted here so a second consumer (context-cost-sidebar.tsx)
// can cost the *actual* text a send would use instead of guessing at a
// parallel approximation. There is exactly one correct answer to "what DSL
// governs this call's lore" and this is it -- not a bare `context.lore`
// name reference (which resolves the *branch's* default, blind to any
// per-call override the user has made) and not a hand-synthesized
// reconstruction of the checkbox state (which could silently drift from
// what LoreRow itself renders/edits).
//
// Ground truth precedence, matching `Server.Writer.File.chatWriter`'s own
// `mLore`/`setContextOverride` resolution exactly:
//   1. `edits.loreOverride` -- this call's own touched override, if any.
//   2. This project's committed `context/lore.dsl` (`readContextFunction`),
//      if one exists.
//   3. The compiled-in default's real source (`readContextDefault`).
// Nothing here is a guess: 2 and 3 are literally what the server would
// resolve `context.lore` to for an untouched call, fetched the same way
// LoreRow already does.
export function useLoreDraft(path: string | null, branch: string | null): { draft: string; resetTarget: string; hasOverride: boolean; sendText: string | null } {
  const edits = useCallContext((s) => (path ? s.files[path] : undefined)) ?? DEFAULT_EDITS;
  const [committedDefault, setCommittedDefault] = useState<string | null | undefined>(undefined);
  const [compiledDefault, setCompiledDefault] = useState("");

  useEffect(() => {
    if (!branch) return;
    let cancelled = false;
    setCommittedDefault(undefined);
    setCompiledDefault("");
    readContextFunction("lore")
      .then((text) => { if (!cancelled) setCommittedDefault(text); })
      .catch(() => { if (!cancelled) setCommittedDefault(null); });
    readContextDefault("context.lore")
      .then((text) => { if (!cancelled) setCompiledDefault(text); })
      .catch(() => {}); // no compiled-in default -- fine, nothing to seed from
    return () => { cancelled = true; };
  }, [branch]);

  // Ground truth for "no override touched yet" -- the committed project
  // file, or (only if nothing was ever committed) the real compiled-in
  // default source -- never a bare `context.lore` name reference, which
  // resolves correctly server-side at send time but can't be shown/edited
  // here as if it were real content of its own. Exposed separately from
  // `draft` (always the *default*, regardless of whether an override is
  // currently active) since callers that toggle/reset need to seed from
  // or compare against it independent of the override's own current state.
  const resetTarget = committedDefault ?? compiledDefault;
  return {
    draft: edits.loreOverride ?? resetTarget,
    resetTarget,
    hasOverride: edits.loreOverride !== null,
    // What a real send would put on the wire as `lore` -- `null` means
    // "omit the field entirely" (nothing touched, server resolves its own
    // context.lore). Deliberately reuses `synthesizeLoreOverride`
    // (dslCompose.ts) rather than re-deriving the loreEnabled/loreOverride
    // precedence here a second time: that function is the literal code
    // path `fileview.actions.ts` calls to build the wire field, so a
    // consumer asking "what will actually be sent" gets the same answer a
    // real chat.writer command would carry, including the disabled case
    // (loreEnabled === false sends `'"" \n'` -- an explicitly EMPTY
    // program -- never the resolved default; sending the default while a
    // user unchecked "Story lore" would silently ignore that toggle).
    sendText: synthesizeLoreOverride(edits),
  };
}

// 'useLoreDraft''s own twin for `context.other` -- identical ground-truth
// precedence (this call's own touched override, then a committed
// `context/other.dsl`, then the compiled-in default's real source), just
// against `otherOverride`/`synthesizeOtherOverride`.
export function useOtherDraft(path: string | null, branch: string | null): { draft: string; resetTarget: string; hasOverride: boolean; sendText: string | null } {
  const edits = useCallContext((s) => (path ? s.files[path] : undefined)) ?? DEFAULT_EDITS;
  const [committedDefault, setCommittedDefault] = useState<string | null | undefined>(undefined);
  const [compiledDefault, setCompiledDefault] = useState("");

  useEffect(() => {
    if (!branch) return;
    let cancelled = false;
    setCommittedDefault(undefined);
    setCompiledDefault("");
    readContextFunction("other")
      .then((text) => { if (!cancelled) setCommittedDefault(text); })
      .catch(() => { if (!cancelled) setCommittedDefault(null); });
    readContextDefault("context.other")
      .then((text) => { if (!cancelled) setCompiledDefault(text); })
      .catch(() => {}); // no compiled-in default -- fine, nothing to seed from
    return () => { cancelled = true; };
  }, [branch]);

  const resetTarget = committedDefault ?? compiledDefault;
  return {
    draft: edits.otherOverride ?? resetTarget,
    resetTarget,
    hasOverride: edits.otherOverride !== null,
    sendText: synthesizeOtherOverride(edits),
  };
}

// The flat file-path list a named context slot (`"context.lore"`,
// `"context.other"`) currently resolves to for `path`, live -- what
// LoreRow/OtherRow's own checkbox lists read, via the `context.entries`
// command on the existing $context/{path} connection (see
// Storyteller.Writer.Agent.ContextPreview.buildEntries0/buildEntries1 and
// Server.Writer.ContextView.Connection's own `pushEntries`). Server-
// authoritative and live-updating (re-pushed on every branch change, same
// as `context.preview`) rather than a client-side glob guess — see
// WS-PROTOCOL.md's "Backend-authoritative vs. frontend-advisory
// duplication": which files a slot resolves to is something an agent
// (writeAgent itself) acts on, so it must come from the server, not be
// re-derived here.
//
// `entriesPath` is `context.other`'s own framing argument (the file about
// to be written) -- omit it for a 0-arity slot like `context.lore`.
export function useContextEntries(branch: string | null, name: string, entriesPath?: string | null): string[] {
  const [entries, setEntries] = useState<string[]>([]);

  useEffect(() => {
    if (!branch) { setEntries([]); return; }
    const connLabel = `context.entries:${branch}:${name}:${entriesPath ?? ""}`;
    setConnStatus(connLabel, "connecting");
    setEntries([]);

    const conn = contextViewConn(branch, entriesPath ?? "");
    conn.subscribe((evt) => {
      bumpActivity(connLabel);
      if (evt.type === "context.entries") {
        setEntries(evt.entries);
        setConnStatus(connLabel, "connected");
      } else if (evt.type === "error") {
        setConnStatus(connLabel, "error");
      }
    });
    (async () => {
      try {
        await conn.connect();
        conn.send({ type: "context.entries", name, path: entriesPath ?? undefined });
      } catch {
        setConnStatus(connLabel, "error");
      }
    })();
    return () => {
      conn.close();
      removeConn(connLabel);
    };
  }, [branch, name, entriesPath]);

  return entries;
}

export async function writeContextFunction(name: string, source: string): Promise<void> {
  // $raw?whole keeps the file atom-tracked -- so it never shows up in the
  // library tree flagged binary the way uploadBranchFile's opaque PUT made
  // it (see Server.Writer.Library's lfcTracked / withBinaryFlags: "binary"
  // there means "never had an atom tick", not a content/mime check) --
  // while keeping it at exactly *one* atom holding the whole program.
  // Plain $raw (saveRawFile) would reconcile it paragraph by paragraph
  // like prose, accumulating an atom per added stanza; an atom boundary
  // inside a DSL program says nothing true about it. Same write the .dsl
  // file editor uses (app/dsl-file-view.tsx), so a snippet saved from
  // either surface has the same shape.
  await saveRawFileWhole(contextsBranchName, dslPath(name), source);
}

export async function deleteContextFunction(name: string): Promise<void> {
  // No dedicated DELETE endpoint -- the server's branch-file API uses
  // PUT for everything. For Phase 1 we don't expose deletion in the UI
  // (a saved function is cheap to leave around); a future
  // `chat.delete-file` or similar wire command would slot in here.
  throw new Error("deleteContextFunction: not yet wired (no DELETE in the file API)");
}

// ─── Listing ──────────────────────────────────────────────────────────────

export interface SavedContextFunction {
  // The bare function name -- `context/alice-battle.dsl` becomes
  // `"alice-battle"`. What the wire's `context` field would carry to
  // call this function.
  name: string;
  // The full path on the contexts branch (for diagnostics / future
  // raw-edit affordances).
  path: string;
}

// List every `context/*.dsl` on the contexts branch. Opens a one-shot
// `branchConn` (auto-closes after the first `branch.ready`), filters
// the file list, and returns. Caller is responsible for keeping the
// result fresh enough for its UI -- a refresh button is enough at the
// scales this branch will see.
export async function listContextFunctions(): Promise<SavedContextFunction[]> {
  return new Promise((resolve, reject) => {
    const conn = branchConn(contextsBranchName);
    let settled = false;
    const cleanup = () => {
      if (settled) return;
      settled = true;
      try { conn.close(); } catch { /* already gone */ }
    };
    const timer = setTimeout(() => {
      if (settled) return;
      cleanup();
      reject(new Error("context list timed out"));
    }, 5000);

    conn.subscribe((evt: BranchEvent) => {
      if (evt.type !== "branch.ready") return;
      clearTimeout(timer);
      const fns = evt.files
        .filter((p) => p.startsWith(`${DSL_DIR}/`) && p.endsWith(".dsl"))
        .map((p) => ({
          path: p,
          name: p.slice(DSL_DIR.length + 1, -".dsl".length),
        }))
        .sort((a, b) => a.name.localeCompare(b.name));
      cleanup();
      resolve(fns);
    });

    conn.connect().catch((err) => {
      clearTimeout(timer);
      cleanup();
      reject(err);
    });
  });
}

// ─── Name validation ──────────────────────────────────────────────────────

// A saved function's name is also its filename and its DSL identifier
// (when sent as a bare-name program). Keep it conservative: letters,
// digits, `-`, `_`, `.`; must start with a letter or `_`. Same shape
// the DSL's identifier parser already accepts, so a saved name is
// always syntactically callable.
export function isValidFunctionName(name: string): boolean {
  return /^[A-Za-z_][A-Za-z0-9_.-]*$/.test(name) && !name.includes("/");
}

// A friendly display name from a raw one -- the user might type
// "Alice Battle Scene" and we save it as "alice-battle-scene". Same
// slug logic other parts of the app use (see lib/utils for character
// branch slugging).
export function slugifyFunctionName(raw: string): string {
  return raw
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9._-]+/g, "-")
    .replace(/^-+|-+$/g, "");
}
