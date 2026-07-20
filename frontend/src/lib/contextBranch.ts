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

import { branchFileUrl, uploadBranchFile, branchConn } from "./ws";
import type { BranchEvent } from "./ws";

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

export async function writeContextFunction(name: string, source: string): Promise<void> {
  // PUT bytes -- matches how sidebar.actions's `uploadBranchFile` is
  // used elsewhere. The server reconciles file writes the same way it
  // would for any branch file.
  await uploadBranchFile(contextsBranchName, dslPath(name), new Blob([source]));
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
