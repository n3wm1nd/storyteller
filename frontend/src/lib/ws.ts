// WebSocket connection abstractions for the storyteller server.
//
// Four connection types mirror the server's four endpoints:
//   sessionConn    (/session)              — branch management
//   branchConn     (/branch/{name})        — full branch tick chain + file tree
//   fileConn       (/branch/{name}/{path}) — file-scoped tick chain
//   characterConn  (/character/{name})     — sidebar-facing character state (read-only)
//
// All connections support auto-reconnect. Reconnecting is the only resync
// mechanism — the server pushes full state on every new connection.

function wsBase() {
  if (process.env.NEXT_PUBLIC_WS_URL) return process.env.NEXT_PUBLIC_WS_URL;
  const proto = window.location.protocol === "https:" ? "wss:" : "ws:";
  return `${proto}//${window.location.hostname}:8090`;
}

// Same server, plain HTTP — for the GET/PUT /branch/{name}/{path} endpoints
// (file download/embed and upload), which don't go over the WS connections
// below at all. Derived from 'wsBase()' rather than duplicating the
// NEXT_PUBLIC_WS_URL/hostname:8090 fallback logic.
function httpBase() {
  return wsBase().replace(/^wss:/, "https:").replace(/^ws:/, "http:");
}

function encodePath(path: string) {
  return path.split("/").map((p) => encodeURIComponent(decodeURIComponent(p))).join("/");
}

// Current raw content of a branch file — for downloading or embedding
// (e.g. <img src>) directly, without tunneling bytes through the WS
// connection just to simulate it.
export function branchFileUrl(branch: string, path: string) {
  return `${httpBase()}/branch/${encodeURIComponent(branch)}/${encodePath(path)}`;
}

// Upload/replace a branch file's content directly from its bytes — the PUT
// counterpart to 'branchFileUrl'. Replaces the old WS 'upload' command: a
// dropped file's bytes go straight over HTTP instead of being read as text,
// JSON-encoded, and tunneled through the branch connection.
export async function uploadBranchFile(branch: string, path: string, content: Blob) {
  const res = await fetch(branchFileUrl(branch, path), { method: "PUT", body: content });
  if (!res.ok) throw new Error(`upload failed: ${res.status} ${path}`);
}

// ── Shared event types ────────────────────────────────────────────────────────

export type ErrorEvent    = { type: "error";     message: string };
export type AgentLogEvent = { type: "agent.log"; level: "info" | "warning" | "error"; message: string };

// Ephemeral, best-effort streamed draft of an in-flight chat.prompt/chargen
// call. Not correlated by id — a connection only ever has one command in
// flight at a time. Must be discarded the instant the real Update/error for
// that command arrives, and cleared on "chat.preview.end" regardless (a
// call can finish with nothing persisted at all). See WS-PROTOCOL.md.
export type ChatPreviewEvent =
  | { type: "chat.preview.start" }
  | { type: "chat.preview";          text: string }
  | { type: "chat.preview.thinking"; text: string }
  | { type: "chat.preview.end" };

// ── Shared tick + update types ────────────────────────────────────────────────

// A tick as sent over the wire. Flat representation — the client interprets
// kind/fields/content to decide how to render it.
export interface WireTick {
  tickId:   string;
  kind:     string;
  refs:     string[];
  fields?:  Record<string, string>;
  message:  string;
  content?: string | null;
  parent:   string | null;
}

// Server push: upsert these ticks into the client store, set head to `head`.
export interface Update {
  type:  "update";
  ticks: WireTick[];
  head:  string;
}

// ── Session protocol ──────────────────────────────────────────────────────────

export type SessionCommand =
  | { type: "create-branch"; id?: string; branch: string }
  | { type: "delete-branch"; id?: string; branch: string };

// One character branch's raw summary — sheet.md content, unprocessed (see
// WS-PROTOCOL.md's "read is raw-but-complete" rule). The client is
// responsible for decoding this into a display name (first Markdown H1
// line, falling back to the branch id) the same way it decodes any other
// raw content into a concept it needs.
export interface CharacterSummary {
  branch: string;
  sheet: string | null;
}

// branch.list and character.list are always unprompted — pushed once right
// after session.ready, and again whenever the underlying set changes (see
// Server.Writer.Session.Connection's notifier). There is no request for
// either: a session only ever listens.
export type SessionEvent =
  | { type: "session.ready" }
  | { type: "branch.list";     branches: string[] }
  | { type: "branch.created";  id?: string; branch: string }
  | { type: "branch.deleted";  id?: string; branch: string }
  | { type: "character.list";  characters: CharacterSummary[] }
  | ErrorEvent;

// ── Branch protocol ───────────────────────────────────────────────────────────

export type BranchCommand =
  | { type: "track";       id?: string; source: string; files: { from: string; to: string }[] }
  | { type: "chargen";     id?: string; path: string; scenario: string; seed?: number }
  | { type: "add.note";    id?: string; refTickId: string; text: string }
  | { type: "move.tick";   id?: string; tickId: string; afterTickId?: string }
  | { type: "delete.tick"; id?: string; tickId: string }
  // Rebase, same shape as FileCommand's — generic capability, no client
  // trigger uses this yet (would be a future Ticks-view rebase marker).
  | { type: "at";          id?: string; tickId: string; command: BranchCommand };

export type BranchEvent =
  | { type: "branch.ready"; id?: string; branch: string; files: string[] }
  | { type: "file.added";   id?: string; path: string }
  | Update
  | AgentLogEvent
  | ChatPreviewEvent
  | ErrorEvent;

// ── File protocol ─────────────────────────────────────────────────────────────

// A pinned atom/annotation attached to a chat.writer/chat.fixer command as
// reference context. `content` is what the agent reads; `tickId`/`kind` are
// for traceability only. `branch` is set only when the item comes from a
// branch other than the one this command is being sent on (e.g. a character
// journal selection pinned to a story-file command) — the connection's own
// branch is always implied and never needs restating. See SELECTION.md.
export interface ContextItem {
  tickId:  string;
  kind:    string;
  content: string;
  branch?: string;
}

export type FileCommand =
  // Create: introduce this path into the tree as its own tick, empty —
  // distinct from chat.append, which both creates (implicitly, on a
  // not-yet-tracked path) and appends content in one step. Fails on an
  // already-present path rather than truncating it.
  | { type: "file.create"; id?: string }
  | { type: "chat.append"; id?: string; content: string }
  | { type: "delete";      id?: string }
  | { type: "edit.atom";   id?: string; tickId: string; content: string }
  | { type: "delete.atom"; id?: string; tickId: string }
  | { type: "move.atom";   id?: string; tickId: string; afterTickId?: string }
  // Merge: combine a contiguous run of one file's atoms (`targets`) into one.
  | { type: "merge.atoms"; id?: string; targets: string[] }
  // Split: re-run the splitter over each of `targets`' own content, in place.
  | { type: "split.atoms"; id?: string; targets: string[] }
  // Writer, or FlowWriter (implicitly) when `flowTid` is set — the tick
  // that was HEAD when the user started typing, so the agent can judge
  // whether atoms generated since then are still provisional.
  | { type: "chat.writer"; id?: string; text: string; context?: ContextItem[]; flowTid?: string }
  // Fixer: `targets` are the atoms flagged as the subject of `text`.
  | { type: "chat.fixer";  id?: string; text: string; context?: ContextItem[]; targets?: string[] }
  // Regen: rewrite this chapter to fit its beat sheet (ch{N}.outline.md by
  // convention), respecting `text` as the user's steer. A reconciliation, not
  // a wipe — unchanged prose keeps its atoms. `byBeat` selects the
  // beat-by-beat driver over the whole-chapter one.
  | { type: "chat.regen";  id?: string; text: string; context?: ContextItem[]; byBeat?: boolean }
  // Outline: split this file (a whole-story outline, outline.md by convention)
  // into per-chapter beat sheets. No prompt — the outline text is the whole
  // input; the model decides the chapter breakdown and writes each sheet.
  | { type: "chat.outline"; id?: string }
  // Note: instant, non-LLM, like chat.append — attaches `text` as an
  // annotation on each of `targets`, or (when empty) on the file's current
  // HEAD tick.
  | { type: "chat.note";   id?: string; text: string; targets?: string[] }
  // Presence: a character (character/{id} branch) enters or leaves the
  // scene on this file — recorded as a "presence" tick scoped to this
  // file's own chain, not the whole branch (a scene is a file — see
  // WRITER.md). Wrapping in `at` (below) rebases it at a historical tick,
  // same as any other file command — no separate mechanism needed.
  | { type: "enter.scene"; id?: string; character: string }
  | { type: "leave.scene"; id?: string; character: string }
  // Rebase: run `command` as if `tickId` were HEAD, then replay everything
  // that came after it on top of the result. Lets the client re-target any
  // command at a historical point in the file's chain. `branches` carries
  // the corresponding "as of" position (a tick id) in every other branch
  // relevant to this file — currently the journal of each character present
  // in the scene at `tickId` — so a command run at a historical point in the
  // file doesn't silently see those characters' journals still at their
  // live HEAD. See SELECTION.md. Optional and currently unconsumed
  // server-side; sent ahead of the backend reading it.
  | { type: "at";          id?: string; tickId: string; command: FileCommand; branches?: { branch: string; tickId: string }[] };

export type FileEvent =
  | { type: "file.present"; id?: string }
  | { type: "file.absent";  id?: string }
  | Update
  // A rebase/replace/move rewrote tick ids; [from, to] pairs. Apply to any
  // tickId held locally (rebase marker, context selection) — a no-op for ids
  // this client doesn't track.
  | { type: "tick.remap"; mapping: [string, string][] }
  | AgentLogEvent
  | ChatPreviewEvent
  | ErrorEvent;

// ── Character protocol ────────────────────────────────────────────────────────

// Read-only: no commands. Every field is collected-and-augmented server-side
// (see Server/Writer/Character.hs) — sheet edits go through the file
// connection for sheet.md, never through this one.
export type CharacterEvent =
  | { type: "character.update"; name: string; sheet?: string }
  | ErrorEvent;

// ── Connection ────────────────────────────────────────────────────────────────

type Listener<E> = (event: E) => void;
export type WsStatus = "connecting" | "connected" | "disconnected";

export class StoryWS<Cmd, Evt> {
  private ws: WebSocket | null = null;
  private listeners: Set<Listener<Evt>> = new Set();
  private statusListeners: Set<Listener<WsStatus>> = new Set();
  private queue: Cmd[] = [];
  private stopped = false;
  private onConnected: () => void;

  constructor(private url: string, onConnected: () => void = () => {}) {
    this.onConnected = onConnected;
  }

  connect(): Promise<void> {
    this.stopped = false;
    return this._connect();
  }

  private _connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      this._emit("connecting");
      const ws = new WebSocket(this.url);
      this.ws = ws;

      ws.onopen = () => {
        this._emit("connected");
        for (const cmd of this.queue) this._send(cmd);
        this.queue = [];
        this.onConnected();
        resolve();
      };

      ws.onerror = () => {
        reject(new Error(`WebSocket error: ${this.url}`));
      };

      ws.onmessage = (e) => {
        try {
          const evt = JSON.parse(e.data) as Evt;
          for (const fn of this.listeners) fn(evt);
        } catch {
          // ignore malformed messages
        }
      };

      ws.onclose = (e) => {
        console.log(`[ws] closed ${this.url} code=${e.code} reason=${e.reason} wasClean=${e.wasClean}`);
        this.ws = null;
        if (!this.stopped) {
          this._emit("disconnected");
          this._scheduleReconnect();
        }
      };
    });
  }

  private _scheduleReconnect() {
    setTimeout(() => {
      if (!this.stopped) this._connect().catch(() => {});
    }, 500);
  }

  send(cmd: Cmd) {
    if (this.ws?.readyState === WebSocket.OPEN) this._send(cmd);
    else this.queue.push(cmd);
  }

  subscribe(fn: Listener<Evt>): () => void {
    this.listeners.add(fn);
    return () => this.listeners.delete(fn);
  }

  onStatus(fn: Listener<WsStatus>): () => void {
    this.statusListeners.add(fn);
    return () => this.statusListeners.delete(fn);
  }

  close() {
    this.stopped = true;
    this.ws?.close();
    this.ws = null;
  }

  private _send(cmd: Cmd) {
    this.ws!.send(JSON.stringify(cmd));
  }

  private _emit(s: WsStatus) {
    for (const fn of this.statusListeners) fn(s);
  }
}

// ── Exported constructors ─────────────────────────────────────────────────────

export function sessionConn() {
  return new StoryWS<SessionCommand, SessionEvent>(`${wsBase()}/session`);
}

export function branchConn(name: string) {
  return new StoryWS<BranchCommand, BranchEvent>(`${wsBase()}/branch/${encodeURIComponent(name)}`);
}

export function fileConn(branch: string, path: string) {
  const encodedPath = path.split("/").map((p) => encodeURIComponent(decodeURIComponent(p))).join("/");
  return new StoryWS<FileCommand, FileEvent>(`${wsBase()}/branch/${encodeURIComponent(branch)}/${encodedPath}`);
}

// No commands, so 'Cmd' is 'never' — nothing can be sent on this connection.
export function characterConn(branch: string) {
  return new StoryWS<never, CharacterEvent>(`${wsBase()}/character/${encodeURIComponent(branch)}`);
}
