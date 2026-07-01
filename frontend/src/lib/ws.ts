// WebSocket connection abstractions for the storyteller server.
//
// Three connection types mirror the server's three endpoints:
//   sessionConn  (/session)              — branch management
//   branchConn   (/branch/{name})        — full branch tick chain + file tree
//   fileConn     (/branch/{name}/{path}) — file-scoped tick chain
//
// All connections support auto-reconnect. Reconnecting is the only resync
// mechanism — the server pushes full state on every new connection.

const WS_BASE = process.env.NEXT_PUBLIC_WS_URL ?? "ws://localhost:8090";

// ── Shared event types ────────────────────────────────────────────────────────

export type ErrorEvent    = { type: "error";     message: string };
export type AgentLogEvent = { type: "agent.log"; level: "info" | "warning" | "error"; message: string };

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
  | { type: "list-branches"; id?: string }
  | { type: "create-branch"; id?: string; branch: string }
  | { type: "delete-branch"; id?: string; branch: string };

export type SessionEvent =
  | { type: "session.ready" }
  | { type: "branch.list";    id?: string; branches: string[] }
  | { type: "branch.created"; id?: string; branch: string }
  | { type: "branch.deleted"; id?: string; branch: string }
  | ErrorEvent;

// ── Branch protocol ───────────────────────────────────────────────────────────

export type BranchCommand =
  | { type: "track";       id?: string; source: string; files: { from: string; to: string }[] }
  | { type: "chargen";     id?: string; path: string; scenario: string; seed?: number }
  | { type: "add.note";    id?: string; refTickId: string; text: string }
  | { type: "move.tick";   id?: string; tickId: string; afterTickId?: string }
  | { type: "delete.tick"; id?: string; tickId: string }
  | { type: "chat.prompt"; id?: string; path: string; text: string };

export type BranchEvent =
  | { type: "branch.ready"; id?: string; branch: string; files: string[] }
  | { type: "file.added";   id?: string; path: string }
  | Update
  | AgentLogEvent
  | ErrorEvent;

// ── File protocol ─────────────────────────────────────────────────────────────

export type FileCommand =
  | { type: "append";      id?: string; content: string }
  | { type: "delete";      id?: string }
  | { type: "edit.atom";   id?: string; tickId: string; content: string }
  | { type: "delete.atom"; id?: string; tickId: string }
  | { type: "move.atom";   id?: string; tickId: string; afterTickId?: string };

export type FileEvent =
  | { type: "file.present"; id?: string }
  | { type: "file.absent";  id?: string }
  | Update
  | AgentLogEvent
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
  return new StoryWS<SessionCommand, SessionEvent>(`${WS_BASE}/session`);
}

export function branchConn(name: string) {
  return new StoryWS<BranchCommand, BranchEvent>(`${WS_BASE}/branch/${encodeURIComponent(name)}`);
}

export function fileConn(branch: string, path: string) {
  const encodedPath = path.split("/").map((p) => encodeURIComponent(decodeURIComponent(p))).join("/");
  return new StoryWS<FileCommand, FileEvent>(`${WS_BASE}/branch/${encodeURIComponent(branch)}/${encodedPath}`);
}
