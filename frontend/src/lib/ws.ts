// WebSocket connection abstractions for the storyteller server.
//
// Two connection types mirror the server's two endpoints:
//   sessionConn  (/session)          — branch management
//   branchConn   (/branch/{name})    — file ops and agents within a branch
//
// Both support auto-reconnect. A reconnect resets server-side implied state,
// so callers pass an onConnected callback to re-initialize after each connect.

const WS_BASE = process.env.NEXT_PUBLIC_WS_URL ?? "ws://localhost:8090";

// ── Protocol types ────────────────────────────────────────────────────────────

export type SessionCommand =
  | { type: "list-branches"; id?: string }
  | { type: "create-branch"; id?: string; branch: string }
  | { type: "delete-branch"; id?: string; branch: string };

export type SessionEvent =
  | { type: "session.ready" }
  | { type: "branch.list";    id?: string; branches: string[] }
  | { type: "branch.created"; id?: string; branch: string }
  | { type: "branch.deleted"; id?: string; branch: string }
  | { type: "error"; message: string };

export interface ContextItem {
  tickId:  string;
  kind:    string;
  content: string;
}

export type BranchCommand =
  | { type: "append";      id?: string; path: string; content: string }
  | { type: "read";        id?: string; path: string }
  | { type: "read.ticks";  id?: string }
  | { type: "delete.file"; id?: string; path: string }
  | { type: "track";       id?: string; source: string; files: { from: string; to: string }[] }
  | { type: "chargen";     id?: string; path: string; scenario: string; seed?: number }
  | { type: "add.note";    id?: string; refTickId: string; text: string }
  | { type: "move.tick";   id?: string; tickId: string; afterTickId?: string }
  | { type: "delete.tick"; id?: string; tickId: string }
  | { type: "chat.prompt"; id?: string; path: string; text: string; context?: ContextItem[] };

export type BranchTick =
  | { kind: "atom";   tickId: string; parent: string | null; refs: string[]; message: string; file?: string }
  | { kind: "note";   tickId: string; parent: string | null; ref: string; text: string }
  | { kind: "prompt"; tickId: string; parent: string | null; file: string; text: string };

export type BranchEvent =
  | { type: "branch.ready";      id?: string; branch: string; files: string[] }
  | { type: "branch.ticks";      ticks: BranchTick[] }
  | { type: "ticks.invalidated"; id?: string; mapping: IdMapping[] }
  | { type: "file.added";        id?: string; path: string }
  | { type: "agent.log";         level: "info" | "warning" | "error"; message: string }
  | { type: "error"; message: string };

export type FileCommand =
  | { type: "append";      id?: string; content: string }
  | { type: "read";        id?: string }
  | { type: "delete";      id?: string }
  | { type: "edit.atom";   id?: string; tickId: string; content: string }
  | { type: "delete.atom"; id?: string; tickId: string }
  | { type: "move.atom";   id?: string; tickId: string; afterTickId?: string };

export interface FileTick {
  tickId:  string;
  kind:    string;
  refs:    string[];
  fields:  Record<string, string>;
  message: string;
  content: string | null;   // only present for atoms
  parent:  string | null;
}

export type IdMapping = { old: string; new: string };

export type FileEvent =
  | { type: "file.ticks";    ticks: FileTick[] }
  | { type: "file.absent";   id?: string }
  | { type: "tick.appended"; tick: FileTick }
  | { type: "atom.replaced"; id?: string; oldTickId: string; tick: FileTick }
  | { type: "atom.deleted";  id?: string; oldTickId: string; mapping: IdMapping[] }
  | { type: "atom.moved";    id?: string; mapping: IdMapping[] }
  | { type: "file.updated";  id?: string; content: string }
  | { type: "file.deleted";  id?: string }
  | { type: "agent.log";     level: "info" | "warning" | "error"; message: string }
  | { type: "error"; message: string };

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
  // Decode first in case the server already sent an encoded path, then re-encode cleanly.
  const encodedPath = path.split("/").map((p) => encodeURIComponent(decodeURIComponent(p))).join("/");
  return new StoryWS<FileCommand, FileEvent>(`${WS_BASE}/branch/${encodeURIComponent(branch)}/${encodedPath}`);
}
