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

export type BranchCommand =
  | { type: "append";      id?: string; path: string; content: string }
  | { type: "read";        id?: string; path: string }
  | { type: "delete.file"; id?: string; path: string }
  | { type: "track";       id?: string; source: string; files: { from: string; to: string }[] }
  | { type: "chargen";     id?: string; path: string; scenario: string; seed?: number };

export type BranchEvent =
  | { type: "branch.ready"; id?: string; branch: string; files: Record<string, string> }
  | { type: "file.content"; id?: string; path: string; content: string }
  | { type: "file.updated"; id?: string; path: string; content: string }
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
        this._emit("disconnected");
        if (!this.stopped) this._scheduleReconnect();
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
