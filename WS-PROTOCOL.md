# WebSocket Protocol

## Core model

A WebSocket connection is a **scope** — a live, server-maintained view of some piece of server state (a branch, a file, an agent run, a session). The server owns that state. The client holds a cached copy and renders it.

On connect, the server immediately pushes everything required to fully represent the current state of that scope. A connection is fully self-contained — it does not depend on any other connection being open, and is not affected by the state of other connections. A file connection does not care how the client learned the filename; a branch connection does not care whether any file connections are open. Each connection converges to its own current state independently, so connections lagging behind each other on a lossy network is not a consistency problem — just each stream being momentarily behind its own HEAD.

Connections do interact at the storage level — a write on a file connection creates ticks that the branch connection will subsequently push as an update. But from the branch connection's perspective the origin of those ticks is opaque: it sees new ticks appear at HEAD and pushes them, with no knowledge of which connection or agent caused them. After that it keeps the client up to date by pushing changes as they happen. The client never polls, never requests a resync. **Reconnecting is resync** — the connect-time push is the only mechanism for full state delivery.

---

## Server → Client: state pushes

### Tick updates

Tick pushes and head updates are **deliberately separate**:

- **`{ type: "ticks", ticks: WireTick[] }`** — store these ticks. They may be new or updated. The client upserts them into its local tick map by `tickId`. This alone does not mean anything visually changed yet.

- **`{ type: "update", ticks: WireTick[], head: string }`** — combined: upsert the ticks, then set the head pointer. The head pointer is the signal to rebuild the displayed chain. The client walks backwards from `head` through `parent` links to reconstruct chain order.

The client must always apply tick upserts before updating head, since head references ticks by id.

### Structural events

Connection-specific events that describe the scope itself rather than its tick content. Examples: `branch.ready` (branch name + file list), `file.present` / `file.absent` (whether the file exists). These are sent once on connect.

### Agent log

`{ type: "agent.log", level: "info"|"warning"|"error", message: string }` — streamed as an agent runs on this connection's scope. Ephemeral; not part of persistent state.

### Error

`{ type: "error", message: string }` — something went wrong processing a command. Human-readable, not machine-parseable.

---

## Client → Server: commands

Commands communicate **intent**, not operations. The client says what it wants done; the server decides how to do it, what ticks result, and pushes the updated state back.

The client does **not** apply commands locally. It sends the command and waits for the server to push back the resulting state update.

### Request ids

Some commands carry an optional `id` field. This is a client-chosen correlation handle — the server passes it through into the resulting update event so the client can associate a specific UI action (e.g. a pending spinner) with the state change that results from it. It is not universal: most commands don't need one, and the server never requires it. The client is never "waiting for a response" in a request/response sense.

### What clients do and don't express

| Clients express | Clients do not express |
|---|---|
| `add.note { refTickId, text }` | "create a tick of type note" |
| `edit.atom { tickId, content }` | "replace blob at this hash" |
| `move.tick { tickId, afterTickId }` | "rebase the chain" |
| `chat.prompt { path, text }` | "run model X with these tokens" |
| `append { content }` | "split into atoms and write" |

The server decodes intent into typed Haskell values, performs the operation, and pushes back the resulting state. **This is where business logic lives** — it should be clean, well-typed, and testable independently of the WebSocket layer.

---

## Client responsibilities

- Maintain a local tick map (`tickId → WireTick`) per connection scope.
- Walk the chain from `head` through `parent` links to produce an ordered list for rendering.
- Decide how to render: as prose (file view), as a timeline (ticks view), as a diff, etc.
- Manage ephemeral UI state (selection, pending indicators, annotation mode).
- Never interpret the storage format — just store and render what the server sends.

## Client non-responsibilities

- Computing what changed after a command.
- Knowing which ticks are "new" vs "updated".
- Knowing how a command affects the chain (rebase, append, replace).
- Filtering or projecting ticks — the server sends the right set for this scope.

---

## Tick wire format

One type for all connections:

```typescript
interface WireTick {
  tickId:   string;
  kind:     string;          // "atom", "note", "prompt", "root", ...
  refs:     string[];        // referenced tick ids (for rebasing, annotations)
  fields?:  Record<string, string>;  // structured metadata (e.g. "file")
  message:  string;          // raw encoded payload: "type:<kind>\n<body>"
  content?: string | null;   // atom blob content, absent for other kinds
  parent:   string | null;   // previous tick in chain, null for root
}
```

The `message` field carries the full encoded form including the `type:<kind>\n` prefix. Clients that need just the body strip the first line. The `kind` field is pre-extracted for convenience.

**Root ticks** (kind `"root"`, parent `null`) are structural artifacts. The server sends them as part of the full chain — it does not filter. Clients skip them at render time.

---

## Connection types

### `/session`

**Scope:** the user's session — branch list, settings.  
**On connect:** `session.ready`, then responds to `list-branches` with `branch.list`.  
**Commands:** `list-branches`, `create-branch`, `delete-branch`.  
**Events:** `session.ready`, `branch.list`, `branch.created`, `branch.deleted`, `error`.  
**No tick state** — sessions don't have a tick chain.

### `/branch/{name}`

**Scope:** a branch — its full tick chain and file tree.  
**On connect:** `branch.ready` (file list) + `update` (full tick chain, current head).  
**Commands:** `add.note`, `move.tick`, `delete.tick`, `chat.prompt`, `track`, `chargen`.  
**Events:** `branch.ready`, `file.added`, `update`, `agent.log`, `error`.  
**Head semantics:** the most recent content tick on the branch.

### `/branch/{name}/{path}`

**Scope:** a single file within a branch — ticks whose content is associated with that path.  
**On connect:** `file.present` + `update` (file's tick chain, current head), or `file.absent` (no ticks yet — normal initial state before first write).  
**After first write:** server pushes `file.present` + `update` — the connection transitions from absent to present without reconnecting.  
**Commands:** `append`, `edit.atom`, `delete.atom`, `move.atom`, `delete`.  
**Events:** `file.present`, `file.absent`, `update`, `agent.log`, `error`.  
**Head semantics:** the most recent atom tick for this file.

### `/agent/{id}` *(planned)*

**Scope:** a running agent.  
**On connect:** minimal state (`{ running: true }` or already-finished status).  
**Events:** `agent.log` (streamed), `agent.done` or `agent.error` on completion.  
**No persistent tick state** — agents write ticks to a branch, not to the agent connection itself. Observe the branch connection to see the ticks they produce.

---

## Responsibility matrix

| Concern | Owner | Notes |
|---|---|---|
| Persistent state | Server | Storage is authoritative; client holds a cache |
| Performing actions | Server | Decodes intent, executes, updates storage |
| Pushing state to clients | Server (connection) | On connect and after every mutation |
| Encoding (wire format) | Server | `Server.$scope` produces typed values; dispatch serialises |
| Decoding (wire format) | Client | Receives `WireTick` and raw events, interprets locally |
| Rendering | Client | Decides how to display a tick chain (prose, list, diff…) |
| UI state | Client | Tabs, toggles, selection, pending indicators — not the server's concern |

When something is broken or missing, this matrix tells you where to look:
- Data wrong or stale → server storage or push logic
- Data not arriving → connection / dispatch
- Displayed wrong → client rendering
- Action not taking effect → `Server.$scope` handler
- Wire shape mismatch → server encoding / client decoding

---

## Server-side structure

The dispatch layer is **routing only** — it pattern-matches the incoming command type and delegates to a pure or limited-monad handler function. No WS concerns inside handlers.

Handler functions return typed results. A helper assembles the `Update` from storage state after the mutation. This is the layer to unit-test.

```
Connection  →  Dispatch (routing)  →  Handler (pure/limited effects)
                                   →  Update builder (reads storage → produces Update)
                                   →  emit Update over WS
```

The `Update` builder always reads full current state from storage after a mutation — it does not try to compute a delta. This keeps it simple and correct; the client's upsert model handles receiving redundant ticks gracefully.

### Possible future optimisation (not implemented)

On slower connections it may be worth sending only tick ids in the update, letting the client request the ones it doesn't already have. This saves retransmitting ticks the client has already cached. On localhost the extra round-trip makes this strictly worse than unconditional sends, so it is not worth doing until there is a concrete need.
