# WebSocket Protocol

## Core model

A WebSocket connection is a **scope** — a live, server-maintained view of some piece of server state (a branch, a file, an agent run, a session). The server owns that state. The client holds a cached copy and renders it.

On connect, the server immediately pushes everything required to fully represent the current state of that scope. A connection is fully self-contained — it does not depend on any other connection being open, and is not affected by the state of other connections. A file connection does not care how the client learned the filename; a branch connection does not care whether any file connections are open. Each connection converges to its own current state independently, so connections lagging behind each other on a lossy network is not a consistency problem — just each stream being momentarily behind its own HEAD.

Connections do interact at the storage level — a write on a file connection creates ticks that the branch connection will subsequently push as an update. But from the branch connection's perspective the origin of those ticks is opaque: it sees new ticks appear at HEAD and pushes them, with no knowledge of which connection or agent caused them. After that it keeps the client up to date by pushing changes as they happen. The client never polls, never requests a resync. **Reconnecting is resync** — the connect-time push is the only mechanism for full state delivery.

---

## Scope design principles

A connection scope models *some object the client cares about* — a branch, a file, eventually a chapter, a character. The rules below govern how new scopes get drawn and what they may contain.

### Read is raw-but-complete, not raw-but-partial

"Raw" describes how much interpretation the server does — none: no truncation, no display formatting, no pre-built UI objects — not how much data it sends. Every reference within a scope's payload must be fully resolved before it is sent: no tick id the client has to separately look up, no field that requires a second connection to complete. The connection open *is* the request; there is no follow-up round trip.

This doubles as the test for whether a scope is drawn correctly: if fully resolving a scope's references means pulling in a large, open-ended fraction of the graph (e.g. every entity branch that ever touched the story), the scope is too big and needs to be split or narrowed. A scope has to stay push-cheap indefinitely, not just at connect time.

### The decode/encode burden tracks who acts on the data, not who displays it

- **Read side:** the server does no interpretation; the client decodes raw ticks into whatever concepts it needs (chapter, character, whatever). The client must understand the raw structure — it is not shielded from it.
- **Write side:** the client sends unprocessed intent; the server decodes that intent into a structurally valid mutation (respecting append-only, atom boundaries, rebase). The client does not need to understand how intent becomes a tick operation.

The asymmetry follows responsibility, not secrecy: whoever performs the mutation must own turning intent into structure, because storage correctness is its job (the server); whoever renders must own turning structure into meaning, because presentation is its job (the client).

### Backend-authoritative vs. frontend-advisory duplication

Both sides end up independently deriving the same domain concepts from the same raw substrate — "a chapter is the files under `chapters/`" gets encoded once server-side (because an agent, e.g. the Summarizer, has to act on it) and once client-side (because a panel has to render it). This duplication is expected, not a smell — and it is not symmetric:

- The **server's** version is authoritative: it's what agents actually execute against, and what enforces the append-only/rebase invariants. If it's wrong, it corrupts data or misleads an agent.
- The **client's** version is advisory: it exists only to render and to pre-empt requests already known to fail. If it's wrong, the cost is a UI bug — a wrongly grayed-out range, a misplaced note, an offered action that errors when sent. Never data corruption, because the server re-derives and re-validates everything itself regardless of what the client assumed.

Practical test for any given piece of logic: **does something server-side (an agent, the storage layer) act on this, or does a human only look at it?** If an agent acts on it, it must be server-authoritative, full stop. If only a human looks at it, the client is free to have its own approximate, occasionally-wrong copy — fixing it is a client-only change, never a data-integrity concern.

### Preview vs. commit

Live interactive feedback — a timeline drag preview, a hover state, a grayed-out range while dragging — is always a client-local derivation from data already held on an already-open connection, never its own round trip. Standing up a connection or firing a request per frame of a drag would apply server authority to a moment where nothing is actually being committed yet. The server only re-enters the picture once, at actual commit time, through the ordinary intent-command path — re-validated exactly the same way any other write is, regardless of what the client's preview assumed.

### The client never mutates synced data locally

Everything the client displays must trace back to something a connection actually sent — with one exception: volatile, display-only state (which panel is open, what's typed in an input box, scroll position, selection). Outside of that exception, the client does not get to change data in response to a local action. It communicates intent and waits for the resulting `update` (or `error`) to arrive; the cached tick map is never edited directly by client code.

This does not forbid an optimistic-feeling UI. An in-flight edit can show its expected result immediately, but only via a client-only *override* layered on top of the real cached value — "display this atom as X instead of the cached content, until the real update arrives" — never by writing X into the tick map itself. The override is discarded the moment the corresponding `update` or `error` lands (whichever comes first, same rule as the chat preview above), and it does not survive a reload, because it was never part of the synced state to begin with. The tick map is exactly what "reconnecting is resync" (see Core model) reconstructs from scratch — anything durable enough to survive a refresh has to have actually come from the server, by construction.

### New scopes are app-specific, not core

`/branch/{name}` and `/branch/{name}/{path}` are generic, storage-shaped scopes with no domain vocabulary — they're reusable across whatever application gets built on this backend. A scope like `/chapters/{id}` (chapter metadata only — title, tick-range boundaries, summary tick, character-presence list; deliberately *not* prose content, which still comes from the file connection) is specific to one application build and isn't meant to generalize to a hypothetical sibling app's scopes. Duplicating connection/dispatch glue across future app-specific servers, rather than abstracting it into a shared framework, is the expected and accepted cost.

Because scope boundaries are drawn by judgment, not derived mechanically, expect them to be imperfect: a panel occasionally opening more connections than strictly necessary, or receiving a few unused fields on one it already has open. Both failure modes are cheap and reversible — reshuffling which scope owns a field is a local fix, not a protocol renegotiation.

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

### Chat preview (streaming)

Sent while an LLM call triggered by this connection's command (`chat.prompt`, `chargen`, ...) is in flight, if the underlying model/provider supports streaming:

- **`{ type: "chat.preview.start" }`** — a streaming LLM call has begun. Reset any locally accumulated preview text.
- **`{ type: "chat.preview", text: string }`** — an incremental chunk of assistant text. Append to the accumulated preview.
- **`{ type: "chat.preview.thinking", text: string }`** — an incremental chunk of reasoning/thinking text, if the model exposes it. Kept separate from `chat.preview` text; append to its own accumulated buffer.
- **`{ type: "chat.preview.end" }`** — the streaming call has finished (successfully or not).

This preview is a **best-effort draft only** — nothing more than a live look at tokens as they arrive over the wire. There is no guarantee it matches, or is followed by, anything persisted: an agent may run multiple LLM calls before writing a tick (each with its own `start`...`end` cycle), may write ticks whose content differs from the last streamed draft, or may finish with nothing persisted at all. The client must therefore:

- Discard/replace the preview the instant this connection's `update` or `error` arrives, whichever comes first — that is the real result, superseding any draft.
- Also clear the preview on `chat.preview.end` regardless of whether an `update`/`error` follows, since a call can legitimately end with no persisted result.

Not correlated by request `id` — a connection's command loop processes one command at a time, so there is never more than one in-flight preview per connection.

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

### Working tree access *(planned)*

Neither connection type currently exposes the raw, ephemeral **working tree** described in DATA-MODEL.md — only the tick chain. Planned extension: raw working-tree content becomes part of connection state, the same way ticks are today. A `/branch/{name}` connection's working tree covers every file in the branch; a `/branch/{name}/{path}` connection's covers just that path. No `open`/`list` command is needed — the same connect-time-push-then-push-on-every-change model that already governs ticks (see "Core model" above) applies: on connect, the working tree's current raw content is pushed as part of state; any edit — from this connection, another connection, an agent, or the save process itself — produces an immediate update with the new raw content.

Planned commands, none implemented yet:
- **Write** — overwrite (or patch) a file's raw content in the working tree. No tick is created.
- **Upload** — introduce a brand-new file into the working tree.
- **Save** — runs the diff-and-merge path: reconciles the working tree's raw content against the last-saved tick chain and produces one or more atoms — appending where possible, merging into history where not, so the append-only invariant on the tick chain always holds afterward. This is the only point at which ticks are created; raw writes never touch the tick chain directly.

Not implemented: no commands, no wire types, and no push event exist for this yet. `WorkingTree` today is purely an internal storage detail — shared `State WorkingTree` used transiently within a single handler call — not something a client can address.

### `/chapters/{id}` *(planned, app-specific)*

**Scope:** metadata for one chapter — not its prose (see "New scopes are app-specific, not core" above).  
**On connect:** title, tick-range boundaries (start/end refs), the summary tick if one exists, character-presence list (resolved from cross-branch refs into the range).  
**Commands:** none yet envisioned — this scope is read-only; edits happen through the file connection for the chapter's content.  
**Events:** an `update`-shaped push whenever a tick lands inside the range or a referencing entity-branch tick appears.  
**Why separate from `/branch/{name}/{path}`:** keeps the (large, per-atom) prose stream and the (small, cross-cutting) metadata stream from duplicating each other over the wire — a panel that only needs the chrome (breadcrumb, summary, cast list) doesn't have to receive or filter the full atom chain to get it.

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

Each branch/file connection (`Server.*.Connection`) runs two independent, long-lived interpreter stacks on separate threads, both entering the same branch scope once and holding it for the connection's lifetime rather than re-entering per command or per push:

```
Connection
 ├─ command thread:  receive → decode → Dispatch (routing) → Handler (pure/limited effects)
 │                                                          → push*   → emit over WS
 └─ notify thread:   watchBranch (generic ref-move watcher) → push*   → emit over WS
```

These two axes are where the actual meaning of a connection lives, and they answer different questions:

- **Dispatch** (`Server.*.Dispatch`) answers *"what can a connected client do, and in what environment does it run?"* It is routing only — pattern-match the incoming command, delegate to a handler, no WS concerns inside handlers. This is the layer to unit-test independently of the socket.
- **`pushInitial` / `pushIncremental`** (in `Connection.hs`) answer *"what does this connection's scope look like, and how do we tell the client what changed?"* This is where file-presence tri-states, "does this update touch my chain," and similar scope-specific logic live. It's intentionally *not* shared between connection types — a branch connection and a file connection disagree on what "changed" even means, so forcing one push function to serve both would blur the one thing that actually distinguishes them.

The notify thread's watch loop itself — block on the ref-move channel, skip notifications for other branches, thread an accumulator (e.g. last HEAD pushed) through repeated calls to `pushIncremental` — is pure plumbing with no domain meaning, so it *is* shared, as `Server.Notification.watchBranch`. It only assumes `Embed IO`; it has no opinion on what effects the push function needs or what "changed" means, so it composes with `runM . withBranch "name" $ watchBranch ... handler` rather than dictating the stack shape.

The `Update` builder inside each push function always reads full current state from storage after a mutation — it does not try to compute a delta. This keeps it simple and correct; the client's upsert model handles receiving redundant ticks gracefully.

### Possible future optimisation (not implemented)

On slower connections it may be worth sending only tick ids in the update, letting the client request the ones it doesn't already have. This saves retransmitting ticks the client has already cached. On localhost the extra round-trip makes this strictly worse than unconditional sends, so it is not worth doing until there is a concrete need.
