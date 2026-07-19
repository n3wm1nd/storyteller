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

## HTTP endpoints (not part of this protocol)

Everything above is metadata and tick history — even a `WireTick`'s `content` is one atom's text, not "this file's current bytes." Getting the latter in or out doesn't belong on a persistent connection at all: it's a snapshot request (give me these bytes / here are new bytes), not a stream, and a browser already has first-class primitives for exactly that (`<img>`, `<a download>`, `fetch`). So alongside the WS endpoints, the server exposes two plain HTTP ones on the same port (`app/Server.hs`):

- **`GET /branch/{name}/{path}`** — the file's current raw content, `Content-Type` guessed from the extension. Lets the frontend embed an image directly (`<img src="…/branch/{name}/{path}">`) or link a download — no fetch-and-blob-URL dance, no base64 through JSON, no WS round trip to simulate what the browser already does natively.
- **`PUT /branch/{name}/{path}`** — replace the file's content with the request body and commit it as a tick (`Server.Writer.Branch.uploadFile`, the same `commitFiles` path other writes use, just for whole raw bytes instead of an LLM-authored atom). This is the only way to upload now — there is no WS command for it.

These don't follow the rules above (no scope, no push-on-change, no "reconnecting is resync") — each is a one-shot request that opens and closes its own branch scope server-side. But they do still interact with everything above: a `PUT` commits through the same `gitNotify`/`notifyRemaps` machinery every WS write goes through, so a `/branch/{name}` or `/branch/{name}/{path}` connection watching that branch sees the resulting tick via its ordinary ref-move notification, same as if the write had come in over WS.

The delimiter this draws: **WS carries what's inherently a stream** — tick history, live change notification, file lists, presence, agent logs. **HTTP carries what's inherently a snapshot** — a path's bytes, right now, in or out. Neither transport is asked to fake the other's job.

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

**Scope:** the user's session — branch list, character-branch list, settings.  
**On connect:** `session.ready`, then, unprompted, `branch.list` and `character.list` — there is no list-branches/list-characters command; a session only ever listens, never asks. Both are re-pushed unprompted again whenever the underlying set changes anywhere (any branch ref move), regardless of which connection caused it.  
**Commands:** `create-branch`, `delete-branch`.  
**Events:** `session.ready`, `branch.list`, `branch.created`, `branch.deleted`, `character.list`, `error`.  
**No tick state** — sessions don't have a tick chain.

### `/branch/{name}`

**Scope:** a branch — its full tick chain and file tree.  
**On connect:** `branch.ready` (file list) + `update` (full tick chain, current head).  
**Commands:** `add.note`, `move.tick`, `delete.tick`, `track`, `chargen`, `at` (generic rebase wrapper — see the file connection's `at`; no client trigger uses this one yet, would back a future Ticks-view rebase marker).  
**Events:** `branch.ready`, `file.added`, `update`, `agent.log`, `error`.  
**Head semantics:** the most recent content tick on the branch.  
**Not here:** scene presence (`enter.scene`/`leave.scene`) — a scene is a file, not the whole branch, so those live on the file connection below. See WRITER.md.

### `/branch/{name}/{path}`

**Scope:** a single file within a branch — ticks whose content is associated with that path.  
**On connect:** `file.present` + `update` (file's tick chain, current head), or `file.absent` (no ticks yet — normal initial state before first write).  
**After first write:** server pushes `file.present` + `update` — the connection transitions from absent to present without reconnecting.  
**Commands:** `file.create` (introduce this path into the tree as its own tick, empty — fails if the path already has ticks), `chat.append`, `edit.atom`, `delete.tick` (generic over any tick kind on this path's chain — an atom, or an annotation: note, prompt, summary occurrence, ask, image), `move.atom`, `delete`, `chat.writer`, `chat.fixer`, `chat.note`, `enter.scene`, `leave.scene`, `at` (rebase wrapper — any command above, including `enter.scene`/`leave.scene`, can be sent wrapped in `at` to run as of a historical tick).  
**Events:** `file.present`, `file.absent`, `update`, `tick.remap`, `agent.log`, `chat.preview*`, `error`.  
**Head semantics:** the most recent atom tick for this file.  
**Not here:** the file's raw current bytes — for downloading, embedding, or uploading a file wholesale (as opposed to editing it atom-by-atom), see "HTTP endpoints" above; `GET`/`PUT /branch/{name}/{path}` are plain HTTP, not WS.

### Working tree access *(planned)*

Neither connection type currently exposes the raw, ephemeral **working tree** described in DATA-MODEL.md as *live, uncommitted, editable* state — only the tick chain. This is a different feature from the `GET`/`PUT /branch/{name}/{path}` HTTP endpoints above: those read/replace-and-commit a file's bytes in one shot (no staging, no partial edit visible before commit); what's planned here is the working tree itself becoming addressable mid-edit, independent of any single write landing as a tick. Planned extension: raw working-tree content becomes part of connection state, the same way ticks are today. A `/branch/{name}` connection's working tree covers every file in the branch; a `/branch/{name}/{path}` connection's covers just that path. No `open`/`list` command is needed — the same connect-time-push-then-push-on-every-change model that already governs ticks (see "Core model" above) applies: on connect, the working tree's current raw content is pushed as part of state; any edit — from this connection, another connection, an agent, or the save process itself — produces an immediate update with the new raw content.

Planned commands, none implemented yet:
- **Write** — overwrite (or patch) a file's raw content in the working tree. No tick is created.
- **Upload** — introduce a brand-new file into the working tree.
- **Save** — runs the diff-and-merge path: reconciles the working tree's raw content against the last-saved tick chain and produces one or more atoms — appending where possible, merging into history where not, so the append-only invariant on the tick chain always holds afterward. This is the only point at which ticks are created; raw writes never touch the tick chain directly.

Not implemented: no commands, no wire types, and no push event exist for this yet. `WorkingTree` today is purely an internal storage detail — shared `State WorkingTree` used transiently within a single handler call — not something a client can address.

### `/library/{name}` *(app-specific)*

**Supersedes** the earlier `/chapters/{id}` sketch (per-chapter-only metadata) — same
motivation, generalized to the whole branch's organizational tree instead of one
chapter at a time.

**Scope:** the writer's organizational view over one branch — which
folders/chapters/outline artifacts exist, and how they relate, *not* their
prose content (see "New scopes are app-specific, not core" above; prose
still comes from the file connection). File-first: a unit's identity is its
file path, same as everywhere else in this backend — there is no separate id
space for "chapter 5" distinct from whatever file backs it.

Detection is deliberately permissive, by convention, not by prescribed depth
or folder naming — see `Storyteller.Writer.Library.classifyPath` and
WRITER.md's "Story structure" (the authoritative rule; not repeated here). A
path is recognized as a `unit` (prose) if some segment of it — any ancestor
directory name, or the leaf's own basename stem — contains a marker word
(`story`/`book`/`chapter`/`scene`, singular or plural, or `ch`), a `unit-
outline` for the reserved `outline.md`/`{stem}.outline.md` shapes, and
`other` otherwise — still a real, labeled tree node, never filtered out just
for not matching a known convention; the library view *labels*, it never
limits how a user chooses to organize. Ordering is always plain alphabetical
by path — no number is ever parsed out of a name.

**On connect:** two things, both derived from the same read —
- `nodes`: the full raw organizational tree, every node carrying its path,
  kind (`folder`/`unit`/`unit-outline`/`other`), and for a `unit`, `heading`
  (its own first line — see "Read side" below).
- `chapters`: every recognized unit, in the tree's own (alphabetical)
  reading order, already paired with its own beat sheet if any
  (`Storyteller.Writer.Library.narrativeUnits`) — `path`/`outlinePath` each
  absent independently depending on which artifact(s) exist (a beat sheet
  with no prose yet is still real planning content, see WRITER.md's
  "disposable scaffolding"; a unit's `heading` isn't repeated here — look it
  up on the matching `nodes` entry by `path`). This pairing is computed
  once, server-side, rather than left for each consumer (this connection's
  own UI, and the Summarizer agent) to reconstruct independently: "this
  chapter exists" is a real domain fact, not a display-only grouping, so
  duplicating it per-consumer would risk two independently-driftable
  answers to "what belongs to this chapter."

Payload is deliberately open-ended and expected to grow the same way
`/character/{charBranch}`'s does: character-presence per unit, a chapter's
own summary preview (the Summarizer agent — WRITER.md's "Summarization" —
exists now but isn't surfaced on this connection yet), notes, style guides.
New fields get added additively as those features land, not as a wire
redesign.

**Read side stays a bandwidth call, not an interpretation call.** A chapter
node's `heading` is its file's raw first line, not a parsed/validated H1 —
same "server hands over raw text, client decides what a display name is"
contract `sheet.md` already has (see WRITER.md), just narrowed to one line
instead of the whole file so a tree covering many chapters stays push-cheap
(see "stays push-cheap indefinitely" above) — sending each chapter's entire
prose just to read its first line would defeat that test.

**No tick chain of its own** — like `/character/{charBranch}`, this is
composed/derived data, not a tick stream, so it does not use the
`ticks`/`update` event shape. Its own structural push event (`library.tree`)
carries the whole tree; there is no separate per-unit sub-push.

**Commands:** `chapter.create { path, name }` — write `path` as a new file,
seeded with `# {name}` as its first line (the same "first H1 line is the
display name" convention `sheet.md` uses, see WRITER.md), and commit it as its
own atom tick. This is *not* the same as `file.create`: it's the one command
here that needs to know the heading convention, which is this connection's
business, not the generic file connection's — `file.create` still creates an
empty file with no header at all. Deliberately doesn't validate that `path`
matches the `chapters/ch{N}.md` shape — detection is freeform (see above), so
a path that doesn't match is still created, just not later recognized as a
`chapter` node. No other mutation lives here — everything else (actual
content edits) goes through `/branch/{name}/{path}` as usual, same "read is
raw-but-complete, write path stays unified" split as `/character/{charBranch}`.

**Events:** `library.tree`, `error`.

**Notification:** watches this branch's own ref-moves via the same
`watchBranch` plumbing `/branch/{name}` uses. Extend to also watch relevant
entity/character branches once character-presence is actually implemented —
not needed yet, since nothing derived here crosses branches today.

**Derivation lives in `Storyteller.Writer`, not in the connection.** "What
files belong to which book/chapter/scene, given the file-organization
conventions" has a second real consumer already anticipated (the Summarizer
agent will need the identical answer to know its own chapter boundaries), so
this logic belongs as its own function in `Storyteller.Writer`, with the
`/library/{name}` connection as its first caller — not inlined into the push
function, where a second consumer could silently drift from it later.

**Why per-branch, not global:** a global, cross-branch view (compare book X's
divergent versions across two branches, or browse the whole project's shelf
regardless of which branch is checked out) was considered and deliberately
deferred — see "Not here" below.

**Not here:**
- The prose content itself — same as `/character/{charBranch}`, opening a unit
  for editing goes through `/branch/{name}/{path}`, reusing the ordinary file
  view rather than a bespoke chapter editor.
- A cross-branch view — branches are already this project's organizational
  layer (including divergent, not-yet-merged storylines), so a per-branch
  `/library/{name}` doesn't need to flatten across them; a single-user tool
  focused on one checked-out branch at a time doesn't need every other
  branch's tree kept live just in case. If a genuine cross-branch feature
  (comparing/reconciling divergent books) becomes a real need later, it gets
  its own deliberately-designed scope for exactly that, rather than
  retrofitting dual-mode behavior into this one.

### `/library` *(reserved, not yet designed)*

The root path is reserved for a possible future global, cross-branch view —
see "Why per-branch, not global" above. No handler exists for it today; only
`/library/{name}` is implemented. Reserving the path now means that feature,
if it ever becomes a real need, gets a clean namespace instead of retrofitting
one into `/library/{name}` after the fact.

### `/character/{charBranch}` *(app-specific)*

**Scope:** sidebar-facing data about one character — not its journal (see
"New scopes are app-specific, not core" above; journal stays on the file
connection). Naming/structure conventions (`sheet.md`, `journal.md`,
`character/{id}` branch prefix) are documented in WRITER.md, not here — this
section only covers the connection.  
**On connect:** composed, augmented-but-not-processed character data —
starting with name + sheet content, expected to grow (mood, status, ...) as
sidebar features are added. May read across more than one branch to assemble
its payload.  
**Commands:** none yet envisioned — read-only; sheet edits happen through the
file connection.  
**Events:** an `update`-shaped push whenever the underlying sheet (or
whatever else the payload composes from) changes.  
**Why separate from `/branch/{name}`:** `/branch/{name}` is generic and
knows nothing about "character" — it wouldn't know to read `sheet.md`'s
content, compose it with other data, or scope by character branch
independent of whichever story branch happens to be open.

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
