// WebSocket connection abstractions for the storyteller server.
//
// Six connection types mirror the server's six endpoints:
//   sessionConn    (/session)                        — branch management
//   branchConn     (/branch/{name})                  — full branch tick chain + file tree
//   fileConn       (/branch/{name}/{path})            — file-scoped tick chain
//   characterConn  (/character/{name})                — sidebar-facing character state (read-only)
//   contextViewConn (/branch/{name}/$context/{path})  — stateless Context DSL program preview
//   libraryConn    (/library/{name})                  — writer-facing book/chapter tree (mostly read-only)
//
// All connections support auto-reconnect. Reconnecting is the only resync
// mechanism — the server pushes full state on every new connection.

function wsBase() {
  if (process.env.NEXT_PUBLIC_WS_URL) return process.env.NEXT_PUBLIC_WS_URL;
  const proto = window.location.protocol === "https:" ? "wss:" : "ws:";
  // Single-process production build (STATIC_DIR, see app/Server.hs): the
  // page and the API are the same origin, whatever host:port that turned
  // out to be — set only by `npm run build:static` (next.config.ts's
  // STATIC_EXPORT), baked in at build time like any other NEXT_PUBLIC_ var.
  // Dev mode never sets this, so it keeps hitting the hard-coded :8090
  // below, same as always.
  if (process.env.NEXT_PUBLIC_WS_SAME_ORIGIN) return `${proto}//${window.location.host}`;
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
// connection just to simulate it. Real file paths always sit under the
// fixed "file/" segment (see app/Server.hs's httpApp) — never bare at the
// branch's own root — so a path that happens to be named "$raw"/"$image"
// can never be misrouted into one of those reserved-command endpoints.
export function branchFileUrl(branch: string, path: string) {
  return `${httpBase()}/branch/${encodeURIComponent(branch)}/file/${encodePath(path)}`;
}

// Upload/replace a branch file's content directly from its bytes — the PUT
// counterpart to 'branchFileUrl'. Replaces the old WS 'upload' command: a
// dropped file's bytes go straight over HTTP instead of being read as text,
// JSON-encoded, and tunneled through the branch connection.
export async function uploadBranchFile(branch: string, path: string, content: Blob) {
  const res = await fetch(branchFileUrl(branch, path), { method: "PUT", body: content });
  if (!res.ok) throw new Error(`upload failed: ${res.status} ${path}`);
}

// Attach an image to `path`'s own tick timeline (see app/Server.hs's
// PUT /branch/{name}/$image/{path} and Server.Writer.Branch.uploadImage):
// the bytes land as their own sibling asset, and a new Image tick pointing
// at it joins `path`'s timeline — not a plain file replacement the way
// 'uploadBranchFile' is. `path` here names the file whose timeline is being
// dropped onto, not the image itself, so the original filename travels
// separately as a query param for the server to derive the asset's path from.
export async function uploadImage(branch: string, path: string, file: File, caption?: string) {
  const q = new URLSearchParams({ filename: file.name });
  if (caption) q.set("caption", caption);
  const url = `${httpBase()}/branch/${encodeURIComponent(branch)}/$image/${encodePath(path)}?${q.toString()}`;
  const res = await fetch(url, { method: "PUT", body: file });
  if (!res.ok) throw new Error(`image upload failed: ${res.status} ${path}`);
}

// Raw-edit-mode save: whole-file text replace, reconciled against the
// path's existing atom chain server-side (see app/Server.hs's
// PUT /branch/{name}/$raw/{path} and Server.Writer.Branch.saveFile) rather
// than deposited as an opaque binary like 'uploadBranchFile' — unchanged
// atoms keep their ids, only the parts that actually changed get rewritten.
export async function saveRawFile(branch: string, path: string, content: string) {
  const url = `${httpBase()}/branch/${encodeURIComponent(branch)}/$raw/${encodePath(path)}`;
  const res = await fetch(url, { method: "PUT", body: content });
  if (!res.ok) throw new Error(`save failed: ${res.status} ${path}`);
}

// "Save as new": the same PUT /$raw/{path} resource, but with the "?asNew"
// query flag instead of the default reconciled diff (see app/Server.hs and
// Server.Writer.Branch.saveFileAsNew/Storage.Ops.saveFileAsNew) — a
// wholesale replacement, no note/atom continuity carried forward. The raw/
// markdown editor's own escape hatch for "this isn't an edit, it's a
// replacement" (a structural change to a file's own list/table content
// that shouldn't be tracked atom-by-atom). `newPath` forks to a different
// file instead of replacing this one in place; omitted, it defaults to
// `path` itself server-side.
export async function saveRawFileAsNew(branch: string, path: string, content: string, newPath?: string) {
  const url = `${httpBase()}/branch/${encodeURIComponent(branch)}/$raw/${encodePath(path)}?asNew${newPath ? `&newPath=${encodeURIComponent(newPath)}` : ""}`;
  const res = await fetch(url, { method: "PUT", body: content });
  if (!res.ok) throw new Error(`save as new failed: ${res.status} ${path}`);
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
  | { type: "delete-branch"; id?: string; branch: string }
  // Restore every tracked ref to the state recorded by 'entryId' (an
  // UndoLog entry's own id) — see Storyteller.Core.Undo.resetToUndo.
  // Symmetric: entryId can name any entry, earlier or later than the
  // current one, so this doubles as both undo and redo.
  | { type: "undo.reset"; id?: string; entryId: string }
  // Ask whatever branch/file connection is running the command with wire
  // id `targetId` to stop early — sent here, on /session, rather than on
  // that command's own connection, since that connection's command loop
  // only reads its next message after the current one finishes (see
  // Server.Writer.Session.Protocol's Cancel). Fire-and-forget: no response
  // event, and canceling an already-finished/unknown id is a silent no-op.
  | { type: "cancel"; id?: string; targetId: string }
  // Atomically create a character branch and deposit a fixed set of text
  // files, plus an optional base64-encoded avatar image, onto it — the
  // SillyTavern character-card-import drop zone (see lib/taverncard.ts's
  // 'buildCharacterFiles' for how a dropped card becomes this shape, and
  // Server.Writer.Session.Protocol's ImportCharacterCard for why this is
  // one command instead of a create-branch, per-file saves, and a
  // separate avatar PUT — that split raced the avatar's upload against
  // the branch's own creation in practice, not just in theory). `avatar`
  // is `undefined` for a card with no image (any .json card, or a .png
  // with no embedded portrait a viewer would show). `note`, when given,
  // lands as a free-floating Note tick rather than sheet.md prose — a
  // card's provenance/creator-notes are metadata about the import for the
  // human author, not part of what an agent should read as the
  // character's identity or voice (see lib/taverncard.ts's
  // buildImportNote).
  | { type: "import-character-card"; id?: string; branch: string; files: { path: string; content: string }[]; avatar?: string; note?: string };

// One character branch's raw summary — sheet.md content, unprocessed (see
// WS-PROTOCOL.md's "read is raw-but-complete" rule). The client is
// responsible for decoding this into a display name (first Markdown H1
// line, falling back to the branch id) the same way it decodes any other
// raw content into a concept it needs.
// `avatar` is an existence flag, not the image data -- the actual bytes are
// a plain GET away at branchFileUrl(branch, "avatar.png"), same route any
// other branch file uses, so there's no reason to duplicate binary content
// over this push the way `sheet` duplicates text.
export interface CharacterSummary {
  branch: string;
  sheet: string | null;
  avatar: boolean;
}

// One entry in the shared, session-wide undo log (Storyteller.Core.Undo) --
// a whole-repo snapshot taken after every real tracked ref write, anywhere.
// The log itself is flat and append-only, never a tree (a real branching
// history is a future project -- see Storyteller.Core.Undo's haddock), and
// carries no notion of "current" -- a jump ("undo.reset") never adds to or
// otherwise changes this list at all. "Which dot is active" and "what
// would redo do" are purely local, ephemeral UI state derived from which
// entry was last jumped to and whether this list has grown since -- see
// app/undo-timeline.tsx.
// 'kind' is whatever tag the write's own tick led with server-side (e.g.
// "atom", "root", "note") -- absent for a branch deletion (nothing left to
// read) or anything that didn't decode one. Opaque here: this module
// doesn't know the full set of tags, undo-timeline.tsx owns turning
// whichever one shows up into a color, with an "unknown tag" fallback so a
// server-side tag this client hasn't been taught yet still renders instead
// of erroring.
export interface WireUndoEntry {
  id: string;
  time: string;
  kind: string | null;
}

// branch.list, character.list, and undo.log are always unprompted -- pushed
// once right after session.ready, and again whenever the underlying set
// changes (see Server.Writer.Session.Connection's notifier). There is no
// request for any of them: a session only ever listens. undo.log is
// chronological, oldest first -- the order a timeline renders in.
export type SessionEvent =
  | { type: "session.ready" }
  | { type: "branch.list";     branches: string[] }
  | { type: "character.list";  characters: CharacterSummary[] }
  | { type: "undo.log";        entries: WireUndoEntry[] }
  | ErrorEvent;

// ── Branch protocol ───────────────────────────────────────────────────────────

export type BranchCommand =
  // onlyFile restricts to one source file; omitted, every file on the
  // source branch is tracked into `to` — see
  // Storyteller.Writer.Agent.Tracker.trackBranch.
  | { type: "track";       id?: string; source: string; onlyFile?: string; to: string }
  // sync.tasks reconciles tasks.md against this (the command's own)
  // branch's own content — onlyFile has the same "one file, or every
  // file" shape as track's, `to` defaults server-side to "tasks.md" — see
  // Storyteller.Writer.Agent.Tasks. suggest.tasks always reads this
  // branch's own full character context (sheet, other context files,
  // recent journal) instead — no onlyFile, it isn't file-selectable.
  // suggest.tasks's loreSource, when given, additionally folds that
  // (story) branch's own world lore in as source material — never that
  // branch's raw scene content, so a character's suggestions only ever
  // draw on what they'd actually know — see
  // Server.Writer.Branch.Protocol.SuggestTasks.
  | { type: "sync.tasks";    id?: string; onlyFile?: string; to?: string }
  | { type: "suggest.tasks"; id?: string; loreSource?: string; to?: string }
  | { type: "chargen";     id?: string; path: string; scenario: string; seed?: number }
  // Run one summarizer pass for `kind` (see Server.Writer.Branch.summarize)
  // — "prose/chapter", "lore/article", or "journal" (a convenience that
  // cascades both journal tiers server-side; "journal/chunk"/"journal/meta"
  // also work individually but nothing here needs to name them). Free to
  // call with nothing new to summarize — idempotent, no LLM call happens
  // unless a real trigger point (a touched file, a full chunk) is reached.
  // No direct response: the result (if any) is a new "summary"-kind tick
  // riding this file's ordinary push — see
  // Server.Writer.File.summaryTicksFor's own Haddock.
  | { type: "summarize";   id?: string; kind: string }
  | { type: "add.note";    id?: string; refTickId: string; text: string }
  | { type: "move.tick";   id?: string; tickId: string; afterTickId?: string }
  | { type: "delete.tick"; id?: string; tickId: string }
  // Rebase, same shape as FileCommand's — generic capability, no client
  // trigger uses this yet (would be a future Ticks-view rebase marker).
  | { type: "at";          id?: string; tickId: string; command: BranchCommand };

export type BranchEvent =
  | { type: "branch.ready"; id?: string; branch: string; files: string[] }
  | { type: "file.added";   id?: string; path: string }
  | { type: "file.removed"; id?: string; path: string }
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
  | { type: "rename";      id?: string; newPath: string }
  // Checkpoint: freeze this path's current lifetime and clone it in full
  // (every atom, plus every note/fixup/swipe attached to one) onto a fresh
  // one. From here on, an atom edit/delete can only reach the new copies —
  // everything before this point stays exactly as it was, just no longer
  // reachable through ordinary editing (see Storage.Ops.checkpointFile).
  | { type: "checkpoint";  id?: string }
  | { type: "edit.atom";   id?: string; tickId: string; content: string }
  // Edit a chat prompt tick's text in place — distinct from edit.atom: a
  // prompt isn't file content, so this doesn't restage anything.
  | { type: "edit.prompt"; id?: string; tickId: string; content: string }
  // Delete: drop every tick in `targets` from the chain, in one
  // transaction — generic over any tick kind, not just atoms (an
  // annotation — note, prompt, summary occurrence, ask, image — is a real
  // chain-position tick exactly like an atom is). The server sorts
  // descendants-first internally (Storage.Ops.descendantsFirst) — targets
  // can be given in any order.
  | { type: "delete.ticks"; id?: string; targets: string[] }
  | { type: "move.atom";   id?: string; tickId: string; afterTickId?: string }
  // Merge: combine a contiguous run of one file's atoms (`targets`) into one.
  | { type: "merge.atoms"; id?: string; targets: string[] }
  // Split: re-run the splitter over each of `targets`' own content, in place.
  | { type: "split.atoms"; id?: string; targets: string[] }
  // Hide/unhide: tag (or untag) each of `targets` as excluded from an
  // agent's ambient context, in place — the atoms stay in the file, just
  // marked (see Storage.Ops.setAtomHidden). Surfaces on the tick as
  // `fields.hide === "true"`.
  | { type: "hide.atoms";   id?: string; targets: string[] }
  | { type: "unhide.atoms"; id?: string; targets: string[] }
  // Writer, or FlowWriter (implicitly) when `flowTid` is set — the tick
  // that was HEAD when the user started typing, so the agent can judge
  // whether atoms generated since then are still provisional. `pinned` is
  // the user's own explicit atom/annotation selection (see ContextItem) —
  // unrelated to context assembly, added as reference text regardless of
  // the other fields below.
  //
  // Three independent, narrow slots — replacing the old single `context`
  // whole-program override (see the project chat that settled this: full
  // per-call DSL control over the *entire* writer context moved expertise
  // away from the agent and doubled every piece of context-assembly
  // knowledge across two hand-synced implementations; only the inputs a
  // user genuinely knows better than the agent stay client-choosable):
  //
  //  - `lore`: an optional Context DSL program overriding `context.lore`
  //    for this one call (omitted = compiled-in lore).
  //  - `pastChaptersMode`: `"full"` (default) or `"compressed"` — a fixed
  //    toggle between two compiled-in chapter-framing shapes, never a
  //    program.
  //  - `pinnedPrograms`: zero or more bare 0-arity Context DSL programs
  //    (typically just a name, like `rules.magic`), each resolved and
  //    folded into this call's pinned/authors-notes content alongside
  //    `pinned`'s own plain items.
  //
  // Style, character identity, and "other notes" stay entirely agent-
  // owned — no client knob over any of them.
  | { type: "chat.writer"; id?: string; text: string; pinned?: ContextItem[]; lore?: string; pastChaptersMode?: "full" | "compressed"; pinnedPrograms?: string[]; flowTid?: string }
  // Roleplay: every character present on this file is interrogated, in
  // character, for what they'd do or say before one scene gets written and
  // appended — see Server.Writer.File.roleplayWriter. `text` is the
  // author's direction and may be empty (the scene just continues
  // naturally). No context of its own yet — each interrogated character
  // reads their own full branch, not a curated ambient slice.
  | { type: "chat.roleplay"; id?: string; text: string }
  // Fixer: `targets` are the atoms flagged as the subject of `text`. No
  // context program of its own — with targets present the Fixer agent takes
  // no ambient context at all; with none, the server falls through to
  // chat.writer with an empty (default-resolving) program.
  | { type: "chat.fixer";  id?: string; text: string; pinned?: ContextItem[]; targets?: string[] }
  // Regen: rewrite this chapter to fit its beat sheet (ch{N}.outline.md by
  // convention), respecting `text` as the user's steer. A reconciliation, not
  // a wipe — unchanged prose keeps its atoms. `byBeat` selects the
  // beat-by-beat driver over the whole-chapter one. No context program of
  // its own yet, same as chat.fixer.
  | { type: "chat.regen";  id?: string; text: string; pinned?: ContextItem[]; byBeat?: boolean }
  // Correct: delete `promptTickId` and every atom in `targets` (an
  // instruction group's own prompt + generated output), then regenerate
  // from `text` via chat.writer, rebased at the prompt tick's own parent —
  // all as one server-side transaction (one undo point, the group staying
  // on screen untouched until the replacement lands, rather than
  // vanishing tick-by-tick as N separate delete.atom round trips would).
  // Same `pinned`/`lore`/`pastChaptersMode`/`pinnedPrograms` shape as
  // chat.writer — it rebases and re-runs exactly that command.
  | { type: "correct.group"; id?: string; promptTickId: string; targets: string[]; text: string; pinned?: ContextItem[]; lore?: string; pastChaptersMode?: "full" | "compressed"; pinnedPrograms?: string[] }
  // Converse: discuss, don't write. Send a message to the chat agent — see
  // WRITER.md's chat/ convention. No context/targets: a chat file has no
  // atom-selection concept of its own.
  | { type: "chat.converse"; id?: string; text: string }
  // Regenerate a chat exchange's reply, keeping the old reply as a
  // cycle-able alternate (a "swipe") instead of discarding it — unlike
  // chat.converse, the prompt tick is edited in place rather than resent
  // as a new one.
  | { type: "chat.converse.regen"; id?: string; promptTickId: string; atomTickId: string; text: string }
  // Rotate an atom's own alternates forward one step. Generic — any atom,
  // chat or prose.
  | { type: "atom.swipe.cycle"; id?: string; tickId: string }
  // Outline: split this file (a whole-story outline, outline.md by convention)
  // into per-chapter beat sheets. No prompt — the outline text is the whole
  // input; the model decides the chapter breakdown and writes each sheet.
  | { type: "chat.outline"; id?: string }
  // Summarize exactly this file — unlike the branch-level "summarize"
  // command (BranchCommand, runs a whole kind across every file that
  // qualifies), this never touches any other file, even one that's also
  // stale. See Server.Writer.File.summarizePath's own Haddock.
  | { type: "summarize.file"; id?: string }
  // Create an empty summary occurrence to write into directly — a
  // distinct intent from summarize.file (no LLM call at all), the same
  // way file.create is distinct from an LLM-populated write. Reuses
  // summarize.file's own coverage-finding and cursor-positioning
  // machinery underneath (Server.Writer.File.summarizePathManual).
  | { type: "summarize.create"; id?: string }
  // Note: instant, non-LLM, like chat.append — attaches `text` as an
  // annotation on each of `targets`, or (when empty) on the file's current
  // HEAD tick.
  | { type: "chat.note";   id?: string; text: string; targets?: string[] }
  // Reference: attach an image already sitting in the branch (dragged in
  // from the file tree) to this file's timeline by its existing asset path
  // — instant, non-LLM, no bytes travel. Contrast with uploadImage (ws.ts),
  // which PUTs fresh bytes via the $image HTTP endpoint for an OS file drop.
  | { type: "reference.image"; id?: string; asset: string; caption?: string }
  // Presence: a character (character/{id} branch) enters or leaves the
  // scene on this file — recorded as a "presence" tick scoped to this
  // file's own chain, not the whole branch (a scene is a file — see
  // WRITER.md). Wrapping in `at` (below) rebases it at a historical tick,
  // same as any other file command — no separate mechanism needed.
  | { type: "enter.scene"; id?: string; character: string }
  | { type: "leave.scene"; id?: string; character: string }
  // Ask: pose `question` to `character`, answered from only their own
  // branch (sheet, journal — not this scene, not any other character). The
  // exchange is recorded on *this* branch (the scene, not the character's —
  // asking doesn't give a character a new memory, it only reads what they
  // already know), and the answer is pushed straight back as a
  // `character.answered` event rather than relying on a ref-move
  // notification (the character's own branch didn't change).
  | { type: "ask.character"; id?: string; character: string; question: string }
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
  // The answer to an `ask.character` command, correlated by `id` (the
  // command's own id — see `withId` server-side) since a connection can
  // have more than one ask in flight.
  | { type: "character.answered"; id?: string; character: string; question: string; answer: string }
  | AgentLogEvent
  | ChatPreviewEvent
  | ErrorEvent;

// ── Character protocol ────────────────────────────────────────────────────────

// Read-only: no commands. Every field is collected-and-augmented server-side
// (see Server/Writer/Character.hs) — sheet edits go through the file
// connection for sheet.md, never through this one.
export type CharacterEvent =
  | { type: "character.update"; name: string; sheet?: string; avatar: boolean }
  | ErrorEvent;

// ── Context-view protocol ─────────────────────────────────────────────────────

// Runs a client-submitted Context DSL program (see CONTEXT-DSL.md; same
// convention as fileview.actions.ts's `fcContext` on chat.writer/
// correct.group) against the branch, staged as this one request's own
// `context.writer` override — see Storyteller.Writer.Agent.ContextPreview's
// own Haddock — and returns exactly what it resolved to. `path` is the
// program's own `path:` parameter, the same target file a real send would
// pass.
//
// `context.cost` asks instead for a per-line size breakdown (see
// Storyteller.Writer.Agent.ContextCost) — same (path, program) shape, but
// materially more expensive to answer (the whole program re-runs once per
// candidate line, via ablation), so it's its own command rather than
// riding along on every `context.preview` response.
//
// `context.cost.adhoc` is the same idea for a bare 0-arity snippet with no
// `path` of its own — what a `pinnedPrograms` entry (see
// Server/Writer/File/Protocol.hs's own `ChatWriter`) actually is, now that
// `context.writer` no longer accepts a whole-program override to estimate
// against (see lib/dslCompose.ts's own header on the writer context's
// three-slot model).
//
// Every request is self-contained — resolved fresh each time, same
// discipline an LLM call's full history follows. Nothing about a
// submitted program persists across requests server-side.
export type ContextViewCommand =
  | { type: "context.preview"; id?: string; path: string; program: string }
  | { type: "context.cost"; id?: string; path: string; program: string }
  | { type: "context.cost.adhoc"; id?: string; program: string };

// A node mirrors the DSL's own Value shape: its own text content (each
// source Message flattened to its text, in order), then named child
// entries (an `as "name": ...` export) — see
// Storyteller.Context.DSL.Rendering.RenderedContext.
export interface PreviewNode {
  content: string[];
  entries: { name: string; node: PreviewNode }[];
}

// One ablation candidate's own measured contribution — `line`/`col`
// identify the exact source statement (see
// Storyteller.Context.DSL.AST.Pos; unique per statement, so no separate id
// is needed), `chars` is the rendered-character delta removing just that
// statement produces (baseline minus with-it-ablated) — see
// Storyteller.Writer.Agent.ContextCost's own Haddock for why this is
// measured by ablation (re-running the whole program) rather than a
// static per-statement sum.
export interface LineCost {
  line: number;
  col: number;
  chars: number;
}

export type ContextViewEvent =
  | { type: "context.preview"; id?: string; result: PreviewNode }
  | { type: "context.cost"; id?: string; costs: LineCost[] }
  | ErrorEvent;

// ── Library protocol ──────────────────────────────────────────────────────────

// One node in the branch's organizational tree (see WS-PROTOCOL.md's
// /library/{name} and Storyteller.Writer.Library). 'kind' is server-detected
// by a marker-word heuristic (story/book/chapter/scene, singular or plural,
// or "ch", appearing anywhere in the path — see WRITER.md), not a fixed
// folder name or depth; "other" is not an error, just a path with no marker
// word anywhere on it (still shown, just unrecognized). 'heading' is a
// chapter's raw first line, not a parsed/validated H1 — same "server hands
// over raw text, client decides" contract as CharacterSummary.sheet, just
// narrowed to one line so a tree covering many chapters stays cheap to push.
// Mirrored (not shared) by 'classifyPath' in lib/library.ts, for the one UI
// spot (the Explorer tab) that classifies a raw path list without a
// /library round trip — see that module's own header.
export interface LibraryNode {
  path: string;
  name: string;
  kind: "folder" | "unit" | "unit-outline" | "other";
  heading?: string;
  // True when this path has no atom history at all (an uploaded binary
  // asset, or anything else that opted out of atom tracking — see
  // Storage.Ops.hasAnyAtom). Never open the prose/atom file-viewer
  // (openFile in fileview.actions.ts) for one of these — there's no tick
  // chain for it to show, and writing to it would just glue text onto
  // whatever binary content is actually there.
  binary?: boolean;
  children: LibraryNode[];
}

// One recognized prose unit, already paired with its own beat sheet if any
// (see Storyteller.Writer.Library.narrativeUnits) — either the chapter
// file, the beat sheet, or both existing already means the chapter exists
// as a concept, which is a real domain fact, not a display grouping this
// client should reconstruct itself (the Summarizer agent needs the
// identical answer). `path`/`outlinePath` absent means that artifact
// doesn't exist yet. No `number` — ordering is purely the position in this
// already-ordered list; a unit's own heading isn't repeated here either,
// look it up on the matching LibraryNode in `nodes` by `path`.
export interface ChapterUnit {
  path?: string;
  outlinePath?: string;
}

// The only mutation here: introduce `path` as a new chapter file, seeded
// with "# {name}" as its first line (same convention sheet.md uses for a
// character's display name). Distinct from file.create, which has no
// heading convention to seed. Doesn't require `path` to match
// chapters/ch{N}.md — detection is freeform, so a non-matching path is
// still created, just shown as "other" rather than "chapter".
export type LibraryCommand =
  | { type: "chapter.create"; path: string; name: string };

export type LibraryEvent =
  | { type: "library.tree"; nodes: LibraryNode[]; chapters: ChapterUnit[] }
  | ErrorEvent;

// ── Lore protocol ─────────────────────────────────────────────────────────────

// One node in the branch's codex tree (see WS-PROTOCOL.md's /lore/{name} and
// Storyteller.Writer.Lore). A leaf here is already known to be codex-eligible
// content (server excludes chapters/outlines/chat/binaries — see
// Storyteller.Writer.Lore.isLoreEligible), so unlike LibraryNode there's no
// 'kind' to branch on client-side. 'blurb' is a file's raw first non-blank
// line, same "server hands over raw text, client decides" contract as
// LibraryNode's 'heading'. 'aliases' is parsed server-side from a plain
// "**Aliases:** a, b, c" markdown line in the file's first section (see
// Storyteller.Writer.Lore.parseAliases) — empty when the file declares none.
export interface LoreNode {
  path: string;
  name: string;
  blurb: string;
  aliases: string[];
  children: LoreNode[];
}

// Read-only connection — no commands.
export type LoreEvent =
  | { type: "lore.tree"; nodes: LoreNode[] }
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
  // "file/" is a fixed, reserved segment (see app/Server.hs's wsRouter) —
  // real file paths always sit under it, never bare at the branch's own
  // root, so a path named "$context" can't be mistaken for that connection.
  return new StoryWS<FileCommand, FileEvent>(`${wsBase()}/branch/${encodeURIComponent(branch)}/file/${encodedPath}`);
}

// No commands, so 'Cmd' is 'never' — nothing can be sent on this connection.
export function characterConn(branch: string) {
  return new StoryWS<never, CharacterEvent>(`${wsBase()}/character/${encodeURIComponent(branch)}`);
}

export function libraryConn(name: string) {
  return new StoryWS<LibraryCommand, LibraryEvent>(`${wsBase()}/library/${encodeURIComponent(name)}`);
}

// No commands, so 'Cmd' is 'never' — nothing can be sent on this connection.
export function loreConn(name: string) {
  return new StoryWS<never, LoreEvent>(`${wsBase()}/lore/${encodeURIComponent(name)}`);
}

// Stateless: send a full slot list whenever the filter changes, get a full
// preview back. No presence/absence handshake — just request/response, plus
// unsolicited re-pushes if the branch's files change under an already-sent
// filter.
export function contextViewConn(branch: string, path: string) {
  const encodedPath = path.split("/").map((p) => encodeURIComponent(decodeURIComponent(p))).join("/");
  return new StoryWS<ContextViewCommand, ContextViewEvent>(
    `${wsBase()}/branch/${encodeURIComponent(branch)}/$context/${encodedPath}`
  );
}
