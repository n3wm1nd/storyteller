// WebSocket connection abstractions for the storyteller server.
//
// Six connection types mirror the server's six endpoints:
//   sessionConn    (/session)                        — branch management
//   branchConn     (/branch/{name})                  — full branch tick chain + file tree
//   fileConn       (/branch/{name}/{path})            — file-scoped tick chain
//   characterConn  (/character/{name})                — sidebar-facing character state (read-only)
//   contextViewConn (/branch/{name}/$context/{path})  — stateless context-filter preview
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
  | { type: "delete.atom"; id?: string; tickId: string }
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
  // whether atoms generated since then are still provisional. `contextLayout`
  // is the user-configured bucket-picker ordering for this call's ambient
  // context (see PickerRule below); omitted/empty falls back to the
  // server's default alphabetical order. `characterLayouts` is the same
  // picker model, one entry per active character branch that's actually
  // been curated (branch name -> layout) — a branch absent here means "no
  // override", which the server reads as today's fixed sheet-in/journal-out
  // behavior, not "show nothing" (see Server.Writer.File.activeCharacterContext).
  | { type: "chat.writer"; id?: string; text: string; context?: ContextItem[]; contextLayout?: PickerRule[]; flowTid?: string; characterLayouts?: Record<string, PickerRule[]> }
  // Fixer: `targets` are the atoms flagged as the subject of `text`.
  | { type: "chat.fixer";  id?: string; text: string; context?: ContextItem[]; targets?: string[] }
  // Regen: rewrite this chapter to fit its beat sheet (ch{N}.outline.md by
  // convention), respecting `text` as the user's steer. A reconciliation, not
  // a wipe — unchanged prose keeps its atoms. `byBeat` selects the
  // beat-by-beat driver over the whole-chapter one.
  | { type: "chat.regen";  id?: string; text: string; context?: ContextItem[]; byBeat?: boolean }
  // Correct: delete `promptTickId` and every atom in `targets` (an
  // instruction group's own prompt + generated output), then regenerate
  // from `text` via chat.writer, rebased at the prompt tick's own parent —
  // all as one server-side transaction (one undo point, the group staying
  // on screen untouched until the replacement lands, rather than
  // vanishing tick-by-tick as N separate delete.atom round trips would).
  | { type: "correct.group"; id?: string; promptTickId: string; targets: string[]; text: string; context?: ContextItem[]; contextLayout?: PickerRule[]; characterLayouts?: Record<string, PickerRule[]> }
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

// How a slot is delivered to whichever agent/subagent consumes it — fixed by
// the command that declares the slot (e.g. Write always injects character
// context ambiently; Chat always exposes branch files as on-demand tool
// reads). Not something the client picks per file.
export type ContextMode = "ambient" | "on-demand";

// One claim in a ContextLayout: every path matching `pattern` that no
// earlier rule in the list already claimed is assigned `bucket`. Glob
// syntax, same as the server's own file glob op. `bucket` omitted (or null)
// means an explicit trash claim — hide this path even if a later, broader
// rule would also match it (see Storyteller.Writer.Agent.ContextFilter).
// Claim order (list position) and bucket order (the number itself) are
// independent axes: a narrow pattern needs to claim ahead of a broad
// catch-all regardless of which bucket either targets.
export interface PickerRule {
  pattern: string;
  bucket?: number;
}

// An ordered picker list. Empty means "no layout configured" — falls back
// to the server's default alphabetical order, not "claim nothing" (that's
// what an empty layout means to buildSlotPreview/applyContextLayout
// directly — see their own docs for why the two differ).
export type ContextLayout = PickerRule[];

// One named context slot: a label the command chose (e.g.
// "character:alice-chen", "branch-files"), its fixed mode, and the layout
// selecting and ordering its files — the one part the client configures.
export interface ContextSlot {
  label: string;
  mode: ContextMode;
  layout?: ContextLayout;
}

export interface ContextEntry {
  path: string;
  bucket: number | null;  // null for a file no rule claims — still listed, just shaded, not hidden
  content?: string;       // full text, only present for claimed Ambient entries
  blurb?: string;         // short teaser, only present for claimed OnDemand entries
}

export interface ContextSlotPreview {
  label: string;
  mode: ContextMode;
  entries: ContextEntry[];
}

// Every request is self-contained — the full slot list, resolved fresh each
// time, same discipline an LLM call's full history follows. Nothing about a
// submitted filter persists across requests server-side.
export type ContextViewCommand =
  | { type: "context.preview"; id?: string; slots: ContextSlot[] };

export type ContextViewEvent =
  | { type: "context.preview"; id?: string; slots: ContextSlotPreview[] }
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
// LibraryNode's 'heading'.
export interface LoreNode {
  path: string;
  name: string;
  blurb: string;
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
  return new StoryWS<FileCommand, FileEvent>(`${wsBase()}/branch/${encodeURIComponent(branch)}/${encodedPath}`);
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
