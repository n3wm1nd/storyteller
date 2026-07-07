# Writer app conventions

`Storyteller.Core`/`Server.Core` are a generic tick-chain storage system with
no domain vocabulary — see DATA-MODEL.md and WS-PROTOCOL.md. The Writer app
(`Storyteller.Writer`/`Server.Writer`, see STRUCTURE.md) builds actual
writing-tool concepts — chapters, characters, scenes — on top of that generic
substrate by agreeing on naming and structure conventions the type system
does not enforce.

This file is the one place both frontend and backend can check for those
conventions instead of relying on them being independently reimplemented (and
silently drifting) on each side. **None of this is load-bearing at the
storage layer** — a branch that doesn't match these conventions is still a
perfectly valid branch, it just won't be picked up by Writer-specific
features (chapter view, the character sidebar, etc). Expect this file to
change as the app grows; it documents current convention, not a schema
anyone is validating against.

---

## Branch naming

- `story/{storythread}` — a story branch.
- `character/{characterid}` — a character (or, generally, entity — group,
  place, object) branch. See DATA-MODEL.md for why entity branches are
  partial-view, not narrative.

The prefix is how server-side code decides how to interpret a branch (e.g.
whether it's eligible for the character list/sidebar) and how the frontend
decides how to render it. Nothing currently enforces the prefix at branch
creation — a branch named without one is just not picked up by anything
Writer-specific.

## Story structure

- `chapters/ch{N}.md` — one file per chapter, narrative order. The first
  Markdown H1 (`# `) line is the chapter's **display name** — same convention
  `sheet.md` uses for a character's display name (see "Character structure"
  below), and the same "server hands over raw text, client decides" contract:
  the `/library/{name}` connection's `chapter.create` command writes this
  line, but reading it back into a display name is a client-side concern
  (WS-PROTOCOL.md's read-side principle), not something the server parses.
- Surrounding folder structure is otherwise **freeform, not prescribed** —
  `chapters/ch1.md` and `series/epic/book3/act1/chapters/ch1.md` are both
  recognized identically, since detection only ever looks at a path's own
  basename and immediate parent directory name
  (`Storyteller.Writer.Library.classifyPath`, see WS-PROTOCOL.md's
  `/library/{name}`). Nothing requires a `chapters/` folder to exist at any
  particular depth, or to exist at all — a story with no chapter files just
  has an empty chapter list, same as any other convention in this document.

## Outlines and beat sheets

Two levels of planning artifact, both **ordinary markdown files on the story
branch** — no special tick kind, no parser contract, nothing the storage
layer knows about. They are context that happens to be written before the
prose, which the append-only model already makes safe: a beat can only be
referenced by prose that comes after it, so "outline before prose" holds by
construction (DATA-MODEL.md, monotonic-reference invariant) without anything
enforcing it.

- `outline.md` — the whole-story plan. One markdown heading per chapter (or
  arc), with prose notes underneath: what happens, why it matters, where it
  sits in the larger shape. Coarse.

- `chapters/ch{N}.outline.md` — the **beat sheet** for one chapter, expanded
  from the relevant slice of `outline.md`. One heading per beat, and under
  each, prose covering what happens, the logistics (who's where, what has to
  be true), the emotional turn, and a rough length. This is the reviewable
  middle rung: coarser than prose, finer than the story outline. It is
  **disposable scaffolding** — edit it to steer a chapter, or delete it once
  the prose exists and you no longer care. Nothing holds a hard reference to
  it, so deleting it breaks nothing (it's a forward `Replace`, still
  reachable via `At`).

**Not a schema.** These are "sensible markdown a person would write," used
consistently so the model produces them reliably and a human can skim them —
not fields anything greps for. The generation pipeline reads them as free
text; it never parses them into a rigid structure. A file that doesn't
follow the convention is still a valid file, it just won't read as cleanly
to the model or the author.

**The pipeline** (see `Storyteller.Writer.Agent.Outline` and the
`story-outline`/`story-chapter` tools) is one `expand` step applied per
level — `outline.md` → `ch{N}.outline.md` → `chapters/ch{N}.md` — three
levels, fixed for now. A fourth (act) tier is the same `expand` applied once
more, added only if a story needs it. Prose generation from a beat sheet
comes in two variants under trial (whole-chapter in one call vs. an
LLM-driven beat-by-beat loop); both read the same free-form beat sheet and
share the same prose core, so any quality difference is attributable to
chunking strategy alone.

**Back-references (extra credit, not yet wired).** Because chapters live in
separate files, the beat-sheet→prose correspondence is already encoded
positionally (`ch{N}.outline.md` ↔ `chapters/ch{N}.md`). A finer, per-beat
link — a prose atom carrying a `tickRefs` back to the beat atom it realizes,
enabling a `follow`-based "which beats have no prose yet" coverage query — is
a deferred optional step, not needed for either prose variant to work.

## Character structure

- `sheet.md` — current-state description (mood, traits, whatever a sheet
  ends up holding). Read directly as a file for full history; composed into
  the `character/{charBranch}` connection payload for the sidebar (see
  WS-PROTOCOL.md). The first Markdown H1 (`# `) line in `sheet.md` is the
  character's **display name** — what the UI shows for them (sidebar,
  presence markers, etc). This may be a nickname, may differ from the full
  name used in prose, and may differ from the `character/{characterid}`
  branch id (which often can't hold the real name verbatim — spaces, special
  characters, renames). Falls back to the branch id if no H1 is present.
- `journal.md` — the character's own account, in fiction-time order (not
  necessarily story order — flashbacks etc). Read directly via a normal file
  connection; no special connection needed just to read it.

## Scene presence

No dedicated `Scene` entity — **a scene is a file.** Presence is scoped per
file, not to the whole branch: a fresh file implicitly starts with nobody in
it. Writing a scratch/mid-chapter scene in a separate file must not inherit
whoever happened to be present in the last file worked on — files are
independent chains for most practical purposes (see DATA-MODEL.md's
append-only model; a file's own tick chain is a *projection* of the branch
chain, not a contiguous walk — `walkFileTicks`, Storyteller.Core.Git, is
what makes that projection self-contained, see below).

A file's tick chain carries presence markers — "character X is here" /
"character X leaves" — as a tick kind alongside atoms and notes. "Who's
active" at any point in that file is derived by folding presence ticks from
root to a given tick, not stored separately. A scene typically opens with a
cluster of "is here" ticks establishing the starting cast.

Implemented: the `presence` tick kind (`Storyteller.Writer.Types.Presence`,
`Storyteller.Writer.Presence.recordPresence`) and the `enter.scene`/
`leave.scene` commands on `/branch/{name}/{path}` (the file connection —
**not** `/branch/{name}`; see WS-PROTOCOL.md and
`Server.Writer.File.Protocol`). Because it's a `FileCommand`, presence gets
the existing `at` rebase wrapper for free — no special-casing needed to add
a character to a scene at a historical point.

`character` is stored as a `character/{id}` branch-name field, not a tick
ref — no rebase fixup needed since it isn't a reference into this branch's
own chain. `file` is stored the same way `Prompt` already stores its file
association (a plain `"file"` field `walkFileTicks` matches against) — a
hint, not a hard reference; expect this mechanism to change shape later
(it makes file renaming awkward), and presence to move with it.
`walkFileTicks` relinks each returned tick's parent to the nearest tick
still in that file's projection, so a tick like `presence` that carries a
file hint but no atom content doesn't leave a gap a client's chain walk can
fall out of (this was a real bug, fixed — see `test/Server/FileSpec.hs`'s
"unrelated tick" regression test).

The **branch** connection also has a generic `at` wrapper on `BranchCommand`
(same shape, for `Track`/`CharGen`/`AddNote`/`MoveTick`/`DeleteTick`) — no
client trigger uses it yet, but it'd be the mechanism behind a possible
future Ticks-view rebase marker, the branch-level equivalent of the file
view's drag handle.

## Chat files

- `chat/*.md` — a file whose purpose is conversation, not prose (e.g.
  `chat/conceptart.md` for brainstorming with the LLM about the story rather
  than adding to it). Structurally this is nothing new: the same
  `Prompt`/`Atom` tick pair every other file already produces when a message
  is sent (`Prompt` for the user's message, `Atom` for the response) — a
  chat file is just a file where that's the *whole* content, read back as a
  transcript instead of prose. `historyFromFileTicks`
  (`Storyteller.Writer.Agent.Chat`) is what turns that tick chain into an
  LLM message history; `chatConverse`/`chat.converse` is the command that
  drives it, distinct from `chatWriter`/`chat.writer` only in agent persona
  (discuss vs. continue prose) and in appending exactly one atom per turn
  rather than splitting into paragraphs.

  No context is gathered up front — by default the chat agent sees only the
  conversation itself. It can find and read files on the current branch via
  tool calls (`glob`/`read_file`/`sed_print` for a line range out of a long
  file, all reused directly from `Runix.Tools` rather than reimplemented —
  `grep`/`diff` aren't, both shell out and need a real filesystem path a
  git branch doesn't have), the same bind-a-real-effect-behind-a-tool
  pattern `ReplaceTool` uses, and the same query-then-loop-on-tool-calls
  shape as `runix-code`'s agent loop. This is also why the system prompt is
  fully static (no per-call content spliced in) and history only ever grows
  by appending whole turns — both matter for prompt-cache hit rate, not
  just correctness (see the module header on `Storyteller.Writer.Agent.Chat`
  for the full reasoning). Tool-call exploration within one turn isn't
  persisted as ticks — only the final reply is — so a later turn doesn't
  inherit an earlier turn's file reads; the model just asks again.

  Making `glob` actually work against a branch required filling in a real
  gap: `Storyteller.Core.Git`'s git-branch `FileSystem` interpreter had
  `Glob` stubbed as "not yet implemented" (nothing before this needed it —
  `gatherFileContext` always used the recursive `listAllFiles`/
  `listAllFilesS` instead). It's now backed by
  `Storyteller.Core.StorageMonad.listAllFilesS` plus `System.FilePath.Glob`
  for in-memory pattern matching, since there's no real directory to shell
  out to the way the on-disk `FileSystem` interpreter does.

  On the frontend, a `chat/` path gets an additional "Chat" tab next to
  "File"/"Ticks" (`page.tsx`'s `centerTab`, `chatview.tsx`) rendering the
  same tick chain as bubbles — an alternative view, not a replacement; the
  "File" tab still shows the same exchange as ordinary prompt/atom ticks.
  Like every other convention here, this is a prefix check
  (`isChatFile`), not something the storage layer enforces — a `chat/` file
  that doesn't get treated this way for some reason is still a perfectly
  valid file, and any other file could be made to render this way later by
  the same mechanism.

## File extensions

Default is `.md` unless a file is explicitly created with another extension.
Not yet decided whether the UI hides/auto-adds the extension.

---

## Open questions / not decided yet

- Whether any of the above gets validated/enforced anywhere, or stays purely
  conventional.
- `characters` (plural) session-scoped connection: a filtered, augmented
  branch list for an add-to-scene picker. Deferred — see WS-PROTOCOL.md's
  connection scoping principles for how it would be shaped when built.

## Implemented so far

- `presence` tick kind + `enter.scene`/`leave.scene` on
  `/branch/{name}/{path}` (file-scoped — see "Scene presence" above).
- `/character/{charBranch}` connection (`Server.Writer.Character*`) —
  read-only, pushes `{ name, sheet }` on connect and on every change to the
  character branch. Branch names containing `/` must be percent-encoded in
  the URL path (`character%2Falice`).
- Frontend: right-hand character sidebar (scene membership for the open
  file, add/remove, sheet preview), left-sidebar "Characters" tab (filtered
  branch list, hover-to-highlight), presence bars in the file view (per-run
  colored lines next to the selection bar; toolbar toggle for "show all
  characters" vs. hover-only), all reading/writing the open file's own
  chain, not the branch-wide one.
- Chat files (`chat/*.md`, see "Chat files" above): `chatAgent` +
  `historyFromFileTicks` (`Storyteller.Writer.Agent.Chat`), the
  `chat.converse` command (`Server.Writer.File.chatConverse`), and the
  frontend's "Chat" tab (`chatview.tsx`) with per-turn regenerate (only on
  the latest exchange, since regenerate deletes and re-appends).
