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

- `chapters/ch{N}.md` — one file per chapter, narrative order.

## Character structure

- `sheet.md` — current-state description (mood, traits, whatever a sheet
  ends up holding). Read directly as a file for full history; composed into
  the `character/{charBranch}` connection payload for the sidebar (see
  WS-PROTOCOL.md).
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
