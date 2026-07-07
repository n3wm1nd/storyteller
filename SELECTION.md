# Global selection

A single, client-side, connection-agnostic notion of "what the user is
currently pointing at" — the target/context that targeted commands (delete,
append, write, `chat.*`, ...) act on, instead of each command inventing its
own ad-hoc target argument. This is frontend-only state (see WS-PROTOCOL.md's
"client never mutates synced data locally" — selection is exactly the kind of
volatile, display-only exception that rule already carves out) that shapes
what gets sent *with* a command, not something the server tracks.

**Status: not implemented.** This file records the design intent and the
decisions already made, ahead of writing the code. See STRUCTURE.md /
CLAUDE.md for where the frontend store and WS layer live.

---

## Why this doesn't exist yet

Selection today is fragmented and file-scoped, not global:

- `contextAtoms` / `contextAnnotations: Set<string>` (`frontend/src/lib/store.ts:104-105`)
  — tick ids selected for LLM context. Connection-agnostic in the sense that a
  journal atom id and a scene atom id can coexist in the same set, but not
  namespaced per branch.
- `rebaseMarker: string | null` (`store.ts:107-110`) — a single time-position
  pointer (the file view's drag handle), scoped to the currently open file,
  cleared on file/branch change.
- `selectedFile` — which file is open — is local `page.tsx` React state, not
  in the store at all.
- `activeBranch` (`store.ts:58`) is the *only* branch the store has any
  concept of. There is no multi-branch layout/position state.

Commands take explicit target arguments today (`editAtom(path, tickId,
content)`, `deleteAtom(path, tickId)`, `chatWrite(path, text)` —
`store.ts:745-772`) and are sent on that file's own WS connection
(`get().openFiles[path]`). Nothing routes through a shared selection concept.

The WS command shape (`FileCommand`, `frontend/src/lib/ws.ts:100-126`) already
carries `context?: ContextItem[]` and `targets?: string[]` on some commands,
but no field carries *branch* — branch is implicit in which connection
(`/branch/{name}/{path}`) a command rides on. There is no references field and
no multi-branch addressing anywhere yet.

`selectBranch` (`store.ts:433-497`) tears down every connection and resets all
selection/context/rebase state on switch. Per the decision below, this is
correct behavior, not a gap — see "Lifetime" below.

---

## Model

**Selected objects.** Each selected object is tagged with the branch it lives
in (new — no existing type pairs a reference/target id with a branch today).
Selection is a set of these, held in the global store, replacing the
per-command ad-hoc targets above.

Branch tagging only matters for *cross-branch* references. When a command is
sent over a file (or branch) connection, that connection already implies its
own branch — tick ids passed as arguments to that command don't need to
restate it. The branch tag on a selected object is only needed when the
object lives in a branch other than the one the command is being sent on
(e.g. a character reference pulled into a scene command running on a story
branch).

**Targeting.** Targeted commands (delete, append, write, ...) read their
target from the current selection instead of taking it as a bespoke argument.
A selection can span objects across branches, but how a given command
resolves that into what actually gets sent is decided per command — see
"Per-command target resolution" below, rather than one universal fan-out
rule.

**Fixed-target inputs are an exception.** Commands issued from the main
input bar always target the main window, regardless of what else is
currently selected elsewhere in the UI. The same pattern applies anywhere
else an input has one fixed, implicit target — e.g. the append input in the
character journal always targets that journal, never the current selection.

**Wire shape.** The full context is sent with a command both as text and as
references — each reference carries the branch it came from (when that
branch differs from the connection's own), extending the existing
`context`/`targets` fields on `FileCommand` (ws.ts:100-126) with branch tags
rather than replacing them. It's fine to wire up sending references from the
frontend before the backend consumes them — an unread field is harmless, and
it decouples the two halves of the work.

**`At`'s position field.** `At` has its own selection mechanism, the
draggable bar (the file view's rebase marker — see `rebaseMarker` above, and
WS-PROTOCOL.md's file-connection `at` wrapper). A branch's "position" for
this purpose is exactly what the rebase bar already models: a point *between
two ticks* in that branch's chain — not scroll offset or viewport state.

The rebase bar itself is file-scoped, but the sidebar's open/active
characters for that file are already linked to it. What's still needed is
sending *their* branch positions too, alongside the file's own, so `At` never
silently runs a command as of a point in the past on the file while a
character present via a presence tick has already moved on further in their
own branch.

Mechanically, other branches' positions ride along as additional arguments
on the `at`-wrapped call, populated when relevant to the command being run.
`At Append` (e.g. the journal append input) only ever acts on one branch, so
it can receive this extra information and simply not use it — no
per-command opt-out mechanism needed.

**Rebase-bar stability heuristic.** With plain appends and simple edits the
bar can just move along with the chain. But since later commands can create,
edit, delete, or merge ticks, exact positional stability can't be guaranteed
in general. Working heuristic: ticks *after* the rebase bar should not
change except for their tick ids and references — later history may get
relinked/renumbered around a change, but its content and order stay put.
Not a hard invariant yet, just the sensible default to implement against.

**Lifetime.** Selection is deliberately volatile: it is the target of the
*next* command only, not a persisted mode. Switching branch or file is
understood as giving up the current selection — no restore-on-return, no
cross-navigation persistence. This matches `selectBranch`'s existing
full-reset behavior, which needs no change on this account.

---

## Per-command target resolution

There is no single universal rule for how a command consumes the selection —
this is decided per command, case by case. Guiding principle: stay local to
the connection whenever the command's semantics allow it. E.g. `delete`
operates on a file, so every tick id it's given must belong to that file's
own chain — resolving the selection for `delete` means filtering to the
current file's ticks, not shipping arbitrary cross-file/cross-branch ids to
a single-file connection. Other commands (append, write, `chat.*` variants)
will need their own resolution rule worked out individually as they're wired
up to the selection, rather than trying to anticipate all of them up front.

---

## Open questions

- Exact shape of the branch-tagged reference type (a TS type pairing a
  reference id/kind with a branch name, used both for cross-branch selection
  references and for `At`'s extra per-branch position arguments) — not yet
  designed, though its role is now clear.
- Whether the existing character-sidebar linkage is the only source `At`
  needs for "other relevant branch positions," or whether a future
  multi-branch view would need its own registry of open branches beyond
  that.
- Per-command selection-resolution rules beyond `delete` (append, write,
  `chat.*` variants) still need to be worked out individually as each is
  wired up — see "Per-command target resolution" above.
