# Storyteller: Data Model

## Core Principle

**Story branches are the product. Entity branches are partial views. The gap between them is where dramatic irony, unreliable narrators, and distinct character voices live.**

Prose carries craft. Entity branches carry perspective. These are separate concerns given separate homes.

---

## The Append-Only Invariant

Every file in every branch is append-only: each tick may only extend a file beyond its previous content. New files and directories are permitted; deletions and in-place modifications are not.

**Why this constraint exists.** Git commits are snapshots, not diffs. When git merges two branches, it infers diffs between snapshots and hopes they don't collide — and when they do, a human has to resolve it. The fundamental problem is that arbitrary file changes are not composable: two changes to the same region of the same file can be irreconcilable.

An atom eliminates this problem by making the diff the primitive. An atom *is* "append X to file F" — a complete, self-describing, trivially conflict-free operation. Two atoms touching the same file always compose: you append both, in order. There is nothing to infer and nothing to conflict. Rebase is not an operation that might succeed — it is an operation that always succeeds, because replaying a sequence of append operations onto a new base is just executing them again.

**Why it isn't a real limitation.** This is not source code. Prose is written linearly — from the first word to the last — and read linearly. The mental model of time matches: at t=0 the file is empty, at t=50 it's halfway done, at t=100 it's finished. Whatever was there at t=50 is still there at t=100; the file just got longer. The append-only constraint encodes exactly how writing already works. The constraint only bites when you want to fix something earlier — and for that, rebase exists.

**What "changing" something means under this invariant:**

- **Append** — add new content at HEAD. The character learns something, a constraint is added, a scene continues. The common case.
- **Amend** — `At` back to the tick where the content was written, write it differently, rebase forward. From HEAD, it was always that way. The old version exists only in the temporal ledger.
- **Replace a file** — write a new file with the same name at HEAD. This is a forward append operation: a new file appears, the old one is no longer in the active tree but remains reachable via `At`. Categorically different from amending: replacement is a forward move, amendment is a rewrite of history.

**Reticking** — atom boundaries can be changed after the fact. Splitting one tick into two requires choosing which inherits the original ID (preserving existing references) and which gets a new one (propagated via `updateReferences`). Merging two ticks into one is the reverse. Because rebase is already guaranteed to succeed, reticking is just another rebase — the storage layer imposes no additional cost.

---

## The Structures

Storyteller uses three distinct structures, each answering a different question:

**Branches** (named chains of ticks)
: What is written, and in what order is it meant to be read?
: Each branch is an ordered chain of ticks. The ordering convention depends on the branch kind — narrative order for story branches, fiction-time for entity branches.

**Temporal ledger** (per-branch audit trail)
: What states has this branch ever had?
: Append-only, mechanical, maintained automatically as a side effect of any write. Never reasoned about directly.

**Storage tree** (author workspace)
: What ideas exist?
: Wallclock-ordered. Notes, outlines, sketches — not part of the product.

A project may contain multiple story branches (a shared universe with several works) and any number of entity branches. There is no mandatory "world ground truth" branch — story branches are the product, and what any entity knows is tracked in their own branch.

---

## Ticks

A **tick** is the smallest unit of chain advancement. Every time the branch moves forward by one step, a tick is added. Ticks are what the storage layer knows about — the application layer interprets what each tick *means*.

Every tick has:
- A **message** — free text, interpreted by the application layer to determine tick kind
- A **parent** — the previous tick in the chain (`Nothing` for root ticks and helper ticks)
- **References** — cross-branch links (e.g. a story tick referenced by an entity tick that records the same event)

The filesystem at any point in the chain is the product of replaying all ticks from the beginning. The tick itself carries no file content — it records that the chain advanced, and the message says why.

A tick that adds content to files is called an **atom** — the finest granularity at which content is addressable. Atoms can be anything from a phrase to a complete generation block; the choice determines how precisely history can be navigated and how finely other ticks (summaries, annotations, cross-branch references) can point into it. Ticks that carry no filesystem change are not atoms — they sit *between* atoms in the chain, logically anchored to the atom that immediately precedes them.

### Tick ID Stability

Tick IDs are content-addressed object hashes — like git commit hashes, they identify a specific object in the store permanently. A tick ID is always valid: the object it points to exists and its content never changes.

However, a tick ID is not a stable reference to a position in a branch. After a rebase, the same logical position in the chain has a new tick ID — the content of the diff may be identical, but the object is new because its parent changed. The old tick ID still resolves to the original object, which is no longer on the active chain.

The practical consequence: tick IDs are safe to use within a tight fetch-work-use cycle, or for historical reference ("this specific object"). They should not be stored as durable handles to "the current third atom of chapter 2" — that position's ID will change whenever anything before it is rebased.

### Invariants

Two invariants the storage layer enforces:

1. **Monotonic references** — following any combination of parent and ref links must eventually terminate. There are no cycles, and no tick may reference a tick that comes after it in the same chain. References to other chains or orphan objects are unrestricted. This is what makes chain position meaningful: within a chain, anything a tick references is guaranteed to precede it. Unlike git, where content-addressing enforces this for free, here `updateReferences` mutates the graph in place after a rebase — so the monotonic invariant must be explicitly checked on every `updateReferences` call, verifying that each redirected ref still precedes its referencing tick in its chain.
2. **All tick references must be declared** — every reference to another tick must appear in `tickParent` or `tickRefs`. Tick IDs must never be embedded in the message or anywhere else. This is what makes rebase fixups complete and mechanical.

Helper ticks (parentless ticks used purely as reference containers, e.g. to store a variable-length list of references that a single tick can then point to via `tickRefs`) are valid and follow the same invariants.

### Tick Kinds

The storage layer is agnostic to tick kind. The application layer tags the message and interprets accordingly:

**The general pattern:** any agent or system component can insert a tick that carries no filesystem change. Such a tick is logically *at a position in the chain* — it sits between specific atoms of content — without being *part of the content*. Its position is its primary meaning; its message and refs carry whatever additional data the inserting agent needs. This is how summaries, annotations, consistency flags, task completions, and anything else agents want to attach to a specific point in the story are all expressed through the same mechanism. The file, read plainly, is unaffected; the chain, traversed by an agent, sees everything.

**Prose tick** (atom) — the common case. The filesystem gained one more unit of content. The message is a brief summary of what was written, or the content itself verbatim. Everything that depends on position (cross-branch references, summary boundaries, `At` targets, knowledge filter granularity) bottoms out at the atom. A practical atom is somewhere between a sentence and a ~200-word LLM response block: fine enough to pinpoint where a piece of knowledge entered the story, coarse enough that history stays navigable. Atom granularity is a **policy decision in application code**, not a storage constraint — it can be configured per workflow and changed after the fact via reticking.

**Summary tick** — the filesystem did not change. The message summarizes a range of prior ticks. References point to the first and last tick of the summarized range. These are free navigation artifacts: "what happened in act 2?" = read the act 2 summary tick message, no files needed.

**Annotation tick** — metadata added in-stream: author notes, consistency flags, task completions. Filesystem unchanged. Invisible on a plain file read; present in the chain for agents that care.

**Swipe tick** — records that an alternative was generated but not chosen. An empty merge tick with references pointing to the alternative tick(s). The rejected alternatives exist in the chain for potential later use; the filesystem reflects the chosen version.

New tick kinds can be added without changing the storage layer. Anything that doesn't match a known tag defaults to a prose tick.

---

## Branch Orderings

There are exactly two orderings an author or agent ever reasons about:

**Narrative sequence** (story branches)
: Each tick presupposes the ones before it in reading order. A flashback in chapter 3 depicting events from before chapter 1 sits *after* the chapter 1 ticks — because the reader understanding that flashback presupposes they've reached chapter 3. The chain encodes reading-order dependency, not fiction-time.

**Fiction-time** (entity branches)
: Each tick presupposes the ones before it in the entity's lived experience. If chapter 3 contains a flashback to Alice's childhood, that event appears *early* in Alice's branch — where it happened to her — not at the chapter 3 position. Her branch reads as her life, in the order she lived it.

The storage layer enforces neither. These are conventions the application layer and authors uphold.

---

## Story Branches

A story branch is a chain of ticks in narrative order. Reading the story means reading the filesystem at head. The tick chain records how it got there.

Editing a tick is not a new tick — it amends the existing one and replays what follows. The story branch always reflects the story as it should be read, not the sequence in which it was written. The authoring history (drafts, regenerations, the path taken) lives in the temporal ledger, not in the story branch.

Summary ticks make the chain hierarchically navigable without loading files. An agent reading context for a scene can read a chapter summary tick, find it sufficient, and never touch the filesystem.

---

## Entity Branches

An entity branch is a chain of ticks in fiction-time order — what this entity knows, believes, or remembers, in the order they experienced it. "Entity" most commonly maps to characters, but also institutions, and occasionally locations or objects.

Entity branches are **not** readable narratives. Clarity is the only virtue that matters. They are allowed to be blunt, repetitive, and full of unnecessary detail.

### The File Tree

The filesystem within an entity branch is author-controlled. A deep character might have:

```
alice/
  sheet.md         — name, appearance, voice, core traits (always loaded)
  active.md        — current case state, live suspicions
  bob.md           — everything she knows/suspects about Bob
  biography/
    early.md       — formative experiences
    cases.md       — prior work that shaped her instincts
```

A minor character might have:
```
fence/
  sheet.md         — description, motivations, voice
  biography.md     — the three scenes he appeared in, in order
```

The system scans and adapts to whatever structure the author uses.

### Tick Kinds in Entity Branches

Entity branches use the same tick kinds. Prose ticks record what the entity experienced. Annotation ticks carry authorial corrections ("Tony wouldn't say that — add this constraint at this fiction-time position"). Summary ticks compress biography spans for efficient navigation.

Authorial corrections are scoped by fiction-time position. A constraint added because of a failure in chapter 4 sits at the chapter 4 position in Tony's branch, shaping everything after it. When Tony exits context, his branch isn't loaded. When he returns, the constraint is exactly where it belongs.

### Branches as Current Understanding

An entity branch represents the entity's history **as they currently understand it**:

- Ticks can be revised when understanding changes
- Ticks can be removed by amending back to where they were introduced and not writing them
- False beliefs sit in the branch as fact, from the entity's perspective
- Inferences and working theories are recorded, even if wrong

A corrected misremembering: go back, amend the tick, replay forward. The old version exists in the temporal ledger. No corrective tick appended at the end.

### Entity Branch as Editable Internal State

An entity branch is not just a knowledge log — it is the entity's **editable internal state**. The file tree within the branch is author-controlled and can contain anything that shapes how the entity is written:

- `sheet.md` — identity, voice, core traits (always loaded)
- `active.md` — current goals, focus, live suspicions
- `bob.md` — everything this entity knows or suspects about Bob
- `inventory.md` — current possessions (replaced entirely when it changes, not appended)
- `tasks.md` — what the entity is currently trying to accomplish

The author has four operations available, each with distinct semantics:

| Operation | Mechanism | Effect |
|---|---|---|
| **Append** | New content at HEAD | Character learns something, state advances |
| **Amend** | `At` + rewrite + rebase | It was always this way; old version in ledger only |
| **Replace** | Remove + recreate file at HEAD | Clean break; old file reachable via `At` but not active |
| **Constrain** | Append blunt instruction to history | Permanent behavioral constraint from that fiction-time position forward |

The correction loop for LLM output is correspondingly precise: if a character behaves wrongly, amend their branch at the point where the wrong belief or constraint was introduced, and regenerate. The fix is permanent — not a patch at HEAD that decays as context shifts.

---

## Summarization

A summary is just a tagged tick with no filesystem change, whose message contains a compressed description of a range of prior atoms, and whose refs point to the start of that range. Nothing more. The storage layer has no special concept of a summary — it is purely an application-layer convention on top of the general non-atom tick pattern.

This means there is no single canonical summary tree. Any number of summarizer agents can run independently and insert their own summary ticks into the chain, each tagged with their kind. A character entity branch might have a "casual appearance" summary (how this character comes across to strangers), a "professional context" summary (their work habits and competencies), and an "internal state" summary (private beliefs and emotional state) — all covering the same range of atoms, all inserted by different agents running at different times, all available for any downstream agent to discover and choose between.

When a narrator or context-assembly agent needs to load an entity, it walks the chain, discovers the available summaries via their tags, and decides: use one of the pre-generated summaries as-is, combine a summary with selected raw content, or generate a bespoke compression on the fly tailored to the current scene. Pre-generated summaries are a cache of likely-useful compressions. They are never mandatory.

**Validity.** Because the underlying content is append-only, a summary is never stale in the sense of covering content that was later modified — modification doesn't exist. The only way a summary can become inaccurate is if the atoms it covers were rewritten via `At` and rebase. A summary tick stores the start-atom ID in its refs; a background validation pass can check whether that ID still resolves to the same content, and flag or regenerate the summary if not. Summaries produced after a rebase that didn't touch their covered range remain valid without any verification.

**Summarization is a background task.** Summarizer agents run when content is stable — a chapter closes, a character exits, an arc resolves — and insert their ticks without blocking the writing workflow. If no summary exists yet, the consuming agent falls back to raw content or generates its own. The chain always has a correct answer; summaries only make retrieval cheaper.

---

## Cross-Branch References

When a story tick is copied to an entity branch (because the entity was present and experienced that event), the entity tick carries a reference to the story tick. This is the primary cross-branch link.

References are embedded in the tick and survive as long as the tick does. When a rebase changes a tick's identity, all references to the old identity are updated across all branches as part of the rebase operation. This is mechanical and complete — the storage layer knows every tick that was rewritten and propagates the mapping.

---

## The Temporal Ledger

Each branch maintains a temporal ledger: append-only, never rebased.

Every time a branch moves — forward, replayed, amended — the ledger records the previous head. This makes destructive rewrites safe: branches can be freely amended with the ledger silently accumulating every state they ever had. The undo path exists without polluting the branch structure.

The ledger is a storage implementation detail. The application layer never reasons about it directly — it is just there, making nothing permanently lost.

### Global Snapshot Branch

A single snapshot branch accumulates one commit per write operation, pointing to all current branch heads at that moment. This is nearly free to maintain and provides two properties:

- **Global undo** — roll back to any snapshot to restore all branch heads simultaneously to a consistent prior state, reversing rebases and any other changes.
- **GC anchor** — every object reachable from any snapshot commit is pinned. Objects rebased away from active branches remain reachable and are not garbage collected as long as a snapshot references them.

The snapshot branch uses normal commit semantics: each commit's parent is the previous snapshot, and the current branch heads are encoded in the message or refs. No new storage machinery required.

---

## The Storage Tree

Separate from all of the above: an author workspace for notes, outlines, sketches, planned scenes, ideas that may or may not become story.

The storage tree does not use chain semantics. It is just a place where ideas live, tracked by wallclock time.

Loose linking is appropriate: a story tick can reference a planning note it came from. The reverse is not required. An idea that was never used is not a consistency problem.

The storage tree is loaded as creative prompting context during brainstorming and planning, not during prose generation.

---

## The Working Tree

Not to be confused with the storage tree above (wallclock-ordered author notes): the **working tree** is the filesystem state implied by a branch's chain at a given position, plus whatever hasn't been committed as a tick yet (`Storyteller.Git`'s `WorkingTree`). Every mutation stages into it before `Store`/`Replace` commits; `At`/`Drop` move it between chain positions; `WithFS` snapshots and restores it around a scoped filesystem operation.

The append-only invariant governs what gets written as a tick — not the working tree itself. Content in the working tree can be freely rewritten; the invariant only bites at the moment that state is checked in via `Store`/`Replace`, which requires the new content to be an append over the previous tick.

This is the concept underlying any operation that needs to reconcile raw, freeform edits into the tick chain — an edit doesn't have to land as an append directly; it can be staged in the working tree and diffed against the chain at commit time to produce a valid append (or an amend, via `At` and rebase).

---

## Summary: What Each Structure Answers

| Structure | Question | Ordering | Semantics |
|---|---|---|---|
| Story branches | What is written, in what reading order? | Narrative dependency | Chain of ticks |
| Entity branches | What does this entity know at this point in their timeline? | Fiction-time | Chain of ticks |
| Temporal ledger | What states has this branch ever had? | Wallclock | Append-only ref log |
| Storage tree | What ideas exist? | Wallclock | Flat workspace |
| Working tree | What's staged right now, committed or not? | N/A (current state) | Ephemeral staging |
