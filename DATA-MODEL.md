# Storyteller: Data Model

## Core Principle

**Story branches are the product. Entity branches are partial views. The gap between them is where dramatic irony, unreliable narrators, and distinct character voices live.**

Prose carries craft. Entity branches carry perspective. These are separate concerns given separate homes.

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

### Invariants

Two invariants the storage layer enforces:

1. **No cycles** — if a tick has a parent, following parents must eventually reach a root (a tick with no parent).
2. **All tick references must be declared** — every reference to another tick must appear in `tickParent` or `tickRefs`. Tick IDs must never be embedded in the message or anywhere else. This is what makes rebase fixups complete and mechanical.

Helper ticks (parentless ticks used purely as reference containers, e.g. to store a variable-length list of references that a single tick can then point to via `tickRefs`) are valid and follow the same invariants.

### Tick Kinds

The storage layer is agnostic to tick kind. The application layer tags the message and interprets accordingly:

**Prose tick** — the common case. The filesystem gained one more paragraph (or sentence, or LLM-generated block — the granularity is flexible). The message is a brief summary of what was written, or the paragraph itself verbatim.

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
- Ticks can be deleted (forgotten, suppressed, never actually known)
- False beliefs sit in the branch as fact, from the entity's perspective
- Inferences and working theories are recorded, even if wrong

A corrected misremembering: go back, amend the tick, replay forward. The old version exists in the temporal ledger. No corrective tick appended at the end.

---

## Summarization

Summaries form a tree over the chain. A chapter-level summary tick covers all scene-level ticks within it; a scene-level summary covers all prose ticks within it. Agents navigate this tree dynamically — reading a chapter summary, deciding one scene is relevant, drilling into that scene's ticks directly, ignoring the rest.

Summary ticks are free navigation artifacts — they cost one extra tick and give hierarchical zoom for free.

Summarization is a background task. The natural trigger is completion: a chapter closes, a character exits, an arc resolves. The content is stable; the agent has moved on; the summary will be ready before it's needed.

---

## Cross-Branch References

When a story tick is copied to an entity branch (because the entity was present and experienced that event), the entity tick carries a reference to the story tick. This is the primary cross-branch link.

References are embedded in the tick and survive as long as the tick does. When a rebase changes a tick's identity, all references to the old identity are updated across all branches as part of the rebase operation. This is mechanical and complete — the storage layer knows every tick that was rewritten and propagates the mapping.

---

## The Temporal Ledger

Each branch maintains a temporal ledger: append-only, never rebased.

Every time a branch moves — forward, replayed, amended — the ledger records the previous head. This makes destructive rewrites safe: branches can be freely amended with the ledger silently accumulating every state they ever had. The undo path exists without polluting the branch structure.

The ledger is a storage implementation detail. The application layer never reasons about it directly — it is just there, making nothing permanently lost.

---

## The Storage Tree

Separate from all of the above: an author workspace for notes, outlines, sketches, planned scenes, ideas that may or may not become story.

The storage tree does not use chain semantics. It is just a place where ideas live, tracked by wallclock time.

Loose linking is appropriate: a story tick can reference a planning note it came from. The reverse is not required. An idea that was never used is not a consistency problem.

The storage tree is loaded as creative prompting context during brainstorming and planning, not during prose generation.

---

## Summary: What Each Structure Answers

| Structure | Question | Ordering | Semantics |
|---|---|---|---|
| Story branches | What is written, in what reading order? | Narrative dependency | Chain of ticks |
| Entity branches | What does this entity know at this point in their timeline? | Fiction-time | Chain of ticks |
| Temporal ledger | What states has this branch ever had? | Wallclock | Append-only ref log |
| Storage tree | What ideas exist? | Wallclock | Flat workspace |
