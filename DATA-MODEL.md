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
- **Read** — `At` back to a tick purely to inspect state at that point (a historical UI view, a background summarizer or tracker walking prior ticks) without writing anything. This is not a lesser form of amending — it's a different operation with a different cost. Amending rebases the tail forward and every rewritten tick gets a new id, propagated to anything referencing it. A read touches none of that: nothing is rebased, no tick ever changes identity, and the branch is left exactly as it was found once the read completes. Reading the past must never accidentally rewrite it, and must never cost as much as if it had.

**Reticking** — atom boundaries can be changed after the fact. Splitting one tick into two requires choosing which inherits the original ID (preserving existing references) and which gets a new one (propagated through the remap table — see The Remap Table below). Merging two ticks into one is the reverse. Because rebase is already guaranteed to succeed, reticking is just another rebase — the storage layer imposes no additional cost.

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
- A **parent** — the previous tick in the chain (`Nothing` for root ticks)
- **References** — cross-branch links (e.g. a story tick referenced by an entity tick that records the same event)

The filesystem at any point in the chain is the product of replaying all ticks from the beginning. The tick itself carries no file content — it records that the chain advanced, and the message says why.

A tick that adds content to files is called an **atom** — the finest granularity at which content is addressable. Atoms can be anything from a phrase to a complete generation block; the choice determines how precisely history can be navigated and how finely other ticks (summaries, annotations, cross-branch references) can point into it. Ticks that carry no filesystem change are not atoms — they sit *between* atoms in the chain, logically anchored to the atom that immediately precedes them.

### Tick ID Stability

Tick IDs are content-addressed object hashes — like git commit hashes, they identify a specific object in the store permanently. A tick ID is always valid: the object it points to exists and its content never changes.

However, a tick ID is not a stable reference to a position in a branch. After a rebase, the same logical position in the chain has a new tick ID — the content of the diff may be identical, but the object is new because its parent changed. The old tick ID still resolves to the original object, which is no longer on the active chain.

The practical consequence: tick IDs are safe to use within a tight fetch-work-use cycle, or for historical reference ("this specific object"). They should not be stored as durable handles to "the current third atom of chapter 2" — that position's ID will change whenever anything before it is rebased.

### Invariants

Two invariants the storage layer enforces:

1. **Monotonic references** — following any combination of parent and ref links must eventually terminate. There are no cycles, and no tick may reference a tick that comes after it in the same chain. References to other chains or orphan objects are unrestricted. This is what makes chain position meaningful: within a chain, anything a tick references is guaranteed to precede it. Unlike git, where content-addressing enforces this for free, here the remap table redirects references after a rebase (on read immediately, in place at the transaction boundary — see The Remap Table below) — but that redirection is order-preserving by construction: a rebase substitutes a tick's *identity*, never its *position*, so a redirected ref precedes its referencing tick exactly when the original did. There is no global check on the redirection itself; the invariant is enforced at the operations that actually change positions — `moveTick` rejects a move that would put a tick ahead of something it references (or behind something that references it), and `mergeAtoms` refuses gapped selections.
2. **All tick references must be declared** — every reference to another tick must appear in `tickParent` or `tickRefs`. Tick IDs must never be embedded in the message or anywhere else. This is what makes rebase fixups complete and mechanical.

Parentless "helper ticks" (reference containers a tick could point at via `tickRefs` to hold a variable-length ref list) appeared in earlier drafts of this design but are **not representable** in the current encoding: a commit's first parent slot is unconditionally the chain parent — `tickRefs` are recovered as everything after it — so a parentless tick's first ref would be misread as its parent. A variable-length ref list has to live on an ordinary in-chain tick instead.

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

A summary is a tick (kind `Summary`) on the branch it summarizes, carrying a free-text `kind` (app-chosen, e.g. `"prose/chapter"`, `"character/internal-state"` — the storage layer never inspects it) and a ref to the HEAD of a separate **alternate chain**: an ordinary commit chain whose tree holds compressed files standing in for a range of the source branch's own content. Unlike an earlier, simpler design (a tick whose own message held compressed text inline, no filesystem footprint), this lets a summarizer produce a whole compressed filesystem — necessary once "summarize this" means more than one file, or means turning content into other files rather than prose.

**The alternate chain has no branch of its own.** It is never named, never given a ref, never created through the ordinary branch-creation path. It exists purely as loose commits, kept reachable only by the `Summary` tick's own ref to its tip — the same anchor-by-reference mechanic the temporal ledger and global snapshot branch already rely on (see below). Amend or rebase away the tick that named it, with nothing newer taking its place, and it becomes an ordinary unreachable object, reclaimed by GC with no explicit deletion anywhere. A summarizer's very first pass (nothing to build on yet) extends a fixed, parentless, empty-tree commit — content-addressed, so every summarizer everywhere computes and writes the exact same object; nothing needs naming even at the starting point.

**No canonical summary tree, and no explicit range.** Any number of summarizer agents can run independently and insert their own `Summary` ticks, each its own `kind`. A character entity branch might carry a "casual appearance" summary and an "internal state" summary side by side, both available for a downstream agent to discover and choose between. A summary's range is never stored: it covers everything back to the *previous* `Summary` tick of the same kind on the chain (or root, if none) — implicit in the tick's own position, discoverable by walking backward until the last match. Nothing needs a stored range-start, since a tick id may only ever live in a ref, never in a message or field, and a range-start would just be the tick's own position restated.

**Overwrite is a plain append, never a rebase.** A later summary of the same kind is just another `Summary` tick, its ref pointing at a *newer* commit on the same alternate chain — the chain gained one more commit on top of its own previous head. The older alternate-chain commit is untouched and stays reachable through the chain's own history for as long as anything still names it or a descendant of it.

**One commit per summarization pass, not one per file.** Every file a summarizer writes in one pass must land in a single alternate-chain commit, since only that commit is recorded on the new `Summary` tick. This is what makes "which point in the source chain was this particular file in the summary tree built from" answerable at all: find which alternate-chain commit last introduced or replaced that file (an ordinary content comparison across the chain's own history, nothing summary-specific), then find the `Summary` tick whose ref names that commit — a reverse lookup that only ever gives a real answer if a batch never spread across more than one commit.

**Hierarchical summarization stays on the one real branch.** A book-tier summarizer, reading a chapter-tier summary's own alternate-chain content as its input, still records its own `Summary` tick on the *same* real source branch the chapter-tier summarizer used — never onto the chapter-tier's own alternate chain, which (having no ref of its own) can't be opened to extend directly. Every tier's reachability is anchored to the one real branch this way, however many tiers exist.

**A summary's own commit needn't be empty either.** A summarizer may write an optional short blurb at a conventional path inside the alternate tree — a synopsis of what to expect there, cheap to read without fetching any real content. Entirely optional: a summarizer for which this makes no sense just never writes it.

**Consumption picks a level, not a structure.** A context-assembly agent walks an explicit, ordered list of kinds (finest first — the hierarchy the app has configured, not something to auto-discover from branch structure, since there's no branch structure left to discover it from) and asks for the densest level that still satisfies some acceptability check over the candidate text — `const True` for "no compression, give me the raw file," `const False` for "as compressed as this gets," a token/word-count budget for anything between. Every level offered is complete, never stale: since a summary only ever covers content that existed when its summarizer last ran, whatever's been appended to the source since is folded back in as a raw tail before a level is ever handed to a caller — picking a coarser level trades detail for compactness, never trades away completeness.

**Summarization is a background task.** Summarizer agents run when content is stable — a chapter closes, a character exits, an arc resolves — and insert their ticks without blocking the writing workflow. If no summary exists yet, the consuming agent falls back to raw content or generates its own. The chain always has a correct answer; summaries only make retrieval cheaper.

---

## Cross-Branch References

When a story tick is copied to an entity branch (because the entity was present and experienced that event), the entity tick carries a reference to the story tick. This is the primary cross-branch link.

References are embedded in the tick and survive as long as the tick does. When a rebase changes a tick's identity, every reference to the old identity — on any branch, whether anything ever opened it — follows automatically: reads resolve through the shared remap table the moment the rename is recorded, and the enclosing transaction's boundary rewrites the referencing commits physically, in one pass, once (see The Remap Table under Core Chain Operations). This is mechanical and complete — the operation that triggered the rebase never has to know propagation exists, and nothing per-branch has to be remembered, listed, or threaded for it to happen. A read of historical state produces no identity changes and so has nothing to propagate — see "Read" above.

A `Summary` tick's ref (see Summarization above) is the one case where the target isn't the head of any named branch at all — it names a commit in an alternate chain that has no branch or ref of its own. The mechanism is otherwise identical: the same embedded reference, the same remap/resolve machinery on a rebase of the referencing branch. What differs is what keeps the *target* alive: an ordinary cross-branch reference points at a tick some other branch's own ref already keeps reachable regardless; a `Summary` ref is the *only* thing keeping its target reachable at all, closer in spirit to the temporal ledger's GC anchor (see below) than to an ordinary cross-branch tie.

---

## Associations

Not every tick needs a hard reference. A tick can instead carry an **association**: a named, string-matched field in its payload (e.g. `file`, `character`) that relates it to another entity by name, scoped to the branch it lives on. `Presence` uses this to attach itself to a file's scene without pointing at any specific atom; `Prompt` uses the same mechanism. An association may be list-valued in the future (e.g. a tick relevant to more than one file) — it isn't required to be one-to-one the way a reference is.

**Authoritative for filtering, not a ceiling.** Anything that filters or scopes a tick stream (a file's projected chain, a notifier deciding what to push to a connection) can treat a tick's association as ground truth: if it says `file: scene.md`, it belongs to that file's view. But a filter is free to pull in *more* than its associated ticks by following hard references outward from what's already in scope — an annotation with no `file` association at all, but a hard ref into an atom that *is* in scene.md, is fair game for a file notifier to include (and for a UI to then highlight the referenced atom). Association is a floor, not a boundary.

**Branch-scoped.** An association only makes sense to resolve within the branch it's read from. If the entity it names is renamed, every branch that might hold a tick associated with the old name has to be walked and updated — there's no single place a rename touches, because the association itself has no structural link back to the entity it names.

**Contrast with hard references.** `tickRefs` (used by `Note`, `Fixup`) are structural: extra git-parent links, automatically kept in sync by `cascadeReplace` whenever the referenced tick's identity changes via rebase. An association carries no such guarantee — it's just text that has to already match. This is a deliberate trade, not an oversight: for something like `Presence`, the association's looseness is what makes the fold order (not the tick's target identity) the source of truth for what "enter"/"leave" mean. Coupling a presence tick to a specific atom's identity would force `moveTick` to drag it along whenever that atom moved, which is backwards: moving an atom past a `Leave` tick should change that atom's presence, not carry the `Leave` along with it.

**Relabel (pattern, not yet built).** No feature exists today that renames an associated entity, so no `relabel` operation has been implemented — but the shape it would need to take is worth recording so a future rename doesn't reinvent this: given an old and new name in some namespace (e.g. `character`), first check every branch for any tick already associating with the new name (a real collision — renaming into a name already in use would otherwise silently merge two identities, which might be desired but should never happen by accident), then, if clear, walk every branch's chain rewriting every tick's matching association value from old to new. This mirrors `cascadeReplace`'s "walk everything, rewrite what matches" shape, just keyed on text equality in a payload field instead of hash equality in a parent list.

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

## Core Chain Operations

Everything above — amending, reticking, the rebase-marker UI, background summarizers and trackers reading historical state — is built from a small closed set of operations in `Storage.Core`: a plain monad transformer (`StoreT`) over any content-addressed object store, with no dependency on git specifically. Nothing above needs its own bespoke implementation; each is one of these, or a composition of them. (The `Sem` world sees all of this through one first-order effect, `BranchOp` — a whole computation, however many nested rebases deep, is a single dispatch.)

**store / drop** — the only two operations that mutate the chain. `store` commits one tick onto head; an atom's new tree is computed from the parent commit's own tree (read the file's old blob, extend it, splice it back in), so it is an append *by construction* — there is nothing to verify afterward. `drop` pops the tick at head and hands back everything it was; `store =<< drop` rebuilds the same commit in place.

```haskell
store :: Tick -> StoreT m ObjectHash
drop  :: StoreT m Tick
```

**follow** — walk the chain backward from HEAD, tick by tick, folding an accumulator and deciding at each step whether to continue (always to the parent — there is only one meaningful "backward" in a linear chain). This is how anything inspects chain structure: finding a tick's position, building a file's atom-by-atom history, checking that one tick precedes another. `memoFold` is the memoized variant for incremental consumers: given checkpoints from a previous walk, it stops at the nearest one — content-addressing guarantees everything below an unchanged hash is unchanged.

```haskell
follow :: b -> (b -> ObjectHash -> Tick -> (b, Bool)) -> StoreT m b
```

**at / readAt** — move to a given tick's position and run an action there. Both are compositions of `store`/`drop`, not primitives of their own. `at` replays everything after the target back on top of whatever the action produced — a rebase; every rewritten tick gets a new id. `readAt` is the pure read: nothing is replayed, no tick changes identity, the chain is restored exactly as found. Read or write is the *caller's* choice of operation, made up front — not, as in an earlier version of this design, inferred from whether the action happened to write. `atWith` generalizes `at` with a hook applied to every replayed tail tick (`renameFile` uses it to rewrite each replayed atom's path in the same single pass).

```haskell
at     :: ObjectHash -> StoreT m a -> StoreT m a
readAt :: ObjectHash -> StoreT m a -> StoreT m a
atWith :: (Tick -> Tick) -> ObjectHash -> StoreT m a -> StoreT m a
```

### The Remap Table

Every rebase renames ticks: each replayed tick gets a new id because its parent changed. Something has to make sure every reference to an old id — a tick's own `tickRefs` on any branch, an id a caller read earlier and still holds, a client's rebase marker — ends up pointing at the new one. That something is the remap table, and its design rests on one separation: **recording a rename, resolving through it, and applying it are three different moments.**

**One table, owned by the store.** `at` does not return the old→new mapping, and nothing ever passes one around. The table belongs to the object store itself (`MonadStore`'s `resolveHash`/`recordRemap`), which in practice means the enclosing transaction — so every branch scope inside one transaction shares one table automatically. A rename made on one branch is immediately visible to every other, with no seeding, forwarding, or per-branch bookkeeping anywhere. The table is kept transitively closed as it grows (`composeMapping`): resolving is always a single lookup, never a chain walk, and cascades cannot loop.

**Recording.** `editTick`, `at`'s tail replay, and the chain-editing operations built on them (`moveTick`, `mergeAtoms`, `splitTick`, …) log each rename as it happens (`logRemap`). An action passed to `at` that replaces the target with something `at`'s own fallback can't recognize — several successors, a composite move — says explicitly what the target became; everything else is automatic. Recording does nothing but grow the table: no commit anywhere is rewritten mid-flight.

**Resolving.** Every id is resolved against the table *at the point of use*, never earlier: `store` resolves a tick's refs as it commits them, `at`/`readAt`/`syncTo` resolve their target, `readTick` hands back refs already resolved. A caller-held id from before an earlier edit in the same transaction therefore still lands on the right tick — holding ids across edits is safe by construction; caching a *resolved* id for later is the only mistake left to make.

One deliberate asymmetry: only cross-references resolve on read — **the chain parent never does**. Navigation (`follow`, `at`'s descent, `drop`) walks the live chain exactly as written. Under content addressing an old hash can come back from the dead (re-storing identical content on an identical parent re-mints the identical id — a swipe carousel returning to an earlier rotation does exactly this), so "this id was superseded" is not proof it isn't also a live chain member again; redirecting structure through the table could teleport a walk onto orphaned history. A dangling ref resolved eagerly is at worst a wrong answer; a rewritten parent is a broken walk. Structural rewriting belongs to the boundary, below.

**Applying.** Nothing is physically rewritten until a transaction boundary. There, one cascade (`cascadeReplace`) sweeps every story branch with the whole accumulated table: commits whose parents (chain or ref) name a superseded id are rewritten bottom-up, branch refs move, and descendants the cascade itself had to reparent join the table as discoveries. One boundary means one cascade and one client notification for the entire transaction, however many operations renamed however many ticks — and the cascade covers *all* branches, including ones nothing in the transaction ever opened.

**Scoping.** `withStorage` is the transaction: ref writes and renames buffer in its overlay, reads see the overlay (a scope opened mid-transaction sees the transaction's own pending state, not stale disk), and only a successful exit folds anything into the parent scope — collapsed to one ref write per branch, one pending table, one boundary. A failure folds nothing; `withStorageDiscard` folds nothing even on success (loose objects may exist, no ref moves, no rename escapes — a dry run with a hard guarantee). Transactions nest: an inner exit folds into the outer's buffer, and only the outermost boundary applies. The eager root interpreter is the degenerate case — every dispatch is its own boundary.

**What stays out.** The temporal ledger and snapshot refs deliberately point at superseded history — that is their whole job as undo path and GC anchor. The cascade only walks story branch refs, and ledger reads go through raw git, never through the resolving read: restoring a snapshot must land on the history that actually was, not a redirected version of it.

**inWorktree** — run an action against an ambient working tree freshly reset to head's committed content, then restore whatever the ambient tree held before. This replaces the old `withFS`: same purpose (see committed content instead of in-progress edits, from wherever it's called), but it is explicitly *the ambient tree's* scoping operation and touches the chain not at all — see The Working Tree below for the two-piece state model this lives in.

```haskell
inWorktree :: StoreT m a -> StoreT m a
```

**readPathAt** — a single path's content at a given tick, directly. Every tick already carries a complete tree snapshot (that's what makes it a tick), so this needs no chain walk: it reads the target commit, then walks straight down the path's own segments, one tree object per level. Cost is proportional to the path's depth, never to how far back in the chain the tick sits.

A modest but real shortcut for the "historical read" idiom below when only one path is wanted, not the whole tree. `readAt` itself is already O(1) (a direct head-pointer swap, not a walk), and `inWorktree`'s own `loadWorkingTree` never reads blob *content* for files it isn't asked about either — it only lists each directory's entries (name, hash) via one `readTreeM` per directory, recursing into subtrees. So `readAt tid (inWorktree (readFile path))` costs one tree-object read per directory in the whole branch rather than one per directory *on `path`'s own route* — real, but bounded by how many directories the branch happens to have, not by content size or file count. It adds up specifically because `Storage.Ops.foldInto` calls it once per ancestor commit it considers while walking backward, comparing each one's real content against what's left to explain to decide whether it can stop there — paying the whole-tree directory count on every one of those calls, instead of just `path`'s own depth, is the part actually worth avoiding.

```haskell
readPathAt :: ObjectHash -> FilePath -> m (Maybe ByteString)
```

**Composing them.** Everything above is a plain monadic value in `StoreT` — composition is ordinary `do`-notation, and a whole composition, however many nested rebases deep, is still one `runStorage` dispatch. Three idioms cover nearly everything:

- **Historical read**: `readAt tid (inWorktree action)` — `action` sees the files exactly as committed at `tid`, and nothing anywhere changes. The composition has to be said explicitly: the ambient tree does not follow chain position on its own, so an action that wants historical file *content* must ask for it with `inWorktree`. For a single known path, prefer `readPathAt` directly — see above.
- **Historical write**: chain edits under `at` — `at tid (editTick f)` amends the atom at `tid` and replays the tail; `at tid (store t)` inserts after it. Each rewritten tail tick's rename lands in the remap table as it happens.
- **Holding ids across steps**: safe. An id read before an `at` still lands on the right tick after it, because every operation resolves ids at the point of use (see The Remap Table). Compose freely; don't pre-resolve.

The exception is `atGeneric` — the same wind-back/replay shape one level up, at the effect stack, for an inner action that interleaves non-storage effects (LLM calls, recursive command dispatch — the rebase-marker feature). Two things distinguish it from `at`, both following from "run this *as if* the target were HEAD": it syncs the ambient tree (to the target on arrival, to the replayed head on return — the same convention `syncTo` sets for any other jump; uncommitted ambient edits do not survive it), and its descent publishes the wound-back head into the transaction's ref overlay, so any *other* scope the inner action opens on that branch — an agent reading it by name — finds it at the wound-back position too. That last property is what the multi-branch `At` command is built on: it nests one `atGeneric` per user-chosen branch around the main rebase, and the inner command runs in a world where every chosen branch, opened or not, sits at its chosen point. No fixup pass follows any of this — cross-branch refs into the rebased region are the boundary cascade's job, wherever they live.

---

## The Working Tree

Not to be confused with the storage tree above (wallclock-ordered author notes): the **working tree** is `Storage.Core`'s ambient, in-memory filesystem — the second, entirely independent piece of a branch scope's state, alongside its chain position. All plain file operations a branch exposes (read, write, list, remove — the `FileSystem` effects) act on it by path. It does not follow the chain: moving through history (`at`/`readAt`) never touches it, and writing to it never touches the chain. The two meet only at explicit sync points — `reset` (make the ambient tree match head's committed content, discarding what was there) and `inWorktree` (scope an action to a fresh sync, then restore what was pending).

The append-only invariant governs ticks, not the working tree. Content there can be freely rewritten — and committing an atom doesn't even look at it: `store` computes an atom's tree from the parent commit directly, so the append is by construction, never verified against staged state. Freeform ambient edits reach the chain through reconciliation instead (`commitWorktree`/`commitFiles` in `Storage.Ops`): each file's ambient content is diffed against its own atom history and folded in as kept atoms, same-position replacements (an amend, via `at` and rebase), drops, and new standalone atoms — an edit doesn't have to land as an append directly; reconciliation derives the appends and amends that express it.

Two tick kinds bypass prose reconciliation and *do* read the ambient tree when committed: `Binary` adopts the ambient content at one declared path verbatim (an uploaded portrait), and `Opaque` adopts the whole ambient tree at once (content some other tool wrote, whose paths we can't enumerate). Both are trusted, not diffed.

---

## Summary: What Each Structure Answers

| Structure | Question | Ordering | Semantics |
|---|---|---|---|
| Story branches | What is written, in what reading order? | Narrative dependency | Chain of ticks |
| Entity branches | What does this entity know at this point in their timeline? | Fiction-time | Chain of ticks |
| Temporal ledger | What states has this branch ever had? | Wallclock | Append-only ref log |
| Storage tree | What ideas exist? | Wallclock | Flat workspace |
| Working tree | What's staged right now, committed or not? | N/A (current state) | Ephemeral staging |
