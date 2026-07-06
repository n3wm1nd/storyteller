# Plan: A dedicated storage monad to replace `At`/`WithFS`'s `interpretH` cost

Status: spec, not yet implemented. Written to be picked up cold in a future session.

## The problem, in one paragraph

`At`/`WithFS` are Polysemy **higher-order** effects — their constructors carry
an arbitrary inner computation as a field (`At :: Bool -> TickId -> m a ->
StoryBranch branch m a`). Polysemy can only interpret a higher-order effect via
`interpretH`, which has to reify ("weave") that inner computation so it can be
replayed inside a modified interpreter context. That reification is expensive:
measured at roughly **2000x** the cost of an ordinary first-order effect
dispatch for a single nested read (see `bench/EffectOverhead.hs`'s header for
the earlier, simpler A/B). `runAtH` — the recursive rebase/replay
implementation backing every `At` call — runs its *entire* recursive walk from
inside one such `interpretH` context, so every read and write at every level
of the walk pays that tax, not just the boundary crossing. At 330 ticks this
already costs ~9s for a deep merge; the target is ~10,000 ticks, where the
same shape of cost is unusable.

The fix is not "stop using effects" or "stop using a monad" — first-order
effect dispatch was independently measured at ~1.46x, i.e. basically free.
The fix is to stop needing `interpretH` at all for this family of operations,
by recognizing that none of them actually need "run arbitrary caller code at
a different point in history" — they need a fixed, closed-form sequence of
storage reads and writes, which is expressible as ordinary monadic code with
no higher-order effect construction anywhere in it.

## How we got here (the reasoning trail)

This is the load-bearing part for anyone picking this up cold — the
conclusions below look obvious in hindsight but were each arrived at by
measuring, not by guessing, and each ruled out a plausible-looking
alternative:

1. **Started by asking**: how much of `At`'s cost is inherent to using
   Polysemy at all, vs. removable without giving up the abstraction? Built
   `bench/GitBranchEffect.hs`: a `MonadGitStorage` typeclass + a single
   first-order effect boundary (`GitBranch`/`onBranch`) wrapping a whole
   transaction, leaves still sending through the real `Git` effect. Measured
   **~14x** faster than production Polysemy `mergeAtoms`, consistently across
   scales (N=200 and N=1000) — the ratio doesn't grow with N, which matters
   for the next point.

2. **Then asked**: is that 14x really "removed the interpretH tax," or is it
   partly an artifact of the benchmark cutting corners? First cut of
   `DirectImpl` (fully bare, no monad, no effect) bypassed storage entirely
   with a private in-memory `Map`, measuring ~440x vs. production — but this
   was **wrong**: it skipped whatever real work the storage backend does, so
   the number conflated "removed dispatch" with "didn't do the work." Fixed
   to route through the same real `Git` effect and `Git.Mock` interpreter as
   everything else. Once storage was fair, `DirectImpl` (plain `Sem`, no
   typeclass) came out only **~1.17x** faster than `GitBranchEffect` — meaning
   the `MonadGitStorage` typeclass/single-effect-boundary layer was *already*
   capturing nearly all the available win. This is the key finding that
   licenses the whole plan: **a typeclass-based storage monad embedded as one
   first-order effect is not a compromise vs. going fully bare — it is
   already close to the floor.**

3. **Ruled out caching as the lever.** Hypothesis: a read cache local to the
   interpreter (checked before ever dispatching to `Git`) would cut cost by
   avoiding redundant reads. Tested three shapes:
   - `Runix.Git.withGitCache` (a `Sem`-`State`-backed `intercept` wrapping
     `Git` a second time) — made `mergeAtoms` **~33% slower**, not faster:
     every read, hit or miss, now pays two effect dispatches instead of one,
     and this specific workload (single linear chain of unique-content
     atoms) never has a cache hit to offset that cost.
   - A cache reached via `Polysemy.State`'s `get`/`modify` directly (still
     one `send` per access) — architecturally cleaner than an interceptor,
     but still pays a dispatch per read.
   - The one that matters: cache fetched **once per top-level operation**
     (`get`/`put` around the whole call, not around each read) and threaded
     as a **plain function argument/return value** through
     `loadWorkingTree`/`readTreeRecursive`/`applyDiff`/`runAtH`'s recursion —
     zero effect dispatch for a hit, a bare `Map.lookup` on a value already
     in hand. This is implemented today in `runStoryBranchGit` (`ReadCache` in
     `Storyteller/Core/Git.hs`). It costs nothing when idle (confirmed: no
     measurable slowdown) but also **doesn't speed up `mergeAtoms`**, because
     the benchmark scenario structurally has no redundant reads (every atom's
     content, and so every tree/commit hash, is unique across the walk).
     Caching was never the lever for this specific cost; it's a correct,
     free-when-idle optimization for workloads that *do* have read overlap
     (e.g. multi-branch tracking fan-out), orthogonal to this plan.

4. **Pinned down the actual shape of the cost.** Is `runAtH`'s cost "each
   level forwards through progressively more interpreter layers" (would show
   up as the Polysemy-vs-`GitBranchEffect` ratio *growing* with chain depth
   N), or "every op pays one flat tax, and there are O(depth) ops" (ratio
   roughly constant across N)? Measured: ~15.8x at N=200, ~13.9x at N=1000 —
   flat, not growing. Confirms the second shape: `runAtH`'s own recursion
   never opens a new `interpretH` layer per level (it's an ordinary Haskell
   function calling itself); the *one* layer opened when `At` is first
   dispatched simply never closes for the whole walk, and every read/write
   inside it — `readCommit`, `loadWorkingTree` (x2), `applyDiff`'s blob reads,
   `writeCommit` — pays the same large constant tax, once per level, for as
   many levels as the rebase is deep.

5. **Confirmed `WithFS` specifically is not the culprit for merge/split.**
   `mergeAtoms` uses `sneakyAt` (`send (At True tid action)`), never
   `WithFS` — atoms' content lives in the commit message, not the tree, so
   merge/split never need historical file content. `WithFS`'s tree-load only
   matters for the (separate) family of operations that need historical
   *file content* (`atWithFS`/`readAtWithFS`), and even those turned out (see
   point 7) to have no need for Polysemy's higher-order machinery either.

6. **Arrived at the actual fix.** Since first-order dispatch is cheap and the
   tax is specifically about higher-order effect construction, the fix is to
   never construct a higher-order effect for this family of operations at
   all. `GitBranchEffect`'s trick: its `OnBranch` constructor's argument is a
   **rank-2-polymorphic value in an independent type** (`forall n.
   MonadGitStorage n => n a`), not `Sem`'s own `m` — so `m` never appears
   nested in the constructor, and Polysemy interprets it with plain
   `interpret`, no reification needed. The computation is self-contained;
   there's no continuation to weave.

7. **Checked the actual scope** by grepping every current caller of
   `at`/`sneakyAt`/`atWithFS`/`sneakyAtWithFS`/`readAt` in the codebase
   (`Storyteller/Core/Edit.hs`, `Storyteller/Core/Append.hs`,
   `Server/Core/File.hs`). Every single inner action turned out to be purely
   storage-level (`drop`, `popTick`, `pushTick`, `appendFile`, `storeAs`,
   `get`) — **none** interleave an LLM call, logging, or any other Polysemy
   effect mid-flight. This matters a lot: it means the new monad's `at` isn't
   a special case for a handful of closed-form algorithms, it can be a full
   replacement for `At`/`WithFS` as they exist in this codebase today. (If a
   future caller genuinely needs to interleave a non-storage effect inside a
   time-travelled action, `At`/`WithFS` still exist as a fallback — the
   `interpretH` cost was always the right price for that generality, we just
   don't need to pay it for anything that exists right now.)

8. **Settled where `WorkingTree` lives**, by two arguments that turned out to
   agree: (a) tick-chain movement and working-tree reconstruction are
   *interleaved step-by-step* in the replay algorithm — at level *i* you need
   level *i*'s tree to decide what to write, and that tree needs level *i*'s
   reads — so they can't be scheduled as separate phases, only as code
   living in the same monadic context; (b) `WorkingTree`'s cells are
   literally `ObjectHash` references into the same object store the monad
   already manages, so keeping tree reconstruction in a genuinely separate
   system (e.g. Polysemy `State WorkingTree` + a parallel `Git` effect call)
   would mean two places independently dereferencing the same hashes,
   reintroducing a boundary to cross at every step. Conclusion: `WorkingTree`
   is not a typeclass primitive, but it's not external either — it's derived
   data, reconstructed by ordinary polymorphic functions that live and run
   *inside* the monad's execution context, freely interleaved with
   tick-chain movement, because both are just composed calls against the
   same read/write primitives.

## Invariants this design relies on (and would break without)

These are the domain facts that make an efficient, *automatic* replay-merge
possible at all — without them, replaying a tail after a rebase would be a
general three-way-merge problem, which is genuinely hard and genuinely
un-cacheable. With them, it's mechanical. (See `DATA-MODEL.md`'s "Append-Only
Invariant" section for the product-level statement this derives from.)

- **Every tick's tree is either identical to its parent's, or differs from it
  by exactly one append to exactly one file.** No tick ever deletes,
  shrinks, or edits-in-place any file. This is what makes "replay tick T on
  top of a new parent" well-defined and always-succeeding: either there's
  nothing to redo (tree unchanged), or the fix is "take whatever's at that
  path in the new parent, append the same suffix" (`applyDiff` already
  implements exactly this).
- **An atom is precisely a tick with a file-content change** — the two are
  synonymous in this design. Any tick without an associated append is a
  no-op on the tree (e.g. a "presence" tick), and the replay algorithm can
  treat it uniformly: compute the diff (possibly empty), reapply it.
- **A tick's own content (for atoms) lives in the commit message, not the
  tree** — this is why merge/split never need `WithFS`'s historical-tree
  load: they only ever need tick metadata, never reconstructed file content.
- **Rebase always succeeds** (`DATA-MODEL.md`): because replay is "reapply a
  known, mechanical diff," not "reconcile two arbitrary snapshots," there is
  no failure mode to handle mid-replay — this is what allows `at` to make
  replay-on-return an unconditional, built-in part of its own semantics
  rather than something each caller must handle.

If any of these invariants is ever relaxed (e.g. allowing in-place edits, or
letting a tick touch more than one file), the mechanical replay this design
depends on breaks, and `at`'s implementation would need to go back to
something like a real three-way merge — at which point this whole
architecture should be reconsidered, not patched.

## The design

### Vocabulary: tick/tree level, not raw git level

The typeclass's primitives are shaped around the domain (ticks, the working
tree, the append invariant), not around git objects. Raw git primitives
(`readCommit`/`writeCommit`/`readTree`/`writeTree`/`readBlob`/`writeBlob`/
`resolveRef`/`setRef`/`listRefs`) become the *interpreter's* implementation
detail for this typeclass — client code (`mergeAtoms`, `moveTick`, ...) never
touches them directly, the same way it doesn't today.

```haskell
class Monad m => MonadStorageView m where
  -- the storage view: (WorkingTree, HEAD tick)
  headTick    :: m TickId
  headTree    :: m WorkingTree                   -- reconstructed at HEAD

  -- tick chain navigation
  tickAt      :: TickId -> m Tick                 -- fetch tick data + position
  followChain :: b -> (b -> Tick -> (b, Maybe TickId)) -> TickId -> m b

  -- writes (append-only, tick-kind-aware)
  storeAtom   :: FilePath -> Text -> m TickId     -- append content, new tick
  storeTick   :: TickData -> m TickId             -- non-atom tick, tree unchanged
  dropTick    :: m ()                             -- HEAD -> parent, discard

  -- the auto-merging time-travel primitive -- this replaces At/WithFS
  at          :: TickId -> m a -> m (a, [(TickId, TickId)])
     -- moves to tid, runs the action, replays the (append-only) tail back
     -- on top of whatever the action produced -- mechanical, using the
     -- invariants above, never a generic merge
```

(Exact method list/names to be finalized against what `mergeAtoms`/`moveTick`
/`splitTick`/`unstoreAtom`/`rewriteAtom` actually call today via
`Storyteller.Core.Storage`'s vocabulary — `popTick`/`pushTick`/`storeAs`/
`sync`/`reset`/`follow` etc. This sketch is a starting point, not a final
API.)

### `WorkingTree`

Stays the plain `Map FilePath FSNode` it is today. Not a typeclass method —
`loadWorkingTree`/`applyDiff`/`flushWorkingTree`/`checkAppendOnly` become
ordinary `MonadStorageView m => ... -> m WorkingTree`-shaped functions (today
they're `Members '[Git, Fail] r => ...`), living in the same module,
interleaved freely with tick-chain code, exactly as they are interleaved in
`runAtH` today. The *ambient*, current-head `FileSystemRead`/`FileSystemWrite`
Polysemy effects (ordinary file access outside any rebase, e.g.
`Server.Core.File`'s current-file reads) are untouched by this migration —
they were never part of the expensive path.

### Effect embedding into Polysemy

One first-order effect per branch scope, mirroring `bench/GitBranchEffect.hs`
exactly:

```haskell
data GitBranchOp (branch :: k) m a where
  RunStorage :: (forall n. MonadStorageView n => n a) -> GitBranchOp branch m a
```

The constructor's argument is rank-2-polymorphic in an independent type `n`
— `m` never appears nested in it — so this interprets via plain `interpret`,
no `interpretH`, no reification. The concrete instance (`GitStoryM`-shaped:
a thin newtype over `Sem r` whose methods `send` into the real `Git` effect)
lives underneath, same as `GitBranchEffect.hs`'s prototype.

## Migration scope

Every current caller of `at`/`sneakyAt`/`atWithFS`/`sneakyAtWithFS`/`readAt`
(confirmed by grep, see reasoning point 7 — all storage-only inner actions):

| Module | Operation | Current call |
|---|---|---|
| `Storyteller/Core/Edit.hs` | drop-at-tick | `at @branch tid $ drop @branch` |
| `Storyteller/Core/Edit.hs` | `moveTick` | 3x `sneakyAt @branch ...` |
| `Storyteller/Core/Edit.hs` | `splitTick` | `sneakyAt @branch tid $ ...` |
| `Storyteller/Core/Edit.hs` | `mergeAtoms` | `sneakyAt @branch lastTid $ ...` |
| `Storyteller/Core/Edit.hs` | `emitStandaloneGap` (file-content variant) | `sneakyAtWithFS @branch anchor $ ...` |
| `Storyteller/Core/Append.hs` | `unstoreAtom` | `sneakyAt @branch tid (drop @branch)` |
| `Storyteller/Core/Append.hs` | `rewriteAtom` | `sneakyAt` + nested `withFS` |
| `Server/Core/File.hs` | historical tick fetch | `readAt @Main tid (get @Main)` |

**Not in scope** (plain `withFS` with no enclosing `at`/`sneakyAt` — already
O(1), current-head only, never pays the `interpretH` tax): `pushTick`'s own
top-level use, `Append.hs`'s plain-write helpers, `withBranch`/
`runBranchAndFS` themselves (branch-scope setup, not part of the rebase
cost).

Suggested order: port `mergeAtoms` first (we already have a validated shape
for its core algorithm from `GitBranchEffect.mergeAndRebase`) and benchmark it
against real production data before porting the rest — confirms the ~14x (or
better, given the tick-level vocabulary avoids re-deriving diff logic per
call) holds outside the synthetic harness. The rest should follow an
identical pattern once `mergeAtoms` is validated.

## Open questions (not yet resolved, need answers before/while implementing)

1. **Cross-branch cascade scope.** Today, `Replace`/`moveTick` rewrite *other*
   branches' refs when a tick they reference gets superseded
   (`cascadeReplaceOtherBranches` in `Storyteller/Core/Git.hs`). Does the new
   `at`'s automatic replay stay scoped to one branch, with cross-branch
   cascade triggered as a separate step by the caller afterward (matching
   today's structure), or does `MonadStorageView` need a built-in notion of
   "the other branches that reference this one"?

2. **`StoryStorage`'s transactional buffering.** `withStorage` (`Storyteller/
   Core/Git.hs`) buffers ref writes in memory and replays them as one batch
   only once a whole logical operation succeeds — this is what keeps
   intermediate rebase states from becoming individually observable/
   notified. Does that stay a Polysemy concern wrapping the `GitBranchOp`
   call (the storage monad computation runs to completion, *then* Polysemy
   decides whether/how to publish), or does the storage monad need its own
   buffering?

3. **Naming and module home.** New module under `Storyteller.Core.*`
   (`Storyteller.Core.StorageMonad`? `Storyteller.Core.TickStorage`?), or
   does this absorb/replace parts of `Storyteller.Core.Git`? Given
   `WorkingTree`/`applyDiff`/`checkAppendOnly` already live there and stay
   conceptually attached to this monad, replacing large parts of that module
   in place (rather than a new module importing from it) may be more natural
   — to be decided when implementation starts.

4. **Exact method list.** The sketch above is a starting point; the real API
   should be derived from what `Storyteller.Core.Storage`'s existing
   vocabulary (`popTick`/`pushTick`/`storeAs`/`storeData`/`sync`/`reset`/
   `follow`/`get`/`drop`) actually needs, not designed from scratch.

## References

- `bench/GitBranchEffect.hs` — the validated typeclass-plus-single-boundary
  pattern this design is built on.
- `bench/DirectImpl.hs` — confirms the typeclass layer isn't a compromise
  (only ~1.17x from the floor, once storage cost is measured fairly).
- `bench/EffectOverhead.hs` — the A/B/C/D harness (`cabal bench
  storyteller-effect-overhead`), including the flat-vs-growing ratio check
  across N=200/N=1000 that pinned down the cost's shape.
- `bench/ReadCachePerf.hs`, `bench/ExpandRefsPerf.hs` — ruled out caching as
  the lever for this specific cost; both still valid, orthogonal
  optimizations for workloads with real read redundancy.
- `Storyteller/Core/Git.hs`'s `ReadCache`/`cachedReadCommit`/etc. — the
  "fetch once per top-level op, thread purely" local-cache pattern, already
  applied to `runStoryBranchGit`/`runAtH`/`loadWorkingTree`/`applyDiff`.
- `DATA-MODEL.md`'s "Append-Only Invariant" section — the product-level
  statement of the invariant this whole design leans on.
