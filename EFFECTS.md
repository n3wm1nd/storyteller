# Adding functionality: think in effects

This is the convention to follow whenever agent or DSL code needs some new
capability from storage (or from anything else pluggable). The worked
example throughout is `src/Storyteller/Core/ContentEffects.hs`, written
during the pass that moved the context DSL and its agents off direct
`Storage.Core.StoreT` access (see `[[project_mcp_export_effect_boundary]]`).
Read that module's Haddocks alongside this doc — this explains *why* it's
shaped the way it is; the module is the concrete instance.

## Start from the concept, not the primitive

Before writing a GADT, name the *capability* in one sentence: "which
characters are present in this file's history," "a curated recent slice of
the journal," "turn-shaped conversation history." If the sentence is
"read/write this thing" with no derivation or curation involved, it's
probably not a new effect at all — see "Reuse before inventing" below.

The wrong shape is one effect per module that happens to own some `StoreT`
code today (a catch-all `DSLStore`, a catch-all `TasksStore`). That just
relocates the "one fat interface" problem: a backend able to honestly
support half of a bundled effect still can't get an interpreter for the
whole thing, so it loses every function that touches *any* constructor in
it, including ones it could actually run. `ContentEffects.hs` splits seven
narrow effects (`TreeAccess`, `Presence`, `JournalAccess`,
`ConversationAccess`, `TrackedFiles`, `FileTicks`, `BranchResolve`) rather
than one `DSLStore`, precisely so a backend can support, say,
`ConversationAccess` (a SillyTavern-style chat log already *is*
turn-shaped) without needing tick-history machinery for the rest.

Self-contained is the default (task tracking's `TasksSync` doesn't reach
into journal or conversation concerns), but real interdependency is fine
when the concept genuinely has it — `Storyteller.Writer.Agent.Summarizer`'s
internal `Summarization` effect is this: its four operations
(`PendingSummary`/`RecordSummary` for a whole-branch pass,
`PendingPathSummary`/`RecordPathSummary` for one path) are one effect
because they're one concept ("what's pending, record this pass," two
granularities), confirmed by the fact that every real caller
(`runSummarizer` vs. `runSummarizerForPath`) already picks between the pair
at one call site, not because they happen to share an implementation. (A
shared, ContentEffects-level write vocabulary analogous to the read side
above — "commit content, several ways" — was sketched at one point but
never built: every write path today instead calls `Storage.Ops`/
`Storage.Tick` directly from inside whichever narrower, module-local effect
actually needs it, `Summarization` here and `Storyteller.Writer.Agent.
Tasks`'s `TasksSync` being the two real instances.)

## Two layers: the GADT vs. the exported library

An effect module has two things in it, and they are allowed to differ:

1. **The GADT** — the minimal set of operations an interpreter must
   implement. Keep this as small as the concept allows. It's fine (and
   often right, see "Carrying errors" below) for a constructor's return
   type to be more informative than what most callers want, e.g. `Either`
   or `Maybe` instead of a bare value — that's for interpreters and
   interceptors to see, not ordinary callers.
2. **The exported functions** — the actual library callers reach for. Some
   are literally `makeSem`'s generated sends (`ContentEffects.hs` does this
   for all seven effects, since each GADT constructor already *is* the
   natural call a caller wants — `charactersPresent`, `journalWindow`,
   etc.). Others should be hand-written on top of the raw sends: a
   convenience wrapper that collapses an `Either`/`Maybe` into `Fail`, an
   `append` defined as `read` then `write`, two constructors combined into
   one natural-language call. The GADT describes the interpreter's
   contract; the exported functions describe the *user's* library. They
   don't have to be the same shape, and forcing them to be is how you end
   up exposing raw storage primitives as if they were the concept.

## Reuse before inventing

There's no single fixed criterion for "does this need a new effect" —
it's two different situations, and they get different treatment:

- **A functionality achievable with what's already there.** Composing
  existing effects at the call site (a plain multi-effect function, see
  below) is enough — don't add a new effect just because a function
  happens to call two others.
- **An operation that structurally requires several functionalities
  together as one thing.** `Storyteller.Writer.Agent.Summarizer`'s
  internal `Summarization` effect is this: its four operations are one
  concept because every real caller already picks between the
  whole-branch and single-path pair at a single point of use, not a
  coincidence of implementation. When it's this, name it.

Check whether an existing effect already covers it before writing a new
one. `ContentEffects.hs`'s own design pass found that plain path-based
file read/write, with no history involved, is already `Runix.FileSystem`'s
`FileSystemRead`/`FileSystem` — no new effect needed for that half of
`Storyteller.Writer.Agent.Tasks`'s `readTasksFile`/`resolveCharacterName`.
New effects are for the genuinely history- or chain-dependent remainder.

Also decide deliberately whether the new effect needs its own `(branch ::
k)` phantom, matching `Storyteller.Core.Branch.BranchOp`/
`Runix.FileSystem`'s convention: it does, if evaluation isn't pinned to one
fixed branch chosen once at interpreter-wiring time (all eight
`ContentEffects` had this except `BranchResolve`, which resolves a *name*
through the project-global `StoryStorage` rather than reading from an
already-open branch scope). Getting this wrong doesn't fail loudly at
first — it fails the first time a real caller needs two different branches
live in the same request (this happened once already; see
`[[project_mcp_export_effect_boundary]]`'s design-corrections section).

## Position: type-level phantom vs. value-level argument

A subtler version of the same reuse question: if something *looks* like it
should fold into an existing effect because the operations are shaped the
same ("list some paths, read a blob"), check where the position comes
from before merging them.

`Runix.FileSystem`'s own `project` phantom is fixed once, at compile time,
when an interpreter is wired (`runStoryFSGit @branch`) — right for "the one
named branch I keep reading from, always its current live state."
`ContentEffects.hs`'s `TreeAccess` looks superficially identical
("`TreeSnapshot`/`ReadTreeBlob` read a tree/blob") but takes its position as
a *value*-level `Core.ObjectHash` argument instead, because the DSL's own
cross-branch reads only learn *which* commit to read at from a
`BranchResolve` call moments earlier in the same evaluation — often a
different one on every loop iteration. Minting a fresh phantom-tagged
`FileSystem` interpreter per dynamically-resolved commit isn't expressible
(a Polysemy row is fixed at compile time), so `TreeAccess` stays its own
effect. See its Haddock in `ContentEffects.hs` for the fuller version of
this argument — it's the kind of judgment call worth writing down at the
declaration, not just here.

## Native types at the boundary

Once a caller is on the near side of an effect's `Member` constraint, it
should be looking at ordinary Haskell types for the concept — `Set
Character`, `[Turn]`, `[Text]` — never a backend's own representation
leaking through. `ContentEffects.hs` is not fully clean on this point
today, and says so in its own Haddocks: `BranchResolve`'s `ResolveBranch ::
BranchName -> BranchResolve m (Maybe Core.ObjectHash)` and `TreeAccess`'s
`TreeSnapshot`/`ReadTreeBlob` hand back a raw `Storage.Core.ObjectHash` —
i.e. "the position of a branch is a hash of its content," an assumption a
backend with no content-addressing (a plain directory, a SillyTavern
export) genuinely can't satisfy. The fix, not yet done, is an opaque
position/ref type owned by the effect module itself, produced by
`BranchResolve` and consumed by `TreeSnapshot`/`ReadTreeBlob`/
`JournalWindow`, with the git-backed interpreter free to implement it *as*
an `ObjectHash` internally without exposing that choice to callers.
Contrast this with `Presence`'s `CharactersPresent`, which is done right:
it returns `Set Character` (a real domain type), not the raw `FileTick`
list the git interpreter actually walked to derive it — the derivation is
the interpreter's job, not something every caller repeats.

The test: if a caller needs to know something about *how* the backend
stores data in order to use the return value (that it's a hash, that it's
addressable, that two of them can be compared for object identity rather
than semantic equality), the boundary is leaking. Push that knowledge back
into the interpreter, or into an opaque type the effect module itself
owns.

What that opaque position type actually looks like is open — deliberately
not designed yet. It needs more analysis before committing to a shape (does
it need `Eq`/`Ord`? is it one type shared by every effect that resolves or
consumes a position, or does each effect own its own?) — don't invent an
answer under time pressure the next time `BranchResolve`/`TreeAccess` come
up; treat it as its own design pass.

## Interpreters vs. plain multi-effect functions

Effects are allowed to depend on other effects — a git-backed interpreter
for effect `X` can legitimately be written by calling into effect `Y`
rather than reaching for a primitive. `runBranchResolve` does this: it's
built on `StoryStorage`, not on raw git. Intermediate effects are entirely
permissible; the story-DSL is layered several effects deep already
(`TreeAccess`/`Presence`/... on `BranchOp`, `BranchOp` on `StoryStorage`
and `Git`).

But keep two different things distinct in your own head, because they read
differently and are wired at different places:

- **A plain function written against several effects.** No GADT of its
  own — it's just ordinary business logic that happens to need more than
  one capability (`Members '[TreeAccess branch, Presence branch] r =>
  ...`). This is the common case, and it's what almost every DSL library
  function and agent should be.
- **An interpreter for effect `X`, implemented by translating each
  constructor into calls on effect `Y`** (`reinterpret`/`interpret`
  discharging `X` while re-emitting `Y` sends). This is a real
  implementation strategy for `X`, not a caller of `X` — it belongs next
  to the rest of `X`'s interpreters, wired into the stack at the point `X`
  is discharged, not scattered into business-logic modules.

Both are fine. What isn't fine is *unnecessarily* repeated round-trips
through an effect boundary when one send would do — see below.

## Batch the primitive; keep the effect call intent-shaped

An effect call should say *what* the caller wants ("give me this file's
recent journal window," "commit this content") — one send, one meaningful
unit of work — even when the interpreter underneath has to do several
low-level operations to satisfy it. It should not become a relay for each
individual primitive step.

This is exactly what `Storyteller.Core.Branch.BranchOp`'s `RunStorage ::
(forall n. StoreM n => StoreT n a) -> BranchOp branch m a` already gives
every interpreter in `ContentEffects.hs`: `runJournalAccess`'s
`JournalWindow` constructor is *one* `BranchOp` effect call
(`runStorage @branch (...)`), even though the `StoreT` computation inside
it walks tick history, filters, and pads a window — several git-level
operations batched and run to completion inside that single send, with
only the final `[Text]` crossing back out. The alternative — sending one
effect call to read the tick list, another to filter it, another to slice
the window, each returning through `Sem r` — would multiply effect
dispatch overhead for no benefit (nothing between those steps needs to
observe or intercept them individually) and would scatter the concept
`JournalWindow` is supposed to name across several call sites instead of
naming it once.

The rule of thumb: if two steps always happen together, are never
individually intercepted, and nothing meaningful could happen in between
them from a different part of the program, they belong inside one
interpreter case, run against the underlying primitive layer in one shot
— not as two round-trips through the effect boundary.

## The user-facing story

From the point of view of someone adding a capability, the workflow is
meant to be exactly this and nothing more:

a. **I want that functionality, so I import those functions from the
   effect module.** No storage internals to learn — see `STRUCTURE.md`'s
   "erring toward specificity" section for the parallel argument about
   why agent authors shouldn't need to know `Storage.*`.
b. **The compiler tells me to add the effect to my function's `Members`
   list** — and transitively, to every caller above it, all the way up to
   wherever an interpreter is eventually supplied.
c. **The interpreter goes wherever it's logical, not necessarily the main
   stack.** Most of `ContentEffects.hs`'s interpreters compose directly
   into `Storyteller.Core.Runtime.runStoryGit` (git already backs
   everything). But an interpreter that needs extra data an app's main
   stack doesn't carry (a cache, a rate limiter's own state, per-request
   config) is legitimately wired closer to where that data lives —
   `Storyteller.Core.Context.runContextValue` interprets its four
   content effects fresh, per call, rather than once globally, because
   the branch it's scoped to isn't known until the call site (see that
   module's own redesign history in `[[project_mcp_export_effect_boundary]]`
   for why a global wiring was actually wrong here, not just less
   convenient).
d. **Recoverable errors don't get hand-rolled at the call site.** See
   below.

This is also the whole answer to "is this portable to a different
backend": a backend only has to write interpreters for the effects it can
honestly support. Anything requiring an unsupported effect simply fails to
compile *for that backend* — there is no separate runtime capability check
to write, because the type is the check.

## Error handling: fix at the interpreter/interceptor level, not the call site

Agent and filter functions should read as pure happy path — see
`../runix/ERRORHANDLING.md` in full, this project follows it directly, not
as an aspiration. In effect terms:

- A GADT constructor can carry a richer return type (`Either`/`Maybe`) than
  what most callers should see — `BranchResolve`'s `ResolveBranch ::
  BranchName -> BranchResolve m (Maybe Core.ObjectHash)` is exactly this:
  the raw send exposes absence, but a caller that has nothing useful to say
  about a missing branch should get a convenience wrapper that collapses
  it to `Fail` instead of matching on `Maybe` everywhere it's used.
- A **fixing** interceptor sits between the caller and the interpreter,
  sees every request, and can repair a failure into a valid value without
  the caller ever knowing anything went wrong (ERRORHANDLING.md's
  `intercept`-based `withFileDefaults` example). This is where recovery
  belongs when the right context to fix things — a cache, a default, a
  retry policy — lives at a different layer than either the call site or
  the interpreter.
- `Fail` is for the genuinely unrecoverable remainder, and it can be
  caught locally (`runFail`) at whatever level actually has a sensible
  fallback ("skip this notification silently"), without forcing every
  intermediate function to thread `Either` through its own signature.

This doesn't compete with `Storage.Core.StoreT`'s own existing support for
defaults and conditionals inside a batched computation (see "Batch the
primitive" above) — those two mechanisms operate at different scopes and
both stay. **Most errors are not fixable by any information available
anywhere**, because they're the natural result of bad input, bad state, or
a logic error — failing is the *correct* outcome for those, not a gap to
paper over with a cleverer interceptor. `intercept`-based fixing earns its
keep only for the narrow, genuinely-recoverable class: retrying an LLM call
that returned a syntactically successful but malformed response,
materializing a missing file from a template, and similar cases where a
valid substitute value really does exist and the fix is legitimate rather
than papering over a bug. Reach for it there; don't reach for it as a
general substitute for `Fail`.

The test for whether a new effect's design is right: does the exported
library function read as an ordinary, unconditional call — `charactersPresent
path`, `journalWindow ... path lookback maxOut padding` — with no
error-shaped noise in the agent code that calls it? If an agent needs a
`case` on the result to do something other than propagate failure, that's a
sign the fix belongs in an interceptor between the agent and the
interpreter, not inline in the agent.

## Composing the whole vocabulary

Once several effects are naturally used together against the same backend,
give callers one combinator that discharges all of them at once — not so
callers are *forced* to take the bundle (they still request the individual
effects, per function, in their own `Members` lists), but so wiring an
interpreter stack for "the whole vocabulary, one branch, one backend"
doesn't mean writing out seven `.`-composed lines by hand at every call
site and keeping them in sync. `ContentEffects.hs`'s `runContentEffectsGit`
is this: it composes the seven branch-scoped interpreters (everything but
`BranchResolve`, which has no branch to scope to and is wired once,
project-wide, separately). A different backend would supply its own
equivalent, discharging whichever subset it can honestly back — there's no
requirement that a second backend's combinator cover all seven.

## Testing an effect

If an effect is easily mockable, or has an honest pure implementation (no
real I/O needed to behave correctly — an in-memory map standing in for
tick history, say), that test interpreter belongs in the effect's own
module, alongside the real one, the same way `Git.Mock` sits next to the
real git interpreter for `test/`'s existing `runCWT`/`runEdit` pattern.
Write it as part of adding the effect, not as an afterthought once
something breaks.

If it isn't honestly mockable that way (an LLM call, anything where a fake
implementation would test the fake rather than real behavior), it relies on
mocking further up the stack instead — up to and including the
integration-test pattern already used elsewhere in this project: caching
real responses once and replaying them (see the `agent-integration` journey
tests). Don't invent a third option (a "plausible-looking" hand-written
stub for something that can't be genuinely mocked) just to have something
local to the module — that produces exactly the false-green risk
`CLAUDE.md`'s testing section already warns about.
