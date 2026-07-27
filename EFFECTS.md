# Adding functionality: effects, and when not to reach for one

This is the convention to follow whenever agent, DSL, or server code needs
some new capability.

**The short version: write a function.** Put it in the module that owns the
concept, give it `Members '[BranchOp branch, ...] r` for whatever it
genuinely touches, export it. That is the default and it covers most new
work. An effect is a specific tool for a specific problem, and the rest of
this document is about recognizing that problem.

This is a correction, not the original position. An earlier pass built
`Storyteller.Core.ContentEffects` — seven narrow effects (`TreeAccess`,
`Presence`, `JournalAccess`, `ConversationAccess`, `FileTicks`,
`Summarized`, `BranchResolve`, plus `Cast`) plus module-local
`Summarization` and `TasksSync` — on the theory that each named capability
should be independently swappable. All of them are gone now. The reasoning
that removed them is in "When an effect earns its place" below, and it is
worth reading before adding anything, because the shape they had is an easy
one to reach for again: every one of them looked correct at the time.

## The default: a function in the module that owns the concept

`Storyteller.Writer.Conversation` owns "a file's ticks read as a
conversation." `Storyteller.Writer.Journal` owns "a curated recent slice of
a journal." `Storyteller.Writer.Cast` owns "every character branch and what
its sheet says." Each is a module, a domain type where one is warranted, and
a couple of functions over `BranchOp`/`Branches`. No GADT, no `makeSem`, no
interpreter, no discharge site.

What you get from this is what the effects were actually delivering:

- **A name for the concept**, which is what stops the next person
  re-implementing it. Modules do this for free.
- **A domain type at the boundary** — `Turn`, `CastMember`,
  `JournalCuration`, `Set Character`. This matters more than the effect
  question and is covered in "Native types at the boundary" below.
- **One place the derivation lives.** `turnsFromFileTicks` is the only code
  in the tree that decides what a `"prompt"` tick means.

What you don't get is a compile-time guarantee that a caller went through
your function rather than calling `runStorage @branch (...)` itself. That
guarantee was largely notional even when the effects existed — see
"What we gave up, honestly."

## When an effect earns its place

Ask this, and be strict about it:

> **Does the interpreter need capabilities its callers don't already hold
> — and won't end up holding anyway?**

That last clause is load-bearing. `Cast` passed the first half: its
interpreter needed `Branches` + `StoryStorage`, which `trackPresenceFor`
didn't name. But `Branches` appears in ~150 signatures across `src`+`app`;
it is effectively ambient, and every real caller acquired it one layer down
regardless. So `Cast` wasn't bounding authority, it was adding a row entry
that already implied itself. An effect wrapping capabilities the caller
acquires anyway is a rename with ceremony.

Beyond that test, three things genuinely justify a GADT:

1. **A second interpreter that actually exists.** Not one a hypothetical
   backend might write — one in this repository, today.
   `PromptStorage` has `interpretPromptStorageFS` and
   `interpretPromptStorageMap` (tests run without git). `Snapshot`,
   `FileSystem`, `LLM` likewise. This is the strongest justification and
   the easiest to check.
2. **A real interception point.** Somewhere in the tree, an `intercept`
   sits between callers and the interpreter. Note where these actually
   are: the `Git` undo log, the `LLM` budget, the `FileSystemWrite` path
   filter. Every interceptor in this codebase attaches to a
   runix-level effect; none has ever attached to a domain effect. If you
   are adding an effect *so that* it could be intercepted later, you are
   guessing, and the guess has a 100% miss rate so far.
3. **Per-scope state or a scoped resource.** `Branches` is `Scoped`, and
   entering a branch is a real lifecycle with a beginning and an end.
   `ContextStorage` carries per-request override state. These are readers
   and resource brackets, which is a different thing from a capability
   vocabulary — see "The remaining effects" below.

**And one thing that argues strongly against a GADT:** if the operation
takes an effectful callback (`([Text] -> Sem r Text)`), an effect makes it
expensive. A higher-order effect constructor needs `interpretH` and the
`Tactics` vocabulary — `bindT`, `pureT`, `getInspectorT`,
`getInitialStateT` — which is the hardest part of Polysemy to get right and
the part that produces the most confusing type errors. As a plain function,
the callback is just an argument. Agent code is largely
effectful-callback-shaped (`tieredPass` takes the reduce step,
`syncTasksWith` takes the generation hook), so this comes up constantly.
Note that `BranchOp` is deliberately first-order for exactly this reason:
one `RunStorage` constructor carrying a closed `StoreT` computation, no
`interpretH` anywhere.

## The remaining effects, and what each is for

Two different things live in this row and it helps to name them separately.

**Doors — the ones that actually abstract a backend:**

- `BranchOp branch` — "I am working in *the* branch scope that is open."
  One constructor, `RunStorage :: (forall n. StoreM n => StoreT n a) ->
  BranchOp branch m a`. Its interpreter holds git; no caller does. This is
  the real abstraction boundary in this codebase, and almost everything
  that was deleted had been stacked on top of it.
- `Branches` — `Scoped Anchor (BranchOp Visited)`, the door into *another*
  scope, by name (`ByName`) or by position (`ByPosition`). A row fixes how
  many scopes exist at compile time, so visiting a branch you can only name
  at runtime needs this and can't be done with `BranchOp` alone.
- `FileSystem`/`FileSystemRead`/`FileSystemWrite` — the primary way file
  content is reached. A function written against these runs against a
  branch, a snapshot, a real directory or a test filesystem without knowing
  which. Prefer them even when a positioned read would be cheaper; see
  `Storyteller.Writer.Cast.knownCast`'s Haddock for that trade made
  explicitly.
- `StoryStorage` — branch enumeration and creation. Project-global; nothing
  to scope it to.
- `LLM role` — genuinely several providers.

**Readers and brackets — carriers of implicit parameters, not
abstractions:**

- `ContextStorage`, `PromptStorage` — per-request overrides and prompt
  defaults. `PromptStorage` also has the second-interpreter justification.
- `Snapshot`, `Timetravel branch` — positioned reads.
- `Undo`, `Splitter` — pluggable policy.

Adding to the first group is a real design decision. Adding to the second
is cheap and usually fine — a reader effect that carries a parameter you'd
otherwise thread through forty signatures is earning its keep even if
nothing will ever swap its interpreter.

## Native types at the boundary

This survives from the original document unchanged, because it was never
really about effects — it is about what your function returns.

A caller should be looking at ordinary Haskell types for the concept —
`Set Character`, `[Turn]`, `[CastMember]`, `[Text]` — never storage
vocabulary leaking through. `conversationTurns` returns `[Turn]`, not
`[FileTick]`, because `ftKind`/`ftFields`/the hide flag are things a caller
asking "what was said" should not have to know. `journalWindow` takes a
named `JournalCuration` record rather than three same-typed `Int`s that are
easy to transpose.

The test: **if a caller needs to know something about how the backend
stores data in order to use the return value** — that it's a hash, that
it's addressable, that two of them compare by object identity rather than
semantic equality — the boundary is leaking.

The worst instance of this in the old design is worth recording because it
shows what the leak costs. `BranchResolve` returned a raw
`Storage.Core.ObjectHash`, and `journalWindow` took a `Maybe ObjectHash`
position to consume it. Every caller crossing branches had to resolve a
name to a hash and carry it. That is "the position of a branch is a hash of
its content" — an assumption a plain directory can't satisfy — appearing in
two signatures and one parameter. It was noted as a FIXME with a proposed
opaque position type. The actual fix turned out not to need a new type at
all: **enter, don't carry.** `withBranch (BranchName ("character/" <>
ident)) $ journalWindow "journal.md" curation` — no hash, no parameter, no
`BranchResolve`. Both the effect and the leak went away together.

Reach for that first. If something wants to hand you a position, check
whether it should be handing you a scope.

## Batch the primitive; keep the call intent-shaped

A function should say *what* the caller wants ("this file's recent journal
window") and do it in one `runStorage` dispatch, even when the `StoreT`
computation inside walks tick history, filters, and pads a window. Don't
relay each primitive step back out through `Sem r`.

`BranchOp`'s `RunStorage` is what makes this cheap: one send, one closed
`StoreT` computation, several git-level operations run to completion inside
it, only the final `[Text]` crossing back out.

Functions have one advantage over effects here that we are not yet using.
Two effect constructors can never be fused into one transaction — they're
separate dispatches by construction. Two functions can: `pendingSummary`
followed by `recordSummary` is still two `runStorage` calls today, but it
is now *possible* to write it as one. If you find yourself doing several
dispatches that logically form one transaction, that's now a fixable
problem rather than a structural one.

The rule of thumb: if two steps always happen together, are never
individually intercepted, and nothing meaningful could happen in between
them from a different part of the program, they belong inside one `StoreT`
computation.

## Error handling: fix at the interpreter/interceptor level, not the call site

Agent and filter functions should read as pure happy path — see
`../runix/ERRORHANDLING.md` in full; this project follows it directly.

- A function can carry a richer return type (`Either`/`Maybe`) internally
  than what most callers should see, and export a wrapper that collapses it
  to `Fail`. A caller with nothing useful to say about a missing branch
  shouldn't be matching on `Maybe` everywhere.
- A **fixing** interceptor sits between the caller and an interpreter, sees
  every request, and repairs a failure into a valid value without the
  caller knowing (ERRORHANDLING.md's `withFileDefaults`). This belongs
  where the right context to fix things — a cache, a default, a retry
  policy — lives at a different layer than either the call site or the
  interpreter. Note that this requires an effect to intercept, and is one
  of the few genuine reasons to have one.
- `Fail` is for the unrecoverable remainder, caught locally (`runFail`)
  wherever a sensible fallback exists.

**Most errors are not fixable by any information available anywhere** —
they're the natural result of bad input, bad state, or a logic error, and
failing is the *correct* outcome. `intercept`-based fixing earns its keep
only for the narrow, genuinely-recoverable class: retrying an LLM call that
returned a malformed response, materializing a missing file from a
template. Don't reach for it as a general substitute for `Fail`.

The test for whether a new function's design is right: does it read as an
ordinary, unconditional call — `conversationTurns path`,
`journalWindow path curation` — with no error-shaped noise in the agent
code calling it?

## What we gave up, honestly

Three things, and one of them is real:

**Signalling.** `Members '[Presence branch, JournalAccess branch]` told you
what a function touches at a glance; `Members '[BranchOp branch]` tells you
"storage, somehow." This is a genuine regression when reading an unfamiliar
agent, and it is the cost paid daily rather than hypothetically. What
replaces it is import lists — weaker, because a row is type-checked and an
import list is only read.

**Alternate backends.** `ConversationAccess` was the one case with a
plausible alternate implementation: a SillyTavern-style chat log natively
*is* turn-shaped, no tick history involved. It's now a DAG walk. The
mitigation is that `Turn` survived as a type and `turnsFromFileTicks` is
pure, so the seam is still there — re-effectifying is small and local
*because the type exists*. **This is the general lesson: the type is the
seam, not the GADT.** If you're worried about a future backend, define the
domain type carefully and keep the pure derivation separate. That buys you
most of the optionality at none of the cost.

**Mandatory vocabulary.** Nothing now stops an agent calling
`runStorage @branch (Tick.fileTicksOf path)` raw. But this was already
failing: while `FileTicks` existed, four sites
(`Writer.Agent.Write`, `Writer.Presence`, `Server.Writer.File`,
`Server.Writer.Library`) wrote that exact line raw anyway, and
`Member (FileTicks branch) r` never appeared in a single signature because
every caller discharged it locally. The discipline was always module
discipline; the effect was charging ceremony for enforcement it wasn't
providing.

And one thing that got *better* than expected: the effect boundary was
actively hiding duplication. `Presence` was a rename of
`Writer.Presence.activeCharactersFor`, which already existed — the effect
sat beside the function doing the same thing, and the layering made two
owners look like one concept in two layers. `ConversationAccess`'s own fold
was `Chat.historyFromFileTicks` transcribed into a second message type.
Declaring a concept in a GADT makes it *look* owned, which is precisely
what stops anyone checking whether it already was.

## Testing

If something has an honest pure implementation (no real I/O needed to
behave correctly), test it directly — `turnsFromFileTicks` takes a
`[FileTick]` and returns `[Turn]`, so its whole contract is testable
without a storage stack at all. Extracting the pure core is usually the
right first move; see `test/Storyteller/ChatSpec.hs`.

For anything needing storage, the harness already exists: real storage
effect stack, in-memory git (`Git.Mock`), plain hspec — see the
`runCWT`/`runEdit` runners in `CommitWorkingTreeSpec`/`EditSpec` for the
pattern to copy.

For an effect with multiple interpreters, the test interpreter belongs in
the effect's own module, alongside the real one. Write it as part of adding
the effect, not once something breaks.

If it isn't honestly mockable (an LLM call, anything where a fake would
test the fake), mock further up the stack — up to the integration-test
pattern of caching real responses once and replaying them (see the
`agent-integration` journey tests). Don't invent a third option, a
plausible-looking hand-written stub for something that can't be genuinely
mocked, just to have something local to the module — that produces exactly
the false-green risk `CLAUDE.md`'s testing section warns about.
