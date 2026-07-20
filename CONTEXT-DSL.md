# Context DSL

Status: **implemented.** `Storyteller.Context.DSL.{AST,Parser,Compile,Value,QQ}`, tested in
`test/Storyteller/Context/DSL/{ParserSpec,CompileSpec,QQSpec}.hs`. This document was
originally a pre-implementation design spec; it's now the reference for what actually
exists, trimmed to essentials — the extensive per-decision rationale that guided the
original design lives in git history, not here.

## Purpose

Most useful, project-specific context-assembly logic (voice-drift checks, invented-calendar
math, a living glossary, ...) is too specific to ship as a built-in feature, but writing each
one as a hand-rolled Haskell agent is real engineering work per idea. This DSL lets a
power-user express **"what context is showable for this call"** without writing Haskell.
It's deliberately not a general-purpose scripting language — see Non-goals — and it isn't a
prompt-templating language either: `PromptStorage` remains the separate owner of system
prompts. The DSL decides *what's* showable; framing/system-prompt text stays outside it.

**The primitive set is frozen by design.** Eight primitives plus a closed filter vocabulary
are the whole language. Almost all extensibility happens through filters or through ordinary
function parameters (`Binding`, below) — not new syntax.

## Value model

```haskell
data Value = Value
  { valueDefault :: Action [Message]
  , valueEntries :: [(Name, Action Value)]
  }

data Message = FileRead FilePath Text | User Text | Assistant Text
```

One type. A leaf is just a `Value` with empty `valueEntries` — there is no separate leaf
type, list type, or path type. A for-loop variable, a glob result, a function's return
value, a character-name parameter: all the same type, in different positions.

**`valueEntries` is an ordered association list, not a `Map`.** Order is a real, preserved
property — construction order by default (`as`-export declaration order, a branch tree's own
order), freely reassignable by a filter or a loaded function (`sortBy`, a non-lexical chapter
ordering). This is deliberate: a `Map` would collapse order to key-sort regardless of how
entries were actually produced, which is exactly what made non-lexical ordering awkward
before `valueEntries` became a list.

**Three ways a `Message` gets constructed:** `read` produces `FileRead` (role deliberately
undecided — what it becomes is an interpretation decision, out of scope here); a bare string
literal produces `User text`; `>"text"` produces `Assistant text`. `<expr` isn't a fourth way
— it re-tags whatever `expr` already evaluated to as `User`, without constructing new text.
Filters and interpolation that need plain text (`%name%`, `charA | charname`) work on
message content, ignoring role.

**Quoting distinguishes a literal string from a live glob, and it's meaningful, not
stylistic.** `"**/*"` is inert text. `**/*` (bare, unquoted) pattern-matches against the
current Reader scope's entries. Only the unquoted form triggers matching; a glob pattern can
never be computed from a `read` result and then matched.

**`Action` is `Core.StoreT`-shaped, not a bare monadic value.** `Value`'s fields are
`Action`s, not already-run results, so forcing a leaf is just running one:

```haskell
newtype Action a = Action
  { runAction :: forall m. (Core.StoreM m, MonadBranch m) => Core.StoreT m a }
```

Every real caller already runs the DSL from inside a storage transaction positioned at some
commit, so the Reader scope's own bootstrap (`currentScope`) just reads that ambient
position — no branch name or lookup needed for "run in whatever I'm already in."
`MonadBranch` (`resolveBranch :: BranchName -> m (Maybe ObjectHash)`) is a separate
constraint from `Core.StoreM`, not folded into it — a real backend satisfies both, but
nothing here assumes that pairing, matching how `StoryStorage` and `Core.MonadStore` are
already kept separate throughout this codebase.

## Primitives

Eight, plus a closed filter vocabulary. Nothing else.

1. **Function definitions** (Nix-style curried heads): `charname: characterId: ...body...`.
   A definition with no head is a 0-ary function — an ordinary value. Functions are just
   files (or local `let` bindings) — no separate `function`/`def` keyword, no registry
   distinct from the filesystem.

2. **Bare statement → emit.** Any expression standing alone has its `default` forced and
   appended to the enclosing writer target — string literals, `read` results, function
   calls, all handled identically. `"hello world"` is a complete, valid program.

3. **`read <path>`.** Resolves one literal path against the current Reader scope's tree.
   Never takes a glob or a predicate — multiplicity lives in `for` and filters.

4. **`x = <body>`.** Evaluates `<body>` against a fresh writer target, binds the result to
   `x`. Local, shadowable, never part of the enclosing scope's exports.

5. **`as "name": <body>`.** Like 4, plus: inserts the result into the enclosing scope's
   `valueEntries` under `"name"`. Duplicate names within one definition are a compile-time
   error.

6. **`in <expr>: <body>`.** Evaluates `<expr>` to a `Value`, runs `<body>` with the Reader
   scope replaced by it. The writer target is untouched — bare statements inside still emit
   to whatever the enclosing scope was writing to. Crossing branches always goes through
   `in`, never through a path: `in (charname | branch): read "sheet.md"`. Reader scope
   resolution is dynamic, never captured lexically — a local binding stores the operation,
   not a snapshot of the ambient tree. One consequence worth knowing: narrowing only ever
   goes one direction; capture a wider scope as a plain value *before* narrowing
   (`root = **/*`) if a nested call needs to reach outside a `for`'s own glob.

7. **`> <string>`.** Produces `Assistant text` instead of the `User text` a bare literal
   would.

8. **`< <expr>`.** Symmetric counterpart — re-tags whatever `<expr>` evaluates to as `User`.
   Most useful on `read` (role-undecided `FileRead` → `User`): `< read notes.md`.

**Calling a function is not a ninth case** — it's "evaluate a body against a fresh writer
target," identical to what 4/5 already do. A callee's own `as`-exports never leak into the
caller's `valueEntries`; they stay nested, reachable only via `in callResult: read "name"`.

**`for f in <glob>: <body>`** is not a separate primitive — each matched path folds `<body>`
into the *enclosing* accumulator, exactly like `in` does (rule 6's "writer target untouched"
extended across iterations). The loop variable is a fresh `Value` with empty entries (never
the richer `Value` sitting at that key in the source tree) — deliberate, to preserve
laziness: a loop that only wants filenames (`f | filewithname`) never forces content.
`for`'s source is always a literal glob — never filter-derived — since there's no list type
independent of what a glob produces.

## Filters and `Binding`

```haskell
type DSLFilter = Value -> [Value] -> Value
```

Filters (`expr | filterName(args...)`) are the closed, host-provided vocabulary
(`coreFilters`) — not definable inside the DSL, never threaded as a parameter. They're
**synchronous and pure**: a filter can defer forcing into the *result's own* `Action` fields
(`fJoin` builds a new deferred recipe over its children), but it can never force anything
itself to make an in-place decision — no filter can read a file's content to decide how to
sort, because deciding *now* would need `Action`/storage access a plain `Value -> [Value] ->
Value` doesn't have. `branch` is the one filter-shaped operation that's dispatched outside
`coreFilters` for exactly this reason (resolving a branch name is inescapably a storage +
`MonadBranch` operation).

**A parameter isn't restricted to a leaf value — `Binding` is the real currency crossing a
`Definition`'s boundary, and a plain value is just its 0-arity case:**

```haskell
data Binding = Binding Int ([Action Value] -> Value -> Action Value)

bval :: Action Value -> Binding                          -- the ordinary "just a value" case
fn1  :: (Action Value -> Action Value) -> Binding         -- a host-supplied function, 1 arg
fn2  :: (Action Value -> Action Value -> Action Value) -> Binding
```

This is how a case like invented-calendar date arithmetic — too story-specific for a filter,
too complex for the DSL's own primitives — enters the language: an agent passes the actual
Haskell function in as an ordinary argument. Unlike a filter, a `Binding`'s own function runs
in `Action`, so it *can* read storage (`liftStore`, arbitrary `MonadStore` operations,
including loading and running another `Definition` from stored text) — the real boundary
isn't "must be pure Haskell," it's "confined to `StoreM`/`MonadBranch`, same ceiling as
everything else the interpreter does." No LLM call, no effect outside storage reads, is
reachable from inside a `Binding` — `Action`'s own `forall m. (StoreM m, MonadBranch m)`
quantification makes that a type error, not a convention.

## Embedding from Haskell

`[dsl| ... |]` (`Storyteller.Context.DSL.QQ`) parses its contents *at compile time* — a
malformed definition is a GHC error at the quote's own location — and splices a **curried
Haskell function of exactly the source's own arity**, one `Binding` parameter per declared
parameter, returning `Action Value` once fully applied:

```haskell
injuryStatus :: Action Value
injuryStatus = [dsl| as "injury": read status/injury.md |]

castingStatus :: Binding -> Action Value
castingStatus = [dsl|
  charname:
    in (charname | branch): read "status/casting_log.md" | orifempty "no casting today"
|]
```

GHC checks arity at every call site (`castingStatus (bval charArg)`), not
`compileDefinition` at runtime. The scope is always whatever's ambient when the returned
`Action` runs (`currentScope`) — there's no way to splice in a different scope from the
quoter; anything that needs one calls `Storyteller.Context.DSL.Parser.parseDefinition` and
`compileDefinition` directly, the same as loading a `Definition` from stored text at runtime
(a project's own override of a conventional path, say) and running it via `runDefinition`.

A `Definition` (the parsed AST — `defParams :: [Name]`, `defBody :: Block`) is the one thing
about this whole pipeline that's actually inspectable and rewritable from Haskell: it's plain
data (`Eq`, `Show`, `Lift`), so a real static call graph (which names an `EIdent`/`EApp`
references) is ordinary tree traversal, and it's provably acyclic — `Env` bindings are
non-recursive `let`, so no self-reference is expressible. Once compiled to `Action`/`Binding`,
that inspectability is gone — both are opaque Haskell closures, same as any other function
value. What survives compilation is *structure*, not *derivation*: `valueEntries`'s keys are
plain data, forced for free, so you can still pick apart which named children a compiled
`Value` has (and reuse any of them as an ordinary parameter to a different `Definition`)
without ever being able to ask how their content came to be.

## Non-goals

- **Not Turing-complete, provably.** No `if`, no recursion (`let` bindings can't see their
  own name), no `while`. `for` only ever ranges over an already-finite, already-materialized
  glob match — never something computed or self-referential. Every evaluation terminates by
  structural induction over the parsed AST; the one way to reopen that is a host-built
  primitive that loads and runs a `Definition` sourced from file content whose own body
  loads-and-runs that same path — an explicit, opt-in capability, never ambient.
- **No user-defined filters**, no filter-manufactured list for `for` to iterate.
- **No direct story mutation.** The only effect is `read` (or whatever a filter/`Binding`
  does internally, within `StoreM`/`MonadBranch`) — never writing a tick.
- **No query-predicate syntax** in `read`/`for`. Latest-of-type, tag-filtered, sorted,
  limited: all filters over a glob's output, not primitive grammar.

## Open / deferred

- **Wired into `chatWriter` for lore/chapters/style** (`Server.Writer.File.chatWriter`, via
  `resolveContextQuery "context.main"`), but **character context still isn't** —
  `activeCharacterContext`/`charSummaryWithJournal` (`Server.Writer.File.hs`) is the
  hand-written path both `chatWriter` and `roleplayWriter` still use for characters, and
  `roleplayWriter` hasn't been touched at all (still 100% `gatherFileContext`). The DSL side
  is ready for this: `Storyteller.Context.DSL.Library.contextCharacter` produces a rich,
  five-bucket character context (`"sheet"`, `"blurb"`, `"full"`, `"journal"`,
  `"journalFull"`, default = blurb) that every consumer (ambient scene context,
  `askCharacterAgent`, `roleplayAgent`) is meant to share and pick buckets from — see its own
  Haddock. `"journal"`/`"journalFull"` are the curated-vs-uncurated pair: same underlying
  file, one deduped via `journalDelta`, one verbatim, so `askCharacterAgent`/`roleplayAgent`
  (which currently want full self-knowledge, not ambient curation) have a bucket to read once
  they move over. Unused buckets are genuinely free — `Value`'s own entries are `Action`s, not
  already-run results, and `as "name": body` stores that `Action` via a plain `let`, never
  forcing it (`Compile.hs`'s `runStmts`, the `SAs` case) — so `activeCharacterContext` (which
  only ever reads `"sheet"`/`"full"`/`"journal"`) never resolves `"journalFull"`'s branch hop
  at all. Swapping the Server-side callers over is the next real step, not a redesign.
- **`sortBy`, `name`, `abstract` are implemented** (`Storyteller.Context.DSL.Compile`).
  `name`/`abstract` extract a Markdown document's leading `# Title` and its opening
  paragraph by convention (no LLM) — this is what backs `character.blurb`
  (`Storyteller.Context.DSL.Library.characterBlurb`): "the acquaintance-level summary" turned
  out to be already-stored data (the sheet's own header + first paragraph), not a
  summarization problem, so the `Pending`-message idea below never became necessary for it.
  `summarize`, `draftDefinition`, `extractProperNouns`, `whereType`, `whereTag` remain
  `fNotImplemented` stubs — write them only once something actually needs them (a real
  content-analysis-driven `summarize` is the one that still plausibly wants the `Pending`
  design below; the other three are speculative until a concrete caller shows up).
- **A journal-delta primitive exists, but as a host-supplied `Binding`, not a filter.**
  `Storyteller.Context.DSL.Compile.journalDelta` wraps `Storage.Tick.recentAtomsOf` (drops
  journal entries byte-identical to whatever they reference, keeps genuinely-changed ones
  plus padding). It can't be a plain `DSLFilter` — `recentAtomsOf` needs real `StoreT`
  access — and it can't rely on an enclosing `in (charname | branch): ...` either, because
  `in`/`branch` only ever redirect the Reader-scope `Value` `read`/`for` glob against; they
  never reposition `Core.StoreT`'s own ambient scope (`headHash`). So it resolves the
  character's branch and hops there itself via `Core.readAt`, the same primitive
  `Storyteller.Common.Summary` uses for a historical peek that mustn't disturb the caller's
  position. This same constraint bit `character.blurb` during design: a `bval`-wrapped
  0-arity `Action` (an *already-scoped* Binding, not a re-resolving one) doesn't dynamically
  inherit an enclosing `in`'s narrowed scope either — there's no dynamic-scope crossing
  between two separately-compiled `Definition`s, only within one definition's own body (see
  `Binding`'s own Haddock on `bval`). Any future cross-branch host function needs to take the
  branch identifier as its own explicit argument and do its own `in`/`branch`-equivalent
  resolution, the same way `journalDelta` and `characterBlurb` both do now.
- **A tagged "needs postprocessing" message** (`Pending PostProcess Text`, a fourth `Message`
  constructor) is still undesigned — still the plan if/when a real `summarize` filter needs
  an LLM call deferred past `Action`'s own `StoreM`/`MonadBranch`-only ceiling.
- **File-level ticks, not flattened content.** `read` only ever resolves to a file's current
  flattened blob. Exposing the tick-level edit history underneath as a general DSL primitive
  is still deliberately not designed — `journalDelta` sidesteps this by being a purpose-built
  host function for exactly one convention (`journal.md`) rather than a general primitive.
- **A branch-hosted, override-with-fallback function library.** `embed`-style helpers
  (`renderEmbeddedFile`'s `<context-file path="...">` convention, shared across three
  existing agents) fit naturally as `Definition`s stored at a conventional path with a
  compiled-in default, loaded and cached the same way `PromptStorage` overrides work today
  — not yet built.
- **The `Contexts`-branch convention** (where definitions live, parameter-declaration
  relative to `.llmsettings.yaml`) is unresolved.
- Whether `FileRead`/`User`/`Assistant` needs a fourth `System` case is unconfirmed —
  current assumption is DSL output always splices into an existing system-prompt-anchored
  conversation, never authors the system prompt itself.

## Worked examples

**Injury/status continuity** — no parameter; "which character" is answered by whichever
branch the caller already positioned the interpreter at:

```
as "injury": read status/injury.md
read "injury" | orifempty "not injured"
```

**Chekhov's-gun list** — full-content export alongside a name-only listing, via the same
`as`/`for` combination, no separate "full"/"short" primitives needed:

```
as "open":
  for f in tracking/**.md:
    as f: read f

"open threads:"
for f in tracking/**.md:
  f | filewithname
```

**Invented calendar** — a `Binding` parameter for logic too story-specific for a filter, too
complex for the DSL's own primitives:

```
calendar_context = dateMath:
  as "rules": read "lore/calendar_system.md"
  dateMath (read "calendar/log.md" | latest(1))
```

called as `calendarContext (fn1 emberfallDateMath)` from Haskell, where
`emberfallDateMath`'s own choice of calendar system is the caller's, never baked into the
definition.
