# Context DSL

Status: **design spec, not yet implemented.** This document records the language design
arrived at through direct exploration of the storage engine's actual capabilities, then
pressure-tested against eight concrete, project-specific scenarios (see "Worked examples").
Every primitive below exists because a scenario needed it, not because it seemed like a
nice thing for a DSL to have. See "Non-goals" for what was deliberately cut.

## Purpose

Storyteller's agents already compose freely in Haskell — that's not the gap. The gap is
that most useful, project-specific automation (voice-drift checks, invented-calendar math,
a living glossary, relationship tracking between two named characters, ...) is *too
specific to ship as a built-in feature*, but writing each one as a hand-rolled Haskell agent
is real engineering work per idea. This DSL exists to let a technical power-user express
**"what context is showable for this call"** without writing Haskell — nothing more. It is
deliberately not a general-purpose scripting or automation language; see Non-goals.

**This is context *assembly*, not a prompt-templating language.** `PromptStorage` remains
the separate, existing owner of system prompts (`agent.writer`, `agent.chat`, ...) — this
DSL doesn't replace or subsume it. Some authored framing text is expected and fine (raw
data often needs a label to be intelligible to whatever reads it — `"Voice profile for
%charname%:"` in the voice-drift example is exactly this), but the goal is never "write a
good prompt," it's "decide what's showable." Interestingly, the parameter/lambda calling
convention designed here (see "Function definitions," below) might turn out to be a good
fit for `PromptStorage` templates too, someday — but that's a separate, later question, not
something this document takes a position on.

**The point of all of it is definitional reuse.** "How to summarize a character" should be
defined exactly once, even though it might have several answers depending on how much
detail a given call wants — that's what the `as`-named-export/resolution-tier pattern (see
the Chekhov's-list worked example) and the eval/interpretation split (see Implementation
strategy) are actually *for*: letting "how detailed" be a decision made by the caller or
the interpreter, never baked into the definition itself.

**The core primitive set is frozen by design.** Every DSL's failure mode is the same one:
it gets extended, one reasonable-seeming case at a time, until it's a worse version of a
real programming language. The defense here isn't vigilance, it's structural — the eight
primitives plus filters are the whole language, and **all extensibility happens through the
filter vocabulary, almost never through new syntax** (the two deliberate exceptions, `>` and
`<`, are justified precisely in "primitive 7" and "primitive 8," below). A scenario that seems to need a new keyword
(set operations on Values were considered and rejected this way — see the filter section)
almost certainly just needs a new filter instead: filters can be arbitrarily rich, effectful,
even LLM-backed, internally, precisely so the primitives themselves rarely have to grow to
accommodate what any one of them does. When conflict-resolution or combination policy has
no single obviously-correct answer (e.g. what a union of two Values should do when they
disagree at the same key), that's a signal it belongs in a filter's implementation — which
can offer several named alternatives — not in core language semantics that would force one
answer on everyone.

## Value model

There is exactly **one type**: a **Value**, a lazy pair

```
Value = { default :: Thunk [Message], entries :: Map Name Value }
Message = FileRead Path Text | User Text | Assistant Text
```

`default` is a sequence of role-tagged messages rather than plain text, so that "what's in
the context" can carry fine-grained speaker/provenance information all the way through
instead of collapsing to an undifferentiated blob at the last moment. Three ways a
`Message` gets constructed, and only three:

- **`read`** produces `FileRead` — deliberately *not* pre-assigned a conversational role.
  What a `FileRead` becomes (a simulated tool-call/result pair, an inlined message, plain
  text) is entirely the interpreter's decision — see "Interpretation is not part of this
  spec." This is also where the tool-mount consumption mode gets its role-tagging for free:
  when an LLM actually explores a mounted tree itself via real tool calls, the tool-call/
  tool-result pair *is* the interpretation of the `FileRead`s it would otherwise produce.
- **A bare string literal** (`"text"`) produces `User "text"` — the sensible default, since
  most authored content in a context reads as "here's what's relevant," not as something
  attributed to the assistant.
- **`> "text"`** produces `Assistant "text"` — for the deliberately rarer case of authoring
  content that should read as the assistant's own voice (seeding a response, building a
  synthetic exchange). See "primitive 7," below, for why this gets dedicated syntax rather
  than a filter.

Everything else composes from these three the same way it always did — a bare statement's
`default` (now a `[Message]`) still just concatenates onto the enclosing writer target,
list-append instead of string-append, same operation. Filters and interpolation that need
plain text (`charA | charname`, `%chapterPath%`) work on the underlying message content,
ignoring role — role only matters to whichever interpreter eventually consumes the tree.

`<` (see "primitive 8," below) isn't a fourth way a `Message` gets constructed — it never
runs on text of its own — it re-tags whichever of the three ways an *already-evaluated*
expression used, forcing the result to `User` regardless. The "only three" above is about
where a `Message`'s content and role are first decided; `<` (like `>`) only ever changes
which role a message already has.

A leaf is just a Value with an empty `entries` map — there is no separate leaf type. There
is no separate list type, no separate "Path" type, no separate record type. Everything —
function parameters, loop variables, glob results, read results, function return values —
is this one type, used in different positions.

**What we colloquially call a "Path"** (a for-loop variable, or a simple identifier
parameter like a character name) is just a Value whose `entries` happen to be empty because
nothing has been read through it yet, and whose `default` is an already-known `[Message]`
— typically a single `User`-tagged message, since it usually originated from a literal at
some call site. It behaves identically to any other Value for every purpose — filters can
be applied to it directly (`charA | charname`, `f | filewithname`), it can be interpolated
into a string, and it can be passed anywhere a function expects an argument. Nothing
distinguishes it structurally; it's simply the simplest possible member of the one type.
A function parameter is *not* restricted to receiving a bare identifier — passing a fully
computed Value as an argument works identically, since `in`, filters, and interpolation
never cared which they were given.

**A glob result** is the same type too: a Value whose `entries` are keyed by matched path,
each entry's own Value being what `read` of that path would produce (lazily — see below).
Its own `default` is empty, since nothing ever writes to it (a glob is a primitive
producer, not a body).

**Quoting is what distinguishes a literal string from a live glob, and it's meaningful, not
stylistic.** `"**/*"` — quoted — is an ordinary string Value: `default = "**/*"`, `entries =
{}`. It's inert, never pattern-matched, exactly like any other string literal (`read
"some/path"`, an interpolated message). `**/*` — bare, unquoted — is a live glob
expression: `default = ""`, `entries = {...matches...}`. Only the unquoted form triggers
pattern-matching against the current Reader scope; the quoted form is just text. This is
also why a glob pattern can never be *computed* — assembled from a value obtained via
`read`, say, and then matched — matching only ever happens for a pattern written literally,
unquoted, at a fixed position in source. That's not an oversight; it's the same restriction
already stated for `for` under Non-goals (no filter-derived iteration targets), seen from
the pattern side instead of the iteration-target side. Interpolation still works in either
form (`"chapter %n%"` or bare `chapters/%n%/*.md`) — it's a substitution over raw source
text that happens before the quoted/bare fork is even relevant, not something added to
glob's own matching semantics.

Forcing a `Thunk` runs the underlying computation and memoizes the result, keyed by a
content-addressed hash of: the source expression, the resolved Reader-scope tree, applied
parameters, and the transitive set of paths actually `read` during evaluation.

## Primitives

There are eight, plus a closed, host-provided filter vocabulary. Nothing else exists.

### 1. Function definitions (Nix-style curried heads)

```
charname: characterId:
  ...body...
```

A file (or a local binding) with no head is a 0-ary function — i.e. an ordinary value.
Application is space-juxtaposition and is curried: `f a b` applies one argument at a time.

**Functions are just files.** A callable, named, parameterized context is identified
*entirely* by its path — there is no separate `function`/`def` keyword, and no registry
distinct from the filesystem. This mirrors the existing `Prompts` branch convention
(dotted-key-as-path) exactly; a `Contexts` branch holds these files the same way.
Locally-scoped, private helpers don't need a separate mechanism either — `x = a: b: ...`
binds an ordinary (unexported) function value, reusable within the same file, exactly the
way Nix's own `let` binds functions without a separate `def`.

**The function-storage branch is a completely separate address space from story content,
and the DSL has no access to it as data.** `read`/`in`/glob resolve exclusively against the
story tree — whatever `in` currently scopes to, ultimately rooted in whatever the host
supplied at invocation. None of them can ever target, glob over, or read from the
`Contexts` branch itself. Resolving a named function reference (`character charname`) is a
completely different, static mechanism: the identifier is looked up against local
`let`-bindings, or against the fixed, pre-existing set of `Contexts`-branch files — never
through `read`, never dynamically. There is no way for a definition to discover, enumerate,
or construct a callable function at runtime based on story content it just read; the set of
things any given definition could possibly call is fixed and fully known without evaluating
anything. This is what actually closes off reflection/data-driven dispatch as a loophole in
the non-Turing-completeness guarantee — not just "no `if`, no recursion," but the call graph
itself can never vary based on data.

### 2. Bare statement → emit

Any expression standing alone in statement position has its `default` forced and appended
to the *enclosing* writer target. This is uniform — string literals, `read` results,
function calls, local variable references, filtered path references are all handled
identically. There is no separate `print` keyword; `"hello world"` is a complete, valid
program.

### 3. `read <path>`

Resolves a single literal path against the current Reader environment's tree (looks it up
by key, recursively) and returns the Value found there. **`read` never takes a glob or a
predicate query** — multiplicity and filtering live entirely in the filter vocabulary,
applied to what `for` produces. If left bare, `read`'s result is emitted per rule 2.

### 4. `x = <body>`

Evaluates `<body>` against a **fresh, private writer target**, and binds the resulting
Value to `x`. `x` is:
- local and freely shadowable (needed for loop-local temporaries — see Iteration),
- never part of the enclosing scope's `entries`,
- reusable anywhere in the rest of the enclosing body, including as a function if `<body>`
  was itself a function head.

### 5. `as "name": <body>`

Identical to rule 4, *plus*: the resulting Value is inserted into the enclosing scope's
`entries` map under `"name"`. Writing the same literal name twice within one definition is
a compile-time error (this is what keeps a tiered-output definition trustworthy — no silent
overwrite). A name computed per-iteration (`as "chapters/%ch%/full": ...`, or even `as f:
...` using a bare loop variable directly as the name, per the Chekhov's-list example below)
is fine, since each iteration produces a distinct key.

There is no separate "value form" of `as` — `as "full": full` (commit a body that's just
one bare variable reference) already falls out of rule 2 with no extra grammar.

### 6. `in <expr>: <body>`

Evaluates `<expr>` to a Value, then runs `<body>` with the Reader environment's current
tree replaced by it. **The writer target is untouched** — bare statements inside still
emit to whatever the enclosing scope was already writing to; `in` only ever changes what
unqualified `read`/glob resolve against. Because every Value is uniformly tree-shaped
(including leaves, which just have no children), `in` is fully general — entering a leaf
value is legal and simply means nothing further can be read inside it.

**The Reader scope is very often a real branch/filesystem tree, but doesn't have to be —
that's entirely the caller's choice at the point `in` is used.** It can just as well be the
result of a previous glob, another context's whole return Value, or anything else with the
right shape. This is what makes glob composition below possible: `in` doesn't care what
kind of tree it's pointed at.

**Crossing into a different branch always goes through `in`, never through a path.**
Branches (a character's own branch, say) are separate refs with their own root trees, not
subdirectories nested under whatever branch is currently ambient — there is no such thing
as a path that reaches across a branch boundary. `"character/%charname%/sheet.md"` cannot
work as a `read` target for that reason; the correct form resolves the branch first, through
the `branch` filter, then reads relative to *its* root: `in (charname | branch): read
"sheet.md"`. This also interacts nicely with how the interpreter actually runs: a branch
only needs to be opened once, at the `in` that switches into it — not re-resolved on every
subsequent path lookup inside.

`in` is *only* used for deliberate, local reader-scope changes an author writes
explicitly. The **initial** Reader environment — which branch, rewound to which point in
the story if relevant — is never expressed in DSL source at all. It is supplied by the
host at the moment an agent invokes the interpreter (the same way `runReader` takes its
initial environment as a plain argument, not as an action inside the effect). An agent
resolves "character branch, as of wherever the story currently stands" using the engine's
existing historical-tree resolution, and *that* becomes the ambient environment every
top-level `read`/`for` in the called definition resolves against. Nested calls inherit it
automatically; only a definition that deliberately wants a *different* tree mid-computation
writes its own `in`. In practice this means a context written to answer "what's true about
the current character" often takes **no parameter at all** — which character is answered
by which branch the caller already positioned the interpreter at, not by a string threaded
through path construction (see the injury-status example below).

### 7. `> <string>`

Produces `Assistant text` instead of the `User text` a bare string literal would produce.
This is the one deliberate exception to "richness lives in filters, never new syntax" —
worth being explicit about why, so it doesn't read as the primitive set quietly eroding.
Every other rejected syntax addition (glob negation, computed glob patterns) would have
extended what an *existing* primitive means; `>` doesn't extend anything, it's a second
form of literal construction, given the same prefix-invoked status as `read` specifically
because message-role tagging is as fundamental to what this language assembles as reading
is — not incidental convenience bolted onto one primitive's behavior. `"text" | assistant`
was the filter-form alternative considered and rejected: given that nearly every piece of
authored text needs *some* role and `User` already covers the default silently, requiring
an explicit filter suffix on every line that needs the other common case was judged worse
than one small, deliberate, clearly-justified addition to the primitive count.

### 8. `< <expr>`

The symmetric counterpart to `>`: re-tags whatever `<expr>` itself evaluates to as `User`,
overriding that expression's own default role rather than constructing new text. A bare,
unprefixed statement already gets whatever role its own primitive naturally defaults to —
`User` for a string literal, `FileRead` for `read` (see rule 2 and "Value model") — so `<`
only has real work to do wrapping something whose own default *isn't* already `User`, most
often `read`: `< read notes.md` reads as ordinary authored text (`User`) instead of the
role-undecided `FileRead` a bare `read notes.md` would produce, regardless of which
interpreter later renders it. Justified the same way `>` is (see "primitive 7," above) —
`read notes.md | asUser` was the filter-form alternative, rejected for the identical reason:
role-tagging is fundamental enough to earn prefix status matching `>`'s own, not incidental
behavior bolted onto a filter. Unlike `>`, which only ever wraps a literal string, `<` takes
a general expression — its job is redecorating whatever an arbitrary already-composed
expression yields (an application, a filter chain, a nested `read`), not constructing text
from scratch — so it composes with the rest of the primitive set (`< f arg`, `< read x |
truncate(80)`) rather than standing apart from it the way `>` does.

### Calling a function is not a ninth case

A function call — whether to another named context (a file) or a local helper — is not
special. It's just "evaluate a body against a fresh writer target, produce a Value,"
identical to what rules 4/5 already do with inline statements. The callee never knows
whether it's being captured privately, exported, or emitted immediately; it just has a
writer to write to. `tmp = someContext(x); tmp` and bare `someContext(x)` are the same
program — this is rule 2 applied to an ordinary Value, not an optimization.

A callee's own internal `as`-exports never leak into the caller's `entries` — they stay
nested inside the one Value the call produced, reachable only via `in callResult: read
"name"`. This is what lets a context be included "without having to know exactly how it
builds its context" — the founding requirement this whole design was checked against.

**Reader scope is resolved entirely dynamically, at the point code actually runs — never
captured lexically at the point it's written.** A local binding like `readouter = f: read
f` stores the *operation*, not a snapshot of whatever tree happened to be ambient when
`readouter` was defined. When `readouter` is later called, `read f` resolves against
whatever is ambient *at that call site* — which follows directly from `in` being dynamic
scoping (`local`, not lexical capture) and calling a function never being special: there is
no exception carved out for locally-bound closures, named functions, or anything else: all
of them just run against whatever's currently ambient.

**One real consequence, worth stating as a permanent property rather than a rough edge:
narrowing only ever goes one direction.** Once inside a nested `in`/`for`, there is no
operation that reaches back out to an enclosing scope — the only way to make a wider scope
available from a nested position is to have captured it as a plain value *before* the
narrowing happened, since `=` fixes the result of evaluating in whatever scope was active
at that point and doesn't track further narrowing afterward. Concretely, this matters the
moment a loop body calls something that expects a wider scope than the loop itself narrowed
to — `for`'s own implicit narrowing (see Iteration and glob, below) applies to everything in
its body including calls to other functions, no exception:

```
for f in tracking/**.md:
  checkAgainstStyleGuide f
```

If `checkAgainstStyleGuide` reads `"config/style.md"` — outside the `tracking/` subtree
`for` narrowed to — that read silently fails to find anything, because `config/style.md`
was never one of `tracking/**.md`'s own matches. The fix is to capture the wider scope
explicitly before entering the loop and re-anchor at the one call site that needs it:

```
root = **/*
for f in tracking/**.md:
  in root: checkAgainstStyleGuide f
```

No new primitive is needed for this — `root = **/*` captures the whole ambient tree as an
ordinary Value using the existing bare-glob/`=` mechanics, and `in root:` re-anchors just
the one call that needs it. An "access the enclosing scope" primitive was considered and
isn't warranted: capture-before-narrowing already covers the real need completely. (This
specific tension doesn't actually show up in the magic-system worked example below, despite
first appearing to — see that example's note for why fixing a *different*, more fundamental
bug there made it moot.)

### Iteration and glob

```
for f in tracking/**.md:
  ...body referencing `f` as a bare Value with empty entries...
```

Not a primitive — sugar over a builtin higher-order filter (`each`) applied to a glob's
output. **`for`'s source is always a literal glob expression — never a filter-derived
list.** There is no list type independent of what glob produces, so there is nothing else
for `for` to iterate; a filter that manufactures something list-shaped from arbitrary
content (e.g. "extract every character mentioned in this prose") is explicitly rejected —
if a scenario needs to iterate "the characters present in this chapter," that has to be a
real, glob-able fact some agent already maintains (a file per present character), not
something derived live from unstructured content. See the magic-system example below,
which was corrected on exactly this point.

**Glob is generic, not filesystem-specific: it pattern-matches against the entries-map keys
of whatever tree is currently in Reader scope.** This is what makes glob composition work —
`in (someGlob): for f in anotherPattern: ...` narrows further, because the inner glob
matches against the *outer glob's own result*, not against the original root. Stacking `in`
around successive globs is how heterogeneous filters combine (e.g. "under this path pattern,
AND starting with this prefix") — no combined pattern grammar is needed, each stage is
oblivious to how the previous one matched.

`for`'s loop variable is bound to each matched **key**, wrapped as a fresh Value with empty
entries (never the richer Value that might already be sitting at that key in the glob's own
`entries` map) — deliberately, to preserve laziness. A loop body that only wants filenames
(`f | filewithname`, no `read`) must not pay for content it never asked for; forcing every
match's content just because the loop touched it would defeat the point. Any Value stored at
a glob's entry remains reachable directly via `in`/`read` for anyone who does want it — `for`
just chooses not to force it automatically.

Iteration order must be deterministic (path-sorted), since it feeds content-addressed
caching.

### Filters

```
expr | filterName(args...)
```

The *only* value-transform mechanism, drawn from a fixed, host-provided vocabulary —
**not definable inside the DSL.** This is what keeps the language closed: the only ways
computation can vary are (a) a bounded, host-controlled filter set, and (b) iteration over
an already-finite glob result. There is no way to construct unbounded recursion or an
open-ended loop from inside a script, structurally — not by convention, by construction.

Filters apply uniformly to any Value, including the simplest ones (bare loop variables,
bare parameters) — `f | filewithname` and `charA | charname` never call `read` first, they
operate directly on the identifier text.

Filters may be effectful and even LLM-backed (`draftDefinition`, `summarize`) — from the
DSL's point of view a filter that calls an LLM and one that does string manipulation are
the same kind of thing, both external, both atomic, both non-programmable in-DSL.

**Filters may take a function-valued Value as an argument and invoke it — this is not new
scope, it's already required.** `each`, the builtin `for` desugars to, has to take the loop
body as a function argument and call it once per glob match; there is no other way for
iteration to work. This is the same model Jinja uses (filters are where real code lives —
implemented host-side, never authorable in-template), just made explicit: passing a
function to a filter doesn't grant the DSL any new computational power, because the
function being passed is still an ordinary DSL body, bound by every restriction that
already applies to one (no recursion, no unbounded loops) — the filter only controls *how
many times* an already-bounded computation runs, which is exactly what iteration needed
and nothing more.

### Builtins are not filters

A context can also simply take a function as one of its ordinary parameters, supplied by
the calling agent at invocation — nothing about this needs new grammar, it's rule 1
(functions are values) plus "a parameter can be any Value, including a function." This is
how arbitrarily complex, *pure* computation enters the language for cases too specific and
too complex for either the DSL's own primitives or a global filter to reasonably handle.
The canonical case is invented-calendar date arithmetic (see the worked example below): an
agent that knows this particular story's calendar system passes in the actual Haskell
function that does that math, and the context just calls it with the raw date it read.

This is meaningfully different from a filter, not a variant of one:

- **Filters** are the fixed, global, host-registered vocabulary — the same `summarize` is
  available in every context everywhere, and filters are the *only* place an effect
  (including an LLM call) is allowed to happen.
- **Builtins** (caller-supplied function parameters) are story-specific and call-specific,
  chosen by whoever is invoking the context for that particular need — and must be pure.
  Arbitrary complexity is fine (a full invented-calendar system is far more code than
  anything expressible in the DSL's own primitives), but no side effects: **purity is what
  makes injecting one safe, not a restriction on how complex it's allowed to be.** A need
  that's both complex *and* effectful (an LLM call wrapped in elaborate custom logic)
  belongs in a filter, not a builtin.

Expected initial vocabulary (illustrative, not exhaustive): `summarize`, `truncate(n)`,
`orifempty(text)` (default-on-absence), `join(sep)`, `pinned`, `filewithname` (derive a
display name from a path), `branch` (resolve a branch-identifier Value to that branch's
root tree, for use with `in` — see "Crossing into a different branch," above), and
list-level ones applied to glob output: `whereType(t)`, `whereTag(t)`, `latest(n)`,
`sortBy(f)`. `pinned` marks a leaf as exempt from substitution by an alternate interpreter
— e.g. a voice sample that must never be paraphrased away by a compacting pass.

Excluding an entry from an already-built Value is filter territory too, not a separate
mechanism — `someResult | without("name")` / `| only("name")` operate on the `entries` map
the same way any other filter transforms its input. This is the one case authoring-time
omission (simply not writing an `as` for something) can't cover: consuming a *composed*
context from elsewhere — another author's black box, per the whole composability premise —
whose exposed entries you don't control the construction of, but still want to trim before
propagating further.

## Non-goals

Stated explicitly because narrowing scope was most of the actual design work:

- **Not Turing-complete.** No `if`, no boolean logic, no user-defined recursion, no
  `while`. Conditional *inclusion* needs (omit a section if nothing matches) are handled by
  absence — a `read`/glob that finds nothing produces an empty Value, and filters like
  `orifempty(...)` handle the common case — not by a control-flow keyword.
- **No user-defined filters.** The filter vocabulary is closed and host-provided.
- **No list type independent of glob.** A filter cannot manufacture something for `for` to
  iterate; only a literal glob can. If a scenario needs to iterate something, that thing
  needs to already exist as real, globbable files.
- **No direct story mutation.** The only effect is `read` (plus whatever effects filters
  perform internally, such as an LLM call) — never writing a tick. This DSL answers "what
  is showable," never "what should happen." Action/automation scripting (gate generation,
  auto-write a Note, regenerate on conflict) is a different, heavier problem explicitly
  deferred — it needs a different trust model (annotate-only vs. context-shaping vs.
  gate-and-regenerate carry very different risk, and only the first two even relate to this
  DSL at all).
- **No query-predicate syntax in `read`/`for`.** Both take exactly a literal path or a
  literal glob. Anything richer (latest-of-type, tag-filtered, sorted, limited) is a filter
  applied to a glob's output, not new primitive grammar.
- **No `print` keyword.** Bare-statement emission covers it entirely.

## Interpretation is not part of this spec

This spec defines what a definition *evaluates to* — a lazy, described Value. It
deliberately does not define:

- **How a Value gets consumed.** Flattened to a string for direct context injection,
  presented as a browsable file tree to a user, or mounted as tool-accessible surface for
  an LLM to explore itself (reusing the engine's existing tool-binding machinery,
  `glob`/`read_file` bound to a subtree) — all three are valid, and it's the *calling
  agent's* decision, not this language's.
- **How a `FileRead` becomes an actual rendered message.** Simulated tool-call/result pair,
  inlined as plain content, something else entirely — the DSL only commits to tagging a
  read as distinct from authored `User`/`Assistant` text, never to how that distinction
  surfaces. The tool-mount consumption mode gets this for free (a real tool call *is* the
  interpretation), but that's one interpreter's choice among several, not a rule.
- **How "compact" is computed.** A v1 interpreter can simply force every non-`pinned` leaf
  to its smallest already-cached tier, eagerly, with no budget awareness — this covers most
  cases and requires no allocator. A later interpreter can replace it with a budget-aware
  pass (reserve cost for `pinned` content first, then allocate remaining token budget by
  priority across the rest) **without any authored definition changing** — same source,
  smarter interpreter. This is the whole point of keeping evaluation and interpretation
  separate.

## Implementation strategy

Because the call graph is statically closed (see the function-storage separation above —
no reflection, nothing dynamically discoverable), a DSL definition doesn't need a runtime
AST-walking interpreter at all. It compiles directly — a shallow embedding, not a deep one
— into an expression built from the storage engine's own `StoreM`/`StorageMonad`
combinators, the same effect stack every hand-written Haskell agent already runs in. `read`
becomes a `StoreM` read at the current tree; `in` becomes which tree argument gets threaded
into the `read`s inside its body; `as`/`=`/bare-emit become ordinary Haskell data
construction; filters and builtins become ordinary function application. A compiled context
ends up with exactly the type shape a hand-written agent already has (`Value -> ... -> Sem
r Value`, curried per parameter) — nothing calling into a context needs to know or care
whether it was authored in the DSL or directly in Haskell.

The one thing that has to be preserved deliberately, because compiling it away would erase
everything this design relies on: the target of compilation is a `Value` whose fields are
themselves **`StoreM` actions**, not already-run results —

```
Value = { default :: StoreM [Message], entries :: Map Name (StoreM Value) }
```

Forcing a leaf just means running (binding) that action, and because `StoreM` is an
ordinary monad, a leaf's action can itself return a further `Value` full of more `StoreM`
actions — running the outer one and getting a usable result never requires a bespoke
recursive "force everything underneath" walker; ordinary monadic composition already does
it. Nothing about this is specific to context-DSL definitions — it's just how running
effects in a monad works, applied here.

This is also where the earlier free-monad reinterpretation idea (running the same
definition under different handlers to get verbatim vs. compact rendering) turns out to be
the *same* mechanism as compiling to `StoreM`, not a separate thing to reconcile with it.
"Which interpreter runs this" is just "which handler `StoreM`'s actions get run under" —
one handler executes them against real storage and reduces everything to concrete `Text`;
another can capture the sequence of reads instead of performing them directly, producing an
inspectable trace of what *would* be read rather than committing to running it — the same
substitution point that lets `summarize` mean something different in a budget-aware pass
without any authored definition changing.

## Worked examples

Validated against eight independent, project-specific scenarios (none generic enough to
ship as a built-in writing-tool feature). Two are shown in full below; the rest reduced to
straightforward retrieval once state-maintenance was correctly treated as out of scope (see
Non-goals) — which is itself a useful confirmation, not a letdown: most of the real
difficulty in these scenarios lives in *maintaining* the tracked state, not in assembling
context from it once it exists.

**Injury/status continuity** — no parameter at all; "which character" is answered by
whichever branch the caller already positioned the interpreter at:

```
as "injury": read status/injury.md
read "injury" | orifempty "not injured"
```

The raw fact stays reachable via `in thisResult: read "injury"` for a caller that wants it
unfiltered; a caller that just uses the result as a plain value gets the friendly fallback
text for free.

**Invented calendar** — the scenario that forced the builtin/filter distinction above. Real
date arithmetic under this story's specific invented calendar (ten-day weeks, three moons,
whatever) is too complex for the DSL's own arithmetic-free primitives and too story-specific
for a global filter, so it's a pure function the calling agent injects as an ordinary
parameter — the context itself only plumbs the raw stored date to it:

```
calendar_context = dateMath:
  as "rules": read "lore/calendar_system.md"
  dateMath (read "calendar/log.md" | latest(1))
```

called as `in (resolveAsOf storyPosition characterBranch): calendar_context
emberfallDateMath` — where both `resolveAsOf` (which tree, rewound to where) and
`emberfallDateMath` (which calendar system's arithmetic applies) are the *agent's* choices,
made once at the call site, not baked into the context definition.

**Chekhov's-gun list** — a full-content export alongside a name-only default listing, which
gets the resolution-ladder benefit (full detail vs. quick summary) without needing separate
`full`/`short` blocks at all:

```
as "open":
  for f in tracking/**.md:
    as f: read f

"open threads:"
for f in tracking/**.md:
  f | filewithname
```

**Voice-drift check** (per-audience visibility, no dedicated visibility mechanism needed —
just two named exports, one of which never reads the sensitive source; both cross into the
character's own branch via `charname | branch`, since a character's files live in a
separate branch, not a subpath of whatever branch this is called from):

```
voice_check = charname:
  as "generator_context":
    in (charname | branch): read "sheet.md"
    -- samples deliberately absent; the prose agent never sees them

  as "checker_context":
    "Voice profile for %charname%:"
    in (charname | branch): read "dialogue_samples.md" | pinned
```

**Relationship temperature** — two-argument curried function, both parameters used directly
in interpolation and piped through a filter without ever being `read`. Needs `charA |
branch` specifically (not ambient positioning) because this is the one context that
genuinely has to reach into *two* different characters' branches in one call — there's no
single branch the caller could have pre-positioned into that would cover both:

```
relationship_context = charA: charB:
  "%charA | charname% thinks of %charB | charname%:"
  in (charA | branch): read "relationships/%charB%/temperature.md"
  -- repeat in the other direction
```

**Personal prose-tic detector** — the simplest possible case, no wrapping needed at all:

```
read "style/tics.md"
```

**Magic-system compliance** — the one scenario that genuinely needs iteration inside a
fixed context. Went through two corrections: originally written with a filter that derived
a character list from prose content (rejected — a filter manufacturing something
`for`-iterable, fixed by requiring presence-per-chapter to already be a maintained,
globbable fact); then a second time because `casting_status` treated a character's branch
as a subpath (`"character/%charname%/..."`), which can't work — branches are separate refs
with their own root trees, not directories nested under the current one. Crossing into a
specific character's branch always needs an explicit `charname | branch` resolved through
`in`, the same fix `voice_check` and `relationship_context` needed above. One side effect
worth noting: this also removes the need for any capture-and-reanchor dance around the
`for` loop — `casting_status` now establishes its own correct scope via `in (charname |
branch):` regardless of what the loop narrowed *around* it, so the loop's implicit
narrowing (see "Calling a function is not a ninth case," above) never becomes a problem
here in the first place:

```
casting_status = charname:
  in (charname | branch): read "status/casting_log.md" | orifempty "no casting today"

magic_compliance_context = chapterPath:
  as "rules": read "lore/magic_system.md"
  as "casting_history":
    for p in presence/%chapterPath%/*.md:
      as p: casting_status (p | charname)
```

**Living glossary** — iteration over a filter-narrowed glob result (not a filter-*derived*
list — `extractProperNouns`/`exclude` narrow what a glob already found in `chapterPath`'s
own directory of tracked mentions, they don't manufacture iterability from nowhere):

```
glossary_update = chapterPath:
  known = read "glossary/index.md"
  for term in mentions/%chapterPath%/**:
    name = read term | extractProperNouns
    if_new = name | exclude(known)
    as name:
      "%name%: "
      name | draftDefinition(chapterPath)
```

## Open items, deliberately not resolved here

- Concrete syntax is still provisional throughout this document (string interpolation as
  `%name%`, filter-call parens, etc.) — validated for expressiveness, not finalized for a
  parser.
- The exact initial filter vocabulary needs to be enumerated against real scenarios beyond
  the ones above.
- Where `Contexts`-branch files live, and their parameter-declaration convention relative
  to the existing Prompts `.llmsettings.yaml` sidecar pattern, is unresolved.
- The budget-aware compacting interpreter (beyond the eager v1) is intentionally
  unspecified until the eager version is built and its limits are actually felt.
- Whether `FileRead`/`User`/`Assistant` is a deliberately complete set, or whether a
  `System` case is missing, is unconfirmed. Current assumption: DSL-produced content is
  always spliced into an existing system-prompt-anchored conversation (owned by the
  `agent.writer`/`agent.chat` `PromptStorage` keys) rather than authoring the system prompt
  itself, so a fourth case shouldn't be needed — but this hasn't been checked against a
  concrete scenario the way the other eight primitives were.
