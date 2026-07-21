# Context DSL

Status: **implemented**, as of the 2026-07-21 redesign session, with a few
pieces still deliberately deferred (see Open questions and the notes inline
below marking exactly what's built vs. not). This document was rewritten
from scratch against the prior implementation's real bug (an override
registered in `defaultLibrary` that a compiled-in call site silently
bypassed) and now describes the actual code in `src/Storyteller/Context/DSL/`,
corrected once against it after implementation turned up a few places the
original design chat got wrong or over-engineered — notably, a speculative
`dslwith` quoter and explicit `compile` function that turned out to be
unnecessary once the real `ContextLibrary`/`Action` plumbing was inspected
(see Library, compilation, and override, below).

## Purpose

A project (a specific story, run by someone who doesn't write Haskell and can't
redeploy) needs to customize **what facts get shown to the LLM for a given
call** — which files, in what shape, under what name — without a code change.
We ship sensible defaults; a project overrides pieces of that policy by
committing replacement text. That is the entire job. It follows that:

- The **unit of override is a name**, not a file boundary or a Haskell type.
  "I want to change what `character.blurb` means" has to be expressible
  without touching anything else that references it.
- **Verification has a hard, honest ceiling.** Our own defaults can be checked
  as rigorously as we like, ahead of time. A project's override is unknown
  text until someone commits it — the best available is "checked immediately
  and loudly when it shows up," never "checked before it exists." This isn't
  a gap to engineer around; it's what "user-editable text" means.
- It is deliberately not a general-purpose scripting language (see
  Non-goals), and not a prompt-templating language either — `PromptStorage`
  remains the separate owner of system prompts. The DSL decides *what's*
  includable; framing/system-prompt text stays outside it.

## Containment: why a dynamically-resolved language is safe next to typed agents

Most of this codebase leans hard on static guarantees — effect capabilities,
even which LLM roles/models a call can reach, are type-checked. A textual,
runtime-resolved DSL is a real departure from that, and it's supposed to be:
the moment logic can be edited by someone who isn't compiling this project,
GHC cannot be involved in checking it, by definition. The discipline that
makes this acceptable is **not** "make the DSL as safe as Haskell" — that's
not achievable — it's **shrinking what the dynamic part is allowed to do**
until getting it wrong has a small, well-understood blast radius:

1. **The effect ceiling is still fully type-checked**, even though the text
   isn't. `Action`'s own quantification (below) makes "call an LLM" or
   "write a tick" a type error, not a convention, for every DSL program —
   built-in or overridden. Whatever a broken program does, it can only ever
   produce wrong or missing *read-only* content, never a wrong capability.
2. **Anything needing a real guarantee is pushed out of the DSL entirely**,
   into a `Binding` — ordinary, fully type-checked Haskell.
3. **Agents never depend on the DSL at all.** They depend on `RenderedContext`
   (below), a plain typed boundary value. The DSL is one producer of it, not
   something agent code reaches into.

Given that containment, the remaining exposure really is just "an override
might reference a name that doesn't resolve" or "show the wrong file" — never
"an override might do something."

## The Value model

```haskell
data Value = Value
  { valueDefault :: Action [Message]
  , valueEntries :: [(Name, Action Value)]
  , valueMeta    :: Meta
  }

data Message = FileRead FilePath Text | User Text | Assistant Text

data Meta = Meta
  { metaProvenance :: Maybe Provenance   -- set by `read` itself; filters don't invent this
  , metaPriority   :: Priority           -- default Normal; settable by a filter
  , metaFlags      :: Set ItemFlag       -- Droppable | Summarizable | Pinned; default empty
  }

data Provenance = Provenance { provPath :: FilePath, provTick :: Core.ObjectHash }
```

One type, still. A leaf is just a `Value` with empty `valueEntries` — no
separate leaf type, list type, or path type. `valueMeta` is the one addition
over the original design: an orthogonal field most code never touches, and
the only channel through which the rendering step (below) learns anything
beyond content and structure.

**`valueEntries` is an ordered association list, not a `Map`** — order is a
real, preserved property (construction order by default, freely reassignable
by a filter), exactly as before.

**Three ways a `Message` gets constructed** — unchanged: `read` produces
`FileRead` (role deliberately undecided), a bare string literal produces
`User text`, `>"text"` produces `Assistant text`. `<expr` re-tags an existing
value as `User` rather than constructing new text.

**`Action` is `Core.StoreT`-shaped, not a bare monadic value** — unchanged:

```haskell
newtype Action a = Action
  { runAction :: forall m. (Core.StoreM m, MonadBranch m) => Core.StoreT m a }
```

Nothing here forces eagerly. `read <path>` still just builds an `Action`
recipe; whether that recipe ever runs depends entirely on which renderer
walks the resulting `Value` (see Rendering) — the DSL itself never has to
decide "eager or lazy," because `Action` already defers everything until
something asks.

## Primitives

Unchanged from the original eight (function definitions, bare-statement
emit, `read`, `x = ...`, `as "name": ...`, `in <expr>: ...`, `> <string>`,
`< <expr>`), plus `for`, not a separate ninth case, **except that `read` is
now a generalization, not a special case restricted to one literal path.**
A bare glob (wildcarded or not — `*.md` and `test.md` are the same kind of
expression, one just happens to match at most one entry) is itself a
complete, meaningful expression: it gathers matching entries into content
but, on its own, **writes nothing** to the enclosing writer target — the
same "build structure, don't emit" behavior `for`'s own body already has.
`read <expr>` is what adds emission: it forces `<expr>` (a glob, a literal
path, a variable, whatever it evaluates to) and additionally appends its
content to the current default stream. So `read *.md` and `read test.md`
are both just `read` applied to a glob expression — one multi-match, one
degenerate single-match — not two different primitives. `read` always
stamps `metaProvenance` on the `Value` it produces (path + the tick it was
read at) — free, since the path and ambient commit are already known before
anything is forced.

## Grammar: bare tokens are disambiguated lexically, at parse time — no runtime fallback

An earlier draft of this section proposed resolving a bare, argument-less
token (`test.md`, `contextLore`) as a name first, falling back to treating it
as a glob if nothing bound it — a runtime, semantic disambiguation. That
turned out to be unnecessary: `Storyteller.Context.DSL.Parser` already
disambiguates **lexically**, at parse time, with no fallback and no
ambiguity ever reaching the interpreter.

The actual rule (see the parser's own "Concrete-syntax decisions not pinned
down by the spec" note): a bare, unquoted token is lexed permissively —
letters, digits, `_.%-*/` all allowed in one lexeme — and classified
immediately after lexing, purely by shape: **containing `/` or `*` makes it
a path/glob `EString`; anything else is an `EIdent`.** `tracking/**.md` is
unambiguously a glob; `contextLore` is unambiguously an identifier. There is
no case where the same token could be read either way, so there is nothing
for the interpreter to disambiguate at run time at all.

**Identifiers may contain interior dots** (`character.blurb`, `agent.writer`-
style), deliberately — this is what lets a project's own dotted override
address (`context.lore`, `character.blurb`) be written and referenced
directly as a bare identifier inside a DSL body, rather than needing a
second, separately-namespaced spelling.

**`read`'s own argument gets one extra rule the general case doesn't need**,
covering exactly the case the worked examples require: a *single-segment*
bare token with no `/`, no `*`, and no `%...%` interpolation (`read f`,
inside a `for f in ...:` body) is lexically identical whether the author
meant "the literal filename `f`" or "whatever path is bound to the loop
variable `f`". Since a real filename can never simultaneously be a bound
local name, `Storyteller.Context.DSL.Compile.pathLitText` prefers the bound
variable when one exists, falling straight through to literal-text lookup
otherwise — covering both readings without new syntax, and without needing
the general bare-token rule above to bend at all. A *quoted* token
(`read "injury"`) is excluded from this: quoting is meaningful, not
stylistic, so a quoted path is always literal text, never a same-named local
variable.

**An identifier that fails to resolve is a hard, loud failure — never a
silent fallback of any kind.** `Storyteller.Context.DSL.Compile.resolveIdent`
checks the current definition's local `Env` (parameters, `let`s, `for`-loop
variables) first, falls back to the shared `Library` only on a local miss,
and fails outright (`"unknown identifier: ..."`) if neither has it. A typo'd
`contextLroe` is a loud runtime `Fail`, not a silently-empty glob match —
there was never a design tension here to trade off; it only looked like one
before the parser's own lexical rule was actually read.

## Filters

```haskell
type DSLFilter = Value -> [Value] -> Action Value
```

**Filters are now effectful**, at the same ceiling as everything else in the
DSL (`StoreM`/`MonadBranch`, no LLM, no mutation) — this is a deliberate
change from the original design, which kept filters pure specifically
because their type had no way to express storage access. That restriction
was a consequence of the type we picked, not a principle worth protecting:
"fully applied, a filter still produces a Value" remains true in exactly the
sense it's true everywhere else in this DSL — `Action Value` *is* "a Value"
the moment something forces it, and nothing changes at the surface syntax
(`expr | filterName(args)` still just denotes another Value).

This unblocks filters that need to inspect content or history to decide
something — sort by tick timestamp, filter by tag, filter by type — without
smuggling them in as host `Binding`s. `priority(n)`, `pinned`, `summarizable`
are ordinary filters under this model: they return a copy of their input
`Value` with `valueMeta` updated, nothing else touched.

Filters remain a **closed, host-provided vocabulary** — not definable inside
the DSL, never threaded as a parameter, never user-extensible (see
Non-goals). Only their *capability* changed, not their status as fixed,
Haskell-authored, arity-known operations — which is why they don't disturb
the verification story below.

## Library, compilation, and override

There is no separate `compile`/`Library` type or `dslwith` quoter — an
earlier draft of this design proposed both, reasoning (correctly, in the
abstract) that override resolution needs to be "one function, given an
explicit environment." Reading the actual implementation
(`Storyteller.Context.DSL.Value`/`Compile`) turned up that this was already
built, just under different names, and more elegantly than the abstract
proposal: `ContextLibrary` (`newtype ContextLibrary = ContextLibrary (Map
Name Binding)`) travels as a plain Reader-shaped parameter on **every**
`Action`, regardless of which quoter produced it:

```haskell
newtype Action a = Action
  { runAction :: forall m. (Core.StoreM m, MonadBranch m) => ContextLibrary -> Core.StoreT m a }
```

Cross-definition reference (`EIdent`/`EApp`) resolves through
`Storyteller.Context.DSL.Compile.resolveIdent`: check the current
definition's own local `Env` (parameters, `let`s, loop variables) first,
fall back to `Storyteller.Context.DSL.Value.lookupLibrary` (which reads
whatever `ContextLibrary` the *currently running* `Action` was handed) on a
local miss. Since every `Action`, built-in or override, resolves against
whatever `ContextLibrary` it's actually run with — never one fixed at the
point it was written — **there is only one quoter that matters for this**:
`[dsl| ... |]` still parses at GHC compile time and still splices a curried
function of the definition's own arity, but the library its body's
cross-references resolve against was never fixed by the quoter at all; it
was always going to be "whatever's ambient when this runs." A speculative
`dslwith` quoter taking an explicit `Library` argument would have been
solving a problem this already didn't have.

`Storyteller.Context.DSL.Compile.definitionBinding :: Definition -> Binding`
is what turns a parsed `Definition` (built-in, or freshly parsed from a
project's committed override text) into the `Binding` shape `ContextLibrary`
holds — the same function either way, so a compiled-in default and a
project's override are genuinely the same kind of table entry, not two
mechanisms.

**Override = define-new, same operation.** `Storyteller.Core.Context.buildContextLibrary`
resolves "what's bound to this name right now" — there's no gate requiring a
name to already exist before a project can bind it. A project committing
`contexts/glossary.dsl` under a name we never shipped is exactly as valid as
one committing `contexts/character/blurb.dsl` to replace `character.blurb`.

**One real asymmetry, and one real subtlety about naming, both worth being
precise about:**

- Our compiled-in defaults form a closed graph that can never reference a
  name a project hasn't invented yet (built at a point in time before any
  project's overrides exist); a project's own new name can reference
  anything in that graph, plus anything else the project has defined. The
  dependency edge only ever points from project-authored text toward the
  built-in graph, never the reverse.
- `buildContextLibrary` checks a project's committed override text against
  `defaultLibrarySource`'s own keys **independently, per key**. A
  definition registered under two keys pointing at the *same* underlying
  `Definition` (a bare name like `contextLore`, and a dotted override
  address like `context.lore`) does **not** automatically move together
  under one committed override — a project overriding `context.lore` does
  not thereby change what `contextLore` (the bare name `contextWriter`'s own
  body references) resolves to. This is exactly the bug class
  `character.blurb` used to have, fixed for that one case by giving it only
  the dotted key and having every reference — including its own use inside
  `contextCharacter` — go through that same dotted name. `contextLore`
  itself still has the two-key shape and the same latent gap; it hasn't
  been revisited (see Open questions).

## Verification

**Not yet implemented** — this section describes the intended design; there
is no closure-check test in the codebase yet (see Open questions). Worth
building once the graph is large enough that a broken reference is a real,
recurring risk rather than a hypothetical.

The language has **no `if` and no recursion** (see Non-goals) — every
statement in a definition's body always executes when that body runs, so
the full set of names a definition *could* call is identical to the set it
*will* call. That's what makes exhaustive static checking meaningful rather
than approximate. Now that bare-vs-glob disambiguation is known to be purely
lexical (see Grammar above), this is simpler than the original proposal: an
`EIdent`/`EApp` is *always* a name reference, checkable exhaustively, with
no "bare tokens get a softer guarantee" carve-out — that carve-out was only
ever needed under the (incorrect) assumption that bare-token resolution
involved a runtime glob fallback.

Verification would not be a separate mechanism from ordinary resolution —
the same `resolveIdent`/`lookupLibrary` walk, run structurally against a
fixed `ContextLibrary` snapshot instead of lazily inside a running `Action`:

- **Our own defaults**: walk every default `Definition`'s body and check
  every referenced name exists in `defaultLibrarySource` (∪ that
  definition's own parameters) at the right arity, for every `EIdent`/`EApp`
  found. Exhaustive, because the graph is finite, known, and provably
  non-recursive. Run on every `cabal test`/CI — this is the actual
  "compile-time verification" ask, delivered as a test over the whole graph
  rather than a per-quote GHC check (which, even where it existed, only
  ever covered Haskell-side call sites and never checked the bare-name
  DSL-to-DSL references that are the *primary* composition mechanism).
- **A live library snapshot** (defaults + whatever a project has currently
  committed, assembled once per interpretation via
  `Storyteller.Core.Context.buildContextLibrary`) could be walked the
  identical way, before serving any request against it — catching a broken
  override up front rather than discovering it mid-query. Still open:
  should a broken override be rejected/flagged **at commit time**, or only
  discovered opportunistically the next time something resolves it (see
  Open questions)?
- **Structural checking never forces content.** It walks the parsed AST —
  names and arities — never runs an `Action`, so it costs nothing in
  storage reads regardless of how deep or wide the graph is.

## Rendering

Lives in `Storyteller.Context.DSL.Rendering`. `Value` stays exactly as lazy
as always — nothing forces until something asks. What's new is that there
are **two independent ways to ask**, each interpreting that laziness
differently, off the same tree:

```haskell
renderContext    :: Value -> Action Context
-- forces everything: every valueDefault, every valueEntries child, recursively.
-- the fixed, curated bundle for direct inclusion, and for a plain-text preview.

renderFileSystem :: Value -> Action FileSystemView
-- forces only shape: valueEntries' own names, recursively -- never a leaf's own valueDefault.
-- what a tool-using agent browses, and what a $context/{path} preview endpoint resolves one path against.
```

`Provenance` is available under *either* renderer without forcing content —
it's decided structurally, by `read`/`treeValueOfCommit` themselves, before
any content is ever touched.

**Real, load-bearing limitation, not yet resolved:** `Provenance` only
survives on a `Value` node that is *exactly* what `treeValueOfCommit`
produced, untouched. The moment content passes through
`Storyteller.Context.DSL.Compile.runStmts`/`mkValue` — which is to say, any
real multi-statement definition, even something as small as `loreEntry`
(a heading plus one `read`) — the result is a fresh `Value` with
`defaultMeta`, because a node built by folding more than one source
together has no single sensible provenance to assign; there's no "the" file
it came from. So `renderFileSystem` is honestly only meaningful directly on
a Reader scope (`Storyteller.Context.DSL.Compile.currentScope`,
`treeValueOfBranch`) or a bare, unfiltered `read` result — not on an
already-composed library definition's output.

### `RenderedContext`

```haskell
data RenderedContext a = Node
  { rcContent :: [a]
  , rcEntries :: [(Name, RenderedContext a)]
  }
  deriving (Functor, Foldable, Traversable)

type Context = RenderedContext ContextItem

data ContextItem = ContextItem
  { ciMessage :: Message   -- reuses Message directly rather than inventing a redundant role type --
                           -- Message already is "content + role" (see Value model)
  , ciMeta    :: Meta      -- priority, flags, provenance, carried straight from the Value node
  }
```

Deliberately mirrors `Value`'s own shape (own stream + named children, in
order) rather than flattening — flattening would have turned bucket access
(`contextCharacter`'s `sheet`/`blurb`/`full`/`journal`) into string-matching
a flat list; keeping the tree makes it a real, checkable structural lookup
(`namedChild :: Name -> RenderedContext a -> Maybe (RenderedContext a)`),
while `Foldable` still gives a consumer that wants "everything, in order,
regardless of structure" a structure-blind `toList` — used by
`listDeferred` (below), deliberately *not* used by `renderText`/
`renderMessages` (see their own note: `rcContent` and `rcEntries` are not
disjoint by design, so a full `toList` walk would double-count real
definitions like `contextLore`). `Functor`/`Traversable` are what a
budget-aware pass would map over to shrink/replace content while the shape
carries along unchanged.

### `FileSystemView`

Same shape, unforced leaves:

```haskell
type FileSystemView = RenderedContext ContextRef

data ContextRef = ContextRef
  { crSource :: Provenance
  , crMeta   :: Meta
  }
```

No role field here — unlike the original proposal, a `ContextRef` does
*not* try to predict what role its content will get once read: that's
decided by the `Value`'s own `valueDefault` recipe (`>`/`<` wrapping), which
cannot be known without running it, so predicting it ahead of time isn't
actually possible, only content is deferred, not the decision of whether
something's worth reading.

Backs a real tool surface: `listDeferred :: FileSystemView -> [ContextRef]`
(the menu — free to compute, no content; a plain `toList`, since a
`FileSystemView` node's "own" vs "children" genuinely are disjoint,
unlike `Context`'s), `readRef :: ContextRef -> Action ContextItem` (force
exactly one, on demand — re-resolves via
`Storyteller.Context.DSL.Compile.treeValueOfCommit` against the ref's own
stored commit, not the caller's current ambient position, since a ref may
be read well after `renderFileSystem` itself ran). This is the "context is
a filesystem" model from the original design, recovered — it was always
structurally right (decide-then-read, no forced cost for what's never
read); it was only ever missing the framing metadata (`Meta`) that
`ContextRef` now carries.

### The pure floor

```haskell
renderText :: Context -> Text
renderText = T.intercalate "\n\n" . map (messageText . ciMessage) . rcContent

renderMessages :: Context -> [ULLM.Message mdl]
renderMessages = map (dslMessageToLLM . ciMessage) . rcContent
```

Both read only `rcContent` — the top-level node's own stream — deliberately
not a full-tree `toList` walk into `rcEntries` too. This was a real bug,
not a design choice made in the abstract: an earlier version of both
functions did walk the whole tree, and it double-counted real definitions
(`contextLore`'s own per-file `for` loop folds each file's content into its
own top-level default *and* exports the identical content again as a named
entry — walking both gave every file's content twice). `rcContent` is
always "the definition's own answer"; `rcEntries` is always "additional or
different depth, reachable only by name" (see Authoring guidance) — never
something to be silently unioned back in.

`renderText` is the true floor — no role, no model shape, nothing but
concatenated content in order. It has to be unconditionally meaningful,
because a model whose API has no turn/role concept at all could still
consume it correctly; it could never consume `[Message mdl]`.
`renderMessages` is the chat-shaped specialization built from the identical
`rcContent` traversal, so flattening its output back to plain text is
`renderText` by construction — each source `Value` node's own default
already decided its own message-by-message role (`>`/`<`), so this is an
ordinary per-item map, not a re-grouping pass over contiguous same-role
runs (the original design speculated grouping would be needed; it isn't,
since a `ContextItem` already wraps exactly one role-tagged `Message`).

### The effectful ceiling

**Not yet implemented** — the design below is the intended shape; see Open
questions for what's still undecided about it.

```haskell
renderWithBudget
  :: Members '[LLM] r    -- storage deliberately absent unless a specific consumer opts in
  => Budget -> Context -> Sem r [ULLM.Message mdl]
```

The opposite effect ceiling from the DSL: LLM-capable (real `summarize`
finally has somewhere to live), storage-free by construction, because every
`ContextItem` it operates on already carries its full materialized text —
no re-fetch ever needed. `Summarizable`/`Droppable`/`Pinned` flags on
`Meta` are what this pass acts on: drop lowest-priority-and-droppable items
first, replace a `Summarizable` item's content with an LLM summary of the
text it already has in hand, never touch anything `Pinned`. Whether dropping
should be allowed at subtree granularity (discard a whole named bucket at
once) as well as item granularity is a real, still-open question — the
types above support it (nothing stops `Meta` from living on
`(Name, RenderedContext a)` as well as on `a` itself), but it isn't decided.

## Authoring guidance

A definition's **bare/default stream** is the safe, always-sufficient answer
for a caller that never asks for more — what `renderContext` forces and
hands to an agent that doesn't explore. Each **`as "name": ...` block** is
additional or differently-shaped depth, reachable only by name, never forced
unless something asks for it by that name — whether a tool-using agent
browsing via `renderFileSystem`, or a Haskell caller reaching for a specific
bucket on purpose. If a definition's default stream is trying to be
everything at once, that's the sign content belongs in its own `as` block
instead.

## Non-goals

Unchanged in spirit from the original design:

- **Not Turing-complete, provably.** No `if`, no recursion, no `while`.
  `for`'s source is a general expression (see AST/Grammar above — a glob
  was never structurally special), but whatever it evaluates to is always
  an already-finite, already-materialized `Value`'s own entries — a glob
  match, a filtered expression, a fully-applied function's result. There
  is still no way to iterate something open-ended or self-referential.
  This is load-bearing for Verification above, not incidental.
- **No user-defined filters.** The vocabulary is closed and host-provided —
  filters gaining `Action` capability doesn't change who gets to define one.
- **No direct story mutation.** The only effect is read access (`read`, a
  filter, a `Binding`) — never writing a tick. Enforced by `Action`'s own
  type, not convention.
- **No query-predicate syntax** in `read`/`for` — filters over an
  expression's output, not primitive grammar. `for`'s source being general
  now (any `Value`-producing expression, not just a glob) doesn't reopen
  this: selection criteria still only ever enter through the closed filter
  vocabulary applied to that expression, never through new grammar in
  `for`'s own clause.

## Open questions

- **`branch`**: it was pulled out of `coreFilters` specifically because
  resolving a branch name needed storage access a pure filter couldn't
  express. That reason is gone now that filters are `Action`-typed — does
  it move back into the ordinary filter vocabulary, or stay visibly
  special-cased because it's the one filter that redirects subsequent reads
  to a different branch, which might deserve to stand out regardless?
  Still open; no code changed either way.
- **`journalDelta`** stays a `Binding`, confirmed — not because of its
  storage-access needs (moot now that filters are effectful too), but
  because it's curried over Haskell-level `Int` configuration
  (`journalDelta 30 10 2`) before any DSL text is parsed, which is genuine
  per-caller parametricity a bare-name library reference can't express.
  `context.character`'s own `journal` parameter is exactly this case (see
  Worked examples); `character.blurb` was the different case — a shared
  default with no such parametricity — which is why only it moved to
  bare-name resolution.
- **Two keys, one `Definition`, don't move together under override** — a
  real, confirmed gap, not just a question: `contextLore`/`context.lore`
  (and `contextChapters`/`context.chapters`, `contextOther`/`context.other`)
  still have the same shape `character.blurb` used to, and the same latent
  bug — an override committed under the dotted address doesn't reach a
  bare-name reference to the same `Definition` from another body. Nothing
  has broken yet because no project has tried to override
  `context.lore`\/`context.chapters`\/`context.other` independently of
  `context.writer` as a whole, but the fix (drop the bare alias, reference
  the dotted name directly, as `character.blurb` now does) is the same
  shape and not yet applied to these three.
- **Override failure mode**: reject/flag loudly at commit time, or only
  fail (falling back to default) opportunistically when something resolves
  it? Leaning loud, not confirmed.
- **Subtree- vs leaf-level dropping** in `renderWithBudget` — can a whole
  named bucket be discarded as a unit under budget pressure, or does
  `Meta` only ever act at the leaf?
- **The `Contexts`-branch convention** — where definitions live, parameter
  declaration relative to `.llmsettings.yaml` — carried over from the
  original design, still unresolved.
- Whether `Message` needs a fourth `System` case — carried over, still
  unconfirmed; current assumption is DSL output always splices into an
  existing system-prompt-anchored conversation, never authors the system
  prompt itself.
- **The override store is keyed by `Name` alone, deliberately, not
  `(Name, argument)`.** Confirmed, not left open: `context.character` is
  resolved once per active/present character in several places
  (`Server.Writer.File.activeCharacterContext`,
  `Storyteller.Writer.Agent.Roleplay.askCharacter`/`reflectFor`), and a
  staged or committed override of `"context.character"` necessarily applies
  uniformly across all of them in one request — there is no way, and no
  need, to say "override just for this one character." A project wanting
  genuinely per-character behavior encodes that as *data* on that specific
  character's own branch (a marker file, a tag) for the one shared,
  overridden definition to read and react to via ordinary `read`/
  `orifempty`, not as a second override axis. Separately, per-request
  (client-submitted, ephemeral) override — the mechanism `context.writer`
  has via `Server.Writer.File.chatWriter`'s `fcContext` — was never wanted
  for `context.character` at all: that mechanism exists to serve an
  interactive per-message UI need (toggling a mention or a lore file for
  one call), and character-context assembly has no analogous "quick
  toggle" — a project wanting different character-context behavior wants
  it consistently, which the persistent Contexts-branch override already
  covers.

## Worked examples

**Injury/status continuity** — no parameter; "which character" is answered
by whichever branch the caller is already positioned at. Exports and local
use are deliberately separate steps (`x = ...` computes once; `as "name": x`
exports the already-computed value; the bare `x` re-emits it) rather than
relying on the exported string key doubling as a usable identifier — the
same idiom `contextLore`'s own `for` loop already relies on:

```
x = read status/injury.md
as "injury": x
x | orifempty "not injured"
```

**`read` applied directly to a glob** — the payoff of unifying `read` and
glob-matching rather than treating `read` as "one literal path" only. When
per-file structure isn't needed, `read` over a multi-match glob replaces
what used to require an explicit `for`:

```
-- when you need each file addressable by name (a bucket per file):
for f in tracking/**/*.md:
  as f: read f

-- when you only want the flattened content, nothing per-file to address:
"## Everything currently tracked"
read tracking/**/*.md
```

**Bare-token resolution, both outcomes side by side** — illustrating the
Grammar rule directly: the first line is an application-free name lookup
(would fail the closure check if `contextLore` weren't a real library
entry); the second is a glob, unconditionally, whether or not a file by
that name happens to exist:

```
contextLore   -- a name: calls the 0-arity library entry `contextLore`
README.md     -- a glob: matches (or, more often, doesn't) a literal path
```

**Priority and droppability, feeding `renderWithBudget`** — filters setting
`valueMeta` rather than a new primitive. `journalFull` is real
self-knowledge a character-facing agent might want, but it's the first
thing that should give way under budget pressure, and it's fine to
compress rather than drop outright:

```
as "journalFull":
  read journal.md | priority(1) | summarizable
```

**Library composition by name — the fix for the bug this whole redesign
started from.** `character.blurb` is referenced by `context.character`
exactly the way `contextLore` references `loreEntry`: a bare application,
resolved against whatever `Library` the caller compiled `context.character`
against. There is no typed `Binding` parameter standing in for it and no
way for an override to be silently bypassed — if a project's override of
`character.blurb` is in the library `context.character` was compiled with,
every reference to it, from anywhere, sees the override:

```
-- registered under only the dotted name "character.blurb" (no separate
-- bare alias -- see Library's own note on why two keys for one Definition
-- would reopen exactly this bug)
charname:
  in (charname | branch):
    n = read sheet.md | name
    a = read sheet.md | abstract
    "%n%: %a%"

-- context.character composes it by its own dotted name directly, not by parameter --
-- journal stays a real Binding parameter, since journalDelta's Haskell-level
-- lookback/maxOut/padding tuning is genuine per-caller parametricity, not a
-- shared default a project should replace by name
charname: journal:
  as "sheet":  in (charname | branch): read sheet.md | orifempty ""
  as "blurb":  character.blurb charname
  as "journal": journal charname
  character.blurb charname
```

**Browsing before reading — the filesystem-shaped renderer in use.** Since
`renderFileSystem` only carries real provenance directly off a Reader scope
(see Rendering's own note — it doesn't survive composition), this is shown
against a character's own raw branch tree, not an already-composed
definition like `context.character`:

```haskell
tree    <- treeValueOfBranch (BranchName ("character/" <> charname))
menu    <- listDeferred <$> renderFileSystem tree
-- menu :: [ContextRef]  -- paths, priorities -- no content forced

chosen  <- readRef (menu !! pickedByAgent)
-- chosen :: ContextItem -- forced now, exactly this one entry
```

**Invented calendar** — unchanged from the original design: a genuine
per-call-site host function, still a `Binding` parameter rather than a
named library entry, because different callers legitimately want different
date-math for the same definition, not "the current shared default":

```
calendar_context = dateMath:
  as "rules": read lore/calendar_system.md
  dateMath (read calendar/log.md | latest(1))
```

called as `calendarContext (fn1 emberfallDateMath)` from Haskell, where
`emberfallDateMath`'s own choice of calendar system is the caller's, never
baked into the definition.
