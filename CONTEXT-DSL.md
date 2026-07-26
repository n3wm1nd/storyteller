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
`< <expr>`), plus `for`.

**Both `read` and `for` take a general `Expr` now, confirmed and
implemented** (see `Storyteller.Context.DSL.AST.SFor`/`ERead`) — a glob was
never structurally special; a bare glob token is just one more expression
that happens to evaluate to a `Value` with entries (see `EString`'s own
case above), exactly like a local variable, a fully-applied function call
(`for c in (charactersin path): ...`), or a filtered expression (`for f in
(**/* | exclude(...)): ...`).

`for` evaluates its source to a `Value` and iterates its own
`valueEntries`' keys, binding the loop variable to each key's text exactly
as before — whatever produced those entries. The one thing that still
can't appear there is an *unapplied* function reference, and that falls
out for free: `EIdent`/`EApp` already fail before producing a `Value` at
all when a name's `Binding` still needs arguments, the same failure any
other expression position hits — there was never a separate check to add
for `for` specifically.

`read`'s own generalization needed two small, well-justified departures
from the fully general rule, both confined to `Storyteller.Context.DSL.Compile`'s
`ERead` case — not the parser, and not a new primitive:

- **A string literal argument (quoted or bare) always resolves as a
  path/glob, bypassing the ordinary `EString`-quoted meaning ("inert
  text") entirely.** `read`'s whole point is "resolve a path"; quoting
  never meant anything to it beyond "definitely a path, never a variable,"
  deconflicting a literal filename from a same-named local (`read "f"`
  always means the file, even if a variable `f` is also bound; bare `read
  f` prefers the variable). Both quoted and bare string arguments go
  through the identical glob-matching `Storyteller.Context.DSL.Compile.globResolve`
  helper, so `read *.md` and `read "*.md"` behave identically, and both
  genuinely can match more than one file now.
- **A bare identifier that resolves nowhere — not a local, not a library
  name — still means a literal path**, the one place in the language an
  unresolved name isn't a hard failure (see Grammar below for why this
  doesn't reopen the general rule). A bound identifier (a parameter, a
  `let`, a `for`-loop variable — whose own `valueEntries` now carries a
  single self-keyed entry holding its real content, not just its name, see
  `runStmts`'s `SFor` case — or a library function) still evaluates
  normally instead, so `read f` inside a loop still means "the content at
  the path named by `f`," not "the loop variable's own placeholder text."

Whichever path produced it, `read`'s result is built the same way: if the
resolved `Value` has entries, force each one (in `valueEntries` order) and
fold their own content into the result's own default — list
concatenation, no separator inserted (matching how any other multi-message
default already combines) — keeping `valueEntries` itself intact, so a
multi-match result can still be narrowed further afterward. A `Value` with
no entries (an already-resolved single leaf) is returned unchanged. `read`
always stamps `metaProvenance` on each resolved entry (path + the tick it
was read at) — free, since the path and ambient commit are already known
before anything is forced.

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

**An identifier that fails to resolve is a hard, loud failure everywhere
except `read`'s own argument position — never a silent fallback anywhere
else.** `Storyteller.Context.DSL.Compile.resolveIdent` checks the current
definition's local `Env` (parameters, `let`s, `for`-loop variables) first,
falls back to the shared `Library` only on a local miss, and fails outright
(`"unknown identifier: ..."`) if neither has it. A typo'd `contextLroe` used
as an ordinary reference is a loud runtime `Fail`, not a silently-empty glob
match — there was never a design tension here to trade off; it only looked
like one before the parser's own lexical rule was actually read.

`read` is the one deliberate, narrow exception, via
`Storyteller.Context.DSL.Compile.tryResolveIdent` (`resolveIdent`'s
non-failing twin, used *only* inside `ERead`): a bare identifier that
resolves nowhere still means a literal path there, because `read`'s
argument has no other sensible meaning to fall back to — unlike a general
reference (where an unresolved name really might just be a mistake), a name
`read` can't resolve was always, structurally, either "the loop variable
this worked example needs" (covered by ordinary resolution once the
`for`-loop variable itself carries real content, see Primitives above) or
"a literal filename with no subdirectory" (`read notes.md`, `read ch1.md`
— both lexically `EIdent`, since neither contains `/` or `*`), and `read`
has nothing else to do with either case than treat it as a path. This
doesn't reopen the general "no silent fallback" rule — it's scoped to
exactly one primitive whose entire job already is "resolve a path," not a
general softening.

## Concrete syntax: quoting, grouping, statement separators

A few lexical/layout details, settled directly against
`Storyteller.Context.DSL.Parser`:

- **Quoted strings may span multiple lines.** `"..."` is terminated only by
  a closing `"` — there is no separate triple-quote form. A project
  authoring a longer block of prose as a string literal doesn't need
  different syntax for it; the same `"..."` used for a one-line label works
  unchanged for a multi-paragraph one. An unterminated `"` (running to end
  of input with no closing quote) is still a parse error, reported at the
  opening `"` — nothing changed about that; only the *interior* of a
  properly closed string may now contain literal newlines.
- **Escaping is minimal, on purpose: `\"` and `\\` only.** No `\n`, `\t`, or
  other escapes are recognized — there's no need for `\n` once a literal
  newline can already appear inside the string (see above), and the DSL has
  no other use for control characters in text meant for an LLM. A bare `\`
  followed by anything else is just two ordinary characters, not an error.
- **Parenthesized grouping is universal for expressions, not for call
  arguments.** `(expr)` is one of `pAtom`'s own alternatives
  (`Storyteller.Context.DSL.Parser.pParenExpr`), so it composes wherever an
  atom is expected — including as an application argument (`f (a b) c`) or
  inside a filter chain. This is a genuinely different piece of grammar from
  a filter's own `filt(a, b)` argument-list parens
  (`Storyteller.Context.DSL.Parser.pFilterStep`'s `pParenArgs`), which are
  comma-separated and only ever appear right after a filter name — the two
  don't share a production, they just both use `(`/`)` as delimiters.
- **`;` separates statements sharing one inline body, and only there.**
  `as "name": stmt1; stmt2` is now valid — the same widening of `pBody`'s
  own inline case (`Storyteller.Context.DSL.Parser.pBody`) that already let
  it hold a single statement now lets it hold a `;`-separated sequence via
  `sepBy1`. Deliberately scoped to exactly that one position: an indented
  block's own statements (`pStatementsAtCol`) are still one-per-line, parsed
  directly via `pStmtLine` rather than through `pBody`, so indentation
  remains the only block-structuring rule there — `;` never becomes a second
  way to end a statement inside a multi-line block.

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

**A `Library` is an ordered sequence, not a `Map`** — the one representation
decision that actually matters here, settled after the `Map`-based design
(described in earlier revisions of this section) turned out to have no way
to express what the language actually needed:

```haskell
newtype Library r = Library { libraryEntries :: [(Name, Binding r)] }
```

The same `Name` genuinely means *different* `Binding`s at different points
in the sequence — an override replaces what a name means from its own
position onward, but everything compiled *before* that position still
resolves the name to whatever it meant there. "The library" isn't one fixed
table with entries mutated in place; it's a sequence of tables, one per
position, that happen to share a representation. A `Map` can only answer
"what does this name mean in the finished table" — precisely the question
that has no single right answer here. A list answers the question this
language actually needs: "what does this name mean as of *this* slot,"
resolved by walking from that slot toward the entries compiled before it.

**Compiling is one left-to-right fold**
(`Storyteller.Context.DSL.Compile.buildLibrary`): each definition in the
input sequence is compiled — its body's every `EIdent`/`EApp`/`EFilter`
reference (including `%name%` string interpolation, which resolves through
the identical rule) checked and resolved — against exactly the `Library`
built from every entry *before* it, then consed onto the front. A
definition can only ever see what was compiled earlier, never what comes
later or shares its own slot.

**An override doesn't replace its default's entry in the sequence — it
sits right after it, both sharing one `Name`.** Two entries sharing a key is
exactly what the type above already means, not a special case:
`Storyteller.Core.Context.spliceOverrides` builds the input sequence as
`[..., (name, defaultDef), (name, overrideDef), ...]` for an overridden
slot. This is what lets an override's own reference to its own name resolve
to the compiled default sitting one slot behind it, rather than failing or
looping — the identical "previous version, never itself" rule a `let`
shadowing an outer binding already has in any ordinary language, now simply
falling out of the sequence's own order instead of needing a special
self-reference case. True self-reference (a name with no earlier occurrence
at all — a brand-new project-only name referencing itself, or two new names
referencing each other) still has nothing to resolve to and fails to
compile, exactly as it should: the language was never meant to support
recursion (see Non-goals).

**Resolution happens exactly once, at the moment a `Binding` would be
constructed — `evalExpr` never consults `Library` at all, at any point,
for any reference.** This is a real distinction, not phrasing: an earlier
version of this design merely *checked* that every name would resolve
before constructing a `Binding`, then discarded that work and had the
running `Action` redo the identical `Library` lookup from scratch on every
single evaluation of every `EIdent`/`EApp`/`EFilter` — "checked once,
resolved every time it runs" is still deferred, repeated work, just with
an extra up-front validation pass bolted on. The actual fix:
`Storyteller.Context.DSL.Compile.compileDefinition` folds every one of
`Library`'s entries directly into the starting `Env` a single time, so a
name resolves to its real `Binding` (not just "confirmed resolvable") the
moment `Env` is built, and every later `let`/`for`-bound local shadows a
library entry of the same name through the exact same map, by ordinary
`Map.insert` — one lookup mechanism, not two layered ones, and no
`Library` parameter left anywhere in `runStmts`/`evalExpr`/`resolveIdent`
to consult again. `Storyteller.Context.DSL.Compile.definitionBinding`
still walks a `Definition`'s whole body first (mirroring `resolveIdent`'s
own resolution rules exactly, including the closed
`without`/`only`/`exclude`/`latest`/`coreFilters` carve-outs that never
touch `Library` at all, and `read`'s own narrow "unbound bare token means a
literal path" exemption), and only constructs the `Binding` if every
reference resolves — but that walk is what makes folding `Library`
straight into `Env` *safe* (every name accepted by the walk is guaranteed
present in the merged map by construction, so a plain `Map.lookup` on `Env`
alone can never miss something the check already confirmed exists), not a
separate mechanism from actually resolving those names.

This is possible in one pass, exhaustively, because the language has **no
`if` and no recursion** — a definition's body references exactly the names
its text contains, with no data-dependent branching that could make the
answer depend on a runtime value. There is no separate "verification" pass
alongside ordinary compilation; resolving a reference *is* what compiling a
definition now does — and it happens only there, never again.

**One representation subtlety worth being precise about, since it broke
silently once already:** `Library`'s own list is consed front-to-back as
`buildLibrary` compiles left to right, so its head is the *most recently
compiled* entry for any name occurring more than once (an override sitting
right after its default, see `spliceOverrides` above) — `lookup`'s
first-match semantics pick that correctly wherever this module reads
`libraryEntries` directly. Folding that list into a `Map` via `Map.fromList`
needs the list **reversed** first: `Map.fromList` keeps whichever
occurrence comes *last* in the list it's given, which, fed the list
unreversed, would silently pick the earliest-compiled entry (the default)
over a later override sharing its name — inverting the whole point of the
default-then-override pairing. `compileDefinition` reverses before folding
for exactly this reason.

**A broken override is rejected outright, not silently discarded into "use
the default."** `Storyteller.Core.Context.buildContextLibrary` splices
`overrides` into the default sequence and tries to compile the whole thing;
if any slot fails — the override's own body doesn't resolve, or it changes
an arity some other definition still calls at the old one — that one
override is dropped and the whole sequence is recompiled from scratch
without it, repeating until everything compiles. This always terminates:
`overrides` strictly shrinks each retry, and the base case (no overrides at
all) is the default sequence alone, which — being a closed, already-checked
graph over `hostLibrary` — always compiles regardless of what any project
could ever commit. The function returns both the accepted `Library` and the
list of every name whose override was rejected, so a caller can report
*which* commits didn't take, rather than the previous design's fully silent
fallback.

**The slot a compile failure is reported at needn't be the override that
caused it.** An override can break a *default* that references it — a
1-arity `context.style` breaking `context.custom`, which calls it with
none — and `buildLibrary` reports the slot that failed, which is the
default's name. Dropping *that* would be dropping a default and is not
even possible; what has to go is the override. `buildContextLibrary` finds
which one by experiment: remove each committed override in turn and keep
the first whose removal lets the whole sequence compile. Deliberately not
by a separate "which names does this body reference" walk over the AST —
that would be a second, parallel resolver alongside the compiler's own,
free to disagree with what compilation actually resolves; recompiling *is*
the authority, and this only ever runs on an already-failing path, where
the cost (bounded by the number of committed overrides, squared, each a
pure walk over ~15 small definitions) is irrelevant.

This used to be a hard `error` — "a default can't fail to compile, so this
must be our bug." True of the default sequence *alone*; false of the
spliced sequence a project's overrides actually compile in, which is the
one being built. The consequence was that an ordinary wrong-arity commit
crashed every request that resolved any context at all, not just ones
touching the overridden name.

**An override at the wrong arity for its own name's external contract still
compiles and still lands in the library, if nothing inside the DSL calls it
at the old arity to catch the mismatch.** A definition with no DSL-internal
callers has nothing inside the compiled graph to reject a wrong-arity
override on its behalf — the override's own body is checked in isolation
and, if it's otherwise valid text, accepted. What then fails is the actual
call: `Storyteller.Core.Context.resolveContext0`/`resolveContext1`
(`Storyteller.Context.DSL.Compile.runNamed` underneath) are a lookup
expecting a specific arity at that name, and a mismatch there is exactly
the same "wrong number of arguments" failure calling any mismatched
function would give — not a gap to engineer around, and not something
`buildContextLibrary` should guess at ahead of time: a project committing a
different arity for a name is semantically discouraged, but mechanically
permitted, and the failure belongs at the one place that actually knows
what arity was expected.

This paragraph used to cite `context.style` as its example. That stopped
being true the moment `context.custom` was added and called it — which is
worth recording rather than quietly editing, because it's the general
shape: a name's membership in this class is a property of *who currently
calls it*, not of the name, and adding one caller anywhere moves it into
the class above. The class itself is not empty (`context.chaptersCompressed`,
among others, still has no DSL-internal caller); it just has no
stable, citable member.

**Override = define-new, same operation, otherwise.**
`Storyteller.Core.Context.buildContextLibrary` resolves "what's bound to
this name right now" — there's no gate requiring a name to already exist
before a project can bind it. A project committing `contexts/glossary.dsl`
under a name we never shipped is exactly as valid as one committing
`contexts/character/blurb.dsl` to replace `character.blurb`; a genuinely new
name is simply appended once, after the whole default sequence, with no
default-then-override pair (there is no default to pair it with).

**One real asymmetry, still true:** our compiled-in defaults form a closed
graph that can never reference a name a project hasn't invented yet (fixed
before any project's overrides exist); a project's own new name can
reference anything in that graph, plus anything else the project has
defined earlier in the same commit. The dependency edge only ever points
from project-authored text toward the built-in graph, never the reverse.

**Naming note, resolved:** an earlier revision of this section described a
live bug class — a `Definition` registered under two keys (a bare alias and
a dotted override address) not moving together under one committed
override, since only the dotted key was actually override-addressable. That
was real for `character.blurb` (fixed) and was, for a while, still latent
for `context.lore`/`context.chapters`/`context.other`. All of them now carry
only their dotted key in `Storyteller.Context.DSL.Library.defaultLibraryOrder`
— no two-key default exists anywhere in the library any more, so this bug
class is now avoided by convention across the whole graph, not a live gap.

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
- ~~**Two keys, one `Definition`, don't move together under override**~~ —
  **resolved**, not open: `Storyteller.Context.DSL.Library.defaultLibraryOrder`
  registers `context.lore`/`context.chapters`/`context.other` under *only*
  their dotted names (see that map's own Haddock on "one key per
  definition"), the same shape `character.blurb` already has — every other
  default referencing them (`context.other`, `context.writer`, their own
  `*Without` variants) does so by the dotted name, never a bare alias. This
  entry used to describe a real gap while the fix was still pending for
  these three; the fix has since landed, so there is no bare-alias/dotted-key
  split left anywhere in `defaultLibraryOrder` to cause the
  `character.blurb`-style bug. Left here struck through rather than deleted,
  since it's exactly the kind of claim worth re-checking against the code
  before trusting again.
- ~~**Override failure mode**~~ — **resolved**: `buildContextLibrary`
  rejects a broken override loudly, at the moment the whole sequence is
  compiled (which happens once per interpretation, before anything
  resolves against it), reporting exactly which name was rejected — never
  a silent, opportunistic discovery the next time something happens to
  resolve it. See "Library, compilation, and override" above.
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

**`read` applied directly to a multi-match glob** — confirmed and
implemented (see Primitives above): when per-file structure isn't needed,
`read` over a multi-match glob replaces what used to require an explicit
`for`, reading every match in order and concatenating their content:

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
