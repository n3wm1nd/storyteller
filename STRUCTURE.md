# Project structure — two Core/Writer splits

This project has two separate Core/(something) splits, cutting on two
different axes. Don't confuse them:

1. **`Server.Core` vs. `Server.Writer`** — generic connection/dispatch
   infrastructure vs. this-app-specific assembly and business logic. The cut
   is "would this survive unmodified in a sibling app (Roleplay, Lector)."
2. **`Storyteller.Core` vs. `Storyteller.Agent`** — the tick/atom/git storage
   engine vs. the story-writing business logic built on top of it. The cut
   is "is this the substrate every app would share, or the policy that makes
   this app what it is" — the closest thing this codebase has to
   "user-serviceable parts."

Both splits happen to reuse the word "Core" because they're the same *shape*
of decision (substrate vs. specific), just applied at different layers — the
server's request-handling layer, and the underlying storage SDK.

Background/principles this follows: `WS-PROTOCOL.md`'s scope-design section,
and the [[feedback_app_specific_not_universal]] memory (QtCreator not
Notepad++ — duplicate glue rather than build extension points).

# Part 1: Server — `Server.Core` vs. `Server.Writer`

## Mental model

`Server.Writer` is not a child that inherits from `Server.Core`. `Server.Core`
is what you'd get by extracting the common operations out of `Server.Writer`,
`Server.Roleplay`, and `Server.Lector` — a library factored out of siblings,
not a base class siblings extend. Since only Writer exists so far, that
extraction can't honestly happen yet for anything above the pure-function
layer: the test for "does this belong in Core" isn't "does this look
app-agnostic" (everything looks app-agnostic with one sample), it's "would
this survive unmodified if Roleplay or Lector needed it too" — and for
env/notification/connection assembly, there's no second app yet to check that
against. That's why those stay in Writer even though nothing about the code
itself makes them look Writer-specific.

## The cut: library vs. assembly

`Server.Core` is a **library** — a repository of building-block functions.
Nothing in it wires anything together: no WS connection lifecycle, no
command-protocol decoding, no notification pub/sub, no `ServerEnv`/effect-stack
assembly. Just `Sem` functions a caller composes.

`Server.Writer` is **both**: it's where the actual application gets
assembled (connection lifecycle, dispatch, notification wiring, env), *and*
it holds Writer-specific business-logic functions of the same shape/category
as Core's (e.g. `trackFiles`/`charGen` sit next to `Server.Core.Branch`'s
`moveTickInBranch` in spirit, just too specific to be universal).

Concretely: connection lifecycle and notification handling are assembly, not
library, even though `Server.Writer.File.Connection`/`Server.Writer.Branch.Connection`
look structurally identical to each other. That similarity is a coincidence
of only having one app so far, not evidence they're a reusable "tick
notifier" abstraction — Branch and File *already* handle `TicksRemapped`
differently (File forwards it to the client as `tick.remap`; Branch drops
it). Don't extract a shared notifier now and hit that mismatch later;
duplicate the whole connection+notification module and let it diverge until
a second app actually forces the shared part into view.

## `Server.Core.*` — pure library, no assembly

| Module | Contains |
|---|---|
| `Server.Core.Protocol` | `WireTick`, `Update`, `toWireTick`, `tickToWireTick`, `withId` — wire-shape data + pure conversions |
| `Server.Core.Util` | `withBranch` — generic scope-opening helper |
| `Server.Core.Run` | `SessionEffects` type alias — a type-level list of effect memberships (Random/Sleep/Time/Git/Fail/Logging/Error/StoryStorage/LLM). A declaration any library function can require, not wiring; the interpreters that satisfy it are Writer's job (`Server.Writer.Run`). Split out from the rest of `Run` specifically because `Server.Core.File`'s `appendToFile` needs the constraint for logging — a library function, so it can't depend on Writer. |
| `Server.Core.Branch` | `BranchOpen`, `branchState`, `branchStateSince`, `moveTickInBranch`, `deleteTickFromBranch`, `addNote` (re-exported from `Storyteller.Core.Annotation`) — generic tick-chain state/mutation, no JSON/WebSocket |
| `Server.Core.File` | `FileOpen`, `fileState`, `fileStateSince`, `appendToFile`, `editFileAtom`, `deleteFileAtom`, `moveFileAtom`, `chatNote` — generic atom-chain state/mutation, no JSON/WebSocket |

That's the whole of Core — five modules. Everything else is assembly (or
Writer-specific business logic).

## `Server.Writer.*` — assembly + Writer-specific library functions

| Module | Role |
|---|---|
| `Server.Writer.Env` | `ServerEnv`/`AppState` — app config and shared STM state |
| `Server.Writer.Run` | Effect-stack assembly: `actionStack`, `wsAction`, `loggingWS`, `streamChunksWS`, `gitNotify`/`storageNotify` wiring. Imports `SessionEffects` from `Server.Core.Run` rather than redefining it. |
| `Server.Writer.Notification` | `BranchNotification`, `watchBranch` — this app's pub/sub wiring |
| `Server.Writer.Session.Protocol`/`.Connection`/`.Dispatch` | `/session` connection: list/create/delete branch. Storage-generic in content, but protocol decode and connection lifecycle are wiring, so it lives here, not Core. |
| `Server.Writer.Branch` | `trackFiles`, `charGen` — Writer-specific business logic, same shape as `Server.Core.Branch` but too specific to be universal |
| `Server.Writer.Branch.Protocol` | `BranchCommand`/`BranchEvent` — the full `/branch/{name}` wire protocol |
| `Server.Writer.Branch.Dispatch` | Assembles the protocol onto both layers: `MoveTick`/`DeleteTick`/`AddNote` call `Server.Core.Branch`, `Track`/`CharGen` call `Server.Writer.Branch` |
| `Server.Writer.Branch.Connection` | `/branch/{name}` connection lifecycle |
| `Server.Writer.File` | `chatWriter`, `chatFixer` — Writer-specific business logic (runs the Writer/FlowWriter/Fixer prose agents) |
| `Server.Writer.File.Protocol` | `FileCommand`/`FileEvent` — the full `/branch/{name}/{path}` wire protocol |
| `Server.Writer.File.Dispatch` | Assembles the protocol onto both layers: `ChatAppend`/`EditAtom`/`DeleteAtom`/`MoveAtom`/`ChatNote` call `Server.Core.File`, `ChatWriter`/`ChatFixer` call `Server.Writer.File`. `At` (the rebase wrapper) is generic either way — it just recurses back into this same dispatch for whichever inner command. |
| `Server.Writer.File.Connection` | `/branch/{name}/{path}` connection lifecycle |

### Executable

`app/Server.hs` wires `Server.Writer.Session.Connection.runSession` and the
Writer File/Branch connection runners together in `wsRouter`. This file is
inherently the "final assembly" — it's the one place that decides "this app
is the Writer app."

## Test modules

`test/Server/TestStack.hs` is the shared interpreter stack. `BranchSpec`/
`FileSpec` exercise `Server.Core.Branch`/`Server.Core.File` directly (the
business-logic layer, not the wire protocol), run once each under an eager
interpreter and once buffered through `Storyteller.Core.Git.withStorage` (see
`test/Main.hs`). `NotificationSpec` exercises `Server.Writer.Notification`.

# Part 2: SDK — `Storyteller.Core` vs. `Storyteller.Agent`

## `Storyteller.Core.*` — the storage engine

Everything the tick/atom/branch model needs, with zero awareness of LLMs,
prompts, or story-writing policy. This is the part that would survive
unmodified if a Roleplay or Lector app were built on the same substrate.

| Module | Contains |
|---|---|
| `Storyteller.Core.Types` | `TickId`, `Tick`, `TickData`, `TickPos`, `Branch`, `BranchName`, the `TickType` typeclass — the base tick vocabulary |
| `Storyteller.Core.Storage` | `StoryBranch`/`StoryStorage` effects: `store`, `replace`, `drop`, `get`, `reset`, `at`, `sneakyAt`, `readAt`, `withFS`, `atWithFS` — the storage effect interface |
| `Storyteller.Core.Git` | The git-backed interpreters for `StoryStorage`/`StoryBranch` — ref layout, commit encoding, working-tree (de)serialization |
| `Storyteller.Core.Edit` | Chain editing: `deleteTick`, `editAtom`, `moveTick`, `commitWorkingTree` — composed from storage primitives, no LLM/splitter involvement |
| `Storyteller.Core.Atom` | The `Atom` tick kind (file-append ticks) |
| `Storyteller.Core.Annotation` | `addNote` and the annotation-tick vocabulary — new ticks that reference an existing one, distinct from `Edit`'s in-place chain restructuring |
| `Storyteller.Core.Runtime` | `StoryModel`, the `Main` branch-phantom, `runInfrastructure`/`runStoryGit` — shared IO effect-stack assembly |
| `Storyteller.Core.CLI.Env` | `StoryEnv`, `loadEnv`, `modelConfigs` — the ENV-variable configuration shared by all CLI entry points |

## `Storyteller.Agent.*` — the business logic (unchanged)

Already lived in its own namespace before this split and didn't need to
move — it was already correctly separated by directory, just not by a name
that made the boundary explicit. This is where the actual "what does this
app do" policy lives: `Agent` (shared cross-agent vocabulary — `Prompt`,
`Instruction`, `Prose`, `ContextBlock`, etc.), `Append`, `CharContext`,
`CharGen`, `Continuation` (the prose-generation core), `Fix`, `FlowWrite`,
`ReplaceTool`, `Splitter` (the atom-splitting policy — deliberately an
effect, so callers aren't coupled to it), `Tracker`, `Write`.

None of this moved in this pass. A further split *within* `Storyteller.Agent`
(Core/Common/Writer, by how Writer-specific each agent's *policy* is — not
to be confused with the engine/logic split above) is still only sketched,
not done: `Splitter`/`Tracker` look like they'd survive across apps
unmodified, `Write`/`FlowWrite`/`Fix`/`CharGen`/`CharContext`/`Continuation`
look Writer-specific, but nothing has forced the actual boundary into view
yet — same "don't extract before a second app exists" reasoning as
everywhere else in this doc.

## Blast radius

`Storyteller.Core.Types`/`Storage`/`Git` in particular are imported almost
everywhere (45 files at the time of this move) — they're the closest thing
this codebase has to a standard library. Renaming them was a pure mechanical
move (module path + import lines only), no behavior change; the git history
for this commit is a straightforward rename diff.
