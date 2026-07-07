# Project structure — two Core/Writer splits

This project has two separate Core/(something) splits, cutting on two
different axes. Don't confuse them:

1. **`Server.Core` vs. `Server.Writer`** — generic connection/dispatch
   infrastructure vs. this-app-specific assembly and business logic. The cut
   is "would this survive unmodified in a sibling app (Roleplay, Lector)."
2. **`Storyteller.Core` vs. `Storyteller.Writer.Agent`** (with a `Common`
   middle tier) — the tick/atom/git storage engine vs. the story-writing
   business logic built on top of it. The cut is "is this the substrate
   every app would share, or the policy that makes this app what it is" —
   the closest thing this codebase has to "user-serviceable parts."

Both splits reuse the word "Core" because they're the same *shape* of
decision (substrate vs. specific), just applied at different layers — the
server's request-handling layer, and the underlying storage SDK.

Background/principles this follows: `WS-PROTOCOL.md`'s scope-design section,
and the [[feedback_app_specific_not_universal]] memory (QtCreator not
Notepad++ — duplicate glue rather than build extension points).

# Part 1: Server — `Server.Core` vs. `Server.Writer`

## Mental model

`Server.Writer` is not a child that inherits from `Server.Core`. `Server.Core`
is what you'd get by extracting the common operations out of `Server.Writer`,
`Server.Roleplay`, and `Server.Lector` — a library factored out of siblings,
not a base class siblings extend. The test for "does this belong in Core"
isn't "does this look app-agnostic" (everything looks app-agnostic with one
app in existence), it's "would this survive unmodified if Roleplay or Lector
needed it too." Env/notification/connection assembly stay in Writer even
though nothing about the code itself makes them look Writer-specific,
because there's no second app yet to check that against.

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

Connection lifecycle and notification handling are assembly, not library,
even though `Server.Writer.File.Connection`/`Server.Writer.Branch.Connection`
look structurally identical to each other. That similarity is a coincidence
of only having one app so far, not evidence they're a reusable "tick
notifier" abstraction — Branch and File already handle `TicksRemapped`
differently (File forwards it to the client as `tick.remap`; Branch drops
it). Don't extract a shared notifier and risk a mismatch surfacing later;
let the connection+notification modules diverge freely per app until a
second app actually forces the shared part into view.

## `Server.Core.*` — pure library, no assembly

| Module | Contains |
|---|---|
| `Server.Core.Protocol` | `WireTick`, `Update`, `toWireTick`, `tickToWireTick`, `withId` — wire-shape data + pure conversions |
| `Server.Core.Util` | `withBranch` — generic scope-opening helper |
| `Server.Core.Run` | `SessionEffects` type alias — a type-level list of effect memberships (Random/Sleep/Time/Git/Fail/Logging/Error/StoryStorage/LLM). A declaration any library function can require, not wiring — kept separate from the rest of `Run` (which is the interpreters that satisfy it) so library code can depend on the constraint without depending on Writer's assembly. |
| `Server.Core.Logging` | Generic "what ran and how long it took" wrapper around a connection's command-dispatch loop — logs to server stdout directly (deliberately not through `Runix.Logging`; see its own Haddock), reusable by any app's File/Branch/Session connection |
| `Server.Core.Branch` | `BranchOpen`, `branchState`, `branchStateSince`, `moveTickInBranch`, `deleteTickFromBranch`, `addNote` (re-exported from `Storyteller.Common.Annotation`) — generic tick-chain state/mutation, no JSON/WebSocket |
| `Server.Core.File` | `FileOpen`, `fileState`, `fileStateSince`, `createFile`, `appendToFile`, `editFileAtom`, `deleteFileAtom`, `moveFileAtom`, `mergeFileAtoms`, `splitFileAtoms`, `chatNote`, `readFileContent` — generic atom-chain state/mutation, no JSON/WebSocket |

That's the whole of Core — six modules. Everything else is assembly (or
Writer-specific business logic).

## `Server.Writer.*` — assembly + Writer-specific library functions

| Module | Role |
|---|---|
| `Server.Writer.Env` | `ServerEnv`/`AppState` — app config and shared STM state |
| `Server.Writer.Run` | Effect-stack assembly: `actionStack`, `wsAction`, `loggingWS`, `streamChunksWS`, `gitNotify`/`storageNotify` wiring. Imports `SessionEffects` from `Server.Core.Run` rather than redefining it. |
| `Server.Writer.GitWorker` | A single shared git-storage worker thread for the whole process — one interpreter stack/`git cat-file --batch` reader for every connection and HTTP request to submit jobs to, instead of one per connection. See PLAN-git-storage-worker.md. |
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
| `Server.Writer.Character`/`.Protocol`/`.Connection` | `/character/{charBranch}` connection: the sidebar-facing view of a character branch (display name + `sheet.md` content) — knows the `character/{id}` naming convention from WRITER.md, which Core has no business knowing |
| `Server.Writer.ContextView.Protocol`/`.Connection` | `/branch/{name}/$context/{path}` connection: a read-only, preview-only sibling of `Server.Writer.File.Connection` — resolves a command's context slots into the files that would populate them, without running any agent or LLM call |

### Executable

`app/Server.hs` wires `Server.Writer.Session.Connection.runSession` and the
Writer File/Branch/Character/ContextView connection runners together in
`wsRouter`. This file is inherently the "final assembly" — it's the one
place that decides "this app is the Writer app."

## Test modules

`test/Server/TestStack.hs` is the shared interpreter stack. `BranchSpec`/
`FileSpec` exercise `Server.Core.Branch`/`Server.Core.File` directly (the
business-logic layer, not the wire protocol), run once each under an eager
interpreter and once buffered through `Storyteller.Core.Git.withStorage` (see
`test/Main.hs`). `NotificationSpec` exercises `Server.Writer.Notification`.

# Part 2: SDK — `Storage`/`Storyteller.Core` vs. `Storyteller.Common` vs. `Storyteller.Writer.Agent`

## `Storage.*` — the backend-agnostic chain engine

The content-addressed core the tick/atom/branch model is built from —
knows nothing about git, Polysemy, or Storyteller's own typed-tick
vocabulary (`TickId`/`TickData`/...). Any content-addressed object store
(git, or anything else) could supply a `MonadStore` instance and reuse this
unchanged; see [[project_storage_engine_swappability]].

| Module | Contains |
|---|---|
| `Storage.Core` | `ObjectHash`, `CommitData`, `MonadStore`, the `StoreT` monad and its primitives — `store`/`drop`/`at`/`readAt`/`follow`/`syncTo`/`reset`/`inWorktree`/`readFile`/`writeFile`, its own backend-agnostic `Tick` (`Atom` vs `NonAtom`) |
| `Storage.FS` | The ambient-tree directory/listing operations (`createDirectory`/`remove`/`removeRecursive`/`list`/`isDirectory`/`listChildren`) built on `Storage.Core`'s `getAmbientTree`/`modifyAmbientTree` seam |
| `Storage.Ops` | User-facing operations composed from `Storage.Core`/`Storage.FS`: `addAtom`/`append`/`editAtom`/`editAtomAt`/`commitWorktree`, plus the position-aware chain edits `moveTick`/`mergeAtoms`/`splitTick`/`deleteTick` |
| `Storage.Tick` | The bridge from `Storage.Core`'s own `Tick`/`ObjectHash` to Storyteller's typed `Tick`/`TickId` vocabulary — `storeAs`/`getTypesTick`/`readTypesTick`/`fileTicksOf`, and the wire encoding (`encodeTickData`) |

## `Storyteller.Core.*` — the storage engine

Everything that wires `Storage.*` to git and to Storyteller's own tick
vocabulary, with zero awareness of LLMs, prompts, or story-writing policy.
This is the part that would survive unmodified if a Roleplay or Lector app
were built on the same substrate.

| Module | Contains |
|---|---|
| `Storyteller.Core.Types` | `TickId`, `Tick`, `TickData`, `TickPos`, `Branch`, `BranchName`, the `TickType` typeclass, the `Root` tick kind — the base tick vocabulary |
| `Storyteller.Core.Storage` | The `StoryStorage` effect: `createBranch`/`deleteBranch`/`listBranches`/`getBranch`/`updateReferences`/`setRef` — cross-branch operations, always first-order (per-branch tick-chain operations live in `Storage.Core`/`Storyteller.Core.Branch` instead) |
| `Storyteller.Core.Branch` | `BranchOp`, `runStorage` — the Polysemy effect boundary that dispatches a whole closed `Storage.Core.StoreT` computation as one first-order effect per branch scope |
| `Storyteller.Core.Git` | The git-backed interpreters for `StoryStorage`/`BranchOp` (`runStoryStorageGit`/`runBranchOpGit`/`runBranchAndFS`) — ref layout, commit encoding, working-tree (de)serialization, the cross-branch reference cascade (`cascadeReplace`) |
| `Storyteller.Core.Atom` | The `Atom` tick kind (file-append ticks), for code still working in Storyteller's typed-tick vocabulary rather than `Storage.Core`'s own `Atom` |
| `Storyteller.Core.Create` | `createFile` — introduce a new, empty file as its own tick; a plain composition of `Storage.Ops` primitives |
| `Storyteller.Core.Prompt` | User-overridable agent-facing text (system prompts/templates), storable without a rebuild |
| `Storyteller.Core.Undo` | The undo tree: an append-only log of whole-repository snapshots, independent of `StoryStorage`/branches/ticks — pure `Runix.Git` vocabulary |
| `Storyteller.Core.Runtime` | `StoryModel`, the `Main` branch-phantom, `runInfrastructure`/`runStoryGit` — shared IO effect-stack assembly |
| `Storyteller.Core.CLI.Env` | `StoryEnv`, `loadEnv`, `modelConfigs` — the ENV-variable configuration shared by all CLI entry points |

## `Storyteller.Common.*` — reusable, but not foundational

Not every tick kind (or effect) is foundational the way `Root` is (every
branch needs a root tick regardless of app), but not everything is
app-specific either — some things any agent, in any app built on this
storage model, would plausibly want:

| Module | Contains |
|---|---|
| `Storyteller.Common.Types` | `Note` (user-authored comment on zero or more ticks), `Fixup` (agent-authored record of why it changed an atom) |
| `Storyteller.Common.Annotation` | `addNote` — the operation that creates a `Note` tick |
| `Storyteller.Common.Splitter` | The `Splitter` effect and `splitAtoms`/`splitByParagraph`/`byParagraph` — the policy for dividing raw text into atoms. Closer to a type/effect declaration than an agent, and needs to compose with `Storage.Ops.append` at the prose agents' call sites without either depending on the other — see below. |

`Fixup` is the fuzzier of the two `Common.Types` calls — it's currently only
produced by one agent (`Storyteller.Writer.Agent.ReplaceTool`, used by
`Fix`/`FlowWrite`), so "common" here is a judgment call rather than
something a second caller has already proven. Revisit if it turns out to be
genuinely Fix/FlowWrite-only.

## `Storyteller.Writer.Types`/`Storyteller.Writer.Presence` — Writer-specific tick kinds

Mirrors the `Common.Types`/`Common.Annotation` split, one level more
specific: `Storyteller.Writer.Types` holds `Presence`/`PresenceEvent` (the
`character/{id}` branch-naming convention from WRITER.md, which only means
something to this app), and `Storyteller.Writer.Presence` is the operation
(`recordPresence`) that creates a `Presence` tick — the same relationship
`Storyteller.Common.Annotation` has to `Storyteller.Common.Types`.

## `Storyteller.Writer.Agent.*` — the business logic

Named `Writer.Agent`, not bare `Agent`, because this is the "what does this
app actually do" policy layer — the least reusable part of the whole SDK.
Holds: `Agent` (shared cross-agent vocabulary — `UserInput`, `Instruction`,
`Prompt`, `Prose`, etc.), `CharContext`, `CharGen`, `Chat`, `ContextPreview`,
`Continuation` (the prose-generation core), `Fix`, `FlowWrite`, `Outline`,
`ReplaceTool`, `Tracker`, `Write`. There's no `Append` module here — see
below.

## Erring toward specificity

The rule for placing anything in `Storyteller.Core`/`Common` vs.
`Writer.Agent`: default to Writer-specific unless something *forces* the
boundary into view — a real second consumer today, not a plausible future
one. "Looks reusable" is not sufficient on its own. `Splitter` lives in
`Common` because appending generated prose has a genuine, structural need to
compose with it (see below) — not merely because splitting policy sounds
generic. `Tracker` looks just as reusable (copying atoms between branches
isn't inherently a Writer-only idea), but nothing depends on it outside
`Storyteller.Writer.Agent` today, so it stays there until something does.

## Why `append` and `Splitter` are split the way they are

`Server.Core.File.appendToFile` must work for any app, and it needs to
append a single atom verbatim. Core code can never depend on
`Storyteller.Writer.Agent` (Writer depends on Core, not the reverse), so
that operation — `Storage.Ops.append` — lives in the backend-agnostic
`Storage` layer and must not require the `Splitter` effect.

Splitting generated prose into paragraph atoms before appending each one is
a different, Splitter-dependent operation, needed only by the prose agents
(`Write`, `Fix`, `FlowWrite`). There's no dedicated function for this at
all — no `Storyteller.Writer.Agent.Append` module, no `appendAgent`. Each of
the three call sites just writes the composition out directly:
`mapM (append @branch path) =<< splitAtoms content`, using
`Storage.Ops.append` and `Storyteller.Common.Splitter.splitAtoms` straight
from their own modules. Two ordinary operations composed at the point of
use don't need a name of their own.

`Splitter` itself lives in `Common` rather than `Writer.Agent` specifically
so this composition is possible without `Storage.Ops` depending on
`Storyteller.Writer.Agent`.

## Cross-cutting note

`Storyteller.Core.Types`/`Storage.Core`/`Storyteller.Core.Git` are imported
by nearly every module in this codebase — they're the closest thing here to
a standard library. Any change to them has wide blast radius; check callers
broadly before editing their public interface.
