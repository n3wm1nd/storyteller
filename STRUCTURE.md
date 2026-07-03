# Server / Agent structure — Core vs. Writer

How the server is split between a generic `Server.Core` and the app-specific
`Server.Writer`, and how the same split will eventually apply to
`Storyteller.Agent.*`.

Background/principles this follows: `WS-PROTOCOL.md`'s scope-design section,
and the [[feedback_app_specific_not_universal]] memory (QtCreator not
Notepad++ — duplicate glue rather than build extension points).

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
| `Server.Core.Branch` | `BranchOpen`, `branchState`, `branchStateSince`, `moveTickInBranch`, `deleteTickFromBranch`, `addNote` (re-exported from `Storyteller.Annotation`) — generic tick-chain state/mutation, no JSON/WebSocket |
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
interpreter and once buffered through `Storyteller.Git.withStorage` (see
`test/Main.hs`). `NotificationSpec` exercises `Server.Writer.Notification`.

## Planned: agent split (`Storyteller.Agent.*`)

Not yet done. Same library-vs-assembly criterion will apply once this
happens — an agent that's just a reusable `Sem` function is Core/Common;
nothing about agents is "assembly" the way connections are, so this split
may end up simpler than the server one:

- **Core**: absolute tick/atom/branch basics — `Storyteller.Types`,
  `Storyteller.Storage`, `Storyteller.Git`, `Storyteller.Edit`,
  `Storyteller.Atom`, `Storyteller.Annotation`.
- **Common**: reusable but not foundational — candidates: `Splitter`
  (paragraph splitting, generic text utility), `Tracker` (cross-branch file
  tracking mechanism — reusable for e.g. RP NPC sheets too, even though
  today's only caller tracks chapters/characters).
- **Writer**: `Write`, `FlowWrite`, `Fix`, `Append`, `CharGen`, `CharContext`,
  `Continuation` — need individual confirmation once we get here; some (e.g.
  `Append`'s `appendUnsplit`) might turn out Core rather than Writer on
  closer look.
