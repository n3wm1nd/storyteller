# TODO
> this file contains all tasks in no particular order that need to be worked on
> leave quotes like these verbatim intact and treat them as authoritative statements and instructions
> you can reorganize and reformulate the rest as tasks are worked on
> if you are encountering any uncertainty or open questions, add them with their tasks to get answers

# Frontend

## Filename extension handling
> we pretty much settled on using mardown files throughout, so we can decide to drop the extension entirely and just assume .md (maybe a toggle to re-display them)

**Goal:** stop showing/requiring the `.md` extension in the frontend UI (file trees, tabs, titles, etc.) since all files are markdown by convention.

**Context (current state):**
- Display sites, none of which strip extensions today:
  - File tree: `frontend/src/app/sidebar.tsx`, `buildTree` (17-36) splits path on `/` and shows each segment verbatim; `FileTreeNode` renders `node.name` as-is (line 77).
  - Open-file tab: `page.tsx:89` — `decodeURIComponent(selectedFile.split("/").pop() ?? "")` — full filename incl. extension.
  - New-file input: `page.tsx:349`, free-text path input, placeholder `"path/to/file.md"`; `handleCreateFile` (page.tsx:271-277) only trims and strips leading slashes, no extension logic — user must type `.md` themselves today.
  - No rename dialog exists anywhere (only create-file and delete flows).
- No extension-stripping/formatting logic exists anywhere in `src/lib` or `src/app` (confirmed by grep).
- No settings/preferences mechanism exists in the frontend at all — no settings store, no preferences panel. The closest analogues are plain ephemeral `useState` toggles in `page.tsx` (e.g. `annotationMode`, `showAllPresence`, `leftOpen`/`rightOpen`). A persisted "show extensions" toggle needs a new small mechanism (a store slice or localStorage-backed setting) — there's nothing to hook into.

**Decision (from project owner):** non-`.md` files keep their visible extension. Files without any extension differentiate from `.md` files by color and/or icon, not by extension text (since most files will be `.md` anyway).

**Work:**
- In the file tree, tab, and create-file input, strip `.md` from display for markdown files; leave other extensions visible as-is.
- Give extensionless files a distinct color/icon in the tree (new visual treatment — no existing icon/color-by-type logic was found to extend, this is greenfield UI work).
- Assume `.md` on create/write when no extension is given.
- Add a toggle (new settings mechanism, see above) to re-enable showing full filenames with extensions.

# Backend

## Characters list endpoint — DONE
> a characters WS endpoint that lists the available characters and heir metadata like name, open tasks, and any other information that we deem important for a "list of characters", note the overlap with the existing character endpoint, where you connect to when you need the character's full details. characters is for character selections, overviews lists. if it stays just an updated list of available characters with their name, that is perfectly valid too

**Goal:** add a lightweight list-of-characters source for selection/overview UIs, distinct from the existing per-character full-detail connection.

**Done:** `list-characters`/`character.list` on `/session` (`Server/Writer/Session/{Protocol,Dispatch,Connection}.hs`), payload is `{branch, sheet}` per character — raw sheet content, no server-side name extraction (client decodes the display name from the sheet's H1 line, per WS-PROTOCOL.md's "read is raw-but-complete" rule). Live-tracked via a second notifier thread on the session connection, filtering the existing `envNotifyChan` broadcast to the `character/` prefix — this also means edits to `sheet.md` push a fresh list automatically, not just branch creation/deletion (verified live against the running server). Frontend: `characterBranches` store state, wired into the sidebar's Characters tab, the "add to scene" picker, and the character accordion header, all showing real sheet-derived names now instead of the branch id.

**Context (current state):**
- Existing per-character connection: `Server/Writer/Character.hs` — `characterState` (line 41) strips the `character/` branch prefix for display name and reads `sheet.md`. `Server/Writer/Character/Connection.hs` — `runCharacter` (line 48), read-only, pushes `CharacterUpdate {ceName, ceSheet}` on connect and on `RefMoved`. Routed at `app/Server.hs:52` (`["character", name] -> ...`). This connection requires connecting per-character and reads full sheet content — too heavy for an overview list.
- Existing session connection (candidate integration point): `Server/Writer/Session/Dispatch.hs` already has `ListBranches` (line 31-33), returning **all** branches via `Storyteller.Core.Storage.listBranches` — no filtering by naming convention today. `Session/Protocol.hs` has `BranchList {seId, seBranches :: [T.Text]}`.
- Branch enumeration: `Storyteller.Core.Storage.listBranches` (`Storyteller/Core/Storage.hs:238-239`) is the only listing primitive; `getBranch` just filters it by name. **No existing prefix-filter helper** (e.g. `character/*`) — would be new code following the same prefix-stripping idiom already used in `Character.hs:43`.
- `Branch = Branch { branchName, branchHead }` (`Storyteller/Core/Types.hs:46-49`) carries no metadata beyond name/head — no portrait/color field exists anywhere in `src/Storyteller` or `src/Server` (grepped, zero hits). Only existing per-character file convention is `sheet.md`.
- Pattern for a brand-new WS endpoint (as `Character` does it): `Server/Writer/<Feature>.hs` (state assembly) + `Protocol.hs` (commands/events) + `Connection.hs` (lifecycle/notifier) [+ `Dispatch.hs` only if it needs commands] + a route clause in `app/Server.hs` (~lines 31-52). Adding to `Session` instead only touches `Session/Protocol.hs` (new event) and `Session/Dispatch.hs` (new case) — no new files.

**Decisions (from project owner):**
- "Open tasks" isn't tracked yet, but is cheaply derivable later (just reading a branch-scoped file) — not needed for v1.
- The actual point of this endpoint is avoiding the need to connect to every character individually just to render a picker/overview: v1 payload should cover name, plus fields like color/portrait once those conventions exist.
- Worth evaluating folding this into the existing session endpoint (extending `BranchList`/`ListBranches` with a character-filtered, richer variant) rather than standing up an entirely new endpoint — both are viable given the low implementation cost.

**Work:**
- Add a `character/*`-prefix filter over `listBranches`, mirroring the prefix-stripping idiom in `Character.hs:43`.
- Decide new-endpoint vs. session-endpoint-extension (leaning session, given the low cost of extending `ListBranches`/`BranchList`).
- v1 payload: character name (from branch name), derived directly from `listBranches` — no per-character `sheet.md` read required, keeping it a true O(1)-per-character list.
- Leave room to add color/portrait metadata once that per-character file convention exists (doesn't exist yet).

## Folder creation
> investigate supporting creation of empty folders in a branch

**Goal:** determine whether/how the storage and branch model can support empty folders, and implement if feasible.

**Context (current state — this is mostly already built):**
- `Storyteller/Core/Git.hs:94-97` — `data FSNode = FSFile !ObjectHash | FSDir`. Per the module doc (lines 22-25), directories are already explicit empty entries in the `WorkingTree = Map FilePath FSNode` (line 102) model, specifically so empty dirs round-trip through git commits (`loadWorkingTree`/`flushWorkingTree`/`buildTree`, lines 113-189) despite git itself having no native empty-tree concept.
- The git-backed FS interpreter `runStoryFSGit` (lines 580-670) already implements folder creation at the effect level: `CreateDirectory _recursive path -> modify @WorkingTree (Map.insertWith keepExisting path FSDir)` (line 638-640). `WriteFile` also auto-inserts `FSDir` for all ancestor dirs. `IsDirectory`/`ListFiles` are implemented too.
- Runix effect API: `Runix/FileSystem.hs` — `createDirectory :: Members [FileSystemWrite project, Fail] r => Bool -> FilePath -> Sem r ()` (line 125-126).
- No placeholder-file idiom (`.gitkeep` etc.) exists or is needed — `FSDir` is a first-class tree entry, not a workaround.

**Answer to "is an empty folder first-class or UI-only":** first-class — it's already modeled as an explicit `FSDir` entry at the storage-effect level, independent of any file living under it. The investigation the TODO asked for is effectively done; what's missing is only the plumbing above the effect layer.

**Work:**
- Add a `CreateFolder`-style command to `Server/Writer/File/Protocol.hs` + `Dispatch.hs` that calls straight through to `createDirectory @(BranchTag Main)`, mirroring how `appendToFile` (`Server/Core/File.hs:88`) calls into `FileSystemWrite`.
- Surface folder creation in the frontend file tree (new UI affordance — e.g. a "new folder" action alongside the existing new-file input in `page.tsx`).

# Both

## Upload/download — upload DONE, download still open
> drop files to upload to filetree, download too

**Goal:** support drag-and-drop file upload into the file tree, and downloading files back out.

**Done (upload only):** `upload` command on `Server/Writer/Branch/{Protocol,Dispatch}.hs`, calling `Storyteller.Core.Edit.commitFiles` (new — a `commitWorkingTree` variant scoped to just the given paths, see `Storyteller/Core/Edit.hs`) via `Server.Writer.Branch.uploadFiles`. Covered by `test/Server/Writer/BranchSpec.hs` (10 cases, both eager and `withStorage`-buffered). Frontend: native HTML5 drag-and-drop extracted into its own `frontend/src/app/filetree.tsx` (`FileTree` component, split out of `sidebar.tsx` since file-tree operations are expected to keep growing — rename/delete/move/folder-creation next). Verified end-to-end against the live server. Multi-file supported. Also gained a "create file" input in the same component while at it (just opens a not-yet-existing path; no folder-creation affordance yet, no rename).

**Still open — download:** no WS message and no frontend trigger exist yet. Per the earlier decision, this belongs alongside upload in `Server/Writer/Branch/{Protocol,Dispatch}.hs` — fetch a file's current content by path so the frontend can trigger a browser download.

**Context (current state):**
- No drag-and-drop exists anywhere in the frontend today (grepped `draggable`/`onDrop`/`onDragStart`/`dnd-kit`/`react-dnd` — zero hits). `sidebar.tsx`'s `FileTreeNode` is plain click-to-select. This is greenfield UI work; native HTML5 DnD is the lightest option given there's no existing pattern or library dependency to match.
- The frontend has no "write raw file content" WS message today — file content changes go exclusively through chat-agent-driven flows (`chatWrite` → `chat.writer` command → `Server/Writer/File.hs:47` `chatWriter`, which stores a `Prompt` tick, runs the Writer agent, and calls `Storyteller.Core.Append.append` for generated atoms). There's no `create`/`rename`/`upload` message variant in `Server/Writer/File/Protocol.hs` (only `"delete"`, line 79) — an upload path needs a new message type that writes content directly rather than round-tripping through an LLM agent.
- Closest existing lower-level mechanism: `Storyteller.Core.Edit.commitFile` (`Storyteller/Core/Edit.hs:374`), which reconciles a file's full working content against its atom history — built for agent-driven reconcile, not upload semantics, but likely the right thing to call into for "write this dropped file's bytes as the new content."
- Why move/rename isn't in scope: every `Atom` tick stores a `file` field (`atomFile :: FilePath`, `Storyteller/Core/Atom.hs:20-34`), used as a path hint to filter a file's chain without diffing trees (also used in `Git.hs:885,890`, `Writer/Types.hs`, `Writer/Agent.hs`). Renaming/moving a file would mean rewriting the `file` field on every historical tick for that path across the branch — expensive and invasive. Dropping a new file into a folder just assigns it a fresh path, with no historical rewrite needed, which is why this is tractable now and move/rename isn't.

**Decisions (from project owner):**
- Prefer multi-file upload over single-file if it's cheap to add — don't artificially restrict to one file if the drop handler and backend message can take a list with little extra cost.
- There is no "selection" for files/folders in the tree view today (confirmed — `sidebar.tsx`'s `FileTreeNode` is plain click-to-select with no selection state). So "drop lands where dropped" isn't a workaround for a missing selection, it's simply the only sensible target: the literal drop location in the tree is the destination.
- Both upload and download belong under the branch-level endpoint (`Server/Writer/Branch`), not under `Server/Writer/File`: neither is an operation on an already-open file connection — upload sends one or more files in bulk rather than "opening" them, and download just fetches content by path. `Server/Writer/File`'s message set stays scoped to operations on a connection that's already open for a single file.

**Work:**
- Frontend: add native HTML5 drag-and-drop handling on file-tree folder nodes in `sidebar.tsx`; accept one or more dropped files, landing them directly under that folder's path.
- Backend: add new WS messages for upload and download, both in `Server/Writer/Branch/Protocol.hs` + `Dispatch.hs` (existing at `src/Server/Writer/Branch/{Protocol,Dispatch,Connection}.hs`, alongside `Server/Writer/Branch.hs`'s existing branch-level logic like `trackFiles`/`charGen`):
  - Upload: writes one or more dropped files' content directly to their paths — likely via `Storyteller.Core.Edit.commitFile` or a similar direct write, bypassing the chat-agent pipeline.
  - Download: fetches a file's current content by path so the frontend can trigger a browser download. 