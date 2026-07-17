# Storyteller: Design Document

## Overview

Storyteller is an LLM-powered story development system that treats narrative as **queryable, versioned state**. It combines a git-backed data model, hierarchical summarization, and entity knowledge tracking to enable sophisticated story development workflows.

For the full data model — git structure semantics, entity branches, causal graph, worktree/submodule layout — see **[DATA-MODEL.md](DATA-MODEL.md)**.

### What This Is For

The target use case is not serious literary authors — a Brandon Sanderson gains little from LLM assistance regardless of how well the context is managed. The target is the much larger, mostly silent audience of people who use LLMs to *explore* stories: fanfiction writers who never publish, people running long-form character interactions, anyone who wants to read the story they want to read without writing every word themselves.

This audience already uses tools like SillyTavern and similar. Their problems are not quality — the bar is "entertaining and coherent," not "artistically defensible." Their problems are structural: character drift over long sessions, context limits causing degradation and looping, no persistent state between sessions, no way to fix a character that has gone wrong at the root rather than patching around it each time.

Storyteller doesn't make LLM output better or faster. It provides a framework that makes these structural problems solvable incrementally, without building a monolithic agent loop that becomes harder to modify the more features it accumulates. The data model handles persistence and consistency; the agent architecture stays composable; the author stays in control of how much or how little the system does.

---

## Core Principles

1. **Prose carries craft. Entity branches carry perspective.** Story branches are the product; entity branches are partial views. The gap between them is where dramatic irony and unreliable narrators live. See DATA-MODEL.md.
2. **Natural language everywhere** — LLM-generated summaries, no rigid schemas
3. **Hierarchical navigation** — zoom from chapter summaries down to prose via summary checkpoints
4. **Entity branches as partial views** — named perspectives on the world, most commonly characters; depth and scope are author-controlled
5. **Non-destructive experimentation** — branch, rewrite, synthesize with full history preserved in temporal branches
6. **Minimal prescription** — system adapts to your organization, no rigid structure required

---

## Design Philosophy

**Simple core, emergent complexity:**
- Only `.storyteller/` has prescribed meaning (system cache/config)
- Entity branch depth is author-controlled, not system-mandated
- File organization flexible (system scans and adapts)
- Commit format flexible (minimal = summary line, metadata optional)
- Everything derived from git state (no external databases)

**The data model is the feature.** Consistency checking, knowledge filtering, dramatic irony, unreliable narrators, and distinct POV voices are not bolt-on features — they are natural consequences of maintaining entity branches and navigating summary trees correctly. See DATA-MODEL.md for why.

---

## File Organization

**Philosophy:** Minimal prescribed structure. System adapts to your organization.

### What's Fixed

**`.storyteller/` — System files only** *(design; not how configuration actually works today)*
```
.storyteller/
  config.yaml      # Optional configuration
  cache/           # Derived data (not in git)
    embeddings/    # RAG index (rebuilt from git)
```

What actually configures a run today is environment variables read at process/connection start — `STORY_REPO`, `STORY_BRANCH`, `LLAMACPP_ENDPOINT`, `STORY_MODEL`/`JUDGE_MODEL` (see `Storyteller.Core.LLM.Registry.knownModels`), `ACTIVE_CHARS` — not a `.storyteller/` project directory. No embeddings cache exists, since no RAG index exists yet (see Open Questions). Whether a project-local config file is ever worth adding on top of env vars is still open.

### Everything Else: Your Choice

```
/storyteller-project
  .git/
  .storyteller/

  chapters/              # Or: story/, manuscript/, scenes/, whatever
    01-theft.md
    02-investigation.md

  characters/            # Or: cast/, people/, inline with chapters
    alice-chen.md
    detective.md -> alice-chen.md    # Alias via symlink
    bob-martinez.md

  world/                 # Optional
    warehouse.md
    magic-rules.md

  meta/                  # Optional: planning, outlines, notes
    outline.md
    tasks/
      story.md
```

**Canonical name:** Derived from filename (`alice-chen.md` → "Alice Chen") or first heading. Aliases via symlinks.

### Global Configuration

```
~/.storyteller/
  characters/            # Meta characters available to all stories
    writing-coach/
    harsh-critic/
  config.yaml
  templates/
```

**Character discovery:** Story `characters/` first, falls back to `~/.storyteller/characters/`.

---

## Characters

### Story Characters vs. Meta Characters

**Story characters** exist within the narrative. They have entity branches tracking their knowledge and internal state. They only know what their branch contains.

**Meta characters** exist to help the author. No entity branch — they access the full main story when consulted. Examples: Writing Coach, Harsh Critic, Character Therapist.

The distinction is storage location and whether they have an entity branch. No explicit access level field needed.

### Character Definition

**Minimal (sufficient):**
```markdown
# Alice Chen

Detective with trust issues. Dry humor, guarded personality.
```

**Detailed (optional):** Any structure the author finds useful — voice patterns, dialogue examples, public/private personas. The system infers from content; no prescribed sections required.

### Entity Branch Depth

Entity branches exist on a spectrum:

- **Minimal:** No branch, or a stub — a character sheet the LLM uses when the entity appears
- **Standard:** Character sheet + biography tracking significant events
- **Deep:** Structured file tree, frequently updated, selectively loaded per scene

The author decides. The system doesn't require any particular depth. "Entity" is an abstract concept — most commonly characters, but also institutions, and occasionally locations or objects when tracking their evolving state from a particular perspective is useful. See DATA-MODEL.md. What's actually built today is characters specifically (`character/{id}` branches, `sheet.md`/`journal.md`, presence tracking) — nothing about the mechanism is character-specific in principle, but a non-character entity branch hasn't been exercised in practice yet.

---

## Agent Architecture

### Design Philosophy

**Code-based agents** for maximum flexibility, written in Haskell. Agents define their own:
- Context assembly (what data they need, at what granularity)
- Prompts (including system prompts)
- When/how they run
- What they write and where

Users can use pre-built agents or define their own.

**Agents are just functions over effects.** An agent that only needs `LLM` is a pure LLM call with no storage side effects. One that needs `FileSystemRead` is read-only by construction — the compiler enforces it. One that needs `StoryBranch` is explicitly reaching into the narrative structure. The effect constraints are the API surface; capability boundaries are not conventions, they are types.

Most useful agents don't need to know anything about ticks or branches. A style checker, a research tool, a scene outline generator — these are just `LLM + FileSystemRead`. They work identically whether the filesystem underneath is a git-backed branch, a local directory, or anything else. The narrative structure is opt-in, only for agents that genuinely need it.

**Agents, tools, and workflows are the same thing.** Any function over effects can be called directly, composed into a pipeline, or exposed as an LLM tool. There is no architectural distinction between "an agent" and "an LLM-powered function" — a single-shot LLM call, a multi-step pipeline, and a decision loop with tool use are all just `Sem r a`. This is what makes the composability structural rather than aspirational.

A natural consequence: agents can hand the model an actual tool call rather than only free-form text, with no separate machinery for it — a tool is just one of these functions wrapped with a name and parameter schema. The useful pattern this enables is giving the model a tool it cannot misuse: bind the target (which tick, which file) into the tool via closure rather than exposing it as a model-supplied argument, so the model's only latitude is the judgment call itself — does this need to change, and to what. This is how in-place atom editing works today: the model decides whether an atom needs revision and supplies the replacement text, but never the atom's identity.

Because an agent is itself just such a function, this composes recursively: an agent — including one that calls the LLM itself, or drives its own tool-use loop — can be wrapped and handed to a *different* agent as one of its tools, no differently from wrapping a plain effectful function. Subagent delegation isn't a distinct feature to design; it's the same wrapping applied one level up. This is a departure from frameworks (e.g. LangChain) where agents and tools are separate abstractions with their own APIs — here there's one abstraction, so nesting them is free rather than requiring an adapter layer.

It also means a piece doesn't need to be independently useful to be worth extracting — a subfunction that only makes sense as a step inside some larger agent is still worth pulling out if it's reused, because it's still just a function: partial application, `map`/`filter`/`fold` over collections of them, all the ordinary combinators apply before something is handed off as a tool, same as they would anywhere else in the codebase. The one boundary requirement, and the only one, is descriptive types at the edges — the return type needs a codec-backed description, and so does every parameter; how the pieces behind that boundary got composed is invisible to the model and unconstrained by it.

### Input Modes

The core axis is: **how is input interpreted, and what does it produce?**

| Mode | Input interpreted as | Output |
|---|---|---|
| Verbatim | Text to add to the prose as-is | Committed directly, minor fixups only |
| Instruction | Narrator direction — what should happen | Generated prose committed to story branch |
| Discussion | Planning, questions, analysis | Chat response only, nothing committed |
| Character | Text directed at a specific character | Character's response, not added to prose |

These are input modes, not separate agents. This table is the original conceptual axis; see "Interaction Modes" below for how it actually resolved once built — a per-message agent selector plus a `chat/` file convention, not the `/as alice`/`(( instruction ))` command syntax sketched here, which was never implemented. Multiple modes can coexist in a session — a director instruction followed by verbatim prose followed by an out-of-character question is a normal authoring sequence.

The writing style within instruction mode is also configurable on a spectrum:

| Style | Behavior |
|---|---|
| Minimal | Spelling/grammar fixes only, author controls all prose |
| Collaborative | Generates complete passages from brief instructions |
| Generative | Full scene generation from high-level direction |
| Swipe | Multiple alternatives per section for exploration |

Custom styles are pluggable for specialized output (screenplays, poetry, game dialogue, etc.).

### Utility Agents

What's actually built today, in `src/Storyteller/Writer/Agent/`:

**Write/FlowWrite** (`Write.hs`, `FlowWrite.hs`) — the main prose-continuation agent. Given already-gathered context (see Context Assembly below), builds a real per-call `[Message]` history and generates the next passage. `FlowWrite` is the same agent aware that a generation may already have been in flight when a new instruction arrived: it revises whatever was written provisionally since the user started typing (via the Fixer, below) before continuing on top of it.

**Fixer** (`Fix.hs`, `ReplaceTool.hs`) — given an instruction and a set of already-written atoms flagged as its target, decides per-atom whether it needs to change, and if so how: a full rewrite (`replace_atom`) or a targeted, exact-match-once span swap (`replace_text`) that leaves the rest of the atom untouched. Records its own reasoning as a `Fixup` tick, distinct from a user's `Note`, so a later reader can trace why an atom changed.

**Chat** (`Chat.hs`) — discusses the story rather than continuing it. Read-only tool access (`glob`/`read_file`/`sed_print`) over the branch; nothing it does gets committed except the final reply, via the `chat/` file convention (see WRITER.md) that pairs prompt/reply ticks as a conversation instead of prose.

**AskCharacter** (`AskCharacter.hs`) — answers a question grounded only in one character's own branch (sheet, journal, anything else tracked there), never the scene being written or any other character's material. What the correction loop's "character was wrong" case (below) actually queries against before deciding whether a branch fix is warranted.

**Outline/Split** (`Outline.hs`) — the outline → beat-sheet → chapter pipeline (`outline.md` → `ch{N}.outline.md` → `chapters/ch{N}.md`), plus chapter reconciliation when an already-written chapter's beat sheet changes underneath it.

**Tracker** (`Tracker.hs`) — copies new atoms from a trackee branch into a tracker branch, one tracker atom per trackee atom, with cross-branch refs recording the source. The mechanical "carbon-copy" baseline described below, as a real, working primitive rather than just a fallback story.

**Images** (`Storyteller.Core.Image`) — a dedicated tick type, not an agent: an image asset can be attached to a file's timeline (uploaded, or referenced by dragging one from the file tree) and renders inline as an author-facing annotation, same tier as a Note. Not passed to any agent's context today — no vision model call exists yet, so an attached image informs the author, not the LLM. Straightforward to add (bind it in as another message part for vision-capable models), deliberately not done yet: it would be a surprising default (images silently entering the prompt) and a hard compatibility gate (many configured models, especially local ones, aren't vision-capable). Planned as opt-in, not assumed.

None of the following, despite being useful ideas, exist yet: a dedicated Knowledge Filter Agent beyond the Tracker baseline, a Consistency Checker Agent, a Merge/Reconciliation Agent for divergent branches (not to be confused with `Outline.hs`'s `reconcileChapter`, which reconciles one chapter against an edited beat sheet — a different, smaller, already-built thing), a Synthesis Agent, or a Task Tracking Agent. They're worth building; nothing below this line should be read as already true.

The baseline (Tracker's carbon-copy) is already doing real work: a character who was present for a scene has that scene's prose in their branch, which is enough for the LLM to write them consistently in subsequent scenes. LLMs have contextual understanding of what a character notices based on how the prose is written — a character written as oblivious will be treated as oblivious even if the information they're missing appears nearby. Knowledge leaks that do occur are caught on author read and fixed by amending the entity branch at the point of the leak; the fix is permanent. Whether a leak goes unnoticed... if it doesn't cause a visible problem in the prose, it doesn't matter.

Summary checkpoints, a routing/filter agent split for large-scale context selection, and the hierarchical navigation they'd enable are all still design, not code — see Open Questions.

### Context Assembly

What a scene's generation call actually assembles today (`Write.hs`'s `writeAgent`, `buildChapterMessages`) is a real `[Message]` history, not one flattened prompt, in a fixed order:

1. **World lore** — every lore-eligible branch file (`Storyteller.Writer.Lore.isLoreEligible`), read unconditionally, not selected by relevance. One stable early message.
2. **A style guide** (`style.md`) — appended to the system prompt, not a message; a project's standing voice/tone instructions.
3. **Earlier chapters**, oldest first — each one's full current prose, verbatim, one message each. This is what keeps a later chapter consistent with something only an earlier one established.
4. **Every active character's identity** — `sheet.md` plus other tracked context, one block per character, near the start since it's mostly stable for a whole chapter.
5. **This chapter's own conversation so far** — the file's own Prompt/Atom tick pairs, reconstructed into alternating turns.
6. **A shallow splice** — pinned/short-term context (whatever the author selected for this call) plus each active character's curated recent journal excerpt — placed close to the live edge of the conversation, since it's the most volatile part.
7. **The new instruction** — always last.

Selection *within* this — which of a branch's other files count as lore, which count as a given character's context — is author-curated today via a bucket/glob picker in the frontend (`context-source.tsx`), fully wired end to end (branch → WS message → dispatch → assembly), not a heuristic the backend guesses at. There is no relevance-ranking routing agent and no semantic/RAG selection: everything assigned to a bucket is included, every call, in full. That's the real, current form of what this section used to describe as a routing/filter agent split — deliberately simpler, and the one place this architecture doesn't yet have an answer for what happens once a story's total context stops fitting in the window at all (see Open Questions).

---

## Generation Workflow

### Auto-Commit

Every prose paragraph is saved immediately and automatically. Users never decide when to save.

- **User prose:** Added as-is (LLM may offer minor fixups)
- **Generated prose:** Added after each paragraph as it's produced
- **Instructions / questions:** Nothing added to the story — produces chat responses, annotations, or branch updates only

Editing a paragraph is not a new entry — it amends the existing one and rebases what follows. The story branch always reflects the story as it should be read.

### Raw File Editing *(planned)*

Auto-Commit above assumes LLM-driven, paragraph-at-a-time authoring. A separate surface is planned for direct file editing: the frontend renders a file's raw working-tree content instead of its split-into-atoms view, and edits — free-form changes, switching between files, uploading new ones — accumulate in that working tree without creating a tick per keystroke. See WS-PROTOCOL.md's "Working tree access" for the planned wire-level shape.

Saving is the only point where accumulated changes are reconciled: a diff-and-merge pass sorts them into one or more atoms on the tick chain, so the append-only invariant (see DATA-MODEL.md) always holds once saved. Not yet implemented.

### Regeneration and Swipes

"Regenerate" is built, and simpler than originally designed here: not a parallel branch off the same point, but an alternate-content carousel on the atom itself (`Storyteller.Common.Swipe`) — `pushSwipe` swaps in a new alternative, `cycleSwipe` rotates through what's been generated so far, all in place, with the tick count unchanged. No separate temporal branch, and nothing to garbage collect.

### Knowledge Filter Timing *(design, not yet built)*

Beyond the Tracker's mechanical carbon-copy (see Utility Agents), there's no capable filter agent deciding *what* a character would have experienced and how, nor the batching/on-demand timing model this section originally sketched. What's here is still the intended shape once one exists:
- After each scene (default)
- In batches (for local models where caching efficiency matters)
- On demand before a scene that needs a specific character's current state

### Merging *(design, not yet built)*

No Merge/Reconciliation Agent exists yet — divergent branches have no automated reconciliation path today. The intended shape, once built:
1. Identifies the divergence point and all causally dependent commits
2. Runs consistency checker on affected range
3. Generates a reconciliation plan (which commits need rewriting, in what order)
4. Presents plan to user for approval
5. Executes: rewrites scenes, updates entity branches, regenerates summary checkpoints
6. Final consistency check

---

## Interaction Modes

What's built is narrower and more concrete than the mode-switching model this section originally described: there's no single session-wide `<Tab>`-cycled prose/agent/discussion switch. Instead, each message picks which agent handles it, and discussion lives in its own file convention rather than a session mode.

### Agent Selector (per message)

The frontend's input bar carries a small, fixed set of routable agents, cycled with shift-tab: **write** (Writer, or FlowWriter if a generation was already in flight), **fix** (targets the current atom selection via the Fixer), **append** (verbatim, committed as-is), **note** (annotation, nothing generated), **regen** (swipe — an alternative off the same point). This is the practical equivalent of the old "input modes" table (verbatim/instruction/discussion/character), just resolved per-message instead of as a standing mode the whole session is in.

### Discussion

Not a session mode — a file convention. A file under the `chat/` path (see WRITER.md) renders its Prompt/Atom tick pairs as a conversation (user/assistant bubbles) instead of prose, backed by the same `Chat.hs` agent described under Utility Agents: read-only tool access, nothing committed but the final reply. Planning and analysis without touching the story is "open a chat file," not "switch modes and come back."

### Character Mode

Talking to a character (see Character Interaction, below) is its own thing: directed at one character's branch via `AskCharacter`, answered from what that branch alone contains, never added to prose automatically.

### What's Still Just Design

A single tool-access ceiling that scales up for "editing passes, restructuring, maintenance tasks" (the old Agentic Mode) isn't built — the Fixer and FlowWriter already cover targeted, bounded revision, but nothing yet gives a model broad read/write reign across a whole story's branches at once. Worth watching whether it's ever actually needed once the narrower agents have more real usage behind them, rather than building it speculatively now.

---

## Character Interaction

### Talking to Characters

Character mode is an input mode, not a separate system. Whatever you type gets the character's response based on their branch; the response is shown in the interface but not added to the prose. The `/as`-command syntax below is the original design sketch, not implemented as such — the real mechanism is `AskCharacter.hs` (see Utility Agents), reached today via the sidebar's Ask panel rather than a typed command prefix. Meta characters (Writing Coach, Harsh Critic — full story access, no entity branch) remain design, not built.

```
/as alice-chen       # Enter character mode — input directed at Alice, responses from her branch
/as writing-coach    # Writing Coach has full story access (meta character)
/as                  # Return to narrator/director
```

Characters are oracles, not authors. They answer questions — what would they say, do, notice, feel — and the narrator decides what to do with the answer. A character response might become a line of dialogue, inform how the narrator writes the scene, or be discarded entirely. The character doesn't add to the story; the narrator does, armed with the character's answer.

### Correction Loop

When generated output is wrong, there are four distinct cases that route differently — the fourth is a real, tested mechanism as of this writing (`ReplaceTool.hs`, exercised live in the agent-integration suite), the rest are the original design and still the intended shape:

**Character was wrong, at the root** — the prose misrepresents who the character is or what they know, and the fix should hold for every future scene, not just this one.
- Append or edit an entry in their branch (`journal.md`) explaining the correction, at the relevant fiction-time position
- Regenerate — the constraint is now permanent, because every future call to `writeAgent` reads that branch's current state
- This is proven to actually change generated behavior, not just theorized: a manually-added journal resolve measurably changed next-scene output, and a retroactively-edited journal entry produced correct dramatic irony (the character's own behavior consistent with a secret, without leaking it) in live testing against a real model.

**This one atom was wrong, locally** — a single already-written passage needs a targeted fix, but nothing about the character or the story's facts needs to change going forward.
- The Fixer: point it at the atom, give it an instruction, it decides whether and how to change just that span (`replace_text`) or the whole atom (`replace_atom`)
- Nothing goes to any branch — this is a prose-level fix, not a root-cause one, and is the more common case in practice

**Narrator misunderstood** — the prompt was interpreted incorrectly, the wrong thing happened.
- Edit or replace the instruction, regenerate
- Nothing goes to any branch; this was a prompt failure, not a character failure

**Bad roll** — the prompt was right, the character is right, this particular output is just flat or off.
- Swipe: regenerate with the same prompt, pick the better result
- Arrow-up to edit the prompt slightly, or shift-arrow-up to give refinement instructions

The distinction matters: routing a bad roll into a branch entry adds noise; routing a character failure to a swipe means it happens again next time; and reaching for a branch-level fix when a local atom fix would do costs a live regeneration of everything downstream for no reason.

---

## Querying *(design, not yet built)*

**Exact (git)** is real today, since a character branch is just a git branch — this works right now, with no special tooling:
```bash
git log character/alice                    # What does Alice know?
git log main --not character/alice          # What doesn't she know?
git log character/alice character/bob       # Shared experiences
```

**Semantic (RAG)** is not built — no embeddings index, no `storyteller search` CLI. The "one real, deliberately deferred gap" once context stops fitting in a window at all (see Context Assembly, Open Questions).
```bash
storyteller search "when did Alice feel betrayed?"   # aspirational, doesn't exist
```

---

## Consistency Checking *(design, not yet built)*

No Consistency Checker Agent exists. What author-facing consistency checking exists today is manual: reading the prose, noticing a contradiction, and fixing it via the correction loop (see Character Interaction). The intended shape, once a dedicated checker exists:

**What to check:**
- Does this character reference knowledge not in their branch at this fiction-time position?
- Timeline contradictions?
- Voice consistency?
- Lore adherence?

**When:** After scene/chapter completion (optional), before merge (recommended), on demand.

**How:** Queries entity branches at the relevant fiction-time position. Flags violations with references. User reviews — stories can break rules intentionally.

Not computationally solvable. System assists, user decides.

---

## Task Tracking *(design, not yet built)*

No task-tracking agent or auto-injected task files exist. The intended shape:

```markdown
# meta/tasks/story.md
- [ ] Reveal employer identity by end of Act 2
- [ ] Resolve Alice/Bob relationship arc

# meta/tasks/alice-chen.md
- [ ] Find out who Bob is working for
- [ ] Decide if she can trust him again
```

LLM can complete tasks, add tasks, and query what's pending. Prevents forgotten plot threads.

---

## Use Cases

These describe the target vision and mix built and not-yet-built capabilities freely (e.g. "consistency checking," "merge with LLM-assisted reconciliation," and manuscript export below are all still design — see their own sections above for what's actually built in each area).

### Novel Writing
Define structure, generate scenes, view at multiple zoom levels, branch to try story directions, consistency checking, export to manuscript.

### TTRPG / Interactive Roleplay
User plays character(s), system plays world using entity branches. NPCs only know what their branches contain — no cheating. Session log becomes story source material.

### Collaborative Writing (Async)
Multiple authors on branches, characters with "owners," merge with LLM-assisted reconciliation. Standard git workflow (push/pull).

### Visual Novel Authoring
Git branches for routes/endings, merge when routes reconverge. Consistency checking per route. Edit early choice points and see downstream impact across all routes.

---

## Implementation Notes

### Language: Haskell
Strong typing, good git libraries, excellent for DSLs and agent definitions, parser combinators for commit message parsing.

### Interface: Frontend-agnostic
The backend exposes a WebSocket server that any frontend can connect to — this is real and working today, not just a target: `story-server` plus a Next.js web frontend (`frontend/`) built against it, communicating over the protocol documented in `WS-PROTOCOL.md`. The WebSocket model fits the domain well in practice: sessions are stateful, connections are scoped to a branch, and the server pushes updates rather than waiting to be polled. TUI and other surfaces remain possible without backend changes, but none exist yet — the web frontend is the only one built.

### WebSocket Architecture
Each connection is scoped by URL: `/session` for storage-level operations (branch management), `/branch/{name}` for branch-level operations, `/branch/{name}/{path}` for one file's own operations (writer/chat/fixer commands, presence). The branch name is implicit from the URL — commands never repeat it. On connect, the server sends a full snapshot of the branch's current file contents; subsequent events are deltas. Multiple connections to the same branch all receive updates, enabling multiple windows to stay in sync without coordination. Background agents write to the branch and fan out to all connected clients via STM pubsub, backed by a dedicated git-storage worker thread for batched writes.

### Testing Against Real Models

Unit tests (`test/`) run against mocked, deterministic interpreters — they check plumbing, not whether an agent actually gets a real model to do the right thing. A separate suite (`test/agent-integration/`, see its own `PLAN.md`/`FINDINGS.md`) answers that second question directly: real LLM calls, cached to disk so a passing run replays without hitting the network, checking things like "does a character actually get recognized when added to a scene," "does a planted lore fact reach the model," "does editing a character's journal retroactively produce correct dramatic irony." A mismatch there is a finding about the configured model or prompt, not a bug in the suite — the suite existing and passing isn't the goal, what it reveals about the model is.

### Import/Export

**Character card import is built**: SillyTavern-style Tavern Card V1/V2/V3 (JSON or PNG with embedded `tEXt` metadata) imports as a new character branch in one atomic command (`ImportCharacterCard`) — identity/personality/scenario into `sheet.md`, opening message/example dialogue/system prompts into `instructions.md` (parked for a future roleplay agent), an embedded `character_book` into `lore.md`, and provenance/creator notes as a free-floating `Note` tick rather than sheet.md prose. Parsing is client-side (`frontend/src/lib/taverncard.ts`), no server-side dependency on the card format.

Everything else here is still *(design, not yet built)*:
- **Import:** Blank project (templates), existing manuscript (parse and commit)
- **Export:** `cat chapters/*/*.md > story.md`, clean manuscript, EPUB/PDF via pandoc, screenplay format

`cat chapters/*/*.md` already works today because it's just shell against plain files, not a feature of the system.

### External Editor Integration *(design, not yet built)*

No file-watching or auto-detection of external edits exists. The intended shape, once built:
```
Detected changes in chapters/03-confrontation/chapter.md
Diff: [shown]
Commit summary? > Sharpen Alice's dialogue
✓ Committed
```

---

## Open Questions

- **On-demand retrieval at scale:** the one real, deliberately deferred gap in Context Assembly today. Everything assigned to a bucket is read in full, every call — fine at the scale tested so far, with no answer yet for what happens once a story's total lore/chapter history stops fitting in a window at all. The mechanism this needs is smaller than "RAG" implies, though: `Chat.hs` already proves the shape works (tool calls the model can choose to make, whose results shape only that one turn and never get persisted back into history) — `writeAgent` just doesn't have that tool-calling shape yet. The missing piece is a manifest of what's *available but excluded* (path plus a short blurb) plus a bounded tool round before committing to prose, not an embeddings index. Ranking/narrowing that manifest is genuinely additive on top: a `grep`-over-blurbs tool covers it while the corpus is small, and an embeddings-backed search tool is a drop-in upgrade if it ever isn't — neither requires revisiting the underlying decision to make this a tool call. Collapsing settled chapters/sessions to one summary atom (full fidelity still reachable out-of-namespace) remains a separate, complementary idea for shrinking what's in the *included* buckets, not a substitute for this.
- **Agent marketplace:** Community-built agents distributed as git repos (no special infrastructure needed)
- **Voice consistency at scale:** Voice examples in character card + periodic voice-checking agent + user reviews flagged inconsistencies
- **Image generation:** Character portraits, scene backgrounds — straightforward tool integration (ComfyUI, DALL-E, SD APIs), not essential for MVP

## Future Possibilities

- Cross-story character transplanting (archetype is portable)
- Story analysis dashboard (emotion curves, pacing, character frequency)
- VN engine export (Ren'Py, Twine, Ink)
- TTS integration with character voices
- Translation/localization with cultural adaptation
- Franchise/shared universe management
- TUI (the web frontend already exists; a terminal surface against the same WebSocket protocol remains open)
