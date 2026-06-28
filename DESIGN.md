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

**`.storyteller/` — System files only**
```
.storyteller/
  config.yaml      # Optional configuration
  cache/           # Derived data (not in git)
    embeddings/    # RAG index (rebuilt from git)
```

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

The author decides. The system doesn't require any particular depth. "Entity" is an abstract concept — most commonly characters, but also institutions, and occasionally locations or objects when tracking their evolving state from a particular perspective is useful. See DATA-MODEL.md.

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

### Input Modes

The core axis is: **how is input interpreted, and what does it produce?**

| Mode | Input interpreted as | Output |
|---|---|---|
| Verbatim | Text to add to the prose as-is | Committed directly, minor fixups only |
| Instruction | Narrator direction — what should happen | Generated prose committed to story branch |
| Discussion | Planning, questions, analysis | Chat response only, nothing committed |
| Character | Text directed at a specific character | Character's response, not added to prose |

These are input modes, not separate agents. Switching between them can be via commands (`/as alice`, `(( instruction ))`), key prefixes, or explicit mode switches. Multiple modes can coexist in a session — a director instruction followed by verbatim prose followed by an out-of-character question is a normal authoring sequence.

The writing style within instruction mode is also configurable on a spectrum:

| Style | Behavior |
|---|---|
| Minimal | Spelling/grammar fixes only, author controls all prose |
| Collaborative | Generates complete passages from brief instructions |
| Generative | Full scene generation from high-level direction |
| Swipe | Multiple alternatives per section for exploration |

Custom styles are pluggable for specialized output (screenplays, poetry, game dialogue, etc.).

### Utility Agents

Specialized agents for specific pipeline tasks:

**Knowledge Filter Agent** *(optional)*
- Input: New main branch commits, target entity
- Process: Routing agent (cheap) selects relevant branch files; filter agent (capable) determines what the entity would have experienced and how, writes the update
- Output: Commits to entity branch at correct fiction-time position
- Runs: Lazily, in batches, or as a maintenance task — git branch state shows exactly what's pending
- Baseline alternative: copy produced prose to active character branches directly, no LLM needed; refine selectively or post-hoc

The baseline (carbon-copy) is already doing real work: a character who was present for a scene has that scene's prose in their branch, which is enough for the LLM to write them consistently in subsequent scenes. LLMs have contextual understanding of what a character notices based on how the prose is written — a character written as oblivious will be treated as oblivious even if the information they're missing appears nearby. Knowledge leaks that do occur are caught on author read and fixed by amending the entity branch at the point of the leak; the fix is permanent. Whether a leak goes unnoticed... if it doesn't cause a visible problem in the prose, it doesn't matter.

**Summarizer Agent**
- Input: Set of commits (paragraphs, scenes, chapters) and the applicable summarization rule
- Process: Generate summary at the boundary defined by the rule (chapter, scene, N-commit span, etc.)
- Output: Summary checkpoint commit (identical tree, summary as commit message)
- Runs: As a background task when content is complete and stable — a chapter closes, a character exits, an arc resolves. Never blocks the writing workflow.
- Navigation: Agents traverse the resulting summary tree dynamically, drilling from chapter summaries down to individual commits only as needed.

**Consistency Checker Agent**
- Input: Commit range, entity branches at relevant fiction-time positions
- Process: For each fact in a commit, is there a prior commit in the relevant entity branch that sourced it?
- Output: Flagged inconsistencies with references (no commits)

**Merge/Reconciliation Agent** (orchestrator)
- Input: Two divergent branches
- Process: Identify causal dependencies, launch consistency checker, generate reconciliation plan, execute scene rewrites
- Output: Reconciled story graph — never a mechanical merge

**Synthesis Agent**
- Input: Multiple branches, synthesis instructions ("take dialogue from A, pacing from B")
- Process: Load branches at appropriate granularity, generate new version combining specified elements
- Output: Synthesized commits

**Task Tracking Agent**
- Input: Story state, character goals
- Process: Maintain task lists (story-level, per-character), track completion
- Output: Task markdown files, auto-injected into prompts

### Context Assembly

Agents load exactly what a scene needs — never the full story:

- **Routing agent (cheap):** Given a scene and entity file index, which files are relevant? Returns ranked list.
- **Filter agent (capable):** Loads flagged files, does the actual work.
- **Summary checkpoints:** Free navigation anchors — find nearest checkpoint, read message, only walk individual commits if more precision is needed.
- **Subagents:** For uncertain relevance, a micro-call reads a file and returns a relevance summary before the main agent decides whether to load it fully.

---

## Generation Workflow

### Auto-Commit

Every prose paragraph is saved immediately and automatically. Users never decide when to save.

- **User prose:** Added as-is (LLM may offer minor fixups)
- **Generated prose:** Added after each paragraph as it's produced
- **Instructions / questions:** Nothing added to the story — produces chat responses, annotations, or branch updates only

Editing a paragraph is not a new entry — it amends the existing one and rebases what follows. The story branch always reflects the story as it should be read.

### Regeneration and Swipes

"Regenerate" creates an alternative version — a parallel branch off the same point, not a new entry on the main story line. The user selects which version becomes canonical. Rejected alternatives are preserved in the temporal branch, eventually garbage collected from the active graph.

### Knowledge Filter Timing

Entity branch updates are lazy — the system knows what's pending from git branch state (`git log entity-branch..main`). Updates can run:
- After each scene (default)
- In batches (for local models where caching efficiency matters)
- On demand before a scene that needs a specific character's current state

### Merging

Merges are never mechanical. The Merge/Reconciliation Agent:
1. Identifies the divergence point and all causally dependent commits
2. Runs consistency checker on affected range
3. Generates a reconciliation plan (which commits need rewriting, in what order)
4. Presents plan to user for approval
5. Executes: rewrites scenes, updates entity branches, regenerates summary checkpoints
6. Final consistency check

---

## Interaction Modes

The session mode controls what the LLM has access to and what it produces at the macro level. Within a mode, input modes (verbatim, instruction, discussion, character) determine how each individual input is interpreted.

### Prose Mode (Default)

Minimal friction for linear writing. Every instruction produces prose committed immediately. The system prompt is stable for the duration of a chapter — active character sheets, scene context, recent prose — so the session is cache-friendly across many generation calls.

```
You are writing a novel. Continue the story naturally.
Scene: [current scope]
[active character sheets]
Recent prose: [last paragraph or summary]
Write the next passage. Just prose, no commentary.
```

### Agentic Mode

Full tool access. LLM can read/write branches, search history, run consistency checks, manage entity branches. Suitable for editing passes, restructuring, maintenance tasks, and anything that needs to operate across the full story state rather than just continuing the current scene.

### Discussion Mode

Planning and analysis only. Read-only access. Nothing written to story or branches. Natural switching point when thinking through what should happen next before writing it.

### Mode Switching

`<Tab>` cycles through modes. `/mode prose`, `/mode agent`, `/mode discuss`.

---

## Character Interaction

### Talking to Characters

Character mode is an input mode, not a separate system. Whatever you type gets the character's response based on their branch; the response is shown in the interface but not added to the prose.

```
/as alice-chen       # Enter character mode — input directed at Alice, responses from her branch
/as writing-coach    # Writing Coach has full story access (meta character)
/as                  # Return to narrator/director
```

Characters are oracles, not authors. They answer questions — what would they say, do, notice, feel — and the narrator decides what to do with the answer. A character response might become a line of dialogue, inform how the narrator writes the scene, or be discarded entirely. The character doesn't add to the story; the narrator does, armed with the character's answer.

### Correction Loop

When generated output is wrong, there are three distinct cases that route differently:

**Character was wrong** — the prose misrepresents who the character is or what they know.
- Add an entry to their branch explaining why they wouldn't do that, at the relevant fiction-time position
- Regenerate — the constraint is now permanent
- Prefix: `!` or explicit instruction ("fix this in Tony's branch and regenerate")

**Narrator misunderstood** — the prompt was interpreted incorrectly, the wrong thing happened.
- Edit or replace the instruction, regenerate
- Nothing goes to any branch; this was a prompt failure, not a character failure

**Bad roll** — the prompt was right, the character is right, this particular output is just flat or off.
- Swipe: regenerate with the same prompt, pick the better result
- Arrow-up to edit the prompt slightly, or shift-arrow-up to give refinement instructions

The distinction matters: routing a bad roll into a branch entry adds noise; routing a character failure to a swipe means it happens again next time.

---

## Querying

**Exact (git):**
```bash
git log alice-knowledge                    # What does Alice know?
git log main --not alice-knowledge         # What doesn't she know?
git log alice-knowledge bob-knowledge      # Shared experiences
```

**Semantic (RAG):**
```bash
storyteller search "when did Alice feel betrayed?"
storyteller search "Bob's guilt"
```

RAG index is a cache rebuilt from git at any time.

---

## Consistency Checking

**What to check:**
- Does this character reference knowledge not in their branch at this fiction-time position?
- Timeline contradictions?
- Voice consistency?
- Lore adherence?

**When:** After scene/chapter completion (optional), before merge (recommended), on demand.

**How:** Consistency Checker Agent queries entity branches at the relevant fiction-time position. Flags violations with references. User reviews — stories can break rules intentionally.

Not computationally solvable. System assists, user decides.

---

## Task Tracking

Tasks auto-injected into every LLM prompt as reminders.

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

### Interface: TUI
Primary interface similar to Claude Code. Text-focused, mode switching, external editor integration (`$EDITOR`), file watching. Works with existing tools (vim, emacs, git).

### Import/Export
- **Import:** Blank project (templates), existing manuscript (parse and commit), character cards
- **Export:** `cat chapters/*/*.md > story.md`, clean manuscript, EPUB/PDF via pandoc, screenplay format

### External Editor Integration

File changes detected automatically:
```
Detected changes in chapters/03-confrontation/chapter.md
Diff: [shown]
Commit summary? > Sharpen Alice's dialogue
✓ Committed
```

---

## Open Questions

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
- Web UI (shares core library with TUI)
