# Storyteller: Design Document

## Overview

Storyteller is an LLM-powered story development system that treats narrative as **queryable, versioned state**. It combines version control (git), hierarchical summarization, and character knowledge tracking to enable sophisticated story development workflows.

## Core Principles

1. **Every narrative atom is a commit** - paragraphs, scenes, chapters all tracked in git
2. **Natural language everywhere** - LLM-generated summaries, no rigid schemas
3. **Hierarchical navigation** - zoom from chapter summaries down to prose via merge commits
4. **Character knowledge as branches** - filtered timelines tracking what each character knows
5. **Non-destructive experimentation** - branch, rewrite, merge with full history
6. **Minimal prescription** - system adapts to your organization, no rigid structure required

## Design Philosophy

**Simple core, emergent complexity:**
- Only `.storyteller/` has prescribed meaning (system cache/config)
- Character presence implicit from git branches (no separate tracking)
- File organization flexible (system scans and adapts)
- Commit format flexible (minimal = summary line, metadata optional)
- Character format flexible (inferred from filename/heading)
- Everything derived from git state (no external databases)

**Git is the source of truth.** Everything else is cache or inference.

## Architecture

### Storage: Pure Git

Everything lives in a standard git repository:
- **Prose**: Markdown files (typically one per chapter)
- **Metadata**: Natural language summaries in commit messages
- **Characters/World**: Markdown files for definitions
- **Structure**: Hierarchical via commit scope paths and merge commits
- **Queries**: git log + grep for exact lookups, semantic search (RAG) for fuzzy queries

No external databases required - git is the source of truth. Optional caches/indexes can be rebuilt from git history at any time.

### Commit Structure

#### Commit Message Format

**Minimal (required):**
```
One-line summary of what happened

Optional longer description if needed.
```

**Enhanced (optional metadata):**
```
[Scope:Path] One-line summary

Multi-paragraph natural language description of what happened in
this narrative atom. LLM-generated, human-readable.

Characters: Alice Chen, Bob Martinez
Location: Abandoned warehouse
Time: Day 2, afternoon
POV: Alice Chen
```

**Key aspects:**
- **Good summary line**: Standard git practice (what changed)
- **Scope path** (optional): Hierarchical location for navigation (e.g., `story/act2/confrontation/warehouse/p4`)
- **Metadata** (optional): Characters, location, time, POV - helps with queries but not required
- **Character presence is implicit**: If commit is in a character's branch, they were present

**System adapts to whatever format you use.** More metadata = better queries, but minimal commits work fine.

#### Scope Paths: User-Defined Hierarchy

Not hardcoded levels - users define structure:

```
Short story:    story/scene/paragraph
Novel:          story/act/chapter/scene/paragraph
Epic series:    universe/series/book/part/chapter/pov/scene/paragraph
```

System parses depth, doesn't care about level names.

### Hierarchical Summaries via Merge Commits

```
Paragraph commits (leaf nodes):
  C1: Alice accuses Bob
  C2: Bob deflects
  C3: Alice shows evidence
  C4: Bob admits partial truth

Scene merge commit:
  M1: Merge(C1,C2,C3,C4)
  Summary: "Confrontation - Alice corners Bob, he admits involvement"

Chapter merge commit:
  M2: Merge(M1, other_scenes)
  Summary: "The Confrontation - Alice and Bob's relationship fractures"
```

**Merge commits = navigational nodes** for viewing story at different zoom levels.

### Character Knowledge Branches

Each character has a filtered branch tracking their perspective:

```
main branch (omniscient):
  C1: Alice discovers theft
  C2: Bob secretly contacts employer (Alice not present)
  C3: Alice and Bob meet, Bob lies
  C4: Alice confronts Bob

alice-knowledge branch (cherry-picked + filtered):
  C1': Alice discovers theft (she was there)
  C3': Alice and Bob meet - Bob claims he was home
       (filtered: Alice doesn't know it's a lie yet)
  C4': Confrontation - Bob admits he lied
       (now she knows C3 was a lie)
```

**Knowledge Filter Agent** cherry-picks commits where character is present, removes information they couldn't know (internal thoughts, off-screen events).

#### Commit Structure Encodes Knowledge State

**Git commit structure itself provides semantic meaning:**

**Pattern 1: Cherry-picked (same content)**
```
main:             abc123 "Bob hesitates at door"
alice-knowledge:  abc123 (identical content)
```
**Meaning:** Alice observed this objectively, no perceptual difference.

**Pattern 2: Cherry-picked (modified content)**
```
main:             abc123 "Bob hesitates at door"
alice-knowledge:  abc123' "Bob hesitates. Unusual for him—he's nervous."
bob-knowledge:    abc123'' "I hesitate. Should I go in? What if she knows?"
```
**Meaning:** Same event, different perspectives. Content diff shows perceptual/interpretive differences.

**Pattern 3: New commit (only in character branch)**
```
bob-knowledge:
  abc123'' (cherry-picked from main, Bob's POV)
  def456 (NEW - only in bob-knowledge)
  "I'm terrified. If I tell her the truth, they'll kill her.
   Better she hates me than dies trying to save me."
```
**Meaning:** Character's private thoughts, internal elaboration not visible in main narrative or to other characters.

**Pattern 4: Absent from branch**
```
main:             xyz789 "Bob meets employer in secret"
alice-knowledge:  [commit not present]
```
**Meaning:** Alice wasn't present, doesn't know this happened.

**Semantic queries from structure:**
```bash
# What does Alice know?
git log alice-knowledge

# What doesn't Alice know?
git log main --not alice-knowledge

# Alice's private thoughts (not in main)
git log alice-knowledge --not main

# How did Alice perceive this differently?
git diff main:abc123 alice-knowledge:abc123'

# Events both witnessed
git log alice-knowledge bob-knowledge
```

**The git commit graph IS the knowledge state** - no separate metadata needed.

### Interaction Queries (Derived from Branches)

Character interactions are derived by intersecting character branches - no separate tracking needed.

**Alice and Bob's interactions:**
```bash
# Find commits in both branches
git log alice-knowledge bob-knowledge

Results:
  - Café meeting (both present, both remember)
  - Phone call (both participated)
  - Warehouse confrontation (both present)
```

**Group interactions:**
```bash
# Three-way (or more) intersection
git log alice-knowledge bob-knowledge charlie-knowledge

Results:
  - Team meeting (all three present)
  - Final confrontation (all three present)
```

**Who was in a scene:**
Check which character branches include that commit.

**Character branches are the source of truth** for presence, knowledge, and relationships. Everything else is derived.

### Timeline Flexibility

**Story-told order** (main branch) vs **chronological order** (metadata/alternate branch):

```
Main branch (reading order):
  [Ch1] Present: The theft
  [Ch2] Flashback: Bob's recruitment (6 months ago)
  [Ch3] Present: Investigation
  [Ch4] Flashback: Alice's mentor death (3 years ago)

Chronological branch:
  [Ch4] 3 years ago: Mentor dies
  [Ch2] 6 months ago: Bob recruited
  [Ch1] Present: Theft discovered
  [Ch3] Present: Investigation
```

System tracks both. Character knowledge follows story-told order (what they know when reader learns it).

## File Organization

**Philosophy:** Minimal prescribed structure. System adapts to your organization.

### What's Fixed

**`.storyteller/` - System files only**
```
.storyteller/
  config.yaml      # Optional configuration
  cache/           # Derived data (not in git)
    embeddings/    # RAG index (rebuilt from git)
    character-index.json  # Character discovery cache
```

**Git branches - Character knowledge**
```
main                    # Main story timeline
alice-knowledge         # Alice's filtered timeline (auto-maintained)
bob-knowledge          # Bob's filtered timeline (auto-maintained)
explore/*              # User's exploration branches
meta/*                 # User's meta work branches
```

### Everything Else: Your Choice

**Example organization (not required):**
```
/storyteller-project
  .git/
  .storyteller/          # Only prescribed location

  chapters/              # Or: story/, manuscript/, scenes/, whatever
    01-theft.md
    02-investigation.md

  characters/            # Or: cast/, people/, inline with chapters
    alice-chen.md        # Canonical name from filename
    detective.md -> alice-chen.md    # Alias via symlink
    chen.md -> alice-chen.md
    bob-martinez.md

  world/                 # Optional: organize as you like
    warehouse.md
    magic-rules.md

  meta/                  # Optional: planning documents
    outline.md
    tasks/
      story.md
```

**System finds content by:**
- Scanning markdown files for character headers
- Following symlinks for aliases
- Parsing git branches for timelines
- Indexing all content for semantic search

**Location doesn't matter.**

### Global Configuration (across all stories)

```
~/.storyteller/
  characters/                    # Meta characters available to all stories
    writing-coach/
      character.md
    harsh-critic/
      character.md
    character-therapist/
      character.md
    [user's custom meta characters...]

  config.yaml                    # Global settings
  templates/                     # Story structure templates
    novel-3act.yaml
    short-story.yaml
    visual-novel.yaml
```

**Character discovery:** System checks story `characters/` first, falls back to global `~/.storyteller/characters/`

### Characters: Unified Concept

**Characters are universal entities** - some exist within stories, others exist as meta-helpers available across all stories.

**Distinction is storage location:**
- **Story characters:** Anywhere in story repo (Alice, Bob, NPCs)
- **Meta characters:** `~/.storyteller/characters/` in global config (Writing Coach, Harsh Critic)

**No prescribed format** - system infers from content and location.

### Character Definition

**Minimal (sufficient):**
```markdown
# Alice Chen

Detective with trust issues. Dry humor, guarded personality.
```

**Canonical name:** Derived from filename (`alice-chen.md` → "Alice Chen") or first heading

**Detailed (optional):**
```markdown
# Alice "The Detective" Chen

## Core Identity

Resourceful detective with trust issues stemming from childhood
abandonment. Professional and guarded in public, shows dry humor
with close friends, privately driven by fear of betrayal.

## Public Persona

Professional, competent, guarded. Presents confidence without
arrogance. Rarely shows weakness.

## Intimate Persona

With trusted friends, sardonic wit emerges. Struggles to be
vulnerable but fiercely loyal once trust is established.

## Private Self

Driven by childhood abandonment (mother left at age 8). Core fear:
being betrayed by those she trusts. Detective work is both calling
and coping mechanism.

## Voice Pattern

**Summary:** Clipped, direct, occasional sharp sarcasm

**Examples:**

Dialogue:
- "Try again. This time without lying."
- "I've heard prettier lies from worse liars."

Internal monologue:
- "She'd categorized his tells years ago. The slight pause,
  the too-casual shrug. Lies, stacked neatly."

When stressed:
- Short sentences. Fragments.
- "Don't. Just don't."
```

**Aliases via symlinks:**
```bash
# In same directory as alice-chen.md
detective.md -> alice-chen.md
chen.md -> alice-chen.md
alice.md -> alice-chen.md
```

Now `/chat detective`, `/as chen`, etc. all resolve to Alice Chen.

### Meta Character Example

```markdown
# Writing Coach

## Core Identity

Supportive writing mentor focused on craft improvement and
encouragement. Asks questions to help writer discover solutions
rather than prescribing answers.

## Voice Pattern

Warm but professional. Uses "I notice..." and "What if..."
framing. Avoids prescriptive "you should."

**Examples:**
- "I notice the pacing slows in this section. What effect were you going for?"
- "This dialogue feels authentic. What made it click for you?"

## Expertise Areas

- Story structure and pacing
- Character development
- Dialogue crafting
- Avoiding common pitfalls

## Constraints

- Never rewrites prose directly
- Doesn't make decisions for the writer
- Asks clarifying questions first
```

### Character Knowledge

**Story characters:**
- Have their own `<name>-knowledge` branch (auto-maintained by system)
- See only commits in their branch (filtered timeline)
- Don't know events they didn't witness
- Can't read other characters' thoughts

**Meta characters:**
- No knowledge branch (they're not in the story)
- Access full main branch when consulted
- Can analyze structure, all characters, complete timeline
- Exist to help the author, not part of narrative

**Natural boundaries:**
- Talking to Alice → filtered to her knowledge and perspective
- Talking to Writing Coach → full omniscient story access
- Character presence implicit from branch membership
- No explicit access level field needed (derived from context)

### Character Internal Life

**Character branches can contain more than filtered views** - they can include private thoughts and internal elaborations not present in main narrative.

**Use cases:**

**1. Author exploration (understand character deeply):**
```bash
[Agent] > /branch bob-knowledge
[Agent] > /as bob

You: Why am I pushing Alice away?

Bob (internal): If I tell her about the organization, they'll
kill her. Better she hates me than dies trying to save me.

✓ Committed only to bob-knowledge
✗ Not in main (reader doesn't see this yet)
✗ Not in alice-knowledge (she can't read his mind)
```

**2. Agent-generated context (automatic depth):**

When generating scenes, agents can automatically add internal monologue to character branches to provide context for behavior without spelling it out in prose.

```
main:             Bob leaves abruptly
bob-knowledge:    Bob leaves abruptly (his POV)
                  [Internal] "I'm a coward. I should tell her." (agent-added)
```

**3. Foreshadowing and dramatic irony:**

Author knows character motivations (in their private branch) before revealing to reader, enabling planned dramatic irony.

```
Ch3: Bob seems cold (reader confused)
Ch3 bob-knowledge: Bob's internal fear (author knows why)
Ch8: Revelation - "I was protecting you" (reader: "Oh!")
```

**4. Character therapy/analysis:**

Meta characters can analyze character private thoughts to understand psychology and suggest development.

```bash
[Agent] > /chat character-therapist --about bob

Therapist: [Loads bob-knowledge including internal commits]
Looking at Bob's private thoughts, he's operating from trauma...
```

**5. TTRPG/Interactive mode:**

Player can add private thoughts their character doesn't voice, affecting behavior without revealing plans to NPCs.

```bash
[Agent:Alice] > Internal: I don't trust Bob anymore

[Other characters don't know Alice is suspicious]
[Informs her behavior in future scenes]
```

**Queries:**
```bash
# Bob's internal world (commits only in his branch)
git log bob-knowledge --not main

# Compare what Alice thinks vs. what Bob thinks
git diff alice-knowledge:ch3sc5 bob-knowledge:ch3sc5

# Has Bob been keeping secrets?
storyteller show bob-knowledge --internal-only
```

**Character branches are:**
1. Filtered timeline (what they witnessed)
2. Psychological depth (what they thought)
3. Author planning tool (motivations to reveal later)
4. Character development workspace

## Agent Architecture

### Agent Design Philosophy

**Code-based agents** for maximum flexibility, written in Haskell. Agents define their own:
- Context assembly (what data they need, at what granularity)
- Prompts (including system prompts)
- When/how they run
- What they commit

Users can use pre-built agents or define their own.

### Two-Layer Agent Architecture

The system separates **interaction agents** (how the program behaves) from **writing agents** (how content is generated):

#### Interaction Agents (Main Agent Layer)

Define the overall user experience and workflow:

**Writer's Assistant Agent**
- Behavior: Collaborative, asks clarifying questions, suggests improvements
- Tools: Full suite (write, edit, research, consistency checks)
- Use case: Traditional novel writing with AI assistance

**Roleplaying Engine Agent**
- Behavior: Simulates world and NPCs, user is voice-of-god or character inner thoughts
- Tools: Character simulation, dice rolling, scene generation
- Use case: Interactive TTRPG-style story development

**Dungeon Master Agent**
- Behavior: Runs game sessions, maintains world state, adjudicates rules
- Tools: NPC responses, world simulation, consequence tracking
- Use case: Solo or group TTRPG sessions

**Editor's Toolkit Agent**
- Behavior: Minimal generation, focuses on analysis and refinement
- Tools: Consistency checking, voice analysis, pacing tools, structural suggestions
- Use case: Working with existing manuscript

**Business Writing Agent**
- Behavior: Professional, focused on clarity and effectiveness
- Tools: Template generation, tone adjustment, formatting
- Use case: Emails, reports, documentation

Users swap interaction agents to change how the program behaves: `/mode roleplaying`, `/mode assistant`, `/mode editor`

#### Writing Agents (Content Generation Layer)

Define how prose/content is actually generated. **Swappable within any interaction mode:**

**Lightweight Editor Writer**
- Process: Takes user's exact words, fixes spelling/grammar/formatting only
- Output: Minimally modified text preserving author voice completely
- Use case: Author wants full control, just needs cleanup

**Collaborative Scene Writer**
- Process: Generates complete scenes from briefs, shows for approval/revision
- Output: Full prose with character voice, descriptions, pacing
- Use case: Standard assisted novel writing

**Roleplayer Writer**
- Process: Simulates characters and world, responds to user actions
- Output: Character responses, world reactions, consequence descriptions
- Use case: Interactive story development, TTRPG mode

**Swipe Generator Writer**
- Process: Generates multiple complete alternatives for each section
- Output: 3-5 variations with different approaches (emotional/action/mystery focus)
- Use case: Exploration mode, finding best approach

**Business Correspondence Writer**
- Process: Professional tone, clear structure, appropriate formality
- Output: Emails, memos, reports in business style
- Use case: Non-fiction, professional writing

**Ad Copy Writer**
- Process: Punchy, persuasive, attention-grabbing
- Output: Marketing copy, headlines, product descriptions
- Use case: Commercial writing

**Essay Writer**
- Process: Structured arguments, evidence-based, academic tone
- Output: Essays, articles, analysis pieces
- Use case: Non-fiction, analytical writing

**Technical Documentation Writer**
- Process: Clear, precise, structured for reference
- Output: How-tos, API docs, technical guides
- Use case: Technical writing

Users swap writing agents to change output style: `/writer lightweight`, `/writer collaborative`, `/writer roleplayer`

**Key insight:** These are orthogonal dimensions:
- **Interaction agent** = workflow and UX
- **Writing agent** = output style and generation approach

You can combine any interaction agent with any writing agent:
- Writer's Assistant + Lightweight Editor = Cleanup-only assistant
- Writer's Assistant + Collaborative Scene = Traditional AI co-writing
- Roleplaying Engine + Roleplayer Writer = Interactive story game
- Editor's Toolkit + Swipe Generator = Revision exploration tool
- Business Writing Agent + Business Correspondence Writer = Professional communication tool

**Flexibility beyond layers:**

The "two-layer" structure is organizational, not a hard boundary. Writing agents can be invoked contextually within other workflows:

```bash
[Agent:Novel] > Generate email that Alice sends to Bob

# Could invoke Business Correspondence Writer for that section
# Or just let novel writer handle it (LLMs can write business emails in context)
```

Whether specialized writing agents are needed depends on:
- **Context size:** Novel writer already has character voice, may handle it naturally
- **Quality difference:** Does specialized agent produce meaningfully better output?
- **User preference:** Some want explicit control, others prefer seamless flow

**In practice:** Most content will use the default writing agent for the interaction mode. Specialized writers are for when you want explicit style switching or the quality difference matters.

**Example use case:** Writing a novel that includes:
- Narrative prose (collaborative scene writer)
- Character text messages (could invoke ad copy writer for punchy, modern style)
- Legal documents within story (could invoke technical documentation writer for authentic legalese)
- Character emails (novel writer likely handles fine, but could invoke business writer)

System should support agent switching mid-flow, but **keep it simple by default** - specialized writers are optimization, not requirement.

**Implementation:** Writing agents are primarily prompt variations, but can include:
- Different system prompts (voice, style, constraints)
- Different context assembly (what information to load)
- Different output formats (prose vs dialogue vs bullet points)
- Different post-processing (formatting, structure)

Writing agents are **pluggable modules** - users can define custom writers for specialized output styles (poetry, screenplays, legal documents, etc.).

### Utility/Orchestrator Agent Types

Beyond interaction and writing agents, the system includes specialized utility agents for specific tasks:

**Scene Writer Agent** (utility)
- Input: Scene brief, character states, context
- Process: Generate prose, split into paragraphs, create summaries
- Output: Paragraph commits + scene merge commit

**Consistency Checker Agent**
- Input: Commit range to check
- Process: Verify character knowledge, timeline, lore adherence
- Output: Report of inconsistencies (no commits)

**Knowledge Filter Agent**
- Input: Main branch commit, target character
- Process: Filter for character's perspective
- Output: Filtered commit to character-knowledge branch

**Summarizer Agent**
- Input: Set of commits (paragraphs, scenes, chapters)
- Process: Generate hierarchical summary
- Output: Merge commit with summary

**Merge Agent** (orchestrator utility)
- Input: Two divergent branches
- Process: Launch other agents (consistency check, regenerate scenes, update summaries)
- Output: Merged timeline with reconciled changes

**Synthesis Merge Agent**
- Input: Multiple branches, synthesis instructions
- Process: Compare branches, extract best elements per instructions, generate new version
- Output: Synthesized commit combining strengths from multiple approaches

**Character Development Agent**
- Input: Character archetype, scenario
- Process: Generate exploratory scene, update character card
- Output: Commits to character exploration branch

**Task Tracking Agent**
- Input: Story state, character goals
- Process: Maintain task lists (story-level, per-character), track completion, suggest next steps
- Output: Task markdown files, auto-injected into prompts

### Context Assembly Strategy

Agents fetch context at appropriate granularity:

```haskell
-- Scene Writer needs:
characterArchetype     -- Full detail
characterState         -- Current emotional/knowledge state
previousSceneSummary   -- Just summary, not full prose
interactionHistory     -- git log alice-knowledge bob-knowledge (branch intersection)
worldRules             -- Relevant lore constraints

-- Orchestrator needs:
chapterSummaries       -- High-level only
characterArcs          -- Emotional progression
unresolvedPlotThreads  -- From semantic search
```

Agents can:
- Query git directly (`git log --grep`)
- Use semantic search (RAG) for concepts
- Launch sub-agents to extract information
- Receive context from orchestrating agent

### Prompt Strategy

Each agent manages its own prompts. Common patterns:

**System prompt includes:**
- Role definition
- Output format requirements
- Canonical naming conventions
- Content policies (from config)

**Context prompt includes:**
- Relevant character cards (voice, personality)
- Scene structure/beats
- Recent context (summaries, not full prose)
- World rules/constraints

**Generation prompt:**
- Specific task
- User instructions/corrections

Prompts stored as code (Haskell), can reference shared templates/configs.

## Generation Workflow

### Scene Generation Flow

1. **Structure Phase**
   - Orchestrator/user defines scene: beats, participants, tone
   - Present to user for approval

2. **Prose Generation**
   - Scene Writer generates full scene (multiple paragraphs)
   - Shows to user, allow: approve/edit/regenerate (swipes)

3. **Commit Phase**
   - Split prose into logical chunks (passages/paragraphs)
   - Create commit for each chunk
   - LLM generates summary from context (already loaded)

4. **Summary Phase**
   - LLM summarizes full scene
   - Create scene merge commit

5. **Character Branch Updates**
   - Knowledge Filter processes commits
   - Cherry-picks to character branches
   - Filters for each character's perspective

### Swipes (Alternative Generations)

```
User: "Regenerate"

System generates alternative:
  C1 (first attempt) - dangling
  C2 (second attempt) - dangling
  C3 (third attempt) - chosen → HEAD

User can:
- View all swipes
- Choose which becomes canonical
- Branch from any swipe later
```

Swipes are dangling commits, eventually garbage collected or optionally merged as parents (preserving history).

### Draft/Commit Strategy

**Commits are cheap** - create them freely during generation:
- Partial scene = commits for completed paragraphs
- User can stop, resume later
- Rewrites create new commits
- History rewriting (rebase) cleans up if desired

User focuses on story, not on "when to commit" - system handles it.

## Querying

### Two-Tier Search

**Exact queries (git + grep):**
```bash
# Find commit by scope
git log --all --grep='\[Story:Ch3:Sc2\]'

# All commits with character
git log --all --grep='Characters:.*Alice Chen'

# Timeline at location
git log --all --grep='Location:.*warehouse'
```

**Semantic queries (RAG):**
```bash
# Fuzzy concept search
storyteller search "when did Alice feel betrayed?"
storyteller search "trust breakdown between characters"
storyteller search "Bob's guilt"
```

RAG index (embeddings) is cached/derived - rebuild from git anytime.

### Character State Queries

```bash
# What does Alice know at Chapter 5?
storyteller knowledge alice --at=Ch5

# Show Alice's emotional arc
git log alice-knowledge --oneline
# Or with LLM summary: storyteller log alice-knowledge --format=emotional-arc

# Interaction history between Alice and Bob
git log alice-knowledge bob-knowledge --oneline
# Shows commits in both branches (scenes where both were present)

# Group interactions (Alice, Bob, Charlie)
git log alice-knowledge bob-knowledge charlie-knowledge --oneline

# Who was in this scene?
storyteller who-was-in ch3:sc5
# Checks which character branches include this commit
```

**Implementation:**
- Character branches contain their filtered timeline
- Git branch intersection finds interactions (no separate tracking)
- LLM queries over branch commits for "what does X know/feel?"
- Character presence implicit from branch membership

Natural language in, natural language out.

## Consistency Checking

**When to check:**
- After scene/chapter completion (optional)
- Before merge (recommended)
- On demand

**What to check:**
- **Character knowledge**: Does character know something they shouldn't?
- **Timeline**: Chronological contradictions?
- **Voice**: Character speaking consistently?
- **Lore**: Actions violating world rules?
- **Relationships**: Dynamics matching history?

**How it works:**
- Consistency Checker Agent queries relevant branches
- Compares current commit against history
- Flags violations with references
- User reviews, can override (stories can break rules intentionally)

**Not computationally solvable** - system assists, user decides.

## Merging

### Narrative Merge Challenges

Unlike code, story merges involve **narrative conflicts**:

```
Branch A: Alice learns secret in Ch3
Main:     Alice learns secret in Ch5

Merge conflict: All scenes Ch4-5 assume she doesn't know
```

### Merge Agent Workflow

1. **Analyze divergence** (git diff + semantic analysis)
2. **Identify downstream impact** (what commits are affected?)
3. **Launch Consistency Checker** on affected range
4. **Generate reconciliation plan**:
   - Regenerate scenes
   - Adjust character knowledge branches
   - Update summaries
5. **Present plan to user** for approval
6. **Execute**: Launch Scene Writer, Knowledge Filter, Summarizer as needed
7. **Final consistency check**
8. **User review and approve**

**Merge Agent is an orchestrator** - delegates to other agents for actual work.

### Multi-Branch Synthesis

Instead of choosing one branch, the system can synthesize the best elements from multiple branches:

```
[Agent] > I have three versions of Ch5, synthesize the best parts

Branch A: Character-focused, deep emotional work
Branch B: Action-heavy, fast pacing
Branch C: Mystery-focused, clear plot progression

Instructions:
> Take dialogue from A, pacing from B, plot clarity from C

Synthesis Agent:
- Loads all three branches
- Identifies strengths per instruction
- Generates new version combining elements
- Creates synthesis commit

Result: New version better than any individual branch
```

**Works at any scale:**
- **Small (scene-level):** Full branches fit in context (128k tokens)
- **Large (chapter-level):** Dynamic querying - fetch specific sections as needed
- **Massive (book-level):** Hierarchical synthesis - summary-level plan, scene-level execution

**Use cases:**
- Try multiple approaches to a scene (emotional/action/mystery)
- Experiment with character voices, synthesize best elements
- Different pacing strategies, combine optimal rhythm
- Branch story directions, merge compatible elements

**Key insight:** Branches become "large swipes" - experiment freely at chapter/arc scale, then combine the best parts.

## Task/Objective Tracking

**Inspired by Claude's TODO tool** - tasks are auto-injected into LLM prompts as reminders.

### Task Types

**Story-level tasks** (writer's meta-objectives):
```markdown
# meta/tasks/story.md
- [ ] Reveal employer identity by end of Act 2
- [ ] Resolve Alice/Bob relationship arc
- [ ] Follow up on second artifact (mentioned Ch3)
```

**Character-level tasks** (internal motivations):
```markdown
# meta/tasks/alice-chen.md
- [ ] Find out who Bob is working for
- [ ] Decide if she can trust him again
- [ ] Resolve mentor's unsolved case (background thread)

# meta/tasks/bob-martinez.md
- [ ] Protect employer's identity (conflicts with Alice!)
- [ ] Earn Alice's trust back
- [ ] Figure out exit strategy
```

### How It Works

Tasks are **automatically injected into every LLM prompt**:

```
System prompt for Scene Writer:

Current Tasks:
Story:
- [ ] Employer reveal (due: end Act 2)
- [ ] Second artifact thread needs pickup

Alice Chen:
- [ ] Find employer identity
- [ ] Decide about trusting Bob

Bob Martinez:
- [ ] Protect employer secret (conflicts with Alice!)
- [ ] Earn trust back

If you complete a task during generation, mark it done.
If you create new plot threads, add tasks for them.
```

**LLM can:**
- Complete tasks: `<tool_use name="complete_task"><task>employer reveal</task></tool_use>`
- Add tasks: `<tool_use name="add_task"><scope>Alice</scope><task>Investigate second artifact</task></tool_use>`
- Query tasks: See what's pending, suggest scenes to advance them

**Benefits:**
- Prevents forgotten plot threads
- Keeps character motivations consistent
- Tracks story arc progress
- LLM self-manages story structure

**Storage:** Just markdown checkbox files in `/meta/tasks/`

## Use Cases

### Novel Writing

**Workflow:**
- Define structure, create characters
- Generate scenes with LLM assistance
- View at multiple zoom levels (summaries)
- Branch to try different story directions
- Consistency checking prevents plot holes
- Export to clean manuscript

**Advantages over existing tools:**
- Character knowledge tracking (no omniscience leaks)
- Hierarchical navigation (100k words → 10 chapter summaries)
- Non-destructive experimentation (branch freely)
- Version control (track all changes)

### TTRPG/Live Roleplay

**Workflow:**
- User plays character(s)
- System plays NPCs/world using character knowledge branches
- All interactions tracked in relationship journals
- Convert session log to polished story afterward

**Advantages:**
- NPCs only know what they've experienced (no cheating)
- Relationship dynamics persist across sessions
- Session history becomes story source material

### Collaborative Writing (Async)

**Workflow:**
- Multiple authors work on branches
- Characters have "owners" defining their voice
- Merge with system-assisted conflict resolution
- Standard git workflow (push/pull, not real-time)

**Advantages:**
- Proper version control (like code)
- Consistency checking across authors
- Shared character/world database

**Note:** Live multi-user collaboration (real-time editing) is a different tool with different requirements. This system focuses on single-user and asynchronous collaboration via git.

### Visual Novel Authoring

The system naturally supports visual novel development without special features:

**Branching narratives:**
- Git branches for different routes/endings
- Merge branches when routes reconverge (diamond patterns)
- Track which choices led to current state in merge commits
- Complex route trees with multiple decision points

**Asset integration:**
- Character portraits: `<tool_use name="generate_character_portrait">`
- Scene backgrounds: `<tool_use name="generate_scene_background">`
- Expression variants: Generate for emotional beats
- Assets stored in repo, referenced in markdown

**Multi-route management:**
```
          ┌─ romance-ending
          │
main ─────┼─ friendship-ending
          │
          └─ tragedy-ending

Or complex:
            ┌─ trust ─┬─ romance
            │         └─ friend
main ─ ch3 ─┤
            └─ doubt ─┬─ redeem
                      └─ betray
```

**Unique capabilities for VN:**
- Edit early choice points, see downstream impact across ALL routes
- Consistency checking per route (character knowledge tracking)
- Synthesize routes (combine best elements from experimental branches)
- No other tool handles VN branching + consistency this well

**Export:**
- Parse markdown dialogue and assets
- Generate scripts for VN engines (Ren'Py, Twine, Ink)
- External tools handle export (simple post-processing)

**Note:** Storyteller is an authoring tool, not a game engine. It creates the content; VN engines present it to players.

## Killer Features

1. **Hierarchical Navigation**
   - View 100k word novel as 10 chapter summaries
   - Drill down to any level (chapter → scene → paragraph)
   - Works for characters, timelines, relationships

2. **Character Knowledge as Queryable State**
   - "What does Alice know at this point?" is a real query
   - Character branches track filtered timelines automatically
   - Prevents plot holes (character acting on unknown information)
   - Git commit structure encodes knowledge state (no separate tracking)
   - Same event, different perspectives (content differs per branch)
   - Character internal thoughts separate from main narrative

3. **Non-Destructive Experimentation**
   - Branch to try different story directions
   - Merge back if you like it, discard if not
   - Full history preserved

4. **Consistency Enforcement**
   - System helps maintain character voices
   - Catches knowledge leaks
   - Tracks relationship dynamics
   - Verifies lore adherence

5. **Multi-Mode**
   - Same system for novel writing, roleplay, collaboration
   - Convert between modes (roleplay → story)

6. **LLM-Native Design**
   - Built around LLM strengths (summarization, voice matching)
   - Natural language throughout (no rigid schemas)
   - Semantic search for fuzzy queries

7. **Multi-Branch Synthesis**
   - Experiment with multiple approaches (emotional/action/mystery)
   - Synthesize best elements from all versions
   - Risk-free experimentation at any scale (scene to arc)
   - Unique capability not available in other tools

8. **Task Tracking**
   - Auto-injected into prompts (like Claude's TODO tool)
   - Story-level and per-character objectives
   - LLM self-manages plot threads
   - Prevents forgotten story elements

9. **Character Internal Life**
   - Private thoughts/motivations in character branches (not in main)
   - Author planning tool (know motivations before revealing)
   - Dramatic irony and foreshadowing support
   - Character psychology depth without over-explaining in prose
   - Agents can auto-generate internal context

10. **Minimal Prescription, Maximum Flexibility**
   - Only `.storyteller/` has fixed meaning
   - No required file structure or commit format
   - System adapts to your organization
   - Character discovery via scanning (not prescribed locations)
   - Canonical names from filenames, aliases via symlinks
   - Everything derived from git state

11. **Two-Layer Agent Architecture**
   - **Interaction agents** define workflow (assistant, roleplaying engine, editor toolkit)
   - **Writing agents** define output style (lightweight editor, collaborative writer, business correspondence)
   - Orthogonal and composable: any interaction mode + any writing style
   - Single system handles novel writing, TTRPG, business documents, essays, ad copy
   - Swap agents to change behavior without changing underlying story state
   - Users can create custom agents for specialized workflows

## Core Insight

**Stories have complex state** (character knowledge, relationships, world facts) that changes over time in specific ways. Current tools treat this as unstructured text.

**Storyteller makes story state queryable and verifiable**, while keeping everything in natural language that LLMs and humans can work with.

**Key innovations:**
1. **Git commit graph encodes knowledge state** - character presence, perceptions, and internal thoughts are implicit in branch structure
2. **Character branches are richer than main** - can contain private thoughts and motivations not yet revealed
3. **Minimal prescription** - system infers structure, no rigid schemas required
4. **Everything derived from git** - no external databases, caches can be rebuilt

The hierarchical nature of summaries + character branches with filtered timelines enables context assembly without overwhelming LLMs or users. Git provides tracking, querying, and reversibility as natural consequences of the design.

## Technical Constraints

### Performance/Scalability

**Not a problem:**
- Git handles massive repos (Linux kernel = millions of commits)
- Text is tiny (500k word novel ≈ 1MB)
- Hierarchical summaries = logarithmic growth
- Git operations are fast (log/grep on thousands of commits = milliseconds)

**LLM generation time dominates** all other performance considerations.

### Context Size Limits

**When story exceeds LLM context:**
- Use hierarchical summaries (load chapter summaries, not all prose)
- RAG for specific details (semantic search targeted sections)
- Agents request appropriate granularity

This is why hierarchical structure is essential.

### Lore/Rule Enforcement

**Not computationally solvable** - LLMs can check, but:
- False positives (system thinks it's wrong when story intentionally breaks rules)
- False negatives (system misses actual violations)
- User has final say

System assists, doesn't enforce.

## Implementation Notes

### Language: Haskell

**Why Haskell:**
- Strong typing for correctness
- Good git libraries (libgit2 bindings)
- LLM agent framework already prototyped
- Excellent for DSLs (agent definitions)
- Parser combinators (commit message parsing)

### User Interface: TUI (Terminal UI)

**Primary interface is a TUI application** (similar to Claude Code) that provides:
- Text-focused workflow (no browser, no UI chrome)
- Direct prose generation via conversational interface
- Mode switching for different interaction styles
- External editor integration ($EDITOR)
- File watching (auto-detect external edits)

**Why TUI:**
- Writers already work in terminals/editors
- Zero context switching (no browser)
- Works with existing tools (vim, emacs, git)
- Simpler to implement than web UI
- Command-driven interface natural for text work
- Lightweight, focused, no distractions

### Character Conversations and Impersonation

The system supports conversing with characters and impersonating them during story development.

#### Talking to Characters

**Talk to story characters (filtered knowledge):**
```bash
[Agent] > /chat alice-chen

You: How did you feel when Bob lied to you?

Alice: [Responds based on alice-knowledge branch - only what she knows]
```

This is like being Alice's **inner voice** or **conscience** - you're speaking to her internal monologue, influencing her thoughts and decisions.

**Talk to meta characters (omniscient):**
```bash
[Agent] > /chat writing-coach

You: This confrontation scene feels flat

Writing Coach: [Has full story access, discusses craft and structure]
```

#### Impersonation: Input Context

Use `/as <character>` to change how your input is interpreted:

**In Agent mode:**
```bash
[Agent] > /as alice
[Agent:Alice] >

You: I push the door open. "Anyone here?"

[Interpreted as Alice's action/dialog]
[Other characters respond, scene develops]

You: (( Make Bob more nervous ))

[OOC instruction - always interpreted as director, regardless of context]

You: I don't buy his excuse

[Back to Alice's perspective]

[Agent:Alice] > /as
[Agent] >

[Back to director view]
```

**In Story mode:**
```bash
[Story] > /as alice
[Story:Alice] >

You: I discover the theft

[Generates prose from Alice's POV, in her voice]

I stand before the empty case. The cylinder should be here.
My hands shake—I force them still.

You: Continue

[Maintains Alice POV]

I reach for my phone. Bob needs to know. No—Bob can't know.
Not until I understand what happened.
```

#### POV and Input Context are Coupled

**Default behavior:**
- `/as alice` → Input interpreted as Alice + prose generated in Alice's POV
- Natural coupling: if you're "being" Alice, you see as Alice

**Override with instructions:**
- `"Write this in third person omniscient"` - LLM follows instruction
- `"Show Bob's thoughts here"` - Can override POV constraints
- Edit output if it doesn't match intent

**LLMs learn patterns from context** - after establishing POV/voice, they maintain it naturally.

#### Impersonating Other Characters in Conversations

```bash
[Agent] > /chat alice-chen --as bob-martinez

You (as Bob): I need to explain what really happened

Alice: [Responds to Bob based on their relationship history]

[This could become story content if you continue the scene]
```

**Meta conversations (unusual but allowed):**
```bash
[Agent] > /chat writing-coach --as alice-chen

Alice: Am I being too passive in this story?

Writing Coach: [Responds to Alice as if she's aware of being in a story - meta-recursive]
```

### Interaction Modes

The TUI operates in three modes, each optimized for different tasks:

#### Agent Mode (Default)

**Purpose:** Primary workspace for story development

**Capabilities:**
- Navigate structure (show summaries, drill down)
- Create characters/locations via tools
- Generate scenes using tool calls
- Check consistency
- Research past events (RAG queries)
- Manipulate history (edit, branch, merge)
- Query story state at any granularity

**LLM behavior:**
- Has access to full tool suite
- Uses `write_scene()`, `edit_scene()`, `create_character()` tools
- Can perform complex multi-step operations
- Explains actions and reasoning
- Output is structured (tool calls + explanations)

**System prompt:**
```
You are a story development agent with tools to write, edit,
and manage a novel in progress.

Available tools:
- write_scene(scope, prose)
- edit_scene(scope, instruction)
- create_character(name, description)
- check_consistency(scope)
- search_story(query)
- get_interactions(characters, level)
- get_character_state(character, scope, detail_level)
- summarize_character(character, aspects)
- get_story_arcs()
...

Fetch context at appropriate granularity. Generate scene-specific
character summaries rather than loading full cards. Track story
arcs and suggest what advances multiple threads.
```

**Example interaction:**
```
[Agent] > Generate confrontation scene between Alice and Bob

<tool_use name="get_interactions">
  <characters>Alice Chen, Bob Martinez</characters>
  <summary_level>scene</summary_level>
</tool_use>

<tool_use name="get_character_state">
  <character>Alice Chen</character>
  <scope>ch3/confrontation</scope>
  <detail_level>relevant_traits</detail_level>
</tool_use>

<tool_use name="write_scene">
  <scope>ch3/confrontation/warehouse</scope>
  <prose>
  Alice didn't slow her approach. "You're late."
  ...
  </prose>
</tool_use>

✓ Written scene using interaction history and current character states
```

#### Story Mode (Prose Flow)

**Purpose:** Flow state for linear prose generation - minimal friction, natural writing

**Capabilities:**
- Generate prose conversationally
- Switch POV with `/as <character>`
- Continue/regenerate last response (swipes)
- Auto-commit everything immediately
- Minimal interruption

**LLM behavior:**
- No tool calls - just writes prose naturally (text completion, not instruction-following)
- Output is auto-committed immediately
- Each response = new passage/paragraph
- Maintains voice and POV from context

**Key difference from Agent mode:** Story mode strips away agentic scaffolding. The LLM is prompted to complete text naturally, not follow instructions with tools.

**System prompt (minimal):**
```
You are writing a novel. Continue the story naturally.

Scene: [current scope]
Characters: [names with brief traits]
Recent prose: [last paragraph]

Write the next passage. Just prose, no commentary.
```

**With character POV:**
```
You are writing from Alice Chen's perspective.

Alice's voice: [brief traits - clipped thoughts, detective's eye]

Recent prose: [last paragraph in Alice POV]

Continue writing in Alice's voice. Show her thoughts and observations.
Just prose, no commentary.
```

**Example interaction:**
```
[Story] > Alice discovers the theft

Alice stood in the empty display case's reflection...
✓ Auto-committed: [Ch1:Sc1:P1]

> Make her more shocked

Alice's breath caught. The case was empty. Completely,
impossibly empty...
✓ Auto-committed: [Ch1:Sc1:P1] (regenerated)

> Continue

The artifact—a small silver cylinder...
✓ Auto-committed: [Ch1:Sc1:P2]
```

**With POV switching:**
```
[Story] > /as alice
[Story:Alice] >

> I discover the theft

I stand before the empty case. The cylinder should be here.
✓ Auto-committed: [Ch1:Sc1:P1] POV: Alice Chen

> /as bob
[Story:Bob] >

> I arrive at the warehouse

I push through the door. Alice is already here. Shit.
✓ Auto-committed: [Ch1:Sc1:P2] POV: Bob Martinez

> /as
[Story] >

[Back to omniscient or default POV]
```

**Person handling:**
Story mode detects whether prose is first or third person from context and maintains consistency. Can be overridden with natural language instructions:

```
> Write this in third person omniscient

Alice stood before the case. Bob entered, unaware that she
already knew his secret.
```

#### Plan Mode (Discussion Only)

**Purpose:** Think through story without committing

**Capabilities:**
- Discuss structure and possibilities
- Analyze story state (read-only tools)
- Explore "what if" scenarios
- Cannot write to story

**LLM behavior:**
- Can use read-only tools (search, show, analyze)
- Cannot use write tools
- Suggests switching to Agent mode to implement ideas
- Explicitly marked as "cannot commit"

**System prompt:**
```
You are discussing story structure and possibilities.
You CANNOT write or edit the story in this mode.

Story so far: [high-level summaries]
Characters: [character arcs]
Unresolved threads: [plot points]

Available tools (read-only):
- search_story(query)
- show_timeline()
- analyze_pacing(scope)
- get_story_arcs()

Help user think through decisions. Suggest possibilities,
analyze consequences. If user wants to implement, ask if
they want to switch to Agent mode.
```

**Example interaction:**
```
[Plan] > Should Bob die in chapter 5?

<tool_use name="search_story">
  <query>Bob's role in later chapters</query>
</tool_use>

Bob appears in chapters 3, 5, 7, 9. Main functions:
info source about employer, redemption arc, sacrifice in climax.

If he dies Ch5, you'd need different info source and new
character for redemption theme. Big structural change.
Recommend branching to try it.

Want me to switch to Agent mode to implement?
```

### Mode Switching

**Tab key:** Cycles through modes (Agent → Plan → Agent or Story → Agent)
**Commands:** `/mode story`, `/mode agent`, `/mode plan`
**Automatic:** Story mode can auto-return to Agent when scene complete

### Core Commands

```
# Mode switching
<Tab>                    # Cycle through modes
/mode agent              # Switch to agent mode
/mode story              # Switch to story mode
/mode plan               # Switch to plan mode

# Character interaction
/chat <character>        # Talk to character
/chat <char> --as <char> # Talk to character while impersonating another
/as <character>          # Impersonate character (changes input interpretation)
/as                      # Stop impersonating (back to director view)
<ESC>                    # Same as /as (exit character context)

# Story manipulation
/edit                    # Open history in $EDITOR
/undo                    # Revert last commit
/regenerate              # Regenerate last response (swipe)
/show [scope]            # View summaries/chapters
/branch [name]           # Create branch
/synthesize              # Merge multiple branches with instructions

# Character and world
/character [name]        # Create/view character
/check [scope]           # Run consistency check
/tasks                   # View/manage objectives

# Utility
/roll [dice]             # Roll dice (TTRPG mode)

# OOC/Meta instructions (any mode)
(( instruction ))        # Out-of-character instruction, always interpreted as director
```

### External Editor Integration

Users can edit chapter files directly:

```
User: vim chapters/03-confrontation/chapter.md
[Makes changes, saves, exits]

TUI detects change:
─────────────────────────────────────
Detected changes in chapters/03-confrontation/chapter.md

Diff:
+ "You lost that right," Alice said. The words tasted bitter.
- "You lost that right," Alice replied.

Create commit? [y/n/review]
> y

Commit summary:
> Sharpen Alice's dialogue

✓ Committed: [Ch3:Sc2] Sharpen Alice's dialogue
─────────────────────────────────────
```

**The `/edit` command:**
```
> /edit

[Launches $EDITOR with history as markdown]

# Story History - Edit as needed

## [Ch3:Sc1] Alice arrives at warehouse
[full prose]

## [Ch3:Sc2:P1] Alice confronts Bob
[prose...]

[User edits directly, saves, quits]

Detected changes in 3 sections
Regenerating summaries...
✓ Updated commits
```

### Startup Behavior

```bash
$ storyteller

# Empty directory:
Creating new story project in /current/dir
Initialized git repository
What structure? [novel/short/custom]
> novel
Using 3-act structure

[Agent] Ready to begin

# Existing story repo:
Found story: "The Artifact Heist"
Last commit: [Ch3:Sc2:P4]
Loaded 3 chapters, 15 scenes
Characters: Alice Chen, Bob Martinez

[Agent] chapters/03-confrontation/chapter.md

# Non-empty, non-repo:
Directory not empty. Create story here? [y/n/subdir]
> subdir
Story name? > artifact-heist
Created: /current/dir/artifact-heist/

[Agent] Ready to begin
```

### Auto-Commit Strategy

**Everything is auto-committed immediately** - users never need to think about "when to commit."

**Agent mode:**
- Tool calls create commits when they execute
- Scene generation, character creation, edits all commit automatically

**Story mode:**
- Every generated passage is committed immediately
- Regenerations create alternate commits (swipes)
- POV metadata included in commit messages

**Plan mode:**
- Never commits (read-only)

**Benefits:**
- No decision fatigue about when to commit
- Complete history of all generations
- Git handles version control automatically
- Can always `/undo` or revert with git commands

### Why This Interface Works

1. **Text-centric:** Writers work with text, TUI keeps focus there
2. **Mode-optimized:** Each mode has appropriate tools and prompts
3. **External tools:** Works with vim/emacs/git directly
4. **Zero lock-in:** Files are markdown, repo is git, TUI is optional
5. **Graceful degradation:** Power users can bypass TUI entirely
6. **Focused workflow:** One prompt, no UI chrome, just text
7. **Context efficiency:** Story mode uses minimal tokens for instructions

### Import/Export

**Import:**
- Blank project (templates for structure)
- Existing manuscript (parse and commit)
- Character cards from other systems

**Export:**
- Concatenate markdown: `cat chapters/*/*.md > story.md`
- Clean manuscript (strip metadata)
- EPUB/PDF (via pandoc)
- Screenplay format (reformat dialogue)

Git handles the rest (clone, push, pull).

## Additional Features

### Image Generation

**Integration with image generation tools:**
- Character portraits: `generate_character_portrait(character, description, style)`
- Scene backgrounds: `generate_scene_background(scope, description, style)`
- Expression variants: Generate emotional states for characters

**Storage:**
- Images stored in git repo (characters/*/portraits/, scenes/*/backgrounds/)
- Referenced in markdown: `![Portrait](portrait.png)`
- Commit tracks generation (model, prompt, parameters)

**Display:**
- TUI: External viewer (file:// links or explicit open command)
- Web UI: Inline display
- Export: Images bundled with story

**Not essential for MVP** but straightforward to add as tool integration (ComfyUI, DALL-E, Stable Diffusion APIs).

### Dice Rolling (TTRPG)

Simple feature for TTRPG mode:
```bash
[Agent] > /roll 1d20+5
🎲 Rolled 1d20+5: 17 (12 + 5)

[Agent] > Alice picks the lock (DC 15)
Roll: 17 vs DC 15 - Success!
```

**Implementation:**
- Parse dice notation (NdM+X)
- Store rolls in commit metadata (reproducibility)
- Reference in prose generation

**Trivial to add** - single tool, 5 minutes of work.

## Open Questions

### Implementation Priority

What to build first to prove the concept?

1. Core git operations (commit, branch, query)
2. Simple Scene Writer agent
3. Character knowledge branch generation
4. Task tracking system
5. Hierarchical summary viewing
6. Consistency checking
7. Multi-branch synthesis
8. Image generation (optional)

### Agent Marketplace

Should there be a library of community-built agents? How to share/distribute?

**Answer:** Git handles this naturally - agents are code, users can publish repos of custom agents. No special infrastructure needed.

### Voice Consistency

How to maintain character voice across 50+ chapters?
- Voice examples in character card (used in prompts)
- Periodic voice checking agent
- User reviews flagged inconsistencies

**All viable approaches** - depends on user preference and workflow.

## Future Possibilities

- **Cross-story character transplanting**: Use same character in different stories (archetype is portable)
- **Story analysis dashboard**: Visualize emotion curves, pacing, character appearance frequency, sentiment analysis
- **VN engine export**: Automated export to Ren'Py, Twine, Ink formats
- **TTS integration**: Read scenes aloud with character voices
- **Translation/localization**: Maintain story structure across languages with cultural adaptation
- **Franchise management**: Shared universe across multiple books/authors
- **Web UI**: Browser-based interface for wider accessibility (shares core library with TUI)
- **Mobile client**: SSH into server, use TUI remotely (or native client if demand exists)

## Conclusion

Storyteller enables sophisticated story development by treating narrative as **queryable, versioned state** while keeping everything in **natural language** that both humans and LLMs can work with.

The combination of git (version control), hierarchical summaries (navigation), and character knowledge branches (consistency) creates capabilities not available in existing tools.

Core innovation: **Story state is real, trackable, and queryable** - not just unstructured text.
