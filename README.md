# Storyteller

There are two kinds of files.

Files that represent **current truth** — wiki pages, notes, code. You edit them in place. The history is noise. The interesting question is *what is this now.*

Files that represent **accumulated time** — stories, journals, logs. Each line was written when it was the last line. Nothing below it existed yet. The interesting question is *what was true when.*

Most editors are built for the first kind. Storyteller is built for the second.

---

## The problem it solves

LLMs are already good enough to turn ideas into serviceable prose. The bar for most people isn't literary — it's *coherent and entertaining*, and that's reachable today.

What isn't solved is the structural problem: over a long story, things go wrong in ways that accumulate silently.

- A character drifts from who they were established to be
- Alice references something she couldn't have known yet
- A detail from chapter 2 quietly contradicts chapter 8
- You fix it in the moment, but the root cause is still there, and it happens again

These aren't quality problems. They're structural problems. A better LLM doesn't fix them — a better framework does.

Storyteller is that framework.

---

## Who it's for

**The ideas-first person.** Rich internal world, weak or unmotivated prose output. The bottleneck isn't imagination — it's the labor of turning a scene in your head into words on a page. Storyteller lets you direct; the LLM translates; the structure keeps it coherent across sessions.

**The language-first person.** Loves crafting sentences, less confident about plot architecture and consistency. Use Storyteller's structural layer to handle what you don't enjoy, then write the scenes you care about.

**The documentation use case.** Long-running D&D campaigns, TV series bibles, comic multiverses — where the "work product" isn't to read, it's to *know*. What does this NPC know at this point in the timeline? What happened in session 23 that matters now? Storyteller makes those questions answerable without manual bookkeeping.

What it's probably not for: serious authors who already hold complex plot structures in their head, or anyone for whom artistic integrity means no LLM involvement. Those are valid stances — Storyteller just isn't aimed at them.

---

## How it works

At its core, Storyteller is a **structured workspace** for long-form narrative — one that understands the difference between a story and a wiki.

### The story is a log, not a document

Content is written forward, in order. Things get added; nothing gets silently overwritten. If you correct something from earlier in the story, that's a deliberate act with understood consequences — not an invisible edit.

This matches how stories actually work: what Alice knows in chapter 3 is different from what she knows in chapter 7, and that difference matters.

### Characters keep their own view of the world

Every character (or entity — a faction, a location, anything with a perspective) can have their own branch of the story: what they witnessed, what they believe, what they remember — in the order they experienced it.

This makes dramatic irony and unreliable narrators structural rather than editorial. It also means "what does Alice know right now?" is a query, not a guess.

### The LLM gets the right context, not everything

Fitting a novel into a context window doesn't mean the LLM knows where to look. Storyteller assembles focused, relevant context for each generation call — recent prose, the right character sheets, a summary of what's happened — rather than dumping everything and hoping for the best.

This matters especially with local LLMs, where context quality has a direct effect on output quality.

### Fixes are permanent

When a character drifts, you fix it at the root — amend their branch at the point where the wrong belief or constraint entered, and regenerate. The fix propagates forward automatically. You're not patching around the problem every session; you're correcting it once.

---

## What it looks like

Open a project: file tree on the left, prose in the center, an instruction field at the bottom. It looks like a text editor. For most workflows, that's all you interact with.

The structure reveals itself as you need it. The character knowledge panel, the branch view, the consistency checker — they're one interaction away, not in your face on day one.

The interface comes in three flavors optimized for different workflows:

- **Writer** — structured novel and scene writing, with hierarchical navigation and character knowledge tracking
- **RP** — cooperative storytelling and character interaction, closer to the SillyTavern model but with persistent state and a real data model underneath
- **Wiki** — encyclopedia-style reference for lore, characters, arcs, and mysteries, with cross-references and annotation

All three share the same storage backend. A campaign run in RP mode and documented in Wiki mode are looking at the same underlying story.

The backend is frontend-agnostic: a WebSocket server exposes the full agent and storage layer, and any frontend can connect to it. The WebSocket model is a natural fit — each window opens a connection scoped to a branch, receives a full snapshot on connect, and stays live for the duration of the session. Multiple windows on the same branch all receive updates as they happen.

---

## What it isn't

Storyteller doesn't make LLM output better. It makes the structural problems around LLM output solvable — drift detectable, corrections permanent, context focused, history preserved.

The LLM is still the one writing. Storyteller is the framework that makes what it writes stay coherent over time.

---

## Status

Early development. The data model and design are documented in `DESIGN.md` and `DATA-MODEL.md`. A UI mockup exists in `mockup/`. The backend is being built in Haskell using the `runix` effect system. Currently in progress: a WebSocket server (`story-server`) and CLI tools for individual agent operations.
