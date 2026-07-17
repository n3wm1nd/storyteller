# Agent integration findings

`PLAN.md` is the suite's design philosophy — how to write a scenario, what
question it should answer, when a failure means what. This is the other
half: what the suite has actually *found*, running against real models.
Some of this is settled (shipped as a code change, verified to hold). Some
of it is a working hypothesis that held up under one round of testing and
hasn't been pushed on further. Marked accordingly — don't read "documented
here" as "proven forever."

## Settled, shipped

- **`defaultSplitConfig`'s `MaxTokens` was too low for real outlines.**
  1536 was tuned against short, agent-generated outlines
  (`Agent.Integration.Journey.storyPremise`); a real, richly-detailed user
  outline pushes `emit_beat_sheet` calls past it, truncating mid-string —
  the model runs out of budget before finishing the beat sheet, not before
  it "goes wrong" per se. Raised to 4096. Looked identical to an escaping
  bug from the parse error alone; it wasn't one.
- **The split step needs to respect the outline's own chapter boundaries.**
  `defaultSplitSystem` now says explicitly that a marked chapter division is
  deliberate (pacing, cliffhangers, length) and beats shouldn't be moved
  across it — the model doesn't get to "improve" the split.
- **The tool result echoing the just-submitted beat sheet back to the model
  was wasted, actively harmful context.** `emitBeatSheet`'s return value
  (the full `ChapterBeats`) was being serialized as the tool call's own
  result, so every submitted chapter appeared in context *twice* —
  once as the call, once as its own confirmation — accumulating turn over
  turn, with zero new information (`confirmationFor` in `Outline.hs` now
  trims this to just `{"saved": "<path>"}`).
- **A judge that never calls `submit_verdict` is not an acceptable
  answer.** `judge` now retries (bounded) instead of failing immediately —
  see `Agent.Integration.Judge`'s Haddock.
- **A failing judge verdict should say why in the failure message itself,**
  not just log it and leave `expected: True, got: False` for a human to go
  digging for. `judgeOrFail` does this now; every spec uses it.
- **The `escapingArtifacts` heuristic can't be applied to content that's
  allowed to contain plausible technical color** (a file path, a UNC
  share) without producing constant false positives — see `../PLAN.md`'s
  literalism-contract section. It's still a real, working check; it just
  needs a scenario with nothing legitimate to trip it
  (`OutlineSplitEscapingSpec`, a plain pirate outline) to mean anything.
- **`characterReflectAgent`'s (and its siblings') `MaxTokens` were sized only
  for the visible answer, starving a thinking model's reasoning tokens.**
  `RoleplayMidStorySpec` failed with an outright empty journal entry
  (`renEntry \`shouldNotBe\` ""`) — not truncated, empty. Same shape as the
  `defaultSplitConfig` finding above (a fixed budget too small for real
  output), but one layer further back: `characterReflectAgent` runs a tool
  loop, and on a reasoning-capable model, `queryLLM`'s `MaxTokens` has to
  cover *both* the model's thinking tokens and the final text turn from the
  same budget. Anthropic's own thinking budget is
  `min 5000 (maxTokens \`div\` 2)` (`UniversalLLM.Providers.Anthropic.anthropicReasoning`)
  — at the old `MaxTokens 3000`, that's 1500 tokens of thinking budget
  leaving only ~1500 for the answer, and at `defaultQuestionConfig`'s old
  `MaxTokens 1500` the thinking floor (1024) alone left under 500 for the
  answer. A verbose reasoner can burn its whole turn on thinking tokens and
  never emit any `AssistantText`, which is exactly what an empty-string
  result looks like from the caller's side. Raised `defaultQuestionConfig`
  1500→5000, `defaultComposeConfig` 3000→6000, `defaultCharacterConfig` and
  `defaultReflectConfig` 3000→8000 (`Storyteller/Writer/Agent/Roleplay.hs`)
  — sized so the thinking budget can actually reach Anthropic's 5000-token
  cap while still leaving real room for the answer, not just clearing the
  floor. Root-caused and fixed from the code path alone (the OpenAI/
  OpenRouter side has a separate, still-open gap — see below); not yet
  reconfirmed against a live run in this session, since no
  `OPENROUTER_API_KEY` was available here.
  - **Open gap, OpenAI/OpenRouter reasoning models specifically:**
    `openAIReasoning`/`openRouterReasoning`
    (`UniversalLLM.Providers.OpenAI`) hard-code `reasoning_max_tokens =
    Nothing` — no explicit reasoning-token cap is ever sent, so there's no
    code-level guarantee of answer headroom the way Anthropic's
    `thinkingBudget` calculation provides. Raising `MaxTokens` helps here
    too (more total budget), but doesn't reserve output room the same way;
    worth a follow-up if this failure mode recurs against a
    OpenRouter-routed reasoning model specifically.
- **A judge rubric can fail a generation that actually did the right
  thing, if its wording is ambiguous about what "counts."**
  `CharContextWriteSpec`'s question asked whether the text was "consistent
  with a stated aversion to fish"; Mira's sheet says she "goes quiet, pushes
  the plate a small distance away, and changes the subject" when served
  fish, and the generated scene did exactly that, near-verbatim — but the
  judge failed it anyway, reading "stated" as "must be narrated/explained
  in-scene" rather than "behaviorally consistent with." Reworded the
  question to describe the concrete behavioral pattern directly (declining,
  going quiet, physically distancing, changing the subject) and say
  explicitly that the text doesn't need to explain why. Reran live: passes
  now, and the judge's own reasoning cites the actual behavioral markers —
  confirms this was a rubric-wording gap, not the judge becoming more
  lenient in general (`judge`'s system prompt/mechanism is untouched).

## Working hypothesis, one round of evidence

**The tool-call loop itself is a major cause of structural failure in
weaker models — more than raw model capability.**

The clearest single result of this investigation. Same task (split a messy,
3-chapter outline into beat sheets), same model (`openai/gpt-oss-20b` via
OpenRouter), three mechanisms:

| mechanism | gpt-oss-20b | deepseek-v4-flash |
|---|---|---|
| tool-call loop (`splitOutlineAgent`) | 0/3 — duplicated `ch1`, omitted chapters, garbled order, turn counts up to 35 | ~10/11 across the full suite |
| sequential conversation, one chapter per turn (`splitOutlineFreeform`) | 2/3 — clean 3-chapter structure both times; the one failure is a single beat pulled across a boundary, not structural collapse | 3/3 |
| bulk, one response, `---`-delimited (`splitOutlineBulk`) | **3/3** — clean sweep | 3/3 |

Bulk beating sequential for the weak model, specifically, is worth sitting
with: it means re-prompting turn by turn — even *without* tool calls, even
with the bookkeeping burden already moved onto our own code
(`splitOutlineFreeform` counts chapters and assigns paths itself, the model
never has to) — still cost something. A single uninterrupted draft avoided
even the mild boundary drift the sequential variant showed. For deepseek
both land at 3/3, so this only shows up once the model is already weak
enough for turn-by-turn interruption to matter.

A raw probe (bypassing the harness, straight OpenRouter API call, informal
precursor to `splitOutlineBulk`) showed the same thing even more starkly:
asked to just write the three beat sheets as delimited markdown, gpt-oss-20b
got the structure perfectly right — correct count, correct order, no
duplication — while its *prose quality* was still visibly weaker than
deepseek's (some genuinely incoherent sentences by chapter 3). That's two
separable findings, not one:

1. The tool-call mechanism is what breaks structural tracking for a weaker
   model — most likely because each call is an isolated, JSON-syntax-
   constrained completion, and the model's only way to know "what have I
   already covered" is re-deriving it from its own past tool calls in
   context, which is exactly the kind of implicit bookkeeping smaller
   models are worst at. `splitOutlineFreeform` removes the bookkeeping
   burden from the model entirely — the loop itself (Haskell code) counts
   chapters and assigns paths, the model's only job each turn is "write the
   next chapter or say you're done."
2. Raw prose/reasoning quality is a separate, real capability floor that a
   mechanism change doesn't touch — gpt-oss-20b is still weaker prose-wise
   than deepseek-v4-flash even under ideal (non-tool-call) conditions.

**The general shape, not just this one case:** a model is fundamentally a
text-completion engine; chat formatting and tool/function calling are both
layers of fine-tuning imposed on top of that, not the native mode. Chat is
the layer closest to what most models get the most training data for, so
it degrades gracefully; forced tool calling is a further, more specialized
layer on top of *that* — real, but thinner, and models not specifically
trained on agentic workflows (as opposed to e.g. coding-focused models with
heavy agentic RL) have less of it to draw on. That predicts exactly the
ordering measured here (raw completion > chat > tool calls), and predicts
it'll get worse, not better, for other agents built the same way
(`reworkAtom`, the chat agent) against a similarly weak, non-agentic-tuned
model — worth keeping in mind before assuming a tool-call-shaped agent that
works well against a frontier model will degrade gracefully against a
smaller one; on this evidence it won't, disproportionately.

**What this does *not* yet establish**, and would need a second round to
say more confidently:
- Whether a longer, more elaborate prompt helps or hurts a weak model more
  generally — the one experiment run here (asking the tool-call version to
  explicitly enumerate chapters up front) made GPT-OSS-20B's results *worse*
  (turn counts 6→35, more duplication), which is itself only one data point
  against one prompt variant, not a general "don't scaffold more" rule.
- Whether bulk beating sequential holds for a longer outline than 3
  chapters — `defaultBulkConfig`'s `MaxTokens 8192` covers every chapter's
  beat sheet in one response, and that budget only gets tighter as chapter
  count grows, the same way `defaultSplitConfig`'s did for the tool-call
  version. Worth deliberately testing against a bigger outline before
  trusting bulk as the general answer rather than just the answer for
  three-chapter fixtures.
- Whether `splitOutlineFreeform`'s remaining failure mode (a beat pulled
  across a chapter boundary) is itself mechanism-related (still some
  cross-turn bookkeeping, just less of it) or purely a content-judgment
  question independent of mechanism.

**Not a reason to remove `splitOutlineAgent`.** Its explicit per-chapter
path targeting and "skip chapters that already have a beat sheet" logic are
still the right shape for incrementally filling in missing/added chapters
later, where the model needs to name one specific gap rather than continue
a sequence from scratch. `splitOutlineFreeform` is for the first,
whole-outline split; the two aren't in competition for the same job.

## Context assembly rework: first live pass, three findings

`writeAgent`'s per-chapter `[Message]` rework (lore/style/per-character
`CharSummary`/pinned/earlier-chapters/tick-history, replacing one flattened
prompt) needed its own real-model pass — four new scenarios
(`CharacterPresenceSpec`, `WorldLoreSpec`, `JournalInstructionSpec`,
`JournalIronySpec`) all pass live against `deepseek-v4-flash`, confirming
character presence, world lore, and both journal mechanisms (a manually
appended private resolve, and a retroactively *edited* journal atom) all
reach the model and shape its output correctly. Running the full suite
after that turned up three pre-existing failures, unrelated to each other.
One (`CharContextWriteSpec`'s rubric wording) is fixed and moved to
"Settled, shipped" above; the other two are real model findings, left as
failing on purpose rather than loosened:

- **`writeAgent`'s fixed "~300 words" length hint can force a chapter to
  drop entire beats rather than compress them — working hypothesis, one
  data point (`JourneySpec`).** Chapter 1's beat sheet had 5 beats (its own
  estimate: ~12 pages worth); the generated prose landed right on its
  ~300-word target, but got there by dropping Beat 1 (the opening "bean
  dilemma" scene) and Beat 5 (the shipboard coda) entirely, and reordering
  Beat 2's own internal sequence. The judge caught this accurately — verified
  directly against the prose, not just trusting the verdict. Same shape as
  the already-settled `defaultSplitConfig` `MaxTokens` finding above (a fixed
  budget too small for a real beat sheet), but on the prose side and against
  a word count rather than a token budget — `buildChapterMessages`'
  instruction message hardcodes "approximately 300 words" regardless of how
  much the beat sheet actually asks for.
- **A counter-example to the bulk-split table above, from a fixture already
  tracked in this doc.** `OutlineSplitBulkSpec`'s "non-chapter lead-in and
  bracketed author's asides" fixture (`codeFenceAndBackslashes`) — previously
  reported 3/3 clean for `deepseek-v4-flash` in the table above — failed on
  this run: the outline's Chapter 2 ends right as a phone call connects
  ("she calls it anyway, and someone picks up"), with the conversation itself
  ("knows her father's name... it's a promise") explicitly Chapter 3's
  content. The model's ch2 beat sheet pulled that whole conversation forward
  into ch2, then, with nothing left for ch3, **invented a new scene wholesale**
  — an in-person café meeting with a new character ("Miriam") appearing
  nowhere in the outline. Confirmed directly against the cached response,
  not just the judge's summary. This is content invention, not just boundary
  drift, and it's a genuine counter-example to "bulk beats sequential" as a
  general claim — that hypothesis was already flagged above as needing a
  second round; this is that second data point, and it points the other way
  for at least this fixture/model pairing.

## Provider-level quirks observed, not ullm bugs

- **GPT-OSS via OpenRouter occasionally leaks a raw Harmony-format
  `<|channel|>commentary` token into a tool call's `name`** (e.g.
  `emit_beat_sheet<|channel|>commentary`), breaking the tool-name match. A
  direct, isolated probe against OpenRouter's API for the same model
  returned a clean `tool_calls[].function.name` — so this looks like
  inconsistent normalization on the provider/backend side (OpenRouter
  routes gpt-oss-20b across multiple upstream backends; some may handle
  Harmony's channel markers less reliably than others), not something
  `ullm`'s parsing is getting wrong. No defensive stripping added yet —
  logged here so it's not re-discovered as a mystery next time it shows up.
- **`GPTOSS20B` via OpenRouter's `HasJSON` claim is real** (verified via
  `jsonMode`/`jsonSchemaShapeCompliance` protocol probes — genuine
  `response_format` schema enforcement, not a GLM-style "accepted but
  ignored"), but it can under-deliver on requested item counts within an
  otherwise well-formed JSON response (asked for 3 colors, got 1). That's a
  content-instruction-following gap, not a JSON-support gap — the two
  protocol probes are what actually distinguish this from the GLM case.

## Roleplay writer: does per-character knowledge separation actually hold

The question `RoleplaySpec`/`RoleplayMidStorySpec` exist to answer: characters
in `Storyteller.Writer.Agent.Roleplay` are each scoped to only their own
branch (sheet, journal, notes) plus on-demand shared lore — does that
structural separation actually keep one character's private knowledge out of
another's stated intent and journal against a real model, or is it a
plausible-sounding idea that collapses once a model has to keep it straight?
**Settled: it holds**, against both a simple one-directional secret
(`RoleplaySpec`, `deepseek-v4-flash`) and a harder, mid-story scenario with
established history and a *mutual* two-directional secret — two characters
each independently wavering, neither aware the other is too
(`RoleplayMidStorySpec`) — once the issues below were fixed. Both pass live.

Settled, shipped, found while getting there:

- **A per-character question that came back blank wasn't a content problem,
  it was `defaultQuestionConfig`'s `MaxTokens 200`** — the same reasoning-
  token-budget shape as this doc's other `MaxTokens`-too-tight findings, just
  costing the *question*, not the answer: with nothing left after thinking
  tokens, the model emitted no `AssistantText` at all, and
  `questionForCharacterAgent` silently returned `""`, which downstream just
  read as "ask X: " with nothing after the colon. Raised to 1500; reran live,
  every character got a real, specific question.
- **A presence tick only marks who's in a scene that already exists.**
  Recording presence on a scene file with no atom yet (nothing written to it,
  only presence ticks referencing it) means `Tick.fileTicksOf`'s tree-
  presence-scoped walk finds nothing to attach those ticks to —
  `activeCharactersFor` silently comes back empty, `roleplayAgent` runs with
  zero characters, and the model just improvises a scene from the direction
  text alone with nobody actually interrogated. `CharacterPresenceSpec`
  already documents this exact trap; `RoleplaySpec`'s own fixture tripped it
  anyway when written fresh (a real 0.005s "pass" that was actually 0 LLM
  calls, not a fast cache hit). Fixed by seeding one real atom
  (`Ops.addAtom scenePath ""`) before recording presence, matching the
  existing convention.
- **Giving a character `glob`/`read_file` on their own branch is pure
  waste, not a safety net.** `askCharacter` already reads every file on a
  character's branch in full via `charSummaryAgent` and injects it as
  unconditional context before the first turn — the model still spent turns
  defensively re-reading what it already had in front of it. Removed both
  tools for the own-branch case entirely (kept `write_file`/`edit_file`/
  journal tools, which are genuinely needed). Shared world lore stays the
  opposite shape on purpose — read-only, on-demand (`lore_glob`/
  `lore_read_file`), never injected wholesale, since lore can be large and is
  usually mostly irrelevant to any one turn — but even there, the model was
  burning a `lore_glob` round trip just to discover *what* lore exists before
  reading it. Fixed by unconditionally injecting the lore *file list* (paths
  only, not content) so a turn can go straight to `lore_read_file` on a path
  it already knows about.
- **A denied or merely-missing tool call was crashing the whole scenario,
  not just failing that one call.** Two related bugs, one root cause:
  `Runix.Tools`/`Runix.FileSystem`'s convenience wrappers (`writeFile`,
  `readFile`, `editFile`) turn *any* `Left` — a `filterWrite`-denied path,
  or an ordinary "no such file" from a hallucinated path — into an uncaught
  `Polysemy.Fail.fail`, and `UniversalLLM.Tools.executeToolCallFromList`
  never catches it, so it aborts the entire run instead of just that tool
  call. Both were live bugs (the sheet.md/journal.md write-protection case
  was never actually exercised until a model tried it; the "not found" case
  showed up as `characters/owen/sheet.md: not found` killing a whole spec).
  Fixed by switching tool dispatch to `Runix.LLM.ToolExecution.executeTool`
  (already used by runix-code's own agent loop, not something invented for
  this) — it wraps each call in `runFail`, turning a failure into a proper
  `ToolResult (Left ...)` the model actually sees and can react to, instead
  of reimplementing per-tool error handling by hand.
- **`sheet.md`/`journal.md` write-protection belongs at the filesystem-
  effect layer, not inside a tool's own function body.** Same
  `Runix.FileSystem.PathFilter`/`filterWrite` machinery `hideChapters`/
  `hideLore` already use for read-side narrowing (`Storyteller.Writer.Agent.
  ContextFilter`), applied to the write side instead — a real interception
  that can't be bypassed by a differently-shaped future tool, not a guess
  re-implemented per call site. Has to be scoped to specifically `write_file`
  /`edit_file`'s own tool functions, not the whole tool loop: `add_thought`/
  `add_suspicion` legitimately write `journal.md` through this exact same
  effect, and get wrongly denied too if the filter reaches them.
- **The character subagent's final answer being a *performed* line (first-
  person, as if already speaking) rather than *information* was a real
  design gap, not a style nitpick.** It reads naturally but actively works
  against the narrator's own job — `composeSceneAgent` needs material to
  synthesize from, not a pre-written scene it either overrides (redundant
  work) or rubber-stamps (loses its own judgment on pacing/wording).
  Restructured `characterIntentAgent`'s answer into three explicit sections
  (what they'd do, their mood, 2-4 *possible* lines — options, not a script)
  and reframed the identity note and system prompt around "informing the
  narrator," not acting.
- **Tool-usage etiquette (don't narrate the decision to use a tool) belongs
  on the tool's own description, not a general prompt rule.** A reflection
  entry leaked meta-commentary like "No need to update any notes — Owen's
  trusting nature means..." before the actual journal text, because nothing
  told the model that call was invisible to the reader. Fixed by adding
  "this call itself is invisible; don't mention whether or how you used it"
  directly to `add_thought`/`add_suspicion`/`write_file`/`edit_file`'s own
  descriptions, not a rule buried in the system prompt the model has to
  generalize from.
- **Post-scene reflection needs the same tool access as pre-scene intent,
  not less.** `characterReflectAgent` started as a tool-free, single-shot
  prose call — meaning a character could only ever act (update notes, jot a
  thought) on what they *planned*, never on what actually *happened*, which
  is backwards: reflection, after seeing the real outcome, is the natural
  moment to correct a wrong belief about someone or record a thought the
  scene itself prompted. Turned it into a full tool loop too (same
  `characterTools`), run inside the same branch scope as the final,
  ref-carrying journal commit.
- **Character-branch mutations (notes, journal) deliberately never touch
  `Storage.Ops`/`BranchOp`, only the plain `Runix.FileSystem` effect** — the
  one place in this whole design that needs more is the post-scene journal
  commit itself, which carries a real cross-branch ref back to the scene's
  own atom (`Storage.Ops.addAtomWithRefs`) that a bare filesystem write has
  no way to express. Everything else (notes, in-scene thoughts) stays at the
  uniform tool-facing layer on purpose, not because the richer layer wasn't
  considered.

Judge-rubric lessons, same "settled, shipped" bar (the rubric wording changed,
verified live against the same cached generation to isolate the fix from the
model's own output):

- **A rubric that fails *any* noticed behavioral tell as a knowledge leak is
  testing the wrong thing.** `RoleplayMidStorySpec`'s scene had one character
  actually behave subtly out of character (a stated intent that included a
  hesitation, an off smile) — the *other* character, present for it, noticing
  and forming a private suspicion from it is earned dramatic irony, grounded
  in what was genuinely visible in the scene, not a leak. The violation this
  rubric actually needs to catch is knowing or stating the *specific content*
  of the partner's private doubt — what it's about, why it's there — not the
  mere fact that something seemed off. Reworded `irony` to draw that line
  explicitly; the same cached generation that failed the old wording now
  reads as a pass under the corrected one (verified the judge's own new
  reasoning, not just the boolean).
- **A rubric that requires an established feeling to resurface in *every*
  new entry is asserting something false about how characters (or people)
  actually work.** A character's history should inform who they are; it
  doesn't obligate re-litigating the same internal conflict on every single
  turn, and a tactical, present-moment entry with no guilt in it is
  realistic, not a failure of anything the test is actually about. Dropped
  that clause from `irony` entirely — the rubric now tests exactly one claim
  (knowledge separation) and nothing else, which is the only thing this
  scenario was ever built to answer.
