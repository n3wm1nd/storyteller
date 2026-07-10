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
