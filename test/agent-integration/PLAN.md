# Agent integration suite: what it's for

This suite (`storyteller-agent-integration-test`, kept separate from the fast
mocked `storyteller-test` suite) exists to answer one question:
**as configured, do our agents actually work against a real LLM?** Not "does
the plumbing work" — file IO, git storage, JSON decoding, tool-call
dispatch — that's `storyteller-test`'s job, against mocked/deterministic
interpreters. This suite is about model *behaviour*: does the model follow
the prompt, get the tool call right, produce content that actually reflects
its input.

Every LLM response is cached to disk (`test/fixtures/llm-agent-cache`,
committed) so a passing run replays without hitting the network — but the
value of the suite is in the first, live run against a real model, and in
being re-run against a new model/prompt during tuning.

This document is the suite's design philosophy — how to write a scenario,
what question it should answer, when a failure means what. `FINDINGS.md`
is the other half: what running it against real models has actually turned
up, settled and still-working-hypothesis alike.

## What a scenario should look like

- **Realistic input, not adversarial input.** A scenario should look like
  what an actual user gives an agent — imperfect, informally formatted,
  occasionally messy (quoted dialogue, nested bullet lists, a stray code
  fence) — not a deliberately hostile prompt-injection attempt. The suite is
  for tuning prompts against real usage, not for security testing.
- **Check what a user would expect, not just what's structurally present.**
  A file existing at the right path with non-empty content is necessary but
  not sufficient — does chapter 2's beat sheet actually reflect chapter 2 of
  the outline, or did the model garble/swap chapters under irregular
  formatting? Structural checks (`shouldSatisfy`, path conventions) catch the
  former; an LLM-as-judge call (`Agent.Integration.Judge.judge`) is often the
  only way to catch the latter.
- **Minimal, not exhaustive.** Each scenario should isolate roughly one
  real-world irregularity (e.g. "outline written as nested bullets instead of
  prose") rather than piling every irregularity into one fixture — a failure
  should point at what to fix, not require guessing which of five things
  broke. But not so small it stops meaning anything: three chapters is enough
  to see a pattern, one usually isn't.
- **Stages can build on each other**, the way a real Writer-tab session does
  (`Agent.Integration.Journey.runJourney`: outline → split → per-chapter
  prose) — but a scenario built to isolate one specific step's reliability
  (e.g. `Agent.Integration.OutlineSplitQualitySpec`, which only exercises the
  split step) stays standalone on purpose, so a failure there is
  unambiguously about that step.
- **An already-covered flow can be reused as cheap setup for a different
  scenario**, not just re-run for its own sake. Because every LLM call is
  cached (`Runix.LLM.Cache.cacheLLM`), replaying an earlier, already-tested
  flow to reach some starting repository/branch state is near-instant on a
  cache hit — no live round trip — so there's no real cost to, say, calling
  `runJourney` up through the outline step just to get a populated branch to
  test something else against. Wrap the setup call in
  `Agent.Integration.Harness.quietSetup` so its own step-by-step logging
  doesn't drown out the scenario that's actually under test; only what's
  actually being exercised should show up in a run's output.

  Minimal vs. full-scale setup is a real choice, not a default to "always
  use the smallest thing that works": which one to reach for depends on what
  the new scenario is actually testing, *and* on what's already cached.
  Testing something that only needs "a branch with *an* outline in it" wants
  the cheapest setup that satisfies that, generated fresh if nothing fits.
  But if a full-scale flow's result is already sitting in the cache and a
  minimal one isn't, don't manufacture a smaller fixture just for its own
  sake — replaying the full-scale one is just as cheap on a cache hit, and
  every new fixture is another cache key to keep warm and another prompt
  variant readers have to understand. Reach for minimal-and-fresh only when
  the scenario's own point genuinely needs a smaller, more controlled
  starting state than what's already on hand.
- **Check intermediate stages, not just the final result.** Are the files
  the next step needs actually where the convention says they should be, in
  the shape the next step expects, before that next step runs? Waiting until
  the very end to assert everything at once means an early failure gets
  diagnosed by its downstream symptom instead of its actual cause.
- **Fail early, including from inside a single agent's own retry loop.** A
  multi-stage flow (outline → split → prose) should stop at the first stage
  that comes back wrong rather than spending an expensive live generation on
  chapters written from an already-broken beat sheet — `runJourney` enforces
  this itself (via `Fail`), not the spec's assertions after the fact, so a
  spec can assume every earlier stage already held.

  That's stage-level, but the same principle applies *inside* one agent's
  own tool-call loop: a model recovering after one or two retries is
  legitimate self-correction (`splitOutlineAgent`'s retry loop exists for
  exactly that), but a model that's still failing after several is no longer
  "working as designed," it's being propped up by the retry loop, and "it
  took five retries" would otherwise silently read as a pass. Checking that
  needs to live at the LLM-call boundary itself, not after the loop returns
  — see `Agent.Integration.Harness.assertToolCallBudget`, which intercepts
  each response as it arrives and fails as soon as the retry count (read
  straight off the growing conversation history `Runix.LLM.QueryLLM` is
  handed on every turn — no separate counter needed) exceeds a budget.
  Budget `0` means "must be right first try" (`OutlineSplitQualitySpec`,
  whose whole point is measuring that); a small nonzero budget (`runJourney`)
  tolerates real self-correction while still catching a model that's
  actually struggling.
- **Keep the caller informed — these are long-running, live-LLM tests.**
  A run against a real (especially local) model can take minutes with no
  visible progress otherwise. Log liberally with `Runix.Logging.info` at
  every stage boundary and `Runix.Logging.warning` for anything recoverable
  but noteworthy, not just at the end — `Agent.Integration.Harness.recordToolCalls`
  and `assertToolCallBudget` both log every tool call as it comes back for
  exactly this reason, so a stalled or looping run is visible turn by turn
  instead of going silent until it either finishes or times out.
  `Agent.Integration.Harness.loggingPretty` (indented, dimmed/colored) keeps
  that visible against hspec's own interleaved output rather than blending
  into it — kept local to this suite rather than changed in the shared
  `Runix.Logging.loggingIO`, which every other consumer of the library
  (server, CLI, ...) also uses and has no reason to inherit a look this
  specific.
- **"Do the shipped defaults work" and "does the pipeline behave correctly"
  are two different questions — don't let one scenario answer both by
  accident.** A scenario testing pipeline/agent-logic correctness (tool-call
  format, path conventions, retry behaviour, content fidelity) doesn't need
  full-scale output to do it, and forcing full scale just makes the scenario
  slower and more likely to fail on an unrelated resource limit (token
  budget) instead of the thing actually being tested. Nudge the model toward
  shorter, still-representative output via a
  `Storyteller.Core.Prompt.PromptStorage` override
  (`Agent.Integration.Harness.withPromptOverride`) for that kind of scenario
  — see `Agent.Integration.OutlineSplitQualitySpec`. But that nudge means the
  scenario is no longer testing whether the *shipped, unmodified* prompt and
  settings (`Storyteller.Writer.Agent.Outline.defaultSplitConfig` and
  friends) are actually sufficient for the vast majority of models against
  real, unshortened input — that's a separate, independent scenario with no
  override at all (`Agent.Integration.OutlineSplitDefaultsSpec`), reusing the
  same input fixtures but checking a narrower thing: did the model finish at
  all, not whether its output was great.

## When a scenario fails

A mismatch between what the suite expects and what the real model did is a
finding about the model, not a bug in the suite. It means one of:

1. the prompt/instructions or sampling config need tuning
   (`Storyteller.Writer.Agent.Outline`'s `defaultSplitSystem`/
   `defaultSplitInstructions`/`defaultSplitConfig` and friends) — the
   concrete case this suite was actually built to catch turned out to be
   this one: `defaultSplitConfig`'s `MaxTokens` was too low for a real,
   richly-detailed user outline, so `emit_beat_sheet` calls arrived
   truncated mid-string on a chapter the model hadn't finished writing —
   not an escaping bug at all, even though a truncated JSON string and an
   over-escaped one can look identical from the parse error alone,
2. the agent's own logic needs to change (e.g. how it retries a malformed
   tool call), or
3. the configured model has a real, documented limitation worth writing down
   rather than re-discovering next time (see
   `Agent.Integration.ToolCallQuality`'s Haddock for local/quantized models
   over- or under-escaping free-form text inside a JSON tool-call argument —
   genuine, but rarer in practice than truncation turned out to be, and it
   needs a scenario built to actually settle the question rather than a
   heuristic scattered across scenarios that don't: a check flagging
   "literal `\n`" can't tell a model's mistake apart from a Windows path or
   any other content with a legitimate lone backslash followed by the
   letter "n" once it's already been JSON-decoded — so it only means
   anything run against an outline with nothing legitimate to escape in the
   first place. `Agent.Integration.OutlineSplitEscapingSpec` is that
   scenario: a plain premise, isolated from `OutlineSplitQualitySpec`'s
   messy fixtures (which deliberately invite the model to invent technical
   color and would drown the signal in false positives), where any hit is
   an unambiguous finding rather than something a human has to eyeball).

The suite existing and passing is not the goal; what it tells you about the
model is.

## What the outline means: the literalism contract

An outline-splitting scenario that embeds a literal technical excerpt (a
file path, a code snippet, exact quoted text) as part of the plot itself —
not incidental formatting, but the actual clue a mystery hinges on — isn't
testing something real. Reproducing it verbatim in the resulting beat sheet
is *correct* storytelling once the outline hands it over as literal story
content, so a check flagging the resulting raw backslash as a mistake is
flagging the wrong thing (this is what `OutlineSplitQualitySpec`'s original
`codeFenceAndBackslashes` fixture got wrong, and why its rewritten version
drops the embedded code fence).

The reason that's the wrong shape of test rather than just a wrong fixture:
there are two ways `splitOutlineAgent` *could* treat its input, and this
project has to pick one rather than leave it ambiguous. Either (a) the model
is told some outline content might be incidental and should use judgement
about what's really plot versus formatting noise, or (b) the system is
upfront with the *user* that whatever they write in the outline is taken as
literal story content — full stop, no guessing at intent. (b) is the one
this project actually wants: it matches every other agent here (expand,
prose, reconcile — all "faithfully realize what you were given," never
"infer what you probably meant"), and it doesn't ask a model to make a
judgement call this project has no reliable way to verify it's making
correctly. A scenario meant to stress-test messy *formatting* (dashes,
nested lists, informal headings) should still keep that promise: any
bracketed author's-aside-to-self or non-chapter lead-in in a fixture is
fair game (real, common, and doesn't hinge on the model correctly guessing
what's diegetic) — a code-fenced technical artifact presented as the literal
in-story clue is not, because there's no ambiguity to resolve: the contract
already says take it literally, so the model doing exactly that isn't a
finding.
