{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The application's own default context-selection policy, expressed as
--   ordinary DSL definitions rather than Haskell logic -- see the project
--   chat that designed this (2026-07-20): classification (what counts as
--   lore, as a chapter, ...) is real content-selection policy, and belongs
--   in overridable DSL text written against a project's own directory
--   conventions, not a filter or a Haskell predicate standing in for it
--   (that mistake was tried and reverted twice in this same session --
--   first as a hardcoded three-bucket 'Storage.Core.ObjectHash'-keyed
--   scope, then as a @whereType@ filter -- both just moved the same fixed
--   policy one layer down without actually making it project-editable).
--   The DSL never sees a binary file to begin with -- that exclusion is
--   decided at the storage layer, before any DSL text runs, by
--   'Storage.Query.loadLiveWorkingTree' (what
--   'Storyteller.Context.DSL.Compile.treeValueOfCommit' builds every
--   Reader scope from) -- so there's nothing left, Haskell-side or DSL-
--   side, for a project to reach for here even if it wanted to.
--
--   Every definition here is named the same way
--   'Storyteller.Core.Prompt.PromptKey' names a prompt override --
--   dotted, namespaced -- and is looked up on the Contexts branch
--   ('Storyteller.Core.Context') before falling back to the compiled-in
--   'Storyteller.Context.DSL.AST.Definition' registered here
--   ('defaultLibrarySource'). Composition between these pieces
--   ('contextWriter' pulling in 'contextLore'\/'contextChapters'\/
--   'contextOther') is *cross-definition name resolution*, not Haskell
--   parameter passing -- a body referencing @context.lore@ by its dotted
--   name resolves against the compile-time library table
--   'Storyteller.Core.Context.buildContextLibrary' builds (see
--   'Storyteller.Context.DSL.Compile.resolveIdent'), the identical way
--   whether the current name means the compiled-in default or a
--   project's own committed override -- see 'defaultLibraryOrder''s own
--   Haddock for the fixed compile order this now relies on. Only a
--   genuinely host-backed
--   capability (@journalDelta@'s Haskell-level curried tuning, say) still
--   needs Haskell-side parameter passing -- see 'contextCharacter''s own
--   @journal@ parameter -- because that's real per-caller parametricity,
--   not a shared default a project should be able to replace by name (see
--   'characterBlurb''s own haddock for the case that used to be
--   parameter-passed for no good reason, and the bug that came from it).
module Storyteller.Context.DSL.Library
  ( -- * Entry points
    --
    -- $entrypoints
    contextLore
  , contextChapters
  , contextChaptersCompressed
  , contextOther
  , contextWriter
  , contextWriterDef
  , contextCustomDef
  , contextCharacter
  , characterSummaryOf
  , contextMentionFilter
  , toBinding1
  , defaultLibraryOrder
  , defaultLibrarySource
  , hostLibrary
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Polysemy (Member, Members)
import Polysemy.Fail (Fail)

import Storyteller.Context.DSL.AST (Definition, Name)
import Runix.FileSystem (FileSystem, FileSystemRead)
import Storyteller.Context.DSL.Compile (Binding(..), ContextFS, Library, bval, compileDefinition, hostLibrary)
import Storyteller.Context.DSL.Context (toBinding)
import Storyteller.Context.DSL.QQ (defQuote, dsl)
import Storyteller.Context.DSL.Value (Action, Value, namedEntry)
import qualified Storyteller.Context.DSL.Render as Render
import Storyteller.Writer.Agent (CharSummary(..))

-- $entrypoints
--
-- Deliberately /not/ one Haskell wrapper per 'Definition'. The definitions
-- below are the ground truth -- what 'defaultLibraryOrder' registers, what
-- a project overrides by name, what every other definition reaches by
-- name. A wrapper adds nothing to that; it exists only for a Haskell
-- caller wanting to invoke one definition directly.
--
-- There used to be a wrapper for all sixteen, and nine of them had no
-- caller anywhere. That was not free. Every wrapper went through a
-- @runDefinition@ that built the branch's whole readable tree as the
-- ambient scope before the body ran, so each declared a content-read
-- capability whether or not its definition ever read anything.
-- @chapterEntryCompressed@ (@f: "## Chapter: %f%" > (f |
-- summarized("prose"))@) declared one despite never reading a file, and
-- @identity@ (@a: a@) declared one despite touching nothing at all --
-- while 'characterSummaryOf', the most-called function here, needs no
-- capability whatsoever, because it works on a 'Value' it was handed
-- instead of conjuring a scope. The constraint tracked "went through
-- @runDefinition@", not "reads content", and spread across the module for
-- that reason alone.
--
-- So the scope is now an ordinary argument and every entry point here
-- needs nothing but 'Fail'. Reading story content is
-- 'Runix.FileSystem.FileSystemRead', which happens in exactly one place:
-- 'Storyteller.Context.DSL.Compile.currentScope' (plus the @branch@ host
-- binding, for a cross-branch @in@). Not here, and not in fifty
-- signatures.

-- | The one reserved standing-instruction file, if a project has one --
--   mirrors 'Storyteller.Writer.Agent.WorldContext.isSystemContextPath',
--   but as a plain glob, not a predicate: a project keeping its style
--   guide somewhere else just overrides this one definition.
--
--   Quoted via 'defQuote' rather than 'dsl' -- like every other
--   library-registered definition below -- so the same parsed
--   'Definition' backs both this ordinary Haskell value (via
--   'runDefinition') and 'defaultLibrarySource''s entry, with no second,
--   runtime-parsed copy of the source text.
contextStyleDef :: Definition
contextStyleDef = [defQuote|
read "style.md" | orifempty ""
|]

-- | Describes one lore (or "other") entry -- a header naming it, then its
--   content, still role-undecided (see 'read''s own convention).
--   Referenced by plain name from 'contextLore''s\/'contextOther''s own
--   bodies (@loreEntry f@), not threaded in as a parameter: that only
--   works because 'loreEntryDef' is *also* registered in
--   'defaultLibraryOrder', so the compile-time library table resolves the
--   name the same way it would resolve a project's own override -- see
--   'defaultLibraryOrder''s own Haddock for the ordering this depends on.
loreEntryDef :: Definition
loreEntryDef = [defQuote|
f:
  "## %f%"
  read f
|]

-- | Hand-authored lore -- a plain positive convention (@lore\/**@), not
--   "everything except chapters/style/scratch": 'exclude'\/'without'\/
--   'only' can only neuter a key's *content* to 'emptyValue', never
--   actually shrink 'valueEntries'' own key set (their arguments are
--   themselves DSL 'Value's, not knowable without forcing inside an
--   'Action', and a filter has to stay pure/synchronous -- see their own
--   haddocks) -- so an "everything except..." definition re-exported
--   through a second @for@ (exactly what 'contextOther' needs) would
--   resurrect every "excluded" path as an empty-content entry instead of
--   dropping it. A plain positive glob has no such hazard: nothing here
--   ever needs to un-match a key that was never matched in the first
--   place. A project without a @lore\/@ directory gets nothing from this
--   default until it writes its own convention -- consistent with
--   "override, don't guess," not a gap.
--
--   Self-describing (a "## Story background" heading) *and* keeps
--   per-file entries -- both, not one or the other: the entries exist so
--   'contextOther''s own @exclude(context.lore, ...)@ can match this
--   definition's key set (an @exclude@ argument's criteria come from
--   'valueEntries'' own keys, never from a forced default -- see
--   'Storyteller.Context.DSL.Compile.argCriteria'), and the default
--   exists so referencing @context.lore@ (as 'contextWriter' does)
--   gives the whole, honest "what is this" description rather than
--   nothing. @x = loreEntry f@ binds each entry's own recipe once and
--   reuses the same reference for both the @as@-export and the bare
--   re-emit, rather than writing @loreEntry f@ twice.
--
--   Registered under *only* the dotted name @context.lore@ (like
--   @character.blurb@ -- see its own haddock for why a second, bare-name
--   alias pointing at the same 'Definition' would be a real, separate
--   gap, not a convenience): every other definition referencing this one
--   ('contextOther', 'contextWriter') does so by @context.lore@, so a
--   project's own override actually reaches them, instead of the bug this
--   whole module exists to avoid repeating.
contextLoreDef :: Definition
contextLoreDef = [defQuote|
"## Story background"
for f in lore/**/*:
  x = loreEntry f
  as f: x
  x
|]

contextLore :: forall r. Member Fail r => Value r -> Library r -> Action r (Value r)
contextLore scope lib = compileDefinition lib contextLoreDef scope []

-- | 'contextLoreDef', minus one path -- what 'contextWriterDef' actually
--   calls, so the file currently being written never shows up twice (once
--   here, once as 'Storyteller.Writer.Agent.Write.writeAgent''s own
--   current-file framing). Calls @context.lore@ by name first (so a
--   project's own override is seen, same discipline every other
--   composition in this module follows -- see 'contextWriterDef''s own
--   Haddock on the bug a bare-alias shortcut here would reopen), then
--   walks its entries excluding @path@, the same @for f in **\/*@
--   reflatten 'contextChaptersWithoutDef' needs for the identical reason.
--
--   __Emits its own @"## Story background"@ banner unconditionally__,
--   rather than relying on @context.lore@'s own default to carry one
--   through: the reflatten below only ever sees @valueEntries@, never a
--   source's own @valueDefault@ (where @contextLoreDef@'s banner actually
--   lives), so nothing would announce this section at all otherwise --
--   the model would just see file content with no framing for what it's
--   looking at. Restating the *label* here (not the content) is a small,
--   deliberate exception to this module's usual "never restate another
--   definition's own text" discipline -- every result of this walk needs a
--   description one way or another, and there's no other honest place
--   left to put it once the wrap discards the source's own default.
--
--   __On a bare, entry-less override__: @for@ only ever walks
--   @valueEntries@, so an override with no per-file structure of its own
--   (a plain string, say) produces nothing beyond the banner here, not
--   "the override minus one file." This is the honest answer, not a
--   defect to work around -- an override that never exposed file
--   boundaries has nothing for "exclude one file" to mean in the first
--   place; falling back to some other behavior would be guessing on the
--   override author's behalf. A project wanting @context.lore@ overridden
--   *and* path-exclusion-aware writes its override with real per-file
--   entries (@for@\/@as@), the same shape 'contextLoreDef' itself uses.
contextLoreWithoutDef :: Definition
contextLoreWithoutDef = [defQuote|
path:
  "## Story background"
  in (context.lore | exclude(path)): read **/*
|]

-- | Describes one chapter -- a @User@ header immediately followed by its
--   content re-tagged @Assistant@ (@> read f@, per
--   'Storyteller.Context.DSL.AST.EAssistant''s own haddock) -- the exact
--   prior-turn framing @Storyteller.Writer.Agent.Write.
--   buildChapterMessages@ used to hand-construct in Haskell for "earlier
--   chapters," now built once here. Registered in 'defaultLibrarySource'
--   the same way 'loreEntry' is, for the same reason (not referenced by
--   name from anywhere yet, but kept consistent with 'loreEntry' as its
--   own named unit rather than inlined, so a project can override "how
--   one chapter is described" independently of 'contextChapters' as a
--   whole).
chapterEntryDef :: Definition
chapterEntryDef = [defQuote|
f:
  "## Chapter: %f%"
  > read f
|]

-- | 'chapterEntryDef', but the body read through @summarized(f, "prose")@
--   instead of @read f@ -- a separate definition, not a parameterized
--   variant of 'chapterEntryDef', because @f@ there is bound to the bare
--   path itself (interpolated verbatim into the @"## Chapter: %f%"@
--   header, then re-read inside the body) -- there's no path expression
--   this could thread through 'chapterEntry''s existing 'Action'
--   parameter without breaking that interpolation.
chapterEntryCompressedDef :: Definition
chapterEntryCompressedDef = [defQuote|
f:
  "## Chapter: %f%"
  > (f | summarized("prose"))
|]

-- | Chapter prose, in natural reading order (@ch2@ before @ch11@, not
--   @ch11@ before @ch2@) -- 'sortBy''s reordering now survives the
--   re-export through a second glob (see
--   'Storyteller.Context.DSL.Compile.globMatchPat''s own haddock for why
--   that used to silently undo it). The @in (x | sortBy): for f in
--   **/*@ shape stays, deliberately, even though @for@ can now iterate
--   any expression directly (see
--   'Storyteller.Context.DSL.AST.SFor''s own haddock) -- unlike
--   'contextOther''s filtered @**/*@ (whose surviving entries are the
--   *same* underlying tree reads either way, just narrowed), @x@'s own
--   entries are @chapterEntry f@'s already-synthesized result, not raw
--   file content -- @read f@ inside the loop has to resolve against @x@
--   itself to see that synthesized entry, so @in@'s scope-repositioning
--   is load-bearing here, not just a source of iteration keys. Self-
--   describing and entry-keeping, same reasoning and same @x = ...; as f:
--   x; x@ shape as 'contextLore'.
contextChaptersDef :: Definition
contextChaptersDef = [defQuote|
x =
  for f in chapters/**/*:
    as f: chapterEntry f
"## Chapters written so far"
in (x | sortBy):
  for f in **/*:
    y = read f
    as f: y
    y
|]

contextChapters :: forall r. Member Fail r => Value r -> Library r -> Action r (Value r)
contextChapters scope lib = compileDefinition lib contextChaptersDef scope []

-- | 'contextChaptersDef', minus one path -- 'contextLoreWithout''s own
--   twin, same reasoning throughout (see its Haddock): calls
--   @context.chapters@ by name so a project's own override is seen, walks
--   its entries excluding @path@ via a bare multi-match @read@ (equivalent
--   to, and simpler than, an explicit @for f in **\/*: read f@ -- see
--   CONTEXT-DSL.md's own worked example on @read@ over a glob), and emits
--   its own @"## Chapters written so far"@ banner unconditionally, for the
--   identical reason 'contextLoreWithoutDef' emits its own: the reflatten
--   only ever sees entries, never @context.chapters@'s own default, where
--   that banner actually lives.
contextChaptersWithoutDef :: Definition
contextChaptersWithoutDef = [defQuote|
path:
  "## Chapters written so far"
  in (context.chapters | exclude(path)): read **/*
|]

-- | 'contextChaptersDef', but each chapter's body read through
--   @summarized(f, "prose")@ instead of @read f@ -- the
--   "compressed past chapters" mode 'Server.Writer.File.chatWriter' picks
--   via its own @pastChaptersMode@ wire field (see that module's own
--   Haddock: a toggle, not a client-authorable program, since "should
--   chapter history be full or compressed" is a shape decision, not a
--   content-selection one the caller has any special knowledge over).
--   Kept as its own sibling definition rather than a parameter on
--   'contextChaptersDef' itself -- overriding "how one chapter reads" and
--   "whether chapters are summarized at all" are two independent axes a
--   project might want to override separately, the same reasoning
--   'chapterEntry' already being its own named unit rests on.
contextChaptersCompressedDef :: Definition
contextChaptersCompressedDef = [defQuote|
x =
  for f in chapters/**/*:
    as f: chapterEntryCompressed f
"## Chapters written so far (compressed)"
in (x | sortBy):
  for f in **/*:
    y = read f
    as f: y
    y
|]

--   Declares no capability beyond 'Fail', despite the compression:
--   @summarized@ is reached through @chapterEntryCompressed@'s own entry in
--   @lib@, so the summary-reading capability
--   lives in the 'Library' this is handed (where 'hostLibrary' establishes
--   it), and the file reading lives in @scope@, which is already-built
--   data. Restating either here would describe this function by what its
--   arguments happen to contain rather than by what it does.
contextChaptersCompressed :: forall r. Member Fail r => Value r -> Library r -> Action r (Value r)
contextChaptersCompressed scope lib = compileDefinition lib contextChaptersCompressedDef scope []

-- | The catch-all: any file that isn't under @lore@\/@chapters@' own
--   convention, or @style.md@, or the @chat/**@ scratch convention, or
--   @path@ (the file a caller is about to write to -- dropped so a query
--   never shows a file to itself as if it were already-existing prior
--   content). Built directly from @context.lore@\/@context.chapters@'s
--   own key sets via @exclude@ -- referenced *by dotted name*, not
--   threaded in as parameters and not by their bare aliases either (see
--   'contextLoreDef''s own haddock: a bare reference here would mean a
--   project's own @context.lore@\/@context.chapters@ override silently
--   never reaches this exclusion, the exact bug this whole module exists
--   to close), so this stays correct even if a project overrides either
--   one independently, without 'contextOther' itself needing to change --
--   not by restating "not lore\/**, not chapters\/**" as a second pattern
--   list that could drift out of sync with their own definitions. Reuses
--   'loreEntry' for the same per-file framing lore gets: a stray file is
--   "just another entry," described the same way.
--
--   @path@ is 'contextOther''s only real parameter -- everything else it
--   needs (@context.lore@, @context.chapters@) it resolves itself,
--   through the shared library, the same way it would honor an override
--   of either.
contextOtherDef :: Definition
contextOtherDef = [defQuote|
path:
  "## Other notes"
  for f in (**/* | exclude(context.lore, context.chapters, "style.md") | exclude("chat/**/*") | exclude(path)):
    x = loreEntry f
    as f: x
    x
|]

contextOther :: forall r. Member Fail r => Value r -> Library r -> Text -> Action r (Value r)
contextOther scope lib p = compileDefinition lib contextOtherDef scope [toBinding p]

-- | The writer agent's own default background context -- what
--   'Server.Writer.File.chatWriter' resolves (branch-override-then-this)
--   when a request carries no context of its own, and what
--   'Storyteller.Writer.Agent.Roleplay.roleplayWriter'\/
--   'chatChapterRegen'\/'chatSplitOutline' and the CLI tools always use
--   (they never take a per-request override). One flat, ordered,
--   self-describing stream -- lore, then whatever's already been written
--   (minus the file about to be written), then everything else, then
--   who's actually here -- not a record of separately-picked buckets:
--   forcing this @Value@'s own default *is* "the context for this call,"
--   honestly, whether this compiled-in body answered it or a project's\/
--   client's own override did (see the project chat that settled this: a
--   context a caller submits has to mean "whatever this writes is what
--   the LLM sees," not something this module quietly reinterprets by
--   picking named entries apart).
--
--   Style is deliberately absent -- it was never "context" (facts about
--   the story) at all, only an instruction about voice, so it stays its
--   own separate lookup (@context.style@) wherever an agent wants it,
--   completely independent of whether this definition or a client's own
--   program produced the stream above.
--
--   The trailing @for c in (charactersin path): x = context.character c;
--   as c: x; x@ is what's meant to replace
--   'Server.Writer.File.activeCharacterContext' -- a Haskell-side loop
--   splicing @[(CharLabel, CharSummary)]@ into 'writeAgent''s own
--   parameter list after this definition had already been resolved. That
--   was exactly the shape this whole redesign was for: context the DSL
--   couldn't see or override, folded in one layer up instead of being
--   *part of* "the context for this call." @x = ...; as c: x; x@ (not a
--   bare @as@, which used to leave this loop contributing nothing to the
--   flat default at all -- @SFor@'s own entries never feed the enclosing
--   block's own default, only its own re-emitted messages would; see
--   'runStmts''s @SFor@ case) both names each active character's whole
--   'context.character' 'Value' (@"sheet"@\/@"blurb"@\/@"full"@\/
--   @"journal"@\/@"journalFull"@, see its own haddock) for a caller
--   reaching in by name, *and* genuinely emits their acquaintance-level
--   blurb into the flat stream every @flatMainMessages@\/@flatMainContext@
--   caller ('roleplayWriter'\/'chatChapterRegen'\/'chatSplitOutline'\/the
--   CLI tools) already reads directly.
--
--   __Known, temporary overlap__: 'Server.Writer.File.chatWriter' still
--   builds @[(CharLabel, CharSummary)]@ itself via 'activeCharacterContext'
--   and threads it through 'writeAgent' as its own separate parameter
--   (spliced at a specific position relative to conversation history --
--   see 'Storyteller.Writer.Agent.Write.buildChapterMessages''s own
--   Haddock -- not something this flat stream's own ordering can express
--   yet), so as of this change 'chatWriter' specifically sees each active
--   character's blurb *twice*: once here, folded into @worldCtx@, and
--   once via @charBlocks@. Retiring 'activeCharacterContext' needs the
--   DSL to first gain some way to express "splice this at a specific
--   message position, interleaved with reconstructed conversation
--   history" -- real design work, not done here; this change only carries
--   the flat-stream side of the migration through for the callers that
--   already read nothing but that stream.
--
--   @path@ is this definition's only real parameter, for the same reason
--   it's 'contextOther''s: everything else is a fact about the branch,
--   resolved through the shared library. Calls @context.loreWithout@\/
--   @context.chaptersWithout@ -- not @context.lore@\/@context.chapters@
--   wrapped at this call site -- to drop @path@ from each: the file
--   currently being written must never appear in either section, since
--   'Storyteller.Writer.Agent.Write.writeAgent' already frames it
--   separately as the file being continued (via 'Storage.Tick.fileTicksOf'
--   in its own current-file history). An earlier version of this fix tried
--   wrapping the already-resolved @context.lore@\/@context.chapters@
--   values inline (@in (context.lore | exclude(path)): for f in **\/*:
--   read f@) -- that silently discarded a project's own override of
--   either name whenever the override had no per-file entries of its own
--   (a bare custom string, say): @for@ only ever walks @valueEntries@,
--   never a source's own @valueDefault@, so an entry-less value's entire
--   content vanished. 'contextLoreWithoutDef'\/'contextChaptersWithoutDef'
--   build their own entries directly off the glob instead (@exclude@
--   applied before 'loreEntry'\/'chapterEntry' ever run), so there is no
--   opaque already-resolved 'Value' to misjudge -- see their own Haddocks.
--
--   References @context.loreWithout@\/@context.chaptersWithout@\/
--   @context.other@ by their dotted names, not the bare aliases those
--   definitions used to be reachable under too -- this used to be the one
--   real, confirmed gap 'characterBlurb''s own haddock called out: a
--   project committing an override to any of those three, however
--   correctly, was silently never seen by this composition, the identical
--   bug @character.blurb@ had before it got the same fix. Closing it here
--   meant dropping the bare aliases from 'defaultLibrarySource' entirely,
--   not just adding the dotted reference alongside the old one -- two keys
--   pointing at one 'Definition' don't move together under a single
--   override (see 'contextCharacterDef''s own haddock on why), so leaving
--   a bare alias registered would have left the same silent-miss risk
--   sitting one key away.
contextWriterDef :: Definition
contextWriterDef = [defQuote|
path:
  context.loreWithout path
  context.chaptersWithout path
  context.other path
  for c in (charactersin path):
    x = context.character c
    as c: x
    x
|]

contextWriter :: forall r. Member Fail r => Value r -> Library r -> Text -> Action r (Value r)
contextWriter scope lib p = compileDefinition lib contextWriterDef scope [toBinding p]

-- | The starting point for a user-defined agent
--   ('Storyteller.Writer.Agent.Custom.customAgent'): everything
--   'Storyteller.Writer.Agent.Write.writeAgent' assembles for itself,
--   written out in the DSL instead of in Haskell.
--
--   __Deliberately spelled out rather than delegated to @context.writer@__,
--   which is what it originally was. A one-line body would have been the
--   same context, but a starting template's job is to be /read/: someone
--   opening their first agent's program should be able to see that the
--   style guide, world lore, past chapters, loose notes, present
--   characters and this file's own conversation are six separate,
--   individually removable decisions -- and delete or reorder any one of
--   them -- without first having to know that @context.writer@ exists and
--   go read what it expands to. Each line names exactly what it
--   contributes, which is the only self-documentation available here
--   (comments parse but don't survive
--   'Storyteller.Context.DSL.PrettyPrint.prettyDefinition', and this
--   definition is served /as pretty-printed source/ to seed a new agent's
--   file -- see below).
--
--   Line by line, matching @writeAgent@'s own numbered Haddock list:
--
--     * @context.style@ -- the standing style guide. Note this is the one
--       piece @context.writer@ itself does /not/ carry: @writeAgent@
--       splices style into its own system prompt rather than its context
--       ('Storyteller.Writer.Agent.Write.writeAgent'), and a custom agent
--       has no such compiled-in splice, so leaving it to @context.writer@
--       would have silently dropped the project's style guide from every
--       user-defined agent.
--     * @context.loreWithout path@ \/ @context.chaptersWithout path@ \/
--       @context.other path@ -- hand-authored lore, every chapter written
--       so far, and loose notes, each with @path@ itself excluded so the
--       file being written never appears twice (once as background, once
--       as the conversation below).
--     * the @for@ loop -- every character present in this scene
--       (@charactersin@ reads presence ticks, the same sole source of
--       truth 'Storyteller.Writer.Presence.activeCharactersFor' uses).
--     * @readconversation path@ -- the volatile tail: this file's own tick
--       history replayed as real user\/assistant turns, the exact
--       @"prompt"@\/@"atom"@ pairing @writeAgent@ reconstructs internally
--       via 'Storyteller.Writer.Agent.Chat.historyFromFileTicks'.
--
--   The one piece of @writeAgent@ deliberately /not/ mirrored here is its
--   mid-depth splice ('Storyteller.Writer.Agent.MessageWindow.injectAtWindow',
--   available in the DSL as @embedshallow@): that's a prompt-cache tuning
--   decision, not a statement about what a story needs, and putting it in
--   a starter template would ask someone to understand cache-prefix
--   arithmetic before making their first edit. An agent that wants it
--   writes @embedshallow (readconversation path) extra@ in place of the
--   bare read -- which is exactly the shape of the line it replaces.
--
--   Registered as an ordinary library name (@context.custom@ -- the
--   namespace root of every @context.custom.\<slug\>@ a project commits,
--   the same root-is-the-default convention 'Storyteller.Core.Prompt' uses
--   for prompt keys), for two reasons: a project's own agent program can
--   reference it directly (@path: context.custom path@, then narrow), and
--   the frontend seeds a newly created agent's @.dsl@ by fetching this
--   definition's real, pretty-printed source over
--   @GET \/context-default\/context.custom@ -- so the starting template a
--   user sees is this definition, never a hand-kept copy of it in
--   TypeScript that could quietly drift.
contextCustomDef :: Definition
contextCustomDef = [defQuote|
path:
  context.style
  context.loreWithout path
  context.chaptersWithout path
  context.other path
  for c in (charactersin path):
    context.character c
  readconversation path
|]

-- | The "and this is the character" acquaintance-level line -- the
--   header @sheet.md@ is required to open with (its display name, see
--   @WRITER.md@), plus whatever paragraph follows it, by convention
--   rather than an LLM call (see the project chat that designed this,
--   2026-07-20: "already stored data", not content analysis). Its own
--   named definition (@character.blurb@), registered in
--   'defaultLibrarySource' under its one dotted name (see below for why
--   only one), what a project override addresses and what
--   'contextCharacter' itself calls, so a project can override just this
--   one definition independently of the richer buckets around it.
--
--   This used to be threaded into 'contextCharacter' as a typed
--   'Binding' parameter instead of referenced by name -- which meant a
--   project's own @character.blurb@ override, however correctly
--   committed, was never actually seen by 'contextCharacter''s
--   composition: the override machinery updated
--   'Storyteller.Context.DSL.Value.ContextLibrary''s entry for the name
--   @character.blurb@, but 'contextCharacterDefault' wired in the
--   compiled-in Haskell closure directly, so nothing ever asked the
--   library about it.
--
--   Registered under *only* the dotted name @character.blurb@ -- not
--   also a separate bare alias the way @loreEntry@\/@contextLore@ are --
--   and 'contextCharacter''s own body below references it by that exact
--   dotted identifier (identifiers may contain interior dots, precisely
--   for this: see "Storyteller.Context.DSL.Parser"'s own concrete-syntax
--   notes). One key, not two aliasing the same 'Definition', is what
--   actually closes the bug: a project's override is committed under the
--   dotted path-derived name (@contexts/character/blurb.dsl@ ->
--   @character.blurb@), and a bare alias pointing at the same
--   'Definition' would only receive an override committed under *that*
--   separate key -- 'Storyteller.Core.Context.buildContextLibrary'\'s
--   'Data.Map.Strict.mapWithKey'-based override application checks each
--   key in 'defaultLibrarySource' independently, so two keys for one
--   definition do not move together under a single override. (This same
--   shape of bug used to sit right next to this fix, unfixed:
--   @contextWriter@'s own body referenced @contextLore@\/@contextChapters@\/
--   @contextOther@ by their bare aliases rather than
--   @context.lore@\/@context.chapters@\/@context.other@, so an override of
--   any of those three didn't reach @contextWriter@'s composition either.
--   Closed the same way -- see 'contextWriterDef''s own haddock -- so
--   'defaultLibrarySource' no longer has bare aliases for any of the
--   three at all, only their dotted names.)
--
--   Takes @charname@ and crosses to that branch itself (@in (charname |
--   branch): ...@), the same as 'contextCharacter''s own @"sheet"@
--   bucket -- it can't rely on a caller's enclosing @in@ instead, for
--   the same reason 'Storyteller.Context.DSL.Compile.journalDelta'
--   can't: there's no dynamic-scope crossing between two separately
--   compiled 'Storyteller.Context.DSL.AST.Definition's, only within one
--   definition's own body. A definition invoked from another's body has
--   to be self-contained about which branch it reads from.
characterBlurbDef :: Definition
characterBlurbDef = [defQuote|
charname:
  in (charname | branch):
    n = read "sheet.md" | name
    a = read "sheet.md" | abstract
    "%n%: %a%"
|]

-- | A named character's rich context, as five independently reachable
--   buckets rather than one flattened blob -- every consumer
--   ('Storyteller.Writer.Agent.AskCharacter.askCharacterAgent',
--   'Storyteller.Writer.Agent.Roleplay.roleplayAgent', ambient scene
--   generation) shares this one definition and picks the buckets it
--   actually wants, the same way 'contextMain''s own
--   @"lore"@\/@"chapters"@\/@"other"@\/@"style"@ split lets
--   'Storyteller.Writer.Agent.Write.writeAgent' place each independently
--   rather than re-deriving its own notion of "a character's context"
--   per call site (see the project chat that designed this, 2026-07-20).
--
--   * @"sheet"@ -- @sheet.md@ verbatim.
--   * @"blurb"@ -- @character.blurb charname@, referenced by its own
--     dotted name directly (see 'characterBlurb''s own haddock for why
--     this, not a typed parameter or a separate bare alias, is what makes
--     a project's override actually reach every caller).
--   * @"full"@ -- every other file on the character's branch.
--   * @"journal"@ -- 'Storyteller.Context.DSL.Compile.journalDelta',
--     also threaded in as a parameter (a host-supplied 'Binding', not
--     expressible in the DSL itself -- see that function's own haddock
--     for why @in (charname | branch): ...@ alone can't put it on the
--     right branch).
--   * @"journalFull"@ -- @journal.md@ verbatim, uncurated. Together with
--     @"sheet"@\/@"full"@ this is exactly what
--     'Storyteller.Writer.Agent.CharContext.charSummaryFull' builds today
--     for 'askCharacterAgent'\/'roleplayAgent' (a present character's own
--     full self-knowledge, not the ambient-context curation @"journal"@
--     is for) -- included so those two can eventually read through this
--     one definition too, instead of their own separate calls. Costs
--     nothing when a caller never reaches for it: 'Value''s own entries
--     are @Action@s, not already-run results (see @CONTEXT-DSL.md@'s
--     "Value model"), so an unread bucket never resolves the branch or
--     touches storage at all.
--
--   The bare trailing statement re-emits @blurb charname@ as this whole
--   definition's own default: a caller that never picks a bucket (takes
--   the default, or does @in characterContext: read "blurb"@-shaped
--   access without narrowing further) still gets a reasonable
--   "and this is the character" line, per the project chat's own framing
--   ("read \"blurb\" is probably a good default").
--
--   Registered under the dotted name @context.character@ in
--   'defaultLibrarySource' (like @context.writer@\/@context.lore@, not
--   like the old, Haskell-parameter-threaded shape) -- @journal@ used to
--   be a typed 'Binding' parameter (baked in by a separate
--   @contextCharacterDefault@ wrapper), the exact same shape that made
--   @character.blurb@'s own override silently invisible to this
--   definition's composition. It's now @characterJournal@, a bare-name
--   reference to the pre-configured host binding in 'hostLibrary' -- the
--   same fix @character.blurb@ already got, one level up. There is no
--   separate @contextCharacterDefault@ any more: this *is* the
--   1-arity, @Text -> Action Value@ shape a wire-level "which character"
--   call site wants, resolved through 'Storyteller.Core.Context.resolveContext1'
--   exactly like @context.writer@ is.
contextCharacterDef :: Definition
contextCharacterDef = [defQuote|
charname:
  as "sheet": in (charname | branch): read "sheet.md" | orifempty ""
  as "blurb": character.blurb charname
  as "full":
    in (charname | branch):
      in (**/* | exclude("sheet.md", "journal.md")):
        for f in **/*:
          as f: read f
  as "journal": characterJournal charname
  as "journalFull": in (charname | branch): read "journal.md" | orifempty ""
  character.blurb charname
|]

--   Takes a @scope@ like its siblings but never consults it: every read in
--   this definition's body sits inside @in (charname | branch): ...@, and
--   the rest are library calls. It still takes one rather than assuming
--   'Storyteller.Context.DSL.Value.emptyValue', because a project's own
--   override of @context.character@ has no such guarantee.
contextCharacter :: forall r. Member Fail r => Value r -> Library r -> Text -> Action r (Value r)
contextCharacter scope lib charname = compileDefinition lib contextCharacterDef scope [toBinding charname]

-- | Reshapes an already-resolved @context.character@-shaped 'Value' into
--   a 'CharSummary' -- the shared piece every consumer wanting that exact
--   shape reaches for ('Server.Writer.File.activeCharacterContext',
--   ambient scene context; 'Storyteller.Writer.Agent.Roleplay.askCharacter'\/
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent', a
--   character's own subagent), rather than picking 'Value' buckets apart
--   at every call site. @journalBucket@ selects which of
--   'contextCharacter''s own two journal readings a caller wants --
--   @"journal"@ (curated via
--   'Storyteller.Context.DSL.Compile.journalDelta') or @"journalFull"@
--   (verbatim) -- see that definition's own Haddock on the pair.
--
--   Deliberately takes the resolved 'Value', not a @charname@ to resolve
--   itself -- resolving @context.character@ (branch override, then this
--   module's own 'contextCharacter' as fallback) is the caller's
--   job, via 'Storyteller.Core.Context.resolveContextQuery'\/
--   'Storyteller.Core.Context.runContextBinding1'. This function used to
--   call 'contextCharacterDefault' directly, which meant a project
--   committing an override to @contexts/context/character.dsl@ was
--   silently ignored by every real caller -- the override machinery
--   existed ('Storyteller.Core.Context' has run the real branch-backed
--   'Storyteller.Core.Context.interpretContextStorageFS' in production
--   since @context.main@ shipped) but nothing ever asked it about
--   @context.character@. Splitting resolution out is what fixes that:
--   'Action' itself has no 'Storyteller.Core.Context.ContextStorage'
--   effect to reach for (it's constrained to
--   'Storyteller.Context.DSL.Value.MonadBranch'\/'Storage.Core.StoreM'
--   only), so the override lookup has to happen at the 'Polysemy.Sem'
--   level, one step up from here.
characterSummaryOf :: Text -> Value r -> Action r CharSummary
characterSummaryOf journalBucket charVal = do
  sheet   <- Render.valueCharBlocks =<< namedEntry "sheet" charVal
  full    <- Render.valueCharBlocks =<< namedEntry "full" charVal
  journal <- Render.valueCharBlocks =<< namedEntry journalBucket charVal
  pure (CharSummary sheet full journal)

-- | Identity pass-through -- every candidate alias stays active for
--   auto-inclusion on mention until a project's own override narrows it
--   (@aliases | without(...)@\/@only(...)@ -- see the project chat that
--   designed this for why @without@\/@only@ alone are enough here and
--   'contextLore''s @exclude@ isn't needed: alias names never nest into
--   subtrees the way file paths do).
contextMentionFilter :: forall r. Members '[FileSystem ContextFS, FileSystemRead ContextFS, Fail] r => Binding r -> Action r (Value r)
contextMentionFilter = [dsl|
aliases:
  in aliases:
    for f in *:
      as f: read f
|]

-- | Re-curries a QQ-spliced 1-arity definition (@'Binding' ->
--   'Action' 'Value'@, per "Storyteller.Context.DSL.QQ") back into the
--   'Binding' shape 'Storyteller.Context.DSL.Compile.EApp' actually calls
--   -- the inverse of what applying a 'Binding' to a QQ-spliced function
--   normally does. @context.mentionFilter@ is the one remaining caller
--   ("Server.Writer.Lore"'s own inline default), resolved directly via
--   'Storyteller.Core.Context.getContextDefinition' rather than through
--   'defaultLibrarySource', since it needs a live per-call @aliases@
--   argument no static registration could hold. Every other definition
--   this application ships lives in 'defaultLibrarySource' now, including
--   @context.character@ (it used to need a similar Haskell-side fallback,
--   closing over 'Storyteller.Context.DSL.Compile.journalDelta', but that
--   moved to a pre-configured 'hostLibrary' entry instead -- see
--   'contextCharacterDef''s own haddock).
toBinding1 :: Member Fail r => (Binding r -> Action r (Value r)) -> Binding r
toBinding1 f = Binding 1 go
  where
    go [a] _  = f (bval a)
    go args _ = fail $ "expected exactly 1 argument, got " <> show (length args)


-- | Every pure-DSL definition this application ships, as already-parsed
--   'Definition's, in a *fixed compile order* --
--   'Storyteller.Core.Context.buildContextLibrary' folds this list
--   left to right (a project's own
--   'Storyteller.Core.Context.contextsBranchName' override replacing a
--   slot's definition, when one exists and matches arity), each slot
--   compiled against only the table built from everything strictly
--   earlier in this list (plus 'hostLibrary', seeded in first) -- never
--   against the finished table as a whole. This is what makes an
--   override's own self-reference resolve to the *previous* binding
--   (or fail to resolve, compile-time, if there wasn't one) rather than
--   looping into itself: see
--   'Storyteller.Context.DSL.Compile.definitionBinding's own Haddock.
--
--   __The ordering is load-bearing project policy, not incidental__: an
--   entry may only reference another library name (by 'EIdent'\/'EApp')
--   if that name is *strictly earlier* in this list, or lives in
--   'hostLibrary'. Referencing something later fails to resolve the
--   first time that slot is compiled ("unknown identifier"), loudly, not
--   silently. Current dependency edges: @loreEntry@\/@chapterEntry@ are
--   called by @context.lore@\/@context.other@\/@context.chapters@;
--   @context.other@\/@context.writer@ call @context.lore@\/
--   @context.chapters@; @context.writer@ calls @context.other@ and
--   @context.character@; @context.character@ calls @character.blurb@ and
--   @characterJournal@ (the latter from 'hostLibrary'). Adding a new
--   default that references an existing one must place it later in this
--   list; adding one two existing defaults should both be able to see
--   requires placing it earlier than both.
--
--   One key per definition (no second copy of the source, unlike this
--   map's own predecessor -- see 'loreEntryDef''s Haddock on why a bare
--   'Definition' rather than text is what makes that possible). Two kinds
--   of key, though, and they're not interchangeable: a plain internal
--   helper with no project-facing identity of its own (@loreEntry@,
--   @chapterEntry@ -- reused by more than one @context.*@ definition, but
--   never itself something a project overrides independently) keeps its
--   bare name only; anything a project\/client can meaningfully override
--   on its own (@context.lore@, @context.chapters@, @context.other@,
--   @context.writer@, @context.style@, @character.blurb@,
--   @context.character@) is registered under *only* its dotted name, and
--   every other definition referencing it does so by that same dotted
--   name -- see 'contextWriterDef''s\/'characterBlurbDef''s own haddocks
--   for the bug a bare alias sitting alongside the dotted one causes
--   (two keys, one 'Definition', don't move together under a single
--   override). What 'resolveContext0'\/'resolveContext1' callers pass is
--   always one of these dotted names. Every definition that *can't* live
--   here -- needs a real Haskell-supplied capability, not expressible as
--   parsed DSL text -- is in 'hostLibrary' instead; see that map's own
--   Haddock for exactly which and why.
defaultLibraryOrder :: [(Name, Definition)]
defaultLibraryOrder =
  [ ("loreEntry",         loreEntryDef)
  , ("context.lore",      contextLoreDef)
  , ("context.loreWithout", contextLoreWithoutDef)  -- needs loreEntry (above)
  , ("chapterEntry",      chapterEntryDef)
  , ("context.chapters",  contextChaptersDef)
  , ("context.chaptersWithout", contextChaptersWithoutDef)  -- needs chapterEntry (above)
  , ("chapterEntryCompressed",     chapterEntryCompressedDef)
  , ("context.chaptersCompressed", contextChaptersCompressedDef)
  , ("context.other",     contextOtherDef)
  , ("context.style",     contextStyleDef)
  , ("character.blurb",   characterBlurbDef)
  , ("context.character", contextCharacterDef)  -- needs character.blurb (above), characterJournal (hostLibrary)
  , ("context.writer",    contextWriterDef)     -- needs context.loreWithout/chaptersWithout/other/character (all above)
  , ("context.custom",    contextCustomDef)     -- needs context.writer (above), readconversation (hostLibrary)
  ]

-- | 'defaultLibraryOrder', as the plain 'Map' shape callers that only care
--   about "is there a compiled-in default for this name" want (arity
--   checks, 'Map.difference' against a project's own overrides) --
--   derived, never hand-duplicated, so the two can't drift apart.
defaultLibrarySource :: Map Name Definition
defaultLibrarySource = Map.fromList defaultLibraryOrder
