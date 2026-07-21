{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
--   parameter passing -- a body referencing @contextLore@ by bare name
--   resolves against the shared 'Storyteller.Context.DSL.Value.ContextLibrary'
--   (see 'Storyteller.Context.DSL.Compile.resolveIdent'), the identical
--   way whether the current name means the compiled-in default or a
--   project's own committed override. Only a genuinely host-backed
--   capability (@journalDelta@'s Haskell-level curried tuning, say) still
--   needs Haskell-side parameter passing -- see 'contextCharacter''s own
--   @journal@ parameter -- because that's real per-caller parametricity,
--   not a shared default a project should be able to replace by name (see
--   'characterBlurb''s own haddock for the case that used to be
--   parameter-passed for no good reason, and the bug that came from it).
module Storyteller.Context.DSL.Library
  ( defaultLibrary
  , contextStyle
  , loreEntry
  , contextLore
  , chapterEntry
  , contextChapters
  , contextOther
  , contextWriter
  , contextCharacter
  , contextCharacterDefault
  , characterBlurb
  , characterSummaryOf
  , contextMentionFilter
  , toBinding1
  , identity
  , defaultLibrarySource
  , hostLibrary
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Storyteller.Context.DSL.AST (Definition, Name)
import Storyteller.Context.DSL.Compile (Binding(..), branchBinding, bval, charactersInBinding, embedShallow, journalDelta, readConversation, runDefinition)
import Storyteller.Context.DSL.Context (toBinding)
import Storyteller.Context.DSL.QQ (defQuote, dsl)
import Storyteller.Context.DSL.Value (Action, Value, namedEntry)
import qualified Storyteller.Context.DSL.Render as Render
import Storyteller.Writer.Agent (CharSummary(..))

-- | Host-backed library entries -- real Haskell closures, never
--   expressible as parsed DSL text, so they can never be branch-
--   overridden (see 'Storyteller.Context.DSL.Value.ContextLibrary''s own
--   Haddock). Merged into the shared library alongside
--   'defaultLibrarySource' by 'Storyteller.Core.Context.buildContextLibrary',
--   resolved the identical way by 'Storyteller.Context.DSL.Compile's
--   'EIdent'\/'EApp' -- a DSL body referencing @readconversation@ can't
--   tell it apart from a bare reference to @lore@.
hostLibrary :: Map Name Binding
hostLibrary = Map.fromList
  [ ("readconversation", readConversation)
  , ("embedshallow",     embedShallow)
  , ("branch",           branchBinding)
  , ("charactersin",     charactersInBinding)
  ]

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

contextStyle :: Action Value
contextStyle = runDefinition contextStyleDef []

-- | Describes one lore (or "other") entry -- a header naming it, then its
--   content, still role-undecided (see 'read''s own convention).
--   Referenced by plain name from 'contextLore''s\/'contextOther''s own
--   bodies (@loreEntry f@), not threaded in as a parameter: that only
--   works because 'loreEntryDef' is *also* registered in
--   'defaultLibrarySource', so the shared library table
--   ('Storyteller.Context.DSL.Value.ContextLibrary') resolves the name at
--   runtime the same way it would resolve a project's own override.
loreEntryDef :: Definition
loreEntryDef = [defQuote|
f:
  "## %f%"
  read f
|]

loreEntry :: Action Value -> Action Value
loreEntry a = runDefinition loreEntryDef [toBinding a]

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
--   'contextOther''s own @exclude(contextLore, ...)@ can match this
--   definition's key set (an @exclude@ argument's criteria come from
--   'valueEntries'' own keys, never from a forced default -- see
--   'Storyteller.Context.DSL.Compile.argCriteria'), and the default
--   exists so referencing @contextLore@ bare (as 'contextWriter' does)
--   gives the whole, honest "what is this" description rather than
--   nothing. @x = loreEntry f@ binds each entry's own recipe once and
--   reuses the same reference for both the @as@-export and the bare
--   re-emit, rather than writing @loreEntry f@ twice.
contextLoreDef :: Definition
contextLoreDef = [defQuote|
"## Story background"
for f in lore/**/*:
  x = loreEntry f
  as f: x
  x
|]

contextLore :: Action Value
contextLore = runDefinition contextLoreDef []

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

chapterEntry :: Action Value -> Action Value
chapterEntry a = runDefinition chapterEntryDef [toBinding a]

-- | Chapter prose, in natural reading order (@ch2@ before @ch11@, not
--   @ch11@ before @ch2@) -- 'sortBy''s reordering now survives the
--   re-export through a second glob (see
--   'Storyteller.Context.DSL.Compile.globMatchPat''s own haddock for why
--   that used to silently undo it). Self-describing and entry-keeping,
--   same reasoning and same @x = ...; as f: x; x@ shape as 'contextLore'.
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

contextChapters :: Action Value
contextChapters = runDefinition contextChaptersDef []

-- | The catch-all: any file that isn't under @lore@\/@chapters@' own
--   convention, or @style.md@, or the @chat/**@ scratch convention, or
--   @path@ (the file a caller is about to write to -- dropped so a query
--   never shows a file to itself as if it were already-existing prior
--   content). Built directly from @contextLore@\/@contextChapters@'s own
--   key sets via @exclude@ -- referenced *by name*, not threaded in as
--   parameters, so this stays correct even if a project overrides either
--   one independently, without 'contextOther' itself needing to change --
--   not by restating "not lore\/**, not chapters\/**" as a second pattern
--   list that could drift out of sync with their own definitions. Reuses
--   'loreEntry' for the same per-file framing lore gets: a stray file is
--   "just another entry," described the same way.
--
--   @path@ is 'contextOther''s only real parameter -- everything else it
--   needs (@contextLore@, @contextChapters@) it resolves itself, through
--   the shared library, the same way it would honor an override of
--   either.
contextOtherDef :: Definition
contextOtherDef = [defQuote|
path:
  "## Other notes"
  in (**/* | exclude(contextLore, contextChapters, "style.md") | exclude("chat/**/*") | exclude(path)):
    for f in **/*:
      x = loreEntry f
      as f: x
      x
|]

contextOther :: Text -> Action Value
contextOther p = runDefinition contextOtherDef [toBinding p]

-- | The writer agent's own default background context -- what
--   'Server.Writer.File.chatWriter' resolves (branch-override-then-this)
--   when a request carries no context of its own, and what
--   'Storyteller.Writer.Agent.Roleplay.roleplayWriter'\/
--   'chatChapterRegen'\/'chatSplitOutline' and the CLI tools always use
--   (they never take a per-request override). One flat, ordered,
--   self-describing stream -- lore, then whatever's already been written
--   (minus the file about to be written), then everything else -- not a
--   record of separately-picked buckets: forcing this @Value@'s own
--   default *is* "the context for this call," honestly, whether this
--   compiled-in body answered it or a project's\/client's own override
--   did (see the project chat that settled this: a context a caller
--   submits has to mean "whatever this writes is what the LLM sees," not
--   something this module quietly reinterprets by picking named entries
--   apart).
--
--   Style is deliberately absent -- it was never "context" (facts about
--   the story) at all, only an instruction about voice, so it stays its
--   own separate lookup (@context.style@) wherever an agent wants it,
--   completely independent of whether this definition or a client's own
--   program produced the stream above.
--
--   @path@ is this definition's only real parameter, for the same reason
--   it's 'contextOther''s: everything else is a fact about the branch,
--   resolved through the shared library. Excluding @path@ from
--   @contextChapters@ needs the walk-and-reflatten form (an @exclude@
--   only shrinks entries, so it can't act on 'contextChapters''s own
--   already-built default) -- deliberately *not* repeating
--   @contextChapters@'s own @"## Chapters written so far"@ banner here:
--   that text belongs to @contextChapters@, and restating it would be
--   exactly the drift risk this whole module exists to avoid. The
--   transition from @contextLore@'s own heading into each chapter's own
--   @"## Chapter: %f%"@ header is enough structure on its own.
contextWriterDef :: Definition
contextWriterDef = [defQuote|
path:
  contextLore
  in (contextChapters | exclude(path)):
    for f in **/*: read f
  contextOther path
|]

contextWriter :: Text -> Action Value
contextWriter p = runDefinition contextWriterDef [toBinding p]

-- | The "and this is the character" acquaintance-level line -- the
--   header @sheet.md@ is required to open with (its display name, see
--   @WRITER.md@), plus whatever paragraph follows it, by convention
--   rather than an LLM call (see the project chat that designed this,
--   2026-07-20: "already stored data", not content analysis). Its own
--   named definition (@character.blurb@), registered in
--   'defaultLibrarySource' under both a bare name (what 'contextCharacter'
--   itself calls) and the dotted @character.blurb@ (what a project
--   override addresses), so a project can override just this one
--   definition independently of the richer buckets around it.
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
--   definition do not move together under a single override. (This is a
--   real, separate gap worth knowing about: @contextWriter@'s own body
--   still references @contextLore@ by its bare alias rather than
--   @context.lore@, so an override of @context.lore@ today does *not*
--   reach @contextWriter@'s composition either -- out of scope for this
--   fix, but the same shape of bug, sitting right next to it.)
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

characterBlurb :: Text -> Action Value
characterBlurb charname = runDefinition characterBlurbDef [toBinding charname]

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
contextCharacter :: Text -> Binding -> Action Value
contextCharacter = [dsl|
charname: journal:
  as "sheet": in (charname | branch): read "sheet.md" | orifempty ""
  as "blurb": character.blurb charname
  as "full":
    in (charname | branch):
      in (**/* | exclude("sheet.md", "journal.md")):
        for f in **/*:
          as f: read f
  as "journal": journal charname
  as "journalFull": in (charname | branch): read "journal.md" | orifempty ""
  character.blurb charname
|]

-- | 'contextCharacter', fully applied to the compiled-in
--   'Storyteller.Context.DSL.Compile.journalDelta' default -- the actual
--   1-arity function (just @charname@) registered as @context.character@,
--   matching the arity a wire-level "which character" call site actually
--   has to supply. @journalDelta@'s curation numbers are
--   'Server.Writer.File.activeCharacterContext''s own prior
--   @journalLookback@\/@journalMaxOut@\/@journalPadding@ constants, moved
--   to this definition's own default rather than duplicated at every
--   future call site -- still Haskell-level parameter passing,
--   deliberately, since different callers legitimately wanting different
--   curation tuning is genuine per-call parametricity, not a shared
--   default a project should replace by name (contrast 'characterBlurb',
--   which is exactly that, and is referenced by name instead).
contextCharacterDefault :: Text -> Action Value
contextCharacterDefault charname =
  contextCharacter charname (journalDelta 30 10 2)

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
--   module's own 'contextCharacterDefault' as fallback) is the caller's
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
characterSummaryOf :: Text -> Value -> Action CharSummary
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
contextMentionFilter :: Binding -> Action Value
contextMentionFilter = [dsl|
aliases:
  in aliases:
    for f in *:
      as f: read f
|]

-- | The definitions that genuinely can't live as pure DSL source in
--   'defaultLibrarySource', keyed by the dotted name a client\/project
--   override addresses -- 'Binding' already carries its own arity (see
--   its own haddock: "values are just 0-arity functions, otherwise no
--   different"), so a uniform @[(Name, Binding)]@ is exactly the right
--   shape regardless. @context.mentionFilter@ is the one remaining entry
--   that needs a real Haskell-supplied fallback
--   ('Storyteller.Core.Context.resolveContextQuery''s own @def@
--   parameter): it's resolved directly via
--   'Storyteller.Core.Context.getContextDefinition' by its one caller
--   ("Server.Writer.Lore"), which supplies a live per-call @aliases@
--   argument no static registration could hold. @context.character@ used
--   to belong here too (it closed over
--   'Storyteller.Context.DSL.Compile.journalDelta', a host function), but
--   'contextCharacterDefault' now has the plain @Text -> Action Value@
--   shape 'Storyteller.Core.Context.resolveContext1' already expects, the
--   same as @context.writer@\/'contextWriter' -- see
--   'Server.Writer.File.activeCharacterContext'. Every other definition
--   this application ships lives in 'defaultLibrarySource', resolved
--   with no Haskell-side fallback at all (see
--   'Storyteller.Core.Context.resolveContext0'\/'resolveContext1').
defaultLibrary :: [(Name, Binding)]
defaultLibrary =
  [ ("context.mentionFilter", toBinding1 contextMentionFilter)
  ]

-- | Re-curries a QQ-spliced 1-arity definition (@'Binding' ->
--   'Action' 'Value'@, per "Storyteller.Context.DSL.QQ") back into the
--   'Binding' shape 'Storyteller.Context.DSL.Compile.EApp' actually calls
--   -- the inverse of what applying a 'Binding' to a QQ-spliced function
--   normally does, needed here only because 'defaultLibrary' wants one
--   uniform list rather than a fixed-arity field per entry.
toBinding1 :: (Binding -> Action Value) -> Binding
toBinding1 f = Binding 1 go
  where
    go [a] _  = f (bval a)
    go args _ = fail $ "expected exactly 1 argument, got " <> show (length args)

-- | The identity -- wraps a plain string as a 'Value' whose own default
--   is exactly that text, nothing else. What used to get hand-rolled
--   inline (@[dsl| a: a |]@, or reaching for
--   'Storyteller.Context.DSL.Value.leafValue' directly) at any call site
--   that just needed "this text, as a DSL value" -- one named, reusable
--   definition instead.
identity :: Text -> Action Value
identity = [dsl| a: a |]

-- | Every pure-DSL definition this application ships, as already-parsed
--   'Definition's -- what 'Storyteller.Core.Context.buildContextLibrary'
--   folds a project's own 'Storyteller.Core.Context.contextsBranchName'
--   overrides on top of, once, into the shared table
--   'Storyteller.Context.DSL.Value.ContextLibrary' resolves both
--   cross-definition reference *and* 'Storyteller.Core.Context.resolveContext0'\/
--   'resolveContext1''s own external lookups against -- one map serves
--   both, since they're really the same question ("what does this name
--   mean right now"), just asked from inside a definition's own body or
--   from a plain @Sem@-level caller.
--
--   Two keys per definition, both pointing at the identical parsed value
--   (no second copy of the source, unlike this map's own predecessor --
--   see 'loreEntryDef''s Haddock on why a bare 'Definition' rather than
--   text is what makes that possible): a bare name (@loreEntry@,
--   @contextLore@, ...) for what a sibling definition's own body
--   actually writes when it references this one, and a dotted name
--   (@context.lore@, ...) for what a client\/project override addresses
--   and what 'resolveContext0'\/'resolveContext1' callers pass. Every
--   definition that *can't* live here -- needs a real Haskell-supplied
--   fallback -- is in 'defaultLibrary' instead; see that list's own
--   Haddock for exactly which two and why.
defaultLibrarySource :: Map Name Definition
defaultLibrarySource = Map.fromList
  [ ("loreEntry",       loreEntryDef)
  , ("contextLore",     contextLoreDef)
  , ("context.lore",    contextLoreDef)
  , ("chapterEntry",    chapterEntryDef)
  , ("contextChapters", contextChaptersDef)
  , ("context.chapters", contextChaptersDef)
  , ("contextOther",    contextOtherDef)
  , ("context.other",   contextOtherDef)
  , ("contextWriter",   contextWriterDef)
  , ("context.writer",  contextWriterDef)
  , ("context.style",   contextStyleDef)
  , ("character.blurb", characterBlurbDef)
  ]
