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
--   The only Haskell-side policy left after this module is
--   'Storyteller.Context.DSL.Scope.liveTreeValueOfCommit''s binary
--   exclusion, which genuinely can't be expressed any other way (see its
--   own haddock).
--
--   Every definition here is named the same way
--   'Storyteller.Core.Prompt.PromptKey' names a prompt override --
--   dotted, namespaced -- and is meant to be looked up on a future
--   Contexts branch (still unbuilt; see @CONTEXT-DSL.md@'s own "a
--   branch-hosted, override-with-fallback function library" deferral)
--   before falling back to the compiled-in 'Binding' here. Composition
--   between these pieces ('contextMain' pulling in 'contextLore'\/
--   'contextChapters'\/'contextStyle') is ordinary Haskell parameter
--   passing, the same pattern the spec's own invented-calendar example
--   uses for a host-supplied function -- there's no cross-definition name
--   resolution inside the interpreter itself yet, so a project overriding
--   just @context.lore@ still gets its result threaded into the
--   (unmodified, or also-overridden) @context.main@ the same way either
--   way.
module Storyteller.Context.DSL.Library
  ( defaultLibrary
  , contextStyle
  , contextLore
  , contextChapters
  , contextCharacter
  , contextCharacterDefault
  , characterBlurb
  , characterSummary
  , contextMentionFilter
  , contextMain
  , contextQuery
  , toBinding1
  ) where

import Data.Text (Text)

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (Binding(..), bval, journalDelta)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value (Action, Value, Message(..), leafValue, namedEntry)
import qualified Storyteller.Context.DSL.Render as Render
import Storyteller.Writer.Agent (CharSummary(..))

-- | The one reserved standing-instruction file, if a project has one --
--   mirrors 'Storyteller.Writer.Agent.WorldContext.isSystemContextPath',
--   but as a plain glob, not a predicate: a project keeping its style
--   guide somewhere else just overrides this one definition.
contextStyle :: Action Value
contextStyle = [dsl|
read "style.md" | orifempty ""
|]

-- | Hand-authored lore -- a plain positive convention (@lore\/**@), not
--   "everything except chapters/style/scratch": 'exclude'\/'without'\/
--   'only' can only neuter a key's *content* to 'emptyValue', never
--   actually shrink 'valueEntries'' own key set (their arguments are
--   themselves DSL 'Value's, not knowable without forcing inside an
--   'Action', and a filter has to stay pure/synchronous -- see their own
--   haddocks) -- so an "everything except..." definition re-exported
--   through a second @for@ (exactly what composing this into 'contextMain'
--   needs) would resurrect every "excluded" path as an empty-content
--   entry instead of dropping it. A plain positive glob has no such
--   hazard: nothing here ever needs to un-match a key that was never
--   matched in the first place. A project without a @lore\/@ directory
--   gets nothing from this default until it writes its own convention --
--   consistent with "override, don't guess," not a gap.
contextLore :: Action Value
contextLore = [dsl|
for f in lore/**/*:
  as f: read f
|]

-- | Chapter prose, in natural reading order (@ch2@ before @ch11@, not
--   @ch11@ before @ch2@) -- 'sortBy''s reordering now survives the
--   re-export through a second glob (see
--   'Storyteller.Context.DSL.Compile.globMatchPat''s own haddock for why
--   that used to silently undo it).
--
--   Each chapter's own entry is a @User@ header immediately followed by
--   its content re-tagged @Assistant@ (@> read f@, widened per
--   'Storyteller.Context.DSL.AST.EAssistant''s own haddock) -- the exact
--   prior-turn framing @Storyteller.Writer.Agent.Write.
--   buildChapterMessages@ used to hand-construct in Haskell for
--   "earlier chapters" (a header naming the chapter, then its prose
--   presented as the model's own earlier output), now built once here
--   instead of duplicated at every call site.
contextChapters :: Action Value
contextChapters = [dsl|
x =
  for f in chapters/**/*:
    as f:
      "## Chapter: %f%"
      > read f
in (x | sortBy):
  for f in **/*:
    as f: read f
|]

-- | The "and this is the character" acquaintance-level line -- the
--   header @sheet.md@ is required to open with (its display name, see
--   @WRITER.md@), plus whatever paragraph follows it, by convention
--   rather than an LLM call (see the project chat that designed this,
--   2026-07-20: "already stored data", not content analysis). Its own
--   named definition (@character.blurb@), not folded straight into
--   'contextCharacter', so a project can override just this one
--   definition (what "acquaintance summary" ought to include)
--   independently of the richer buckets around it.
--
--   Takes @charname@ and crosses to that branch itself (@in (charname |
--   branch): ...@), the same as 'contextCharacter''s own @"sheet"@
--   bucket -- it can't rely on a caller's enclosing @in@ instead, for
--   the same reason 'Storyteller.Context.DSL.Compile.journalDelta'
--   can't: 'bval' (what a 0-arity 'Binding' parameter is built from)
--   wraps an *already-scoped* 'Action', not one that re-resolves the
--   caller's own ambient Reader scope on every call (see its own
--   haddock) -- there's no dynamic-scope crossing between two
--   separately compiled 'Storyteller.Context.DSL.AST.Definition's, only
--   within one definition's own body. A definition invoked as a
--   cross-definition parameter has to be self-contained about which
--   branch it reads from, exactly like 'journalDelta'.
characterBlurb :: Binding -> Action Value
characterBlurb = [dsl|
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
--   * @"blurb"@ -- 'characterBlurb', threaded in as a parameter rather
--     than referenced by name, so an override of just @character.blurb@
--     still reaches every caller of this definition (see the module
--     haddock: no cross-definition name resolution inside the
--     interpreter itself). Called with @charname@ explicitly (see
--     'characterBlurb''s own haddock for why it has to cross branches
--     itself rather than inheriting this definition's own @in@).
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
contextCharacter :: Binding -> Binding -> Binding -> Action Value
contextCharacter = [dsl|
charname: blurb: journal:
  as "sheet": in (charname | branch): read "sheet.md" | orifempty ""
  as "blurb": blurb charname
  as "full":
    in (charname | branch):
      in (**/* | exclude("sheet.md", "journal.md")):
        for f in **/*:
          as f: read f
  as "journal": journal charname
  as "journalFull": in (charname | branch): read "journal.md" | orifempty ""
  blurb charname
|]

-- | 'contextCharacter', fully applied to the compiled-in
--   'characterBlurb' and 'Storyteller.Context.DSL.Compile.journalDelta'
--   defaults -- the actual 1-arity function (just @charname@) registered
--   as @context.character@, matching the arity a wire-level "which
--   character" call site actually has to supply. The curation numbers
--   here are 'Server.Writer.File.activeCharacterContext''s own prior
--   @journalLookback@\/@journalMaxOut@\/@journalPadding@ constants,
--   moved to this definition's own default rather than duplicated at
--   every future call site.
contextCharacterDefault :: Binding -> Action Value
contextCharacterDefault charnameB =
  contextCharacter charnameB (toBinding1 characterBlurb) (journalDelta 30 10 2)

-- | Runs 'contextCharacterDefault' for one character and reshapes its
--   buckets into a 'CharSummary' -- the shared piece every consumer
--   wanting that exact shape reaches for
--   ('Server.Writer.File.activeCharacterContext', ambient scene context;
--   'Storyteller.Writer.Agent.Roleplay.askCharacter'\/
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent', a
--   character's own subagent), rather than picking 'Value' buckets apart
--   at every call site. @journalBucket@ selects which of
--   'contextCharacter''s own two journal readings a caller wants --
--   @"journal"@ (curated via
--   'Storyteller.Context.DSL.Compile.journalDelta') or @"journalFull"@
--   (verbatim) -- see that definition's own Haddock on the pair.
characterSummary :: Text -> Text -> Action CharSummary
characterSummary journalBucket charname = do
  charVal <- contextCharacterDefault (bval (pure (leafValue [User charname])))
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

-- | The default top-level program a chat\/write query sends as its one
--   context parameter -- composes 'contextLore'\/'contextChapters'\/
--   'contextStyle' by passing each in as an ordinary parameter (see the
--   module haddock for why, not by referencing them as free identifiers).
--
--   Exports four *distinguished, unflattened* top-level entries --
--   @"lore"@, @"chapters"@, @"other"@, @"style"@ -- each its own nested
--   container, rather than merging everything into one flat pool. This
--   matters beyond bookkeeping: 'Storyteller.Writer.Agent.Write.
--   writeAgent' has to keep chapters in their own cache-stable,
--   alternating-role slot and style out of the message stream entirely
--   (see that module's own Haddock on why flattening would break its
--   prompt-cache-prefix discipline) -- so the wiring layer needs to pull
--   each bucket out separately, not walk one merged entry list.
--
--   @"other"@ is the catch-all: any file that isn't under either
--   @lore@\/@chapters@' own convention (a stray hand-authored note
--   nobody's filed into @lore\/@ yet, say) still shows up, built directly
--   from @lore@\/@chapters@'s own key sets via @exclude@ -- not by
--   restating "not lore\/**, not chapters\/**" as a second pattern list
--   that could drift out of sync with their own definitions. Chained
--   @exclude@ calls compose the same way a single multi-argument call
--   would (@exclude(lore, chapters, ...) | exclude("chat/**/*")@ is
--   equivalent to one call with every argument) -- @chat\/**@ stays a
--   plain literal pattern here since it's not a whole other definition's
--   own result, just a fixed scratch-space convention.
contextMain :: Binding -> Binding -> Binding -> Action Value
contextMain = [dsl|
lore: chapters: style:
  as "lore":
    in lore:
      for f in **/*:
        as f: read f
  as "chapters":
    in chapters:
      for f in **/*:
        as f: read f
  as "other":
    in (**/* | exclude(lore, chapters, "style.md") | exclude("chat/**/*")):
      for f in **/*:
        as f: read f
  as "style": style
|]

-- | 'contextMain', fully applied to the other three default definitions --
--   the actual 0-arity program a chat\/write query's own wire-level
--   context field resolves to by default (see
--   'Storyteller.Core.Context.resolveContextQuery'\/'getContextDefinition':
--   both only ever run a 0-arity 'Binding', since the wire never sends
--   parameters). 'contextMain' itself stays exported separately since it's
--   the reusable composer -- a project overriding just @context.lore@
--   still gets it threaded into this same composition, unmodified.
contextQuery :: Action Value
contextQuery = contextMain (bval contextLore) (bval contextChapters) (bval contextStyle)

-- | Every default definition this application ships. Arity differs per
--   entry ('contextStyle'\/'contextLore'\/'contextChapters'\/'contextQuery'\/
--   'characterBlurb' are plain 0-arity values; 'contextCharacterDefault'\/
--   'contextMentionFilter' take one argument, the raw 3-arity
--   'contextCharacter' fully applied down to that same one-argument shape)
--   -- 'Binding' already carries its own arity (see its
--   own haddock: "values are just 0-arity functions, otherwise no
--   different"), so a uniform @[(Name, Binding)]@ is exactly the right
--   shape regardless, with no arity-indexed type needed. @context.main@
--   names 'contextQuery' (0-arity, ready to run as-is), not 'contextMain'
--   itself (3-arity, a composer) -- the dotted key is what a client\/branch
--   override addresses, and an override is always a fresh 0-arity program,
--   never something that takes parameters.
defaultLibrary :: [(Name, Binding)]
defaultLibrary =
  [ ("context.style",         bval contextStyle)
  , ("context.lore",          bval contextLore)
  , ("context.chapters",      bval contextChapters)
  , ("character.blurb",       toBinding1 characterBlurb)
  , ("context.character",     toBinding1 contextCharacterDefault)
  , ("context.mentionFilter", toBinding1 contextMentionFilter)
  , ("context.main",          bval contextQuery)
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
