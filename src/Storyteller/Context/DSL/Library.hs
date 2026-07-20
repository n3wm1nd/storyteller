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
  , contextMentionFilter
  , contextMain
  ) where

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (Binding(..), bval)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value (Action, Value)

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
contextChapters :: Action Value
contextChapters = [dsl|
x =
  for f in chapters/**/*:
    as f: read f
in (x | sortBy):
  for f in **/*:
    as f: read f
|]

-- | A named character's own sheet, read from their branch -- the spec's
--   own cross-branch worked example verbatim.
contextCharacter :: Binding -> Action Value
contextCharacter = [dsl|
charname:
  in (charname | branch): read "sheet.md" | orifempty ""
|]

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
--   @"style"@ is exported as its own top-level entry, distinct from
--   @lore@\/@chapters@' individual per-file entries, so a caller that
--   wants to fold it into a system prompt instead of the ordinary message
--   stream can still tell it apart.
--
--   The third block is the catch-all: any file that isn't under either
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
  in lore:
    for f in **/*:
      as f: read f
  in chapters:
    for f in **/*:
      as f: read f
  in (**/* | exclude(lore, chapters, "style.md") | exclude("chat/**/*")):
    for f in **/*:
      as f: read f
  as "style": style
|]

-- | Every default definition this application ships. Arity differs per
--   entry ('contextStyle'\/'contextLore'\/'contextChapters' are plain
--   0-arity values; 'contextCharacter'\/'contextMentionFilter' take one
--   argument; 'contextMain' takes three) -- 'Binding' already carries its
--   own arity (see its own haddock: "values are just 0-arity functions,
--   otherwise no different"), so a uniform @[(Name, Binding)]@ is exactly
--   the right shape regardless, with no arity-indexed type needed.
defaultLibrary :: [(Name, Binding)]
defaultLibrary =
  [ ("context.style",         bval contextStyle)
  , ("context.lore",          bval contextLore)
  , ("context.chapters",      bval contextChapters)
  , ("context.character",     toBinding1 contextCharacter)
  , ("context.mentionFilter", toBinding1 contextMentionFilter)
  , ("context.main",          toBinding3 contextMain)
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

-- | 'toBinding1', three arguments (what 'contextMain' needs).
toBinding3 :: (Binding -> Binding -> Binding -> Action Value) -> Binding
toBinding3 f = Binding 3 go
  where
    go [a, b, c] _ = f (bval a) (bval b) (bval c)
    go args _      = fail $ "expected exactly 3 arguments, got " <> show (length args)
