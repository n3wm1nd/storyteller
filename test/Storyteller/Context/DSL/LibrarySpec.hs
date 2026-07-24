{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The default library ("Storyteller.Context.DSL.Library") actually
--   composes against a real (mock-git-backed) branch -- a clean compile
--   here proves nothing about whether 'contextWriter' correctly composes
--   'contextLore'\/'contextChapters'\/'contextOther' (a wrong parameter, a
--   swapped @in@, would still typecheck) -- and, since 'contextWriter'
--   references those by plain name rather than as parameters, this is
--   also the one place proving the shared-library cross-definition
--   mechanism ("Storyteller.Context.DSL.Value".'ContextLibrary') actually
--   resolves them at runtime, not just at the type level.
module Storyteller.Context.DSL.LibrarySpec (spec) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified Storage.Ops as Ops
import Storyteller.Core.Context (ContextRow, ContextStorage, buildContextLibrary, getContextOverrides, interpretContextStorageMap, runContextValue)
import Storyteller.Core.ContentEffects (BranchResolve)
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (Library, bval)
import qualified Storyteller.Context.DSL.Compile as Compile
import Storyteller.Context.DSL.Library
  (contextCharacter, contextLore, contextMentionFilter, contextWriter)
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

-- | Runs against 'Storyteller.Context.DSL.Library.defaultLibrarySource',
--   not an empty library -- unlike a leaf definition with no
--   cross-references, 'contextWriter'\/'contextOther'\/'contextLore' only
--   resolve at all because their own sibling names ('loreEntry',
--   'Storyteller.Context.DSL.Library.chapterEntry', ...) are in it.
runDslOn
  :: forall a
  .  BranchName
  -> (forall r. Members '[BranchOp Main, BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = runBranchAndFS @Main bname $ do
  overrides <- getContextOverrides
  runContextValue @Main (act (buildContextLibrary @Main overrides))

-- | 'runDslOn', but against a library assembled with the given committed
--   overrides -- what 'contextCharacterBlurbOverrideSpec' uses to prove an
--   override actually reaches 'contextCharacter''s own composition,
--   instead of just compiling. Locally re-interprets 'ContextStorage'
--   (shadowing "Server.TestStack"'s own empty-map one) so only calls made
--   within @act@ itself see these overrides.
runDslOnWith
  :: forall a
  .  Map Name Text -> BranchName
  -> (forall r. Members '[BranchOp Main, BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOnWith overrides bname act =
  runBranchAndFS @Main bname $ interpretContextStorageMap overrides $ do
    ovr <- getContextOverrides
    runContextValue @Main (act (buildContextLibrary @Main ovr))

entryTexts :: Value r -> Action r (Map Text Text)
entryTexts v = Map.fromList <$>
  mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries v)

spec :: Spec
spec = do
  contextWriterSpec
  contextWriterLoreOverrideSpec
  contextLoreSpec
  contextMentionFilterSpec
  contextCharacterBlurbOverrideSpec
  contextStyleSelfReferenceOverrideSpec
  newNameSelfReferenceOverrideSpec
  mutualReferenceOverrideSpec

-- | The regression test for the sibling bug 'contextCharacterBlurbOverrideSpec'
--   flagged as "the same shape, sitting right next to it": @contextWriter@'s
--   own body used to reference @contextLore@ by its bare alias rather than
--   @context.lore@, so a project's own override of @context.lore@ was
--   silently invisible to @contextWriter@'s composition, exactly like
--   @character.blurb@ was to @contextCharacter@ before that fix. Now that
--   'contextWriterDef' references @context.lore@ by its dotted name (and
--   'defaultLibrarySource' no longer even registers a bare @contextLore@
--   alias to fall back to), an override committed under that exact key has
--   to reach @contextWriter@'s own flat default stream.
--
--   The override is a bare string, with no per-file entries of its own --
--   so @contextOther@'s own @exclude(context.lore, ...)@ (matched against
--   @context.lore@'s own 'valueEntries', never a forced default -- see
--   'contextOtherDef''s own haddock) has nothing to exclude @lore\/notes.md@
--   by, and it falls through into "Other notes" too. Asserting that
--   honestly, rather than a narrower fixture that hides it, is the point:
--   an override replacing @context.lore@ wholesale genuinely does affect
--   what @contextOther@ sees, not just what @context.lore@ itself prints.
contextWriterLoreOverrideSpec :: Spec
contextWriterLoreOverrideSpec =
  describe "contextWriter honors a committed override of context.lore" $
    it "uses the overridden lore definition, not the compiled-in default, in its own flat stream" $
      run (testStack $ do
        seedBranch "main" [("lore/notes.md", "a hand-authored note")]
        runDslOnWith overrides (BranchName "main") go)
      `shouldBe` Right
        [ User "this is a project-committed override, not the default"
        , User "## Other notes"
        , User "## lore/notes.md"
        , FileRead "lore/notes.md" "a hand-authored note"
        ]
  where
    overrides = Map.fromList
      [ ("context.lore", "\"this is a project-committed override, not the default\"") ]
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) [Message]
    go table = valueDefault =<< contextWriter @Main table ""

-- | The regression test for the bug that started this whole redesign:
--   'contextCharacter' used to take @blurb@ as a typed 'Binding'
--   parameter, wired in Haskell by a separate @contextCharacterDefault@
--   wrapper -- so a project's own override of @character.blurb@, however
--   correctly committed to the Contexts branch, was silently never seen
--   by 'contextCharacter''s composition, because nothing about that
--   composition ever asked the library about the name @character.blurb@
--   at all. Now that 'contextCharacter''s own body references
--   @character.blurb@ by its dotted name directly, an override committed
--   under that exact key has to reach the @"blurb"@ bucket -- this test
--   is what would have caught the bug, not just a compile-time check that
--   the wiring typechecks.
contextCharacterBlurbOverrideSpec :: Spec
contextCharacterBlurbOverrideSpec =
  describe "contextCharacter honors a committed override of character.blurb" $
    it "uses the overridden blurb definition, not the compiled-in default, in the \"blurb\" bucket" $
      run (testStack $ do
        seedBranch "main" []
        _ <- createBranch (BranchName "character/aria")
        runBranchOpGit @Main (BranchName "character/aria")
          (runStorage @Main (Ops.addAtom "sheet.md" "# Aria\n\nA wandering rogue."))
        runDslOnWith overrides (BranchName "main") go)
      `shouldBe` Right "this is a project-committed override, not the default"
  where
    overrides = Map.fromList
      [ ("character.blurb", "charname:\n  \"this is a project-committed override, not the default\"") ]
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) Text
    go table = do
      v <- contextCharacter @Main table "aria"
      Just blurbAction <- pure (lookup "blurb" (valueEntries v))
      messagesText <$> (valueDefault =<< blurbAction)

-- | The regression test for the whole point of the compile-by-reference
--   redesign: an override that calls its own name resolves to whatever
--   that name meant *before* the override -- the compiled-in default, in
--   this case -- rather than to the override itself, which would loop
--   forever under the old live-lookup-by-name design. @context.style@ is
--   the target here because it has no dependents of its own (unlike
--   @context.lore@\/@character.blurb@, already exercised above for
--   ordinary cross-name visibility), keeping this test about self-
--   reference specifically.
contextStyleSelfReferenceOverrideSpec :: Spec
contextStyleSelfReferenceOverrideSpec =
  describe "an override that calls its own name resolves to the previous binding, not itself" $
    it "context.style calling context.style reaches the compiled-in default, not an infinite loop" $
      run (testStack $ do
        seedBranch "main" [("style.md", "write in past tense")]
        runDslOnWith overrides (BranchName "main") go)
      `shouldBe` Right "prefix: write in past tense"
  where
    overrides = Map.fromList
      [ ("context.style", "\"prefix: %context.style%\"") ]
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) Text
    go table = case Map.lookup "context.style" table of
      Just (Binding 0 fn) -> do
        scope <- Compile.currentScope @Main
        messagesText <$> (valueDefault =<< fn [] scope)
      _ -> fail "expected context.style to be a 0-arity binding"

-- | The other half of the same story: a genuinely *new* override-only
--   name (nothing compiled-in ever bound it) that references its own name
--   has no "previous binding" to fall back to at all -- an "unknown
--   identifier" failure the moment it's actually run, not a loop and not
--   a silent no-op.
newNameSelfReferenceOverrideSpec :: Spec
newNameSelfReferenceOverrideSpec =
  describe "a brand-new override-only name that self-references" $
    it "fails to resolve -- there is no previous binding to fall back to" $
      run (testStack $ do
        seedBranch "main" []
        runDslOnWith overrides (BranchName "main") go)
      `shouldSatisfy` \case
        Left _  -> True
        Right _ -> False
  where
    overrides = Map.fromList
      [ ("context.brandNew", "context.brandNew") ]
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) Text
    go table = case Map.lookup "context.brandNew" table of
      Just (Binding 0 fn) -> messagesText <$> (valueDefault =<< fn [] (emptyValue @(ContextRow Main r)))
      _                   -> fail "expected context.brandNew to be a 0-arity binding"

-- | Two new override-only names, each referencing the other -- mutual
--   recursion across distinct names is exactly as unsupported as
--   self-reference into a name with no prior binding: whichever compiles
--   first (library-internal 'Data.Map.Strict.toList' order for new names --
--   @context.mutualA@ before @context.mutualB@) can't see the other, since
--   it hasn't been compiled yet. A resolution failure the moment it's
--   actually run, not a loop -- the language was never meant to support
--   arbitrary recursion (see the project chat that settled this design).
mutualReferenceOverrideSpec :: Spec
mutualReferenceOverrideSpec =
  describe "two new override-only names that reference each other" $
    it "fails to resolve for whichever compiles first" $
      run (testStack $ do
        seedBranch "main" []
        runDslOnWith overrides (BranchName "main") go)
      `shouldSatisfy` \case
        Left _  -> True
        Right _ -> False
  where
    overrides = Map.fromList
      [ ("context.mutualA", "context.mutualB")
      , ("context.mutualB", "context.mutualA")
      ]
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) Text
    go table = case Map.lookup "context.mutualA" table of
      Just (Binding 0 fn) -> messagesText <$> (valueDefault =<< fn [] (emptyValue @(ContextRow Main r)))
      _                   -> fail "expected context.mutualA to be a 0-arity binding"

contextWriterSpec :: Spec
contextWriterSpec = describe "contextWriter (the default context.writer library entry)" $ do
  it "composes contextLore/contextChapters/contextOther by name into one self-describing stream -- chapters sorted and User/Assistant framed, style absent entirely" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/notes.md", "a hand-authored note")
        , ("style.md", "write in past tense")
        , ("chapters/ch11.md", "chapter eleven prose")
        , ("chapters/ch2.md", "chapter two prose")
        , ("chat/scratch.md", "chat scratch, never lore or a chapter")
        , ("todo.md", "a stray root note, filed under neither lore/ nor chapters/")
        ]
      runDslOn (BranchName "main") (`go` ""))
    `shouldBe` Right
      [ User "## Story background"
      , User "## lore/notes.md"
      , FileRead "lore/notes.md" "a hand-authored note"
      , User "## Chapter: chapters/ch2.md"
      , Assistant "chapter two prose"
      , User "## Chapter: chapters/ch11.md"
      , Assistant "chapter eleven prose"
      , User "## Other notes"
      , User "## todo.md"
      , FileRead "todo.md" "a stray root note, filed under neither lore/ nor chapters/"
      ]

  it "excludes the target path from chapters, but never from lore" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/notes.md", "a hand-authored note")
        , ("chapters/ch2.md", "chapter two prose")
        ]
      runDslOn (BranchName "main") (`go` "chapters/ch2.md"))
    `shouldBe` Right
      [ User "## Story background"
      , User "## lore/notes.md"
      , FileRead "lore/notes.md" "a hand-authored note"
      , User "## Other notes"
      ]

  -- | The case this section's own module exists to prove: an active
  --   character reaches 'contextWriter''s own result as a named entry
  --   (@as c: context.character c@ in 'Storyteller.Context.DSL.Library
  --   .contextWriterDef'), without changing the flat default stream
  --   above at all -- structural, additive access, not a fold. Presence
  --   is keyed off the same @path@ 'contextWriter' itself takes, exactly
  --   like 'Storyteller.Context.DSL.CompileSpec.forOverBindingResultSpec'
  --   proved @charactersin@ resolves it.
  it "exposes each active character as a named entry, carrying their own context.character bucket, alongside the unchanged flat default" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/notes.md", "a hand-authored note")
        , ("chapters/ch2.md", "chapter two prose")
        ]
      _ <- createBranch (BranchName "character/aria")
      runBranchOpGit @Main (BranchName "character/aria")
        (runStorage @Main (Ops.addAtom "sheet.md" "# Aria\n\nA wandering rogue."))
      runBranchOpGit @Main (BranchName "main") $
        void (recordPresence @Main "chapters/ch2.md" (Character (BranchName "character/aria")) Enter)
      runDslOn (BranchName "main") goWithCharacter)
    `shouldBe` Right
      ( [ User "## Story background"
        , User "## lore/notes.md"
        , FileRead "lore/notes.md" "a hand-authored note"
        , User "## Other notes"
        ]
      , ["Aria: A wandering rogue."]
      )
  where
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Text -> Action (ContextRow Main r) [Message]
    go table path = valueDefault =<< contextWriter @Main table path
    goWithCharacter :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) ([Message], [Text])
    goWithCharacter table = do
      v         <- contextWriter @Main table "chapters/ch2.md"
      def       <- valueDefault v
      Just aria <- pure (lookup "aria" (valueEntries v))
      ariaTexts <- messagesText <$> (valueDefault =<< aria)
      pure (def, [ariaTexts])

-- | 'contextLore'\/'contextOther' each on their own -- self-describing
--   *and* keeping per-file entries, both at once (see 'contextLore''s own
--   Haddock on why: entries for @exclude@ to match against, a default for
--   a caller referencing it bare, same as 'contextWriter' does).
contextLoreSpec :: Spec
contextLoreSpec = describe "contextLore/contextOther (standalone)" $ do
  it "contextLore: a heading plus one already-framed message pair per file, in both its own default and its own entries" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") go)
    `shouldBe` Right
      ( [ User "## Story background"
        , User "## lore/notes.md"
        , FileRead "lore/notes.md" "a hand-authored note"
        ]
      , Map.fromList [("lore/notes.md", "## lore/notes.md\na hand-authored note")]
      )
  where
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) ([Message], Map Text Text)
    go table = do
      v      <- contextLore @Main table
      def    <- valueDefault v
      texts  <- entryTexts v
      pure (def, texts)

contextMentionFilterSpec :: Spec
contextMentionFilterSpec = describe "contextMentionFilter (the default context.mentionFilter library entry)" $
  it "is the identity: every candidate alias stays active by default" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", "Aria is a wandering rogue.")]
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList [("Aria", "Aria is a wandering rogue.")])
  where
    aliases :: Action r (Value r)
    aliases = pure Value
      { valueDefault = pure []
      , valueEntries = [("Aria", pure (leafValue [User "Aria is a wandering rogue."]))]
      , valueMeta = defaultMeta
      }
    go :: forall r. Members '[BranchResolve, ContextStorage, Fail] r => Library (ContextRow Main r) -> Action (ContextRow Main r) (Map Text Text)
    go _table = do
      v <- contextMentionFilter @Main (bval aliases)
      entryTexts v
