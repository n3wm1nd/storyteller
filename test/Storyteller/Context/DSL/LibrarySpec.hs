{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Context (buildContextLibrary)
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (bval)
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
runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act (buildContextLibrary Map.empty))

-- | 'runDslOn', but against a library assembled with the given committed
--   overrides -- what 'contextCharacterBlurbOverrideSpec' uses to prove an
--   override actually reaches 'contextCharacter''s own composition,
--   instead of just compiling.
runDslOnWith :: Map Name Text -> BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOnWith overrides bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act (buildContextLibrary overrides))

entryTexts :: Value -> Action (Map Text Text)
entryTexts v = Map.fromList <$>
  mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries v)

spec :: Spec
spec = do
  contextWriterSpec
  contextWriterLoreOverrideSpec
  contextLoreSpec
  contextMentionFilterSpec
  contextCharacterBlurbOverrideSpec

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
    go = valueDefault =<< contextWriter ""

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
    go = do
      v <- contextCharacter "aria"
      Just blurbAction <- pure (lookup "blurb" (valueEntries v))
      messagesText <$> (valueDefault =<< blurbAction)

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
      runDslOn (BranchName "main") (go ""))
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
      runDslOn (BranchName "main") (go "chapters/ch2.md"))
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
    go path = valueDefault =<< contextWriter path
    goWithCharacter = do
      v         <- contextWriter "chapters/ch2.md"
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
    go = do
      v      <- contextLore
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
    aliases = pure Value
      { valueDefault = pure []
      , valueEntries = [("Aria", pure (leafValue [User "Aria is a wandering rogue."]))]
      , valueMeta = defaultMeta
      }
    go = do
      v <- contextMentionFilter (bval aliases)
      entryTexts v
