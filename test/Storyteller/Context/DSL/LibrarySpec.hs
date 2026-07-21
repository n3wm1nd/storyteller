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
import Storyteller.Context.DSL.Compile (bval, journalDelta)
import Storyteller.Context.DSL.Library
  (contextCharacter, contextLore, contextMentionFilter, contextWriter)
import Storyteller.Context.DSL.Value

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
  contextLoreSpec
  contextMentionFilterSpec
  contextCharacterBlurbOverrideSpec

-- | The regression test for the bug that started this whole redesign:
--   'contextCharacter' used to take @blurb@ as a typed 'Binding'
--   parameter, wired in Haskell by 'Storyteller.Context.DSL.Library.contextCharacterDefault'
--   -- so a project's own override of @character.blurb@, however
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
      v <- contextCharacter "aria" (journalDelta 30 10 2)
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
  where
    go path = valueDefault =<< contextWriter path

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
