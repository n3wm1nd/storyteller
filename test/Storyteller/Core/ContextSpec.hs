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

-- | 'Storyteller.Core.Context.ContextStorage' -- the Context DSL's
--   'Storyteller.Core.Prompt.PromptStorage' equivalent. Checks the pure
--   override-resolution decision ('resolveOverrideDefinition') directly,
--   then 'resolveContext0'\/'resolveContext1' end to end against both
--   interpreters: a missing override falls back to the caller's own
--   compiled-in default unchanged, and a real committed override on the
--   dedicated 'Contexts' branch actually takes over -- run from the
--   *caller's* ambient branch position (not the Contexts branch itself),
--   the same "whatever I'm already in" contract every other Context DSL
--   definition gets.
module Storyteller.Core.ContextSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified UniversalLLM as LLM

import qualified Storage.Ops as Ops
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Core.Context
  ( contextsBranchName, setContextOverride, interpretContextStorageFS, interpretContextStorageMap
  , resolveOverrideDefinition, ContextRow, ContextStorage, resolveContext0, resolveContext1, resolveAdhoc0, runContextValue )
import Storyteller.Core.ContentEffects (BranchResolve, TreeAccess)
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Compile (Library)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.Rendering (renderContext, renderText, renderMessages, namedChild)
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Presence (enters)
import Storyteller.Writer.Types (Character(..))

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

spec :: Spec
spec = do
  resolveOverrideDefinitionSpec
  resolveContext0Spec
  resolveContext1Spec
  resolveAdhoc0Spec
  clientSubmittedContextProgramSpec
  frontendSynthesizedProgramShapeSpec

-- | The actual point of the whole override mechanism, end to end, against
--   the real production definition and the real rendering pipeline --
--   every other test in this module proves the *mechanism* works against a
--   toy @context.greeting@ default; this is what a client sending a DSL
--   program with its own request (@fcContext@ on
--   'Server.Writer.File.Protocol.ChatWriter', staged via
--   'setContextOverride' exactly the way
--   'Server.Writer.File.chatWriter' does) actually changes: not just "some
--   binding resolves to different text," but the literal messages
--   'Storyteller.Writer.Agent.Write.writeAgent' would receive, once
--   rendered ('Storyteller.Context.DSL.Rendering.renderContext' ->
--   'Storyteller.Context.DSL.Rendering.renderText'\/'Storyteller.Context.DSL.Rendering.renderMessages').
--   Without a client program, resolving @context.writer@ against seeded
--   lore falls through to the compiled-in default
--   ('Storyteller.Context.DSL.Library.contextWriter'); with one staged,
--   the client's own program wins completely, discarding that lore.
clientSubmittedContextProgramSpec :: Spec
clientSubmittedContextProgramSpec = describe "a client-submitted context.writer program, end to end" $ do
  it "with no client program, resolves to the compiled-in default (real lore included)" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runBranchAndFS @Main (BranchName "main") $ do
        writerV <- resolveContext1 @Main "context.writer" (CtxLibrary.contextWriter @Main) "target.md"
        renderText <$> runContextValue @Main (renderContext writerV))
    `shouldBe` Right "## Story background\n\n## lore/notes.md\n\na hand-authored note\n\n## Chapters written so far\n\n## Other notes"

  it "a client program staged via setContextOverride replaces the default completely, seeded lore included" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.writer" "path:\n  \"a client-submitted override, replacing everything\"\n"
        writerV <- resolveContext1 @Main "context.writer" (CtxLibrary.contextWriter @Main) "target.md"
        ctx <- runContextValue @Main (renderContext writerV)
        pure (renderText ctx, map describeMessage (renderMessages ctx :: [LLM.Message ProseModel])))
    `shouldBe` Right
      ( "a client-submitted override, replacing everything"
      , [(LLM.User, "a client-submitted override, replacing everything")]
      )

-- | The frontend's own half of this contract, pinned from the backend
--   side: @frontend\/src\/lib\/dslCompose.ts@'s @synthesizeProgram@\/
--   @composeSendProgram@ produce exactly this shape (a leading @path:@
--   parameter line, everything else indented under it) for the same
--   reason 'clientSubmittedContextProgramSpec' stages one by hand --
--   'resolveOverrideDefinition' silently discards any override whose
--   parsed arity doesn't match @context.writer@'s own (1), falling back
--   to the compiled-in default with no error surfaced anywhere. A drift
--   between what the frontend actually emits and what this test embeds
--   is a real risk (two independent renderings of the same contract, in
--   two different languages) -- this is the guard against that drift
--   silently regressing, the same way 'contextCharacterBlurbOverrideSpec'
--   guards @character.blurb@'s own composition path.
frontendSynthesizedProgramShapeSpec :: Spec
frontendSynthesizedProgramShapeSpec =
  describe "the frontend's own synthesized context.writer override, staged verbatim" $
    it "parses at arity 1 and actually overrides context.writer, excluding path from chapters and including an active character" $
      run (testStack $ do
        seedBranch "main"
          [ ("lore/notes.md", "a hand-authored note")
          , ("chapters/ch2.md", "chapter two prose")
          ]
        _ <- createBranch (BranchName "character/aria")
        runBranchOpGit @Main (BranchName "character/aria")
          (runStorage @Main (Ops.addAtom "sheet.md" "# Aria\n\nA wandering rogue."))
        runBranchAndFS @Main (BranchName "main") $ do
          _ <- enters @Main "chapters/ch2.md" (Character (BranchName "character/aria"))
          setContextOverride "context.writer" frontendProgram
          writerV <- resolveContext1 @Main "context.writer" (CtxLibrary.contextWriter @Main) "chapters/ch2.md"
          runContextValue @Main $ do
            rc <- renderContext writerV
            pure (renderText rc, renderText <$> namedChild "aria" rc))
      `shouldBe` Right
        ( "## Story background\n\n## lore/notes.md\n\na hand-authored note"
        , Just "Aria: A wandering rogue."
        )
  where
    -- Exactly `synthesizeProgram`'s own output shape for
    -- `{ baseline: { lore: true, chapters: true, style: false },
    --    characters: [], extraFiles: [] }` -- see dslCompose.ts.
    frontendProgram = T.unlines
      [ "path:"
      , "  context.lore"
      , "  in (context.chapters | exclude(path)):"
      , "    for f in **/*: read f"
      , "  for c in (charactersin path):"
      , "    as c: context.character c"
      ]

-- | 'LLM.Message' has no 'Eq' -- compare on 'LLM.messageDirection' plus
--   the rendered text, same pattern
--   "Storyteller.Context.DSL.RenderingSpec" already uses.
describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

-- | An arity-0 default -- what @context.lore@\/@context.chapters@\/
--   @context.style@-style definitions actually have. Takes (and ignores)
--   the compile-time 'Library' table 'resolveContext0' now always applies
--   to its @def@ argument -- this stub references no other library name,
--   so it never needs to consult it.
defaultGreeting :: forall branch r. Members '[TreeAccess branch, Fail] r => Library r -> Action r (Value r)
defaultGreeting _table = pure (leafValue [User "default text"])

-- | An arity-1 default -- the shape @context.character@-style definitions
--   actually have (real production callers now resolve @context.character@
--   through exactly this same machinery, see
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent') -- echoes
--   its own argument's text back, wrapped, so a caller can tell whether
--   the argument it passed in actually reached the running definition.
defaultEcho :: forall branch r. Members '[TreeAccess branch, Fail] r => Library r -> Text -> Action r (Value r)
defaultEcho _table arg = pure (leafValue [User ("default: " <> arg)])

resolveOverrideDefinitionSpec :: Spec
resolveOverrideDefinitionSpec = describe "resolveOverrideDefinition" $ do
  it "returns Nothing when there's no override" $
    resolveOverrideDefinition 0 Nothing `shouldBe` Nothing

  it "returns Nothing on a malformed override" $
    resolveOverrideDefinition 0 (Just "as \"unterminated:") `shouldBe` Nothing

  it "returns Nothing when the override's own arity doesn't match" $
    resolveOverrideDefinition 0 (Just "charname:\n  charname\n") `shouldBe` Nothing

  it "returns Just the parsed Definition when it parses and the arity matches" $
    case resolveOverrideDefinition 1 (Just "charname:\n  charname\n") of
      Just _  -> pure ()
      Nothing -> expectationFailure "expected a valid arity-1 override to parse"

-- | 'resolveContext0' end to end, against both real interpreters. Each
--   case is two steps: 'resolveContext0' itself (a plain 'Sem' call,
--   already fully run) hands back a 'Value', which still needs a
--   *second*, separate 'runContextValue' call to force its own
--   'valueDefault' -- exactly the two-call shape the old
--   'runContextBinding0' used to wrap into one.
resolveContext0Spec :: Spec
resolveContext0Spec = describe "resolveContext0" $ do
  it "falls through to the caller's own default when nothing is staged" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      runBranchAndFS @Main (BranchName "empty") $ do
        v <- resolveContext0 @Main "context.greeting" (defaultGreeting @Main)
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "default text"

  it "a staged override is visible to a lookup in the same interpretation" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      runBranchAndFS @Main (BranchName "empty") $ do
        setContextOverride "context.greeting" "\"staged text\"\n"
        v <- resolveContext0 @Main "context.greeting" (defaultGreeting @Main)
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "staged text"

  it "a staged override still only wins when its arity matches -- same silent-fallback rule as a branch commit" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      runBranchAndFS @Main (BranchName "empty") $ do
        setContextOverride "context.greeting" "charname:\n  charname\n"
        v <- resolveContext0 @Main "context.greeting" (defaultGreeting @Main)
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "default text"

  it "a staged override takes priority over a same-named branch commit" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting.dsl", "\"from the branch\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          setContextOverride "context.greeting" "\"staged text\"\n"
          v <- resolveContext0 @Main "context.greeting" (defaultGreeting @Main)
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "staged text"

  it "runs a real committed override, positioned at the caller's own branch, not the Contexts branch" $
    run (testStack $ do
      seedBranch "main" [("greeting.md", "hello from main")]
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting.dsl", "< read \"greeting.md\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveContext0 @Main "context.greeting" (defaultGreeting @Main)
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "hello from main"

-- | 'resolveContext1' end to end -- the 1-arity counterpart, e.g.
--   @context.character@'s own shape. Regression for the real gap the
--   project chat found: every real character-context caller used to call
--   'Storyteller.Context.DSL.Library.contextCharacter' directly, never
--   through this machinery, so a project committing an override for
--   @context.character@ was silently ignored.
resolveContext1Spec :: Spec
resolveContext1Spec = describe "resolveContext1" $ do
  it "falls back to the 1-arity default (echoing its own argument) when no override is committed" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveContext1 @Main "context.greeting1" (defaultEcho @Main) "Aria"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "default: Aria"

  it "resolves and runs a real 1-arity override too, with the real argument reaching it" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting1.dsl", "name:\n  \"overridden for %name%\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveContext1 @Main "context.greeting1" (defaultEcho @Main) "Aria"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "overridden for Aria"

-- | 'resolveAdhoc0' -- what a per-call @pinnedPrograms@ entry
--   ('Server.Writer.File.chatWriter''s own wire field) resolves through:
--   no name, no compiled-in default to fall back to, just "run this
--   0-arity program against the library, or fail." A bare call to an
--   existing library name (@rules.magic@, say -- any project's own
--   committed @context.*@-style definition) resolves the same way any
--   other cross-definition reference does, since it's compiled against
--   the exact same 'buildContextLibrary' table 'resolveContext0'\/
--   'resolveContext1' use.
resolveAdhoc0Spec :: Spec
resolveAdhoc0Spec = describe "resolveAdhoc0" $ do
  it "runs a bare literal 0-arity program directly" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveAdhoc0 @Main "\"hand-authored pinned text\"\n"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "hand-authored pinned text"

  it "resolves a bare call to a project's own committed library definition" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", "# Aria\n\nA wandering rogue.")]
      seedBranch (unBranchName contextsBranchName)
        [("rules/magic.dsl", "\"the rules of magic\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveAdhoc0 @Main "rules.magic\n"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "the rules of magic"

  it "fails (rather than silently contributing nothing) for a 1-arity program -- there is no slot default to fall back to" $
    (run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveAdhoc0 @Main "name:\n  \"got %name%\"\n"
        runContextValue @Main (messagesText <$> valueDefault v))
      :: Either String Text)
    `shouldSatisfy` \case
      Left _  -> True
      Right _ -> False
