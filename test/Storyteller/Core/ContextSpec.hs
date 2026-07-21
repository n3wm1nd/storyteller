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

-- | 'Storyteller.Core.Context.ContextStorage' -- the Context DSL's
--   'Storyteller.Core.Prompt.PromptStorage' equivalent. Checks the pure
--   override-resolution decision ('resolveContextOverride') directly, then
--   both interpreters end to end: a missing override falls back to the
--   caller's own default 'Storyteller.Context.DSL.Compile.Binding'
--   unchanged, and a real committed override on the dedicated 'Contexts'
--   branch actually takes over -- run from the *caller's* ambient branch
--   position (not the Contexts branch itself), the same "whatever I'm
--   already in" contract every other Context DSL definition gets.
module Storyteller.Core.ContextSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)
import qualified UniversalLLM as LLM

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Core.Context
  ( contextsBranchName, getContextDefinition, setContextOverride, interpretContextStorageFS, interpretContextStorageMap
  , resolveContextOverride, resolveContext1, runContextBinding1, runContextValue )
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Compile (Binding(..), bval, fn1)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.Rendering (renderContext, renderText, renderMessages)
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDefaultZeroAry :: BranchName -> Binding -> Sem (StoryStorage : TestEffects '[]) (Either String Text)
runDefaultZeroAry bname (Binding 0 fn) = resolveBranch bname >>= \case
  Nothing -> pure (Left ("branch not found: " <> T.unpack (unBranchName bname)))
  Just h  -> do
    (msgs, _) <- Core.runStoreT h (runAction (fn [] emptyValue >>= valueDefault) (ContextLibrary Map.empty))
    pure (Right (messagesText msgs))
runDefaultZeroAry _ (Binding n _) = pure (Left ("expected arity 0, got " <> show n))

spec :: Spec
spec = do
  resolveContextOverrideSpec
  setContextOverrideSpec
  interpretContextStorageMapSpec
  interpretContextStorageFSSpec
  runContextBinding1Spec
  clientSubmittedContextProgramSpec

-- | The actual point of the whole override mechanism, end to end, against
--   the real production definition and the real rendering pipeline --
--   every other test in this module proves the *mechanism* works against a
--   toy @context.greeting@ 'Binding'; this is what a client sending a DSL
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
      runBranchOpGit @Main (BranchName "main") $ do
        ctx <- interpretContextStorageMap Map.empty $ do
          writerV <- resolveContext1 @Main "context.writer" CtxLibrary.contextWriter "target.md"
          runContextValue @Main (renderContext writerV)
        pure (renderText ctx))
    `shouldBe` Right "## Story background\n\n## lore/notes.md\n\na hand-authored note\n\n## Other notes"

  it "a client program staged via setContextOverride replaces the default completely, seeded lore included" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runBranchOpGit @Main (BranchName "main") $ do
        ctx <- interpretContextStorageMap Map.empty $ do
          setContextOverride "context.writer" "path:\n  \"a client-submitted override, replacing everything\"\n"
          writerV <- resolveContext1 @Main "context.writer" CtxLibrary.contextWriter "target.md"
          runContextValue @Main (renderContext writerV)
        pure (renderText ctx, map describeMessage (renderMessages ctx :: [LLM.Message ProseModel])))
    `shouldBe` Right
      ( "a client-submitted override, replacing everything"
      , [(LLM.User, "a client-submitted override, replacing everything")]
      )

-- | 'LLM.Message' has no 'Eq' -- compare on 'LLM.messageDirection' plus
--   the rendered text, same pattern
--   "Storyteller.Context.DSL.RenderingSpec" already uses.
describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

defaultGreeting :: Binding
defaultGreeting = bval (pure (leafValue [User "default text"]))

-- | An arity-1 default -- the shape @context.character@-style definitions
--   actually have (real production callers now resolve @context.character@
--   through exactly this same machinery, see
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent') -- echoes
--   its own argument's text back, wrapped, so a caller can tell whether
--   the argument it passed in actually reached the running 'Binding'.
defaultEcho :: Binding
defaultEcho = fn1 (\arg -> do
  msgs <- valueDefault =<< arg
  pure (leafValue [User ("default: " <> messagesText msgs)]))

resolveContextOverrideSpec :: Spec
resolveContextOverrideSpec = describe "resolveContextOverride" $ do
  it "returns the default unchanged when there's no override" $
    let Binding arity _ = resolveContextOverride defaultGreeting Nothing
    in arity `shouldBe` 0

  it "falls back to the default on a malformed override" $
    let Binding arity _ = resolveContextOverride defaultGreeting (Just "as \"unterminated:")
    in arity `shouldBe` 0

  it "falls back to the default when the override's own arity doesn't match" $
    let Binding arity _ = resolveContextOverride defaultGreeting (Just "charname:\n  charname\n")
    in arity `shouldBe` 0

-- | 'setContextOverride' stages an override for the rest of *this*
--   interpretation only -- never written anywhere durable, but otherwise
--   indistinguishable from a branch-committed one once staged: the same
--   'getContextDefinition' lookup finds it, with the same
--   'resolveContextOverride' arity check applied. This is what lets a WS
--   handler turn "the client sent this program for @context.writer@" into
--   exactly "treat this request as if @context.writer@ had this override"
--   with no separate wire-override code path anywhere else (see
--   'Server.Writer.File.chatWriter''s own use).
setContextOverrideSpec :: Spec
setContextOverrideSpec = describe "setContextOverride" $ do
  it "a name with nothing staged falls through to the branch/compiled default, same as before" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      binding <- interpretContextStorageMap Map.empty
                   (getContextDefinition "context.greeting" defaultGreeting)
      runDefaultZeroAry (BranchName "empty") binding)
    `shouldBe` Right (Right "default text")

  it "a staged override is visible to a lookup later in the same interpretation" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      binding <- interpretContextStorageMap Map.empty $ do
        setContextOverride "context.greeting" "\"staged text\"\n"
        getContextDefinition "context.greeting" defaultGreeting
      runDefaultZeroAry (BranchName "empty") binding)
    `shouldBe` Right (Right "staged text")

  it "a staged override still only wins when its arity matches -- same silent-fallback rule as a branch commit" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      binding <- interpretContextStorageMap Map.empty $ do
        setContextOverride "context.greeting" "charname:\n  charname\n"
        getContextDefinition "context.greeting" defaultGreeting
      runDefaultZeroAry (BranchName "empty") binding)
    `shouldBe` Right (Right "default text")

  it "a staged override takes priority over a same-named branch commit" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting.dsl", "\"from the branch\"\n")]
      binding <- interpretContextStorageFS $ do
        setContextOverride "context.greeting" "\"staged text\"\n"
        getContextDefinition "context.greeting" defaultGreeting
      runDefaultZeroAry (BranchName "main") binding)
    `shouldBe` Right (Right "staged text")

interpretContextStorageMapSpec :: Spec
interpretContextStorageMapSpec = describe "interpretContextStorageMap" $ do
  it "resolves an override from the map (a pure literal, no branch content needed)" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      let overrides = Map.fromList [("context.greeting", "\"overridden text\"\n")]
      binding <- interpretContextStorageMap overrides
                   (getContextDefinition "context.greeting" defaultGreeting)
      runDefaultZeroAry (BranchName "empty") binding)
    `shouldBe` Right (Right "overridden text")

  it "falls back to the caller's default on a map miss" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      binding <- interpretContextStorageMap Map.empty
                   (getContextDefinition "context.greeting" defaultGreeting)
      runDefaultZeroAry (BranchName "empty") binding)
    `shouldBe` Right (Right "default text")

interpretContextStorageFSSpec :: Spec
interpretContextStorageFSSpec = describe "interpretContextStorageFS" $ do
  it "falls back to the caller's default when no override is committed" $
    run (testStack $ do
      seedBranch "main" [("greeting.md", "hello from main")]
      binding <- interpretContextStorageFS (getContextDefinition "context.greeting" defaultGreeting)
      runDefaultZeroAry (BranchName "main") binding)
    `shouldBe` Right (Right "default text")

  it "runs a real committed override, positioned at the caller's own branch, not the Contexts branch" $
    run (testStack $ do
      seedBranch "main" [("greeting.md", "hello from main")]
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting.dsl", "< read \"greeting.md\"\n")]
      binding <- interpretContextStorageFS (getContextDefinition "context.greeting" defaultGreeting)
      runDefaultZeroAry (BranchName "main") binding)
    `shouldBe` Right (Right "hello from main")

  -- | Regression for the real gap the project chat found: every real
  --   character-context caller used to call
  --   'Storyteller.Context.DSL.Library.contextCharacterDefault' directly,
  --   never through 'getContextDefinition', so a project committing an
  --   override for @context.character@ (a 1-arity key, unlike every other
  --   registered definition tested above) was silently ignored even
  --   though 'interpretContextStorageFS' itself worked fine. Proves a
  --   1-arity override actually takes over, with the real argument
  --   ('runContextBinding1's own job) reaching the overriding definition.
  it "resolves and runs a real 1-arity override too, e.g. context.character's own shape" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/greeting1.dsl", "name:\n  \"overridden for %name%\"\n")]
      binding <- interpretContextStorageFS (getContextDefinition "context.greeting1" defaultEcho)
      runBranchOpGit @Main (BranchName "main") $ do
        v <- runContextBinding1 @Main binding "Aria"
        messagesText <$> runContextValue @Main (valueDefault v))
    `shouldBe` Right "overridden for Aria"

  it "falls back to the 1-arity default (echoing its own argument) when no override is committed" $
    run (testStack $ do
      seedBranch "main" []
      binding <- interpretContextStorageFS (getContextDefinition "context.greeting1" defaultEcho)
      runBranchOpGit @Main (BranchName "main") $ do
        v <- runContextBinding1 @Main binding "Aria"
        messagesText <$> runContextValue @Main (valueDefault v))
    `shouldBe` Right "default: Aria"

runContextBinding1Spec :: Spec
runContextBinding1Spec = describe "runContextBinding1" $
  it "fails loudly on an arity mismatch rather than silently misapplying" $
    run (testStack $ do
      seedBranch "main" []
      runBranchOpGit @Main (BranchName "main") $ do
        v <- runContextBinding1 @Main defaultGreeting "Aria"
        messagesText <$> runContextValue @Main (valueDefault v))
    `shouldBe` Left "expected a 1-arity context definition, got arity 0"
