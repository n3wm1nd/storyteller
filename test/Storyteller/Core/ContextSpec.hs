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

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Context
  (contextsBranchName, getContextDefinition, interpretContextStorageFS, interpretContextStorageMap, resolveContextOverride)
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Compile (Binding(..), bval)
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
    (msgs, _) <- Core.runStoreT h (runAction (fn [] emptyValue >>= valueDefault))
    pure (Right (messagesText msgs))
runDefaultZeroAry _ (Binding n _) = pure (Left ("expected arity 0, got " <> show n))

spec :: Spec
spec = do
  resolveContextOverrideSpec
  interpretContextStorageMapSpec
  interpretContextStorageFSSpec

defaultGreeting :: Binding
defaultGreeting = bval (pure (leafValue [User "default text"]))

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
