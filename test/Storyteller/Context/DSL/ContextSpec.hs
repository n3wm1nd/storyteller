{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Storyteller.Context.DSL.Context.Context''s own 'Monoid' (composing
--   already-DSL-sourced fragments and literal text via @('<>')@) and
--   'Storyteller.Context.DSL.Context.ToBinding' (a @['dsl'| ... |]@
--   parameter accepting a plain 'Text'\/'Context' argument directly, no
--   'Storyteller.Context.DSL.Compile.bval' at the call site -- see
--   "Storyteller.Context.DSL.QQ"'s own codegen change). Same
--   mock-git-backed harness "Storyteller.Context.DSL.CompileSpec" uses.
module Storyteller.Context.DSL.ContextSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Polysemy (Sem, run)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Context (Context, toContext, user, assistant, runContext)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> show bname)
  Just h  -> fst <$> Core.runStoreT h (runAction act)

spec :: Spec
spec = do
  contextMonoidSpec
  toBindingSpec

contextMonoidSpec :: Spec
contextMonoidSpec = describe "Context (Semigroup/Monoid)" $ do

  it "toContext combines a Value's own forced messages in order" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a"), ("lore/b.md", "lore b")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (toContext loreDsl)))
    `shouldBe` Right ["lore a", "lore b"]

  it "<> concatenates two Contexts, left to right" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (user "first" <> toContext loreDsl <> assistant "last")))
    `shouldBe` Right ["first", "lore a", "last"]

  it "mempty is the identity" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (mempty <> toContext loreDsl <> mempty)))
    `shouldBe` Right ["lore a"]
  where
    loreDsl :: Action Value
    loreDsl = [dsl|
      for f in lore/**/*:
        as f: read f
      |]

-- | A @['dsl'| ... |]@ definition called with a plain 'Text' argument
--   directly, and one called with a 'Context' argument directly -- proof
--   the QQ's own codegen change (applying
--   'Storyteller.Context.DSL.Context.toBinding' per argument) actually
--   lets a call site skip 'Storyteller.Context.DSL.Compile.bval', not just
--   that the old 'Storyteller.Context.DSL.Compile.Binding'-typed call
--   shape still compiles.
toBindingSpec :: Spec
toBindingSpec = describe "ToBinding (plain values as [dsl| |] arguments)" $ do

  it "accepts a bare Text argument, no bval wrapping" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch "character/aria" [("sheet.md", "Aria is a wandering rogue.")]
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< crossBranchDsl "aria")))
    `shouldBe` Right "Aria is a wandering rogue."

  it "accepts a bare Context argument, no bval wrapping" $
    run (testStack $ do
      seedBranch "main" []
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< splicesDsl (user "hello"))))
    `shouldBe` Right "hello"
  where
    crossBranchDsl :: Text -> Action Value
    crossBranchDsl = [dsl|
      charname:
        in (charname | branch): read "sheet.md"
      |]

    splicesDsl :: Context -> Action Value
    splicesDsl = [dsl|
      ctx:
        < ctx
      |]
