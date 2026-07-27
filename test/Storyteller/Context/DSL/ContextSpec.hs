{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
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

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified Storage.Ops as Ops
import Storyteller.Core.Branch (Branches)
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Core.Context (ContextRow, ContextStorage, runContextValue)
import Runix.FileSystem (FileSystem, FileSystemRead)
import Storyteller.Core.ContentEffects (BranchResolve)

import Storyteller.Context.DSL.Compile (ContextFS, Library)
import Storyteller.Context.DSL.Context (Context, toContext, user, assistant, runContext)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.QQ (dsl, dslWith)
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn
  :: forall a
  .  BranchName
  -> (forall r. Members '[BranchOp Main, Branches, BranchResolve, ContextStorage, Fail] r => Action (ContextRow Main r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = runBranchAndFS @Main bname (runContextValue @Main act)

spec :: Spec
spec = do
  contextMonoidSpec
  toBindingSpec
  inlineLiteralSpec

-- | How close inline @['dsl'| ... |]@ snippets already get to "just wrap
--   this string as a Value" \/ "just make a one-key Value" without a
--   named top-level definition -- checking against the real interpreter,
--   not just reasoning about the grammar.
inlineLiteralSpec :: Spec
inlineLiteralSpec = describe "inline single-expression [dsl| |] snippets" $ do

  it "a: a -- the identity, no branch/file access needed at all" $
    run (testStack $ do
      seedBranch "main" []
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< identityDsl @Main "the value as text")))
    `shouldBe` Right "the value as text"

  it "a: b: as a: b -- a one-key Value, keyed by the first argument's own text" $
    run (testStack $ do
      seedBranch "main" []
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< (scopedDsl @Main "key" "value" >>= namedEntry "key"))))
    `shouldBe` Right "value"
  where
    identityDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Text -> Action r (Value r)
    identityDsl = [dsl| a: a |]

    scopedDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Text -> Text -> Action r (Value r)
    scopedDsl = [dsl| a: b: as a: b |]

contextMonoidSpec :: Spec
contextMonoidSpec = describe "Context (Semigroup/Monoid)" $ do

  it "toContext combines a Value's own forced messages in order" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a"), ("lore/b.md", "lore b")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (toContext (loreDsl @Main))))
    `shouldBe` Right ["lore a", "lore b"]

  it "<> concatenates two Contexts, left to right" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (user "first" <> toContext (loreDsl @Main) <> assistant "last")))
    `shouldBe` Right ["first", "lore a", "last"]

  it "mempty is the identity" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore a")]
      runDslOn (BranchName "main")
        (map messageText <$> runContext (mempty <> toContext (loreDsl @Main) <> mempty)))
    `shouldBe` Right ["lore a"]
  where
    loreDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Action r (Value r)
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
        (messagesText <$> (valueDefault =<< crossBranchDsl @Main (CtxLibrary.hostLibrary @Main) "aria")))
    `shouldBe` Right "Aria is a wandering rogue."

  it "accepts a bare Context argument, no bval wrapping" $
    run (testStack $ do
      seedBranch "main" []
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< splicesDsl @Main (user "hello"))))
    `shouldBe` Right "hello"
  where
    crossBranchDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,BranchResolve, Fail] r => Library r -> Text -> Action r (Value r)
    crossBranchDsl = [dslWith|
      charname:
        in (charname | branch): read "sheet.md"
      |]

    splicesDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Context r -> Action r (Value r)
    splicesDsl = [dsl|
      ctx:
        < ctx
      |]
