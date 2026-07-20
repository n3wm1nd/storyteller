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

-- | 'Storyteller.Context.DSL.Scope.liveTreeValueOfCommit' against a real
--   (mock-git-backed) branch: everything else about "what's lore" is DSL
--   policy now (see the module's own haddock), so the only thing left to
--   check here is the one fact that has to be decided in Haskell -- a
--   genuinely never-atom-tracked path (same fixture shape as
--   'Storyteller.ContextFilterSpec's own 'hideBinaryFiles' case) never
--   shows up in the scope at all.
module Storyteller.Context.DSL.ScopeSpec (spec) where

import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.FileSystem (writeFile)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchTag, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Scope (liveTreeValueOfCommit)
import Storyteller.Context.DSL.Value

import Prelude hiding (writeFile)

instance Members '[Git, StoryStorage, Fail] r => MonadBranch (Sem r) where
  resolveBranch name = getBranch name >>= \case
    Nothing -> pure Nothing
    Just b  -> pure (Just (Core.ObjectHash (unTickId (branchHead b))))

spec :: Spec
spec = describe "liveTreeValueOfCommit" $
  it "excludes a never-atom-tracked path, keeping every atom-tracked one" $ do
    let result = run $ testStack $ do
          _ <- createBranch (BranchName "story")
          runBranchAndFS @Main (BranchName "story") $ do
            _ <- runStorage @Main (Ops.addAtom "notes.md" "a hand-authored note")
            _ <- runStorage @Main (Ops.addAtom "chapters/ch1.md" "chapter one prose")
            writeFile @(BranchTag Main) "cover.png" "\xFF\xFE\x00"
            _ <- runStorage @Main (Ops.commitFiles ["cover.png"])
            Just b <- getBranch (BranchName "story")
            let commit = Core.ObjectHash (unTickId (branchHead b))
            scope <- fst <$> Core.runStoreT commit (runAction (liveTreeValueOfCommit commit))
            pure (map fst (valueEntries scope))
    result `shouldBe` Right ["chapters/ch1.md", "notes.md"]
