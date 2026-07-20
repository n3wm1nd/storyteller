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

-- | 'Storage.Query.loadLiveWorkingTree' against a real (mock-git-backed)
--   branch: a genuinely never-atom-tracked path (same fixture shape as
--   'Storyteller.ContextFilterSpec's own 'hideBinaryFiles' case) never
--   shows up in its result at all.
module Storage.QuerySpec (spec) where

import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.FileSystem (writeFile)
import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storage.Query (loadLiveWorkingTree)
import Storyteller.Core.Git (BranchTag, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Prelude hiding (writeFile)

spec :: Spec
spec = describe "loadLiveWorkingTree" $
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
            files <- fst <$> Core.runStoreT commit (loadLiveWorkingTree commit)
            pure (map fst files)
    result `shouldBe` Right ["chapters/ch1.md", "notes.md"]
