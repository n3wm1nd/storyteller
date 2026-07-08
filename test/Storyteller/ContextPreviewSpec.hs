{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Writer.Agent.ContextPreview.buildSlotPreview' -- specifically
--   that an excluded file stays in the result (shaded, not removed) so the
--   Agents tab's context preview can show it in place rather than having it
--   vanish from the tree. The concrete regression this guards: an earlier
--   version filtered excluded paths out of the returned list entirely,
--   which is indistinguishable, from a client's perspective, from the file
--   never having existed.
module Storyteller.ContextPreviewSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Storage.Ops as Ops

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextPreview

withPreviewBranch
  :: T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withPreviewBranch name action = run $ testStack $ do
  _ <- createBranch (BranchName name)
  runBranchAndFS @Main (BranchName name) action

spec :: Spec
spec = describe "buildSlotPreview" $ do

  it "keeps an excluded file in the result, marked not-included and without content" $ do
    let slot = ContextSlot "story" Ambient (PathFilter [] ["secret.md"])
        result = withPreviewBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "secret.md" "shh\n")
          buildSlotPreview @(BranchTag Main) slot
    case result of
      Left err -> expectationFailure err
      Right (ContextSlotPreview _ _ entries) -> do
        map cePath entries `shouldMatchList` ["scene.md", "secret.md"]
        let Just secret = lookup "secret.md" [ (cePath e, e) | e <- entries ]
            Just scene  = lookup "scene.md"  [ (cePath e, e) | e <- entries ]
        ceIncluded secret `shouldBe` False
        ceContent  secret `shouldBe` Nothing
        ceIncluded scene  `shouldBe` True
        ceContent  scene  `shouldBe` Just "p1\n"

  it "inverted (include-only) filter still lists everything, marking only the include list as included" $ do
    let slot = ContextSlot "story" Ambient (PathFilter ["scene.md"] [])
        result = withPreviewBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "other.md" "p2\n")
          buildSlotPreview @(BranchTag Main) slot
    case result of
      Left err -> expectationFailure err
      Right (ContextSlotPreview _ _ entries) -> do
        map cePath entries `shouldMatchList` ["scene.md", "other.md"]
        let Just scene = lookup "scene.md" [ (cePath e, e) | e <- entries ]
            Just other = lookup "other.md" [ (cePath e, e) | e <- entries ]
        ceIncluded scene `shouldBe` True
        ceIncluded other `shouldBe` False
