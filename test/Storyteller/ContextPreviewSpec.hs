{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Writer.Agent.ContextPreview.buildPreview' -- that it runs
--   a client-submitted Context DSL program the *same* way a real
--   @chat.writer@\/@correct.group@ call would ('Server.Writer.File.chatWriter'
--   staging @fcContext@ via @setContextOverride@, then resolving
--   @context.writer@), and renders the identical tree shape
--   'Storyteller.Context.DSL.Rendering.renderContext' produces for real
--   generation. This replaces the old bucket-picker preview spec: there is
--   no longer a separate glob\/bucket layout to preview, so there is
--   nothing left to test but "does the program that would actually run,
--   actually run" -- which is also, by construction, the guarantee that a
--   preview can never show something a real send wouldn't.
module Storyteller.ContextPreviewSpec (spec) where

import Test.Hspec

import Polysemy (run)

import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Storage.Ops as Ops

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextPreview

spec :: Spec
spec = describe "buildPreview" $ do

  it "a program that calls context.lore directly resolves against real content" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $ do
        _ <- runStorage @Main (Ops.addAtom "lore/notes.md" "a hand-authored note")
        buildPreview @Main "target.md" "path:\n  context.lore\n")
    `shouldBe`
      Right (PreviewNode ["## Story background", "## lore/notes.md", "a hand-authored note"] [])

  it "a client program replaces the default completely, producing exactly its own tree" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $ do
        _ <- runStorage @Main (Ops.addAtom "lore/secret.md" "should not appear")
        buildPreview @Main "target.md" "path:\n  x = \"only this\"\n  as \"custom\": x\n  x\n")
    `shouldBe`
      Right (PreviewNode ["only this"] [("custom", PreviewNode ["only this"] [])])

  it "resolves per the submitted path, not a fixed one" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $ do
        buildPreview @Main "chapters/ch1.md" "path:\n  path\n")
    `shouldBe`
      Right (PreviewNode ["chapters/ch1.md"] [])
