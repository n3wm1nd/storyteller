{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles' -- the
--   interceptor that hides never-atom-tracked (binary) paths from a
--   filesystem read. The concrete bug this guards: a binary file uploaded
--   into a branch used to crash a raw 'Runix.FileSystem.readFile' outright
--   (an unsafe UTF-8 decode of raw image bytes) rather than being silently
--   excluded. Still used by UI-facing browsing reads ("Server.Writer.Lore",
--   "Server.Writer.ContextView.Connection") -- agent context assembly goes
--   through the Context DSL now (see CONTEXT-DSL.md), which excludes
--   binaries at the storage layer instead (see "Storage.QuerySpec").
module Storyteller.ContextFilterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles, readFile, writeFile)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)

import Prelude hiding (readFile, writeFile)

withFilterBranch
  :: T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withFilterBranch name action = run $ testStack $ do
  _ <- createBranch (BranchName name)
  runBranchAndFS @Main (BranchName name) action

spec :: Spec
spec = hideBinaryFilesSpec

hideBinaryFilesSpec :: Spec
hideBinaryFilesSpec = describe "hideBinaryFiles" $ do

  it "excludes a never-atom-tracked binary path from listAllFiles" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          writeFile @(BranchTag Main) "portrait.png" "\xFF\xFE\x00"
          _ <- runStorage @Main (Ops.commitFiles ["portrait.png"])
          hideBinaryFiles @(BranchTag Main) @Main (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldBe` ["scene.md"]

  it "leaves listAllFiles untouched when nothing is binary" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "other.md" "p2\n")
          hideBinaryFiles @(BranchTag Main) @Main (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldMatchList` ["scene.md", "other.md"]

  it "fails a readFile attempted on a hidden binary path, instead of crashing on the bytes themselves" $ do
    let result = withFilterBranch "story" $ do
          writeFile @(BranchTag Main) "portrait.png" "\xFF\xFE\x00"
          _ <- runStorage @Main (Ops.commitFiles ["portrait.png"])
          hideBinaryFiles @(BranchTag Main) @Main (readFile @(BranchTag Main) "portrait.png")
    result `shouldBe` Left "Access denied: binary files are hidden"
