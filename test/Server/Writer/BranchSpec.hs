{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Writer.BranchSpec (spec) where

import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy (Sem, run)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listFiles, listAllFiles, readFile)
import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Storage (StoryBranch, StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.Writer.Branch (uploadFiles)
import Server.TestStack

import Prelude hiding (readFile)

-- | 'uploadFiles' is the plain 'Sem' function
--   'Server.Writer.Branch.Dispatch' calls for an 'Upload' command, exercised
--   here directly with no WebSocket/dispatch layer involved — same as
--   'Server.BranchSpec' tests 'Server.Core.Branch' directly. It does write
--   through 'StoryStorage' (via 'commitFiles'), so — like the mutating
--   operations in 'Server.BranchSpec' — it's run under both 'runner'
--   variants (see 'test/Main.hs'): eager, and buffered through
--   'Storyteller.Core.Git.withStorage', the transaction a real upload
--   command actually runs inside.
withBranch_
  :: TestRunner
  -> BranchName
  -> Sem ( StoryBranch Main
         : FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withBranch_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

spec :: TestRunner -> Spec
spec runner = describe "uploadFiles" $ do

  it "creates a new file with the uploaded content" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("dropped.md", "hello world")]
          content <- readFile @(BranchTag Main) "dropped.md"
          return (TE.decodeUtf8 content)
    result `shouldBe` Right "hello world"

  it "the uploaded path appears in the branch's file list" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("dropped.md", "hello world")]
          listFiles @(BranchTag Main) "/"
    result `shouldSatisfy` either (const False) (elem "dropped.md")

  it "uploads multiple files in one call" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          paths <- uploadFiles [("a.md", "content a"), ("folder/b.md", "content b")]
          files <- listAllFiles @(BranchTag Main) "/"
          return (paths, files)
    case result of
      Left err -> expectationFailure err
      Right (paths, files) -> do
        paths `shouldBe` ["a.md", "folder/b.md"]
        files `shouldContain` ["a.md"]
        files `shouldContain` ["folder/b.md"]

  it "returns the uploaded paths, so the caller can push FileAdded events" $ do
    let result = withBranch_ runner (BranchName "test") $
          uploadFiles [("one.md", "1"), ("two.md", "2")]
    result `shouldBe` Right ["one.md", "two.md"]

  it "an uploaded file's content survives being read back after a second, unrelated upload" $ do
    -- Regression check for 'commitFiles' being scoped to just the given
    -- paths (see Storyteller.Core.Edit) rather than reconciling the whole
    -- branch: a second, unrelated upload must not disturb an already
    -- uploaded file's content.
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("first.md", "first content")]
          _ <- uploadFiles [("second.md", "second content")]
          readFile @(BranchTag Main) "first.md"
    fmap TE.decodeUtf8 result `shouldBe` Right "first content"
